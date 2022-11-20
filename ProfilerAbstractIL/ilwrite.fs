module ProfilerAbstractIL.ILBinaryWriter

open System
open System.Collections.Generic
open System.IO

open ProfilerAbstractIL.Utilities.Library
open ProfilerAbstractIL.IL
open ProfilerAbstractIL.Diagnostics
open ProfilerAbstractIL.BinaryConstants
open ProfilerAbstractIL.IO

//---------------------------------------------------------------------
// Byte, byte array fragments and other concrete representations
// manipulations.
//---------------------------------------------------------------------

// Little-endian encoding of int32
let b0 n = byte (n &&& 0xFF)
let b1 n = byte ((n >>> 8) &&& 0xFF)
let b2 n = byte ((n >>> 16) &&& 0xFF)
let b3 n = byte ((n >>> 24) &&& 0xFF)

// Little-endian encoding of int64
let dw7 n = byte ((n >>> 56) &&& 0xFFL)
let dw6 n = byte ((n >>> 48) &&& 0xFFL)
let dw5 n = byte ((n >>> 40) &&& 0xFFL)
let dw4 n = byte ((n >>> 32) &&& 0xFFL)
let dw3 n = byte ((n >>> 24) &&& 0xFFL)
let dw2 n = byte ((n >>> 16) &&& 0xFFL)
let dw1 n = byte ((n >>> 8) &&& 0xFFL)
let dw0 n = byte (n &&& 0xFFL)

let bitsOfSingle (x: float32) = BitConverter.ToInt32(BitConverter.GetBytes x, 0)
let bitsOfDouble (x: float) = BitConverter.DoubleToInt64Bits x

/// Arbitrary value
[<Literal>]
let EmitBytesViaBufferCapacity = 10
let emitBytesViaBuffer f = use bb = ByteBuffer.Create EmitBytesViaBufferCapacity in f bb; bb.AsMemory().ToArray()

/// Alignment and padding
let align alignment n = ((n + alignment - 1) / alignment) * alignment

//---------------------------------------------------------------------
// Concrete token representations etc. used in PE files
//---------------------------------------------------------------------

type ByteBuffer with

    /// Z32 = compressed unsigned integer
    static member Z32Size n =
      if n <= 0x7F then 1
      elif n <= 0x3FFF then 2
      else 4

    /// Emit int32 as compressed unsigned integer
    member buf.EmitZ32 n =
        if n >= 0 && n <= 0x7F then
            buf.EmitIntAsByte n
        elif n >= 0x80 && n <= 0x3FFF then
            buf.EmitIntAsByte (0x80 ||| (n >>> 8))
            buf.EmitIntAsByte (n &&& 0xFF)
        else
            buf.EmitIntAsByte (0xC0 ||| ((n >>> 24) &&& 0xFF))
            buf.EmitIntAsByte ((n >>> 16) &&& 0xFF)
            buf.EmitIntAsByte ((n >>> 8) &&& 0xFF)
            buf.EmitIntAsByte (n &&& 0xFF)

    member buf.EmitPadding n =
        for i = 0 to n-1 do
            buf.EmitByte 0x0uy

    // Emit compressed untagged integer
    member buf.EmitZUntaggedIndex big idx =
        if big then buf.EmitInt32 idx
        else
            // Note, we can have idx=0x10000 generated for method table idx + 1 for just beyond last index of method table.
            // This indicates that a MethodList, FieldList, PropertyList or EventList has zero entries
            // For this case, the EmitInt32AsUInt16 writes a 0 (null) into the field.  Binary readers respect this as an empty
            // list of methods/fields/properties/events.
            if idx > 0x10000 then
                System.Diagnostics.Debug.Assert (false, "EmitZUntaggedIndex: too big for small address or simple index")
            buf.EmitInt32AsUInt16 idx

    // Emit compressed tagged integer
    member buf.EmitZTaggedIndex tag nbits big idx =
        let idx2 = (idx <<< nbits) ||| tag
        if big then buf.EmitInt32 idx2
        else buf.EmitInt32AsUInt16 idx2


let getUncodedToken (tab: TableName) idx = ((tab.Index <<< 24) ||| idx)

// --------------------------------------------------------------------
// Fixups
// --------------------------------------------------------------------

/// Check that the data held at a fixup is some special magic value, as a sanity check
/// to ensure the fixup is being placed at a ood location.
let checkFixup32 (data: byte[]) offset exp =
    if data[offset + 3] <> b3 exp then failwith "fixup sanity check failed"
    if data[offset + 2] <> b2 exp then failwith "fixup sanity check failed"
    if data[offset + 1] <> b1 exp then failwith "fixup sanity check failed"
    if data[offset] <> b0 exp then failwith "fixup sanity check failed"

let applyFixup32 (data: byte[]) offset v =
    data[offset] <- b0 v
    data[offset+1] <- b1 v
    data[offset+2] <- b2 v
    data[offset+3] <- b3 v

type ExceptionClauseKind =
  | FinallyClause
  | FaultClause
  | TypeFilterClause of int32
  | FilterClause of int

type ExceptionClauseSpec = int * int * int * int * ExceptionClauseKind

/// Arbitrary value
[<Literal>]
let CodeBufferCapacity = 200 

/// Buffer to write results of emitting code into. Also record:
///   - branch sources (where fixups will occur)
///   - possible branch destinations
///   - locations of embedded handles into the string table
///   - the exception table
type CodeBuffer =
    {
      code: ByteBuffer

      /// (instruction; optional short form); start of instr in code buffer; code loc for the end of the instruction the fixup resides in ; where is the destination of the fixup
      mutable reqdBrFixups: ((int * int option) * int * ILCodeLabel list) list

      availBrFixups: Dictionary<ILCodeLabel, int>

      /// data for exception handling clauses
      mutable seh: ExceptionClauseSpec list
    }

    interface IDisposable with
        member this.Dispose() =
            (this.code :> IDisposable).Dispose()

    static member Create _nm =
        { seh = []
          code= ByteBuffer.Create CodeBufferCapacity
          reqdBrFixups=[]
          availBrFixups = Dictionary<_, _>(10, HashIdentity.Structural)
        }

    member codebuf.EmitExceptionClause seh = codebuf.seh <- seh :: codebuf.seh

    member codebuf.EmitByte x = codebuf.code.EmitIntAsByte x

    member codebuf.EmitUInt16 x = codebuf.code.EmitUInt16 x

    member codebuf.EmitInt32 x = codebuf.code.EmitInt32 x

    member codebuf.EmitInt64 x = codebuf.code.EmitInt64 x

    member codebuf.EmitUncodedToken u = codebuf.EmitInt32 u

    member codebuf.RecordReqdBrFixups i tgs =
        codebuf.reqdBrFixups <- (i, codebuf.code.Position, tgs) :: codebuf.reqdBrFixups
        // Write a special value in that we check later when applying the fixup
        // Value is 0x11 {deadbbbb}* where 11 is for the instruction and deadbbbb is for each target
        codebuf.EmitByte 0x11 // for the instruction
        (if fst i = i_switch then
          codebuf.EmitInt32 tgs.Length)
        List.iter (fun _ -> codebuf.EmitInt32 0xdeadbbbb) tgs

    member codebuf.RecordReqdBrFixup i tg = codebuf.RecordReqdBrFixups i [tg]

    member codebuf.RecordAvailBrFixup tg =
        codebuf.availBrFixups[tg] <- codebuf.code.Position



/// Applying branch fixups. Use short versions of instructions
/// wherever possible. Sadly we can only determine if we can use a short
/// version after we've layed out the code for all other instructions.
/// This in turn means that using a short version may change
/// the various offsets into the code.
module Codebuf =

    let binaryChop p (arr: 'T[]) =
        let rec go n m =
            if n > m then raise (KeyNotFoundException("binary chop did not find element"))
            else
                let i = (n+m)/2
                let c = p arr[i]
                if c = 0 then i elif c < 0 then go n (i-1) else go (i+1) m
        go 0 (Array.length arr)

    let applyBrFixups (origCode : byte[]) origExnClauses (origAvailBrFixups: Dictionary<ILCodeLabel, int>) origReqdBrFixups =
      let orderedOrigReqdBrFixups = origReqdBrFixups |> List.sortBy (fun (_, fixupLoc, _) -> fixupLoc)

      use newCode = ByteBuffer.Create origCode.Length

      // Copy over all the code, working out whether the branches will be short
      // or long and adjusting the branch destinations. Record an adjust function to adjust all the other
      // gumpf that refers to fixed offsets in the code stream.
      let newCode, newReqdBrFixups, adjuster =
          let mutable remainingReqdFixups = orderedOrigReqdBrFixups
          let mutable origWhere = 0
          let mutable newWhere = 0
          let mutable doneLast = false
          let mutable newReqdBrFixups = []
          let mutable adjustments = []

          while (remainingReqdFixups <> [] || not doneLast) do
              let doingLast = isNil remainingReqdFixups
              let origStartOfNoBranchBlock = origWhere
              let newStartOfNoBranchBlock = newWhere

              let origEndOfNoBranchBlock =
                if doingLast then origCode.Length
                else
                  let _, origStartOfInstr, _ = List.head remainingReqdFixups
                  origStartOfInstr

              // Copy over a chunk of non-branching code
              let nobranch_len = origEndOfNoBranchBlock - origStartOfNoBranchBlock
              newCode.EmitBytes origCode[origStartOfNoBranchBlock..origStartOfNoBranchBlock+nobranch_len-1]

              // Record how to adjust addresses in this range, including the branch instruction
              // we write below, or the end of the method if we're doing the last bblock
              adjustments <- (origStartOfNoBranchBlock, origEndOfNoBranchBlock, newStartOfNoBranchBlock) :: adjustments

              // Increment locations to the branch instruction we're really interested in
              origWhere <- origEndOfNoBranchBlock
              newWhere <- newWhere + nobranch_len

              // Now do the branch instruction. Decide whether the fixup will be short or long in the new code
              if doingLast then
                  doneLast <- true
              else
                  let (i, origStartOfInstr, tgs: ILCodeLabel list) = List.head remainingReqdFixups
                  remainingReqdFixups <-List.tail remainingReqdFixups
                  if origCode[origStartOfInstr] <> 0x11uy then failwith "br fixup sanity check failed (1)"
                  let i_length = if fst i = i_switch then 5 else 1
                  origWhere <- origWhere + i_length

                  let origEndOfInstr = origStartOfInstr + i_length + 4 * tgs.Length
                  let newEndOfInstrIfSmall = newWhere + i_length + 1
                  let newEndOfInstrIfBig = newWhere + i_length + 4 * tgs.Length

                  let short =
                    match i, tgs with
                    | (_, Some i_short), [tg]
                        when
                           // Use the original offsets to compute if the branch is small or large. This is
                           // a safe approximation because code only gets smaller.
                           (let origDest =
                                match origAvailBrFixups.TryGetValue tg with
                                | true, fixup -> fixup
                                | _ ->
                                    // TODO RP import formatCodeLabel into il.fs
                                    dprintn ("branch target " + formatCodeLabel tg + " not found in code")
                                    666666
                            let origRelOffset = origDest - origEndOfInstr
                            -128 <= origRelOffset && origRelOffset <= 127)
                      ->
                        newCode.EmitIntAsByte i_short
                        true
                    | (i_long, _), _ ->
                        newCode.EmitIntAsByte i_long
                        (if i_long = i_switch then
                          newCode.EmitInt32 tgs.Length)
                        false

                  newWhere <- newWhere + i_length
                  if newWhere <> newCode.Position then dprintn "mismatch between newWhere and newCode"

                  tgs |> List.iter (fun tg ->
                        let origFixupLoc = origWhere
                        checkFixup32 origCode origFixupLoc 0xdeadbbbb

                        if short then
                            newReqdBrFixups <- (newWhere, newEndOfInstrIfSmall, tg, true) :: newReqdBrFixups
                            newCode.EmitIntAsByte 0x98 (* sanity check *)
                            newWhere <- newWhere + 1
                        else
                            newReqdBrFixups <- (newWhere, newEndOfInstrIfBig, tg, false) :: newReqdBrFixups
                            newCode.EmitInt32 0xf00dd00f (* sanity check *)
                            newWhere <- newWhere + 4
                        if newWhere <> newCode.Position then dprintn "mismatch between newWhere and newCode"
                        origWhere <- origWhere + 4)

                  if origWhere <> origEndOfInstr then dprintn "mismatch between origWhere and origEndOfInstr"

          let adjuster =
            let arr = Array.ofList (List.rev adjustments)
            fun addr ->
              let i =
                  try binaryChop (fun (a1, a2, _) -> if addr < a1 then -1 elif addr > a2 then 1 else 0) arr
                  with
                     :? KeyNotFoundException ->
                         failwith ("adjuster: address "+string addr+" is out of range")
              let origStartOfNoBranchBlock, _, newStartOfNoBranchBlock = arr[i]
              addr - (origStartOfNoBranchBlock - newStartOfNoBranchBlock)

          newCode.AsMemory().ToArray(),
          newReqdBrFixups,
          adjuster

      // Now adjust everything
      let newAvailBrFixups =
          let tab = Dictionary<_, _>(10, HashIdentity.Structural)
          for KeyValue(tglab, origBrDest) in origAvailBrFixups do
              tab[tglab] <- adjuster origBrDest
          tab
      let newExnClauses =
          origExnClauses |> List.map (fun (st1, sz1, st2, sz2, kind) ->
              (adjuster st1, (adjuster (st1 + sz1) - adjuster st1),
               adjuster st2, (adjuster (st2 + sz2) - adjuster st2),
               (match kind with
               | FinallyClause | FaultClause | TypeFilterClause _ -> kind
               | FilterClause n -> FilterClause (adjuster n))))

      // Now apply the adjusted fixups in the new code
      newReqdBrFixups |> List.iter (fun (newFixupLoc, endOfInstr, tg, small) ->
          match newAvailBrFixups.TryGetValue tg with
          | true, n ->
              let relOffset = n - endOfInstr
              if small then
                  if Bytes.get newCode newFixupLoc <> 0x98 then failwith "br fixup sanity check failed"
                  newCode[newFixupLoc] <- b0 relOffset
              else
                  checkFixup32 newCode newFixupLoc 0xf00dd00fl
                  applyFixup32 newCode newFixupLoc relOffset
          | _ -> failwith ("target " + formatCodeLabel tg + " not found in new fixups"))

      newCode, newExnClauses

    // --------------------------------------------------------------------
    // Structured residue of emitting instructions: SEH exception handling
    // and scopes for local variables.
    // --------------------------------------------------------------------

    // Emitting instructions generates a tree of seh specifications
    // We then emit the exception handling specs separately.
    // nb. ECMA spec says the SEH blocks must be returned inside-out
    type SEHTree =
      | Node of ExceptionClauseSpec option * SEHTree list

    // --------------------------------------------------------------------
    // Table of encodings for instructions without arguments, also indexes
    // for all instructions.
    // --------------------------------------------------------------------

    let encodingsForNoArgInstrs = Dictionary<_, _>(300, HashIdentity.Structural)
    let _ =
      List.iter
        (fun (x, mk) -> encodingsForNoArgInstrs[mk] <- x)
        (noArgInstrs.Force())
    let encodingsOfNoArgInstr si = encodingsForNoArgInstrs[si]

    // --------------------------------------------------------------------
    // Emit instructions
    // --------------------------------------------------------------------

    /// Emit the code for an instruction
    let emitInstrCode (codebuf: CodeBuffer) i =
        if i > 0xFF then
            assert (i >>> 8 = 0xFE)
            codebuf.EmitByte ((i >>> 8) &&& 0xFF)
            codebuf.EmitByte (i &&& 0xFF)
        else
            codebuf.EmitByte i

    let emitTypeInstr codebuf i (tlb, idx) =
        emitInstrCode codebuf i
        codebuf.EmitUncodedToken (getUncodedToken (TableName.FromIndex tlb) idx)

    let emitMethodSpecInfoInstr codebuf i (tlb, idx) =
        emitInstrCode codebuf i
        codebuf.EmitUncodedToken (getUncodedToken (TableName.FromIndex tlb) idx)

    // TODO RP varargs
    let emitMethodSpecInstr codebuf i ((tlb, idx), _) =
        emitInstrCode codebuf i
        codebuf.EmitUncodedToken (getUncodedToken (TableName.FromIndex tlb) idx)

    let emitFieldSpecInstr codebuf i (tlb, idx) =
        emitInstrCode codebuf i
        codebuf.EmitUncodedToken (getUncodedToken (TableName.FromIndex tlb) idx)

    let emitShortUInt16Instr codebuf (i_short, i) x =
        let n = int32 x
        if n <= 255 then
            emitInstrCode codebuf i_short
            codebuf.EmitByte n
        else
            emitInstrCode codebuf i
            codebuf.EmitUInt16 x

    let emitShortInt32Instr codebuf (i_short, i) x =
        if x >= -128 && x <= 127 then
            emitInstrCode codebuf i_short
            codebuf.EmitByte (if x < 0x0 then x + 256 else x)
        else
            emitInstrCode codebuf i
            codebuf.EmitInt32 x

    let emitTailness codebuf tl =
        // RP TODO do we really want to turn off tail calls
        // if tl = Tailcall && cenv.emitTailcalls then emitInstrCode codebuf i_tail
        if tl = Tailcall then emitInstrCode codebuf i_tail

    let emitVolatility codebuf tl =
        if tl = Volatile then emitInstrCode codebuf i_volatile

    let emitConstrained codebuf (tlb, idx) =
        emitInstrCode codebuf i_constrained
        codebuf.EmitUncodedToken (getUncodedToken (TableName.FromIndex tlb) idx)

    let emitAlignment codebuf tl =
        match tl with
        | Aligned -> ()
        | Unaligned1 -> emitInstrCode codebuf i_unaligned; codebuf.EmitByte 0x1
        | Unaligned2 -> emitInstrCode codebuf i_unaligned; codebuf.EmitByte 0x2
        | Unaligned4 -> emitInstrCode codebuf i_unaligned; codebuf.EmitByte 0x4

    let rec emitInstr codebuf instr =
        match instr with
        | si when isNoArgInstr si ->
             emitInstrCode codebuf (encodingsOfNoArgInstr si)
        | I_brcmp (cmp, tg1) ->
            codebuf.RecordReqdBrFixup ((Lazy.force ILCmpInstrMap)[cmp], Some (Lazy.force ILCmpInstrRevMap).[cmp]) tg1
        | I_br tg -> codebuf.RecordReqdBrFixup (i_br, Some i_br_s) tg
        | I_leave tg -> codebuf.RecordReqdBrFixup (i_leave, Some i_leave_s) tg
        | I_call (tl, mspec, varargs) ->
            emitTailness codebuf tl
            emitMethodSpecInstr codebuf i_call (mspec, varargs)
            //emitAfterTailcall codebuf tl
        | I_callvirt (tl, mspec, varargs) ->
            emitTailness codebuf tl
            emitMethodSpecInstr codebuf i_callvirt (mspec, varargs)
            //emitAfterTailcall codebuf tl
        | I_callconstraint (callvirt, tl, ty, mspec, varargs) ->
            emitTailness codebuf tl
            emitConstrained codebuf ty
            let instr = if callvirt then i_callvirt else i_call
            emitMethodSpecInstr codebuf instr (mspec, varargs)
            //emitAfterTailcall codebuf tl
        | I_newobj (mspec, varargs) ->
            emitMethodSpecInstr codebuf i_newobj (mspec, varargs)
        | I_ldftn mspec ->
            emitMethodSpecInstr codebuf i_ldftn (mspec, None)
        | I_ldvirtftn mspec ->
            emitMethodSpecInstr codebuf i_ldvirtftn (mspec, None)

        | I_calli (tl, callsig, varargs) ->
            emitTailness codebuf tl
            emitMethodSpecInstr codebuf i_calli (callsig, varargs)
            //emitAfterTailcall codebuf tl

        | I_ldarg u16 -> emitShortUInt16Instr codebuf (i_ldarg_s, i_ldarg) u16
        | I_starg u16 -> emitShortUInt16Instr codebuf (i_starg_s, i_starg) u16
        | I_ldarga u16 -> emitShortUInt16Instr codebuf (i_ldarga_s, i_ldarga) u16
        | I_ldloc u16 -> emitShortUInt16Instr codebuf (i_ldloc_s, i_ldloc) u16
        | I_stloc u16 -> emitShortUInt16Instr codebuf (i_stloc_s, i_stloc) u16
        | I_ldloca u16 -> emitShortUInt16Instr codebuf (i_ldloca_s, i_ldloca) u16

        | I_cpblk (al, vol) ->
            emitAlignment codebuf al
            emitVolatility codebuf vol
            emitInstrCode codebuf i_cpblk
        | I_initblk (al, vol) ->
            emitAlignment codebuf al
            emitVolatility codebuf vol
            emitInstrCode codebuf i_initblk

        | AI_ldc (DT_I4, ILConst.I4 x) ->
            emitShortInt32Instr codebuf (i_ldc_i4_s, i_ldc_i4) x
        | AI_ldc (DT_I8, ILConst.I8 x) ->
            emitInstrCode codebuf i_ldc_i8
            codebuf.EmitInt64 x
        | AI_ldc (_, ILConst.R4 x) ->
            emitInstrCode codebuf i_ldc_r4
            codebuf.EmitInt32 (bitsOfSingle x)
        | AI_ldc (_, ILConst.R8 x) ->
            emitInstrCode codebuf i_ldc_r8
            codebuf.EmitInt64 (bitsOfDouble x)

        | I_ldind (al, vol, dt) ->
            emitAlignment codebuf al
            emitVolatility codebuf vol
            emitInstrCode codebuf
              (match dt with
              | DT_I -> i_ldind_i
              | DT_I1 -> i_ldind_i1
              | DT_I2 -> i_ldind_i2
              | DT_I4 -> i_ldind_i4
              | DT_U1 -> i_ldind_u1
              | DT_U2 -> i_ldind_u2
              | DT_U4 -> i_ldind_u4
              | DT_I8 -> i_ldind_i8
              | DT_R4 -> i_ldind_r4
              | DT_R8 -> i_ldind_r8
              | DT_REF -> i_ldind_ref
              | _ -> failwith "ldind")

        | I_stelem dt ->
            emitInstrCode codebuf
              (match dt with
              | DT_I | DT_U -> i_stelem_i
              | DT_U1 | DT_I1 -> i_stelem_i1
              | DT_I2 | DT_U2 -> i_stelem_i2
              | DT_I4 | DT_U4 -> i_stelem_i4
              | DT_I8 | DT_U8 -> i_stelem_i8
              | DT_R4 -> i_stelem_r4
              | DT_R8 -> i_stelem_r8
              | DT_REF -> i_stelem_ref
              | _ -> failwith "stelem")

        | I_ldelem dt ->
            emitInstrCode codebuf
              (match dt with
              | DT_I -> i_ldelem_i
              | DT_I1 -> i_ldelem_i1
              | DT_I2 -> i_ldelem_i2
              | DT_I4 -> i_ldelem_i4
              | DT_I8 -> i_ldelem_i8
              | DT_U1 -> i_ldelem_u1
              | DT_U2 -> i_ldelem_u2
              | DT_U4 -> i_ldelem_u4
              | DT_R4 -> i_ldelem_r4
              | DT_R8 -> i_ldelem_r8
              | DT_REF -> i_ldelem_ref
              | _ -> failwith "ldelem")

        | I_stind (al, vol, dt) ->
            emitAlignment codebuf al
            emitVolatility codebuf vol
            emitInstrCode codebuf
              (match dt with
              | DT_U | DT_I -> i_stind_i
              | DT_U1 | DT_I1 -> i_stind_i1
              | DT_U2 | DT_I2 -> i_stind_i2
              | DT_U4 | DT_I4 -> i_stind_i4
              | DT_U8 | DT_I8 -> i_stind_i8
              | DT_R4 -> i_stind_r4
              | DT_R8 -> i_stind_r8
              | DT_REF -> i_stind_ref
              | _ -> failwith "stelem")

        | I_switch labs -> codebuf.RecordReqdBrFixups (i_switch, None) labs

        | I_ldfld (al, vol, fspec) ->
            emitAlignment codebuf al
            emitVolatility codebuf vol
            emitFieldSpecInstr codebuf i_ldfld fspec
        | I_ldflda fspec ->
            emitFieldSpecInstr codebuf i_ldflda fspec
        | I_ldsfld (vol, fspec) ->
            emitVolatility codebuf vol
            emitFieldSpecInstr codebuf i_ldsfld fspec
        | I_ldsflda fspec ->
            emitFieldSpecInstr codebuf i_ldsflda fspec
        | I_stfld (al, vol, fspec) ->
            emitAlignment codebuf al
            emitVolatility codebuf vol
            emitFieldSpecInstr codebuf i_stfld fspec
        | I_stsfld (vol, fspec) ->
            emitVolatility codebuf vol
            emitFieldSpecInstr codebuf i_stsfld fspec

        | I_ldtoken (tbl, idx) ->
            emitInstrCode codebuf i_ldtoken
            codebuf.EmitUncodedToken (getUncodedToken (TableName.FromIndex tbl) idx)
        | I_ldstr (tbl, idx) ->
            emitInstrCode codebuf i_ldstr
            codebuf.EmitUncodedToken (getUncodedToken (TableName.FromIndex tbl) idx)

        | I_box ty -> emitTypeInstr codebuf i_box ty
        | I_unbox ty -> emitTypeInstr codebuf i_unbox ty
        | I_unbox_any ty -> emitTypeInstr codebuf i_unbox_any ty

        | I_newarr (shape, ty) ->
            if (shape = ILArrayShape.SingleDimensional) then
                emitTypeInstr codebuf i_newarr ty
            else
                ()
                // TODO RP multi dimensional arrays
                //let args = List.init shape.Rank (fun _ -> cenv.ilg.typ_Int32)
                //emitMethodSpecInfoInstr codebuf i_newobj (".ctor", mkILArrTy(ty, shape), ILCallingConv.Instance, args, ILType.Void, None, [])

        | I_stelem_any (shape, ty) ->
            if (shape = ILArrayShape.SingleDimensional) then
                emitTypeInstr codebuf i_stelem_any ty
            else
                ()
                // TODO RP multi dimensional arrays
                //let args = List.init (shape.Rank+1) (fun i -> if i < shape.Rank then cenv.ilg.typ_Int32 else ty)
                //emitMethodSpecInfoInstr codebuf i_call ("Set", mkILArrTy(ty, shape), ILCallingConv.Instance, args, ILType.Void, None, [])

        | I_ldelem_any (shape, ty) ->
            if (shape = ILArrayShape.SingleDimensional) then
                emitTypeInstr codebuf i_ldelem_any ty
            else
                ()
                // TODO RP multi dimensional arrays
                //let args = List.init shape.Rank (fun _ -> cenv.ilg.typ_Int32)
                //emitMethodSpecInfoInstr codebuf i_call ("Get", mkILArrTy(ty, shape), ILCallingConv.Instance, args, ty, None, [])

        | I_ldelema (ro, _isNativePtr, shape, ty) ->
            if (ro = ReadonlyAddress) then
                emitInstrCode codebuf i_readonly
            if (shape = ILArrayShape.SingleDimensional) then
                emitTypeInstr codebuf i_ldelema ty
            else
                ()
                // TODO RP multi dimensional arrays
                //let args = List.init shape.Rank (fun _ -> cenv.ilg.typ_Int32)
                //emitMethodSpecInfoInstr codebuf i_call ("Address", mkILArrTy(ty, shape), ILCallingConv.Instance, args, ILType.Byref ty, None, [])

        | I_castclass ty -> emitTypeInstr codebuf i_castclass ty
        | I_isinst ty -> emitTypeInstr codebuf i_isinst ty
        | I_refanyval ty -> emitTypeInstr codebuf i_refanyval ty
        | I_mkrefany ty -> emitTypeInstr codebuf i_mkrefany ty
        | I_initobj ty -> emitTypeInstr codebuf i_initobj ty
        | I_ldobj (al, vol, ty) ->
            emitAlignment codebuf al
            emitVolatility codebuf vol
            emitTypeInstr codebuf i_ldobj ty
        | I_stobj (al, vol, ty) ->
            emitAlignment codebuf al
            emitVolatility codebuf vol
            emitTypeInstr codebuf i_stobj ty
        | I_cpobj ty -> emitTypeInstr codebuf i_cpobj ty
        | I_sizeof ty -> emitTypeInstr codebuf i_sizeof ty
        | EI_ldlen_multi (_, m) ->
            ()
            // TODO RP no idea what this instruction means
            //emitShortInt32Instr codebuf (i_ldc_i4_s, i_ldc_i4) m
            //emitInstr codebuf (mkNormalCall(mkILNonGenericMethSpecInTy(cenv.ilg.typ_Array, ILCallingConv.Instance, "GetLength", [cenv.ilg.typ_Int32], cenv.ilg.typ_Int32)))

        | _ -> failwith "an IL instruction cannot be emitted"

    // Used to put local debug scopes and exception handlers into a tree form
    let rangeInsideRange (start_pc1, end_pc1) (start_pc2, end_pc2) =
      (start_pc1: int) >= start_pc2 && start_pc1 < end_pc2 &&
      (end_pc1: int) > start_pc2 && end_pc1 <= end_pc2

    let lranges_of_clause cl =
      match cl with
      | ILExceptionClause.Finally r1 -> [r1]
      | ILExceptionClause.Fault r1 -> [r1]
      | ILExceptionClause.FilterCatch (r1, r2) -> [r1;r2]
      | ILExceptionClause.TypeCatch (_ty, r1) -> [r1]


    let labelsToRange (lab2pc : Dictionary<ILCodeLabel, int>) p = let l1, l2 = p in lab2pc[l1], lab2pc[l2]

    let labelRangeInsideLabelRange lab2pc ls1 ls2 =
        rangeInsideRange (labelsToRange lab2pc ls1) (labelsToRange lab2pc ls2)

    let findRoots contains vs =
        // For each item, either make it a root or make it a child of an existing root
        let addToRoot roots x =
            // Look to see if 'x' is inside one of the roots
            let roots, found =
                (false, roots) ||> List.mapFold (fun found (r, children) ->
                    if found then ((r, children), true)
                    elif contains x r then ((r, x :: children), true)
                    else ((r, children), false))

            if found then roots
            else
                // Find the ones that 'x' encompasses and collapse them
                let yes, others = roots |> List.partition (fun (r, _) -> contains r x)
                (x, yes |> List.collect (fun (r, ch) -> r :: ch)) :: others

        ([], vs) ||> List.fold addToRoot

    let rec makeSEHTree (pc2pos: int[]) (lab2pc : Dictionary<ILCodeLabel, int>) (exs : ILExceptionSpec list) =

        let clause_inside_lrange cl lr =
          List.forall (fun lr1 -> labelRangeInsideLabelRange lab2pc lr1 lr) (lranges_of_clause cl)

        let tryspec_inside_lrange (tryspec1: ILExceptionSpec) lr =
          (labelRangeInsideLabelRange lab2pc tryspec1.Range lr && clause_inside_lrange tryspec1.Clause lr)

        let tryspec_inside_clause tryspec1 cl =
          List.exists (fun lr -> tryspec_inside_lrange tryspec1 lr) (lranges_of_clause cl)

        let tryspec_inside_tryspec tryspec1 (tryspec2: ILExceptionSpec) =
          tryspec_inside_lrange tryspec1 tryspec2.Range ||
          tryspec_inside_clause tryspec1 tryspec2.Clause

        let roots = findRoots tryspec_inside_tryspec exs
        let trees =
            roots |> List.map (fun (cl, ch) ->
                let r1 = labelsToRange lab2pc cl.Range
                let conv ((s1, e1), (s2, e2)) x = pc2pos[s1], pc2pos[e1] - pc2pos[s1], pc2pos[s2], pc2pos[e2] - pc2pos[s2], x
                let children = makeSEHTree pc2pos lab2pc ch
                let n =
                    match cl.Clause with
                    | ILExceptionClause.Finally r2 ->
                        conv (r1, labelsToRange lab2pc r2) ExceptionClauseKind.FinallyClause
                    | ILExceptionClause.Fault r2 ->
                        conv (r1, labelsToRange lab2pc r2) ExceptionClauseKind.FaultClause
                    | ILExceptionClause.FilterCatch ((filterStart, _), r3) ->
                        conv (r1, labelsToRange lab2pc r3) (ExceptionClauseKind.FilterClause pc2pos[lab2pc[filterStart]])
                    | ILExceptionClause.TypeCatch ((tbl, idx), r2) ->
                        conv (r1, labelsToRange lab2pc r2) (TypeFilterClause (getUncodedToken (TableName.FromIndex tbl) idx))
                SEHTree.Node (Some n, children) )

        trees

    // Emit the SEH tree
    let rec emitExceptionHandlerTree (codebuf: CodeBuffer) (Node (x, childSEH)) =
        List.iter (emitExceptionHandlerTree codebuf) childSEH // internal first
        x |> Option.iter codebuf.EmitExceptionClause

    let emitCode (codebuf: CodeBuffer) (code: ILCode) =
        let instrs = code.Instrs

        // Build a table mapping Abstract IL pcs to positions in the generated code buffer
        let pc2pos = Array.zeroCreate (instrs.Length+1)
        let pc2labs = Dictionary()
        for KeyValue (lab, pc) in code.Labels do
            match pc2labs.TryGetValue pc with
            | true, labels ->
                pc2labs[pc] <- lab :: labels
            | _ -> pc2labs[pc] <- [lab]

        // Emit the instructions
        for pc = 0 to instrs.Length do
            match pc2labs.TryGetValue pc with
            | true, labels ->
                for lab in labels do
                    codebuf.RecordAvailBrFixup lab
            | _ -> ()
            pc2pos[pc] <- codebuf.code.Position
            if pc < instrs.Length then
                match instrs[pc] with
                | I_br l when code.Labels[l] = pc + 1 -> () // compress I_br to next instruction
                | i -> emitInstr codebuf i

        // Build the exceptions and locals information, ready to emit
        let SEHTree = makeSEHTree pc2pos code.Labels code.Exceptions
        List.iter (emitExceptionHandlerTree codebuf) SEHTree

        // TODO RP I think this is only need for debug info
        // Build the locals information, ready to emit
        //let localsTree = makeLocalsTree cenv importScope localSigs pc2pos code.Labels code.Locals

        // Adjust the scopes for shadowing
        //let unshadowed = List.collect (unshadowScopes >> Array.toList) localsTree
        //unshadowed

    let EmitMethodCode nm code =
        use codebuf = CodeBuffer.Create nm
        emitCode codebuf code
        let origCode = codebuf.code.AsMemory().ToArray()
        let origExnClauses = List.rev codebuf.seh
        let origAvailBrFixups = codebuf.availBrFixups
        let origReqdBrFixups = codebuf.reqdBrFixups

        let newCode, newExnClauses =
            applyBrFixups origCode origExnClauses origAvailBrFixups origReqdBrFixups

        (newExnClauses, newCode)


let GenILMethodBody mname (il: ILMethodBody) =


    let seh, code = Codebuf.EmitMethodCode mname il.Code
    let codeSize = code.Length
    use methbuf = ByteBuffer.Create (codeSize * 3)
    // Do we use the tiny format?
    if Option.isNone il.Locals && il.MaxStack <= 8 && isNil seh && codeSize < 64 then
        // Use Tiny format
        let alignedCodeSize = align 4 (codeSize + 1)
        let codePadding = (alignedCodeSize - (codeSize + 1))
        methbuf.EmitByte (byte codeSize <<< 2 ||| e_CorILMethod_TinyFormat)
        methbuf.EmitBytes code
        methbuf.EmitPadding codePadding
        methbuf.AsMemory().ToArray()
    else
        // Use Fat format
        let flags =
            e_CorILMethod_FatFormat |||
            (if seh <> [] then e_CorILMethod_MoreSects else 0x0uy) |||
            (if il.IsZeroInit then e_CorILMethod_InitLocals else 0x0uy)

        let localToken =
            match il.Locals with
            | None -> 0x0
            | Some (tbl, idx) ->
                getUncodedToken (TableName.FromIndex tbl) idx

        let alignedCodeSize = align 0x4 codeSize
        let codePadding = (alignedCodeSize - codeSize)

        methbuf.EmitByte flags
        methbuf.EmitByte 0x30uy // last four bits record size of fat header in 4 byte chunks - this is always 12 bytes = 3 four word chunks
        methbuf.EmitUInt16 (uint16 il.MaxStack)
        methbuf.EmitInt32 codeSize
        methbuf.EmitInt32 localToken
        methbuf.EmitBytes code
        methbuf.EmitPadding codePadding

        if not (isNil seh) then
            // Can we use the small exception handling table format?
            let smallSize = (seh.Length * 12 + 4)
            let canUseSmall =
              smallSize <= 0xFF &&
              seh |> List.forall (fun (st1, sz1, st2, sz2, _) ->
                  st1 <= 0xFFFF && st2 <= 0xFFFF && sz1 <= 0xFF && sz2 <= 0xFF)

            let kindAsInt32 k =
              match k with
              | FinallyClause -> e_COR_ILEXCEPTION_CLAUSE_FINALLY
              | FaultClause -> e_COR_ILEXCEPTION_CLAUSE_FAULT
              | FilterClause _ -> e_COR_ILEXCEPTION_CLAUSE_FILTER
              | TypeFilterClause _ -> e_COR_ILEXCEPTION_CLAUSE_EXCEPTION
            let kindAsExtraInt32 k =
              match k with
              | FinallyClause | FaultClause -> 0x0
              | FilterClause i -> i
              | TypeFilterClause uncoded -> uncoded

            if canUseSmall then
                methbuf.EmitByte e_CorILMethod_Sect_EHTable
                methbuf.EmitByte (b0 smallSize)
                methbuf.EmitByte 0x00uy
                methbuf.EmitByte 0x00uy
                seh |> List.iter (fun (st1, sz1, st2, sz2, kind) ->
                    let k32 = kindAsInt32 kind
                    methbuf.EmitInt32AsUInt16 k32
                    methbuf.EmitInt32AsUInt16 st1
                    methbuf.EmitByte (b0 sz1)
                    methbuf.EmitInt32AsUInt16 st2
                    methbuf.EmitByte (b0 sz2)
                    methbuf.EmitInt32 (kindAsExtraInt32 kind))
            else
                let bigSize = (seh.Length * 24 + 4)
                methbuf.EmitByte (e_CorILMethod_Sect_EHTable ||| e_CorILMethod_Sect_FatFormat)
                methbuf.EmitByte (b0 bigSize)
                methbuf.EmitByte (b1 bigSize)
                methbuf.EmitByte (b2 bigSize)
                seh |> List.iter (fun (st1, sz1, st2, sz2, kind) ->
                    let k32 = kindAsInt32 kind
                    methbuf.EmitInt32 k32
                    methbuf.EmitInt32 st1
                    methbuf.EmitInt32 sz1
                    methbuf.EmitInt32 st2
                    methbuf.EmitInt32 sz2
                    methbuf.EmitInt32 (kindAsExtraInt32 kind))

        methbuf.AsMemory().ToArray()
