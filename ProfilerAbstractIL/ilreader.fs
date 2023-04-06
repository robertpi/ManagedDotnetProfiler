module ProfilerAbstractIL.ILReader

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Collections.Immutable
open System.Text

open ProfilerAbstractIL.Utilities.Library
open ProfilerAbstractIL.IL
open ProfilerAbstractIL.BinaryConstants
open ProfilerAbstractIL.Diagnostics
open ProfilerAbstractIL.IO

let logging = false

type private BinaryView = ReadOnlyByteMemory

[<NoEquality; NoComparison>]
type ILInstrPrefixesRegister =
    {
        mutable al: ILAlignment
        mutable tl: ILTailcall
        mutable vol: ILVolatility
        mutable ro: ILReadonly
        mutable constrained: ILType option
    }

let noPrefixes mk prefixes =
    if prefixes.al <> Aligned then
        failwith "an unaligned prefix is not allowed here"

    if prefixes.vol <> Nonvolatile then
        failwith "a volatile prefix is not allowed here"

    if prefixes.tl <> Normalcall then
        failwith "a tailcall prefix is not allowed here"

    if prefixes.ro <> NormalAddress then
        failwith "a readonly prefix is not allowed here"

    if prefixes.constrained <> None then
        failwith "a constrained prefix is not allowed here"

    mk

let volatileOrUnalignedPrefix mk prefixes =
    if prefixes.tl <> Normalcall then
        failwith "a tailcall prefix is not allowed here"

    if prefixes.constrained <> None then
        failwith "a constrained prefix is not allowed here"

    if prefixes.ro <> NormalAddress then
        failwith "a readonly prefix is not allowed here"

    mk (prefixes.al, prefixes.vol)

let volatilePrefix mk prefixes =
    if prefixes.al <> Aligned then
        failwith "an unaligned prefix is not allowed here"

    if prefixes.tl <> Normalcall then
        failwith "a tailcall prefix is not allowed here"

    if prefixes.constrained <> None then
        failwith "a constrained prefix is not allowed here"

    if prefixes.ro <> NormalAddress then
        failwith "a readonly prefix is not allowed here"

    mk prefixes.vol

let tailPrefix mk prefixes =
    if prefixes.al <> Aligned then
        failwith "an unaligned prefix is not allowed here"

    if prefixes.vol <> Nonvolatile then
        failwith "a volatile prefix is not allowed here"

    if prefixes.constrained <> None then
        failwith "a constrained prefix is not allowed here"

    if prefixes.ro <> NormalAddress then
        failwith "a readonly prefix is not allowed here"

    mk prefixes.tl

let constraintOrTailPrefix mk prefixes =
    if prefixes.al <> Aligned then
        failwith "an unaligned prefix is not allowed here"

    if prefixes.vol <> Nonvolatile then
        failwith "a volatile prefix is not allowed here"

    if prefixes.ro <> NormalAddress then
        failwith "a readonly prefix is not allowed here"

    mk (prefixes.constrained, prefixes.tl)

let readonlyPrefix mk prefixes =
    if prefixes.al <> Aligned then
        failwith "an unaligned prefix is not allowed here"

    if prefixes.vol <> Nonvolatile then
        failwith "a volatile prefix is not allowed here"

    if prefixes.tl <> Normalcall then
        failwith "a tailcall prefix is not allowed here"

    if prefixes.constrained <> None then
        failwith "a constrained prefix is not allowed here"

    mk prefixes.ro


[<NoEquality; NoComparison>]
type ILInstrDecoder =
    | I_u16_u8_instr of (ILInstrPrefixesRegister -> uint16 -> ILInstr)
    | I_u16_u16_instr of (ILInstrPrefixesRegister -> uint16 -> ILInstr)
    | I_none_instr of (ILInstrPrefixesRegister -> ILInstr)
    | I_i64_instr of (ILInstrPrefixesRegister -> int64 -> ILInstr)
    | I_i32_i32_instr of (ILInstrPrefixesRegister -> int32 -> ILInstr)
    | I_i32_i8_instr of (ILInstrPrefixesRegister -> int32 -> ILInstr)
    | I_r4_instr of (ILInstrPrefixesRegister -> single -> ILInstr)
    | I_r8_instr of (ILInstrPrefixesRegister -> double -> ILInstr)
    | I_field_instr of (ILInstrPrefixesRegister -> ILFieldSpec -> ILInstr)
    | I_method_instr of (ILInstrPrefixesRegister -> ILMethodSpec * ILVarArgs -> ILInstr)
    | I_unconditional_i32_instr of (ILInstrPrefixesRegister -> ILCodeLabel -> ILInstr)
    | I_unconditional_i8_instr of (ILInstrPrefixesRegister -> ILCodeLabel -> ILInstr)
    | I_conditional_i32_instr of (ILInstrPrefixesRegister -> ILCodeLabel -> ILInstr)
    | I_conditional_i8_instr of (ILInstrPrefixesRegister -> ILCodeLabel -> ILInstr)
    | I_string_instr of (ILInstrPrefixesRegister -> (int * int) -> ILInstr)
    | I_switch_instr of (ILInstrPrefixesRegister -> ILCodeLabel list -> ILInstr)
    | I_tok_instr of (ILInstrPrefixesRegister -> ILToken -> ILInstr)
    | I_sig_instr of (ILInstrPrefixesRegister -> ILCallingSignature * ILVarArgs -> ILInstr)
    | I_type_instr of (ILInstrPrefixesRegister -> ILType -> ILInstr)
    | I_invalid_instr

let mkStind dt =
    volatileOrUnalignedPrefix (fun (x, y) -> I_stind(x, y, dt))

let mkLdind dt =
    volatileOrUnalignedPrefix (fun (x, y) -> I_ldind(x, y, dt))

let instrs () =
    [
        i_ldarg_s, I_u16_u8_instr(noPrefixes mkLdarg)
        i_starg_s, I_u16_u8_instr(noPrefixes I_starg)
        i_ldarga_s, I_u16_u8_instr(noPrefixes I_ldarga)
        i_stloc_s, I_u16_u8_instr(noPrefixes mkStloc)
        i_ldloc_s, I_u16_u8_instr(noPrefixes mkLdloc)
        i_ldloca_s, I_u16_u8_instr(noPrefixes I_ldloca)
        i_ldarg, I_u16_u16_instr(noPrefixes mkLdarg)
        i_starg, I_u16_u16_instr(noPrefixes I_starg)
        i_ldarga, I_u16_u16_instr(noPrefixes I_ldarga)
        i_stloc, I_u16_u16_instr(noPrefixes mkStloc)
        i_ldloc, I_u16_u16_instr(noPrefixes mkLdloc)
        i_ldloca, I_u16_u16_instr(noPrefixes I_ldloca)
        i_stind_i, I_none_instr(mkStind DT_I)
        i_stind_i1, I_none_instr(mkStind DT_I1)
        i_stind_i2, I_none_instr(mkStind DT_I2)
        i_stind_i4, I_none_instr(mkStind DT_I4)
        i_stind_i8, I_none_instr(mkStind DT_I8)
        i_stind_r4, I_none_instr(mkStind DT_R4)
        i_stind_r8, I_none_instr(mkStind DT_R8)
        i_stind_ref, I_none_instr(mkStind DT_REF)
        i_ldind_i, I_none_instr(mkLdind DT_I)
        i_ldind_i1, I_none_instr(mkLdind DT_I1)
        i_ldind_i2, I_none_instr(mkLdind DT_I2)
        i_ldind_i4, I_none_instr(mkLdind DT_I4)
        i_ldind_i8, I_none_instr(mkLdind DT_I8)
        i_ldind_u1, I_none_instr(mkLdind DT_U1)
        i_ldind_u2, I_none_instr(mkLdind DT_U2)
        i_ldind_u4, I_none_instr(mkLdind DT_U4)
        i_ldind_r4, I_none_instr(mkLdind DT_R4)
        i_ldind_r8, I_none_instr(mkLdind DT_R8)
        i_ldind_ref, I_none_instr(mkLdind DT_REF)
        i_cpblk, I_none_instr(volatileOrUnalignedPrefix I_cpblk)
        i_initblk, I_none_instr(volatileOrUnalignedPrefix I_initblk)
        i_ldc_i8, I_i64_instr(noPrefixes (fun x -> (AI_ldc(DT_I8, ILConst.I8 x))))
        i_ldc_i4, I_i32_i32_instr(noPrefixes mkLdcInt32)
        i_ldc_i4_s, I_i32_i8_instr(noPrefixes mkLdcInt32)
        i_ldc_r4, I_r4_instr(noPrefixes (fun x -> (AI_ldc(DT_R4, ILConst.R4 x))))
        i_ldc_r8, I_r8_instr(noPrefixes (fun x -> (AI_ldc(DT_R8, ILConst.R8 x))))
        i_ldfld, I_field_instr(volatileOrUnalignedPrefix (fun (x, y) fspec -> I_ldfld(x, y, fspec)))
        i_stfld, I_field_instr(volatileOrUnalignedPrefix (fun (x, y) fspec -> I_stfld(x, y, fspec)))
        i_ldsfld, I_field_instr(volatilePrefix (fun x fspec -> I_ldsfld(x, fspec)))
        i_stsfld, I_field_instr(volatilePrefix (fun x fspec -> I_stsfld(x, fspec)))
        i_ldflda, I_field_instr(noPrefixes I_ldflda)
        i_ldsflda, I_field_instr(noPrefixes I_ldsflda)
        (i_call,
         I_method_instr(
             constraintOrTailPrefix (fun (c, tl) (mspec, y) ->
                 match c with
                 | Some ty -> I_callconstraint(false, tl, ty, mspec, y)
                 | None -> I_call(tl, mspec, y))
         ))
        i_ldftn, I_method_instr(noPrefixes (fun (mspec, _y) -> I_ldftn mspec))
        i_ldvirtftn, I_method_instr(noPrefixes (fun (mspec, _y) -> I_ldvirtftn mspec))
        i_newobj, I_method_instr(noPrefixes I_newobj)
        (i_callvirt,
         I_method_instr(
             constraintOrTailPrefix (fun (c, tl) (mspec, y) ->
                 match c with
                 | Some ty -> I_callconstraint(true, tl, ty, mspec, y)
                 | None -> I_callvirt(tl, mspec, y))
         ))
        i_leave_s, I_unconditional_i8_instr(noPrefixes (fun x -> I_leave x))
        i_br_s, I_unconditional_i8_instr(noPrefixes I_br)
        i_leave, I_unconditional_i32_instr(noPrefixes (fun x -> I_leave x))
        i_br, I_unconditional_i32_instr(noPrefixes I_br)
        i_brtrue_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_brtrue, x)))
        i_brfalse_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_brfalse, x)))
        i_beq_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_beq, x)))
        i_blt_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_blt, x)))
        i_blt_un_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_blt_un, x)))
        i_ble_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_ble, x)))
        i_ble_un_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_ble_un, x)))
        i_bgt_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_bgt, x)))
        i_bgt_un_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_bgt_un, x)))
        i_bge_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_bge, x)))
        i_bge_un_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_bge_un, x)))
        i_bne_un_s, I_conditional_i8_instr(noPrefixes (fun x -> I_brcmp(BI_bne_un, x)))
        i_brtrue, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_brtrue, x)))
        i_brfalse, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_brfalse, x)))
        i_beq, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_beq, x)))
        i_blt, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_blt, x)))
        i_blt_un, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_blt_un, x)))
        i_ble, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_ble, x)))
        i_ble_un, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_ble_un, x)))
        i_bgt, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_bgt, x)))
        i_bgt_un, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_bgt_un, x)))
        i_bge, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_bge, x)))
        i_bge_un, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_bge_un, x)))
        i_bne_un, I_conditional_i32_instr(noPrefixes (fun x -> I_brcmp(BI_bne_un, x)))
        i_ldstr, I_string_instr(noPrefixes I_ldstr)
        i_switch, I_switch_instr(noPrefixes I_switch)
        i_ldtoken, I_tok_instr(noPrefixes I_ldtoken)
        i_calli, I_sig_instr(tailPrefix (fun tl (x, y) -> I_calli(tl, x, y)))
        i_mkrefany, I_type_instr(noPrefixes I_mkrefany)
        i_refanyval, I_type_instr(noPrefixes I_refanyval)
        i_ldelema, I_type_instr(readonlyPrefix (fun ro x -> I_ldelema(ro, false, ILArrayShape.SingleDimensional, x)))
        i_ldelem_any, I_type_instr(noPrefixes (fun x -> I_ldelem_any(ILArrayShape.SingleDimensional, x)))
        i_stelem_any, I_type_instr(noPrefixes (fun x -> I_stelem_any(ILArrayShape.SingleDimensional, x)))
        i_newarr, I_type_instr(noPrefixes (fun x -> I_newarr(ILArrayShape.SingleDimensional, x)))
        i_castclass, I_type_instr(noPrefixes I_castclass)
        i_isinst, I_type_instr(noPrefixes I_isinst)
        i_unbox_any, I_type_instr(noPrefixes I_unbox_any)
        i_cpobj, I_type_instr(noPrefixes I_cpobj)
        i_initobj, I_type_instr(noPrefixes I_initobj)
        i_ldobj, I_type_instr(volatileOrUnalignedPrefix (fun (x, y) z -> I_ldobj(x, y, z)))
        i_stobj, I_type_instr(volatileOrUnalignedPrefix (fun (x, y) z -> I_stobj(x, y, z)))
        i_sizeof, I_type_instr(noPrefixes I_sizeof)
        i_box, I_type_instr(noPrefixes I_box)
        i_unbox, I_type_instr(noPrefixes I_unbox)
    ]

// The tables are delayed to avoid building them unnecessarily at startup
// Many applications of AbsIL (e.g. a compiler) don't need to read instructions.
let mutable oneByteInstrs = None
let mutable twoByteInstrs = None

let fillInstrs () =
    let oneByteInstrTable = Array.create 256 I_invalid_instr
    let twoByteInstrTable = Array.create 256 I_invalid_instr

    let addInstr (i, f) =
        if i > 0xff then
            assert (i >>>& 8 = 0xfe)
            let i = (i &&& 0xff)

            match twoByteInstrTable[i] with
            | I_invalid_instr -> ()
            | _ -> dprintn ("warning: duplicate decode entries for " + string i)

            twoByteInstrTable[i] <- f
        else
            match oneByteInstrTable[i] with
            | I_invalid_instr -> ()
            | _ -> dprintn ("warning: duplicate decode entries for " + string i)

            oneByteInstrTable[i] <- f

    for i in instrs () do
        addInstr i

    for x, mk in noArgInstrs.Force() do
        addInstr (x, I_none_instr(noPrefixes mk))

    oneByteInstrs <- Some oneByteInstrTable
    twoByteInstrs <- Some twoByteInstrTable

let rec getOneByteInstr i =
    match oneByteInstrs with
    | None ->
        fillInstrs ()
        getOneByteInstr i
    | Some t -> t[i]

let rec getTwoByteInstr i =
    match twoByteInstrs with
    | None ->
        fillInstrs ()
        getTwoByteInstr i
    | Some t -> t[i]

//---------------------------------------------------------------------
// Utilities.
//---------------------------------------------------------------------

let align alignment n =
    ((n + alignment - 0x1) / alignment) * alignment

let uncodedToken (tab: TableName) idx = ((tab.Index <<< 24) ||| idx)

let i32ToUncodedToken tok =
    let idx = tok &&& 0xffffff
    let tab = tok >>>& 24
    (TableName.FromIndex tab, idx)

[<Struct>]
type TaggedIndex<'T> =
    val tag: 'T
    val index: int32
    new(tag, index) = { tag = tag; index = index }

let uncodedTokenToTypeDefOrRefOrSpec (tab, tok) =
    let tag =
        if tab = TableNames.TypeDef then
            tdor_TypeDef
        elif tab = TableNames.TypeRef then
            tdor_TypeRef
        elif tab = TableNames.TypeSpec then
            tdor_TypeSpec
        else
            failwith "bad table in uncodedTokenToTypeDefOrRefOrSpec"

    TaggedIndex(tag, tok)

let uncodedTokenToMethodDefOrRef (tab, tok) =
    let tag =
        if tab = TableNames.Method then mdor_MethodDef
        elif tab = TableNames.MemberRef then mdor_MemberRef
        else failwith "bad table in uncodedTokenToMethodDefOrRef"

    TaggedIndex(tag, tok)

let (|TaggedIndex|) (x: TaggedIndex<'T>) = x.tag, x.index

let tokToTaggedIdx f nbits tok =
    let tagmask =
        if nbits = 1 then 1
        elif nbits = 2 then 3
        elif nbits = 3 then 7
        elif nbits = 4 then 15
        elif nbits = 5 then 31
        else failwith "too many nbits"

    let tag = tok &&& tagmask
    let idx = tok >>>& nbits
    TaggedIndex(f tag, idx)

let singleOfBits (x: int32) =
    BitConverter.ToSingle(BitConverter.GetBytes x, 0)

let doubleOfBits (x: int64) = BitConverter.Int64BitsToDouble x


let seekReadByte (mdv: BinaryView) addr = mdv[addr]
let seekReadBytes (mdv: BinaryView) addr len = mdv.ReadBytes(addr, len)
let seekReadInt32 (mdv: BinaryView) addr = mdv.ReadInt32 addr
let seekReadUInt16 (mdv: BinaryView) addr = mdv.ReadUInt16 addr

let seekReadByteAsInt32 mdv addr = int32 (seekReadByte mdv addr)

let seekReadInt64 mdv addr =
    let b0 = seekReadByte mdv addr
    let b1 = seekReadByte mdv (addr + 1)
    let b2 = seekReadByte mdv (addr + 2)
    let b3 = seekReadByte mdv (addr + 3)
    let b4 = seekReadByte mdv (addr + 4)
    let b5 = seekReadByte mdv (addr + 5)
    let b6 = seekReadByte mdv (addr + 6)
    let b7 = seekReadByte mdv (addr + 7)

    int64 b0
    ||| (int64 b1 <<< 8)
    ||| (int64 b2 <<< 16)
    ||| (int64 b3 <<< 24)
    ||| (int64 b4 <<< 32)
    ||| (int64 b5 <<< 40)
    ||| (int64 b6 <<< 48)
    ||| (int64 b7 <<< 56)

let seekReadUInt16AsInt32 mdv addr = int32 (seekReadUInt16 mdv addr)

let seekReadCompressedUInt32 mdv addr =
    let b0 = seekReadByte mdv addr

    if b0 <= 0x7Fuy then
        struct (int b0, addr + 1)
    elif b0 <= 0xBFuy then
        let b0 = b0 &&& 0x7Fuy
        let b1 = seekReadByteAsInt32 mdv (addr + 1)
        struct ((int b0 <<< 8) ||| int b1, addr + 2)
    else
        let b0 = b0 &&& 0x3Fuy
        let b1 = seekReadByteAsInt32 mdv (addr + 1)
        let b2 = seekReadByteAsInt32 mdv (addr + 2)
        let b3 = seekReadByteAsInt32 mdv (addr + 3)
        struct ((int b0 <<< 24) ||| (int b1 <<< 16) ||| (int b2 <<< 8) ||| int b3, addr + 4)

let seekReadSByte mdv addr = sbyte (seekReadByte mdv addr)
let seekReadSingle mdv addr = singleOfBits (seekReadInt32 mdv addr)
let seekReadDouble mdv addr = doubleOfBits (seekReadInt64 mdv addr)

let rec seekCountUtf8String mdv addr n =
    let c = seekReadByteAsInt32 mdv addr

    if c = 0 then
        n
    else
        seekCountUtf8String mdv (addr + 1) (n + 1)

let seekReadUTF8String (mdv: BinaryView) addr =
    let n = seekCountUtf8String mdv addr 0
    mdv.ReadUtf8String(addr, n)

let seekReadBlob mdv addr =
    let struct (len, addr) = seekReadCompressedUInt32 mdv addr
    seekReadBytes mdv addr len

let seekReadUserString mdv addr =
    let struct (len, addr) = seekReadCompressedUInt32 mdv addr
    let bytes = seekReadBytes mdv addr (len - 1)
    Encoding.Unicode.GetString(bytes, 0, bytes.Length)

let seekReadGuid mdv addr = seekReadBytes mdv addr 0x10

let seekReadUncodedToken mdv addr =
    i32ToUncodedToken (seekReadInt32 mdv addr)

let seekReadUInt16Adv mdv (addr: byref<int>) =
    let res = seekReadUInt16 mdv addr
    addr <- addr + 2
    res

let seekReadInt32Adv mdv (addr: byref<int>) =
    let res = seekReadInt32 mdv addr
    addr <- addr + 4
    res

let seekReadUInt16AsInt32Adv mdv (addr: byref<int>) =
    let res = seekReadUInt16AsInt32 mdv addr
    addr <- addr + 2
    res

let seekReadTaggedIdx f nbits big mdv (addr: byref<int>) =
    let tok =
        if big then
            seekReadInt32Adv mdv &addr
        else
            seekReadUInt16AsInt32Adv mdv &addr

    tokToTaggedIdx f nbits tok

let seekReadIdx big mdv (addr: byref<int>) =
    if big then
        seekReadInt32Adv mdv &addr
    else
        seekReadUInt16AsInt32Adv mdv &addr




let seekReadTopCode pev (sz: int) start =

    let labelsOfRawOffsets = Dictionary<_, _>(sz / 2)
    let ilOffsetsOfLabels = Dictionary<_, _>(sz / 2)

    let rawToLabel rawOffset =
        match labelsOfRawOffsets.TryGetValue rawOffset with
        | true, l -> l
        | _ ->
            let lab = generateCodeLabel ()
            labelsOfRawOffsets[rawOffset] <- lab
            lab

    let markAsInstructionStart rawOffset ilOffset =
        let lab = rawToLabel rawOffset
        ilOffsetsOfLabels[lab] <- ilOffset

    let ibuf = ResizeArray<_>(sz / 2)
    let mutable curr = 0

    let prefixes =
        {
            al = Aligned
            tl = Normalcall
            vol = Nonvolatile
            ro = NormalAddress
            constrained = None
        }

    let mutable lastb = 0x0
    let mutable lastb2 = 0x0
    let mutable b = 0x0

    let get () =
        lastb <- seekReadByteAsInt32 pev (start + curr)
        curr <- curr + 1

        b <-
            if lastb = 0xfe && curr < sz then
                lastb2 <- seekReadByteAsInt32 pev (start + curr)
                curr <- curr + 1
                lastb2
            else
                lastb

    let mutable seqPointsRemaining = []

    while curr < sz do
        // registering "+string !curr+" as start of an instruction")
        markAsInstructionStart curr ibuf.Count

        // Insert any sequence points into the instruction sequence
        while (match seqPointsRemaining with
               | (i, _tag) :: _rest when i <= curr -> true
               | _ -> false) do
            // Emitting one sequence point
            let _, tag = List.head seqPointsRemaining
            seqPointsRemaining <- List.tail seqPointsRemaining
            // TODO RP don't support sequence points
            // ibuf.Add(I_seqpoint tag)

        // Read the prefixes. Leave lastb and lastb2 holding the instruction byte(s)
        (prefixes.al <- Aligned
         prefixes.tl <- Normalcall
         prefixes.vol <- Nonvolatile
         prefixes.ro <- NormalAddress
         prefixes.constrained <- None
         get ()

         while curr < sz
               && lastb = 0xfe
               && (b = (i_constrained &&& 0xff)
                   || b = (i_readonly &&& 0xff)
                   || b = (i_unaligned &&& 0xff)
                   || b = (i_volatile &&& 0xff)
                   || b = (i_tail &&& 0xff)) do
             (if b = (i_unaligned &&& 0xff) then
                  let unal = seekReadByteAsInt32 pev (start + curr)
                  curr <- curr + 1

                  prefixes.al <-
                      if unal = 0x1 then
                          Unaligned1
                      elif unal = 0x2 then
                          Unaligned2
                      elif unal = 0x4 then
                          Unaligned4
                      else
                          (dprintn "bad alignment for unaligned"
                           Aligned)
              elif b = (i_volatile &&& 0xff) then
                  prefixes.vol <- Volatile
              elif b = (i_readonly &&& 0xff) then
                  prefixes.ro <- ReadonlyAddress
              elif b = (i_constrained &&& 0xff) then
                  let uncoded = seekReadUncodedToken pev (start + curr)
                  curr <- curr + 4

                  // TODO RP
                  //let ty =
                  //    seekReadTypeDefOrRef ctxt numTypars AsObject [] (uncodedTokenToTypeDefOrRefOrSpec uncoded)

                  prefixes.constrained <- None // Some ty
              else
                  prefixes.tl <- Tailcall)

             get ())

        // data for instruction begins at "+string !curr
        // Read and decode the instruction
        if (curr <= sz) then
            let idecoder =
                if lastb = 0xfe then
                    getTwoByteInstr lastb2
                else
                    getOneByteInstr lastb

            let instr =
                match idecoder with
                | I_u16_u8_instr f ->
                    let x = seekReadByte pev (start + curr) |> uint16
                    curr <- curr + 1
                    f prefixes x
                | I_u16_u16_instr f ->
                    let x = seekReadUInt16 pev (start + curr)
                    curr <- curr + 2
                    f prefixes x
                | I_none_instr f -> f prefixes
                | I_i64_instr f ->
                    let x = seekReadInt64 pev (start + curr)
                    curr <- curr + 8
                    f prefixes x
                | I_i32_i8_instr f ->
                    let x = seekReadSByte pev (start + curr) |> int32
                    curr <- curr + 1
                    f prefixes x
                | I_i32_i32_instr f ->
                    let x = seekReadInt32 pev (start + curr)
                    curr <- curr + 4
                    f prefixes x
                | I_r4_instr f ->
                    let x = seekReadSingle pev (start + curr)
                    curr <- curr + 4
                    f prefixes x
                | I_r8_instr f ->
                    let x = seekReadDouble pev (start + curr)
                    curr <- curr + 8
                    f prefixes x
                | I_field_instr f ->
                    let tab, tok = seekReadUncodedToken pev (start + curr)
                    curr <- curr + 4

                    f prefixes (tab.Index, tok)
                | I_method_instr f ->
                    // method instruction, curr = "+string !curr

                    let tab, idx = seekReadUncodedToken pev (start + curr)
                    curr <- curr + 4

                    // TODO RP arrays / var args not handled
                    //let (VarArgMethodData (enclTy, cc, nm, argTys, varargs, retTy, methInst)) =
                    //    if tab = TableNames.Method then
                    //        seekReadMethodDefOrRef ctxt numTypars (TaggedIndex(mdor_MethodDef, idx))
                    //    elif tab = TableNames.MemberRef then
                    //        seekReadMethodDefOrRef ctxt numTypars (TaggedIndex(mdor_MemberRef, idx))
                    //    elif tab = TableNames.MethodSpec then
                    //        seekReadMethodSpecAsMethodData ctxt numTypars idx
                    //    else
                    //        failwith "bad table in MethodDefOrRefOrSpec"

                    //match enclTy with
                    //| ILType.Array (shape, ty) ->
                    //    match nm with
                    //    | "Get" -> I_ldelem_any(shape, ty)
                    //    | "Set" -> I_stelem_any(shape, ty)
                    //    | "Address" -> I_ldelema(prefixes.ro, false, shape, ty)
                    //    | ".ctor" -> I_newarr(shape, ty)
                    //    | _ -> failwith "bad method on array type"
                    //| _ ->
                    //    let mspec = mkILMethSpecInTy (enclTy, cc, nm, argTys, retTy, methInst)

                    f prefixes ((tab.Index, idx), None)
                | I_type_instr f ->
                    let tab, idx = seekReadUncodedToken pev (start + curr)
                    curr <- curr + 4

                    f prefixes (tab.Index, idx)
                | I_string_instr f ->
                    let tab, idx = seekReadUncodedToken pev (start + curr)
                    curr <- curr + 4

                    if tab <> TableNames.UserStrings then
                        dprintn "warning: bad table in user string for ldstr"

                    f prefixes (tab.Index, idx)

                | I_conditional_i32_instr f ->
                    let offsDest = (seekReadInt32 pev (start + curr))
                    curr <- curr + 4
                    let dest = curr + offsDest
                    f prefixes (rawToLabel dest)
                | I_conditional_i8_instr f ->
                    let offsDest = int (seekReadSByte pev (start + curr))
                    curr <- curr + 1
                    let dest = curr + offsDest
                    f prefixes (rawToLabel dest)
                | I_unconditional_i32_instr f ->
                    let offsDest = (seekReadInt32 pev (start + curr))
                    curr <- curr + 4
                    let dest = curr + offsDest
                    f prefixes (rawToLabel dest)
                | I_unconditional_i8_instr f ->
                    let offsDest = int (seekReadSByte pev (start + curr))
                    curr <- curr + 1
                    let dest = curr + offsDest
                    f prefixes (rawToLabel dest)
                | I_invalid_instr ->
                    dprintn (
                        "invalid instruction: "
                        + string lastb
                        + (if lastb = 0xfe then ", " + string lastb2 else "")
                    )

                    I_ret
                | I_tok_instr f ->
                    let tab, idx = seekReadUncodedToken pev (start + curr)
                    curr <- curr + 4

                    f prefixes (tab.Index, idx)
                | I_sig_instr f ->
                    let tab, idx = seekReadUncodedToken pev (start + curr)
                    curr <- curr + 4

                    f prefixes ((tab.Index, idx), None)
                | I_switch_instr f ->
                    let n = (seekReadInt32 pev (start + curr))
                    curr <- curr + 4

                    let offsets =
                        List.init n (fun _ ->
                            let i = (seekReadInt32 pev (start + curr))
                            curr <- curr + 4
                            i)

                    let dests = List.map (fun offs -> rawToLabel (curr + offs)) offsets
                    f prefixes dests

            ibuf.Add instr
    // Finished reading instructions - mark the end of the instruction stream in case the PDB information refers to it.
    markAsInstructionStart curr ibuf.Count
    // Build the function that maps from raw labels (offsets into the bytecode stream) to indexes in the AbsIL instruction stream
    let lab2pc = ilOffsetsOfLabels
    let instrs = ibuf.ToArray()

    instrs, rawToLabel, lab2pc

let seekReadMethodRVA pev nm =
        let baseRVA = 0
        // ": reading body of method "+nm+" at rva "+string rva+", phys "+string baseRVA
        let b = seekReadByte pev baseRVA

        let isTinyFormat = (b &&& e_CorILMethod_FormatMask) = e_CorILMethod_TinyFormat
        let isFatFormat = (b &&& e_CorILMethod_FormatMask) = e_CorILMethod_FatFormat

        if not isTinyFormat && not isFatFormat then
            failwith "unknown format"
        else

                    // Read any debug information for this method into temporary data structures
                    //    -- a list of locals, marked with the raw offsets (actually closures which accept the resolution function that maps raw offsets to labels)
                    //    -- an overall range for the method
                    //    -- the sequence points for the method
                    if isTinyFormat then
                        let codeBase = baseRVA + 1
                        let codeSize = (int32 b >>>& 2)
                        // tiny format for "+nm+", code size = " + string codeSize)
                        let instrs, _, lab2pc = seekReadTopCode pev codeSize codeBase

                        // Convert the linear code format to the nested code format
                        let code = buildILCode nm lab2pc instrs [] []

                        {
                            IsZeroInit = false
                            MaxStack = 8
                            Locals = None
                            Code = code
                            DebugRange = None
                            DebugImports = None
                        }

                    else
                        let hasMoreSections = (b &&& e_CorILMethod_MoreSects) <> 0x0uy
                        let initlocals = (b &&& e_CorILMethod_InitLocals) <> 0x0uy
                        let maxstack = seekReadUInt16AsInt32 pev (baseRVA + 2)
                        let codeSize = seekReadInt32 pev (baseRVA + 4)
                        let localsTab, localToken = seekReadUncodedToken pev (baseRVA + 8)
                        let codeBase = baseRVA + 12

                        let locals =
                            if localToken = 0x0 then
                                None
                            else
                                Some (localsTab.Index, localToken)

                        // fat format for "+nm+", code size = " + string codeSize+", hasMoreSections = "+(if hasMoreSections then "true" else "false")+", b = "+string b)

                        // Read the method body
                        let instrs, rawToLabel, lab2pc =
                            seekReadTopCode pev codeSize codeBase

                        // Read all the sections that follow the method body.
                        // These contain the exception clauses.
                        let mutable nextSectionBase = align 4 (codeBase + codeSize)
                        let mutable moreSections = hasMoreSections
                        let mutable seh = []

                        while moreSections do
                            let sectionBase = nextSectionBase
                            let sectionFlag = seekReadByte pev sectionBase
                            // fat format for "+nm+", sectionFlag = " + string sectionFlag)
                            let sectionSize, clauses =
                                if (sectionFlag &&& e_CorILMethod_Sect_FatFormat) <> 0x0uy then
                                    let bigSize = (seekReadInt32 pev sectionBase) >>>& 8
                                    // bigSize = "+string bigSize)
                                    let clauses =
                                        if (sectionFlag &&& e_CorILMethod_Sect_EHTable) <> 0x0uy then
                                            // WORKAROUND: The ECMA spec says this should be
                                            // let numClauses = ((bigSize - 4)  / 24) in
                                            // but the CCI IL generator generates multiples of 24
                                            let numClauses = (bigSize / 24)

                                            List.init numClauses (fun i ->
                                                let clauseBase = sectionBase + 4 + (i * 24)
                                                let kind = seekReadInt32 pev (clauseBase + 0)
                                                let st1 = seekReadInt32 pev (clauseBase + 4)
                                                let sz1 = seekReadInt32 pev (clauseBase + 8)
                                                let st2 = seekReadInt32 pev (clauseBase + 12)
                                                let sz2 = seekReadInt32 pev (clauseBase + 16)
                                                let extra = seekReadInt32 pev (clauseBase + 20)
                                                (kind, st1, sz1, st2, sz2, extra))
                                        else
                                            []

                                    bigSize, clauses
                                else
                                    let smallSize = seekReadByteAsInt32 pev (sectionBase + 0x01)

                                    let clauses =
                                        if (sectionFlag &&& e_CorILMethod_Sect_EHTable) <> 0x0uy then
                                            // WORKAROUND: The ECMA spec says this should be
                                            // let numClauses = ((smallSize - 4)  / 12) in
                                            // but the C# compiler (or some IL generator) generates multiples of 12
                                            let numClauses = (smallSize / 12)
                                            // dprintn (nm+" has " + string numClauses + " tiny seh clauses")
                                            List.init numClauses (fun i ->
                                                let clauseBase = sectionBase + 4 + (i * 12)
                                                let kind = seekReadUInt16AsInt32 pev (clauseBase + 0)

                                                if logging then
                                                    dprintn ("One tiny SEH clause, kind = " + string kind)

                                                let st1 = seekReadUInt16AsInt32 pev (clauseBase + 2)
                                                let sz1 = seekReadByteAsInt32 pev (clauseBase + 4)
                                                let st2 = seekReadUInt16AsInt32 pev (clauseBase + 5)
                                                let sz2 = seekReadByteAsInt32 pev (clauseBase + 7)
                                                let extra = seekReadInt32 pev (clauseBase + 8)
                                                (kind, st1, sz1, st2, sz2, extra))
                                        else
                                            []

                                    smallSize, clauses

                            // Morph together clauses that cover the same range
                            let sehClauses =
                                let sehMap = Dictionary<_, _>(clauses.Length, HashIdentity.Structural)

                                for (kind, st1, sz1, st2, sz2, extra) in clauses do
                                    let tryStart = rawToLabel st1
                                    let tryFinish = rawToLabel (st1 + sz1)
                                    let handlerStart = rawToLabel st2
                                    let handlerFinish = rawToLabel (st2 + sz2)

                                    let clause =
                                        let tab, idx = i32ToUncodedToken extra
                                        if kind = e_COR_ILEXCEPTION_CLAUSE_EXCEPTION then
                                            ILExceptionClause.TypeCatch(
                                                (tab.Index, idx),
                                                (handlerStart, handlerFinish)
                                            )
                                        elif kind = e_COR_ILEXCEPTION_CLAUSE_FILTER then
                                            let filterStart = rawToLabel extra
                                            let filterFinish = handlerStart
                                            ILExceptionClause.FilterCatch((filterStart, filterFinish), (handlerStart, handlerFinish))
                                        elif kind = e_COR_ILEXCEPTION_CLAUSE_FINALLY then
                                            ILExceptionClause.Finally(handlerStart, handlerFinish)
                                        elif kind = e_COR_ILEXCEPTION_CLAUSE_FAULT then
                                            ILExceptionClause.Fault(handlerStart, handlerFinish)
                                        else
                                            (dprintn ("unknown exception handler kind: " + string kind)
                                             ILExceptionClause.Finally(handlerStart, handlerFinish))

                                    let key = (tryStart, tryFinish)

                                    match sehMap.TryGetValue key with
                                    | true, prev -> sehMap[key] <- prev @ [ clause ]
                                    | _ -> sehMap[key] <- [ clause ]

                                ([], sehMap)
                                ||> Seq.fold (fun acc (KeyValue (key, bs)) ->
                                    [ for b in bs -> { Range = key; Clause = b }: ILExceptionSpec ] @ acc)

                            seh <- sehClauses
                            moreSections <- (sectionFlag &&& e_CorILMethod_Sect_MoreSects) <> 0x0uy
                            nextSectionBase <- sectionBase + sectionSize

                        let code = buildILCode nm lab2pc instrs seh []

                        if logging then
                            dprintn "done checking code."

                        {
                            IsZeroInit = initlocals
                            MaxStack = maxstack
                            Locals = locals
                            Code = code
                            DebugRange = None
                            DebugImports = None
                        }

