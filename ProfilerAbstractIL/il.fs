module ProfilerAbstractIL.IL

open System
open System.Collections.Generic
open System.Threading

type ILType = int * int
type ILToken = int * int
type ILMethodSpec = int * int
type ILFieldSpec = int * int
type ILCallingSignature = int * int

type ILTypes = uint 
type ILDebugPoint = uint

type ILGuid = byte[]

type ILArrayBound = int32 option

type ILArrayBounds = ILArrayBound * ILArrayBound

[<StructuralEquality; StructuralComparison>]
type ILArrayShape =

    | ILArrayShape of ILArrayBounds list (* lobound/size pairs *)

    member x.Rank = (let (ILArrayShape l) = x in l.Length)

    static member SingleDimensional = ILArrayShapeStatics.SingleDimensional

    static member FromRank n =
        if n = 1 then
            ILArrayShape.SingleDimensional
        else
            ILArrayShape(List.replicate n (Some 0, None))

and ILArrayShapeStatics() =

    static let singleDimensional = ILArrayShape [ (Some 0, None) ]

    static member SingleDimensional = singleDimensional


type ILCodeLabel = int

// --------------------------------------------------------------------
// Instruction set.
// --------------------------------------------------------------------

type ILBasicType =
    | DT_R
    | DT_I1
    | DT_U1
    | DT_I2
    | DT_U2
    | DT_I4
    | DT_U4
    | DT_I8
    | DT_U8
    | DT_R4
    | DT_R8
    | DT_I
    | DT_U
    | DT_REF

[<StructuralEquality; StructuralComparison; RequireQualifiedAccess>]
type ILConst =
    | I4 of int32
    | I8 of int64
    | R4 of single
    | R8 of double

type ILTailcall =
    | Tailcall
    | Normalcall

type ILAlignment =
    | Aligned
    | Unaligned1
    | Unaligned2
    | Unaligned4

type ILVolatility =
    | Volatile
    | Nonvolatile

type ILReadonly =
    | ReadonlyAddress
    | NormalAddress

type ILVarArgs = ILTypes option

[<StructuralEquality; StructuralComparison>]
type ILComparisonInstr =
    | BI_beq
    | BI_bge
    | BI_bge_un
    | BI_bgt
    | BI_bgt_un
    | BI_ble
    | BI_ble_un
    | BI_blt
    | BI_blt_un
    | BI_bne_un
    | BI_brfalse
    | BI_brtrue

[<StructuralEquality; NoComparison>]
type ILInstr =
    | AI_add
    | AI_add_ovf
    | AI_add_ovf_un
    | AI_and
    | AI_div
    | AI_div_un
    | AI_ceq
    | AI_cgt
    | AI_cgt_un
    | AI_clt
    | AI_clt_un
    | AI_conv of ILBasicType
    | AI_conv_ovf of ILBasicType
    | AI_conv_ovf_un of ILBasicType
    | AI_mul
    | AI_mul_ovf
    | AI_mul_ovf_un
    | AI_rem
    | AI_rem_un
    | AI_shl
    | AI_shr
    | AI_shr_un
    | AI_sub
    | AI_sub_ovf
    | AI_sub_ovf_un
    | AI_xor
    | AI_or
    | AI_neg
    | AI_not
    | AI_ldnull
    | AI_dup
    | AI_pop
    | AI_ckfinite
    | AI_nop
    | AI_ldc of ILBasicType * ILConst
    | I_ldarg of uint16
    | I_ldarga of uint16
    | I_ldind of ILAlignment * ILVolatility * ILBasicType
    | I_ldloc of uint16
    | I_ldloca of uint16
    | I_starg of uint16
    | I_stind of ILAlignment * ILVolatility * ILBasicType
    | I_stloc of uint16

    | I_br of ILCodeLabel
    | I_jmp of ILMethodSpec
    | I_brcmp of ILComparisonInstr * ILCodeLabel
    | I_switch of ILCodeLabel list
    | I_ret

    | I_call of ILTailcall * ILMethodSpec * ILVarArgs
    | I_callvirt of ILTailcall * ILMethodSpec * ILVarArgs
    | I_callconstraint of callvirt: bool * ILTailcall * ILType * ILMethodSpec * ILVarArgs
    | I_calli of ILTailcall * ILCallingSignature * ILVarArgs
    | I_ldftn of ILMethodSpec
    | I_newobj of ILMethodSpec * ILVarArgs

    | I_throw
    | I_endfinally
    | I_endfilter
    | I_leave of ILCodeLabel
    | I_rethrow

    | I_ldsfld of ILVolatility * ILFieldSpec
    | I_ldfld of ILAlignment * ILVolatility * ILFieldSpec
    | I_ldsflda of ILFieldSpec
    | I_ldflda of ILFieldSpec
    | I_stsfld of ILVolatility * ILFieldSpec
    | I_stfld of ILAlignment * ILVolatility * ILFieldSpec
    | I_ldstr of (int * int)
    | I_isinst of ILType
    | I_castclass of ILType
    | I_ldtoken of ILToken
    | I_ldvirtftn of ILMethodSpec

    | I_cpobj of ILType
    | I_initobj of ILType
    | I_ldobj of ILAlignment * ILVolatility * ILType
    | I_stobj of ILAlignment * ILVolatility * ILType
    | I_box of ILType
    | I_unbox of ILType
    | I_unbox_any of ILType
    | I_sizeof of ILType

    | I_ldelem of ILBasicType
    | I_stelem of ILBasicType
    | I_ldelema of ILReadonly * bool * ILArrayShape * ILType
    | I_ldelem_any of ILArrayShape * ILType
    | I_stelem_any of ILArrayShape * ILType
    | I_newarr of ILArrayShape * ILType
    | I_ldlen

    | I_mkrefany of ILType
    | I_refanytype
    | I_refanyval of ILType

    | I_break

    | I_arglist

    | I_localloc
    | I_cpblk of ILAlignment * ILVolatility
    | I_initblk of ILAlignment * ILVolatility

    (* FOR EXTENSIONS, e.g. MS-ILX *)
    | EI_ilzero of ILType
    | EI_ldlen_multi of int32 * int32

[<RequireQualifiedAccess>]
type ILExceptionClause =
    | Finally of (ILCodeLabel * ILCodeLabel)
    | Fault of (ILCodeLabel * ILCodeLabel)
    | FilterCatch of filterRange: (ILCodeLabel * ILCodeLabel) * handlerRange: (ILCodeLabel * ILCodeLabel)
    | TypeCatch of ILType * (ILCodeLabel * ILCodeLabel)

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ILExceptionSpec =
    {
        Range: ILCodeLabel * ILCodeLabel
        Clause: ILExceptionClause
    }

/// Indicates that a particular local variable has a particular source
/// language name within a given set of ranges. This does not effect local
/// variable numbering, which is global over the whole method.
[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ILLocalDebugMapping = { LocalIndex: int; LocalName: string }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ILLocalDebugInfo =
    {
        Range: ILCodeLabel * ILCodeLabel
        DebugMappings: ILLocalDebugMapping list
    }

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ILCode =
    {
        Labels: Dictionary<ILCodeLabel, int>
        Instrs: ILInstr[]
        Exceptions: ILExceptionSpec list
        Locals: ILLocalDebugInfo list
    }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ILLocal =
    {
        Type: ILType
        IsPinned: bool
        DebugInfo: (string * int * int) option
    }

type ILLocals = ILLocal list

[<RequireQualifiedAccess; NoEquality; NoComparison>]
type ILDebugImport =
    | ImportType of targetType: ILType // * alias: string option
    | ImportNamespace of targetNamespace: string // * assembly: ILAssemblyRef option * alias: string option

//| ReferenceAlias of string
//| OpenXmlNamespace of prefix: string * xmlNamespace: string

type ILDebugImports =
    {
        Parent: ILDebugImports option
        Imports: ILDebugImport[]
    }

[<NoEquality; NoComparison>]
type ILMethodBody =
    {
        IsZeroInit: bool
        MaxStack: int32
        Locals: (int * int) option
        Code: ILCode
        DebugRange: ILDebugPoint option
        DebugImports: ILDebugImports option
    }

[<RequireQualifiedAccess>]
type ILMemberAccess =
    | Assembly
    | CompilerControlled
    | FamilyAndAssembly
    | FamilyOrAssembly
    | Family
    | Private
    | Public

[<RequireQualifiedAccess; StructuralEquality; StructuralComparison>]
type ILFieldInit =
    | String of string
    | Bool of bool
    | Char of uint16
    | Int8 of int8
    | Int16 of int16
    | Int32 of int32
    | Int64 of int64
    | UInt8 of uint8
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64
    | Single of single
    | Double of double
    | Null

    member x.AsObject() =
        match x with
        | ILFieldInit.String s -> box s
        | ILFieldInit.Bool bool -> box bool
        | ILFieldInit.Char u16 -> box (char (int u16))
        | ILFieldInit.Int8 i8 -> box i8
        | ILFieldInit.Int16 i16 -> box i16
        | ILFieldInit.Int32 i32 -> box i32
        | ILFieldInit.Int64 i64 -> box i64
        | ILFieldInit.UInt8 u8 -> box u8
        | ILFieldInit.UInt16 u16 -> box u16
        | ILFieldInit.UInt32 u32 -> box u32
        | ILFieldInit.UInt64 u64 -> box u64
        | ILFieldInit.Single ieee32 -> box ieee32
        | ILFieldInit.Double ieee64 -> box ieee64
        | ILFieldInit.Null -> (null :> Object)

// --------------------------------------------------------------------
// Native Types, for marshalling to the native C interface.
// These are taken directly from the ILASM syntax, and don't really
// correspond yet to the CLI ECMA-335 Spec (Partition II, 7.4).
// --------------------------------------------------------------------

[<RequireQualifiedAccess; StructuralEquality; StructuralComparison>]
type ILNativeType =
    | Empty
    | Custom of ILGuid * nativeTypeName: string * custMarshallerName: string * cookieString: byte[]
    | FixedSysString of int32
    | FixedArray of int32
    | Currency
    | LPSTR
    | LPWSTR
    | LPTSTR
    | LPUTF8STR
    | ByValStr
    | TBSTR
    | LPSTRUCT
    | Struct
    | Void
    | Bool
    | Int8
    | Int16
    | Int32
    | Int64
    | Single
    | Double
    | Byte
    | UInt16
    | UInt32
    | UInt64
    | Array of
        ILNativeType option *
        (int32 * int32 option) option (* optional idx of parameter giving size plus optional additive i.e. num elems *)
    | Int
    | UInt
    | Method
    | AsAny
    | BSTR
    | IUnknown
    | IDispatch
    | Interface
    | Error
    | SafeArray of ILNativeVariant * string option
    | ANSIBSTR
    | VariantBool

and [<RequireQualifiedAccess; StructuralEquality; StructuralComparison>] ILNativeVariant =
    | Empty
    | Null
    | Variant
    | Currency
    | Decimal
    | Date
    | BSTR
    | LPSTR
    | LPWSTR
    | IUnknown
    | IDispatch
    | SafeArray
    | Error
    | HRESULT
    | CArray
    | UserDefined
    | Record
    | FileTime
    | Blob
    | Stream
    | Storage
    | StreamedObject
    | StoredObject
    | BlobObject
    | CF
    | CLSID
    | Void
    | Bool
    | Int8
    | Int16
    | Int32
    | Int64
    | Single
    | Double
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | PTR
    | Array of ILNativeVariant
    | Vector of ILNativeVariant
    | Byref of ILNativeVariant
    | Int
    | UInt

[<RequireQualifiedAccess; StructuralEquality; StructuralComparison>]
type ILSecurityAction =
    | Request
    | Demand
    | Assert
    | Deny
    | PermitOnly
    | LinkCheck
    | InheritCheck
    | ReqMin
    | ReqOpt
    | ReqRefuse
    | PreJitGrant
    | PreJitDeny
    | NonCasDemand
    | NonCasLinkDemand
    | NonCasInheritance
    | LinkDemandChoice
    | InheritanceDemandChoice
    | DemandChoice

/// Comment on common object cache sizes:
/// mkLdArg - I can't imagine any IL method we generate needing more than this
/// mkLdLoc - I tried 256, and there were LdLoc allocations left, so I upped it o 512. I didn't check again.
/// mkStLoc - it should be the same as LdLoc (where there's a LdLoc there must be a StLoc)
/// mkLdcInt32 - just a guess

let ldargs = [| for i in 0..128 -> I_ldarg(uint16 i) |]

let mkLdarg i =
    if 0us < i && i < uint16 ldargs.Length then
        ldargs[int i]
    else
        I_ldarg i

let mkLdarg0 = mkLdarg 0us

let ldlocs = [| for i in 0..512 -> I_ldloc(uint16 i) |]

let mkLdloc i =
    if 0us < i && i < uint16 ldlocs.Length then
        ldlocs[int i]
    else
        I_ldloc i

let stlocs = [| for i in 0..512 -> I_stloc(uint16 i) |]

let mkStloc i =
    if 0us < i && i < uint16 stlocs.Length then
        stlocs[int i]
    else
        I_stloc i

let ldi32s = [| for i in 0..256 -> AI_ldc(DT_I4, ILConst.I4 i) |]

let mkLdcInt32 i =
    if 0 < i && i < ldi32s.Length then
        ldi32s[i]
    else
        AI_ldc(DT_I4, ILConst.I4 i)


// --------------------------------------------------------------------
// Basic operations on code.
// --------------------------------------------------------------------

let formatCodeLabel (x: int) = "L" + string x

//  ++GLOBAL MUTABLE STATE (concurrency safe)
let codeLabelCount = ref 0
let generateCodeLabel () = Interlocked.Increment codeLabelCount

let instrIsRet i =
    match i with
    | I_ret -> true
    | _ -> false

let nonBranchingInstrsToCode instrs : ILCode =
    let instrs = Array.ofList instrs

    let instrs =
        if instrs.Length <> 0 && instrIsRet (Array.last instrs) then
            instrs
        else
            Array.append instrs [| I_ret |]

    {
        Labels = Dictionary()
        Instrs = instrs
        Exceptions = []
        Locals = []
    }

// REVIEW: this function shows up on performance traces. If we eliminated the last ILX->IL rewrites from the
// F# compiler we could get rid of this structured code representation from Abstract IL altogether and
// never convert F# code into this form.
let buildILCode (_methName: string) lab2pc instrs tryspecs localspecs : ILCode =
    {
        Labels = lab2pc
        Instrs = instrs
        Exceptions = tryspecs
        Locals = localspecs
    }
