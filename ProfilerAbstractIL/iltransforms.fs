module ProfilerAbstractIL.ILTransforms

open ProfilerAbstractIL.IL

// The following is just a toy transformation, it only works if they're no exception handlers.
// A considerable amount of effort is need to provide a set of robust transformations,
// but I believe it is possible!

type InstrsTransform = delegate of ILInstr[] -> ILInstr[]

let Transform (f: InstrsTransform) (body: ILMethodBody) = 
    { body with Code = { body.Code with Instrs = f.Invoke(body.Code.Instrs) } }

let AddStringCall stringToken methodToken ptr sigToken (instrs: ILInstr[]) =
    let strTable, strIdx = ILReader.i32ToUncodedToken stringToken
    let sigTable, sigIdx = ILReader.i32ToUncodedToken sigToken 
    let methodTable, methodIdx = ILReader.i32ToUncodedToken methodToken
    [|
        yield I_ldstr (strTable.Index, strIdx)
        yield AI_ldc(DT_I8, ILConst.I8 ptr)
        yield I_calli(ILTailcall.Normalcall, (sigTable.Index, sigIdx), None)
        yield I_call (ILTailcall.Normalcall, (methodTable.Index, methodIdx), None)
        yield! instrs
    |]

