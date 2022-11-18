module ProfilerAbstractIL.ILTransforms

open ProfilerAbstractIL.IL

type InstrsTransform = delegate of ILInstr[] -> ILInstr[]

let Transform (f: InstrsTransform) (body: ILMethodBody) = 
    { body with Code = { body.Code with Instrs = f.Invoke(body.Code.Instrs) } }

let AddStringCall stringToken methodToken (instrs: ILInstr[]) =
    let strTable, strIdx = ILReader.i32ToUncodedToken stringToken
    let methodTable, methodIdx = ILReader.i32ToUncodedToken methodToken
    [|
        yield I_ldstr (strTable.Index, strIdx)
        yield I_call (ILTailcall.Normalcall, (methodTable.Index, methodIdx), None)
        yield! instrs
    |]

