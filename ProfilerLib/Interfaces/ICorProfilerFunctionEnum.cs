namespace ProfilerLib.Interfaces;

[NativeObject]
public unsafe interface ICorProfilerFunctionEnum :  IUnknown
{
HResult Skip(
    /* [in] */ uint celt);

HResult Reset();

HResult Clone(
    /* [out] */ ICorProfilerFunctionEnum** ppEnum);

HResult GetCount(out uint pcelt);

HResult Next(
    /* [in] */ uint celt,
    /* [length_is][size_is][out] */ COR_PRF_FUNCTION* ids,
/* [out] */ uint* pceltFetched);

}