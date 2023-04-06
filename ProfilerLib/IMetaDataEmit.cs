using NativeObjects;

namespace ProfilerLib;

public unsafe struct IMetaDataEmit
{
    private IMetaDataEmitInvoker _impl;

    public IMetaDataEmit(IntPtr ptr)
    {
        _impl = new(ptr);
    }

    public HResult DefineUserString(char* a0, uint a1, MdString* a2)
    {
        return _impl.DefineUserString(a0, a1, a2);
    }

}