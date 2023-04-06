using NativeObjects;

namespace ProfilerLib;

public struct IMethodMalloc
{
    private IMethodMallocInvoker _impl;

    public IMethodMalloc(IntPtr ptr)
    {
        _impl = new(ptr);
    }

    public unsafe byte* Alloc(ulong a0)
    {
        return _impl.Alloc(a0);
    }

}