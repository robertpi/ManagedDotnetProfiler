namespace ProfilerLib.Interfaces;

[NativeObject]
public unsafe interface IMethodMalloc : IUnknown
{
    byte* Alloc(ulong cb);
}