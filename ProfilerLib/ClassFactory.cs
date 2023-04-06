using System;
using ProfilerLib.Interfaces;

namespace ProfilerLib;

public unsafe class ClassFactory : Interfaces.IClassFactory
{
    private NativeObjects.IClassFactory _classFactory;

    private CorProfilerCallbackBase _corProfilerCallback;

    public ClassFactory(CorProfilerCallbackBase corProfilerCallback)
    {
        _classFactory = NativeObjects.IClassFactory.Wrap(this);
        _corProfilerCallback = corProfilerCallback;
    }

    public IntPtr IClassFactory => _classFactory;

    public HResult CreateInstance(IntPtr outer, in Guid guid, out IntPtr instance)
    {
        Log.WriteLine("ClassFactory - CreateInstance - " + guid);

        instance = _corProfilerCallback.ICorProfilerCallback;
        return HResult.S_OK;

        // return _callback.IUnknown.QueryInterface(_callback.IUnknownObject, guid, out instance);
    }

    public HResult LockServer(bool @lock)
    {
        Log.WriteLine("ClassFactory - LockServer");
        return default;
    }

    public HResult QueryInterface(in Guid guid, out IntPtr ptr)
    {
        Log.WriteLine("ClassFactory - QueryInterface - " + guid);

        if (guid == KnownGuids.ClassFactoryGuid)
        {
            ptr = IClassFactory;
            return HResult.S_OK;
        }

        ptr = IntPtr.Zero;
        return HResult.E_NOTIMPL;
    }

    public int AddRef()
    {
        Log.WriteLine("ClassFactory - AddRef");
        return 1;
    }

    public int Release()
    {
        Log.WriteLine("ClassFactory - Release");
        return 0;
    }
}
