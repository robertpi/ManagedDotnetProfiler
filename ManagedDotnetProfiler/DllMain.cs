﻿using System;
using System.Runtime.InteropServices;
using ProfilerLib;

namespace ManagedDotnetProfiler;

public class DllMain
{
    private static ClassFactory Instance;

    [UnmanagedCallersOnly(EntryPoint = "DllGetClassObject")]
    public static unsafe int DllGetClassObject(void* rclsid, void* riid, nint* ppv)
    {
        Log.WriteLine("DllGetClassObject");

        Instance = new ClassFactory(new CorProfiler());
        *ppv = Instance.IClassFactory;

        return 0;
    }
}