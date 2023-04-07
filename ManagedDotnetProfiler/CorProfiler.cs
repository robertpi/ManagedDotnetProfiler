using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using NativeObjects;
using ProfilerAbstractIL;
using ProfilerLib;
using IMetaDataEmit = ProfilerLib.IMetaDataEmit;
using IMetaDataImport = ProfilerLib.IMetaDataImport;

namespace ManagedDotnetProfiler
{
    internal unsafe class CorProfiler : CorProfilerCallback10Base
    {
        private static readonly string[] MethodNames = {
            "<Main>",
            "OnGet"
        };

        private readonly object _syncRoot = new();
        private readonly Dictionary<ModuleId, ModuleMetadata> _moduleMetadataTable = new();
        private readonly Dictionary<string, ModuleMetadata> _assemblyNameToModuleMetadataTable = new();

        protected override HResult Initialize(int iCorProfilerInfoVersion, bool isAttach)
        {
            if (iCorProfilerInfoVersion < 11)
            {
                return HResult.E_FAIL;
            }

            var eventMask =
                CorPrfMonitor.COR_PRF_MONITOR_MODULE_LOADS
                | CorPrfMonitor.COR_PRF_MONITOR_JIT_COMPILATION
                | CorPrfMonitor.COR_PRF_ENABLE_REJIT;

            Log.WriteLine("Setting event mask to " + eventMask);

            ICorProfilerInfo11.SetEventMask(eventMask);

            LookForRejits();

            return HResult.S_OK;
        }

        private bool IsTargetMethod(string methodName)
        {
            return MethodNames.Any(name => methodName.Contains(name));
        }
        private void LookForRejits()
        {
            ICorProfilerInfo4.EnumJITedFunctions2(out var ppEnum);

            var enumInvoker = new ICorProfilerFunctionEnumInvoker(ppEnum);

            var rejitModules = new List<ModuleId>();
            var rejitMethods = new List<MdMethodDef>();

            enumInvoker.GetCount(out var count);
            for (int i = 0; i < count; i++)
            {
                COR_PRF_FUNCTION function = default;
                uint fetched = 0;
                enumInvoker.Next(1, &function, &fetched);
                ICorProfilerInfo2.GetFunctionInfo(function.functionId, out var classId, out var moduleId, out var mdToken);
                var moduleMetadata = GetModuleMetadata(moduleId);
                var methodDef = new MdMethodDef(mdToken);
                var methodMetadata = moduleMetadata.GetMethodMetadata(methodDef);
                if (IsTargetMethod(methodMetadata.FullyQualifiedName))
                {
                    rejitModules.Add(moduleId);
                    rejitMethods.Add(methodDef);
                }
            }

            if (rejitMethods.Count > 0)
            {
                var thread = new Thread(() =>
                {
                    fixed (ModuleId* p1 = rejitModules.ToArray())
                    {
                        fixed (MdMethodDef* p2 = rejitMethods.ToArray())
                        {
                            ICorProfilerInfo4.RequestReJIT((uint)rejitMethods.Count, p1, p2);
                        }
                    }
                });
                thread.Start();
            }
        }

        protected override HResult ModuleLoadFinished(ModuleId moduleId, HResult hrStatus)
        {
            if (hrStatus == HResult.S_OK)
            {
                var moduleMetadata = CreateModuleMetaData(moduleId);
                lock (_syncRoot)
                {
                    _moduleMetadataTable[moduleId] = moduleMetadata;
                    _assemblyNameToModuleMetadataTable[moduleMetadata.AssemblyName] = moduleMetadata;
                }
            }

            return HResult.S_OK;
        }

        private ModuleMetadata GetModuleMetadata(ModuleId moduleId)
        {
            ModuleMetadata moduleMetadata;
            bool sucess = false;
            lock (_syncRoot)
            {
                sucess = _moduleMetadataTable.TryGetValue(moduleId, out moduleMetadata);
            }

            if (!sucess)
            {
                moduleMetadata = CreateModuleMetaData(moduleId);
                lock (_syncRoot)
                {
                    _moduleMetadataTable[moduleId] = moduleMetadata;
                    _assemblyNameToModuleMetadataTable[moduleMetadata.AssemblyName] = moduleMetadata;
                }
            }

            return moduleMetadata;
        }

        private ModuleMetadata CreateModuleMetaData(ModuleId moduleId)
        {
            var hresult = ICorProfilerInfo2.GetModuleMetaData(moduleId, CorOpenFlags.ofRead, KnownGuids.IMetaDataImport,
                out var ppOutImport);
            var metaDataImport = new IMetaDataImport(ppOutImport);

            hresult = ICorProfilerInfo2.GetModuleMetaData(moduleId, CorOpenFlags.ofRead, KnownGuids.IMetaDataAssemblyImport,
                out var ppOutAssemblyImport);
            var metaDataAssemblyImport = new IMetaDataAssemblyImport(ppOutAssemblyImport);

            hresult = ICorProfilerInfo2.GetModuleMetaData(moduleId, CorOpenFlags.ofRead | CorOpenFlags.ofWrite,
                KnownGuids.IMetaDataEmit, out var ppOutEmit);
            var metaDataEmit = new IMetaDataEmit(ppOutEmit);

            ICorProfilerInfo2.GetModuleInfo(moduleId, out _, 0, out uint moduleSize, null, out _);

            Span<char> moduleBuffer = stackalloc char[(int)moduleSize];

            nint baseAddress;
            AssemblyId assemblyId;

            fixed (char* p = moduleBuffer)
            {
                ICorProfilerInfo2.GetModuleInfo(moduleId, out baseAddress, moduleSize, out _, p, out assemblyId);
            }

            var moduleName = new string(moduleBuffer);

            var moduleMetadata = new ModuleMetadata(moduleId, metaDataImport, metaDataAssemblyImport, metaDataEmit, moduleName,
                assemblyId, baseAddress);

            // Log.WriteLine($"Module Loaded: {moduleMetadata.ModuleName} loaded at address {moduleMetadata.BaseAddress:x2}");
            // Log.WriteLine($"Assembly: {moduleMetadata.AssemblyName}, {moduleMetadata.AssemblyVersion}");

            return moduleMetadata;
        }

        protected override HResult JITCompilationStarted(FunctionId functionId, bool fIsSafeToBlock)
        {
            return JitShared(functionId);
        }

        private HResult JitShared(FunctionId functionId)
        {
            ICorProfilerInfo2.GetFunctionInfo(functionId, out var classId, out var moduleId, out var mdToken);

            var moduleMetadata = GetModuleMetadata(moduleId);

            if (moduleMetadata == null)
            {
                return HResult.S_OK;
            }

            var methodMetadata = moduleMetadata.GetMethodMetadata(new MdMethodDef(mdToken));

            if (IsTargetMethod(methodMetadata.FullyQualifiedName))
            {
                Log.WriteLine($"Starting rewrite {methodMetadata.FullyQualifiedName}");

                var methodBody = methodMetadata.GetParsedBody(ICorProfilerInfo2);

                var message = "Hello from profiler!";
                var mdString = moduleMetadata.DefineUserString(message);

                var mdMethodRef = moduleMetadata.GetMethodRef("System.Console", "System.Console", "WriteLine");

                var newMethodBody =
                    ILTransforms.Transform(instrs => ILTransforms.AddStringCall(mdString.Value, mdMethodRef.Value, instrs),
                        methodBody.ParsedMethodBody);
                methodMetadata.SetParsedBody(ICorProfilerInfo2, newMethodBody);
            }

            return HResult.S_OK;
        }

        protected override HResult ReJITCompilationStarted(FunctionId functionId, ReJITId rejitId, bool fIsSafeToBlock)
        {
            return JitShared(functionId);
        }
    }
}
