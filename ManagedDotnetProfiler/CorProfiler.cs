using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using ProfilerAbstractIL;
using ProfilerLib;

namespace ManagedDotnetProfiler
{
    internal unsafe class CorProfiler : CorProfilerCallback10Base
    {
        private readonly object _syncRoot = new();
        private readonly Dictionary<ModuleId, ModuleMetadata> _moduleMetadataTable = new();
        private readonly Dictionary<string, ModuleMetadata> _assemblyNameToModuleMetadataTable = new();
        protected override HResult Initialize(int iCorProfilerInfoVersion)
        {
            if (iCorProfilerInfoVersion < 11)
            {
                return HResult.E_FAIL;
            }

            var eventMask = CorPrfMonitor.COR_PRF_MONITOR_MODULE_LOADS | CorPrfMonitor.COR_PRF_MONITOR_JIT_COMPILATION;

            Log.WriteLine("Setting event mask to " + eventMask);

            return ICorProfilerInfo11.SetEventMask(eventMask);
        }

        protected override HResult ModuleLoadFinished(ModuleId moduleId, HResult hrStatus)
        {
            if (hrStatus == HResult.S_OK)
            {
                var hresult = ICorProfilerInfo2.GetModuleMetaData(moduleId, CorOpenFlags.ofRead, KnownGuids.IMetaDataImport, out var ppOutImport);
                var metaDataImport = new IMetaDataImport(ppOutImport);

                hresult = ICorProfilerInfo2.GetModuleMetaData(moduleId, CorOpenFlags.ofRead, KnownGuids.IMetaDataAssemblyImport, out var ppOutAssemblyImport);
                var metaDataAssemblyImport = new IMetaDataAssemblyImport(ppOutAssemblyImport);

                hresult = ICorProfilerInfo2.GetModuleMetaData(moduleId, CorOpenFlags.ofRead | CorOpenFlags.ofWrite, KnownGuids.IMetaDataEmit, out var ppOutEmit);
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

                var moduleMetadata = new ModuleMetadata(moduleId, metaDataImport, metaDataAssemblyImport, metaDataEmit, moduleName, assemblyId, baseAddress);

                // Log.WriteLine($"Module Loaded: {moduleMetadata.ModuleName} loaded at address {moduleMetadata.BaseAddress:x2}");
                // Log.WriteLine($"Assembly: {moduleMetadata.AssemblyName}, {moduleMetadata.AssemblyVersion}");

                lock (_syncRoot)
                {
                    _moduleMetadataTable.Add(moduleId, moduleMetadata);
                    _assemblyNameToModuleMetadataTable.Add(moduleMetadata.AssemblyName, moduleMetadata);
                }

            }

            return HResult.S_OK;
        }

        protected override HResult JITCompilationStarted(FunctionId functionId, bool fIsSafeToBlock)
        {
            ICorProfilerInfo2.GetFunctionInfo(functionId, out var classId, out var moduleId, out var mdToken);

            ModuleMetadata moduleMetadata;
            lock(_syncRoot)
            {
                _moduleMetadataTable.TryGetValue(moduleId, out moduleMetadata);
            }

            if (moduleMetadata == null)
            {
                return HResult.S_OK;
            }

            var methodMetadata =  moduleMetadata.GetMethodMetadata(new MdMethodDef(mdToken));


            if (methodMetadata.FullyQualifiedName.Contains("<Main>"))
            {
                Log.WriteLine($"{methodMetadata.FullyQualifiedName}");

                var methodBody = methodMetadata.GetParsedBody(ICorProfilerInfo2);

                var message = "Hello from profiler!";
                var mdString = moduleMetadata.DefineUserString(message);

                var mdMethodRef = moduleMetadata.GetMethodRef("System.Console", "System.Console", "WriteLine");

                var newMethodBody = ILTransforms.Transform(instrs => ILTransforms.AddStringCall(mdString.Value, mdMethodRef.Value, instrs), methodBody.ParsedMethodBody);
                methodMetadata.SetParsedBody(ICorProfilerInfo2, newMethodBody);
            }
            return HResult.S_OK;
        }
    }
}
