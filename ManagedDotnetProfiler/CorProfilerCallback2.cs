using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;
using ProfilerLib;
using ProfilerAbstractIL;
using ProfilerAbstractIL.IO;
using ProfilerAbstractIL.Utilities.Library;

namespace ManagedDotnetProfiler
{
    public unsafe class CorProfilerCallback2 : CorProfilerCallback2Base
    {
        private static readonly Guid ICorProfilerCallback2Guid = Guid.Parse("8a8cc829-ccf2-49fe-bbae-0f022228071a");

        private readonly object _syncRoot = new();
        private readonly Dictionary<ModuleId, ModuleMetadata> _moduleMetadataTable = new();
        private readonly Dictionary<string, ModuleMetadata> _assemblyNameToModuleMetadataTable = new();

        private ICorProfilerInfo3 _corProfilerInfo;

        protected override HResult Initialize(int iCorProfilerInfoVersion)
        {
            if (iCorProfilerInfoVersion < 11)
            {
                return HResult.E_FAIL;
            }

            var eventMask = CorPrfMonitor.COR_PRF_MONITOR_EXCEPTIONS | CorPrfMonitor.COR_PRF_MONITOR_JIT_COMPILATION;

            Console.WriteLine("[Profiler] Setting event mask to " + eventMask);

            return ICorProfilerInfo11.SetEventMask(eventMask);
        }


        protected override HResult ModuleLoadFinished(ModuleId moduleId, HResult hrStatus)
        {
            if (hrStatus == HResult.S_OK)
            {
                var hresult = _corProfilerInfo.GetModuleMetaData(moduleId, CorOpenFlags.ofRead, KnownGuids.IMetaDataImport, out var ppOutImport);
                IMetaDataImport metaDataImport = NativeStubs.IMetaDataImportStub.Wrap((IntPtr)ppOutImport);

                hresult = _corProfilerInfo.GetModuleMetaData(moduleId, CorOpenFlags.ofRead, KnownGuids.IMetaDataAssemblyImport, out var ppOutAssemblyImport);
                IMetaDataAssemblyImport metaDataAssemblyImport = NativeStubs.IMetaDataAssemblyImportStub.Wrap((IntPtr)ppOutAssemblyImport);

                hresult = _corProfilerInfo.GetModuleMetaData(moduleId, CorOpenFlags.ofRead | CorOpenFlags.ofRead, KnownGuids.IMetaDataEmit, out var ppOutEmit);
                IMetaDataEmit metaDataEmit = NativeStubs.IMetaDataEmitStub.Wrap((IntPtr)ppOutEmit);

                _corProfilerInfo.GetModuleInfo(moduleId, out _, 0, out uint moduleSize, null, out _);

                Span<char> moduleBuffer = stackalloc char[(int)moduleSize];

                nint baseAddress;
                AssemblyId assemblyId;

                fixed (char* p = moduleBuffer)
                {
                    _corProfilerInfo.GetModuleInfo(moduleId, out baseAddress, moduleSize, out _, p, out assemblyId);
                }

                var moduleName = new string(moduleBuffer);

                var moduleMetadata = new ModuleMetadata(moduleId, metaDataImport, metaDataAssemblyImport, metaDataEmit, moduleName, assemblyId, baseAddress);

                Console.WriteLine($"Module Loaded: {moduleMetadata.ModuleName} loaded at address {moduleMetadata.BaseAddress:x2}");
                Console.WriteLine($"Assembly: {moduleMetadata.AssemblyName}, {moduleMetadata.AssemblyVersion}");

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
            _corProfilerInfo.GetFunctionInfo(functionId, out var classId, out var moduleId, out var mdToken);

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

            // Console.WriteLine("methodMetadata.FullyQualifiedName: " + methodMetadata.FullyQualifiedName);


            if (methodMetadata.FullyQualifiedName.Contains("<Main>"))
            {
                var methodBody = methodMetadata.GetParsedBody(_corProfilerInfo);

                var message = "Hello from profiler!";
                var mdString = moduleMetadata.DefineUserString(message);

                var mdMethodRef = moduleMetadata.GetMethodRef("System.Console", "System.Console", "WriteLine");

                delegate*<string, string> ptr = &TargetClass.TargetMethod;
                var sigBlob = new byte[]
                {
                    (byte)CorCallingConvention.IMAGE_CEE_CS_CALLCONV_DEFAULT,
                    0x01, // number parameters
                    (byte)CorElementType.ELEMENT_TYPE_STRING, // return type
                    (byte)CorElementType.ELEMENT_TYPE_STRING, // return type
                };

                MdSignature sigToken = default;
                fixed (byte* p = sigBlob)
                {
                    moduleMetadata.Emit.GetTokenFromSig(p, (uint)sigBlob.Length, &sigToken);
                }
                var sigToken2 = sigToken;

                nint nintPtr = (nint)ptr;
                Console.WriteLine($"ptr: {nintPtr:x2}, sigToken2.Value: {sigToken2.Value:x2}");

                var newMethodBody = ILTransforms.Transform(instrs => ILTransforms.AddStringCall(mdString.Value, mdMethodRef.Value, nintPtr, sigToken2.Value, instrs), methodBody.ParsedMethodBody);
                methodMetadata.SetParsedBody(_corProfilerInfo, newMethodBody);
            }

            return HResult.S_OK;
        }

        protected override HResult ExceptionThrown(ObjectId thrownObjectId)
        {
            Console.WriteLine("Enumerating modules");

            //_corProfilerInfo.EnumModules(out void* enumerator);

            //ICorProfilerModuleEnum moduleEnumerator = NativeStubs.ICorProfilerModuleEnumStub.Wrap((IntPtr)enumerator);

            //moduleEnumerator.GetCount(out var modulesCount);

            //Console.WriteLine($"Fetching {modulesCount} modules");

            //var modules = new ModuleId[modulesCount];

            //fixed (ModuleId* p = modules)
            //{
            //    moduleEnumerator.Next(modulesCount, p, out modulesCount);
            //}

            //Console.WriteLine($"Fetched {modulesCount} modules");

            //foreach (var module in modules)
            //{
            //    ModuleMetadata moduleMetadata;
            //    lock (_syncRoot)
            //    {
            //        _moduleMetadataTable.TryGetValue(module, out moduleMetadata);
            //    }

            //    Console.WriteLine($"Module: {moduleMetadata.ModuleName} loaded at address {moduleMetadata.BaseAddress:x2}");
            //}

            //_corProfilerInfo.GetClassFromObject(thrownObjectId, out var classId);
            //_corProfilerInfo.GetClassIdInfo(classId, out var moduleId, out var typeDef);
            //_corProfilerInfo.GetModuleMetaData(moduleId, CorOpenFlags.ofRead, KnownGuids.IMetaDataImport, out void* ppOut);

            //var metaDataImport = NativeStubs.IMetaDataImportStub.Wrap((IntPtr)ppOut);

            //metaDataImport.GetTypeDefProps(typeDef, null, 0, out var nameCharCount, out _, out _);

            //Span<char> buffer = stackalloc char[(int)nameCharCount];

            //fixed (char* p = buffer)
            //{
            //    metaDataImport.GetTypeDefProps(typeDef, p, nameCharCount, out _, out _, out _);
            //}

            //Console.WriteLine("[Profiler] An exception was thrown: " + new string(buffer));

            return HResult.S_OK;
        }

        protected override HResult ExceptionSearchCatcherFound(FunctionId functionId)
        {
            //_corProfilerInfo.GetFunctionInfo(functionId, out var classId, out var moduleId, out var mdToken);

            //_corProfilerInfo.GetModuleMetaData(moduleId, CorOpenFlags.ofRead, KnownGuids.IMetaDataImport, out var ppOut);

            //IMetaDataImport metaDataImport = NativeStubs.IMetaDataImportStub.Wrap((IntPtr)ppOut);

            //metaDataImport.GetMethodProps(new MdMethodDef(mdToken), out _, null, 0, out var size, out _, out _, out _, out _, out _);

            //var buffer = new char[size];

            //MdTypeDef typeDef;

            //fixed (char* p = buffer)
            //{
            //    metaDataImport.GetMethodProps(new MdMethodDef(mdToken), out typeDef, p, size, out _, out _, out _, out _, out _, out _);
            //}

            //metaDataImport.GetTypeDefProps(typeDef, null, 0, out size, out _, out _);

            //var methodName = new string(buffer);

            //buffer = new char[size];

            //fixed (char* p = buffer)
            //{
            //    metaDataImport.GetTypeDefProps(typeDef, p, size, out _, out _, out _);
            //}

            //var typeName = new string(buffer);

            //Console.WriteLine($"[Profiler] Exception was caught in {typeName}.{methodName}");
            return HResult.S_OK;
        }
    }
}
