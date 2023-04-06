using ProfilerAbstractIL.IO;
using ProfilerAbstractIL;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using ProfilerLib;
using static ProfilerAbstractIL.IL;

namespace ManagedDotnetProfiler
{
    internal class MethodMetadata
    {
        public MethodMetadata(ModuleMetadata moduleMetadata, MdMethodDef methodDef, string name, MdTypeDef typeDef, string typeName)
        {
            ModuleMetadata = moduleMetadata;
            MethodDef = methodDef;
            Name = name;
            TypeDef = typeDef;
            TypeName = typeName;
            FullyQualifiedName = $"{typeName}.{name}";
        }

        public ModuleMetadata ModuleMetadata { get; }
        public MdMethodDef MethodDef { get; }
        public string Name { get; }
        public MdTypeDef TypeDef { get; }
        public string TypeName { get; }
        public string FullyQualifiedName { get; }

        public unsafe MethodBody GetParsedBody(ICorProfilerInfo corProfilerInfo)
        {
            var hresult = corProfilerInfo.GetILFunctionBody(ModuleMetadata.ModuleId, MethodDef, out byte* body, out uint methodSize);

            byte[] bodyArray = Utils.BufferToArray(body, methodSize);
            var methodSizeInt = (int)methodSize;
            var pev = new ReadOnlyByteMemory(new ByteArrayMemory(bodyArray, 0, methodSizeInt));
            var methodBody = ILReader.seekReadMethodRVA(pev, Name);

            return new MethodBody(bodyArray, methodBody);
        }

        public unsafe void SetParsedBody(ICorProfilerInfo corProfilerInfo, ILMethodBody newMethodBody)
        {
            var newBodyArray = ILBinaryWriter.GenILMethodBody(Name, newMethodBody);

            var hresult = corProfilerInfo.GetILFunctionBodyAllocator(ModuleMetadata.ModuleId, out var allocatorPtr);
            Console.WriteLine($"GetILFunctionBodyAllocator {hresult}");

            var allocator = NativeStubs.IMethodMallocStub.Wrap(allocatorPtr);

            var bodyBuffer = allocator.Alloc((ulong)newBodyArray.LongLength);
            Utils.ArrayToBuffer(newBodyArray, bodyBuffer);

            hresult = corProfilerInfo.SetILFunctionBody(ModuleMetadata.ModuleId, MethodDef, bodyBuffer);
            Console.WriteLine($"SetILFunctionBody {hresult}");
        }
    }
}
