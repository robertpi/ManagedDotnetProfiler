using System;
using System.Runtime.InteropServices;
using System.Text;
using static ProfilerAbstractIL.IL.ILFieldInit;

namespace ManagedDotnetProfiler
{
    internal class ModuleMetadata
    {
        private const int NameMaxSize = 1024;

        public ModuleMetadata(ModuleId moduleId, IMetaDataImport import, IMetaDataAssemblyImport assemblyImport, IMetaDataEmit emit, string moduleName, AssemblyId assemblyId, nint baseAddress)
        {
            ModuleId = moduleId;
            Import = import;
            AssemblyImport = assemblyImport;
            Emit = emit;
            ModuleName = moduleName;
            AssemblyId = assemblyId;
            BaseAddress = baseAddress;

            GetAssemblyInfo(assemblyImport, out var assemblyName, out var version);

            AssemblyName = assemblyName;
            AssemblyVersion = version;

        }

        private unsafe static void GetAssemblyInfo(IMetaDataAssemblyImport assemblyImport, out string assemblyName, out Version version)
        {
            MdAssembly current = default;
            var hr = assemblyImport.GetAssemblyFromScope(&current);

            Span<char> nameBuffer = stackalloc char[NameMaxSize];
            ulong nameLen = 0;
            ASSEMBLYMETADATA assemblyMetadata = default;
            uint assemblyFlags = 0;

            fixed (char* p = nameBuffer)
            {
                assemblyImport.GetAssemblyProps(current, null, null, null, p, NameMaxSize, &nameLen,
                                           &assemblyMetadata, &assemblyFlags);
            }
            assemblyName = new string(nameBuffer[..(int)nameLen]);
            version = new Version(assemblyMetadata.usMajorVersion, assemblyMetadata.usMinorVersion, assemblyMetadata.usRevisionNumber, assemblyMetadata.usRevisionNumber);
        }

        // could these be made private, so this class hides some of the nastiness using them?
        public IMetaDataImport Import { get; }
        public IMetaDataAssemblyImport AssemblyImport { get; }
        public IMetaDataEmit Emit { get; }


        public ModuleId ModuleId { get; }
        public string ModuleName { get; }
        public AssemblyId AssemblyId { get; }
        public nint BaseAddress { get; }
        public string AssemblyName { get; }
        public Version AssemblyVersion { get; }

        public unsafe MethodMetadata GetMethodMetadata(MdMethodDef methodToken) 
        {
            var hresult = Import.GetMethodProps(methodToken, out var typeDef, null, 0, out var size, out _, out _, out _, out _, out _);

            Span<char> buffer = stackalloc char[NameMaxSize];

            fixed (char* p = buffer)
            {
                Import.GetMethodProps(methodToken, out _, p, size, out size, out _, out _, out _, out _, out _);
            }

            var methodName = new string(buffer[..(int)size]);

            fixed (char* p = buffer)
            {
                hresult = Import.GetTypeDefProps(typeDef, p, size, out _, out _, out _);
            }

            var typeName = new string(buffer[..(int)size]);

            return new MethodMetadata(this, methodToken, methodName, typeDef, typeName);
        }

        public unsafe MdString DefineUserString(string message)
        {
            var length = Encoding.Unicode.GetByteCount(message);
            nint ptr = Marshal.AllocHGlobal(length);
            var messageBytes = Encoding.Unicode.GetBytes(message);
            Utils.ArrayToBuffer(messageBytes, (byte*)ptr);
            MdString mdString = default;
            Emit.DefineUserString((char*)ptr, (uint)length / 2, &mdString);
            Marshal.FreeHGlobal(ptr);
            return mdString;
        }
    }
}
