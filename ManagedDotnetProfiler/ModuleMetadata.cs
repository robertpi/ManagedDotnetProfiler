using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;
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

        public ModuleId ModuleId { get; }
        public IMetaDataImport Import { get; }
        public IMetaDataAssemblyImport AssemblyImport { get; }
        public IMetaDataEmit Emit { get; }
        public string ModuleName { get; }
        public AssemblyId AssemblyId { get; }
        public nint BaseAddress { get; }
        public string AssemblyName { get; }
        public Version AssemblyVersion { get; }
    }
}
