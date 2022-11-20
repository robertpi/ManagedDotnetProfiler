using System;
using System.Reflection;
using System.Runtime.CompilerServices;
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

            GetAssemblyInfo(assemblyImport, out var assemblyName, out var version, out var mdAssembly);

            AssemblyName = assemblyName;
            AssemblyVersion = version;
            // this doesn't seem to work either;
            AssemblyToken = mdAssembly;

            // this doesn't seem to work, always give 1;
            var hresult = import.GetModuleFromScope(out var mdModule);
            ModuleToken = mdModule;
            Console.WriteLine($"ModuleToken: {ModuleToken.Value:x2}, hresult: {hresult.Value:x2}");
        }

        // could these be made private, so this class hides some of the nastiness using them?
        public IMetaDataImport Import { get; }
        public IMetaDataAssemblyImport AssemblyImport { get; }
        public IMetaDataEmit Emit { get; }


        public ModuleId ModuleId { get; }
        public MdModule ModuleToken { get; }
        public string ModuleName { get; }
        public AssemblyId AssemblyId { get; }
        public MdAssembly AssemblyToken { get; }
        public nint BaseAddress { get; }
        public string AssemblyName { get; }
        public Version AssemblyVersion { get; }

        private unsafe static void GetAssemblyInfo(IMetaDataAssemblyImport assemblyImport, out string assemblyName, out Version version, out MdAssembly mdAssembly)
        {
            MdAssembly current = default;
            var hr = assemblyImport.GetAssemblyFromScope(&current);
            Console.WriteLine($"GetAssemblyFromScope: {current.Value:x2}, hresult: {hr:x2}");

            mdAssembly = current;

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

        // TODO add some way of representing the signature
        public unsafe MdMemberRef GetMethodRef(string assemblyName, string typeName, string methodName)
        {
            HCORENUM emumerator;
            MdAssemblyRef currentAssemblyRef = default;
            Span<char> nameBuffer = stackalloc char[NameMaxSize];
            while (true)
            {
                ulong count;
                var hr = AssemblyImport.EnumAssemblyRefs(&emumerator, &currentAssemblyRef, 1, &count);
                if (count == 0)
                {
                    break;
                }

                void* publicKeyToken;
                ulong pktSize = 0;
                ulong cchName = 0;
                ASSEMBLYMETADATA curMD = default;

                fixed (char* p = nameBuffer)
                {
                    hr = AssemblyImport.GetAssemblyRefProps(currentAssemblyRef, &publicKeyToken, &pktSize, p,
                                                                              NameMaxSize, &cchName, &curMD,
                                                                              null /*ppbHashValue*/, null /*pcbHashValue*/,
                                                                              null /*pdwAssemblyRefFlags*/);

                }

                var name = new string(nameBuffer[..(int)cchName]);

                if (name == assemblyName)
                {
                    break;
                }
            }

            MdTypeRef typeRef;
            fixed (char* tn = typeName)
            {
                var hresult = Import.FindTypeRef(new MdToken(currentAssemblyRef.Value), tn, out typeRef);
            }
            var sigBlob = new byte[] 
            {
                (byte)CorCallingConvention.IMAGE_CEE_CS_CALLCONV_DEFAULT,
                0x01, // number parameters
                (byte)CorElementType.ELEMENT_TYPE_VOID, // return type
                (byte)CorElementType.ELEMENT_TYPE_STRING, // first parameter
            };

            MdMemberRef memRefToken;
            fixed (char* mn = methodName)
            {
                fixed (byte* sb = sigBlob)
                {
                    var hresult = Import.FindMemberRef(typeRef, mn, (nint*)sb, (uint)sigBlob.Length, out memRefToken);
                    Console.WriteLine($"FindMemberRef: {hresult.Value:x2}");
                }
            }

            return memRefToken;
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
