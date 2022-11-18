using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Metadata;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace ManagedDotnetProfiler;

[GenerateNativeStub]
internal unsafe interface IMetaDataAssemblyImport: IUnknown
{
    HResult  GetAssemblyProps(            // S_OK or error.
        MdAssembly mda,                    // [IN] The Assembly for which to get the properties.
        void** ppbPublicKey,         // [OUT] Pointer to the public key.
        ulong       *pcbPublicKey,          // [OUT] Count of bytes in the public key.
        ulong* pulHashAlgId,          // [OUT] Hash Algorithm.
        char* szName, // [OUT] Buffer to fill with assembly's simply name.
        ulong       cchName,                // [IN] Size of buffer in wide chars.
        ulong* pchName,               // [OUT] Actual # of wide chars in name.
        ASSEMBLYMETADATA *pMetaData,        // [OUT] Assembly MetaData.
        uint* pdwAssemblyFlags);    // [OUT] Flags.

    HResult  GetAssemblyRefProps(         // S_OK or error.
        MdAssemblyRef mdar,                 // [IN] The AssemblyRef for which to get the properties.
        void** ppbPublicKeyOrToken,  // [OUT] Pointer to the public key or token.
        ulong       *pcbPublicKeyOrToken,   // [OUT] Count of bytes in the public key or token.
        char* szName, // [OUT] Buffer to fill with name.
        ulong       cchName,                // [IN] Size of buffer in wide chars.
        ulong* pchName,               // [OUT] Actual # of wide chars in name.
        ASSEMBLYMETADATA *pMetaData,        // [OUT] Assembly MetaData.
        void** ppbHashValue,         // [OUT] Hash blob.
        ulong       *pcbHashValue,          // [OUT] Count of bytes in the hash blob.
        uint* pdwAssemblyRefFlags); // [OUT] Flags.

    HResult  GetFileProps(                // S_OK or error.
        MdFile mdf,                    // [IN] The File for which to get the properties.
        char*      szName, // [OUT] Buffer to fill with name.
        ulong cchName,                // [IN] Size of buffer in wide chars.
        ulong       *pchName,               // [OUT] Actual # of wide chars in name.
        void** ppbHashValue,         // [OUT] Pointer to the Hash Value Blob.
        ulong       *pcbHashValue,          // [OUT] Count of bytes in the Hash Value Blob.
        uint* pdwFileFlags);    // [OUT] Flags.

    HResult GetExportedTypeProps(        // S_OK or error.
        MdExportedType mdct,              // [IN] The ExportedType for which to get the properties.
        char*      szName, // [OUT] Buffer to fill with name.
        ulong cchName,                // [IN] Size of buffer in wide chars.
        ulong* pchName,               // [OUT] Actual # of wide chars in name.
        MdToken* ptkImplementation,     // [OUT] mdFile or mdAssemblyRef or mdExportedType.
        MdTypeDef* ptkTypeDef,            // [OUT] TypeDef token within the file.
        uint* pdwExportedTypeFlags); // [OUT] Flags.

    HResult  GetManifestResourceProps(    // S_OK or error.
        MdManifestResource mdmr,           // [IN] The ManifestResource for which to get the properties.
        char*      szName,  // [OUT] Buffer to fill with name.
        ulong cchName,                // [IN] Size of buffer in wide chars.
        ulong       *pchName,               // [OUT] Actual # of wide chars in name.
        MdToken* ptkImplementation,     // [OUT] mdFile or mdAssemblyRef that provides the ManifestResource.
        uint       *pdwOffset,             // [OUT] Offset to the beginning of the resource within the file.
        uint* pdwResourceFlags);// [OUT] Flags.

    HResult  EnumAssemblyRefs(            // S_OK or error
        HCORENUM* phEnum,                // [IN|OUT] Pointer to the enum.
        MdAssemblyRef* rAssemblyRefs,      // [OUT] Put AssemblyRefs here.
        ulong cMax,                   // [IN] Max AssemblyRefs to put.
        ulong       *pcTokens);        // [OUT] Put # put here.

    HResult  EnumFiles(                   // S_OK or error
        HCORENUM* phEnum,                // [IN|OUT] Pointer to the enum.
        MdFile[] rFiles,               // [OUT] Put Files here.
        ulong cMax,                   // [IN] Max Files to put.
        ulong       *pcTokens);        // [OUT] Put # put here.

    HResult  EnumExportedTypes(           // S_OK or error
        HCORENUM* phEnum,                // [IN|OUT] Pointer to the enum.
        MdExportedType[] rExportedTypes,  // [OUT] Put ExportedTypes here.
        ulong cMax,                   // [IN] Max ExportedTypes to put.
        ulong       *pcTokens);        // [OUT] Put # put here.

    HResult  EnumManifestResources(       // S_OK or error
        HCORENUM* phEnum,                // [IN|OUT] Pointer to the enum.
        MdManifestResource[] rManifestResources,   // [OUT] Put ManifestResources here.
        ulong cMax,                   // [IN] Max Resources to put.
        ulong       *pcTokens);        // [OUT] Put # put here.

    HResult  GetAssemblyFromScope(        // S_OK or error
        MdAssembly* ptkAssembly);     // [OUT] Put token here.

    HResult  FindExportedTypeByName(      // S_OK or error
        char* szName,                 // [IN] Name of the ExportedType.
        MdToken mdtExportedType,        // [IN] ExportedType for the enclosing class.
        MdExportedType* ptkExportedType); // [OUT] Put the ExportedType token here.

    HResult FindManifestResourceByName(  // S_OK or error
        char* szName,                 // [IN] Name of the ManifestResource.
        MdManifestResource* ptkManifestResource);  // [OUT] Put the ManifestResource token here.

    void CloseEnum(
        HCORENUM hEnum);               // Enum to be closed.

    HResult FindAssembliesByName(        // S_OK or error
        char* szAppBase,                 // [IN] optional - can be NULL
        char* szPrivateBin,              // [IN] optional - can be NULL
        char* szAssemblyName,            // [IN] required - this is the assembly you are requesting
        void* ppIUnk,                 // [OUT] put IMetaDataAssemblyImport pointers here
        ulong cMax,                      // [IN] The max number to put
        ulong* pcAssemblies);       // [OUT] The number of assemblies returned.

}
