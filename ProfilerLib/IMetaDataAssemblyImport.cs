using NativeObjects;

namespace ProfilerLib;

public unsafe struct IMetaDataAssemblyImport
{
    private IMetaDataAssemblyImportInvoker _impl;

    public IMetaDataAssemblyImport(IntPtr ptr)
    {
        _impl = new(ptr);
    }

    public HResult GetAssemblyFromScope(MdAssembly* ptkAssembly)
    {
        return _impl.GetAssemblyFromScope(ptkAssembly);
    }

    public HResult GetAssemblyProps( // S_OK or error.
        MdAssembly mda, // [IN] The Assembly for which to get the properties.
        void** ppbPublicKey, // [OUT] Pointer to the public key.
        ulong* pcbPublicKey, // [OUT] Count of bytes in the public key.
        ulong* pulHashAlgId, // [OUT] Hash Algorithm.
        char* szName, // [OUT] Buffer to fill with assembly's simply name.
        ulong cchName, // [IN] Size of buffer in wide chars.
        ulong* pchName, // [OUT] Actual # of wide chars in name.
        ASSEMBLYMETADATA* pMetaData, // [OUT] Assembly MetaData.
        uint* pdwAssemblyFlags)
    {
        return _impl.GetAssemblyProps(mda, ppbPublicKey, pcbPublicKey, pulHashAlgId, szName, cchName, pchName, pMetaData, pdwAssemblyFlags);
    }

    public HResult EnumAssemblyRefs( // S_OK or error
        HCORENUM* phEnum, // [IN|OUT] Pointer to the enum.
        MdAssemblyRef* rAssemblyRefs, // [OUT] Put AssemblyRefs here.
        ulong cMax, // [IN] Max AssemblyRefs to put.
        ulong* pcTokens)
    {
        return _impl.EnumAssemblyRefs(phEnum, rAssemblyRefs, cMax, pcTokens);
    }

    public HResult  GetAssemblyRefProps(         // S_OK or error.
        MdAssemblyRef mdar,                 // [IN] The AssemblyRef for which to get the properties.
        void** ppbPublicKeyOrToken,  // [OUT] Pointer to the public key or token.
        ulong       *pcbPublicKeyOrToken,   // [OUT] Count of bytes in the public key or token.
        char* szName, // [OUT] Buffer to fill with name.
        ulong       cchName,                // [IN] Size of buffer in wide chars.
        ulong* pchName,               // [OUT] Actual # of wide chars in name.
        ASSEMBLYMETADATA *pMetaData,        // [OUT] Assembly MetaData.
        void** ppbHashValue,         // [OUT] Hash blob.
        ulong       *pcbHashValue,          // [OUT] Count of bytes in the hash blob.
        uint* pdwAssemblyRefFlags)
    {
        return _impl.GetAssemblyRefProps(mdar, ppbPublicKeyOrToken, pcbPublicKeyOrToken, szName, cchName, pchName, pMetaData, ppbHashValue, pcbHashValue, pdwAssemblyRefFlags);
    }

}