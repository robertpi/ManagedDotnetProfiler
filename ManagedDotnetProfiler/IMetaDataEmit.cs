using Microsoft.FSharp.Control;
using Microsoft.FSharp.Data.UnitSystems.SI.UnitNames;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection.Metadata;
using System.Security;
using System.Text;
using System.Threading.Tasks;

namespace ManagedDotnetProfiler;

[GenerateNativeStub]
internal unsafe interface IMetaDataEmit : IUnknown
{
    HResult SetModuleProps(              // S_OK or error.
        char* szName);           // [IN] If not NULL, the name of the module to set.

    HResult Save(                        // S_OK or error.
        char* szFile,                 // [IN] The filename to save to.
        int dwSaveFlags);      // [IN] Flags for the save.

    HResult SaveToStream(                // S_OK or error.
        void* pIStream,              // [IN] A writable stream to save to.
        int dwSaveFlags);      // [IN] Flags for the save.

    HResult GetSaveSize(                 // S_OK or error.
        CorSaveSize fSave,                  // [IN] cssAccurate or cssQuick.
        int* pdwSaveSize);     // [OUT] Put the size here.

    HResult DefineTypeDef(               // S_OK or error.
        char* szTypeDef,              // [IN] Name of TypeDef
        int dwTypeDefFlags,         // [IN] CustomAttribute flags
        MdToken tkExtends,              // [IN] extends this TypeDef or typeref
        MdToken[] rtkImplements,        // [IN] Implements interfaces
        MdTypeDef* ptd);             // [OUT] Put TypeDef token here

    HResult DefineNestedType(            // S_OK or error.
        char* szTypeDef,              // [IN] Name of TypeDef
        int dwTypeDefFlags,         // [IN] CustomAttribute flags
        MdToken tkExtends,              // [IN] extends this TypeDef or typeref
        MdToken[] rtkImplements,        // [IN] Implements interfaces
        MdTypeDef tdEncloser,             // [IN] TypeDef token of the enclosing type.
        MdTypeDef   *ptd);             // [OUT] Put TypeDef token here

    HResult SetHandler(                  // S_OK.
        void* pUnk);            // [IN] The new error handler.

    HResult DefineMethod(                // S_OK or error.
        MdTypeDef td,                     // Parent TypeDef
        char* szName,                 // Name of member
        int dwMethodFlags,          // Member attributes
        nint* pvSigBlob,          // [IN] point to a blob value of CLR signature
        uint cbSigBlob,              // [IN] count of bytes in the signature blob
        uint ulCodeRVA,
        int dwImplFlags,
        MdMethodDef* pmd);             // Put member token here

    HResult DefineMethodImpl(            // S_OK or error.
        MdTypeDef td,                     // [IN] The class implementing the method
        MdToken tkBody,                 // [IN] Method body - MethodDef or MethodRef
        MdToken tkDecl);           // [IN] Method declaration - MethodDef or MethodRef

    HResult DefineTypeRefByName(         // S_OK or error.
        MdToken tkResolutionScope,      // [IN] ModuleRef, AssemblyRef or TypeRef.
        char* szName,                 // [IN] Name of the TypeRef.
        MdTypeRef* ptr);             // [OUT] Put TypeRef token here.

    HResult DefineImportType(            // S_OK or error.
        void* pAssemImport,  // [IN] Assembly containing the TypeDef.
        void* pbHashValue,           // [IN] Hash Blob for Assembly.
        uint       cbHashValue,            // [IN] Count of bytes.
        void* pImport,           // [IN] Scope containing the TypeDef.
        MdTypeDef   tdImport,               // [IN] The imported TypeDef.
        void* pAssemEmit,  // [IN] Assembly into which the TypeDef is imported.
        MdTypeRef   *ptr);             // [OUT] Put TypeRef token here.

    HResult DefineMemberRef(             // S_OK or error
        MdToken tkImport,               // [IN] ClassRef or ClassDef importing a member.
        char* szName,                 // [IN] member's name
        nint* pvSigBlob,          // [IN] point to a blob value of CLR signature
        uint cbSigBlob,              // [IN] count of bytes in the signature blob
        MdMemberRef* pmr);             // [OUT] memberref token

    HResult DefineImportMember(          // S_OK or error.
        void* pAssemImport,  // [IN] Assembly containing the Member.
        void* pbHashValue,           // [IN] Hash Blob for Assembly.
        uint       cbHashValue,            // [IN] Count of bytes.
        void* pImport,           // [IN] Import scope, with member.
        MdToken     mbMember,               // [IN] Member in import scope.
        void* pAssemEmit,  // [IN] Assembly into which the Member is imported.
        MdToken     tkParent,               // [IN] Classref or classdef in emit scope.
        MdMemberRef* pmr);             // [OUT] Put member ref here.

    HResult DefineEvent(
        MdTypeDef td,                     // [IN] the class/interface on which the event is being defined
        char* szEvent,                // [IN] Name of the event
        int dwEventFlags,           // [IN] CorEventAttr
        MdToken tkEventType,            // [IN] a reference (MdTypeRef or MdTypeRef) to the Event class
        MdMethodDef mdAddOn,                // [IN] required add method
        MdMethodDef mdRemoveOn,             // [IN] required remove method
        MdMethodDef mdFire,                 // [IN] optional fire method
        MdMethodDef[] rmdOtherMethods,      // [IN] optional array of other methods associate with the event
        MdEvent* pmdEvent);        // [OUT] output event token

    HResult SetClassLayout(
        MdTypeDef td,                     // [IN] typedef
        int dwPackSize,             // [IN] packing size specified as 1, 2, 4, 8, or 16
        COR_FIELD_OFFSET[] rFieldOffsets,   // [IN] array of layout specification
        uint ulClassSize);      // [IN] size of the class

    HResult DeleteClassLayout(
        MdTypeDef td);               // [IN] typedef whose layout is to be deleted.

    HResult SetFieldMarshal(
        MdToken tk,                     // [IN] given a fieldDef or paramDef token
        nint* pvNativeType,       // [IN] native type specification
        uint cbNativeType);     // [IN] count of bytes of pvNativeType

    HResult DeleteFieldMarshal(
        MdToken tk);               // [IN] given a fieldDef or paramDef token

    HResult DefinePermissionSet(
        MdToken tk,                     // [IN] the object to be decorated.
        int dwAction,               // [IN] CorDeclSecurity.
        void* pvPermission,          // [IN] permission blob.
        uint       cbPermission,           // [IN] count of bytes of pvPermission.
        MdPermission* ppm);            // [OUT] returned permission token.

    HResult SetRVA(                      // S_OK or error.
        MdMethodDef md,                     // [IN] Method for which to set offset
        uint ulRVA);            // [IN] The offset

    HResult GetTokenFromSig(             // S_OK or error.
        byte* pvSig,              // [IN] Signature to define.
        uint cbSig,                  // [IN] Size of signature data.
        MdSignature* pmsig);           // [OUT] returned signature token.

    HResult DefineModuleRef(             // S_OK or error.
        char* szName,                 // [IN] DLL name
        MdModuleRef* pmur);            // [OUT] returned

    // <TODO>@FUTURE:  This should go away once everyone starts using SetMemberRefProps.</TODO>
    HResult SetParent(                   // S_OK or error.
        MdMemberRef mr,                     // [IN] Token for the ref to be fixed up.
        MdToken tk);               // [IN] The ref parent.

    HResult GetTokenFromTypeSpec(        // S_OK or error.
        byte* pvSig,              // [IN] TypeSpec Signature to define.
        uint cbSig,                  // [IN] Size of signature data.
        MdTypeSpec* ptypespec);        // [OUT] returned TypeSpec token.

    HResult SaveToMemory(                // S_OK or error.
        void* pbData,                // [OUT] Location to write data.
        uint cbData);           // [IN] Max size of data buffer.

    HResult DefineUserString(            // Return code.
        char* szString,                   // [IN] User literal string.
        uint cchString,              // [IN] Length of string.
        MdString* pstk);            // [OUT] String token.

    HResult DeleteToken(                 // Return code.
        MdToken tkObj);            // [IN] The token to be deleted

    HResult SetMethodProps(              // S_OK or error.
        MdMethodDef md,                     // [IN] The MethodDef.
        int dwMethodFlags,          // [IN] Method attributes.
        uint ulCodeRVA,              // [IN] Code RVA.
        int dwImplFlags);      // [IN] Impl flags.

    HResult SetTypeDefProps(             // S_OK or error.
        MdTypeDef td,                     // [IN] The TypeDef.
        int dwTypeDefFlags,         // [IN] TypeDef flags.
        MdToken tkExtends,              // [IN] Base TypeDef or TypeRef.
        MdToken[] rtkImplements);  // [IN] Implemented interfaces.

    HResult SetEventProps(               // S_OK or error.
        MdEvent ev,                     // [IN] The event token.
        int dwEventFlags,           // [IN] CorEventAttr.
        MdToken tkEventType,            // [IN] A reference (MdTypeRef or MdTypeRef) to the Event class.
        MdMethodDef mdAddOn,                // [IN] Add method.
        MdMethodDef mdRemoveOn,             // [IN] Remove method.
        MdMethodDef mdFire,                 // [IN] Fire method.
        MdMethodDef[] rmdOtherMethods);// [IN] Array of other methods associate with the event.

    HResult SetPermissionSetProps(       // S_OK or error.
        MdToken tk,                     // [IN] The object to be decorated.
        int dwAction,               // [IN] CorDeclSecurity.
        void* pvPermission,          // [IN] Permission blob.
        uint       cbPermission,           // [IN] Count of bytes of pvPermission.
        MdPermission* ppm);            // [OUT] Permission token.

    HResult DefinePinvokeMap(            // Return code.
        MdToken tk,                     // [IN] FieldDef or MethodDef.
        int dwMappingFlags,         // [IN] Flags used for mapping.
        char* szImportName,           // [IN] Import name.
        MdModuleRef mrImportDLL);      // [IN] ModuleRef token for the target DLL.

    HResult SetPinvokeMap(               // Return code.
        MdToken tk,                     // [IN] FieldDef or MethodDef.
        int dwMappingFlags,         // [IN] Flags used for mapping.
        char* szImportName,           // [IN] Import name.
        MdModuleRef mrImportDLL);      // [IN] ModuleRef token for the target DLL.

    HResult DeletePinvokeMap(            // Return code.
        MdToken tk);               // [IN] FieldDef or MethodDef.

    // New CustomAttribute functions.
    HResult DefineCustomAttribute(       // Return code.
        MdToken tkOwner,                // [IN] The object to put the value on.
        MdToken tkCtor,                 // [IN] Constructor of the CustomAttribute type (MemberRef/MethodDef).
        void* pCustomAttribute,      // [IN] The custom value data.
        uint       cbCustomAttribute,      // [IN] The custom value data length.
        MdCustomAttribute* pcv);       // [OUT] The custom value token value on return.

    HResult SetCustomAttributeValue(     // Return code.
        MdCustomAttribute pcv,              // [IN] The custom value token whose value to replace.
        void* pCustomAttribute,      // [IN] The custom value data.
        uint       cbCustomAttribute);// [IN] The custom value data length.

    HResult DefineField(                 // S_OK or error.
        MdTypeDef td,                     // Parent TypeDef
        char* szName,                 // Name of member
        int dwFieldFlags,           // Member attributes
        nint* pvSigBlob,          // [IN] point to a blob value of CLR signature
        uint cbSigBlob,              // [IN] count of bytes in the signature blob
        int dwCPlusTypeFlag,        // [IN] flag for value type. selected ELEMENT_TYPE_*
        void* pValue,                // [IN] constant value
        uint       cchValue,               // [IN] size of constant value (string, in wide chars).
        MdFieldDef* pmd);             // [OUT] Put member token here

    HResult DefineProperty(
        MdTypeDef td,                     // [IN] the class/interface on which the property is being defined
        char* szProperty,             // [IN] Name of the property
        int dwPropFlags,            // [IN] CorPropertyAttr
        nint* pvSig,              // [IN] the required type signature
        uint cbSig,                  // [IN] the size of the type signature blob
        int dwCPlusTypeFlag,        // [IN] flag for value type. selected ELEMENT_TYPE_*
        void* pValue,                // [IN] constant value
        uint       cchValue,               // [IN] size of constant value (string, in wide chars).
        MdMethodDef mdSetter,               // [IN] optional setter of the property
        MdMethodDef mdGetter,               // [IN] optional getter of the property
        MdMethodDef[] rmdOtherMethods,      // [IN] an optional array of other methods
        MdProperty  *pmdProp);         // [OUT] output property token

    HResult DefineParam(
        MdMethodDef md,                     // [IN] Owning method
        uint ulParamSeq,             // [IN] Which param
        char* szName,                 // [IN] Optional param name
        int dwParamFlags,           // [IN] Optional param flags
        int dwCPlusTypeFlag,        // [IN] flag for value type. selected ELEMENT_TYPE_*
        void* pValue,                // [IN] constant value
        uint       cchValue,               // [IN] size of constant value (string, in wide chars).
        MdParamDef* ppd);             // [OUT] Put param token here

    HResult SetFieldProps(               // S_OK or error.
        MdFieldDef fd,                     // [IN] The FieldDef.
        int dwFieldFlags,           // [IN] Field attributes.
        int dwCPlusTypeFlag,        // [IN] Flag for the value type, selected ELEMENT_TYPE_*
        void * pValue,                // [IN] Constant value.
        uint       cchValue);         // [IN] size of constant value (string, in wide chars).

    HResult SetPropertyProps(            // S_OK or error.
        MdProperty pr,                     // [IN] Property token.
        int dwPropFlags,            // [IN] CorPropertyAttr.
        int dwCPlusTypeFlag,        // [IN] Flag for value type, selected ELEMENT_TYPE_*
        void* pValue,                // [IN] Constant value.
        uint       cchValue,               // [IN] size of constant value (string, in wide chars).
        MdMethodDef mdSetter,               // [IN] Setter of the property.
        MdMethodDef mdGetter,               // [IN] Getter of the property.
        MdMethodDef[] rmdOtherMethods);// [IN] Array of other methods.

    HResult SetParamProps(               // Return code.
        MdParamDef pd,                     // [IN] Param token.
        char* szName,                 // [IN] Param name.
        int dwParamFlags,           // [IN] Param flags.
        int dwCPlusTypeFlag,        // [IN] Flag for value type. selected ELEMENT_TYPE_*.
        void* pValue,                // [OUT] Constant value.
        uint       cchValue);         // [IN] size of constant value (string, in wide chars).

    // Specialized Custom Attributes for security.
    HResult DefineSecurityAttributeSet(  // Return code.
        MdToken tkObj,                  // [IN] Class or method requiring security attributes.
        nint* rSecAttrs,            // [IN] Array of security attribute descriptions.
        uint cSecAttrs,              // [IN] Count of elements in above array.
        uint       *pulErrorAttr);    // [OUT] On error, index of attribute causing problem.

    HResult ApplyEditAndContinue(        // S_OK or error.
        void* pImport);         // [IN] Metadata from the delta PE.

    HResult TranslateSigWithScope(
        void* pAssemImport, // [IN] importing assembly interface
        void* pbHashValue,           // [IN] Hash Blob for Assembly.
        uint       cbHashValue,            // [IN] Count of bytes.
        void* import,            // [IN] importing interface
        nint* pbSigBlob,          // [IN] signature in the importing scope
        uint cbSigBlob,              // [IN] count of bytes of signature
        void* pAssemEmit,  // [IN] emit assembly interface
        void* emit,                // [IN] emit interface
        nint* pvTranslatedSig,     // [OUT] buffer to hold translated signature
        uint cbTranslatedSigMax,
        uint       *pcbTranslatedSig);// [OUT] count of bytes in the translated signature

    HResult SetMethodImplFlags(          // [IN] S_OK or error.
        MdMethodDef md,                     // [IN] Method for which to set ImplFlags
        int dwImplFlags);

    HResult SetFieldRVA(                 // [IN] S_OK or error.
    MdFieldDef fd,                     // [IN] Field for which to set offset
        uint ulRVA);            // [IN] The offset

    HResult Merge(                       // S_OK or error.
        void* pImport,           // [IN] The scope to be merged.
        void* pHostMapToken,         // [IN] Host IMapToken interface to receive token remap notification
        void* pHandler);        // [IN] An object to receive to receive error notification.

    HResult MergeEnd();             // S_OK or error.

}
