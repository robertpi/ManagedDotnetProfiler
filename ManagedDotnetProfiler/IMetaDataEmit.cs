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
using ProfilerLib;
using ProfilerLib.Interfaces;

namespace ManagedDotnetProfiler;

[NativeObject]
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
        int fSave,                  // [IN] cssAccurate or cssQuick.
        int* pdwSaveSize);     // [OUT] Put the size here.

    HResult DefineTypeDef(               // S_OK or error.
        char* szTypeDef,              // [IN] Name of TypeDef
        int dwTypeDefFlags,         // [IN] CustomAttribute flags
        int tkExtends,              // [IN] extends this TypeDef or typeref
        int* rtkImplements,        // [IN] Implements interfaces
        int* ptd);             // [OUT] Put TypeDef token here

    HResult DefineNestedType(            // S_OK or error.
        char* szTypeDef,              // [IN] Name of TypeDef
        int dwTypeDefFlags,         // [IN] CustomAttribute flags
        int tkExtends,              // [IN] extends this TypeDef or typeref
        int* rtkImplements,        // [IN] Implements interfaces
        int tdEncloser,             // [IN] TypeDef token of the enclosing type.
        int   *ptd);             // [OUT] Put TypeDef token here

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
        int* pmd);             // Put member token here

    HResult DefineMethodImpl(            // S_OK or error.
        int td,                     // [IN] The class implementing the method
        int tkBody,                 // [IN] Method body - MethodDef or MethodRef
        int tkDecl);           // [IN] Method declaration - MethodDef or MethodRef

    HResult DefineTypeRefByName(         // S_OK or error.
        int tkResolutionScope,      // [IN] ModuleRef, AssemblyRef or TypeRef.
        char* szName,                 // [IN] Name of the TypeRef.
        int* ptr);             // [OUT] Put TypeRef token here.

    HResult DefineImportType(            // S_OK or error.
        void* pAssemImport,  // [IN] Assembly containing the TypeDef.
        void* pbHashValue,           // [IN] Hash Blob for Assembly.
        uint       cbHashValue,            // [IN] Count of bytes.
        void* pImport,           // [IN] Scope containing the TypeDef.
        int   tdImport,               // [IN] The imported TypeDef.
        void* pAssemEmit,  // [IN] Assembly into which the TypeDef is imported.
        int   *ptr);             // [OUT] Put TypeRef token here.

    HResult DefineMemberRef(             // S_OK or error
        int tkImport,               // [IN] ClassRef or ClassDef importing a member.
        char* szName,                 // [IN] member's name
        nint* pvSigBlob,          // [IN] point to a blob value of CLR signature
        uint cbSigBlob,              // [IN] count of bytes in the signature blob
        int* pmr);             // [OUT] memberref token

    HResult DefineImportMember(          // S_OK or error.
        void* pAssemImport,  // [IN] Assembly containing the Member.
        void* pbHashValue,           // [IN] Hash Blob for Assembly.
        uint       cbHashValue,            // [IN] Count of bytes.
        void* pImport,           // [IN] Import scope, with member.
        int     mbMember,               // [IN] Member in import scope.
        void* pAssemEmit,  // [IN] Assembly into which the Member is imported.
        int     tkParent,               // [IN] Classref or classdef in emit scope.
        int* pmr);             // [OUT] Put member ref here.

    HResult DefineEvent(
        int td,                     // [IN] the class/interface on which the event is being defined
        char* szEvent,                // [IN] Name of the event
        int dwEventFlags,           // [IN] CorEventAttr
        int tkEventType,            // [IN] a reference (MdTypeRef or MdTypeRef) to the Event class
        int mdAddOn,                // [IN] required add method
        int mdRemoveOn,             // [IN] required remove method
        int mdFire,                 // [IN] optional fire method
        int*  rmdOtherMethods,      // [IN] optional array of other methods associate with the event
        int* pmdEvent);        // [OUT] output event token

    HResult SetClassLayout(
        int td,                     // [IN] typedef
        int dwPackSize,             // [IN] packing size specified as 1, 2, 4, 8, or 16
        int* rFieldOffsets,   // [IN] array of layout specification
        uint ulClassSize);      // [IN] size of the class

    HResult DeleteClassLayout(
        MdTypeDef td);               // [IN] typedef whose layout is to be deleted.

    HResult SetFieldMarshal(
        int tk,                     // [IN] given a fieldDef or paramDef token
        nint* pvNativeType,       // [IN] native type specification
        uint cbNativeType);     // [IN] count of bytes of pvNativeType

    HResult DeleteFieldMarshal(
        int tk);               // [IN] given a fieldDef or paramDef token

    HResult DefinePermissionSet(
        int tk,                     // [IN] the object to be decorated.
        int dwAction,               // [IN] CorDeclSecurity.
        void* pvPermission,          // [IN] permission blob.
        uint       cbPermission,           // [IN] count of bytes of pvPermission.
        int* ppm);            // [OUT] returned permission token.

    HResult SetRVA(                      // S_OK or error.
        int md,                     // [IN] Method for which to set offset
        uint ulRVA);            // [IN] The offset

    HResult GetTokenFromSig(             // S_OK or error.
        byte* pvSig,              // [IN] Signature to define.
        uint cbSig,                  // [IN] Size of signature data.
        int* pmsig);           // [OUT] returned signature token.

    HResult DefineModuleRef(             // S_OK or error.
        char* szName,                 // [IN] DLL name
        int* pmur);            // [OUT] returned

    // <TODO>@FUTURE:  This should go away once everyone starts using SetMemberRefProps.</TODO>
    HResult SetParent(                   // S_OK or error.
        MdMemberRef mr,                     // [IN] Token for the ref to be fixed up.
        int tk);               // [IN] The ref parent.

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
        int tkObj);            // [IN] The token to be deleted

    HResult SetMethodProps(              // S_OK or error.
        int md,                     // [IN] The MethodDef.
        int dwMethodFlags,          // [IN] Method attributes.
        uint ulCodeRVA,              // [IN] Code RVA.
        int dwImplFlags);      // [IN] Impl flags.

    HResult SetTypeDefProps(             // S_OK or error.
        MdTypeDef td,                     // [IN] The TypeDef.
        int dwTypeDefFlags,         // [IN] TypeDef flags.
        int tkExtends,              // [IN] Base TypeDef or TypeRef.
        int* rtkImplements);  // [IN] Implemented interfaces.

    HResult SetEventProps(               // S_OK or error.
        int ev,                     // [IN] The event token.
        int dwEventFlags,           // [IN] CorEventAttr.
        int tkEventType,            // [IN] A reference (MdTypeRef or MdTypeRef) to the Event class.
        int mdAddOn,                // [IN] Add method.
        int mdRemoveOn,             // [IN] Remove method.
        int mdFire,                 // [IN] Fire method.
        int* rmdOtherMethods);// [IN] Array of other methods associate with the event.

    HResult SetPermissionSetProps(       // S_OK or error.
        int tk,                     // [IN] The object to be decorated.
        int dwAction,               // [IN] CorDeclSecurity.
        void* pvPermission,          // [IN] Permission blob.
        uint       cbPermission,           // [IN] Count of bytes of pvPermission.
        int* ppm);            // [OUT] Permission token.

    HResult DefinePinvokeMap(            // Return code.
        int tk,                     // [IN] FieldDef or MethodDef.
        int dwMappingFlags,         // [IN] Flags used for mapping.
        char* szImportName,           // [IN] Import name.
        MdModuleRef mrImportDLL);      // [IN] ModuleRef token for the target DLL.

    HResult SetPinvokeMap(               // Return code.
        int tk,                     // [IN] FieldDef or MethodDef.
        int dwMappingFlags,         // [IN] Flags used for mapping.
        char* szImportName,           // [IN] Import name.
        MdModuleRef mrImportDLL);      // [IN] ModuleRef token for the target DLL.

    HResult DeletePinvokeMap(            // Return code.
        int tk);               // [IN] FieldDef or MethodDef.

    // New CustomAttribute functions.
    HResult DefineCustomAttribute(       // Return code.
        int tkOwner,                // [IN] The object to put the value on.
        int tkCtor,                 // [IN] Constructor of the CustomAttribute type (MemberRef/MethodDef).
        void* pCustomAttribute,      // [IN] The custom value data.
        uint       cbCustomAttribute,      // [IN] The custom value data length.
        MdCustomAttribute* pcv);       // [OUT] The custom value token value on return.

    HResult SetCustomAttributeValue(     // Return code.
        MdCustomAttribute pcv,              // [IN] The custom value token whose value to replace.
        void* pCustomAttribute,      // [IN] The custom value data.
        uint       cbCustomAttribute);// [IN] The custom value data length.

    HResult DefineField(                 // S_OK or error.
        int td,                     // Parent TypeDef
        char* szName,                 // Name of member
        int dwFieldFlags,           // Member attributes
        nint* pvSigBlob,          // [IN] point to a blob value of CLR signature
        uint cbSigBlob,              // [IN] count of bytes in the signature blob
        int dwCPlusTypeFlag,        // [IN] flag for value type. selected ELEMENT_TYPE_*
        void* pValue,                // [IN] constant value
        uint       cchValue,               // [IN] size of constant value (string, in wide chars).
        int* pmd);             // [OUT] Put member token here

    HResult DefineProperty(
        int td,                     // [IN] the class/interface on which the property is being defined
        char* szProperty,             // [IN] Name of the property
        int dwPropFlags,            // [IN] CorPropertyAttr
        nint* pvSig,              // [IN] the required type signature
        uint cbSig,                  // [IN] the size of the type signature blob
        int dwCPlusTypeFlag,        // [IN] flag for value type. selected ELEMENT_TYPE_*
        void* pValue,                // [IN] constant value
        uint       cchValue,               // [IN] size of constant value (string, in wide chars).
        int mdSetter,               // [IN] optional setter of the property
        int mdGetter,               // [IN] optional getter of the property
        int* rmdOtherMethods,      // [IN] an optional array of other methods
        int  *pmdProp);         // [OUT] output property token

    HResult DefineParam(
        int md,                     // [IN] Owning method
        uint ulParamSeq,             // [IN] Which param
        char* szName,                 // [IN] Optional param name
        int dwParamFlags,           // [IN] Optional param flags
        int dwCPlusTypeFlag,        // [IN] flag for value type. selected ELEMENT_TYPE_*
        void* pValue,                // [IN] constant value
        uint       cchValue,               // [IN] size of constant value (string, in wide chars).
        int* ppd);             // [OUT] Put param token here

    HResult SetFieldProps(               // S_OK or error.
        int fd,                     // [IN] The FieldDef.
        int dwFieldFlags,           // [IN] Field attributes.
        int dwCPlusTypeFlag,        // [IN] Flag for the value type, selected ELEMENT_TYPE_*
        void * pValue,                // [IN] Constant value.
        uint       cchValue);         // [IN] size of constant value (string, in wide chars).

    HResult SetPropertyProps(            // S_OK or error.
        int pr,                     // [IN] Property token.
        int dwPropFlags,            // [IN] CorPropertyAttr.
        int dwCPlusTypeFlag,        // [IN] Flag for value type, selected ELEMENT_TYPE_*
        void* pValue,                // [IN] Constant value.
        uint       cchValue,               // [IN] size of constant value (string, in wide chars).
        int mdSetter,               // [IN] Setter of the property.
        int mdGetter,               // [IN] Getter of the property.
        int* rmdOtherMethods);// [IN] Array of other methods.

    HResult SetParamProps(               // Return code.
        int pd,                     // [IN] Param token.
        char* szName,                 // [IN] Param name.
        int dwParamFlags,           // [IN] Param flags.
        int dwCPlusTypeFlag,        // [IN] Flag for value type. selected ELEMENT_TYPE_*.
        void* pValue,                // [OUT] Constant value.
        uint       cchValue);         // [IN] size of constant value (string, in wide chars).

    // Specialized Custom Attributes for security.
    HResult DefineSecurityAttributeSet(  // Return code.
        int tkObj,                  // [IN] Class or method requiring security attributes.
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
        int md,                     // [IN] Method for which to set ImplFlags
        int dwImplFlags);

    HResult SetFieldRVA(                 // [IN] S_OK or error.
        int fd,                     // [IN] Field for which to set offset
        uint ulRVA);            // [IN] The offset

    HResult Merge(                       // S_OK or error.
        void* pImport,           // [IN] The scope to be merged.
        void* pHostMapToken,         // [IN] Host IMapToken interface to receive token remap notification
        void* pHandler);        // [IN] An object to receive to receive error notification.

    HResult MergeEnd();             // S_OK or error.

}
