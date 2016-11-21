unit SecEdit;

interface
uses Windows, AclUi, SvcEntry;

type
  TServiceSecurityInformation = class(TInterfacedObject, ISecurityInformation)
  public
    function GetObjectInformation(out pObjectInfo: SI_OBJECT_INFO): HRESULT; stdcall;
    function GetSecurity(RequestedInformation: SECURITY_INFORMATION;
      out ppSecurityDescriptor: PSECURITY_DESCRIPTOR; fDefault: BOOL): HRESULT; stdcall;
    function SetSecurity(SecurityInformation: SECURITY_INFORMATION;
      pSecurityDescriptor: PSECURITY_DESCRIPTOR): HRESULT; stdcall;
    function GetAccessRights(pguidObjectType: PGUID; dwFlags: DWORD;
      out ppAccess: PSI_ACCESS; out pcAccesses, piDefaultAccess: ULONG): HRESULT; stdcall;
    function MapGeneric(pguidObjectType: PGUID; pAceFlags: PUCHAR;
      pMask: PACCESS_MASK): HRESULT; stdcall;
    function GetInheritTypes(out ppInheritTypes: PSI_INHERIT_TYPE;
      out pcInheritTypes: ULONG): HRESULT; stdcall;
    function PropertySheetPageCallback(hwnd: HWND; uMsg: UINT;
      uPage: SI_PAGE_TYPE): HRESULT; stdcall;
  end;

function EditServiceSecurity(const ServiceName: String): boolean;

//TODO: https://msdn.microsoft.com/en-us/library/windows/desktop/aa379105(v=vs.85).aspx
//https://msdn.microsoft.com/en-us/library/windows/desktop/aa375742(v=vs.85).aspx#access_control_editor_functions
//https://msdn.microsoft.com/en-us/library/windows/desktop/ms684935(v=vs.85).aspx
//https://msdn.microsoft.com/en-us/library/windows/desktop/aa379159(v=vs.85).aspx
//https://msdn.microsoft.com/en-us/library/windows/desktop/aa379589(v=vs.85).aspx

implementation

function TServiceSecurityInformation.GetObjectInformation(out pObjectInfo: SI_OBJECT_INFO): HRESULT;
begin
  pObjectInfo.dwFlags := SI_ADVANCED or SI_EDIT_ALL;
  Result := S_OK;
end;

function TServiceSecurityInformation.GetSecurity(RequestedInformation: SECURITY_INFORMATION;
  out ppSecurityDescriptor: PSECURITY_DESCRIPTOR; fDefault: BOOL): HRESULT;
begin

end;

function TServiceSecurityInformation.SetSecurity(SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): HRESULT;
function TServiceSecurityInformation.GetAccessRights(pguidObjectType: PGUID; dwFlags: DWORD;
  out ppAccess: PSI_ACCESS; out pcAccesses, piDefaultAccess: ULONG): HRESULT;
function TServiceSecurityInformation.MapGeneric(pguidObjectType: PGUID; pAceFlags: PUCHAR;
  pMask: PACCESS_MASK): HRESULT;
function TServiceSecurityInformation.GetInheritTypes(out ppInheritTypes: PSI_INHERIT_TYPE;
  out pcInheritTypes: ULONG): HRESULT;
function TServiceSecurityInformation.PropertySheetPageCallback(hwnd: HWND; uMsg: UINT;
  uPage: SI_PAGE_TYPE): HRESULT;

function EditServiceSecurity(const ServiceName: String): boolean;
begin
  //TODO:..
end;

end.
