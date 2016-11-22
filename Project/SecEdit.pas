unit SecEdit;

interface
uses Windows, AclUi, WinSvc, ServiceHelper, SvcEntry;

type
  TServiceSecurityInformation = class(TInterfacedObject, ISecurityInformation)
  class var
    ServiceAccessMasks: array of SI_ACCESS;
    class constructor Create;
  protected
    FServiceName: string;
    hSC: SC_HANDLE;
    hSvc: SC_HANDLE;
  public
    constructor Create(const AServiceName: string);
    destructor Destroy; override;

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

function EditServiceSecurity(hwndOwner: HWND; const ServiceName: String): boolean;

{
Sources:

Service security and access rights:
  https://msdn.microsoft.com/en-us/library/windows/desktop/ms685981(v=vs.85).aspx

SetServiceObjectSecurity:
  https://msdn.microsoft.com/en-us/library/windows/desktop/aa379589(v=vs.85).aspx

ISecurityInformation:
  https://msdn.microsoft.com/en-us/library/windows/desktop/aa379105(v=vs.85).aspx

All authorization functions:
  https://msdn.microsoft.com/en-us/library/windows/desktop/aa375742(v=vs.85).aspx
}

const
  SERVICE_ACCESS_MAP: TGenericMapping = (
    GenericRead: STANDARD_RIGHTS_READ or SERVICE_QUERY_CONFIG or SERVICE_QUERY_STATUS or SERVICE_INTERROGATE or SERVICE_ENUMERATE_DEPENDENTS;
    GenericWrite: STANDARD_RIGHTS_WRITE or SERVICE_CHANGE_CONFIG;
    GenericExecute: STANDARD_RIGHTS_EXECUTE or SERVICE_START or SERVICE_STOP or SERVICE_PAUSE_CONTINUE or SERVICE_USER_DEFINED_CONTROL;
    GenericAll: SERVICE_ALL_ACCESS;
  );


implementation
uses SysUtils;

type
  TAccessParam = record
    m: ACCESS_MASK;
    t: string;
    f: DWORD;
  end;

const
  SERVICE_ACCESS_PARAMS: array[0..17] of TAccessParam = (
    (m: SERVICE_ALL_ACCESS;            t: 'All access';            f: SI_ACCESS_GENERAL),

    (m: STANDARD_RIGHTS_READ;          t: 'Read';                  f: SI_ACCESS_GENERAL),
    (m: STANDARD_RIGHTS_WRITE;         t: 'Write';                 f: SI_ACCESS_GENERAL),
    (m: STANDARD_RIGHTS_EXECUTE;       t: 'Execute';               f: SI_ACCESS_GENERAL),

    (m: SERVICE_CHANGE_CONFIG;         t: 'Change config';         f: SI_ACCESS_SPECIFIC),
    (m: SERVICE_ENUMERATE_DEPENDENTS;  t: 'Enumerate dependents';  f: SI_ACCESS_SPECIFIC),
    (m: SERVICE_INTERROGATE;           t: 'Interrogate';           f: SI_ACCESS_SPECIFIC),
    (m: SERVICE_PAUSE_CONTINUE;        t: 'Pause/Continue';        f: SI_ACCESS_SPECIFIC),
    (m: SERVICE_QUERY_CONFIG;          t: 'Query config';          f: SI_ACCESS_SPECIFIC),
    (m: SERVICE_QUERY_STATUS;          t: 'Query status';          f: SI_ACCESS_SPECIFIC),
    (m: SERVICE_START;                 t: 'Start';                 f: SI_ACCESS_SPECIFIC),
    (m: SERVICE_STOP;                  t: 'Stop';                  f: SI_ACCESS_SPECIFIC),
    (m: SERVICE_USER_DEFINED_CONTROL;  t: 'User defined control';  f: SI_ACCESS_SPECIFIC),

    (m: ACCESS_SYSTEM_SECURITY;        t: 'Access system security'; f: SI_ACCESS_SPECIFIC),
    (m: _DELETE;                       t: 'Delete';                f: SI_ACCESS_SPECIFIC),
    (m: READ_CONTROL;                  t: 'Read control';          f: SI_ACCESS_SPECIFIC),
    (m: WRITE_DAC;                     t: 'Write DAC';             f: SI_ACCESS_SPECIFIC),
    (m: WRITE_OWNER;                   t: 'Write owner';           f: SI_ACCESS_SPECIFIC)
  );

class constructor TServiceSecurityInformation.Create;
var i: integer;
begin
  SetLength(ServiceAccessMasks, Length(SERVICE_ACCESS_PARAMS));
  for i := 0 to Length(SERVICE_ACCESS_PARAMS) do begin
    ServiceAccessMasks[i].pguid := nil;
    ServiceAccessMasks[i].mask := SERVICE_ACCESS_PARAMS[i].m;
    ServiceAccessMasks[i].pszName := PChar(SERVICE_ACCESS_PARAMS[i].t);
    ServiceAccessMasks[i].dwFlags := SERVICE_ACCESS_PARAMS[i].f;
  end;
end;

constructor TServiceSecurityInformation.Create(const AServiceName: string);
begin
  inherited Create;
  hSC := OpenSCManager();
  hSvc := OpenService(hSC, AServiceName);
end;

destructor TServiceSecurityInformation.Destroy;
begin
  if hSvc <> 0 then CloseServiceHandle(hSvc);
  if hSC <> 0 then CloseServiceHandle(hSC);
  inherited;
end;

function TServiceSecurityInformation.GetObjectInformation(out pObjectInfo: SI_OBJECT_INFO): HRESULT;
begin
  pObjectInfo.dwFlags := SI_ADVANCED or SI_EDIT_ALL;
  Result := S_OK;
end;

function TServiceSecurityInformation.GetSecurity(RequestedInformation: SECURITY_INFORMATION;
  out ppSecurityDescriptor: PSECURITY_DESCRIPTOR; fDefault: BOOL): HRESULT;
var err: integer;
  buf: NativeUInt;
  bufSz, bufNeeded: cardinal;
begin
  bufSz := 512;
  buf := LocalAlloc(LMEM_FIXED, bufSz);
  while true do begin
    if QueryServiceObjectSecurity(hSvc, RequestedInformation, pointer(buf), bufSz, bufNeeded) then begin
      ppSecurityDescriptor := PSECURITY_DESCRIPTOR(buf);
      Result := S_OK;
      exit;
    end;

    err := GetLastError();
    if (err <> ERROR_INSUFFICIENT_BUFFER) or (bufNeeded <= bufSz) then
      break;

    bufSz := bufNeeded;
    buf := LocalRealloc(buf, bufNeeded, LMEM_FIXED);
  end;

  LocalFree(buf);
  Result := HResultFromWin32(err);
end;

function TServiceSecurityInformation.SetSecurity(SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): HRESULT;
begin
  if SetServiceObjectSecurity(hSvc, SecurityInformation, pSecurityDescriptor) then
    Result := S_OK
  else
    Result := HResultFromWin32(GetLastError());
end;

function TServiceSecurityInformation.GetAccessRights(pguidObjectType: PGUID; dwFlags: DWORD;
  out ppAccess: PSI_ACCESS; out pcAccesses, piDefaultAccess: ULONG): HRESULT;
begin
  ppAccess := @Self.ServiceAccessMasks[0];
  pcAccesses := Length(Self.ServiceAccessMasks);
  piDefaultAccess := 0; //index of SERVICE_ALL_ACCESS
  Result := S_OK;
end;

function TServiceSecurityInformation.MapGeneric(pguidObjectType: PGUID; pAceFlags: PUCHAR;
  pMask: PACCESS_MASK): HRESULT;
begin
  MapGenericMask(pMask^, SERVICE_ACCESS_MAP);
  Result := S_OK;
end;

function TServiceSecurityInformation.GetInheritTypes(out ppInheritTypes: PSI_INHERIT_TYPE;
  out pcInheritTypes: ULONG): HRESULT;
begin
  ppInheritTypes := nil;
  pcInheritTypes := 0;
  Result := S_OK;
end;

function TServiceSecurityInformation.PropertySheetPageCallback(hwnd: HWND; uMsg: UINT;
  uPage: SI_PAGE_TYPE): HRESULT;
begin
  Result := S_OK;
end;

function EditServiceSecurity(hwndOwner: HWND; const ServiceName: String): boolean;
var secInfo: TServiceSecurityInformation;
begin
  secInfo := TServiceSecurityInformation.Create(ServiceName);
  Result := EditSecurity(hwndOwner, secInfo);
end;

end.
