unit SecEdit;

interface
uses Windows, AclUi, AclHelpers, WinSvc, ServiceHelper, SvcEntry;

type
  TServiceSecurityInformation = class(TInterfacedObject, ISecurityInformation)
  class var
    ServiceAccessMasks: array of SI_ACCESS;
    ServiceAccessNames: array of string;
    class constructor Create;
  protected
    FServiceName: string;
    hSC: SC_HANDLE;
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
  https://msdn.microsoft.com/en-us/library/windows/desktop/gg983193(v=vs.85).aspx

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
uses SysUtils, Viper.Log;

resourcestring
  sSvcAccessAll       = 'Полный доступ';
  sSvcAccessRead      = 'Чтение';
  sSvcAccessExecute   = 'Пуск, стоп и пауза';
  sSvcAccessWrite     = 'Запись';

  sSvcAccessQueryConfig    = 'Чтение настроек';
  sSvcAccessChangeConfig   = 'Изменение настроек';
  sSvcAccessQueryStatus    = 'Запрос состояния';
  sSvcAccessEnumDependents = 'Перечисление зависимостей';
  sSvcAccessStart          = 'Пуск';
  sSvcAccessStop           = 'Стоп';
  sSvcAccessPauseContinue  = 'Пауза и возобновление';
  sSvcAccessInterrogate    = 'Опрос';
  sSvcAccessUserDefinedCtl = 'Иные команды';

  sCmnAccessRead           = 'Чтение разрешений';
  sCmnAccessWriteDac       = 'Смена разрешений';
  sCmnAccessWriteOwner     = 'Смена владельца';
  sCmnAccessDelete         = 'Удаление';

class constructor TServiceSecurityInformation.Create;
var i: integer;
  procedure Push(m: ACCESS_MASK; const t: string; f: DWORD);
  begin
    ServiceAccessNames[i] := t; //we have to keep this somewhere or it'll be freed
    ServiceAccessMasks[i].pguid := nil;
    ServiceAccessMasks[i].mask := m;
    ServiceAccessMasks[i].pszName := PChar(ServiceAccessNames[i]);
    ServiceAccessMasks[i].dwFlags := f;
    Inc(i);
  end;

begin
  SetLength(ServiceAccessNames, 17);
  SetLength(ServiceAccessMasks, 17);
  i := 0;
 //We'd prefer to have a static precompiled table, but alas, resourcestrings cannot be referenced correctly in static
  Push(SERVICE_ALL_ACCESS,            sSvcAccessAll,              SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC);

  Push(STANDARD_RIGHTS_READ,          sSvcAccessRead,             SI_ACCESS_GENERAL);
  Push(STANDARD_RIGHTS_EXECUTE,       sSvcAccessExecute,          SI_ACCESS_GENERAL);
  Push(STANDARD_RIGHTS_WRITE,         sSvcAccessWrite,            SI_ACCESS_GENERAL);

  Push(SERVICE_QUERY_CONFIG,          sSvcAccessQueryConfig,      SI_ACCESS_SPECIFIC);
  Push(SERVICE_CHANGE_CONFIG,         sSvcAccessChangeConfig,     SI_ACCESS_SPECIFIC);
  Push(SERVICE_QUERY_STATUS,          sSvcAccessQueryStatus,      SI_ACCESS_SPECIFIC);
  Push(SERVICE_ENUMERATE_DEPENDENTS,  sSvcAccessEnumDependents,   SI_ACCESS_SPECIFIC);
  Push(SERVICE_START,                 sSvcAccessStart,            SI_ACCESS_SPECIFIC);
  Push(SERVICE_STOP,                  sSvcAccessStop,             SI_ACCESS_SPECIFIC);
  Push(SERVICE_PAUSE_CONTINUE,        sSvcAccessPauseContinue,    SI_ACCESS_SPECIFIC);
  Push(SERVICE_INTERROGATE,           sSvcAccessInterrogate,      SI_ACCESS_SPECIFIC);
  Push(SERVICE_USER_DEFINED_CONTROL,  sSvcAccessUserDefinedCtl,   SI_ACCESS_SPECIFIC);
  Push(_DELETE,                       sCmnAccessDelete,             SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC);

  Push(READ_CONTROL,                  sCmnAccessRead,             SI_ACCESS_SPECIFIC);
  Push(WRITE_DAC,                     sCmnAccessWriteDac,         SI_ACCESS_SPECIFIC);
  Push(WRITE_OWNER,                   sCmnAccessWriteOwner,       SI_ACCESS_SPECIFIC);

 //This one cannot be set as access right... I think
//  Push(ACCESS_SYSTEM_SECURITY,        'Access system security',   SI_ACCESS_SPECIFIC),
end;

constructor TServiceSecurityInformation.Create(const AServiceName: string);
begin
  inherited Create;
  Log('SecurityInformation.Create('+AServiceName+')');
  FServiceName := AServiceName;
  hSC := OpenSCManager(SC_MANAGER_CONNECT);
end;

destructor TServiceSecurityInformation.Destroy;
begin
  if hSC <> 0 then CloseServiceHandle(hSC);
  Log('SecurityInformation.Destroy');
  inherited;
end;

function TServiceSecurityInformation.GetObjectInformation(out pObjectInfo: SI_OBJECT_INFO): HRESULT;
begin
  Log('SecurityInformation.GetObjectInformation');
  pObjectInfo.dwFlags := SI_ADVANCED or SI_EDIT_ALL;
  pObjectInfo.pszObjectName := PChar(FServiceName);
  Result := S_OK;
end;

function TServiceSecurityInformation.GetSecurity(RequestedInformation: SECURITY_INFORMATION;
  out ppSecurityDescriptor: PSECURITY_DESCRIPTOR; fDefault: BOOL): HRESULT;
var err: integer;
  buf: NativeUInt;
  bufSz, bufNeeded: cardinal;
  hSvc: SC_HANDLE;
begin
  Log('SecurityInformation.GetSecurity');
  hSvc := OpenService(hSC, FServiceName, READ_CONTROL);
  if hSvc = 0 then begin
    Result := HResultFromWin32(GetLastError());
    Log('SecurityInformation.GetSecurity: 0x'+IntToHex(Result, 8)+' on OpenService');
    exit;
  end;
  try

    bufSz := 512;
    buf := LocalAlloc(LMEM_FIXED, bufSz);
    while true do begin
      if QueryServiceObjectSecurity(hSvc, RequestedInformation, pointer(buf), bufSz, bufNeeded) then begin
        ppSecurityDescriptor := PSECURITY_DESCRIPTOR(buf);
        Log('SecurityInformation.GetSecurity: S_OK');
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
    Log('SecurityInformation.GetSecurity: 0x'+IntToHex(Result, 8));

  finally
    if hSvc <> 0 then CloseServiceHandle(hSvc);
  end;
end;

function TServiceSecurityInformation.SetSecurity(SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): HRESULT;
var hProcToken: THandle;
  hSvc: SC_HANDLE;
  requiredRights: DWORD;
begin
  Log('SecurityInformation.SetSecurity...');
  hProcToken := 0;
  hSvc := 0;
  try
    case SecurityInformation of
      OWNER_SECURITY_INFORMATION,
      GROUP_SECURITY_INFORMATION:
        requiredRights := WRITE_OWNER;
      DACL_SECURITY_INFORMATION,
      SACL_SECURITY_INFORMATION:
        requiredRights := WRITE_DAC;
    else requiredRights := WRITE_OWNER or WRITE_DAC; //tiny future proofing
    end;

    if requiredRights and WRITE_OWNER <> 0 then
      //Auto-try to get SE_TAKE_OWNERSHIP_NAME just in case we don't have WRITE_OWNER
      if not OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES, hProcToken)
      or not SetPrivilege(hProcToken, SE_TAKE_OWNERSHIP_NAME, true) then begin
        CloseHandle(hProcToken);
        hProcToken := 0; //or we'll try to clear the privilege
        Log('SecurityInformation.SetSecurity: Cannot get SE_TAKE_OWNERSHIP: '+IntToStr(GetLastError()));
      end;

    hSvc := OpenService(hSC, FServiceName, requiredRights);
    if hSvc = 0 then begin
      Result := HResultFromWin32(GetLastError());
      Log('SecurityInformation.GetSecurity: 0x'+IntToHex(Result, 8)+' on OpenService');
      exit;
    end;

    if SetServiceObjectSecurity(hSvc, SecurityInformation, pSecurityDescriptor) then
      Result := S_OK
    else
      Result := HResultFromWin32(GetLastError());
    Log('SecurityInformation.SetSecurity: 0x'+IntToHex(Result, 8));

  finally
    if hProcToken <> 0 then begin
      SetPrivilege(hProcToken, SE_TAKE_OWNERSHIP_NAME, false);
      CloseHandle(hProcToken);
    end;
    if hSvc <> 0 then CloseServiceHandle(hSvc);
  end;
end;

function TServiceSecurityInformation.GetAccessRights(pguidObjectType: PGUID; dwFlags: DWORD;
  out ppAccess: PSI_ACCESS; out pcAccesses, piDefaultAccess: ULONG): HRESULT;
begin
  Log('SecurityInformation.GetAccessRights');
  ppAccess := @Self.ServiceAccessMasks[0];
  pcAccesses := Length(Self.ServiceAccessMasks);
  piDefaultAccess := 0; //index of SERVICE_ALL_ACCESS
  Result := S_OK;
end;

function TServiceSecurityInformation.MapGeneric(pguidObjectType: PGUID; pAceFlags: PUCHAR;
  pMask: PACCESS_MASK): HRESULT;
begin
  Log('SecurityInformation.MapGeneric');
  MapGenericMask(pMask^, SERVICE_ACCESS_MAP);
  Result := S_OK;
end;

function TServiceSecurityInformation.GetInheritTypes(out ppInheritTypes: PSI_INHERIT_TYPE;
  out pcInheritTypes: ULONG): HRESULT;
begin
  Log('SecurityInformation.GetInheritTypes');
  ppInheritTypes := nil;
  pcInheritTypes := 0;
  Result := E_NOTIMPL;
end;

function TServiceSecurityInformation.PropertySheetPageCallback(hwnd: HWND; uMsg: UINT;
  uPage: SI_PAGE_TYPE): HRESULT;
begin
  Log('SecurityInformation.PropertySheetPageCallback');
  Result := S_OK;
end;

function EditServiceSecurity(hwndOwner: HWND; const ServiceName: String): boolean;
var secInfo: TServiceSecurityInformation;
begin
  secInfo := TServiceSecurityInformation.Create(ServiceName);
  Result := EditSecurity(hwndOwner, secInfo);
end;

end.
