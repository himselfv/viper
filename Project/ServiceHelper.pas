unit ServiceHelper;

interface
uses Windows, WinSvc, Registry;

const //New service types from winnt.h
  SERVICE_USER_SERVICE          = $00000040;
  SERVICE_USERSERVICE_INSTANCE  = $00000080;

  SERVICE_USER_SHARE_PROCESS    = (SERVICE_USER_SERVICE or
                                   SERVICE_WIN32_SHARE_PROCESS);
  SERVICE_USER_OWN_PROCESS      = (SERVICE_USER_SERVICE or
                                   SERVICE_WIN32_OWN_PROCESS);

  SERVICE_INTERACTIVE_PROCESS   = $00000100;
  SERVICE_PKG_SERVICE           = $00000200;

 //<=W7 does not support new W10 service types and will return ERROR_INVALID_PARAM,
 //so we keep <=W7 version under a different name so that people may try it.
  SERVICE_TYPE_ALL_W7           = (SERVICE_WIN32 or
                                   SERVICE_ADAPTER or
                                   SERVICE_DRIVER or
                                   SERVICE_INTERACTIVE_PROCESS);

  SERVICE_TYPE_ALL_W10          = (SERVICE_WIN32 or
                                   SERVICE_ADAPTER or
                                   SERVICE_DRIVER or
                                   SERVICE_INTERACTIVE_PROCESS or
                                   SERVICE_USER_SERVICE or
                                   SERVICE_USERSERVICE_INSTANCE or
                                   SERVICE_PKG_SERVICE);

 //We still redefine normal TYPE_ALL since that's what WinSvc.h does.
  SERVICE_TYPE_ALL              = SERVICE_TYPE_ALL_W10;

const
  SERVICE_READ_ACCESS = SERVICE_QUERY_CONFIG
     or SERVICE_QUERY_STATUS
     or SERVICE_ENUMERATE_DEPENDENTS;

  SERVICE_CONTROL_ACCESS = SERVICE_START
    or SERVICE_STOP
    or SERVICE_PAUSE_CONTINUE
    or SERVICE_INTERROGATE;

  SERVICE_WRITE_ACCESS = SERVICE_CHANGE_CONFIG;

function OpenSCManager(dwAccess: cardinal = SC_MANAGER_ALL_ACCESS): SC_HANDLE;

function OpenService(hSC: SC_HANDLE; const AServiceName: string; dwAccess: cardinal = SC_MANAGER_ALL_ACCESS): SC_HANDLE;

function EnumServicesStatus(hSC: SC_HANDLE; ServiceTypes, ServiceState: DWORD;
  out Services: PEnumServiceStatus; out ServicesReturned: cardinal): boolean;

function QueryServiceStatus(hSvc: SC_HANDLE): SERVICE_STATUS; overload;
function QueryServiceStatus(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS; overload;
function QueryServiceStatus(const AServiceName: string): SERVICE_STATUS; overload;

//Result has to be freed
function QueryServiceConfig(hSvc: SC_HANDLE): LPQUERY_SERVICE_CONFIG; overload;
function QueryServiceConfig(hSC: SC_HANDLE; const AServiceName: string): LPQUERY_SERVICE_CONFIG; overload;

const
  SERVICE_CONFIG_LAUNCH_PROTECTED = 12; //Since windows 8

const
  SERVICE_LAUNCH_PROTECTED_NONE = 0;
  SERVICE_LAUNCH_PROTECTED_WINDOWS = 1;
  SERVICE_LAUNCH_PROTECTED_WINDOWS_LIGHT = 2;
  SERVICE_LAUNCH_PROTECTED_ANTIMALWARE_LIGHT = 3;

type
  PSERVICE_LAUNCH_PROTECTED = ^SERVICE_LAUNCH_PROTECTED;
  SERVICE_LAUNCH_PROTECTED = record
    dwLaunchProtected: DWORD;
  end;

//Result has to be freed
function QueryServiceConfig2(hSvc: SC_HANDLE; dwInfoLevel: cardinal): PByte; overload;
function QueryServiceConfig2(hSC: SC_HANDLE; const AServiceName: string; dwInfoLevel: cardinal): PByte; overload;

function QueryServiceDescription(hSvc: SC_HANDLE): string; overload;
function QueryServiceDescription(hSC: SC_HANDLE; const AServiceName: string): string; overload;


const // New triggers from WinSvc.h
  SERVICE_TRIGGER_TYPE_NETWORK_ENDPOINT                 = 6;
  SERVICE_TRIGGER_TYPE_CUSTOM_SYSTEM_STATE_CHANGE       = 7;
  SERVICE_TRIGGER_TYPE_CUSTOM                           = 20;
  SERVICE_TRIGGER_TYPE_AGGREGATE                        = 30;

const
  SERVICE_TRIGGER_DATA_TYPE_LEVEL          = 3;
  SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ANY    = 4;
  SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ALL    = 5;

const
  RPC_INTERFACE_EVENT_GUID: TGuid = '{BC90D167-9470-4139-A9BA-BE0BBBF5B74D}';
  NAMED_PIPE_EVENT_GUID: TGuid = '{1F81D131-3FAC-4537-9E0C-7E7B0C2F4B55}';
  CUSTOM_SYSTEM_STATE_CHANGE_EVENT_GUID: TGuid = '{2d7a2816-0c5e-45fc-9ce7-570e5ecde9c9}'; //used with TRIGGER_TYPE_CUSTOM_SYSTEM_STATE_CHANGE

//
// Service notification trigger identifier
//
type
  SERVICE_TRIGGER_CUSTOM_STATE_ID = packed record
    Data: array[0..1] of DWORD;
  end;
  SERVICE_CUSTOM_SYSTEM_STATE_CHANGE_DATA_ITEM = packed record
  case byte of
    0: (CustomStateId: SERVICE_TRIGGER_CUSTOM_STATE_ID);
    1: (
      DataOffset: DWORD;
      Data: array[0..0] of Byte;
    );
  end;

const
  SC_AGGREGATE_STORAGE_KEY = 'System\CurrentControlSet\Control\ServiceAggregatedEvents';

function QueryServiceTriggers(hSvc: SC_HANDLE): PSERVICE_TRIGGER_INFO; overload;
function QueryServiceTriggers(hSC: SC_HANDLE; const AServiceName: string): PSERVICE_TRIGGER_INFO; overload;

function QueryServiceLaunchProtected(hSvc: SC_HANDLE): PSERVICE_LAUNCH_PROTECTED; overload;
function QueryServiceLaunchProtected(hSC: SC_HANDLE; const AServiceName: string): PSERVICE_LAUNCH_PROTECTED; overload;


//Opens a configuration key for this service in the registry. The result has to be freed.
function OpenServiceKey(const AServiceName: string): TRegistry;

//Services which have svchost as their executable support additional parameter specifying
//the DLL to load.
function QueryServiceServiceDll(const AServiceName: string): string;


procedure StartService(hSvc: SC_HANDLE); overload;
procedure StartService(hSC: SC_HANDLE; const AServiceName: string); overload;
procedure StartService(const AServiceName: string); overload;

function ControlService(hSvc: SC_HANDLE; dwControl: cardinal): SERVICE_STATUS; overload;
function ControlService(hSC: SC_HANDLE; const AServiceName: string; dwControl: cardinal): SERVICE_STATUS; overload;
function ControlService(const AServiceName: string; dwControl: cardinal): SERVICE_STATUS; overload;

function StopService(hSvc: SC_HANDLE): SERVICE_STATUS; overload;
function StopService(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS; overload;
function StopService(const AServiceName: string): SERVICE_STATUS; overload;
function PauseService(hSvc: SC_HANDLE): SERVICE_STATUS; overload;
function PauseService(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS; overload;
function PauseService(const AServiceName: string): SERVICE_STATUS; overload;
function ContinueService(hSvc: SC_HANDLE): SERVICE_STATUS; overload;
function ContinueService(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS; overload;
function ContinueService(const AServiceName: string): SERVICE_STATUS; overload;


procedure ChangeServiceStartType(hSvc: SC_HANDLE; dwStartType: DWORD); overload;
procedure ChangeServiceStartType(hSC: SC_HANDLE; const AServiceName: string; dwStartType: DWORD); overload;
procedure ChangeServiceStartType(const AServiceName: string; dwStartType: DWORD); overload;

procedure ChangeServiceLaunchProtected(hSvc: SC_HANDLE; dwLaunchProtection: DWORD); overload;
procedure ChangeServiceLaunchProtected(hSC: SC_HANDLE; const AServiceName: string; dwLaunchProtection: DWORD); overload;
procedure ChangeServiceLaunchProtected(const AServiceName: string; dwLaunchProtection: DWORD); overload;

procedure OverwriteServiceLaunchProtection(const AServiceName: string; dwLaunchProtection: DWORD);

function EnumDependentServices(hSvc: SC_HANDLE; dwServiceState: DWORD; out ServiceCount: cardinal): LPENUM_SERVICE_STATUS;


implementation
uses SysUtils;

const
  ERROR_MUI_FILE_NOT_FOUND = 15100;

function OpenSCManager(dwAccess: cardinal = SC_MANAGER_ALL_ACCESS): SC_HANDLE;
begin
  Result := WinSvc.OpenSCManager(nil, nil, dwAccess);
  if Result = 0 then
    RaiseLastOsError();
end;


function OpenService(hSC: SC_HANDLE; const AServiceName: string; dwAccess: cardinal): SC_HANDLE;
var err: integer;
begin
  Result := WinSvc.OpenService(hSC, PChar(AServiceName), dwAccess);
  if Result = 0 then begin
    err := GetLastError();
    if err = ERROR_ACCESS_DENIED then
      exit; //as an exception, do not raise
    RaiseLastOsError(err);
  end;
end;

//Enumerates services and their status. The result has to be freed with FreeMem.
//Returns nil on error, query GetLastError for details.
function EnumServicesStatus(hSC: SC_HANDLE; ServiceTypes, ServiceState: DWORD;
  out Services: PEnumServiceStatus; out ServicesReturned: cardinal): boolean;
var BytesNeeded, ResumeHandle: DWORD;
begin
  Result := false;
  Services := nil;
  ServicesReturned := 0;
  ResumeHandle := 0;
  if WinSvc.EnumServicesStatus(hSC, ServiceTypes, SERVICE_STATE_ALL,
     Services^, 0, BytesNeeded, ServicesReturned, ResumeHandle) then exit; //no services
  if GetLastError <> ERROR_MORE_DATA then
    exit;

  GetMem(Services, BytesNeeded);
  ServicesReturned := 0;
  ResumeHandle := 0;
  if not WinSvc.EnumServicesStatus(hSC, ServiceTypes, SERVICE_STATE_ALL,
    Services^, BytesNeeded, BytesNeeded, ServicesReturned, ResumeHandle) then
  begin
    FreeMem(Services); //dont screw LastError plz
    exit;
  end;
  Result := true;
end;


function QueryServiceStatus(hSvc: SC_HANDLE): SERVICE_STATUS;
begin
  if not WinSvc.QueryServiceStatus(hSvc, Result) then
    RaiseLastOsError();
end;

function QueryServiceStatus(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_QUERY_STATUS);
  if hSvc = 0 then FillChar(Result, sizeof(Result), 0) else
  try
    Result := QueryServiceStatus(hSvc);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

function QueryServiceStatus(const AServiceName: string): SERVICE_STATUS;
var hSC: SC_HANDLE;
begin
  hSC := OpenSCManager(SC_MANAGER_CONNECT);
  try
    Result := QueryServiceStatus(hSC, AServiceName);
  finally
    CloseServiceHandle(hSC);
  end;
end;


function QueryServiceConfig(hSvc: SC_HANDLE): LPQUERY_SERVICE_CONFIG;
var lastSize, bufSize: cardinal;
  err: integer;
begin
  Result := nil;
  bufSize := 0;
  lastSize := 0;
  while not WinSvc.QueryServiceConfig(hSvc, Result, lastSize, bufSize) do begin
    err := GetLastError();
    if err <> ERROR_INSUFFICIENT_BUFFER then
      RaiseLastOsError(err);
    if bufSize <= lastSize then //avoid infinite cycle
      RaiseLastOsError(err);
    ReallocMem(Result, bufSize);
    lastSize := bufSize;
  end;
end;

function QueryServiceConfig(hSC: SC_HANDLE; const AServiceName: string): LPQUERY_SERVICE_CONFIG;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_QUERY_CONFIG);
  if hSvc = 0 then Result := nil else
  try
    Result := QueryServiceConfig(hSvc);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

function QueryServiceConfig2(hSvc: SC_HANDLE; dwInfoLevel: cardinal): PByte;
var lastSize, bufSize: cardinal;
  err: integer;
begin
  Result := nil;
  bufSize := 0;
  lastSize := 0;
  while not WinSvc.QueryServiceConfig2(hSvc, dwInfoLevel, Result, lastSize, @bufSize) do begin
    err := GetLastError();
    case err of
      ERROR_INSUFFICIENT_BUFFER: begin
        if bufSize <= lastSize then //avoid infinite cycle
          RaiseLastOsError(err);
       //otherwise fall through and realloc
      end;
     //Might return FILE_NOT_FOUND or RESOURCE_TYPE_NOT_FOUND if the description is EXPAND_SZ, e.g.
     //  %SystemRoot%\lib.dll,15
      ERROR_FILE_NOT_FOUND,
      ERROR_RESOURCE_DATA_NOT_FOUND,
      ERROR_RESOURCE_TYPE_NOT_FOUND,
      ERROR_RESOURCE_NAME_NOT_FOUND,
      ERROR_MUI_FILE_NOT_FOUND: begin
       //TODO: We can sometimes query the source string ourselves (like Autoruns does).
        Result := nil;
        break;
      end;
    else RaiseLastOsError();
    end;
    ReallocMem(Result, bufSize);
    lastSize := bufSize;
  end;
end;

function QueryServiceConfig2(hSC: SC_HANDLE; const AServiceName: string; dwInfoLevel: cardinal): PByte;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_QUERY_CONFIG);
  if hSvc = 0 then Result := nil else
  try
    Result := QueryServiceConfig2(dwInfoLevel, hSvc);
  finally
    CloseServiceHandle(hSvc);
  end;
end;


function QueryServiceDescription(hSvc: SC_HANDLE): string;
var buf: LPSERVICE_DESCRIPTION;
begin
  buf := LPSERVICE_DESCRIPTION(QueryServiceConfig2(hSvc, SERVICE_CONFIG_DESCRIPTION));
  if buf = nil then begin
    Result := '';
    exit;
  end;
  Result := buf.lpDescription;
  FreeMem(buf);
end;

function QueryServiceDescription(hSC: SC_HANDLE; const AServiceName: string): string;
var buf: LPSERVICE_DESCRIPTION;
begin
  buf := LPSERVICE_DESCRIPTION(QueryServiceConfig2(hSC, AServiceName, SERVICE_CONFIG_DESCRIPTION));
  if buf = nil then begin
    Result := '';
    exit;
  end;
  Result := buf.lpDescription;
  FreeMem(buf);
end;


function QueryServiceTriggers(hSvc: SC_HANDLE): PSERVICE_TRIGGER_INFO;
begin
  Result := PSERVICE_TRIGGER_INFO(QueryServiceConfig2(hSvc, SERVICE_CONFIG_TRIGGER_INFO));
end;

function QueryServiceTriggers(hSC: SC_HANDLE; const AServiceName: string): PSERVICE_TRIGGER_INFO;
begin
  Result := PSERVICE_TRIGGER_INFO(QueryServiceConfig2(hSC, AServiceName, SERVICE_CONFIG_TRIGGER_INFO));
end;

function QueryServiceLaunchProtected(hSvc: SC_HANDLE): PSERVICE_LAUNCH_PROTECTED;
begin
  Result := PSERVICE_LAUNCH_PROTECTED(QueryServiceConfig2(hSvc, SERVICE_CONFIG_LAUNCH_PROTECTED));
end;

function QueryServiceLaunchProtected(hSC: SC_HANDLE; const AServiceName: string): PSERVICE_LAUNCH_PROTECTED;
begin
  Result := PSERVICE_LAUNCH_PROTECTED(QueryServiceConfig2(hSC, AServiceName, SERVICE_CONFIG_LAUNCH_PROTECTED));
end;



function OpenServiceKey(const AServiceName: string): TRegistry;
begin
  Result := TRegistry.Create;
  try
    Result.RootKey := HKEY_LOCAL_MACHINE;
    Result.Access := KEY_READ;
    if not Result.OpenKey('\System\CurrentControlSet\services\'+AServiceName, false) then
      RaiseLastOsError();
  except
    FreeAndNil(Result);
    raise;
  end;
end;

//Old version, supposedly slower
function QueryServiceServiceDll2(const AServiceName: string): string;
var reg: TRegistry;
begin
  reg := OpenServiceKey(AServiceName);
  try
    if not reg.OpenKey('Parameters', false) then
      Result := ''
    else
      Result := reg.ReadString('ServiceDll');
  finally
    FreeAndNil(reg);
  end;
end;

function RegQueryValueSz(hKey: HKEY; ValueName: string; out Value: string): cardinal;
var sz: cardinal;
  valType: cardinal;
begin
  sz := MAX_PATH;
  SetLength(Value, sz);

  Result := RegQueryValueEx(hKey, PChar(ValueName), nil, @valType, @Value[1], @sz);
  if Result = ERROR_MORE_DATA then begin
    SetLength(Value, sz);
    Result := RegQueryValueEx(hKey, PChar(ValueName), nil, @valType, @Value[1], @sz);
  end;

  if Result = 0 then begin
    if (valType <> REG_SZ) and (valType <> REG_EXPAND_SZ) then
      Result := ERROR_INVALID_DATATYPE;
    SetLength(Value, StrLen(PChar(Value)));
  end;
end;

function QueryServiceServiceDll(const AServiceName: string): string;
var hk: HKEY;
begin
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar('System\CurrentControlSet\services\'
    +AServiceName+'\Parameters'), 0, KEY_QUERY_VALUE, hk) <> 0 then begin
    Result := '';
    exit;
  end;
  try
    if RegQueryValueSz(hk, 'ServiceDll', Result) <> 0 then
      Result := '';
  finally
    RegCloseKey(HKEY_LOCAL_MACHINE);
  end;
end;



procedure StartService(hSvc: SC_HANDLE);
var args: PWideChar;
begin
  args := nil;
  if not WinSvc.StartService(hSvc, 0, args) then
    RaiseLastOsError();
end;

procedure StartService(hSC: SC_HANDLE; const AServiceName: string);
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_START);
  if hSvc = 0 then RaiseLastOsError() else
  try
    StartService(hSvc);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

procedure StartService(const AServiceName: string); overload;
var hSC: SC_HANDLE;
begin
  hSC := OpenSCManager(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
  try
    StartService(hSC, AServiceName);
  finally
    CloseServiceHandle(hSC);
  end;
end;


function ControlService(hSvc: SC_HANDLE; dwControl: cardinal): SERVICE_STATUS;
begin
  if not WinSvc.ControlService(hSvc, dwControl, Result) then
    RaiseLastOsError();
end;

function ControlService(hSC: SC_HANDLE; const AServiceName: string; dwControl: cardinal): SERVICE_STATUS;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_CONTROL_ACCESS);
  if hSvc = 0 then RaiseLastOsError() else
  try
    Result := ControlService(hSvc, dwControl);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

function ControlService(const AServiceName: string; dwControl: cardinal): SERVICE_STATUS; overload;
var hSC: SC_HANDLE;
begin
  hSC := OpenSCManager(SC_MANAGER_CONNECT);
  try
    ControlService(hSC, AServiceName, dwControl);
  finally
    CloseServiceHandle(hSC);
  end;
end;


function StopService(hSvc: SC_HANDLE): SERVICE_STATUS;
begin
  Result := ControlService(hSvc, SERVICE_CONTROL_STOP);
end;

function StopService(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS;
begin
  Result := ControlService(hSC, AServiceName, SERVICE_CONTROL_STOP);
end;

function StopService(const AServiceName: string): SERVICE_STATUS;
begin
  Result := ControlService(AServiceName, SERVICE_CONTROL_STOP);
end;

function PauseService(hSvc: SC_HANDLE): SERVICE_STATUS;
begin
  Result := ControlService(hSvc, SERVICE_CONTROL_PAUSE);
end;

function PauseService(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS;
begin
  Result := ControlService(hSC, AServiceName, SERVICE_CONTROL_PAUSE);
end;

function PauseService(const AServiceName: string): SERVICE_STATUS;
begin
  Result := ControlService(AServiceName, SERVICE_CONTROL_PAUSE);
end;

function ContinueService(hSvc: SC_HANDLE): SERVICE_STATUS;
begin
  Result := ControlService(hSvc, SERVICE_CONTROL_CONTINUE);
end;

function ContinueService(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS;
begin
  Result := ControlService(hSC, AServiceName, SERVICE_CONTROL_CONTINUE);
end;

function ContinueService(const AServiceName: string): SERVICE_STATUS;
begin
  Result := ControlService(AServiceName, SERVICE_CONTROL_CONTINUE);
end;


procedure ChangeServiceStartType(hSvc: SC_HANDLE; dwStartType: DWORD); overload;
begin
  if not ChangeServiceConfig(hSvc, SERVICE_NO_CHANGE, dwStartType, SERVICE_NO_CHANGE, nil, nil,
    nil, nil, nil, nil, nil) then
    RaiseLastOsError();
end;

procedure ChangeServiceStartType(hSC: SC_HANDLE; const AServiceName: string; dwStartType: DWORD); overload;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_CHANGE_CONFIG);
  if hSvc = 0 then RaiseLastOsError() else
  try
    ChangeServiceStartType(hSvc, dwStartType);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

procedure ChangeServiceStartType(const AServiceName: string; dwStartType: DWORD); overload;
var hSC: SC_HANDLE;
begin
  hSC := OpenSCManager(SC_MANAGER_ALL_ACCESS);
  try
    ChangeServiceStartType(hSC, AServiceName, dwStartType);
  finally
    CloseServiceHandle(hSC);
  end;
end;

procedure ChangeServiceLaunchProtected(hSvc: SC_HANDLE; dwLaunchProtection: DWORD);
var lp: SERVICE_LAUNCH_PROTECTED;
begin
  lp.dwLaunchProtected := dwLaunchProtection;
  if not ChangeServiceConfig2(hSvc, SERVICE_CONFIG_LAUNCH_PROTECTED, @lp) then
    RaiseLastOsError();
end;

procedure ChangeServiceLaunchProtected(hSC: SC_HANDLE; const AServiceName: string; dwLaunchProtection: DWORD);
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_CHANGE_CONFIG);
  if hSvc = 0 then RaiseLastOsError() else
  try
    ChangeServiceLaunchProtected(hSvc, dwLaunchProtection);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

procedure ChangeServiceLaunchProtected(const AServiceName: string; dwLaunchProtection: DWORD);
var hSC: SC_HANDLE;
begin
  hSC := OpenSCManager(SC_MANAGER_ALL_ACCESS);
  try
    ChangeServiceStartType(hSC, AServiceName, dwLaunchProtection);
  finally
    CloseServiceHandle(hSC);
  end;
end;

{
https://msdn.microsoft.com/en-us/library/windows/desktop/dn313124(v=vs.85).aspx

After the service has been configured to launch protected, you cannot change it back.

This circumvents the "feature" by writing directly to registry service configuration. You have to
have appropriate access rights to that key.
You'll probably have to reboot.
}
procedure OverwriteServiceLaunchProtection(const AServiceName: string; dwLaunchProtection: DWORD);
var hk: HKEY;
  err: integer;
begin
  err := RegOpenKey(HKEY_LOCAL_MACHINE, PChar('SYSTEM\CurrentControlSet\Services\'+AServiceName), hk);
  if err <> 0 then
    RaiseLastOsError(err);
  try
    err := RegSetValueEx(hk, PChar('LaunchProtected'), 0, REG_DWORD, @dwLaunchProtection, SizeOf(dwLaunchProtection));
    if err <> 0 then
      RaiseLastOsError(err);
  finally
    RegCloseKey(hk);
  end;
end;


//Returns the list of services dependent on a given service. The list has to be freed.
function EnumDependentServices(hSvc: SC_HANDLE; dwServiceState: DWORD; out ServiceCount: cardinal): LPENUM_SERVICE_STATUS;
var bytesNeeded: cardinal;
begin
  Result := nil;
  bytesNeeded := 0;

  if WinSvc.EnumDependentServices(hSvc, dwServiceState, Result^, 0, bytesNeeded, ServiceCount) then begin
    Result := nil;
    exit;
  end;

  if GetLastError <> ERROR_MORE_DATA then
    RaiseLastOsError();

  GetMem(Result, bytesNeeded);
  if not WinSvc.EnumDependentServices(hSvc, dwServiceState, Result^, bytesNeeded, bytesNeeded, ServiceCount) then
   //We could've adjusted again but risk going into an endless loop
    RaiseLastOsError();
end;





end.
