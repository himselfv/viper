unit ServiceHelper;

interface
uses Windows, WinSvc, Registry;

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

function QueryServiceStatus(hSvc: SC_HANDLE): SERVICE_STATUS; overload;
function QueryServiceStatus(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS; overload;
function QueryServiceStatus(const AServiceName: string): SERVICE_STATUS; overload;

//Result has to be freed
function QueryServiceConfig(hSvc: SC_HANDLE): LPQUERY_SERVICE_CONFIG; overload;
function QueryServiceConfig(hSC: SC_HANDLE; const AServiceName: string): LPQUERY_SERVICE_CONFIG; overload;

//Result has to be freed
function QueryServiceConfig2(hSvc: SC_HANDLE; dwInfoLevel: cardinal): PByte; overload;
function QueryServiceConfig2(hSC: SC_HANDLE; const AServiceName: string; dwInfoLevel: cardinal): PByte; overload;

function QueryServiceDescription(hSvc: SC_HANDLE): string; overload;
function QueryServiceDescription(hSC: SC_HANDLE; const AServiceName: string): string; overload;

function QueryServiceTriggers(hSvc: SC_HANDLE): PSERVICE_TRIGGER_INFO; overload;
function QueryServiceTriggers(hSC: SC_HANDLE; const AServiceName: string): PSERVICE_TRIGGER_INFO; overload;


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


function EnumDependentServices(hSvc: SC_HANDLE; dwServiceState: DWORD; out ServiceCount: cardinal): LPENUM_SERVICE_STATUS;


const
  SERVICE_TRIGGER_TYPE_NETWORK_ENDPOINT = 6; //missing in headers

implementation
uses SysUtils;

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
      ERROR_RESOURCE_NAME_NOT_FOUND: begin
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
