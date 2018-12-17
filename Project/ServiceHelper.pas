unit ServiceHelper;

interface
uses Windows, WinSvc, Registry;

const //New service types from winnt.h
  SERVICE_USER_SERVICE          = $00000040;                        //set for user service prototypes AND instances
  SERVICE_USERSERVICE_INSTANCE  = $00000080;                        //set only for instances

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

//Raises an OS error in case of error
procedure ScmCheck(const AErrorCode: integer); overload; inline;
procedure ScmCheck(const AErrorCode: integer; const AAdditionalInfo: string); overload; inline;

//Opens SCM
function OpenSCManager(dwAccess: cardinal = SC_MANAGER_ALL_ACCESS): SC_HANDLE;
function OpenService(hSC: SC_HANDLE; const AServiceName: string; dwAccess: cardinal = SC_MANAGER_ALL_ACCESS): SC_HANDLE; overload;
procedure OpenScmAndService(out hSC: SC_HANDLE; out hSvc: SC_HANDLE;
  const AServiceName: string; dwScmAccess: cardinal = SC_MANAGER_ALL_ACCESS;
  dwServiceAccess: cardinal = SC_MANAGER_ALL_ACCESS);

{
RAAI service manager / service access.
Usage:
  with OpenScManager(RIGHTS) do
    DoScManagerAction(ScmHandle)
  with OpenService('ServiceName', RIGHTS) do
    DoServiceAction(SvcHandle);
}

type
  IAutoScm = interface
    function ScmHandle: SC_HANDLE;
  end;
  TAutoScm = class(TInterfacedObject, IAutoScm)
  protected
    FScmHandle: SC_HANDLE;
  public
    constructor Create(AScmHandle: SC_HANDLE);
    destructor Destroy; override;
    function ScmHandle: SC_HANDLE;
  end;

  IAutoService = interface
    function ScmHandle: SC_HANDLE;
    function SvcHandle: SC_HANDLE;
  end;
  TAutoService = class(TInterfacedObject, IAutoService)
  protected
    FScmHandle: SC_HANDLE;
    FSvcHandle: SC_HANDLE;
    FOwnsScmHandle: boolean;
  public
    constructor Create(AScmHandle, ASvcHandle: SC_HANDLE; AOwnsScmHandle: boolean);
    destructor Destroy; override;
    function ScmHandle: SC_HANDLE;
    function SvcHandle: SC_HANDLE;
  end;

function OpenScm(dwAccess: cardinal = SC_MANAGER_ALL_ACCESS): IAutoScm;
function OpenService2(const AServiceName: string; dwScmAccess: cardinal = SC_MANAGER_ALL_ACCESS;
  dwServiceAccess: cardinal = SERVICE_ALL_ACCESS): IAutoService; overload;
function OpenService2(hSC: SC_HANDLE; const AServiceName: string;
  dwServiceAccess: cardinal = SERVICE_ALL_ACCESS): IAutoService; overload;


{ Enumeration and status }

function ServiceExists(const AServiceName: string): boolean;

type
  PEnumServiceStatusProcessA = ^TEnumServiceStatusProcessA;
  PEnumServiceStatusProcessW = ^TEnumServiceStatusProcessW;
  PEnumServiceStatusProcess = PEnumServiceStatusProcessW;
  TEnumServiceStatusProcessA = ENUM_SERVICE_STATUS_PROCESSA;
  TEnumServiceStatusProcessW = ENUM_SERVICE_STATUS_PROCESSW;
  TEnumServiceStatusProcess = TEnumServiceStatusProcessW;

  PSERVICE_STATUS_PROCESS = ^SERVICE_STATUS_PROCESS;

function EnumServicesStatus(hSC: SC_HANDLE; ServiceTypes, ServiceState: DWORD;
  out Services: PEnumServiceStatus; out ServicesReturned: cardinal): boolean;
function EnumServicesStatusEx(hSC: SC_HANDLE; ServiceTypes, ServiceState: DWORD; GroupName: PChar;
  out Services: PEnumServiceStatusProcess; out ServicesReturned: cardinal): boolean;
function ShEnumServicesStatusEx(hSC: SC_HANDLE; ServiceTypes, ServiceState: DWORD; GroupName: PChar;
  out Services: PEnumServiceStatusProcess; out ServicesReturned: cardinal): boolean;

function EnumDependentServices(hSvc: SC_HANDLE; dwServiceState: DWORD; out ServiceCount: cardinal): LPENUM_SERVICE_STATUS;

function QueryServiceStatus(hSvc: SC_HANDLE): SERVICE_STATUS; overload;
function QueryServiceStatus(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS; overload;
function QueryServiceStatus(const AServiceName: string): SERVICE_STATUS; overload;

function QueryServiceStatusProcess(hSvc: SC_HANDLE): SERVICE_STATUS_PROCESS; overload;
function QueryServiceStatusProcess(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS_PROCESS; overload;
function QueryServiceStatusProcess(const AServiceName: string): SERVICE_STATUS_PROCESS; overload;


{ Basic configuration }

//Result has to be freed
function QueryServiceConfig(hSvc: SC_HANDLE): LPQUERY_SERVICE_CONFIG; overload;
function QueryServiceConfig(hSC: SC_HANDLE; const AServiceName: string): LPQUERY_SERVICE_CONFIG; overload;

//Returns 0 or GetLastError code
function ChangeServiceConfig(hSvc: SC_HANDLE; const AConfig: QUERY_SERVICE_CONFIG;
  const lpdwTagId: PDword; const lpPassword: PWideChar): integer; overload;
function ChangeServiceConfig(hSC: SC_HANDLE; const AServiceName: string;
  const AConfig: QUERY_SERVICE_CONFIG; const lpdwTagId: PDword;
  const lpPassword: PWideChar): integer; overload;

procedure ChangeServiceStartType(hSvc: SC_HANDLE; dwStartType: DWORD); overload;
procedure ChangeServiceStartType(hSC: SC_HANDLE; const AServiceName: string; dwStartType: DWORD); overload;
procedure ChangeServiceStartType(const AServiceName: string; dwStartType: DWORD); overload;


{ Additional service configuration  }

//Result has to be freed
function QueryServiceConfig2(hSvc: SC_HANDLE; dwInfoLevel: cardinal; out lpData: PByte): integer; overload;
function QueryServiceConfig2(hSvc: SC_HANDLE; dwInfoLevel: cardinal): PByte; overload; inline;
function QueryServiceConfig2(hSC: SC_HANDLE; const AServiceName: string; dwInfoLevel: cardinal): PByte; overload;

//Returns 0 or GetLastError code
function ChangeServiceConfig2(hSvc: SC_HANDLE; dwInfoLevel: cardinal; lpInfo: pointer): integer; overload;
function ChangeServiceConfig2(hSC: SC_HANDLE; const AServiceName: string; dwInfoLevel: cardinal; lpInfo: pointer): integer; overload;
function ChangeServiceConfig2(const AServiceName: string; dwInfoLevel: cardinal; lpInfo: pointer): integer; overload;

//Description
function QueryServiceDescription(hSvc: SC_HANDLE): string; overload;
function QueryServiceDescription(hSC: SC_HANDLE; const AServiceName: string): string; overload;


{ Triggers }

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

procedure ChangeServiceTriggers(hSvc: SC_HANDLE; lpTriggers: PSERVICE_TRIGGER_INFO); overload;
procedure ChangeServiceTriggers(hSC: SC_HANDLE; const AServiceName: string; lpTriggers: PSERVICE_TRIGGER_INFO); overload;

//Lets the clients keep standalone copies of trigger data.

function GetTriggerInfoMemorySize(const Tr: PSERVICE_TRIGGER_INFO): NativeUInt;
function CopyServiceTriggers(const Tr: PSERVICE_TRIGGER_INFO): PSERVICE_TRIGGER_INFO;

function GetTriggerMemorySize(const Tr: PSERVICE_TRIGGER): NativeUInt;
function CopyTrigger(const Tr: SERVICE_TRIGGER): PSERVICE_TRIGGER;
function CopyTriggerDataItem(const Tr: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM): SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
procedure FreeStandaloneTriggerDataItem(const Tr: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM);

//There's no way to uniquely identify a trigger entry except by its full contents.
function IsSameServiceTrigger(const Tr1, Tr2: SERVICE_TRIGGER): boolean;

//The following functions read, modify and store the trigger list.
procedure AddServiceTriggers(hSvc: SC_HANDLE; const Triggers: array of SERVICE_TRIGGER;
  UniqueOnly: boolean = false);
procedure DeleteServiceTriggers(hSvc: SC_HANDLE; const Triggers: array of PSERVICE_TRIGGER); overload;
procedure ChangeServiceTrigger(hSvc: SC_HANDLE; const OldTrigger: SERVICE_TRIGGER; const NewTrigger: SERVICE_TRIGGER); overload;


{ Failure actions }

function CreateEmptyFailureActions(): LPSERVICE_FAILURE_ACTIONS;
function CopyFailureActions(const ASource: LPSERVICE_FAILURE_ACTIONS): LPSERVICE_FAILURE_ACTIONS;


{ Service registry key }

const
  sHkeyLocalMachine = 'HKEY_LOCAL_MACHINE';
  sScmHKEY = sHkeyLocalMachine;
  sScmBasePath = '\System\CurrentControlSet\services'; //Base Services key in HKEY_LOCAL_MACHINE
  sTriggersSubkey = 'TriggerInfo';

//Returns the base key for this service's configuration, relative to HKEY_LOCAL_MACHINE
function GetServiceKey(const AServiceName: string): string; inline;
//Opens a configuration key for this service in the registry. The result has to be freed.
function OpenServiceKey(const AServiceName: string): TRegistry;
//Returns the base key for the service's trigger configuration, relative to HKEY_LOCAL_MACHINE
function GetTriggersKey(const AServiceName: string): string; inline;
function GetTriggerKey(const AServiceName: string; AIndex: integer): string; inline;


type
  TServiceDllInformation = record
    ServiceDll: string;
    ServiceDllUnloadOnStop: boolean;
    ServiceMain: string;
  end;

//Services which have svchost as their executable support additional parameter specifying
//the DLL to load.
function QueryServiceServiceDll(const AServiceName: string): string; overload; inline;
function QueryServiceServiceDllEx(const AServiceName: string): TServiceDllInformation; overload;
procedure ChangeServiceServiceDllEx(const AServiceName: string; const ADllInfo: TServiceDllInformation);


{ Service control }

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


{ Launch protection }

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

function QueryServiceLaunchProtected(hSvc: SC_HANDLE): PSERVICE_LAUNCH_PROTECTED; overload;
function QueryServiceLaunchProtected(hSC: SC_HANDLE; const AServiceName: string): PSERVICE_LAUNCH_PROTECTED; overload;
procedure ChangeServiceLaunchProtected(hSvc: SC_HANDLE; dwLaunchProtection: DWORD); overload;
procedure ChangeServiceLaunchProtected(hSC: SC_HANDLE; const AServiceName: string; dwLaunchProtection: DWORD); overload;
procedure ChangeServiceLaunchProtected(const AServiceName: string; dwLaunchProtection: DWORD); overload;
procedure OverwriteServiceLaunchProtection(const AServiceName: string; dwLaunchProtection: DWORD);


implementation
uses SysUtils;

//Raises an OS error in case of error
procedure ScmCheck(const AErrorCode: integer);
begin
  if AErrorCode <> 0 then
    RaiseLastOsError(AErrorCode);
end;

procedure ScmCheck(const AErrorCode: integer; const AAdditionalInfo: string);
begin
  if AErrorCode <> 0 then
    RaiseLastOsError(AErrorCode, AAdditionalInfo);
end;


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

//Either returns BOTH handles to SC manager and Service opened with given access rights,
//or opens NEITHER (closes any if any faults have occured)
procedure OpenScmAndService(out hSC: SC_HANDLE; out hSvc: SC_HANDLE;
  const AServiceName: string; dwScmAccess: cardinal = SC_MANAGER_ALL_ACCESS;
  dwServiceAccess: cardinal = SC_MANAGER_ALL_ACCESS);
begin
  hSC := OpenScManager(dwScmAccess);
  try
    hSvc := OpenService(hSC, AServiceName, dwServiceAccess);
  except
    CloseServiceHandle(hSC);
    raise;
  end;
end;


constructor TAutoScm.Create(AScmHandle: SC_HANDLE);
begin
  inherited Create;
  Self.FScmHandle := AScmHandle;
end;

destructor TAutoScm.Destroy;
begin
  CloseServiceHandle(Self.FScmHandle);
  inherited;
end;

function TAutoScm.ScmHandle: SC_HANDLE;
begin
  Result := Self.FScmHandle;
end;

constructor TAutoService.Create(AScmHandle, ASvcHandle: SC_HANDLE; AOwnsScmHandle: boolean);
begin
  inherited Create;
  FScmHandle := AScmHandle;
  FSvcHandle := ASvcHandle;
  FOwnsScmHandle := AOwnsScmHandle;
end;

destructor TAutoService.Destroy;
begin
  CloseServiceHandle(FSvcHandle);
  if FOwnsScmHandle then
    CloseServiceHandle(FScmHandle);
  inherited;
end;

function TAutoService.ScmHandle: SC_HANDLE;
begin
  Result := FScmHandle;
end;

function TAutoService.SvcHandle: SC_HANDLE;
begin
  Result := FSvcHandle;
end;

function OpenScm(dwAccess: cardinal = SC_MANAGER_ALL_ACCESS): IAutoScm;
begin
  Result := TAutoScm.Create(OpenScManager(dwAccess));
end;

function OpenService2(const AServiceName: string; dwScmAccess: cardinal;
  dwServiceAccess: cardinal): IAutoService;
var hSC, hSvc: SC_HANDLE;
begin
  OpenScmAndService(hSC, hSvc, AServiceName, dwScmAccess, dwServiceAccess);
  Result := TAutoService.Create(hSC, hSvc, true);
end;

//A version that uses external SCM handle. The handle must stay valid all time
//while you use IAutoService.
function OpenService2(hSC: SC_HANDLE; const AServiceName: string;
  dwServiceAccess: cardinal = SERVICE_ALL_ACCESS): IAutoService; overload;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, dwServiceAccess);
  Result := TAutoService.Create(hSC, hSvc, false);
end;


function ServiceExists(const AServiceName: string): boolean;
var hSvc: SC_HANDLE;
  err: integer;
begin
  Result := false;
  with OpenScm(SC_MANAGER_CONNECT and SC_MANAGER_ENUMERATE_SERVICE) do begin
    hSvc := WinSvc.OpenService(ScmHandle, PChar(AServiceName), SERVICE_QUERY_CONFIG);
    if hSvc <> 0 then begin
      WinSvc.CloseServiceHandle(hSvc);
      Result := true;
      exit;
    end;
    err := GetLastError();
    if (err = ERROR_ACCESS_DENIED) or (err = ERROR_SERVICE_DOES_NOT_EXIST) then
      Result := false
    else
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

//Same but returns the new extended structures
function EnumServicesStatusEx(hSC: SC_HANDLE; ServiceTypes, ServiceState: DWORD; GroupName: PChar;
  out Services: PEnumServiceStatusProcess; out ServicesReturned: cardinal): boolean;
var BytesNeeded, ResumeHandle: DWORD;
begin
  Result := false;
  Services := nil;
  ServicesReturned := 0;
  ResumeHandle := 0;
  if WinSvc.EnumServicesStatusEx(hSC, SC_ENUM_PROCESS_INFO, ServiceTypes, SERVICE_STATE_ALL,
     PByte(Services), 0, @BytesNeeded, @ServicesReturned, @ResumeHandle, GroupName) then exit; //no services
  if GetLastError <> ERROR_MORE_DATA then
    exit;

  GetMem(Services, BytesNeeded);
  ServicesReturned := 0;
  ResumeHandle := 0;
  if not WinSvc.EnumServicesStatusEx(hSC, SC_ENUM_PROCESS_INFO, ServiceTypes, SERVICE_STATE_ALL,
    PByte(Services), BytesNeeded, @BytesNeeded, @ServicesReturned, @ResumeHandle, GroupName) then
  begin
    FreeMem(Services); //dont screw LastError plz
    exit;
  end;
  Result := true;
end;

//Helper wrapper which transparently handles some corner cases
//Functionally identical to normal version if your goal is just to query the service list
function ShEnumServicesStatusEx(hSC: SC_HANDLE; ServiceTypes, ServiceState: DWORD; GroupName: PChar;
  out Services: PEnumServiceStatusProcess; out ServicesReturned: cardinal): boolean;
const W10_SERVICE_TYPES = SERVICE_USER_SERVICE or SERVICE_USERSERVICE_INSTANCE or SERVICE_PKG_SERVICE;
var err: integer;
begin
  Result := EnumServicesStatusEx(hSC, ServiceTypes, ServiceState, GroupName, Services, ServicesReturned);
  if not Result then begin
    err := GetLastError;

    if (err = ERROR_INVALID_PARAMETER)
    and (ServiceTypes and W10_SERVICE_TYPES <> 0) then begin
     //Windows <= W7 will return error when asked for W10 service types, so try again without these
      ServiceTypes := ServiceTypes and not W10_SERVICE_TYPES;
      Result := EnumServicesStatusEx(hSC, ServiceTypes, ServiceState, GroupName, Services, ServicesReturned);
    end;
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


function QueryServiceStatusProcess(hSvc: SC_HANDLE): SERVICE_STATUS_PROCESS;
var bytesNeeded: cardinal;
begin
  if not WinSvc.QueryServiceStatusEx(hSvc, SC_STATUS_PROCESS_INFO, @Result, SizeOf(Result), bytesNeeded) then
    RaiseLastOsError();
end;

function QueryServiceStatusProcess(hSC: SC_HANDLE; const AServiceName: string): SERVICE_STATUS_PROCESS;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_QUERY_STATUS);
  if hSvc = 0 then FillChar(Result, sizeof(Result), 0) else
  try
    Result := QueryServiceStatusProcess(hSvc);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

function QueryServiceStatusProcess(const AServiceName: string): SERVICE_STATUS_PROCESS;
var hSC: SC_HANDLE;
begin
  hSC := OpenSCManager(SC_MANAGER_CONNECT);
  try
    Result := QueryServiceStatusProcess(hSC, AServiceName);
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

{
Calls ChangeServiceConfig with the parameter set taken from AConfig.
lpPassword is missing from QUERY_SERVICE_CONFIG
lpdwTagId is an out parameter (see docs), the value in AConfig is ignored
}
function ChangeServiceConfig(hSvc: SC_HANDLE; const AConfig: QUERY_SERVICE_CONFIG;
  const lpdwTagId: PDword; const lpPassword: PWideChar): integer;
begin
  if not WinSvc.ChangeServiceConfig(hSvc,
    AConfig.dwServiceType, AConfig.dwStartType, AConfig.dwErrorControl,
    AConfig.lpBinaryPathName, AConfig.lpLoadOrderGroup, lpdwTagId,
    AConfig.lpDependencies, AConfig.lpServiceStartName, lpPassword,
    AConfig.lpDisplayName)
  then
    Result := GetLastError()
  else
    Result := 0;
end;

function ChangeServiceConfig(hSC: SC_HANDLE; const AServiceName: string;
  const AConfig: QUERY_SERVICE_CONFIG; const lpdwTagId: PDword;
  const lpPassword: PWideChar): integer;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_CHANGE_CONFIG);
  if hSvc = 0 then Result := GetLastError() else
  try
    Result := ChangeServiceConfig(hSvc, AConfig, lpdwTagId, lpPassword);
  finally
    CloseServiceHandle(hSvc);
  end;
end;


procedure ChangeServiceStartType(hSvc: SC_HANDLE; dwStartType: DWORD); overload;
begin
  if not WinSvc.ChangeServiceConfig(hSvc, SERVICE_NO_CHANGE, dwStartType,
    SERVICE_NO_CHANGE, nil, nil, nil, nil, nil, nil, nil) then
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


//This version returns the error code and leaves error raising to clients
function QueryServiceConfig2(hSvc: SC_HANDLE; dwInfoLevel: cardinal; out lpData: PByte): integer;
var lastSize, bufSize: cardinal;
begin
  bufSize := 0;
  lastSize := 0;
  lpData := nil;
  while not WinSvc.QueryServiceConfig2(hSvc, dwInfoLevel, lpData, lastSize, @bufSize) do begin
    Result := GetLastError();
    case Result of
      ERROR_INSUFFICIENT_BUFFER: begin
        if bufSize <= lastSize then //avoid infinite cycle
          exit; //with this error
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
        if lpData <> nil then begin
          FreeMem(lpData);
          lpData := nil;
        end;
        Result := 0;
        exit;
      end;
    else
      if lpData <> nil then begin
        FreeMem(lpData);
        lpData := nil;
      end;
      exit;
    end;
    ReallocMem(lpData, bufSize);
    lastSize := bufSize;
  end;
  Result := 0;
end;

//This version handles error raising
function QueryServiceConfig2(hSvc: SC_HANDLE; dwInfoLevel: cardinal): PByte;
begin
  Result := nil;
  ScmCheck(QueryServiceConfig2(hSvc, dwInfoLevel, Result));
end;

function QueryServiceConfig2(hSC: SC_HANDLE; const AServiceName: string; dwInfoLevel: cardinal): PByte;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_QUERY_CONFIG);
  if hSvc = 0 then Result := nil else
  try
    Result := QueryServiceConfig2(hSvc, dwInfoLevel);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

function ChangeServiceConfig2(hSvc: SC_HANDLE; dwInfoLevel: cardinal; lpInfo: pointer): integer;
begin
  if WinSvc.ChangeServiceConfig2(hSvc, dwInfoLevel, lpInfo) then
    Result := 0
  else
    Result := GetLastError();
end;

function ChangeServiceConfig2(hSC: SC_HANDLE; const AServiceName: string; dwInfoLevel: cardinal; lpInfo: pointer): integer;
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, AServiceName, SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG or SERVICE_START);
  if hSvc = 0 then Result := GetLastError() else
  try
    Result := ChangeServiceConfig2(hSvc, dwInfoLevel, lpInfo);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

function ChangeServiceConfig2(const AServiceName: string; dwInfoLevel: cardinal; lpInfo: pointer): integer;
var hSC: SC_HANDLE;
begin
  hSC := OpenSCManager(SC_MANAGER_ALL_ACCESS);
  try
    Result := ChangeServiceConfig2(hSC, AServiceName, dwInfoLevel, lpInfo);
  finally
    CloseServiceHandle(hSC);
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

procedure ChangeServiceTriggers(hSvc: SC_HANDLE; lpTriggers: PSERVICE_TRIGGER_INFO);
begin
  if ChangeServiceConfig2(hSvc, SERVICE_CONFIG_TRIGGER_INFO, lpTriggers) <> 0 then
    RaiseLastOsError();
end;

procedure ChangeServiceTriggers(hSC: SC_HANDLE; const AServiceName: string; lpTriggers: PSERVICE_TRIGGER_INFO);
begin
  if ChangeServiceConfig2(hSC, AServiceName, SERVICE_CONFIG_TRIGGER_INFO, lpTriggers) <> 0 then
    RaiseLastOsError();
end;


//Calculates the size in memory all parts of a given trigger would occupy if
//stored consecutively
function GetTriggerMemorySize(const Tr: PSERVICE_TRIGGER): NativeUInt;
var i: integer;
begin
  Result := SizeOf(Tr^);
  if Tr.pTriggerSubtype <> nil then
    Result := Result + SizeOf(TGUID);
  if Tr.pDataItems <> nil then
    for i := 0 to Tr.cDataItems-1 do
      Result := Result + SizeOf(SERVICE_TRIGGER_SPECIFIC_DATA_ITEM) + NativeUInt(tr.pDataItems[i].cbData);
end;

{
Copies all consecutive fields of SERVICE_TRIGGER into another place in memory.
The destination must have enough memory. This function is dangerous due to potential
memory overruns so it should not be published: use CopyTrigger.
Returns the size of destination memory used (might be different from the source
memory size in some edge cases).
}
function MoveTrigger(const AFrom: PSERVICE_TRIGGER; const ATo: PSERVICE_TRIGGER): NativeUint;
var i: integer;
  freePtr: PByte;
  pFromItem, pToItem: PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
begin
  freePtr := PByte(ATo);

  Inc(freePtr, SizeOf(AFrom^));
  ATo.dwTriggerType := AFrom.dwTriggerType;
  ATo.dwAction := AFrom.dwAction;
  ATo.cDataItems := AFrom.cDataItems;

  if AFrom.pTriggerSubtype <> nil then begin
    ATo.pTriggerSubtype := PGuid(freePtr);
    Inc(freePtr, SizeOf(TGUID));
    ATo.pTriggerSubtype^ := AFrom.pTriggerSubtype^;
  end else
    ATo.pTriggerSubtype := nil;

  if (AFrom.pDataItems <> nil)
  and (AFrom.cDataItems > 0)    //so that we don't uint-underflow below
  then begin
    //Allocate memory for the DATA_ITEM headers (need to go sequentially)
    ATo.pDataItems := PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM(freePtr);
    Inc(freePtr, AFrom.cDataItems * SizeOf(SERVICE_TRIGGER_SPECIFIC_DATA_ITEM));

    //Allocate and copy memory for each item
    pFromItem := AFrom.pDataItems;
    pToItem := ATo.pDataItems;
    for i := 0 to AFrom.cDataItems-1 do begin
      pToItem.dwDataType := pFromItem.dwDataType;
      pToItem.cbData := pFromItem.cbData;
      pToItem.pData := freePtr;
      CopyMemory(pToItem.pData, pFromItem.pData, pToItem.cbData);
      Inc(freePtr, pToItem.cbData);
      Inc(pToItem);
      Inc(pFromItem);
    end;
  end else begin
    ATo.pDataItems := nil;
    ATo.cDataItems := 0; //even if AFrom said something else
  end;

  Result := NativeUint(freePtr)-NativeUint(ATo);
end;

//Makes a copy of a given trigger. The copy has to be freed by the caller.
function CopyTrigger(const Tr: SERVICE_TRIGGER): PSERVICE_TRIGGER;
var totalMem: NativeUInt;
begin
  totalMem := GetTriggerMemorySize(@Tr);
  GetMem(Result, totalMem);
  MoveTrigger(@Tr, Result);
end;

//Makes a copy of a given SERVICE_TRIGGER_SPECIFIC_DATA_ITEM. The copy has to be freed by the caller.
//The entries in a SERVICE_TRIGGER are usually stored in the same memory allocation,
//so need not to be neither copied nor released individually (if you CopyTrigger the whole trigger).
function CopyTriggerDataItem(const Tr: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM): SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
begin
  Result.dwDataType := Tr.dwDataType;
  Result.cbData := Tr.cbData;
  if Tr.pData = nil then
    Result.pData := nil
  else begin
    GetMem(Result.pData, Result.cbData);
    CopyMemory(Result.pData, Tr.pData, Result.cbData);
  end;
end;

//Frees a SERVICE_TRIGGER_SPECIFIC_DATA_ITEM allocated through CopyTriggerDataItem
procedure FreeStandaloneTriggerDataItem(const Tr: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM);
begin
  if Tr.pData <> nil then
    FreeMem(Tr.pData);
end;


//Calculates the size in memory all parts of a given trigger info would occupy
//if stored consecutively
function GetTriggerInfoMemorySize(const Tr: PSERVICE_TRIGGER_INFO): NativeUInt;
var i: integer;
begin
  Result := SizeOf(Tr^);
  if Tr.pTriggers <> nil then
    for i := 0 to Tr^.cTriggers-1 do
      Result := Result + GetTriggerMemorySize(@Tr.pTriggers[i]);
end;

{
Copies all the consecutive internal content of a SERVICE_TRIGGER_INFO to another
place in memory.
The destination must have enough memory. This function is dangerous due to potential
memory overruns so it should not be published: use CopyTrigger.
Returns the size of the destination memory used.
}
function MoveServiceTriggers(const AFrom: PSERVICE_TRIGGER_INFO; ATo: PSERVICE_TRIGGER_INFO): NativeUint;
var freePtr, fromPtr: PByte;
  i: integer;
  used: NativeUint;
begin
  freePtr := PByte(ATo);

  Inc(freePtr, SizeOf(ATo^));
  ATo.cTriggers := AFrom.cTriggers;
  ATo.pReserved := AFrom.pReserved;

  if (AFrom.pTriggers <> nil)
  and (AFrom.cTriggers > 0)   //so that we don't uint-underflow below
  then begin
    ATo.pTriggers := PSERVICE_TRIGGER(freePtr);
    fromPtr := PByte(AFrom.pTriggers);
    for i := 0 to AFrom.cTriggers-1 do begin
      used := MoveTrigger(PSERVICE_TRIGGER(fromPtr), PSERVICE_TRIGGER(freePtr));
      Inc(freePtr, used); //might be different from fromPtr sizes in some edge cases
      Inc(fromPtr, GetTriggerMemorySize(PSERVICE_TRIGGER(fromPtr)));
    end;
  end else begin
    ATo.pTriggers := nil;
    ATo.cTriggers := 0; //even if AFrom said something else
  end;

  Result := NativeUint(freePtr)-NativeUint(ATo);
end;

function CopyServiceTriggers(const Tr: PSERVICE_TRIGGER_INFO): PSERVICE_TRIGGER_INFO;
var totalMem: NativeUInt;
begin
  totalMem := GetTriggerInfoMemorySize(Tr);
  GetMem(Result, totalMem);
  MoveServiceTriggers(Tr, Result);
end;


function IsSameServiceTriggerDataItem(Di1, Di2: PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM): boolean; inline;
begin
  Result := (Di1.dwDataType = Di2.dwDataType)
        and (Di1.cbData = Di2.cbData)
        and CompareMem(Di1.pData, Di2.pData, Di1.cbData);
end;

//Returns true if two services triggers are the same.
//Service manager implements no IDs for triggers, so the only way to locate
//a particular one is to compare its data.
function IsSameServiceTrigger(const Tr1, Tr2: SERVICE_TRIGGER): boolean;
var i: integer;
begin
  Result := (Tr1.dwTriggerType = Tr2.dwTriggerType)
        and (Tr1.dwAction = Tr2.dwAction)
        and IsEqualGuid(Tr1.pTriggerSubtype^, Tr2.pTriggerSubtype^)
        and (Tr1.cDataItems  = Tr2.cDataItems);
  if not Result then exit;

  for i := 0 to Tr1.cDataItems-1 do begin
    Result := IsSameServiceTriggerDataItem(@Tr1.pDataItems[i], @Tr2.pDataItems[i]);
    if not Result then exit;
  end;
end;


//Inserts one or more triggers into a set of all triggers for the service
//UniqueOnly: add only triggers which have no exact equivalents already.
procedure AddServiceTriggers(hSvc: SC_HANDLE; const Triggers: array of SERVICE_TRIGGER;
  UniqueOnly: boolean = false);
var trigHead: PSERVICE_TRIGGER_INFO;
  trigData: array of SERVICE_TRIGGER;
  i, j: integer;
  found: boolean;
begin
  if Length(Triggers) <= 0 then exit;

  trigHead := QueryServiceTriggers(hSvc);
  if trigHead = nil then begin
    //We could not get the trigger info struct, but maybe we could still put a new one?
    //Allocate and fill the structure as if there simply were no triggers originally.
    GetMem(trigHead, sizeof(trigHead));
    trigHead.cTriggers := 0;
    trigHead.pTriggers := nil;
    trigHead.pReserved := nil;
  end;
  try
    //Edit inplace where possible since this is our own copy in our memory
    trigHead.cTriggers := trigHead.cTriggers + cardinal(Length(Triggers));

    //Copy old triggers
    SetLength(trigData, trigHead.cTriggers);
    for i := 0 to Length(trigData) - Length(Triggers) - 1 do
      trigData[i] := trigHead.pTriggers[i]; //old triggers

    //Copy new triggers
    for i := 0 to Length(Triggers)-1 do begin
      if UniqueOnly then begin
        found := false;
        for j := 0 to Length(trigData) - Length(Triggers) - 1 do
          if IsSameServiceTrigger(trigData[j], Triggers[i]) then begin
            found := true;
            break;
          end;
        if found then
          continue;
      end;

      trigData[Length(trigData) - Length(Triggers) + i] := Triggers[i];
    end;

    //Rewrite old data pointer
    //We don't free the old pointer because it points inside of trigHead memory block
    //which we'll free at the end of this function
    if trigHead.cTriggers > 0 then
      trigHead.pTriggers := @trigData[0]
    else
      trigHead.pTriggers := nil;

    //Set
    ChangeServiceTriggers(hSvc, trigHead);
  finally
    FreeMem(trigHead);
  end;
end;

//Deletes one or more triggers from a set of all triggers for the service
procedure DeleteServiceTriggers(hSvc: SC_HANDLE; const Triggers: array of PSERVICE_TRIGGER);
var trigHead: PSERVICE_TRIGGER_INFO;
  ptrFrom, ptrTo: PSERVICE_TRIGGER;
  i: integer;
begin
  if Length(Triggers) <= 0 then exit;

  trigHead := QueryServiceTriggers(hSvc);
  if trigHead = nil then begin
    //There's already no triggers for the service or maybe they're not even supported.
    //Let's not make a drama.
    exit;
  end;

  try
    if trigHead.cTriggers = 0 then
      exit; //again, no triggers => nothing to delete

    //Edit inplace where possible since this is our own copy in our memory

    //We will delete all triggers which match one of the given ones.
    ptrFrom := trigHead.pTriggers;
    ptrTo := trigHead.pTriggers;
    while ptrFrom < trigHead.pTriggers + trigHead.cTriggers do begin
      for i := 0 to Length(Triggers)-1 do
        if IsSameServiceTrigger(ptrFrom^, Triggers[i]^) then begin
          //Skip this one
          Inc(ptrFrom);
          Dec(trigHead.cTriggers);
          continue;
        end;
      //Match not found, move the item to the next free slot
      if ptrFrom <> ptrTo then
        ptrTo^ := ptrFrom^;
      Inc(ptrFrom);
      Inc(ptrTo);
    end;

    if trigHead.cTriggers = 0 then
      trigHead.pTriggers := nil; //or SCM complains

    //Set
    ChangeServiceTriggers(hSvc, trigHead);
  finally
    FreeMem(trigHead);
  end;
end;

//Replace the contents of the given trigger entry with a given new (changed) trigger entry
procedure ChangeServiceTrigger(hSvc: SC_HANDLE; const OldTrigger: SERVICE_TRIGGER; const NewTrigger: SERVICE_TRIGGER);
var trigHead: PSERVICE_TRIGGER_INFO;
  i: integer;
begin
  trigHead := QueryServiceTriggers(hSvc);
  if trigHead = nil then
    raise Exception.Create('Source trigger not found');

  try
    if trigHead.cTriggers = 0 then
      raise Exception.Create('Source trigger not found');

    //Edit inplace since this is our own copy in our memory

    for i := 0 to trigHead.cTriggers-1 do
      if IsSameServiceTrigger(trigHead.pTriggers[i], OldTrigger) then
        trigHead.pTriggers[i] := NewTrigger;

    //Set
    ChangeServiceTriggers(hSvc, trigHead);
  finally
    FreeMem(trigHead);
  end;
end;


{ Failure actions }
{
A SERVICE_FAILURE_ACTIONS structure is a memory block with additional SC_ACTION
structures after the end, then any strings the block references.
SCM always uses 3 SC_ACTION slots and doesn't support more than 3 actions,
so we try to always have at least 3.

A LPSERVICE_FAILURE_ACTIONS allocated in our app has to be freed with a call
to FreeMem;
}

//Allocates new SERVICE_FAILURE_ACTIONS structure with 3 standard action slots
//and no actions.
//The result has to be freed.
function CreateEmptyFailureActions(): LPSERVICE_FAILURE_ACTIONS;
begin
  GetMem(Result, SizeOf(Result) + 3*SizeOf(SC_ACTION));
  Result.dwResetPeriod := 0;
  Result.lpRebootMsg := nil;
  Result.lpCommand := nil;
  Result.cActions := 0;
  Result.lpsaActions := LPSC_ACTION(Result+1);
end;

function CopyFailureActions(const ASource: LPSERVICE_FAILURE_ACTIONS): LPSERVICE_FAILURE_ACTIONS;
var LSlotCount: integer;
  LTotalMem: NativeUInt;
  i: integer;
  ptr: PByte;
begin
  //Allocate 3 slots, but more if somehow needed
  LSlotCount := ASource.cActions;
  if LSlotCount < 3 then
    LSlotCount := 3;
  LTotalMem := SizeOf(Result^) + LSlotCount*SizeOf(SC_ACTION)
    + (StrLen(ASource.lpRebootMsg)+1)*SizeOf(Char)
    + (StrLen(ASource.lpCommand)+1)*SizeOf(Char);
  Result.dwResetPeriod := 0;
  Result.cActions := ASource.cActions;
  Result.lpsaActions := LPSC_ACTION(Result+1);
  for i := 1 to Result.cActions do begin
    Result.lpsaActions[i-1] := ASource.lpsaActions[i-1];
  end;
  ptr := PByte(Result+1)+SizeOf(SC_ACTION)*Result.cActions;
  if ASource.lpRebootMsg = nil then
    Result.lpRebootMsg := nil
  else begin
    i := StrLen(ASource.lpRebootMsg);
    Result.lpRebootMsg := PChar(ptr);
    StrLCopy(Result.lpRebootMsg, ASource.lpRebootMsg, i); //adds +1 term null
    Inc(ptr, (i+1)*SizeOf(char));
  end;
  if ASource.lpCommand = nil then
    Result.lpCommand := nil
  else begin
    i := StrLen(ASource.lpCommand);
    Result.lpCommand := PChar(ptr);
    StrLCopy(Result.lpCommand, ASource.lpCommand, i); //adds +1 term null
    Inc(ptr, (i+1)*SizeOf(char));
  end;
end;


{ Service registry key }

function GetServiceKey(const AServiceName: string): string;
begin
  Result := sScmBasePath+'\'+AServiceName;
end;

function OpenServiceKey(const AServiceName: string): TRegistry;
begin
  Result := TRegistry.Create;
  try
    Result.RootKey := HKEY_LOCAL_MACHINE;
    Result.Access := KEY_READ;
    if not Result.OpenKey(GetServiceKey(AServiceName), false) then
      RaiseLastOsError();
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function GetTriggersKey(const AServiceName: string): string;
begin
  Result := GetServiceKey(AServiceName) + '\' + sTriggersSubkey;
end;

function GetTriggerKey(const AServiceName: string; AIndex: integer): string;
begin
  Result := GetTriggersKey(AServiceName) + '\' + IntToStr(AIndex);
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

function RegSetValueSz(hKey: HKEY; ValueName: string; Value: string): cardinal; inline;
begin
  Result := RegSetValueEx(hkey, PChar(ValueName), 0, REG_SZ, PChar(Value), SizeOf(char)*(Length(Value)+1));
end;

function RegSetValueExpandSz(hKey: HKEY; ValueName: string; Value: string): cardinal; inline;
begin
  Result := RegSetValueEx(hkey, PChar(ValueName), 0, REG_EXPAND_SZ, PChar(Value), SizeOf(char)*(Length(Value)+1));
end;

function RegQueryValueDword(hKey: HKEY; ValueName: string; out Value: dword): cardinal;
var sz: cardinal;
  valType: cardinal;
begin
  sz := SizeOf(Value);
  Result := RegQueryValueEx(hKey, PChar(ValueName), nil, @valType, @Value, @sz);
  if Result = 0 then begin
    if valType <> REG_DWORD then
      Result := ERROR_INVALID_DATATYPE;
  end;
end;

function RegSetValueDword(hKey: HKEY; ValueName: string; Value: dword): cardinal; inline;
begin
  Result := RegSetValueEx(hkey, PChar(ValueName), 0, REG_DWORD, PChar(@Value), SizeOf(dword));
end;


const
  sRegParameters = 'Parameters';
  sRegServiceDll = 'ServiceDll';
  sRegServiceDllUnloadOnStop = 'ServiceDllUnloadOnStop';
  sRegServiceMain = 'ServiceMain';

function QueryServiceServiceDll(const AServiceName: string): string;
begin
  Result := QueryServiceServiceDllEx(AServiceName).ServiceDll;
end;

function QueryServiceServiceDllEx(const AServiceName: string): TServiceDllInformation; overload;
var hk: HKEY;
begin
  Result.ServiceDll := '';
  Result.ServiceDllUnloadOnStop := false;
  Result.ServiceMain := '';
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(GetServiceKey(AServiceName)+'\'+sRegParameters),
    0, KEY_QUERY_VALUE, hk) <> 0
  then
    exit;
  try
    if RegQueryValueSz(hk, sRegServiceDll, Result.ServiceDll) <> 0 then
      Result.ServiceDll := '';
    if RegQueryValueDword(hk, sRegServiceDllUnloadOnStop, PDword(@Result.ServiceDllUnloadOnStop)^) <> 0 then
      Result.ServiceDllUnloadOnStop := false;
    if RegQueryValueSz(hk, sRegServiceMain, Result.ServiceMain) <> 0 then
      Result.ServiceMain := '';

    //Fallback: these parameters can also be in the root key of the service
    if Result.ServiceDll = '' then begin
      RegCloseKey(hk);
      hk := HKEY_LOCAL_MACHINE; //so that accidental Close() does nothing
      if RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(GetServiceKey(AServiceName)),
        0, KEY_QUERY_VALUE, hk) <> 0
      then
        exit;
      if RegQueryValueSz(hk, sRegServiceDll, Result.ServiceDll) <> 0 then
        Result.ServiceDll := '';
      if RegQueryValueDword(hk, sRegServiceDllUnloadOnStop, PDword(@Result.ServiceDllUnloadOnStop)^) <> 0 then
        Result.ServiceDllUnloadOnStop := false;
      if RegQueryValueSz(hk, sRegServiceMain, Result.ServiceMain) <> 0 then
        Result.ServiceMain := '';
    end;
  finally
    RegCloseKey(hk);
  end;
end;

procedure ChangeServiceServiceDllEx(const AServiceName: string; const ADllInfo: TServiceDllInformation);
var hk: HKEY;
  err: integer;
begin
  CheckOSError(
    RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(GetServiceKey(AServiceName)+'\'+sRegParameters),
    0, KEY_QUERY_VALUE, hk));
  try
    if ADllInfo.ServiceDll <> '' then
      CheckOsError(RegSetValueExpandSz(hk, sRegServiceDll, ADllInfo.ServiceDll))
    else
      CheckOsError(RegDeleteValue(hk, PChar(sRegServiceDll)));
    CheckOsError(RegSetValueDword(hk, sRegServiceDllUnloadOnStop, dword(ADllInfo.ServiceDllUnloadOnStop)));
    if ADllInfo.ServiceMain <> '' then
      CheckOsError(RegSetValueSz(hk, sRegServiceMain, ADllInfo.ServiceMain))
    else
      CheckOsError(RegDeleteValue(hk, PChar(sRegServiceMain)));

    {
    Fallback: What do we do if there are compatibility keys in the top level key?
    1. Delete them and only use the main ones.
       Pros: Clear strategy.
       Cons: Someone may rely on them. Deleting them requires additional privileges.
    2. Update them to match the main ones.
       Cons: Never saw these params set in BOTH places. Will this work?
    3. Only update in the place where they are already present.
       But what if different params are present at different places?
       What if some params are present at both?
    I'm going to do it the simplest now (1), but maybe 3 is better.
    }

    RegCloseKey(hk);
    hk := HKEY_LOCAL_MACHINE; //so that accidental Close() does nothing
    CheckOsError(RegOpenKeyEx(HKEY_LOCAL_MACHINE, PChar(GetServiceKey(AServiceName)),
      0, KEY_QUERY_VALUE, hk));

    err := RegDeleteValue(hk, PChar(sRegServiceDll));
    if (err <> 0) and (err <> ERROR_FILE_NOT_FOUND) then
      RaiseLastOsError(err);
    err := RegDeleteValue(hk, PChar(sRegServiceDllUnloadOnStop));
    if (err <> 0) and (err <> ERROR_FILE_NOT_FOUND) then
      RaiseLastOsError(err);
    err := RegDeleteValue(hk, PChar(sRegServiceMain));
    if (err <> 0) and (err <> ERROR_FILE_NOT_FOUND) then
      RaiseLastOsError(err);
  finally
    RegCloseKey(hk);
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


{ Launch protection }

function QueryServiceLaunchProtected(hSvc: SC_HANDLE): PSERVICE_LAUNCH_PROTECTED;
begin
  Result := PSERVICE_LAUNCH_PROTECTED(QueryServiceConfig2(hSvc, SERVICE_CONFIG_LAUNCH_PROTECTED));
end;

function QueryServiceLaunchProtected(hSC: SC_HANDLE; const AServiceName: string): PSERVICE_LAUNCH_PROTECTED;
begin
  Result := PSERVICE_LAUNCH_PROTECTED(QueryServiceConfig2(hSC, AServiceName, SERVICE_CONFIG_LAUNCH_PROTECTED));
end;

procedure ChangeServiceLaunchProtected(hSvc: SC_HANDLE; dwLaunchProtection: DWORD);
var lp: SERVICE_LAUNCH_PROTECTED;
begin
  lp.dwLaunchProtected := dwLaunchProtection;
  if ChangeServiceConfig2(hSvc, SERVICE_CONFIG_LAUNCH_PROTECTED, @lp) <> 0 then
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
  err := RegOpenKey(HKEY_LOCAL_MACHINE, PChar(GetServiceKey(AServiceName)), hk);
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


end.
