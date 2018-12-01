unit SvcEntry;

interface
uses SysUtils, Classes, Windows, WinSvc, ServiceHelper, Generics.Collections, ImgList;

const
  {
  Additional autostart mode which you can only use with TServiceEntry's Get/SetStartMode().
  In SCM delayed autostart is stored as SERVICE_AUTO_START + additional flag.

  Any service can be marked as a delayed auto-start service; however, this
  setting has no effect unless the service is an auto-start service.
  https://docs.microsoft.com/en-us/windows/desktop/api/winsvc/ns-winsvc-_service_delayed_auto_start_info
  }
  SERVICE_DELAYED_AUTOSTART = $00010002;


type
  TServiceQueriedBit = (
    qbDescriptionQueried,
    qbConfigQueried,
    qbServiceDllQueried,
    qbLaunchProtectionQueried,
    qbTriggerCountQueried,
    qbDelayedAutostartQueried
  );
  TServiceQueriedData = set of TServiceQueriedBit;

 //Actual registered service on a live system
 //We load a lot of these and querying the info is rather slow, so we do it on-demand.
  TServiceEntry = class
  protected
   //Common ServiceManager handle to query basic information. We share this because we fetch info
   //on demand (=> dissociated queries) and do not want to reopen the manager every request.
    class var FhSC: SC_HANDLE;
    class function GetHSC: SC_HANDLE; inline;
   //Same but for changes. Might be unavailable if you don't have rights; then
   //Get() will fail each time.
    class var FhSC_RW: SC_HANDLE;
    class function GetHSC_RW: SC_HANDLE; inline;
  protected
    FHandle: SC_HANDLE; //Only to query basic information. Open your own with extended rights for changes
    FQueriedData: TServiceQueriedData;
    FDescription: string;
    FConfig: LPQUERY_SERVICE_CONFIG;
    FServiceDll: string;
    FTriggerCount: integer;
    function GetHandle: SC_HANDLE; inline;
    function GetConfig: LPQUERY_SERVICE_CONFIG; inline;
    function GetDescription: string; inline;
    function GetServiceDll: string; inline;
    function GetTriggerCount: integer;
  public
    ServiceName: string;
    DisplayName: string;
    Status: SERVICE_STATUS_PROCESS;
    constructor Create(const AServiceName: string); overload;
    constructor CreateFromEnum(const S: PEnumServiceStatusProcess);
    destructor Destroy; override;
    procedure Refresh(); overload;
    function GetEffectiveDisplayName: string; virtual;
    function GetImageFilename: string;
    function GetLoadOrderGroup: string; inline;
    procedure GetIcon(out AImageList: TCustomImageList; out AIndex: integer); virtual;
    function DependsOn(const ADependency: string): boolean;
    property Handle: SC_HANDLE read GetHandle;
    property Description: string read GetDescription;
    property Config: LPQUERY_SERVICE_CONFIG read GetConfig; //CAN be nil if not accessible to this user
    property ServiceDll: string read GetServiceDll;
    property LoadOrderGroup: string read GetLoadOrderGroup;

  protected
    FLaunchProtection: cardinal;
    FDelayedAutostart: boolean;
    function GetLaunchProtection: cardinal; inline;
    function GetDelayedAutostart: boolean;
    function GetStartTypeEx: dword;
  public
    function CanStart: boolean; inline;
    function CanForceStart: boolean; inline;
    function CanStop: boolean; inline;
    function CanPause: boolean; inline;
    function CanResume: boolean; inline;
    function IsLaunchProtected: boolean; inline;
    procedure SetDelayedAutostart(const Value: boolean);
    procedure SetStartTypeEx(const AValue: dword);
    property StartTypeEx: dword read GetStartTypeEx write SetStartTypeEx;
    property DelayedAutostart: boolean read GetDelayedAutostart write SetDelayedAutostart;
    property LaunchProtection: cardinal read GetLaunchProtection;

    property TriggerCount: integer read GetTriggerCount;
  end;
  PServiceEntry = ^TServiceEntry;

  TServiceEntries = array of TServiceEntry;
  PServiceEntries = ^TServiceEntries;

  TServiceEntryList = class(TObjectList<TServiceEntry>)
  public
    function FindIndex(const AServiceName: string): integer;
    function Find(const AServiceName: string): TServiceEntry;
  end;


//Subscribe to be notified of changes
var
  OnServiceInvalidated: TList<TNotifyEvent>;


//Shortcuts
procedure RefreshService(Service: TServiceEntry);
procedure InvalidateService(Service: TServiceEntry);


implementation
uses WinapiHelper;

class function TServiceEntry.GetHSC: SC_HANDLE;
begin
  if FhSC = 0 then
    FhSC := OpenSCManager(SC_MANAGER_CONNECT);
  Result := FhSC;
 //There's no code to destroy the handle. It persists while the application runs,
 //and there's no real point in closing handles when the app is going down.
end;

class function TServiceEntry.GetHSC_RW: SC_HANDLE;
begin
  if FhSC = 0 then
    FhSC := OpenSCManager(SC_MANAGER_ALL_ACCESS);
  Result := FhSC_RW;
end;


constructor TServiceEntry.Create(const AServiceName: string);
begin
  inherited Create;
  Self.ServiceName := AServiceName;
  Self.Refresh; //to query status
end;

constructor TServiceEntry.CreateFromEnum(const S: PEnumServiceStatusProcess);
begin
  inherited Create;
  Self.ServiceName := S^.lpServiceName;
  Self.DisplayName := S^.lpDisplayName;
  Self.Status := S^.ServiceStatus;
end;

destructor TServiceEntry.Destroy;
begin
  if Self.FConfig <> nil then begin
    FreeMem(Self.FConfig);
    Self.FConfig := nil;
  end;
  inherited;
end;

procedure TServiceEntry.Refresh();
begin
  Self.Status := QueryServiceStatusProcess(Self.Handle);
  Self.FQueriedData := [];
  FreeMem(Self.FConfig);
  Self.FConfig := nil;
end;

function TServiceEntry.GetHandle: SC_HANDLE;
begin
  if Self.FHandle = 0 then
    Self.FHandle := OpenService(Self.GethSC, Self.ServiceName, SERVICE_QUERY_CONFIG or SERVICE_QUERY_STATUS);
  Result := Self.FHandle;
end;

function TServiceEntry.GetConfig: LPQUERY_SERVICE_CONFIG;
begin
  if not (qbConfigQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbConfigQueried]; //BEFORE we run potentially exception-throwing code
    try
      FConfig := QueryServiceConfig(Self.Handle);
    except
      on E: EOsError do begin
        //OS likes to fail this, especially for drivers
        //We don't want to crash and burn, but we cannot just ignore ALL errors. Sigh.
        if (E.ErrorCode = ERROR_RESOURCE_TYPE_NOT_FOUND)
        or (E.ErrorCode = 1168) then //element not found
          FConfig := nil
        else
          raise;
      end;
    end;
  end;
  Result := FConfig;
end;

function TServiceEntry.GetDescription: string;
begin
  if not (qbDescriptionQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbDescriptionQueried];
    FDescription := QueryServiceDescription(Self.Handle);
  end;
  Result := FDescription;
end;

function TServiceEntry.GetServiceDll: string;
begin
  if not (qbServiceDllQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbServiceDllQueried];
    if Self.Config <> nil then
      if (pos('svchost.exe', Self.Config.lpBinaryPathName)>0)
      or (pos('lsass.exe', Self.Config.lpBinaryPathName)>0) then
     //^ this is not a surefire way to test it's running svchost.exe, but we don't need one
     // It would be too complicated to check that it really references svchost, and the one
     // from the system dir and not an impostor.
     // We just optimize away unneccessary registry checks.
      FServiceDll := ExpandEnvironmentStrings(QueryServiceServiceDll(Self.ServiceName));
  end;
  Result := FServiceDll;
end;

function TServiceEntry.GetLaunchProtection: cardinal;
var tmp: PSERVICE_LAUNCH_PROTECTED;
begin
  if not (qbLaunchProtectionQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbLaunchProtectionQueried];
    try
      tmp := QueryServiceLaunchProtected(Self.Handle);
      if tmp <> nil then begin
        FLaunchProtection := tmp.dwLaunchProtected;
        FreeMem(tmp);
      end;
    except
      on E: EOsError do begin
        if E.ErrorCode = ERROR_INVALID_LEVEL then
         //Yah whatever
          FLaunchProtection := SERVICE_LAUNCH_PROTECTED_NONE
        else
          raise;
      end;
    end;
  end;
  Result := FLaunchProtection;
end;

function TServiceEntry.GetTriggerCount: integer;
var triggers: PSERVICE_TRIGGER_INFO;
begin
  if not (qbTriggerCountQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbTriggerCountQueried];
    triggers := QueryServiceTriggers(Self.Handle);
    if triggers = nil then
      FTriggerCount := 0
    else begin
      FTriggerCount := triggers.cTriggers;
      FreeMem(triggers);
    end;
  end;
  Result := FTriggerCount;
end;

function TServiceEntry.GetDelayedAutostart: boolean;
var info: LPSERVICE_DELAYED_AUTO_START_INFO;
begin
  if not (qbDelayedAutostartQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbDelayedAutostartQueried];
    try
      info := LPSERVICE_DELAYED_AUTO_START_INFO(
        QueryServiceConfig2(Self.Handle, SERVICE_CONFIG_DELAYED_AUTO_START_INFO));
    except
      on E: EOsError do begin
        if (E.ErrorCode = ERROR_INVALID_PARAMETER) then
          info := nil
        else
          raise;
      end;
    end;
    try
      Self.FDelayedAutostart := (info <> nil) and info.fDelayedAutostart;
    finally
      FreeMem(info);
    end;
  end;
  Result := Self.FDelayedAutostart;
end;

//Enables/disables delayed autostart; throws if it's not supported
procedure TServiceEntry.SetDelayedAutostart(const Value: boolean);
var info: SERVICE_DELAYED_AUTO_START_INFO;
  res: integer;
begin
  //Any BOOL!=0 is supposed to be equal to TRUE, Delphi uses DWORD(-1),
  //but Win10 requires this to be exactly 1 or fails.
  dword(info.fDelayedAutostart) := 1;
  res := ChangeServiceConfig2(Self.GetHSC_RW, Self.ServiceName, SERVICE_CONFIG_DELAYED_AUTO_START_INFO, @info);
  if res <> 0 then
    RaiseLastOsError(res);
end;


procedure TServiceEntry.GetIcon(out AImageList: TCustomImageList; out AIndex: integer);
begin
  AImageList := nil;
  AIndex := -1;
end;

function TServiceEntry.GetEffectiveDisplayName: string;
begin
  Result := Self.DisplayName;
end;

function TServiceEntry.GetImageFilename: string;
begin
  if Self.ServiceDll <> '' then
    Result := Self.ServiceDll
  else
  if Config <> nil then
    Result := Config.lpBinaryPathName
  else
    Result := '';
end;

function TServiceEntry.GetLoadOrderGroup: string;
begin
  if (Self.Config = nil) or (Self.Config.lpLoadOrderGroup = nil) then
    Result := ''
  else
    Result := Self.Config.lpLoadOrderGroup;
end;

function TServiceEntry.CanStart: boolean;
begin
  Result := (Status.dwCurrentState = SERVICE_STOPPED)
    and ((Config = nil) or (Config.dwStartType <> SERVICE_DISABLED));
end;

function TServiceEntry.CanForceStart: boolean;
begin
  Result := Status.dwCurrentState = SERVICE_STOPPED;
end;

function TServiceEntry.CanStop: boolean;
begin
  Result := (Status.dwCurrentState <> SERVICE_STOPPED)
    and (Status.dwCurrentState <> SERVICE_STOP_PENDING);
end;

function TServiceEntry.CanPause: boolean;
begin
  Result := Status.dwCurrentState = SERVICE_RUNNING;
end;

function TServiceEntry.CanResume: boolean;
begin
  Result := Status.dwCurrentState = SERVICE_PAUSED;
end;

function TServiceEntry.IsLaunchProtected: boolean;
begin
  Result := Self.LaunchProtection <> SERVICE_LAUNCH_PROTECTED_NONE;
end;

//Returns enhanced StartType which includes delayed autostart
function TServiceEntry.GetStartTypeEx: dword;
begin
  if Config = nil then
    Result := SERVICE_DEMAND_START //can't really tell so assume default
  else begin
    Result := Config.dwStartType;
    if (Result = SERVICE_AUTO_START)
    and Self.GetDelayedAutostart then //auto start supports additional delayed flag
      Result := SERVICE_DELAYED_AUTOSTART;
  end;
end;

//Sets enhanced start type which includes delayed autostart
procedure TServiceEntry.SetStartTypeEx(const AValue: dword);
begin
  if AValue = SERVICE_DELAYED_AUTOSTART then begin
    //Set the delayed flag first so that if it's not supported we don't change anything
    Self.SetDelayedAutostart(true);
    ChangeServiceStartType(Self.GetHSC_RW, Self.ServiceName, SERVICE_AUTO_START);
  end else
    ChangeServiceStartType(Self.GetHSC_RW, Self.ServiceName, AValue);
end;


//True if a given string is included in service dependencies
function TServiceEntry.DependsOn(const ADependency: string): boolean;
var deps: TArray<string>;
  dep: string;
begin
  Result := false;
  deps := SplitNullSeparatedList(Self.Config.lpDependencies);
  for dep in deps do
    if SameText(dep, ADependency) then begin
      Result := true;
      break;
    end;
end;


function TServiceEntryList.FindIndex(const AServiceName: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Self.Count-1 do
    if SameText(Self[i].ServiceName, AServiceName) then begin
      Result := i;
      break;
    end;
end;

function TServiceEntryList.Find(const AServiceName: string): TServiceEntry;
var i: integer;
begin
  i := FindIndex(AServiceName);
  if i >= 0 then
    Result := Self[i]
  else
    Result := nil;
end;


//Reload data for a specified service and repaint the nodes
procedure RefreshService(Service: TServiceEntry);
begin
  Service.Refresh();
  InvalidateService(Service);
end;

procedure InvalidateService(Service: TServiceEntry);
var event: TNotifyEvent;
begin
  for event in OnServiceInvalidated do
    event(Service);
end;


initialization
  OnServiceInvalidated := TList<TNotifyEvent>.Create;

finalization
{$IFDEF DEBUG}
  FreeAndNil(OnServiceInvalidated);
{$ENDIF}

end.
