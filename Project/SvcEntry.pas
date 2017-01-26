unit SvcEntry;

interface
uses SysUtils, Classes, Windows, WinSvc, ServiceHelper, Generics.Collections, ImgList;

type
 //Actual registered service on a live system
 //We load a lot of these and querying the info is rather slow, so we do it on-demand.
  TServiceEntry = class
  protected
   //Common ServiceManager handle to query basic information. We share this because we fetch info
   //on demand (=> dissociated queries) and do not want to reopen the manager every request.
    class var FhSC: SC_HANDLE;
    class function GetHSC: SC_HANDLE; inline;
  protected
    FHandle: SC_HANDLE; //Only to query basic information. Open your own with extended rights for changes
    FDescriptionQueried: boolean;
    FDescription: string;
    FConfigQueried: boolean;
    FConfig: LPQUERY_SERVICE_CONFIG;
    FServiceDllQueried: boolean;
    FServiceDll: string;
    FLaunchProtectionQueried: boolean;
    FLaunchProtection: cardinal;
    FTriggerCountQueried: boolean;
    FTriggerCount: integer;
    function GetHandle: SC_HANDLE; inline;
    function GetConfig: LPQUERY_SERVICE_CONFIG; inline;
    function GetDescription: string; inline;
    function GetServiceDll: string; inline;
    function GetLaunchProtection: cardinal; inline;
    function GetTriggerCount: integer;
  public
    ServiceName: string;
    DisplayName: string;
    Status: SERVICE_STATUS;
    constructor CreateFromEnum(hSC: SC_HANDLE; const S: PEnumServiceStatus);
    destructor Destroy; override;
    procedure Refresh(); overload;
    function GetEffectiveDisplayName: string; virtual;
    function GetExecutableFilename: string;
    procedure GetIcon(out AImageList: TCustomImageList; out AIndex: integer); virtual;
    function CanStart: boolean; inline;
    function CanForceStart: boolean; inline;
    function CanStop: boolean; inline;
    function CanPause: boolean; inline;
    function CanResume: boolean; inline;
    function IsLaunchProtected: boolean; inline;
    property Handle: SC_HANDLE read GetHandle;
    property Description: string read GetDescription;
    property Config: LPQUERY_SERVICE_CONFIG read GetConfig;
    property ServiceDll: string read GetServiceDll;
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


constructor TServiceEntry.CreateFromEnum(hSC: SC_HANDLE; const S: PEnumServiceStatus);
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
  Self.Status := QueryServiceStatus(Self.Handle);
  Self.FConfigQueried := false;
  FreeMem(Self.FConfig);
  Self.FConfig := nil;
  Self.FLaunchProtectionQueried := false;
end;

function TServiceEntry.GetHandle: SC_HANDLE;
begin
  if Self.FHandle = 0 then
    Self.FHandle := OpenService(Self.GethSC, Self.ServiceName, SERVICE_QUERY_CONFIG or SERVICE_QUERY_STATUS);
  Result := Self.FHandle;
end;

function TServiceEntry.GetConfig: LPQUERY_SERVICE_CONFIG;
begin
  if not FConfigQueried then begin
    FConfig := QueryServiceConfig(Self.Handle);
    FConfigQueried := true;
  end;
  Result := FConfig;
end;

function TServiceEntry.GetDescription: string;
begin
  if not FDescriptionQueried then begin
    FDescription := QueryServiceDescription(Self.Handle);
    FDescriptionQueried := true;
  end;
  Result := FDescription;
end;

function TServiceEntry.GetServiceDll: string;
begin
  if not FServiceDllQueried then begin
    if Self.Config <> nil then
      if (pos('svchost.exe', Self.Config.lpBinaryPathName)>0)
      or (pos('lsass.exe', Self.Config.lpBinaryPathName)>0) then
     //^ this is not a surefire way to test it's running svchost.exe, but we don't need one
     // It would be too complicated to check that it really references svchost, and the one
     // from the system dir and not an impostor.
     // We just optimize away unneccessary registry checks.
      FServiceDll := ExpandEnvironmentStrings(QueryServiceServiceDll(Self.ServiceName));
    FServiceDllQueried := true;
  end;
  Result := FServiceDll;
end;

function TServiceEntry.GetLaunchProtection: cardinal;
var tmp: PSERVICE_LAUNCH_PROTECTED;
begin
  if not FLaunchProtectionQueried then begin
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
    FLaunchProtectionQueried := true;
  end;
  Result := FLaunchProtection;
end;

function TServiceEntry.GetTriggerCount: integer;
var triggers: PSERVICE_TRIGGER_INFO;
begin
  if not FTriggerCountQueried then begin
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


procedure TServiceEntry.GetIcon(out AImageList: TCustomImageList; out AIndex: integer);
begin
  AImageList := nil;
  AIndex := -1;
end;

function TServiceEntry.GetEffectiveDisplayName: string;
begin
  Result := Self.DisplayName;
end;

function TServiceEntry.GetExecutableFilename: string;
begin
  if Self.ServiceDll <> '' then
    Result := Self.ServiceDll
  else
  if Config <> nil then
    Result := Config.lpBinaryPathName
  else
    Result := '';
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
