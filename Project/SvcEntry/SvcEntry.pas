unit SvcEntry;

interface
uses SysUtils, Classes, Windows, WinSvc, ServiceHelper, Generics.Collections,
  ImgList;

const
  {
  Additional autostart mode which you can only use with TServiceEntry's Get/SetStartMode().
  In SCM delayed autostart is stored as SERVICE_AUTO_START + additional flag.

  Any service can be marked as a delayed auto-start service; however, this
  setting has no effect unless the service is an auto-start service.
  https://docs.microsoft.com/en-us/windows/desktop/api/winsvc/ns-winsvc-_service_delayed_auto_start_info
  }
  SERVICE_DELAYED_AUTOSTART = $00010002;

  //We use a special constant to signal that PFN is disabled, while SCM uses a flag
  //SCM's PFN is ushort so MAXINT is certainly safe for claiming
  PREFERRED_NODE_DISABLED = MAXINT;

type
  //Image path information
  TServiceImageInformation = record
    ImagePath: string;
    ServiceDll: string;
    ServiceDllUnloadOnStop: boolean;
    ServiceMain: string;
  end;

  TServiceEntryQueryFlag = (
    qfStatus,
    qfImageInformation,
    qfDependencies
  );
  TServiceEntryQueryFlags = set of TServiceEntryQueryFlag;

  TServiceEntry = class
  protected
    FQueryFlags: TServiceEntryQueryFlags;
    procedure NotImplemented;
  public
    constructor Create; overload;
    constructor Create(const AServiceName: string); overload;
    procedure Invalidate; virtual;
    procedure Refresh; virtual;
    procedure GetIcon(out AImageList: TCustomImageList; out AIndex: integer); virtual;

  //Basic information
  protected
    FServiceName: string;
    function GetConfig: LPQUERY_SERVICE_CONFIG; virtual;
    function GetServiceType: dword; inline;
    function GetRawDisplayName: string; inline;
    procedure SetRawDisplayName(const AValue: string); virtual;
    function GetRawDescription: string; virtual;
    procedure SetRawDescription(const AValue: string); virtual;
    function GetDisplayName: string; virtual;
    procedure SetDisplayName(const AValue: string); virtual;
    function GetDescription: string; virtual;
    procedure SetDescription(const AValue: string); virtual;
  public
    procedure SetConfig(const AValue: QUERY_SERVICE_CONFIG; const APassword: PChar); virtual;
    function GetEffectiveDisplayName: string; virtual;
    property Config: LPQUERY_SERVICE_CONFIG read GetConfig; //can be nil!
    property ServiceName: string read FServiceName write FServiceName;
    property ServiceType: dword read GetServiceType;
    property RawDisplayName: string read GetRawDisplayName write SetRawDisplayName;
    property RawDescription: string read GetRawDescription write SetRawDescription;
    property DisplayName: string read GetDisplayName write SetDisplayName;
    property Description: string read GetDescription write SetDescription;

  //Dependencies
  protected
    FDependencies: TArray<string>; //cached from config
    function GetDependencyList: string; inline;
    procedure SetDependencyList(const AValue: string); virtual;
    function GetDependencies: TArray<string>; inline;
    function FindDependencyIndex(const AServiceName: string): integer;
  public
    function DependsOn(const AServiceName: string): boolean; inline;
    procedure SetDependencies(const AValue: TArray<string>);
    procedure AddDependency(const AServiceName: string);
    procedure DeleteDependency(const AServiceName: string);
    property DependencyList: string read GetDependencyList;
    property Dependencies: TArray<string> read GetDependencies;

  //Start type
  public
    function GetStartType: dword; inline;
    procedure SetStartType(const AValue: dword); virtual;
    function GetDelayedAutostart: boolean; virtual;
    procedure SetDelayedAutostart(const AValue: boolean); virtual;
    function GetStartTypeEx: dword; inline;
    procedure SetStartTypeEx(const AValue: dword);
    function GetLoadOrderGroup: string; inline;
    function GetTagId: dword; inline;
    procedure SetLoadOrderGroup(const AGroup: string); virtual;
    property StartType: dword read GetStartType write SetStartType;
    property DelayedAutostart: boolean read GetDelayedAutostart write SetDelayedAutostart;
    property StartTypeEx: dword read GetStartTypeEx write SetStartTypeEx;
    property LoadOrderGroup: string read GetLoadOrderGroup; //can only be set by SetLoadOrderGroup
    property TagId: dword read GetTagId; //can only be set by SetLoadOrderGroup

  //Image information.
  //Cached. Must be set at once.
  protected
    FImageInformation: TServiceImageInformation;
    function GetImageInformation: TServiceImageInformation; virtual;
    procedure SetImageInformation(const AValue: TServiceImageInformation); virtual;
    function GetImageInformationCached: TServiceImageInformation; inline;
    procedure SetImageInformationCached(const AValue: TServiceImageInformation); inline;
  public
    function GetRawImagePath: string; inline;
    function GetRawServiceDll: string; inline;
    function GetImageFilename: string;
    property ImageInformation: TServiceImageInformation read GetImageInformationCached
      write SetImageInformationCached;

  //Status
  //Each service object HAS a status structure but it may not be filled with real info
  //if it's not supproted
  protected
    FStatus: SERVICE_STATUS_PROCESS;
    function GetStatus: SERVICE_STATUS_PROCESS; inline;
    procedure UpdateStatus; overload; virtual;
    function GetCurrentState: dword; inline;
    function GetControlsAccepted: dword; inline;
  public
    function CanStart: boolean; inline;
    function CanForceStart: boolean; inline;
    function CanStop: boolean; inline;
    function CanPause: boolean; inline;
    function CanResume: boolean; inline;
    procedure UpdateStatus(const AStatus: SERVICE_STATUS_PROCESS); overload; virtual;
    procedure UpdateStatus(const AStatus: SERVICE_STATUS); overload; virtual;
    property Status: SERVICE_STATUS_PROCESS read GetStatus write UpdateStatus;

  //Security
  protected
    function GetSidType: dword; virtual;
    procedure SetSidType(const AValue: dword); virtual;
    function GetRequiredPrivileges: TArray<string>; virtual;
    procedure SetRequiredPrivileges(const AValue: TArray<string>); virtual;
  public
    function GetLaunchProtection: cardinal; virtual;
    procedure SetLaunchProtection(const AValue: cardinal); virtual;
    function IsLaunchProtected: boolean; inline;
    property LaunchProtection: cardinal read GetLaunchProtection;
    property SidType: dword read GetSidType write SetSidType;
    property RequiredPrivileges: TArray<string> read GetRequiredPrivileges
      write SetRequiredPrivileges;

  //Triggers
  protected
    function GetTriggers: PSERVICE_TRIGGER_INFO; virtual;
    function GetTriggerCount: integer; inline;
  public
    procedure SetTriggers(const ATriggers: PSERVICE_TRIGGER_INFO); virtual;
    property Triggers: PSERVICE_TRIGGER_INFO read GetTriggers; //can be nil
    property TriggerCount: integer read GetTriggerCount;

  //Additional properties obtained through QueryServiceConfig2
  protected
    function GetFailureActions: LPSERVICE_FAILURE_ACTIONS; virtual;
    procedure SetFailureActions(const AValue: LPSERVICE_FAILURE_ACTIONS); virtual;
    function GetFailureActionsOnNonCrashFailures: boolean; virtual;
    procedure SetFailureActionsOnNonCrashFailures(const AValue: boolean); virtual;
    function GetPreshutdownTimeout: dword; virtual;
    procedure SetPreshutdownTimeout(const AValue: dword); virtual;
    function GetPreferredNode: integer; virtual;
    procedure SetPreferredNodeInfo(const ANode: integer); virtual;


  end;
  PServiceEntry = ^TServiceEntry;

  EServiceEntryException = class(Exception);

  TServiceEntries = array of TServiceEntry;
  PServiceEntries = ^TServiceEntries;

  TServiceEntryList = class(TObjectList<TServiceEntry>)
  public
    function FindIndex(const AServiceName: string): integer;
    function Find(const AServiceName: string): TServiceEntry;
  end;


//Subscribe to be notified of changes to the services.
var
  OnServiceInvalidated: TList<TNotifyEvent>;

//Call when the service changes for any reason (internal or external)
procedure InvalidateService(Service: TServiceEntry);

//Call to refresh the service data and invalidate the service
procedure RefreshService(Service: TServiceEntry);


implementation
uses WinApiHelper;

resourcestring
  eServiceEntryFunctionNotImplemented = 'Cannot perform the requested operation for this service';

constructor TServiceEntry.Create;
begin
  inherited;
  FQueryFlags := [];
end;

constructor TServiceEntry.Create(const AServiceName: string);
begin
  Self.Create;
  FServiceName := AServiceName;
end;

procedure TServiceEntry.NotImplemented;
begin
  raise EServiceEntryException.Create(eServiceEntryFunctionNotImplemented);
end;

//Tells this TServiceEntry instance that all cached fields for this service
//should be marked invalid.
procedure TServiceEntry.Invalidate;
begin
  FQueryFlags := [];
end;

//Tells this TServiceEntry to reload all data.
//As implemented, it simply Invalidates it and it's best to stick to this principle
procedure TServiceEntry.Refresh;
begin
  Invalidate;
end;


procedure TServiceEntry.GetIcon(out AImageList: TCustomImageList; out AIndex: integer);
begin
  AImageList := nil;
  AIndex := -1;
end;


//Retrieves basic service config structure.
//Nil if unavailable for this service or not enough rights.
function TServiceEntry.GetConfig: LPQUERY_SERVICE_CONFIG;
begin
  Result := nil;
end;

{
Changes basic service configuration for this service.
The rules are the same as for ChangeServiceConfig parameters. If you don't want
to change a parameter, pass the appropriate NULL/NO_CHANGE value.
https://docs.microsoft.com/en-us/windows/desktop/api/winsvc/nf-winsvc-changeserviceconfiga

* If you set AValue.ServiceStartName (Username), you also may need to pass APassword.
* You cannot change TagId this way.
  TagIds are *generated* by the SCM and *returned* to you and you then should
  register them in certain registry keys (see docs).
  At the moment you cannot change TagIds at all, this requires some thought.
}
procedure TServiceEntry.SetConfig(const AValue: QUERY_SERVICE_CONFIG; const APassword: PChar);
begin
  NotImplemented;
end;

function TServiceEntry.GetServiceType: dword;
var LConfig: LPQUERY_SERVICE_CONFIG;
begin
  LConfig := Self.GetConfig;
  if LConfig <> nil then
    Result := LConfig.dwServiceType
  else
    Result := SERVICE_WIN32; //default type
end;


{
Raw display name and description do not substitute @dllname,index type resource
references.
}

function TServiceEntry.GetRawDisplayName: string;
var LConfig: LPQUERY_SERVICE_CONFIG;
begin
  LConfig := Config;
  if LConfig <> nil then
    Result := LConfig.lpDisplayName
  else
    Result := '';
end;

//Sets service display name.
procedure TServiceEntry.SetRawDisplayName(const AValue: string);
begin
  //Can be implemented via SetConfig but we are lazy.
  NotImplemented;
end;

function TServiceEntry.GetRawDescription: string;
begin
  Result := '';
end;

procedure TServiceEntry.SetRawDescription(const AValue: string);
begin
  NotImplemented;
end;

{
Pretty display name and description do substite resource strings, therefore
less suited for editing.
Note that when editing foreign SCM databases substituting resources locally
might not be wise. Such decendants should just override and return raw values.
}

function TServiceEntry.GetDisplayName: string;
begin
  Result := GetRawDisplayName; //TODO: Expand
  if Result = '' then
    Result := Self.ServiceName;
end;

procedure TServiceEntry.SetDisplayName(const AValue: string);
begin
  SetRawDisplayName(AValue);
end;

function TServiceEntry.GetDescription: string;
begin
  Result := GetRawDescription; //TODO: Expand
end;

procedure TServiceEntry.SetDescription(const AValue: string);
begin
  SetRawDescription(AValue);
end;

{
_Effective_ display name is even more abstract. It doesn't have to related
to SCM's DisplayName at all. It's what the descendants consider the service
shoud be displayed as.
}
function TServiceEntry.GetEffectiveDisplayName: string;
begin
  Result := GetDisplayName;
end;

{
Dependencies
}
function TServiceEntry.GetDependencyList: string;
var LConfig: LPQUERY_SERVICE_CONFIG;
begin
  LConfig := Config;
  if LConfig <> nil then
    Result := LConfig.lpDependencies
  else
    Result := '';
end;

procedure TServiceEntry.SetDependencyList(const AValue: string);
begin
  //Can be implemeneted via SetConfig
  NotImplemented;
  Self.FQueryFlags := Self.FQueryFlags - [qfDependencies]; //don't forget
end;

function TServiceEntry.GetDependencies: TArray<string>;
var LConfig: LPQUERY_SERVICE_CONFIG;
begin
  //Cache results to avoid repeating the parsing
  if not (qfDependencies in Self.FQueryFlags) then begin
    Self.FQueryFlags := Self.FQueryFlags + [qfDependencies];
    LConfig := Config;
    if LConfig <> nil then
      Self.FDependencies := SplitNullSeparatedList(LConfig.lpDependencies)
    else
      SetLength(Self.FDependencies, 0);
  end;
  Result := Self.FDependencies;
end;

//AServiceName needs to include GROUP_MARK if it's a group
function FindDependencyIndex(const AList: TArray<string>; const AServiceName: string): integer; overload;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(AList)-1 do
    if SameText(AServiceName, AList[i]) then begin
      Result := i;
      break;
    end;
end;

//AServiceName needs to include GROUP_MARK if it's a group
function TServiceEntry.FindDependencyIndex(const AServiceName: string): integer;
var LDependencies: TArray<string>;
begin
  LDependencies := Self.Dependencies;
  Result := SvcEntry.FindDependencyIndex(LDependencies, AServiceName);
end;

//True if a given string is included in service dependencies
//AServiceName needs to include GROUP_MARK if it's a group
function TServiceEntry.DependsOn(const AServiceName: string): boolean;
begin
  Result := Self.FindDependencyIndex(AServiceName) >= 0;
end;

procedure TServiceEntry.SetDependencies(const AValue: TArray<string>);
begin
  SetDependencyList(JoinNullSeparatedList(AValue));
end;

//AServiceName needs to include GROUP_MARK if it's a group
procedure TServiceEntry.AddDependency(const AServiceName: string);
var LDependencies: TArray<string>;
begin
  LDependencies := GetDependencies;
  if SvcEntry.FindDependencyIndex(LDependencies, AServiceName) >= 0 then
    exit;
  SetLength(LDependencies, Length(LDependencies)+1);
  LDependencies[Length(LDependencies)-1] := AServiceName;
  SetDependencies(LDependencies);
end;

procedure TServiceEntry.DeleteDependency(const AServiceName: string);
var LDependencies: TArray<string>;
  i: integer;
begin
  LDependencies := GetDependencies;
  i := SvcEntry.FindDependencyIndex(LDependencies, AServiceName);
  if i < 0 then exit;
  while i < Length(LDependencies)-1 do
    LDependencies[i+1] := LDependencies[i];
  SetLength(LDependencies, Length(LDependencies)-1);
  SetDependencies(LDependencies);
end;


{
Service image information. Depends on the service type.
SERVICE_WIN32_OWN_PROCESS or others:
  The service is started in a separate process.
  Only ImagePath is used.
SERVICE_WIN32_SHARE_PROCESS:
  The service is started as a part of svchost.exe shared process.
  All fields are used. ImagePath points to svchost.exe.
All fields must be set at once. Some fields might contain environment variables.
}
function TServiceEntry.GetImageInformation: TServiceImageInformation;
var LConfig: LPQUERY_SERVICE_CONFIG;
begin
  LConfig := Config;
  if LConfig <> nil then
    Result.ImagePath := LConfig.lpBinaryPathName
  else
    Result.ImagePath := '';
  Result.ServiceDll := '';
  Result.ServiceDllUnloadOnStop := false;
  Result.ServiceMain := '';
end;

procedure TServiceEntry.SetImageInformation(const AValue: TServiceImageInformation);
begin
  //ImagePath can be set via Config
  NotImplemented;
end;

function TServiceEntry.GetImageInformationCached: TServiceImageInformation;
begin
  if not (qfImageInformation in Self.FQueryFlags) then begin
    Self.FQueryFlags := Self.FQueryFlags + [qfImageInformation];
    Self.FImageInformation := Self.GetImageInformation;
  end;
  Result := Self.FImageInformation;
end;

procedure TServiceEntry.SetImageInformationCached(const AValue: TServiceImageInformation);
begin
  Self.FQueryFlags := Self.FQueryFlags - [qfImageInformation];
  Self.SetImageInformation(AValue);
end;

//Shortcut. Returns the raw ImagePath parameter
function TServiceEntry.GetRawImagePath: string;
begin
  Result := GetImageInformation.ImagePath;
end;

//Shortcut. Returns the raw ServiceDll parameter
function TServiceEntry.GetRawServiceDll: string;
begin
  Result := GetImageInformation.ServiceDll;
end;

//Points to the "actual place with code to run".
//Returns either the EXE name or the DLL name, depending on which is more specific,
//with environment strings expanded.
function TServiceEntry.GetImageFilename: string;
begin
  if Self.ServiceType = SERVICE_WIN32_SHARE_PROCESS then
    Result := ExpandEnvironmentStrings(Self.GetRawServiceDll)
  else
    Result := ExpandEnvironmentStrings(Self.GetRawImagePath)
{
There's also this way used before. May return to it if checking by type fails:
      if (pos('svchost.exe', Self.Config.lpBinaryPathName)>0)
      or (pos('lsass.exe', Self.Config.lpBinaryPathName)>0) then
     //^ this is not a surefire way to test it's running svchost.exe, but we don't need one
     // It would be too complicated to check that it really references svchost, and the one
     // from the system dir and not an impostor.
     // We just optimize away unneccessary registry checks.
There's also this way:
  if Self.ServiceDll <> '' then
    Result := Self.ServiceDll
  else
  if Config <> nil then
    Result := Config.lpBinaryPathName
  else
    Result := '';
}
end;


{
Start type settings
}

//Returns the raw autostart value, as the OS uses it (without the INTERACTIVE addition too)
function TServiceEntry.GetStartType: dword;
var LConfig: LPQUERY_SERVICE_CONFIG;
begin
  LConfig := Config;
  if LConfig <> nil then
    Result := LConfig.dwStartType and not SERVICE_INTERACTIVE_PROCESS
  else
    Result := SERVICE_DEMAND_START; ///can't tell so assume default
end;

//Sets the raw autostart value
procedure TServiceEntry.SetStartType(const AValue: dword);
begin
  NotImplemented;
end;

function TServiceEntry.GetDelayedAutostart: boolean;
begin
  Result := false;
end;

procedure TServiceEntry.SetDelayedAutostart(const AValue: boolean);
begin
  NotImplemented;
end;

//Returns enhanced StartType which includes delayed autostart
function TServiceEntry.GetStartTypeEx: dword;
begin
  Result := Self.StartType;
  if (Result = SERVICE_AUTO_START) and Self.GetDelayedAutostart then //auto start supports additional delayed flag
    Result := SERVICE_DELAYED_AUTOSTART;
end;

//Sets enhanced start type which includes delayed autostart
procedure TServiceEntry.SetStartTypeEx(const AValue: dword);
begin
  if AValue = SERVICE_DELAYED_AUTOSTART then begin
    //Set the delayed flag first so that if it's not supported we don't change anything
    Self.SetDelayedAutostart(true);
    Self.SetStartType(SERVICE_AUTO_START);
  end else
    Self.SetStartType(AValue);
end;

{
A group to which this service belongs. Services can depend on groups,
and groups together with TagId are used to decide on load order for drivers.
}

function TServiceEntry.GetLoadOrderGroup: string;
var LConfig: LPQUERY_SERVICE_CONFIG;
begin
  LConfig := Config;
  if (LConfig = nil) or (LConfig.lpLoadOrderGroup = nil) then
    Result := ''
  else
    Result := LConfig.lpLoadOrderGroup;
end;

function TServiceEntry.GetTagId: dword;
var LConfig: LPQUERY_SERVICE_CONFIG;
begin
  LConfig := Config;
  if (LConfig = nil) or (LConfig.lpLoadOrderGroup = nil) then
    Result := 0
  else
    Result := LConfig.dwTagId;
end;

procedure TServiceEntry.SetLoadOrderGroup(const AGroup: string);
var LConfig: QUERY_SERVICE_CONFIG;
begin
  FillChar(LConfig, 0, SizeOf(LConfig));
  LConfig.lpLoadOrderGroup := PChar(AGroup);
  Self.SetConfig(LConfig, '');
end;


{
Status
All service objects have the status structure but not all populate it with
meaningful info.
}
//Returns the current status structure
function TServiceEntry.GetStatus: SERVICE_STATUS_PROCESS;
begin
  if not (qfStatus in Self.FQueryFlags) then begin
    Self.FQueryFlags := Self.FQueryFlags + [qfStatus];
    Self.UpdateStatus;
  end;
  Result := Self.FStatus;
end;

//Populates the status structure with actual information
//Descendants can override to query the status on demand if needed
procedure TServiceEntry.UpdateStatus;
begin
  FillChar(Self.FStatus, 0, SizeOf(Self.FStatus));
  FStatus.dwCurrentState := SERVICE_STOPPED; //we don't know better
  FStatus.dwControlsAccepted := 0; //assume nothing is accepted
end;

//Allows the outside observer to update service status fields. This often
//happens as a result of sending controls to the service.
//The function is virtual so the descendants can choose to ignore such updates.
procedure TServiceEntry.UpdateStatus(const AStatus: SERVICE_STATUS_PROCESS);
begin
  Self.FStatus := AStatus;
  Self.FQueryFlags := Self.FQueryFlags + [qfStatus];
end;

procedure TServiceEntry.UpdateStatus(const AStatus: SERVICE_STATUS);
begin
  //SERVICE_STATUS_PROCESS which we keep starts with SERVICE_STATUS
  LPSERVICE_STATUS(@Self.FStatus)^ := AStatus;
  //Do not set the qfStatus flag: we still don't have some fields. But keep it
  //if it's set.
end;

function TServiceEntry.GetCurrentState: dword;
begin
  Result := Status.dwCurrentState;
end;

function TServiceEntry.GetControlsAccepted: dword;
begin
  Result := Status.dwControlsAccepted;
end;

{
The following funcitons give us an estimation of when we can and cannot do
something with the service.
}

function TServiceEntry.CanStart: boolean;
begin
  Result := (GetCurrentState = SERVICE_STOPPED) and (GetStartType <> SERVICE_DISABLED);
end;

//Force-start is "Enable, Start, Restore old start type"
function TServiceEntry.CanForceStart: boolean;
begin
  Result := GetCurrentState = SERVICE_STOPPED;
end;

function TServiceEntry.CanStop: boolean;
var LState: dword;
begin
  LState := Self.GetCurrentState;
  Result := (LState <> SERVICE_STOPPED) and (LState <> SERVICE_STOP_PENDING);
  //May also check ControlsAccepted for SERVICE_ACCEPT_STOP
end;

function TServiceEntry.CanPause: boolean;
begin
  Result := GetCurrentState = SERVICE_RUNNING;
  //May also check SERVICE_ACCEPT_PAUSE_CONTINUE
end;

function TServiceEntry.CanResume: boolean;
begin
  Result := GetCurrentState = SERVICE_PAUSED;
  //May also check SERVICE_ACCEPT_PAUSE_CONTINUE
end;


{
Launch protection.
Editing launch protection is complicated so not unified here yet.
}

function TServiceEntry.GetLaunchProtection: cardinal;
begin
  Result := SERVICE_LAUNCH_PROTECTED_NONE;
end;

procedure TServiceEntry.SetLaunchProtection(const AValue: cardinal);
begin
  NotImplemented;
end;

function TServiceEntry.IsLaunchProtected: boolean;
begin
  Result := Self.LaunchProtection <> SERVICE_LAUNCH_PROTECTED_NONE;
end;

function TServiceEntry.GetSidType: dword;
begin
  Result := SERVICE_SID_TYPE_NONE;
end;

procedure TServiceEntry.SetSidType(const AValue: dword);
begin
  NotImplemented;
end;

function TServiceEntry.GetRequiredPrivileges: TArray<string>;
begin
  SetLength(Result, 0);
end;

procedure TServiceEntry.SetRequiredPrivileges(const AValue: TArray<string>);
begin
  NotImplemented;
end;


{
Triggers
}

//Returns PSERVICE_TRIGGER_INFO for this service or nil if its unavailable
//The returned pointer is owned by TServiceEntry and should not be freed.
function TServiceEntry.GetTriggers: PSERVICE_TRIGGER_INFO;
begin
  Result := nil;
end;

//Sets new trigger configuration for the service.
//The passed pointer is owned by the caller and should be freed by the caller
procedure TServiceEntry.SetTriggers(const ATriggers: PSERVICE_TRIGGER_INFO);
begin
  NotImplemented;
end;

function TServiceEntry.GetTriggerCount: integer;
var LTriggers: PSERVICE_TRIGGER_INFO;
begin
  LTriggers := Self.Triggers;
  if LTriggers <> nil then
    Result := triggers.cTriggers
  else
    Result := 0;
end;


{
Additional properties available via QueryServiceConfig2
}

//Retrieves the LPSERVICE_FAILURE_ACTIONS structure for this service, or nil if
//it's unavailable.
//The structure is allocated by TServiceEntry and should not be freed.
function TServiceEntry.GetFailureActions: LPSERVICE_FAILURE_ACTIONS;
begin
  Result := nil;
end;

//Overwrites LPSERVICE_FAILURE_ACTIONS contents for this service with the given
//information.
//The passed structure is owned and should be freed by the caller.
procedure TServiceEntry.SetFailureActions(const AValue: LPSERVICE_FAILURE_ACTIONS);
begin
  NotImplemented;
end;

function TServiceEntry.GetFailureActionsOnNonCrashFailures: boolean;
begin
  Result := false;
end;

procedure TServiceEntry.SetFailureActionsOnNonCrashFailures(const AValue: boolean);
begin
  NotImplemented;
end;

function TServiceEntry.GetPreshutdownTimeout: dword;
begin
  Result := 0;
end;

procedure TServiceEntry.SetPreshutdownTimeout(const AValue: dword);
begin
  NotImplemented;
end;

//Returns the preferred node for this service
function TServiceEntry.GetPreferredNode: integer;
begin
  Result := PREFERRED_NODE_DISABLED;
end;

//Sets the preferred node or deletes this setting
procedure TServiceEntry.SetPreferredNodeInfo(const ANode: integer);
begin
  NotImplemented;
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
