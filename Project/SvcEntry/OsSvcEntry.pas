unit OsSvcEntry;
{
TServiceEntry implementation based on OS functions.
}

interface
uses SvcEntry, SysUtils, Classes, Windows, WinSvc, ServiceHelper,
  Generics.Collections;

type
  TServiceQueriedData = TServiceQueryBits;

  {
  Actual registered service on a live system
  We load a lot of these and querying the info is rather slow, so do it on-demand.
  }
  TOsServiceEntry = class(TServiceEntry)
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
    FImageInformation: TServiceImageInformation;
    function GetHandle: SC_HANDLE; inline;
    function GetConfig: LPQUERY_SERVICE_CONFIG; override;
    procedure FreeConfig;
    function GetRawDescription: string; override;
    procedure SetRawDescription(const AValue: string); override;
    function GetImageInformation: TServiceImageInformation; override;
    procedure SetImageInformation(const AValue: TServiceImageInformation); override;
    procedure UpdateStatus; override;
  public
    constructor Create(const AServiceName: string); overload;
    constructor CreateFromEnum(const S: PEnumServiceStatusProcess);
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure SetConfig(const AValue: QUERY_SERVICE_CONFIG; const APassword: PChar); override;
    property Handle: SC_HANDLE read GetHandle;
    property Description: string read GetDescription;
    property Config: LPQUERY_SERVICE_CONFIG read GetConfig; //CAN be nil if not accessible to this user

  //Helper functions
  protected
    function SetQueryBit(const ABit: TServiceQueryBit): boolean; inline;
    function QueryConfig2(const ALevel: dword; out AData: PByte): boolean; inline;
    procedure ChangeConfig2(const ALevel: dword; const AData: pointer); inline;

  //Security
  protected
    FLaunchProtection: cardinal;
    FDelayedAutostart: boolean;
    FSidType: dword;
    FRequiredPrivileges: TArray<string>;
  public
    procedure SetStartType(const AValue: dword); override;
    function GetLaunchProtection: cardinal; override;
    procedure SetLaunchProtection(const AValue: cardinal); override;
    function GetDelayedAutostart: boolean; override;
    procedure SetDelayedAutostart(const AValue: boolean); override;
    function GetSidType: dword; override;
    procedure SetSidType(const AValue: dword); override;
    function GetRequiredPrivileges: TArray<string>; override;
    procedure SetRequiredPrivileges(const AValue: TArray<string>); override;

  //Triggers
  protected
    FTriggers: PSERVICE_TRIGGER_INFO;
    function GetTriggers: PSERVICE_TRIGGER_INFO; override;
    procedure FreeTriggers;
  public
    procedure SetTriggers(const ATriggers: PSERVICE_TRIGGER_INFO); override;

  //Additional properties
  protected
    FFailureActions: LPSERVICE_FAILURE_ACTIONS;
    FFailureActionsOnNonCrashFailures: boolean;
    FPreshutdownTimeout: dword;
    FPreferredNode: integer;
    function GetFailureActions: LPSERVICE_FAILURE_ACTIONS; override;
    procedure SetFailureActions(const AValue: LPSERVICE_FAILURE_ACTIONS); override;
    procedure FreeFailureActions;
    function GetFailureActionsOnNonCrashFailures: boolean; override;
    procedure SetFailureActionsOnNonCrashFailures(const AValue: boolean); override;
    function GetPreshutdownTimeout: dword; override;
    procedure SetPreshutdownTimeout(const AValue: dword); override;
    function GetPreferredNode: integer; override;
    procedure SetPreferredNode(const ANode: integer); override;

  end;

implementation
uses WinapiHelper;

class function TOsServiceEntry.GetHSC: SC_HANDLE;
begin
  if FhSC = 0 then
    FhSC := OpenSCManager(SC_MANAGER_CONNECT);
  Result := FhSC;
 //There's no code to destroy the handle. It persists while the application runs,
 //and there's no real point in closing handles when the app is going down.
end;

class function TOsServiceEntry.GetHSC_RW: SC_HANDLE;
begin
  if FhSC_RW = 0 then
    FhSC_RW := OpenSCManager(SC_MANAGER_ALL_ACCESS);
  Result := FhSC_RW;
end;



constructor TOsServiceEntry.Create(const AServiceName: string);
begin
  inherited Create;
  Self.FServiceName := AServiceName;
  Self.Refresh; //to query status
end;

constructor TOsServiceEntry.CreateFromEnum(const S: PEnumServiceStatusProcess);
begin
  inherited Create;
  Self.FServiceName := S^.lpServiceName;
  Self.FStatus := S^.ServiceStatus;
  Self.FQueryFlags := Self.FQueryFlags + [qfStatus];
end;

destructor TOsServiceEntry.Destroy;
begin
  FreeTriggers;
  FreeFailureActions;
  if Self.FConfig <> nil then begin
    FreeMem(Self.FConfig);
    Self.FConfig := nil;
  end;
  inherited;
end;

//Reload all cached values from the SCM
procedure TOsServiceEntry.Invalidate();
begin
  Self.FQueriedData := [];
  FreeConfig;
  FreeTriggers;
end;

function TOsServiceEntry.GetHandle: SC_HANDLE;
begin
  if Self.FHandle = 0 then
    Self.FHandle := OpenService(Self.GethSC, Self.ServiceName, SERVICE_QUERY_CONFIG or SERVICE_QUERY_STATUS);
  Result := Self.FHandle;
end;

{
Sets the given cache bit if it's missing and returns true. Returns false if the
bit was already set.
Use this to test for whether you should requery cached info. Note that the bit
should be set immediately after testing, not after you retrieve the info.
This way if you fail to retreive it, you're not going to stuck querying it again
and again.
}
function TOsServiceEntry.SetQueryBit(const ABit: TServiceQueryBit): boolean;
begin
  Result := not (ABit in FQueriedData);
  if Result then
    Self.FQueriedData := Self.FQueriedData + [ABit]; //BEFORE we run potentially exception-throwing code
end;

{
Queries the given parameter with QueryServiceConfig2. Returns true + the parameter,
false if the parameter is unsupported OR otherwise RECOVERABLY unavailable by
using the default values.
Throws exception on unrecoverable errors.
}
function TOsServiceEntry.QueryConfig2(const ALevel: dword; out AData: PByte): boolean;
var err: integer;
begin
  err := QueryServiceConfig2(Self.Handle, ALevel, AData);
  case err of
  ERROR_INVALID_LEVEL, ERROR_INVALID_PARAMETER:
    Result := false;
  else
    ScmCheck(err);
    Result := true;
  end;
end;

{
Changes the given parameter with ChangeServiceConfig2. Throws if the change is
impossible.
We should throw even if the parameter is simply unsupported because the user
should know they could not set it.
}
procedure TOsServiceEntry.ChangeConfig2(const ALevel: dword; const AData: pointer);
begin
  ScmCheck(ChangeServiceConfig2(Self.GetHSC_RW, Self.ServiceName, ALevel, AData));
end;

procedure TOsServiceEntry.UpdateStatus;
begin
  Self.Status := QueryServiceStatusProcess(Self.Handle);
end;

function TOsServiceEntry.GetConfig: LPQUERY_SERVICE_CONFIG;
begin
  if SetQueryBit(qbConfig) then begin
    FreeConfig; //release older pointer
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

procedure TOsServiceEntry.SetConfig(const AValue: QUERY_SERVICE_CONFIG; const APassword: PChar);
begin
  ScmCheck(ChangeServiceConfig(Self.GetHSC_RW, Self.ServiceName, AValue, nil, APassword));
end;

//Releases the cached config structure
procedure TOsServiceEntry.FreeConfig;
begin
  FreeMem(Self.FConfig);
  Self.FConfig := nil;
end;

function TOsServiceEntry.GetRawDescription: string;
begin
  if SetQueryBit(qbDescription) then
    FDescription := QueryServiceDescription(Self.Handle);
  Result := FDescription;
end;

procedure TOsServiceEntry.SetRawDescription(const AValue: string);
var buf: SERVICE_DESCRIPTION;
begin
  buf.lpDescription := PWideChar(AValue);
  Self.ChangeConfig2(SERVICE_CONFIG_DESCRIPTION, @buf);
  Self.FDescription := AValue;
  SetQueryBit(qbDescription);
end;

function TOsServiceEntry.GetImageInformation: TServiceImageInformation;
var ADllInfo: TServiceDllInformation;
begin
  if SetQueryBit(qbImageInformation) then begin
    FImageInformation := inherited; //reads ImagePath and sets the rest to nil
    if (pos('svchost.exe', FImageInformation.ImagePath)>0)
    or (pos('lsass.exe', FImageInformation.ImagePath)>0) then begin
   //^ this is not a surefire way to test it's running svchost.exe, but we don't need one
   // It would be too complicated to check that it really references svchost, and the one
   // from the system dir and not an impostor.
   // We just optimize away unneccessary registry checks.
      ADllInfo := QueryServiceServiceDllEx(Self.ServiceName);
      FImageInformation.ServiceDll := ADllInfo.ServiceDll;
      FImageInformation.ServiceDllUnloadOnStop := ADllInfo.ServiceDllUnloadOnStop;
      FImageInformation.ServiceMain := ADllInfo.ServiceMain;
    end;
  end;
  Result := FImageInformation;
end;

procedure TOsServiceEntry.SetImageInformation(const AValue: TServiceImageInformation);
var ADllInfo: TServiceDllInformation;
begin
  inherited; //Writes ImagePath through Config and ignores the rest
  //Writing is rare so we don't second-guess and just pass all the properties for writing
  //TODO: NO! Writing requires priveleges. Avoid writing where possible.
  //  But we need to check whether the params are any different because otherwise
  //  we need to delete those keys.
  ADllInfo.ServiceDll := AValue.ServiceDll;
  ADllInfo.ServiceDllUnloadOnStop := AValue.ServiceDllUnloadOnStop;
  ADllInfo.ServiceMain := AValue.ServiceMain;
  ChangeServiceServiceDllEx(Self.ServiceName, ADllInfo);
  Self.FImageInformation := AValue;
  Self.SetQueryBit(qbImageInformation)
end;

function TOsServiceEntry.GetLaunchProtection: cardinal;
var tmp: PSERVICE_LAUNCH_PROTECTED;
begin
  if SetQueryBit(qbLaunchProtection) then begin
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

procedure TOsServiceEntry.SetLaunchProtection(const AValue: cardinal);
begin
  ChangeServiceLaunchProtected(Self.GetHSC_RW, Self.ServiceName, AValue);
end;

procedure TOsServiceEntry.SetStartType(const AValue: dword);
begin
  ChangeServiceStartType(Self.GetHSC_RW, Self.ServiceName, AValue);
end;

function TOsServiceEntry.GetDelayedAutostart: boolean;
var info: LPSERVICE_DELAYED_AUTO_START_INFO;
begin
  if SetQueryBit(qbDelayedAutostart) then begin
    if QueryConfig2(SERVICE_CONFIG_DELAYED_AUTO_START_INFO, PByte(info)) then begin
      Self.FDelayedAutostart := (info <> nil) and info.fDelayedAutostart;
      FreeMem(info);
    end else
      Self.FDelayedAutostart := false;
  end;
  Result := Self.FDelayedAutostart;
end;

//Enables/disables delayed autostart; throws if it's not supported
procedure TOsServiceEntry.SetDelayedAutostart(const AValue: boolean);
var info: SERVICE_DELAYED_AUTO_START_INFO;
begin
  //Any BOOL!=0 is supposed to be equal to TRUE, Delphi uses DWORD(-1),
  //but Win10 requires this to be exactly 1 or fails.
  dword(info.fDelayedAutostart) := 1;
  Self.ChangeConfig2(SERVICE_CONFIG_DELAYED_AUTO_START_INFO, @info);
  Self.FDelayedAutostart := AValue;
  SetQueryBit(qbDelayedAutostart);
end;


function TOsServiceEntry.GetSidType: dword;
var tmp: LPSERVICE_SID_INFO;
begin
  if SetQueryBit(qbSidType) then begin
    if QueryConfig2(SERVICE_CONFIG_SERVICE_SID_INFO, PByte(tmp)) then begin
      Self.FSidType := tmp.dwServiceSidType;
      FreeMem(tmp);
    end else
      Self.FSidType := SERVICE_SID_TYPE_NONE
  end;
  Result := Self.FSidType;
end;

procedure TOsServiceEntry.SetSidType(const AValue: dword);
var tmp: SERVICE_SID_INFO;
begin
  tmp.dwServiceSidType := AValue;
  Self.ChangeConfig2(SERVICE_CONFIG_SERVICE_SID_INFO, @tmp);
  Self.FSidType := AValue;
  SetQueryBit(qbSidType);
end;

function TOsServiceEntry.GetRequiredPrivileges: TArray<string>;
var tmp: LPSERVICE_REQUIRED_PRIVILEGES_INFO;
begin
  if SetQueryBit(qbRequiredPrivileges) then begin
    if QueryConfig2(SERVICE_CONFIG_REQUIRED_PRIVILEGES_INFO, PByte(tmp)) then begin
      Self.FRequiredPrivileges := SplitNullSeparatedList(tmp.pmszRequiredPrivileges);
      FreeMem(tmp);
    end else
      SetLength(Self.FRequiredPrivileges, 0);
  end;
  Result := Self.FRequiredPrivileges;
end;

procedure TOsServiceEntry.SetRequiredPrivileges(const AValue: TArray<string>);
var tmp: LPSERVICE_REQUIRED_PRIVILEGES_INFO;
  tmp_string: string;
begin
  tmp_string := JoinNullSeparatedList(AValue); //to keep it alive until function end
  tmp.pmszRequiredPrivileges := PWideChar(tmp_string);
  Self.ChangeConfig2(SERVICE_CONFIG_REQUIRED_PRIVILEGES_INFO, @tmp);
  Self.FRequiredPrivileges := AValue;
  SetQueryBit(qbRequiredPrivileges);
end;


function TOsServiceEntry.GetTriggers: PSERVICE_TRIGGER_INFO;
begin
  if SetQueryBit(qbTriggers) then begin
    FreeTriggers(); //in case the pointer is somehow present
    Self.FTriggers := QueryServiceTriggers(Self.Handle);
  end;
  Result := Self.FTriggers;
end;

procedure TOsServiceEntry.SetTriggers(const ATriggers: PSERVICE_TRIGGER_INFO);
begin
  Self.ChangeConfig2(SERVICE_CONFIG_TRIGGER_INFO, ATriggers);
  FreeTriggers;
  Self.FTriggers := CopyServiceTriggers(ATriggers);
  SetQueryBit(qbTriggers);
end;

procedure TOsServiceEntry.FreeTriggers;
begin
  if Self.FTriggers <> nil then begin
    FreeMem(Self.FTriggers);
    Self.FTriggers := nil;
  end;
end;


function TOsServiceEntry.GetFailureActions: LPSERVICE_FAILURE_ACTIONS;
var tmp: LPSERVICE_FAILURE_ACTIONS;
begin
  if SetQueryBit(qbFailureActions) then begin
    FreeFailureActions(); //in case the pointer is somehow present
    if QueryConfig2(SERVICE_CONFIG_FAILURE_ACTIONS, PByte(tmp)) then begin
      Self.FFailureActions := tmp;
    end else
      Self.FFailureActions := CreateEmptyFailureActions();
  end;
  Result := Self.FFailureActions;
end;

procedure TOsServiceEntry.SetFailureActions(const AValue: LPSERVICE_FAILURE_ACTIONS);
begin
  Self.ChangeConfig2(SERVICE_CONFIG_FAILURE_ACTIONS, AValue);
  FreeFailureActions;
  Self.FFailureActions := CopyFailureActions(AValue);
  SetQueryBit(qbFailureActions);
end;

procedure TOsServiceEntry.FreeFailureActions;
begin
  if Self.FFailureActions <> nil then begin
    FreeMem(Self.FFailureActions);
    Self.FFailureActions := nil;
  end;
end;

function TOsServiceEntry.GetFailureActionsOnNonCrashFailures: boolean;
var tmp: LPSERVICE_FAILURE_ACTIONS_FLAG;
begin
  if SetQueryBit(qbFailureActionsFlag) then begin
    if QueryConfig2(SERVICE_CONFIG_FAILURE_ACTIONS_FLAG, PByte(tmp)) then begin
      Self.FFailureActionsOnNonCrashFailures := tmp.fFailureActionsOnNonCrashFailures;
      FreeMem(tmp);
    end else
      Self.FFailureActionsOnNonCrashFailures := false
  end;
  Result := Self.FFailureActionsOnNonCrashFailures;
end;

procedure TOsServiceEntry.SetFailureActionsOnNonCrashFailures(const AValue: boolean);
var tmp: SERVICE_FAILURE_ACTIONS_FLAG;
begin
  tmp.fFailureActionsOnNonCrashFailures := AValue;
  Self.ChangeConfig2(SERVICE_CONFIG_FAILURE_ACTIONS_FLAG, @tmp);
  Self.FFailureActionsOnNonCrashFailures := AValue;
  SetQueryBit(qbFailureActionsFlag);
end;

function TOsServiceEntry.GetPreshutdownTimeout: dword;
var tmp: LPSERVICE_PRESHUTDOWN_INFO;
begin
  if SetQueryBit(qbPreshutdownInfo) then begin
    if QueryConfig2(SERVICE_CONFIG_PRESHUTDOWN_INFO, PByte(tmp)) then begin
      Self.FPreshutdownTimeout := tmp.dwPreshutdownTimeout;
      FreeMem(tmp);
    end else
      Self.FPreshutdownTimeout := inherited;
  end;
  Result := Self.FPreshutdownTimeout;
end;

procedure TOsServiceEntry.SetPreshutdownTimeout(const AValue: dword);
var tmp: SERVICE_PRESHUTDOWN_INFO;
begin
  tmp.dwPreshutdownTimeout := AValue;
  Self.ChangeConfig2(SERVICE_CONFIG_PRESHUTDOWN_INFO, @tmp);
  Self.FPreshutdownTimeout := AValue;
  SetQueryBit(qbPreshutdownInfo);
end;

function TOsServiceEntry.GetPreferredNode: integer;
var tmp: LPSERVICE_PREFERRED_NODE_INFO;
begin
  if SetQueryBit(qbPreferredNode) then begin
    if QueryConfig2(SERVICE_CONFIG_PREFERRED_NODE, PByte(tmp)) then begin
      Self.FPreferredNode := tmp.usPreferredNode;
      if tmp.fDelete then
        Self.FPreferredNode := SvcEntry.PREFERRED_NODE_DISABLED;
      FreeMem(tmp);
    end else
      Self.FPreferredNode := inherited;
  end;
  Result := Self.FPreferredNode;
end;

procedure TOsServiceEntry.SetPreferredNode(const ANode: integer);
var tmp: SERVICE_PREFERRED_NODE_INFO;
begin
  tmp.fDelete := (ANode = SvcEntry.PREFERRED_NODE_DISABLED);
  if tmp.fDelete then
    tmp.usPreferredNode := 0
  else
    tmp.usPreferredNode := ANode;
  Self.ChangeConfig2(SERVICE_CONFIG_PREFERRED_NODE, @tmp);
  Self.FPreferredNode := ANode;
  SetQueryBit(qbPreferredNode)
end;


end.
