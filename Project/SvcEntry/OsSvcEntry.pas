unit OsSvcEntry;
{
TServiceEntry implementation based on OS functions.
}

interface
uses SvcEntry, SysUtils, Classes, Windows, WinSvc, ServiceHelper,
  Generics.Collections;

type
  TServiceQueriedBit = (
    qbDescriptionQueried,
    qbConfigQueried,
    qbImageInformationQueried,
    qbLaunchProtectionQueried,
    qbTriggers,
    qbDelayedAutostartQueried,
    qbSidType,
    qbRequiredPrivileges
  );
  TServiceQueriedData = set of TServiceQueriedBit;

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
    procedure SetConfig(const AValue: QUERY_SERVICE_CONFIG; const APassword: PChar); override;
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
    property Handle: SC_HANDLE read GetHandle;
    property Description: string read GetDescription;
    property Config: LPQUERY_SERVICE_CONFIG read GetConfig; //CAN be nil if not accessible to this user

  //Security
  protected
    FLaunchProtection: cardinal;
    FDelayedAutostart: boolean;
    FSidType: dword;
    FRequiredPrivileges: TArray<string>;
  public
    procedure SetStartType(const Value: dword); override;
    function GetLaunchProtection: cardinal; override;
    procedure SetLaunchProtection(const AValue: cardinal); override;
    function GetDelayedAutostart: boolean; override;
    procedure SetDelayedAutostart(const Value: boolean); override;
    function GetSidType: dword; override;
    procedure SetSidType(const AValue: dword); override;
    function GetRequiredPrivileges: TArray<string>; override;
    procedure SetRequiredPrivileges(const AValue: TArray<string>); override;

  //Triggers
  protected
    FTriggers: PSERVICE_TRIGGER_INFO;
    function GetTriggers: PSERVICE_TRIGGER_INFO; override;
    procedure SetTriggers(const ATriggers: PSERVICE_TRIGGER_INFO); override;
    procedure FreeTriggers;

  //Additional properties
  protected
    function GetFailureActions: LPSERVICE_FAILURE_ACTIONS; override;
    procedure SetFailureActions(const AValue: LPSERVICE_FAILURE_ACTIONS); override;
    function GetFailureActionsOnNonCrashFailures: boolean; override;
    procedure SetFailureActionsOnNonCrashFailures(const AValue: boolean); override;
    function GetPreshutdownTimeout: dword; override;
    procedure SetPreshutdownTimeout(const AValue: dword); override;
    function GetPreferredNode: integer; override;
    procedure SetPreferredNodeInfo(const ANode: integer); override;

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

procedure TOsServiceEntry.UpdateStatus;
begin
  Self.Status := QueryServiceStatusProcess(Self.Handle);
end;

function TOsServiceEntry.GetConfig: LPQUERY_SERVICE_CONFIG;
begin
  if not (qbConfigQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbConfigQueried]; //BEFORE we run potentially exception-throwing code
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
  if not (qbDescriptionQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbDescriptionQueried];
    FDescription := QueryServiceDescription(Self.Handle);
  end;
  Result := FDescription;
end;

procedure TOsServiceEntry.SetRawDescription(const AValue: string);
var buf: SERVICE_DESCRIPTION;
begin
  buf.lpDescription := PWideChar(AValue);
  ScmCheck(ChangeServiceConfig2(Self.GetHSC_RW, Self.ServiceName, SERVICE_CONFIG_DESCRIPTION, @buf));
end;

function TOsServiceEntry.GetImageInformation: TServiceImageInformation;
var ADllInfo: TServiceDllInformation;
begin
  if not (qbImageInformationQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbImageInformationQueried];
    Result := inherited; //reads ImagePath and sets the rest to nil
    if (pos('svchost.exe', Result.ImagePath)>0)
    or (pos('lsass.exe', Result.ImagePath)>0) then begin
   //^ this is not a surefire way to test it's running svchost.exe, but we don't need one
   // It would be too complicated to check that it really references svchost, and the one
   // from the system dir and not an impostor.
   // We just optimize away unneccessary registry checks.
      ADllInfo := QueryServiceServiceDllEx(Self.ServiceName);
      Result.ServiceDll := ADllInfo.ServiceDll;
      Result.ServiceDllUnloadOnStop := ADllInfo.ServiceDllUnloadOnStop;
      Result.ServiceMain := ADllInfo.ServiceMain;
    end;
  end;
  Result := FImageInformation;
end;

function TOsServiceEntry.GetLaunchProtection: cardinal;
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

procedure TOsServiceEntry.SetLaunchProtection(const AValue: cardinal);
begin
  ChangeServiceLaunchProtected(Self.GetHSC_RW, Self.ServiceName, AValue);
end;

procedure TOsServiceEntry.SetStartType(const Value: dword);
begin
  ChangeServiceStartType(Self.GetHSC_RW, Self.ServiceName, Value);
end;

function TOsServiceEntry.GetDelayedAutostart: boolean;
var info: LPSERVICE_DELAYED_AUTO_START_INFO;
  err: integer;
begin
  if not (qbDelayedAutostartQueried in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbDelayedAutostartQueried];
    err := QueryServiceConfig2(Self.Handle, LPSERVICE_DELAYED_AUTO_START_INFO, PByte(info));
    case err of
    ERROR_INVALID_LEVEL, ERROR_INVALID_PARAMETER:
      Self.FDelayedAutostart := false;
    else
      ScmCheck(err);
      Self.FDelayedAutostart := (info <> nil) and info.fDelayedAutostart;
      FreeMem(info);
    end;
  end;
  Result := Self.FDelayedAutostart;
end;

//Enables/disables delayed autostart; throws if it's not supported
procedure TOsServiceEntry.SetDelayedAutostart(const Value: boolean);
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

function TOsServiceEntry.GetSidType: dword;
var tmp: SERVICE_SID_INFO;
  err: integer;
begin
  if not (qbSidType in FQueriedData) then begin
    FQueriedData := FQueriedData + [qbSidType];
    err := QueryServiceConfig2(Self.Handle, SERVICE_CONFIG_SERVICE_SID_INFO, PByte(tmp));
    case err of
    ERROR_INVALID_LEVEL, ERROR_INVALID_PARAMETER:
      Self.FSidType := SERVICE_SID_TYPE_NONE
    else
      ScmCheck(err);
      Self.FSidType := tmp.dwServiceSidType;
      FreeMem(tmp);
    end;
  end;
  Result := Self.FSidType;
end;

procedure TOsServiceEntry.SetSidType(const AValue: dword);
var tmp: SERVICE_SID_INFO;
begin
  tmp.dwServiceSidType := AValue;
  ScmCheck(ChangeServiceConfig2(Self.GetHSC_RW, Self.ServiceName, SERVICE_CONFIG_SERVICE_SID_INFO, @tmp));
  Self.FSidType := AValue;
  Self.FQueriedData := Self.FQueriedData + qbSidType;
end;


function TOsServiceEntry.GetTriggers: PSERVICE_TRIGGER_INFO;
begin
  if not (qbTriggers in Self.FQueriedData) then begin
    Self.FQueriedData := Self.FQueriedData + [qbTriggers];
    FreeTriggers(); //in case the pointer is somehow present
    Self.FTriggers := QueryServiceTriggers(Self.Handle);
  end;
  Result := Self.FTriggers;
end;

procedure TOsServiceEntry.FreeTriggers;
begin
  if Self.FTriggers <> nil then begin
    FreeMem(Self.FTriggers);
    Self.FTriggers := nil;
  end;
end;


end.
