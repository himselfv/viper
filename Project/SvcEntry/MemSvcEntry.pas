unit MemSvcEntry;
{
In-memory TServiceEntry
}

interface
uses SysUtils, Windows, WinSvc, SvcEntry;

type
  TMemServiceEntry = class(TServiceEntry)
  public
    constructor Create;
    destructor Destroy; override;

  protected
    FConfig: QUERY_SERVICE_CONFIG;
    //FConfig contains pointers to these. Please remember to update it if you change these!
    FBinaryPathName: string;
    FLoadOrderGroup: string;
    FDependencies: string;
    FServiceStartName: string;
    FDisplayName: string;
    FDescription: string;
    function GetConfig: LPQUERY_SERVICE_CONFIG; override;
    procedure SetConfig(const AValue: QUERY_SERVICE_CONFIG; const APassword: string); override;
    function GetRawDescription: string; override;
    procedure SetRawDescription(const AValue: string); override;

  public
    FDelayedAutostart: boolean;
    FImageInformation: TServiceImageInformation;
    function GetDelayedAutostart: boolean; override;
    procedure SetDelayedAutostart(const AValue: boolean); override;
    function GetImageInformation: TServiceImageInformation; override;
    procedure SetImageInformation(const AValue: TServiceImageInformation); override;

  protected
    FSidType: dword;
    FRequiredPrivileges: TArray<string>;
    FLaunchProtection: cardinal;
    function GetSidType: dword; override;
    procedure SetSidType(const AValue: dword); override;
    function GetRequiredPrivileges: TArray<string>; override;
    procedure SetRequiredPrivileges(const AValue: TArray<string>); override;
    function GetLaunchProtection: cardinal; override;
    procedure SetLaunchProtection(const AValue: cardinal); override;

  protected
    class var FEmptyTriggers: SERVICE_TRIGGER_INFO;
  protected
    FTriggers: PSERVICE_TRIGGER_INFO;
    function GetTriggers: PSERVICE_TRIGGER_INFO; virtual;
    procedure SetTriggers(const ANewTriggers: PSERVICE_TRIGGER_INFO); virtual;
    procedure FreeTriggers;

  protected
    FFailureActionsOnNonCrashFailures: boolean;
    FPreshutdownTimeout: dword;
    FPreferredNode: integer;
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
uses ServiceHelper;

constructor TMemServiceEntry.Create;
begin
  inherited Create;
  FillChar(@Self.FConfig, 0, SizeOf(Self.FConfig));
  //Each instance will override this class var but whatever:
  FEmptyTriggers.cTriggers := 0;
  FEmptyTriggers.pTriggers := nil;
  FPreferredNode := PREFERRED_NODE_DISABLED;
end;

destructor TMemServiceEntry.Destroy;
begin
  FreeTriggers;
  inherited Destroy;
end;

function TMemServiceEntry.GetConfig: LPQUERY_SERVICE_CONFIG;
begin
  Result := @Self.FConfig;
end;

procedure TMemServiceEntry.SetConfig(const AValue: QUERY_SERVICE_CONFIG; const APassword: string);
begin
{
Empty arguments must be handled in exactly the same way as ChangeServiceConfig!
https://docs.microsoft.com/en-us/windows/desktop/api/winsvc/nf-winsvc-changeserviceconfiga
}
  if AValue.dwServiceType <> SERVICE_NO_CHANGE then
    Self.FConfig.dwServiceType := AValue.dwServiceType;
  if AValue.dwStartType <> SERVICE_NO_CHANGE then
    Self.FConfig.dwStartType := AValue.dwStartType;
  if AValue.dwErrorControl <> SERVICE_NO_CHANGE then
    Self.FConfig.dwErrorControl := AValue.dwErrorControl;
  if AValue.lpBinaryPathName <> nil then begin
    Self.FBinaryPathName := AValue.lpBinaryPathName;
    Self.FConfig.lpBinaryPathName := PWideChar(Self.FBinaryPathName);
  end;
  if AValue.lpLoadOrderGroup <> nil then begin
    Self.FLoadOrderGroup := AValue.lpLoadOrderGroup;
    Self.FConfig.lpLoadOrderGroup := PWideChar(Self.FLoadOrderGroup);
  end;
  //AValue.dwTagId: Ignore dwTagId.
  if AValue.lpDependencies <> nil then begin
    //We need a string-typed copy but simply assining will copy until first #00
    Self.FDependencies := CopyNullSeparatedList(AValue.lpDependencies);
    Self.FConfig.lpDependencies := PWideChar(Self.FDependencies);
  end;
  if AValue.lpServiceStartName <> nil then begin
    Self.FServiceStartName := AValue.lpServiceStartName;
    Self.FConfig.lpServiceStartName := PWideChar(Self.FServiceStartName);
  end;
  //APassword: Ignore the password.
  if AValue.lpDisplayName <> nil then begin
    Self.FDisplayName := AValue.lpDisplayName;
    Self.FConfig.lpDisplayName := PWideChar(Self.FDisplayName);
  end;
end;

function TMemServiceEntry.GetRawDescription: string;
begin
  Result := FDescription;
end;

procedure TMemServiceEntry.SetRawDescription(const AValue: string);
begin
  FDescription := AValue;
end;

function TMemServiceEntry.GetDelayedAutostart: boolean;
begin
  Result := Self.FDelayedAutostart;
end;

procedure TMemServiceEntry.SetDelayedAutostart(const AValue: boolean);
begin
  Self.FDelayedAutostart := AValue;
end;

function TMemServiceEntry.GetImageInformation: TServiceImageInformation;
begin
  Result := Self.FImageInformation;
end;

procedure TMemServiceEntry.SetImageInformation(const AValue: TServiceImageInformation);
begin
  Self.FImageInformation := AValue;
end;

function TMemServiceEntry.GetSidType: dword;
begin
  Result := Self.FSidType;
end;

procedure TMemServiceEntry.SetSidType(const AValue: dword);
begin
  Self.FSidType := AValue;
end;

function TMemServiceEntry.GetRequiredPrivileges: TArray<string>;
begin
  Result := Self.FRequiredPrivileges;
end;

procedure TMemServiceEntry.SetRequiredPrivileges(const AValue: TArray<string>);
begin
  Self.FRequiredPrivileges := AValue;
end;

function TMemServiceEntry.GetLaunchProtection: cardinal;
begin
  Result := Self.FLaunchProtection;
end;

procedure TMemServiceEntry.SetLaunchProtection(const AValue: cardinal);
begin
  Self.FLaunchProtection := AValue;
end;

function TMemServiceEntry.GetTriggers: PSERVICE_TRIGGER_INFO;
begin
  Result := Self.FTriggers;
  if Result = nil then
    Result := @Self.FEmptyTriggers; //be nice
end;

procedure TMemServiceEntry.SetTriggers(const ANewTriggers: PSERVICE_TRIGGER_INFO);
begin
  FreeTriggers;
  if ANewTriggers = nil then
    exit; //already cleared our copy!
  Self.FTriggers := CopyServiceTriggers(ANewTriggers);
end;

procedure TMemServiceEntry.FreeTriggers;
begin
  if FTriggers <> nil then begin
    FreeMem(FTriggers);
    FTriggers := nil;
  end;
end;

function TMemServiceEntry.GetFailureActions: LPSERVICE_FAILURE_ACTIONS;
begin

end;

procedure TMemServiceEntry.SetFailureActions(const AValue: LPSERVICE_FAILURE_ACTIONS);
begin

end;

function TMemServiceEntry.GetFailureActionsOnNonCrashFailures: boolean;
begin
  Result := Self.FFailureActionsOnNonCrashFailures;
end;

procedure TMemServiceEntry.SetFailureActionsOnNonCrashFailures(const AValue: boolean);
begin
  Self.FFailureActionsOnNonCrashFailures := AValue;
end;

function TMemServiceEntry.GetPreshutdownTimeout: dword;
begin
  Result := Self.FPreshutdownTimeout;
end;

procedure TMemServiceEntry.SetPreshutdownTimeout(const AValue: dword);
begin
  Self.FPreshutdownTimeout := AValue;
end;

function TMemServiceEntry.GetPreferredNode: integer;
begin
  Result := Self.FPreferredNode;
end;

procedure TMemServiceEntry.SetPreferredNodeInfo(const ANode: integer);
begin
  Self.FPreferredNode := ANode;
end;

end.
