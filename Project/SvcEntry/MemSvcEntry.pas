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
    function GetRawDescription: string; override;
    procedure SetRawDescription(const AValue: string); override;
    procedure SetCBinaryPathName(const AValue: string); inline;
    procedure SetCLoadOrderGroup(const AValue: string); inline;
    procedure SetCDependencies(const AValue: string); inline;
    procedure SetCServiceStartName(const AValue: string); inline;
    procedure SetCDisplayName(const AValue: string); inline;
  public
    //These properly set the stored strings, updating the Config structure
    property CBinaryPathName: string write SetCBinaryPathName;
    property CLoadOrderGroup: string write SetCLoadOrderGroup;
    property CDependencies: string write SetCDependencies;
    property CServiceStartName: string write SetCServiceStartName;
    property CDisplayName: string write SetCDisplayName;
    procedure AddCDependency(const ADependency: string);
    procedure SetConfig(const AValue: QUERY_SERVICE_CONFIG; const APassword: PChar); override;

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
  public
    function GetLaunchProtection: cardinal; override;
    procedure SetLaunchProtection(const AValue: cardinal); override;

  protected
    class var FEmptyTriggers: SERVICE_TRIGGER_INFO;
  protected
    FTriggers: PSERVICE_TRIGGER_INFO;
    function GetTriggers: PSERVICE_TRIGGER_INFO; override;
    procedure FreeTriggers;
  public
    procedure SetTriggers(const ANewTriggers: PSERVICE_TRIGGER_INFO); override;

  protected
    FFailureActions: packed record
      //LPSERVICE_FAILURE_ACTION always points to a block of memory like this:
      FHeader: SERVICE_FAILURE_ACTIONS;
      FActions: array[0..2] of SC_ACTION;
      //These are just data for FHeader's pointers:
      FRebootMsg: string;
      FCommand: string;
    end;
    FFailureActionsOnNonCrashFailures: boolean;
    FPreshutdownTimeout: dword;
    FPreferredNode: integer;
  public
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
uses WinApiHelper, ServiceHelper;

constructor TMemServiceEntry.Create;
begin
  inherited Create;
  FillChar(Self.FConfig, 0, SizeOf(Self.FConfig));
  //Each instance will override this class var but whatever:
  FEmptyTriggers.cTriggers := 0;
  FEmptyTriggers.pTriggers := nil;
  FPreferredNode := PREFERRED_NODE_DISABLED;
  FillChar(Self.FFailureActions, 0, SizeOf(Self.FFailureActions));
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

procedure TMemServiceEntry.SetConfig(const AValue: QUERY_SERVICE_CONFIG; const APassword: PChar);
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
  if AValue.lpBinaryPathName <> nil then
    Self.CBinaryPathName := AValue.lpBinaryPathName;
  if AValue.lpLoadOrderGroup <> nil then
    Self.CLoadOrderGroup := AValue.lpLoadOrderGroup;
  //AValue.dwTagId: Ignore dwTagId.
  if AValue.lpDependencies <> nil then begin
    //We need a string-typed copy but simply assining will copy until first #00
    Self.CDependencies :=  CopyNullSeparatedList(AValue.lpDependencies);
  end;
  if AValue.lpServiceStartName <> nil then
    Self.CDisplayName := AValue.lpServiceStartName;
  //APassword: Ignore the password.
  if AValue.lpDisplayName <> nil then
    Self.CDisplayName := AValue.lpDisplayName;
end;

function TMemServiceEntry.GetRawDescription: string;
begin
  Result := FDescription;
end;

procedure TMemServiceEntry.SetRawDescription(const AValue: string);
begin
  FDescription := AValue;
end;

{ The following functions set the member fields of AConfig while storing the
 data locally }

procedure TMemServiceEntry.SetCBinaryPathName(const AValue: string);
begin
  Self.FBinaryPathName := AValue;
  Self.FConfig.lpBinaryPathName := PWideChar(Self.FBinaryPathName);
end;

procedure TMemServiceEntry.SetCLoadOrderGroup(const AValue: string);
begin
  Self.FLoadOrderGroup := AValue;
  Self.FConfig.lpLoadOrderGroup := PWideChar(Self.FLoadOrderGroup);
end;

procedure TMemServiceEntry.SetCDependencies(const AValue: string);
begin
  Self.FDependencies := AValue;
  Self.FConfig.lpDependencies := PWideChar(Self.FDependencies);
end;

procedure TMemServiceEntry.SetCServiceStartName(const AValue: string);
begin
  Self.FServiceStartName := AValue;
  Self.FConfig.lpServiceStartName := PWideChar(Self.FServiceStartName);
end;

procedure TMemServiceEntry.SetCDisplayName(const AValue: string);
begin
  Self.FDisplayName := AValue;
  Self.FConfig.lpDisplayName := PWideChar(Self.FDisplayName);
end;

//Adds a dependency by storing it in the CDependency null-list
procedure TMemServiceEntry.AddCDependency(const ADependency: string);
var AList: TArray<string>;
begin
  //The slow and reusing way
  AList := SplitNullSeparatedList(PChar(Self.FDependencies));
  SetLength(AList, Length(AList)+1);
  AList[Length(AList)-1] := ADependency;
  SetCDependencies(JoinNullSeparatedList(AList));
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
  Result := @Self.FFailureActions.FHeader;
end;

procedure TMemServiceEntry.SetFailureActions(const AValue: LPSERVICE_FAILURE_ACTIONS);
var i: integer;
begin
  Self.FFailureActions.FHeader := AValue^;
  for i := 0 to Length(Self.FFailureActions.FActions)-1 do begin
    //Even though cAction can indicate any number of actions, in reality there can be at most 3.
    //Write the available ones and zero out the rest
    if (i < integer(AValue.cActions)) and (AValue.lpsaActions <> nil) then
      Self.FFailureActions.FActions[i] := AValue.lpsaActions[i]
    else
      FillChar(Self.FFailureActions.FActions[i], 0, SizeOf(SC_ACTION));
  end;
  Self.FFailureActions.FRebootMsg := AValue.lpRebootMsg;
  Self.FFailureActions.FHeader.lpRebootMsg := PWideChar(Self.FFailureActions.FRebootMsg);
  Self.FFailureActions.FCommand := AValue.lpCommand;
  Self.FFailureActions.FHeader.lpCommand := PWideChar(Self.FFailureActions.FCommand);
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
