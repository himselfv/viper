unit Viper.RestoreServiceConfig;
{
Saves the startup configuration of all services, or selected services, to a file so that it can
later be restored:
* to roll back any changes that were made
* to deploy the configuration to a target PC

Restores the configuration.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  SvcEntry;

type
  TRestoreServiceConfigForm = class(TForm)
    Label1: TLabel;
    lbList: TListBox;
    Label2: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    FFilename: string;
    FEntries: TStringList;
    procedure RestoreServiceConfig(out ANotFound: string; out AFailed: string);
  public
    function OpenRestore(const AFilename: string): TModalResult;
  end;

//Just writes the file
function WriteServiceConfigFile(AServices: TServiceEntries; const AFilename: string): string;

//Has minimal ui
procedure SaveServiceConfig(AParent: HWND; AServices: TServiceEntries; const AFilename: string);

implementation
uses WinSvc, ServiceHelper;

{$R *.dfm}

resourcestring
  eInvalidConfigurationLine = 'Invalid configuration line: %s';
  eInvalidStartupType = '%s is not a valid startup type.';

//Returns false if it's a valid comment line or empty line
function ParseConfigurationLine(ALine: string; out AService: string; out AStartType: string): boolean;
var i_pos: integer;
begin
 //Support some basic comments
  i_pos := pos('#', ALine);
  if i_pos > 0 then
    delete(ALine, i_pos, MaxInt);

  ALine := Trim(ALine);
  if ALine = '' then begin
    Result := false;
    exit;
  end;

  i_pos := pos('=', ALine);
  if i_pos < 0 then
    raise Exception.CreateFmt(eInvalidConfigurationLine, [ALine]);

  AService := copy(ALine, 1, i_pos-1);
  AStartType := copy(ALine, i_pos+1, MaxInt);
  Result := true;
end;

function StartTypeToString(const AStartType: cardinal): string;
begin
  case AStartType of
    SERVICE_BOOT_START: Result := 'boot';
    SERVICE_SYSTEM_START: Result := 'system';
    SERVICE_AUTO_START: Result := 'auto';
    SERVICE_DEMAND_START: Result := 'manual';
    SERVICE_DISABLED: Result := 'disabled';
  else Result := IntToStr(AStartType);
  end;
end;

function TryStringToStartType(const AString: string; out AStartType: cardinal): boolean;
var tmp: integer;
begin
  Result := true;
  if TryStrToInt(AString, tmp) then begin
    AStartType := cardinal(tmp);
    exit;
  end;

  if SameText(AString, 'boot') then
    AStartType := SERVICE_BOOT_START
  else
  if SameText(AString, 'system') then
    AStartType := SERVICE_SYSTEM_START
  else
  if SameText(AString, 'auto') then
    AStartType := SERVICE_AUTO_START
  else
  if SameText(AString, 'manual')
  or SameText(AString, 'demand') then
    AStartType := SERVICE_DEMAND_START
  else
  if SameText(AString, 'disabled') then
    AStartType := SERVICE_DISABLED
  else
    Result := false
end;

function StringToStartType(const AString: string): cardinal;
begin
  if not TryStringToStartType(AString, Result) then
    raise EConvertError.CreateFmt(eInvalidStartupType, [AString]);
end;

//Returns the list of services for which we couldn't query the startup configuration
function WriteServiceConfigFile(AServices: TServiceEntries; const AFilename: string): string;
var AEntries: TStringList;
  i: integer;
begin
  Result := '';
  AEntries := TStringList.Create;
  try
    for i := 0 to Length(AServices)-1 do begin
      if AServices[i].Config = nil then begin
        Result := Result + ','+AServices[i].ServiceName;
        continue;
      end;

      AEntries.Add(AServices[i].ServiceName + '=' + StartTypeToString(AServices[i].Config.dwStartType));
    end;

    AEntries.SaveToFile(AFilename);
  finally
    FreeAndNil(AEntries);
  end;

  if Result <> '' then
    delete(Result, 1, 1); //remove initial ","
end;

resourcestring
  sSuccess = 'Success';
  sHadProblems = 'Some problems encountered';
  sConfigurationSaveFailed =
    'We didn''t have the rights to query the configuration for the following services:'#13
    +'%s'#13
    +'Perhaps you''re not running the application with Administrator rights?';

procedure SaveServiceConfig(AParent: HWND; AServices: TServiceEntries; const AFilename: string);
var AFailedServices: string;
begin
  AFailedServices := Viper.RestoreServiceConfig.WriteServiceConfigFile(AServices, AFilename);
  if AFailedServices <> '' then
    MessageBox(AParent,
      PChar(Format(sConfigurationSaveFailed, [AFailedServices])),
      PChar(sHadProblems),
      MB_OK + MB_ICONERROR);
end;


{
A form to verify the list of services before proceeding with restore operation.
Not very helpful atm but can be expanded.
}

function TRestoreServiceConfigForm.OpenRestore(const AFilename: string): TModalResult;
begin
  Self.FFilename := AFilename;
  Result := Self.ShowModal;
end;

procedure TRestoreServiceConfigForm.FormShow(Sender: TObject);
var i: integer;
  AService, AStartType: string;
begin
  lbList.Clear;
  if FEntries <> nil then
    FEntries.Clear;
  if Self.FFilename = '' then exit;
  if FEntries = nil then
    FEntries := TStringList.Create;

  FEntries.LoadFromFile(Self.FFilename);
  for i := 0 to FEntries.Count-1 do begin
    if not ParseConfigurationLine(FEntries[i], AService, AStartType) then
      continue;
    lbList.Items.Add(AService + ': ' + AStartType)
  end;
end;

procedure TRestoreServiceConfigForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEntries);
end;

resourcestring
  sConfigurationRestored = 'Startup configuration restored successfully.';
  sConfigurationRestoreFailed =
    'Could not restore the startup configuration for the following services: %s'#13
    +'Perhaps you''re not running the application with Administrator rights?';
  sServicesNotFound = 'Not found: %s';
  sServicesFailed = 'Failed: %s';

procedure TRestoreServiceConfigForm.btnOKClick(Sender: TObject);
var ANotFound, AFailed: string;
begin
  Self.RestoreServiceConfig(ANotFound, AFailed);
  if (ANotFound <> '') or (AFailed <> '') then begin
    if AFailed <> '' then
      AFailed := #13 + Format(sServicesFailed, [AFailed]);
    if ANotFound <> '' then
      AFailed := #13 + Format(sServicesNotFound, [ANotFound]) + AFailed;
    MessageBox(Self.Handle,
      PChar(Format(sConfigurationRestoreFailed, [AFailed])),
      PChar(sHadProblems),
      MB_OK + MB_ICONERROR);
  end else
    MessageBox(Self.Handle, PChar(sConfigurationRestored), PChar(sSuccess),
      MB_OK + MB_ICONINFORMATION);
  ModalResult := mrOK;
end;

procedure TRestoreServiceConfigForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//Internal restore operation, the counterpart to WriteServiceConfigFile. Uses the preloaded FEntries
//list so that we don't re-read the file; it could've changed.
procedure TRestoreServiceConfigForm.RestoreServiceConfig(out ANotFound: string; out AFailed: string);
var AService, AStartTypeStr: string;
  AStartType: cardinal;
  hSC: SC_HANDLE;
  AConfig: LPQUERY_SERVICE_CONFIG;
  i: integer;
begin
  ANotFound := '';
  AFailed := '';

  hSC := OpenSCManager(SC_MANAGER_ALL_ACCESS);
  try

    for i := 0 to FEntries.Count-1 do begin
      if not ParseConfigurationLine(FEntries[i], AService, AStartTypeStr) then
        continue;
      AStartType := StringToStartType(AStartTypeStr);
      try
        //Do not bother if it already matches. This way we ignore the services which we can't change, but don't need to.
        AConfig := QueryServiceConfig(hSC, AService);
        //okay to throw, will catch below
        try
          if AConfig.dwStartType = AStartType then
            continue;
        finally
          FreeMem(AConfig);
        end;

        ChangeServiceStartType(hSC, AService, AStartType);
      except
        on E: EOsError do begin
          if E.ErrorCode = ERROR_SERVICE_DOES_NOT_EXIST then
            ANotFound := ANotFound + ',' + AService
          else
            AFailed := AFailed + ',' + AService;
          continue;
        end;
      end;
    end;

  finally
    CloseServiceHandle(hSC);
  end;

  if ANotFound <> '' then
    delete(ANotFound, 1, 1); //remove initial ","
  if AFailed <> '' then
    delete(AFailed, 1, 1); //remove initial ","
end;



end.
