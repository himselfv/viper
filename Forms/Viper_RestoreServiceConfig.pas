unit Viper_RestoreServiceConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  SvcEntry;

{
Saves the startup configuration of all services, or selected services, to a file so that it can
later be restored:
* to roll back any changes that were made
* to deploy the configuration to a target PC

Restores the configuration.
}

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
    function RestoreServiceConfig: string;
  public
    function OpenRestore(const AFilename: string): TModalResult;
  end;

var
  RestoreServiceConfigForm: TRestoreServiceConfigForm;

function WriteServiceConfigFile(AServices: TServiceEntries; const AFilename: string): string;

implementation
uses WinSvc, ServiceHelper;

{$R *.dfm}

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
    raise Exception.Create('Invalid configuration line: '+ALine);

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
    raise EConvertError.Create(AString + ' is not a valid startup type.');
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

procedure TRestoreServiceConfigForm.btnOKClick(Sender: TObject);
var AFailedRestore: string;
begin
  AFailedRestore := Self.RestoreServiceConfig();
  if AFailedRestore <> '' then begin
    MessageBox(Self.Handle,
      PChar('Could not restore the startup configuration for the following services:'#13
        +AFailedRestore+#13
        +'Perhaps you''re not running the application with Administrator rights?'),
      PChar('Some problems encountered'),
      MB_OK + MB_ICONERROR);
  end else
    MessageBox(Self.Handle,
      PChar('Startup configuration restored successfully.'),
      PChar('Success'),
      MB_OK + MB_ICONINFORMATION);
  ModalResult := mrOK;
end;

procedure TRestoreServiceConfigForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

//Internal restore operation, the counterpart to WriteServiceConfigFile. Uses the preloaded FEntries
//list so that we don't re-read the file; it could've changed.
function TRestoreServiceConfigForm.RestoreServiceConfig: string;
var AService, AStartTypeStr: string;
  AStartType: cardinal;
  i: integer;
begin
  Result := '';
  for i := 0 to FEntries.Count-1 do begin
    if not ParseConfigurationLine(FEntries[i], AService, AStartTypeStr) then
      continue;
    AStartType := StringToStartType(AStartTypeStr);
    try
      ChangeServiceStartType(AService, AStartType);
    except
      on EOsError do begin
        Result := Result + ',' + AService;
        continue;
      end;
    end;
  end;

  if Result <> '' then
    delete(Result, 1, 1); //remove initial ","
end;



end.
