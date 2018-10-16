unit Viper.Settings;
{
To store anything in the settings:
1. Register to OnLoadSettings/OnSaveSettings and read/write all the settings
  that you wish to store.
2. Call GetSettings() and read/write particular settings that you care about
  when only those need to be changed/reloaded.

You should have something like this:
  DoSaveLocalSettings(ini) //register as callback
  SaveSettings() //ini := GetSettings; DoSaveLocalSettings(ini); FreeAndNil(ini);
}
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IniFiles,
  Generics.Collections;

type
  TSettingsEvent = procedure(Sender: TObject; Ini: TCustomIniFile) of object;

  TSettingsForm = class(TForm)
    cbPortableMode: TCheckBox;
    lblPortableModeDesc: TLabel;
    btnCancel: TButton;
    btnOk: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  protected
    FOnLoadSettings: TList<TSettingsEvent>;
    FOnSaveSettings: TList<TSettingsEvent>;
    procedure DoLoadLocalSettings(Sender: TObject; Ini: TCustomIniFile);
    procedure DoSaveLocalSettings(Sender: TObject; Ini: TCustomIniFile);
    procedure EnablePortableMode;
    procedure DisablePortableMode;
  public
    procedure LoadSettings;
    procedure SaveSettings;
    //Register to handle mass loading and saving of settings
    property OnLoadSettings: TList<TSettingsEvent> read FOnLoadSettings;
    property OnSaveSettings: TList<TSettingsEvent> read FOnSaveSettings;
  end;

var
  SettingsForm: TSettingsForm;

function GetSettings: TCustomIniFile;

implementation
uses Registry, FilenameUtils;

{$R *.dfm}

function GetPortableIniFilename: string;
begin
  Result := AppFolder + '\' + ChangeFileExt(AppFilename, '.ini');
end;

function HavePortableSettings: boolean;
begin
  Result := FileExists(GetPortableIniFilename);
end;

function GetPortableSettings: TCustomIniFile;
begin
  Result := TIniFile.Create(GetPortableIniFilename);
end;

function GetSettings: TCustomIniFile;
begin
  //Use the portable settings if present
  //We can't just "try to open" them as TIniSettings will create a new one if needed
  if HavePortableSettings() then
    Result := GetPortableSettings()
  else
    Result := TRegistryIniFile.Create('Software\Viper');
end;

procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  FOnLoadSettings := TList<TSettingsEvent>.Create;
  FOnSaveSettings := TList<TSettingsEvent>.Create;
  FOnLoadSettings.Add(Self.DoLoadLocalSettings);
  FOnSaveSettings.Add(Self.DoSaveLocalSettings);
end;

procedure TSettingsForm.FormDestroy(Sender: TObject);
begin
  FOnSaveSettings.Remove(Self.DoSaveLocalSettings);
  FOnLoadSettings.Remove(Self.DoLoadLocalSettings);
  FreeAndNil(FOnSaveSettings);
  FreeAndNil(FOnLoadSettings);
end;

procedure TSettingsForm.LoadSettings;
var ini: TCustomIniFile;
  event: TSettingsEvent;
begin
  ini := GetSettings;
  //No local settings
  //Call subscribers:
  for event in Self.FOnLoadSettings do
    event(Self, ini);
end;

procedure TSettingsForm.SaveSettings;
var ini: TCustomIniFile;
  event: TSettingsEvent;
begin
  ini := GetSettings;
  //No local settings
  //Call subscribers:
  for event in Self.FOnSaveSettings do
    event(Self, ini);
end;


procedure TSettingsForm.btnOkClick(Sender: TObject);
var ini: TCustomIniFile;
begin
  //Enable/disable portable mode before accessing the settings
  if HavePortableSettings xor cbPortableMode.Checked then
    if cbPortableMode.Checked then
      EnablePortableMode
    else begin
      DisablePortableMode;
      ModalResult := mrOk;
      exit; //since disabling it reloads all settings
    end;
  ini := GetSettings;
  try
    DoSaveLocalSettings(Self, ini);
  finally
    FreeAndNil(ini);
  end;
  ModalResult := mrOk;
end;

procedure TSettingsForm.btnCancelClick(Sender: TObject);
var ini: TCustomIniFile;
begin
  ini := GetSettings;
  try
    DoLoadLocalSettings(Self, ini);
  finally
    FreeAndNil(ini);
  end;
  ModalResult := mrCancel;
end;


// Local settings

procedure TSettingsForm.DoLoadLocalSettings(Sender: TObject; Ini: TCustomIniFile);
begin
  cbPortableMode.Checked := HavePortableSettings;

 //No other local settings at the moment
end;

procedure TSettingsForm.DoSaveLocalSettings(Sender: TObject; Ini: TCustomIniFile);
begin
 //No local settings at the moment
end;

resourcestring
  sConfirmDisablePortableCaption = 'Confirm disabling';
  sConfirmDisablePortable = 'Do you really want to disable portable mode?'#13
    +'Your portable settings will be lost and you will use this PC''s settings.';

procedure TSettingsForm.EnablePortableMode;
var ini: TCustomIniFile;
begin
  //Create a portable settings file
  ini := GetPortableSettings;
  try
    ini.WriteBool('General', 'PortableMode', true); //just to make it non-empty
    ini.UpdateFile;
  finally
    FreeAndNil(ini);
  end;
  //We will save all current settings normally
end;

procedure TSettingsForm.DisablePortableMode;
begin
  if MessageBox(Self.Handle, PChar(sConfirmDisablePortable),
    PChar(sConfirmDisablePortableCaption), MB_YESNO or MB_ICONQUESTION) <> ID_YES then
    exit;

  //Delete the portable settings file
  DeleteFile(GetPortableIniFilename);

  //Reload settings
  //Note that this WILL kill any changes made in this form. Anything else would
  //be even stranger.
  LoadSettings;
end;



end.
