unit Viper.Settings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IniFiles;

type
  TSettingsForm = class(TForm)
  public
    procedure LoadSettings;
    procedure SaveSettings;
  end;

var
  SettingsForm: TSettingsForm;

function GetSettings: TCustomIniFile;

implementation
uses Registry;

{$R *.dfm}

function GetSettings: TCustomIniFile;
begin
  Result := TRegistryIniFile.Create('Software\Viper');
end;

procedure TSettingsForm.LoadSettings;
begin
//Nothing atm
end;

procedure TSettingsForm.SaveSettings;
begin
//Nothing atm
end;

end.
