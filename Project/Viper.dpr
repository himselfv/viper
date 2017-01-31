program Viper;

uses
  Vcl.Forms,
  Viper.MainForm in '..\Forms\Viper.MainForm.pas' {MainForm},
  Viper.Log in 'Viper.Log.pas' {LogForm},
  CommonResources in 'CommonResources.pas' {CommonRes: TDataModule},
  Viper.Settings in '..\Forms\Viper.Settings.pas' {SettingsForm},
  Viper.StyleSettings in '..\Forms\Viper.StyleSettings.pas' {StyleSettingsForm},
  Viper.StyleEdit in '..\Forms\Viper.StyleEdit.pas' {StyleEditForm},
  Viper.ServiceList in '..\Forms\Viper.ServiceList.pas' {ServiceList: TFrame},
  Viper.RestoreServiceConfig in '..\Forms\Viper.RestoreServiceConfig.pas' {RestoreServiceConfigForm},
  Viper.TriggerList in '..\Forms\Viper.TriggerList.pas' {TriggerList: TFrame},
  Viper.TriggerBrowser in '..\Forms\Viper.TriggerBrowser.pas' {TriggerBrowserForm},
  Viper.RichEditEx in '..\Forms\Viper.RichEditEx.pas' {RichEditFrame: TFrame},
  ServiceHelper in 'ServiceHelper.pas',
  WinApiHelper in 'WinApiHelper.pas',
  ShellUtils in 'ShellUtils.pas',
  SvcEntry in 'SvcEntry.pas',
  SetupApiHelper in 'SetupApiHelper.pas',
  AclUi in 'AclUi.pas',
  SecEdit in 'SecEdit.pas',
  TriggerUtils in 'TriggerUtils.pas',
  GuidDict in 'GuidDict.pas',
  EtwUtils in 'EtwUtils.pas',
  SvcCat in 'SvcCat.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCommonRes, CommonRes);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TStyleSettingsForm, StyleSettingsForm);
  Application.CreateForm(TTriggerBrowserForm, TriggerBrowserForm);
  Application.Run;
end.
