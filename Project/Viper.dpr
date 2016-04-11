program Viper;

uses
  Vcl.Forms,
  Viper_MainForm in '..\Forms\Viper_MainForm.pas' {MainForm},
  ServiceHelper in 'ServiceHelper.pas',
  WinApiHelper in 'WinApiHelper.pas',
  ShellUtils in 'ShellUtils.pas',
  SvcEntry in 'SvcEntry.pas',
  Viper_ServiceList in '..\Forms\Viper_ServiceList.pas' {ServiceList: TFrame},
  Viper_TriggerList in '..\Forms\Viper_TriggerList.pas' {TriggerList: TFrame},
  Viper_RestoreServiceConfig in '..\Forms\Viper_RestoreServiceConfig.pas' {RestoreServiceConfigForm},
  SetupApiHelper in 'SetupApiHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TRestoreServiceConfigForm, RestoreServiceConfigForm);
  Application.Run;
end.
