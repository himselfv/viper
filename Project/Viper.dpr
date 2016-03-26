program Viper;

uses
  Vcl.Forms,
  Viper_MainForm in '..\Forms\Viper_MainForm.pas' {MainForm},
  ServiceHelper in 'ServiceHelper.pas',
  WinApiHelper in 'WinApiHelper.pas',
  ShellUtils in 'ShellUtils.pas',
  Viper_ServiceList in '..\Forms\Viper_ServiceList.pas' {ServiceList: TFrame},
  SvcEntry in 'SvcEntry.pas',
  Viper_RestoreServiceConfig in '..\Forms\Viper_RestoreServiceConfig.pas' {RestoreServiceConfigForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TRestoreServiceConfigForm, RestoreServiceConfigForm);
  Application.Run;
end.
