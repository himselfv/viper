program Viper;

uses
  Vcl.Forms,
  Viper_MainForm in '..\Forms\Viper_MainForm.pas' {MainForm},
  CommonResources in 'CommonResources.pas' {CommonRes: TDataModule},
  Viper_ServiceList in '..\Forms\Viper_ServiceList.pas' {ServiceList: TFrame},
  Viper_TriggerList in '..\Forms\Viper_TriggerList.pas' {TriggerList: TFrame},
  Viper_RestoreServiceConfig in '..\Forms\Viper_RestoreServiceConfig.pas' {RestoreServiceConfigForm},
  Viper_MainTriggerList in '..\Forms\Viper_MainTriggerList.pas' {MainTriggerList},
  Viper.RichEditEx in '..\Forms\Viper.RichEditEx.pas' {RichEditFrame: TFrame},
  Viper.Log in 'Viper.Log.pas' {LogForm},
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
  Application.CreateForm(TRestoreServiceConfigForm, RestoreServiceConfigForm);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TMainTriggerList, MainTriggerList);
  Application.Run;
end.
