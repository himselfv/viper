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
  Viper.DependencyList in '..\Forms\Viper.DependencyList.pas' {DependencyList: TFrame},
  Viper.RestoreServiceConfig in '..\Forms\Viper.RestoreServiceConfig.pas' {RestoreServiceConfigForm},
  Viper.TriggerList in '..\Forms\Viper.TriggerList.pas' {TriggerList: TFrame},
  Viper.ServiceTriggerList in '..\Forms\Viper.ServiceTriggerList.pas' {ServiceTriggerList: TFrame},
  Viper.MainTriggerList in '..\Forms\Viper.MainTriggerList.pas' {MainTriggerList: TFrame},
  Viper.RichEditEx in '..\Forms\Viper.RichEditEx.pas' {RichEditFrame: TFrame},
  Viper.TriggerEditor in '..\Forms\Viper.TriggerEditor.pas' {TriggerEditorForm},
  Viper.TriggerDataItemEditor in '..\Forms\Viper.TriggerDataItemEditor.pas' {TriggerDataItemEditor},
  Viper.TriggerImport in '..\Forms\Viper.TriggerImport.pas' {TriggerImportForm},
  Viper.TriggerImportList in '..\Forms\Viper.TriggerImportList.pas' {TriggerImportList: TFrame},
  Viper.ServiceImport in 'Viper.ServiceImport.pas' {ServiceImportForm},
  Viper.ServiceEdit in '..\Forms\Viper.ServiceEdit.pas' {ServiceEditForm},
  Viper.ScheduledTasks in '..\Forms\Viper.ScheduledTasks.pas' {ScheduledTasksForm},
  Viper.ScheduledTasksMain in '..\Forms\Viper.ScheduledTasksMain.pas' {ScheduledTasksMainForm},
  ServiceHelper in 'ServiceHelper.pas',
  WinApiHelper in 'WinApiHelper.pas',
  ShellUtils in 'ShellUtils.pas',
  SvcEntry in 'SvcEntry\SvcEntry.pas',
  OsSvcEntry in 'SvcEntry\OsSvcEntry.pas',
  MemSvcEntry in 'SvcEntry\MemSvcEntry.pas',
  RegFileSvcEntry in 'SvcEntry\RegFileSvcEntry.pas',
  SetupApiHelper in 'SetupApiHelper.pas',
  AclUi in 'AclUi.pas',
  SecEdit in 'SecEdit.pas',
  TriggerUtils in 'TriggerUtils.pas',
  GuidDict in 'GuidDict.pas',
  EtwUtils in 'EtwUtils.pas',
  SvcCat in 'SvcCat.pas',
  TriggerExport in 'TriggerExport.pas',
  RegFile in 'RegFile.pas',
  RegExport in 'RegExport.pas',
  WnfUtils in 'WnfUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TCommonRes, CommonRes);
  Application.CreateForm(TLogForm, LogForm);
  Application.CreateForm(TSettingsForm, SettingsForm);
  Application.CreateForm(TStyleSettingsForm, StyleSettingsForm);
  Application.CreateForm(TMainTriggerList, TriggerBrowser);
  Application.CreateForm(TScheduledTasksMainForm, ScheduledTasksMainForm);
  Application.Run;
end.
