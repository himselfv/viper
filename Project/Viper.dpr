program Viper;

uses
  Vcl.Forms,
  Viper_MainForm in '..\Forms\Viper_MainForm.pas' {MainForm},
  ServiceHelper in 'ServiceHelper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
