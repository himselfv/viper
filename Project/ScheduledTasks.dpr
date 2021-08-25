program ScheduledTasks;

uses
  Vcl.Forms,
  ScheduledTasks.MainForm in '..\Forms\ScheduledTasks.MainForm.pas' {ScheduledTasksMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TScheduledTasksMainForm, ScheduledTasksMainForm);
  Application.Run;
end.
