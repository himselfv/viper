unit Viper.ScheduledTasksMain;
{
Inherits from common ScheduledTasksForm to add application-wide functionality.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Actions, ActnList, StdCtrls, ExtCtrls, VirtualTrees,
  CommonResources, Viper.ScheduledTasks;

type
  TScheduledTasksMainForm = class(TScheduledTasksForm)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reload; override;
  protected
    procedure UMClear(var Message: TMessage); message UM_CLEAR;
    procedure UMRefresh(var Message: TMessage); message UM_REFRESH;
    procedure UMQuickFilterChanged(var Message: TMessage); message UM_QUICKFILTER_CHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure ViewOnActivate;
    procedure ViewOnDeactivate;
    procedure FilterTasks;
    procedure FilterTasks_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
  end;

var
  ScheduledTasksMainForm: TScheduledTasksMainForm;


implementation
uses Winapi.WinSvc, Viper.MainForm;

{$R *.dfm}

constructor TScheduledTasksMainForm.Create(AOwner: TComponent);
var mi: TMenuItem;
begin
  inherited;
  MainForm.RegisterView(Self);

  mi := TMenuItem.Create(nil);
  mi.Caption := '-';
  MainForm.miTools.Add(mi);

  mi := TMenuItem.Create(nil);
  mi.Action := aOpenSchedulerRegistry;
  MainForm.miTools.Add(mi);

  mi := TMenuItem.Create(nil);
  mi.Action := aOpenSchedulerFolder;
  MainForm.miTools.Add(mi);

  mi := TMenuItem.Create(nil);
  mi.Action := aOpenSchedulerMMC;
  MainForm.miTools.Add(mi);
end;

destructor TScheduledTasksMainForm.Destroy;
begin
  MainForm.UnregisterView(Self);
  inherited;
end;

procedure TScheduledTasksMainForm.CMShowingChanged(var Message: TMessage);
begin
  if Self.Showing then
    Self.ViewOnActivate
  else
    Self.ViewOnDeactivate;
  inherited;
end;

procedure TScheduledTasksMainForm.ViewOnActivate;
begin
  //Self.Reload;
  //MainForm.SetDetailsPaneFocusedService(Self.GetFocusedService);
end;

procedure TScheduledTasksMainForm.ViewOnDeactivate;
begin
end;

procedure TScheduledTasksMainForm.UMClear(var Message: TMessage);
begin
  Self.Clear;
end;

procedure TScheduledTasksMainForm.UMRefresh(var Message: TMessage);
begin
  if not Self.Visible then exit;
  Self.Reload;
  Self.FilterTasks;
end;

procedure TScheduledTasksMainForm.UMQuickFilterChanged(var Message: TMessage);
begin
  Self.FilterTasks;
end;

procedure TScheduledTasksMainForm.Reload;
begin
  vtTasks.BeginUpdate;
  try
    inherited;
    Self.FilterTasks();
  finally
    vtTasks.EndUpdate;
  end;
end;

procedure TScheduledTasksMainForm.FilterTasks();
begin
  //If it's invisible it's going to be filtered when it's reloaded
  if not Self.Visible then exit;
  Self.ApplyFilter(FilterTasks_Callback, nil);
end;

procedure TScheduledTasksMainForm.FilterTasks_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var task: PNdTaskData;
  isVisible: boolean;
  filterText: string;
begin
  task := Sender.GetNodeData(Node);
  isVisible := true;

  //Quickfilter
  filterText := AnsiLowerCase(string(MainForm.QuickFilter).Trim());
  if filterText <> '' then
    if not AnsiLowerCase(task.Name).Contains(filterText)
    and not AnsiLowerCase(task.Path).Contains(filterText)
    and not AnsiLowerCase(task.Source).Contains(filterText)
    and not AnsiLowerCase(task.GUID).Contains(filterText)
    and not AnsiLowerCase(task.Buckets).Contains(filterText) then
      isVisible := false;

  Sender.IsVisible[Node] := isVisible;
end;


end.
