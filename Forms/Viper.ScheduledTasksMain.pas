unit Viper.ScheduledTasksMain;
{
Inherits from common ScheduledTasksForm to add application-wide functionality.
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Viper.ScheduledTasks, Viper.MainForm;

type
  TScheduledTasksMainForm = class(TScheduledTasksForm)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure UMClear(var Message: TMessage); message UM_CLEAR;
    procedure UMRefresh(var Message: TMessage); message UM_REFRESH;
    procedure UMQuickFilterChanged(var Message: TMessage); message UM_QUICKFILTER_CHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure ViewOnActivate;
    procedure ViewOnDeactivate;
  end;

var
  ScheduledTasksMainForm: TScheduledTasksMainForm;


implementation
uses Menus, Winapi.WinSvc;

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
//  Self.FilterTriggers;
end;

procedure TScheduledTasksMainForm.UMQuickFilterChanged(var Message: TMessage);
begin
//  Self.FilterTriggers;
end;

{
procedure TScheduledTasksMainForm.FilterTriggers();
begin
  //If it's invisible it's going to be filtered when it's reloaded
  if not Self.Visible then exit;
  Self.ApplyFilter(FilterTriggers_Callback, nil);
end;

procedure TScheduledTasksMainForm.FilterTriggers_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var triggerData: PNdTriggerFacet;
  svc: TExtServiceEntry;
  isService: boolean;
  isVisible: boolean;
  filterText: string;
begin
  triggerData := Sender.GetNodeData(Node);

  svc := TExtServiceEntry(MainForm.Services.Find(triggerData.Trigger.ServiceName));
  if svc = nil then begin
    //Maybe someone created a service while we weren't reloading? Show for now.
    Sender.IsVisible[Node] := true;
    exit;
  end;

  isService := (svc.Status.dwServiceType and SERVICE_WIN32 <> 0);
  isVisible := true;

  //Filter out drivers if disabled
  if not MainForm.aShowDrivers.Checked and not isService then
    isVisible := false;

  //Quickfilter
  filterText := AnsiLowerCase(string(MainForm.QuickFilter).Trim());
  if filterText <> '' then
    if not AnsiLowerCase(svc.ServiceName).Contains(filterText)
    and not AnsiLowerCase(svc.DisplayName).Contains(filterText)
    and not AnsiLowerCase(triggerData.Description).Contains(filterText)
    and not AnsiLowerCase(triggerData.Params).Contains(filterText)
    and not AnsiLowerCase(TriggerActionToString(triggerData.Action)).Contains(filterText) then
      isVisible := false;

  Sender.IsVisible[Node] := isVisible;
end;
}


end.
