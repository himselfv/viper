unit Viper.MainTriggerList;
{
Inherits from common TriggerList to add application-wide functionality.
}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Viper.TriggerList, Vcl.ExtCtrls,
  System.Actions, Vcl.ActnList, Vcl.Menus, VirtualTrees, Viper.MainForm;

type
  TMainTriggerList = class(TTriggerList)
    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
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
    procedure FilterTriggers();
    procedure FilterTriggers_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);

  protected
    function GetFocusedService: TExtServiceEntry;

  end;

var
  TriggerBrowser: TMainTriggerList;


implementation
uses Winapi.WinSvc, SvcEntry, TriggerUtils;

{$R *.dfm}

constructor TMainTriggerList.Create(AOwner: TComponent);
begin
  inherited;
  MainForm.RegisterView(Self);
end;

destructor TMainTriggerList.Destroy;
begin
  MainForm.UnregisterView(Self);
  inherited;
end;

procedure TMainTriggerList.CMShowingChanged(var Message: TMessage);
begin
  if Self.Showing then
    Self.ViewOnActivate
  else
    Self.ViewOnDeactivate;
  inherited;
end;

procedure TMainTriggerList.ViewOnActivate;
begin
  Self.Reload;
  MainForm.SetDetailsPaneFocusedService(Self.GetFocusedService);
end;

procedure TMainTriggerList.ViewOnDeactivate;
begin
end;

procedure TMainTriggerList.UMClear(var Message: TMessage);
begin
  Self.Clear;
end;

procedure TMainTriggerList.UMRefresh(var Message: TMessage);
begin
  //TriggerBrowser is dumb and currently must reload fully
  if not Self.Visible then exit;
  Self.Reload;
  Self.FilterTriggers;
end;

procedure TMainTriggerList.UMQuickFilterChanged(var Message: TMessage);
begin
  Self.FilterTriggers;
end;

procedure TMainTriggerList.FilterTriggers();
begin
  //If it's invisible it's going to be filtered when it's reloaded
  if not Self.Visible then exit;
  Self.ApplyFilter(FilterTriggers_Callback, nil);
end;

procedure TMainTriggerList.FilterTriggers_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
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



//This returns our INTERNAL TServiceEntry matching the trigger entry currently
//focused in the TriggerBrowser
//Returns Nil if a matching service cannot be found which is a valid corner
//case so handle gracefully.
function TMainTriggerList.GetFocusedService: TExtServiceEntry;
var TriggerData: PNdTriggerFacet;
begin
  TriggerData := Self.FocusedFacet;
  if TriggerData = nil then
    Result := nil
  else
    //There can be some corner cases where the trigger has already been detected
    //by the TriggerBrowser but the matching new services is not yet in our local list.
    //Just deal with it for now. (Maybe later we'll rely on shared ServiceList in triggers too)
    Result := TExtServiceEntry(MainForm.Services.Find(TriggerData.Trigger.ServiceName))
end;

procedure TMainTriggerList.TreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  inherited;
  if not Self.Visible then exit; //we'll handle the TB focus when showing the TB
  //Show details for this service in details pane.
  MainForm.SetDetailsPaneFocusedService(Self.GetFocusedService);
end;


end.
