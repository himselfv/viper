unit Viper.MainServiceList;

interface
uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Actions, ActnList, ExtCtrls, ImgList, UiTypes, Menus, StdCtrls, ComCtrls, ActiveX, VirtualTrees,
  Generics.Collections, IniFiles, ServiceHelper, SvcEntry, OsSvcEntry, SvcCat,
  Viper.ServiceList, Viper.TriggerList, Viper.DependencyList, Viper.RichEditEx,
  Viper.ServiceTriggerList, CommonResources;

type
  TExtServiceEntry = class(TOsServiceEntry)
    Info: TServiceInfo;
    function GetEffectiveDisplayName: string; override;
    procedure GetIcon(out AImageList: TCustomImageList; out AIndex: integer); override;
  end;

  TMainServiceList = class(TServiceList)
    Splitter2: TSplitter;
    pcBottom: TPageControl;
    tsDescription: TTabSheet;
    Label1: TLabel;
    Splitter3: TSplitter;
    mmDetails: TMemo;
    NotesFrame: TRichEditFrame;
    tsDependencies: TTabSheet;
    DependencySvcList: TDependencyList;
    tsDependents: TTabSheet;
    DependentsSvcList: TDependencyList;
    tsTriggers: TTabSheet;
    TriggerList: TServiceTriggerList;
    tsOperations: TTabSheet;
    aEditServiceNotes: TAction;
    aSaveNotes: TAction;
    aShowDrivers: TAction;
    aShowUserPrototypes: TAction;
    procedure vtServicesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtServicesFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure tsDependenciesShow(Sender: TObject);
    procedure tsDependentsShow(Sender: TObject);
    procedure tsTriggersShow(Sender: TObject);
    procedure aEditServiceNotesExecute(Sender: TObject);
    procedure mmNotesExit(Sender: TObject);
    procedure aSaveNotesExecute(Sender: TObject);
    procedure aShowDriversExecute(Sender: TObject);
    procedure aShowUserPrototypesExecute(Sender: TObject);


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

  protected
    FServices: TServiceEntryList;
    function IsFolderContainsService(AFolder: PVirtualNode; AService: TServiceEntry; ARecursive: boolean = false): boolean;
    procedure IsFolderContainsService_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure FilterServices_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
  public
    procedure RefreshServiceList;
    procedure FilterServices;
    procedure Clear; reintroduce;
    procedure Refresh;
    function AllServiceEntries(const ATypeFilter: cardinal = SERVICE_WIN32): TServiceEntries;
    property Services: TServiceEntryList read FServices;

  protected //Details pane
    FDetailsPaneFocusedService: TExtServiceEntry;
    procedure ReloadDetails;
    procedure ReloadServiceInfo;
    procedure ReloadServiceDependencies;
    procedure ReloadServiceDependents;
    procedure ReloadTriggers;
    function mmNotes: TRichEdit; inline;
  public
    procedure SetDetailsPaneFocusedService(AService: TExtServiceEntry);

  public //Service editing
    function CanEditServiceInfo: boolean;
    procedure SaveNotes;

  end;


implementation
uses Viper.MainForm;

{$R *.dfm}

function TExtServiceEntry.GetEffectiveDisplayName: string;
begin
  if (Info <> nil) and (Info.DisplayName <> '') then
    Result := Info.DisplayName.Replace('%1', inherited)
  else
    Result := inherited;
end;

procedure TExtServiceEntry.GetIcon(out AImageList: TCustomImageList; out AIndex: integer);
begin
  AImageList := CommonRes.ilImages;
  if Self.Status.dwServiceType and SERVICE_DRIVER <> 0 then
    AIndex := CommonRes.iDriver
  else
    AIndex := CommonRes.iService;
end;


constructor TMainServiceList.Create(AOwner: TComponent);
begin
  inherited;
  TriggerList.Initialize;
  FServices := TServiceEntryList.Create;
  DependencySvcList.SetServiceList(FServices);
  DependentsSvcList.SetServiceList(FServices);
 //These had been in the MainForm's OnShow so maybe they aren't needed
  pcBottom.ActivePage := tsDescription;
  ReloadDetails; //tsDescription doesn't update itself and SetDetailsPaneFocusedService will not work
end;

destructor TMainServiceList.Destroy;
begin
  //This had been in MainForm's OnClose, so maybe it has to happen somewhere earlier
  SaveNotes; //since OnExit won't happen
  //Clear everyone who's using FServices
  Self.Clear;
  DependencySvcList.Clear;
  DependentsSvcList.Clear;
  FreeAndNil(FServices);
  inherited;
end;


procedure TMainServiceList.CMShowingChanged(var Message: TMessage);
begin
  if Self.Showing then
    Self.ViewOnActivate
  else
    Self.ViewOnDeactivate;
  inherited;
end;

procedure TMainServiceList.ViewOnActivate;
begin
  Self.FilterServices;
  SetDetailsPaneFocusedService(TExtServiceEntry(Self.GetFocusedService));
end;

procedure TMainServiceList.ViewOnDeactivate;
begin
//
end;

procedure TMainServiceList.vtServicesFocusChanging(Sender: TBaseVirtualTree;
  OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
begin
  inherited;
  if OldNode <> NewNode then begin
    //Save notes on focus change
    if aEditServiceNotes.Checked then
      SaveNotes;
  end;
end;

procedure TMainServiceList.vtServicesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  inherited;
  Self.vtServicesFocusChanged(Sender, Node, Column); //inherited
  if Self.Visible then
    SetDetailsPaneFocusedService(TExtServiceEntry(Self.GetFocusedService));
  //Action availability checking is done in OnChanged, depends on Selection
end;


procedure TMainServiceList.UMClear(var Message: TMessage);
begin
  Self.Clear;
end;

procedure TMainServiceList.UMRefresh(var Message: TMessage);
begin
  if not Self.Visible then exit;
  Self.Refresh;
  Self.FilterServices;
end;

procedure TMainServiceList.UMQuickFilterChanged(var Message: TMessage);
begin
  Self.FilterServices;
end;


procedure TMainServiceList.FilterServices();
begin
  ApplyFilter(FilterServices_Callback, nil);
end;

procedure TMainServiceList.FilterServices_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var folderNode: PVirtualNode;
  folderData: TNdFolderData;
  nodeData: TObject;
  svc: TExtServiceEntry;
  isService: boolean;
  isVisible: boolean;
  filterText: string;
begin
  nodeData := TObject(Sender.GetNodeData(Node)^);
  Assert(nodeData <> nil);
  Assert(nodeData is TExtServiceEntry); //ServiceList can host folders and more but we don't use it yet!
  svc := TExtServiceEntry(nodeData);

  isService := (svc.Status.dwServiceType and SERVICE_WIN32 <> 0);
  isVisible := true;

  //Filter by folder
  folderNode := MainForm.vtFolders.GetFirstSelected();
  if folderNode <> nil then begin
    folderData := MainForm.GetFolderData(folderNode);
    case MainForm.SpecialFolderType(folderData) of
      ntNone: isVisible := false;
      ntAllServicesDrivers: isVisible := true;
      ntRunningServices: isVisible := isService and (svc.Status.dwCurrentState <> SERVICE_STOPPED);
      ntAllServices: isVisible :=  isService;
      ntUnknownServices: isVisible := isService and ((svc.Info = nil) or (Length(svc.Info.Folders) <= 0));
      ntRunningDrivers: isVisible := (not isService) and (svc.Status.dwCurrentState <> SERVICE_STOPPED);
      ntAllDrivers: isVisible := not isService;
      ntTriggers: isVisible := false;
      ntScheduledTasks: isVisible := false;
    else //pointer
      isVisible := isService and IsFolderContainsService(folderNode, svc, {Recursive=}MainForm.aIncludeSubfolders.Checked);
    end;
  end;

  //Filter out drivers if disabled
  if not aShowDrivers.Checked and not isService then
    isVisible := false;

  //Filter UserService prototypes. Instances would have SERVICE_USERSERVICE_INSTANCE
  if not aShowUserPrototypes.Checked and (svc.Status.dwServiceType and SERVICE_USER_SERVICE <> 0)
    and (svc.Status.dwServiceType and SERVICE_USERSERVICE_INSTANCE = 0) then
    isVisible := false;

  //Quickfilter
  filterText := AnsiLowerCase(MainForm.QuickFilter.Trim());
  if filterText <> '' then
    if not AnsiLowerCase(svc.ServiceName).Contains(filterText)
    and not AnsiLowerCase(svc.DisplayName).Contains(filterText) then
      isVisible := false;

  Sender.IsVisible[Node] := isVisible;
end;

{
Service list refresh currently depends on first refreshing the actual loaded service list
in the main form.
After that, call our own refresh.
Ideally I think the main list has to be moved somewhere neutral, and our own
Refresh has to recursively call it instead of relying on the MainForm to arrange that.
Then other actors (TriggerList?) that rely on the main service list can just also
query its reloading.
}

{
Updates the list of services and their status.
To do a full reload, clear everything then refresh. Otherwise tries to keep changes to a minimum.
}
procedure TMainServiceList.RefreshServiceList;
var hSC: SC_HANDLE;
  ServiceTypes: dword;
  Services, S: PEnumServiceStatusProcess;
  ServicesReturned: cardinal;
  i, j: integer;
  svc: TExtServiceEntry;
  ServiceFound: array of boolean;
  Node: PVirtualNode;
  Abort: boolean;
begin
  //Let's load all services since we need all for dependencies, just make sure to handle
  //permission denials well.
  ServiceTypes := SERVICE_TYPE_ALL;

  //Mark already known services for sweeping
  SetLength(ServiceFound, FServices.Count);
  for i := 0 to Length(ServiceFound)-1 do
    ServiceFound[i] := false;

  //Actualize the list of services
  Services := nil;
  hSC := OpenSCManager(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
  try
    if not ShEnumServicesStatusEx(hSC, ServiceTypes, SERVICE_STATE_ALL, nil, Services, ServicesReturned) then
      RaiseLastOsError();

    S := Services;
    for i := 0 to ServicesReturned - 1 do begin
      j := FServices.FindIndex(S.lpServiceName);
      if j >= 0 then begin
        ServiceFound[j] := true;
        FServices[j].Status := S.ServiceStatus;
        //Will be invalidated later
      end else begin
        svc := TExtServiceEntry.CreateFromEnum(S);
        svc.Info := MainForm.ServiceCat.Find(svc.ServiceName);
        FServices.Add(svc);
        //Will be pushed to UI later
      end;
      Inc(S);
    end;

  finally
    if Services <> nil then
      FreeMem(Services);
    CloseServiceHandle(hSC);
  end;

  MainForm.FilterFolders; //service list changed, re-test which folders are empty

  //Update the UI
  Self.BeginUpdate;
  try
    //Add new services
    for i := Length(ServiceFound) to FServices.Count-1 do begin
      Abort := false;
      Node := Self.AddService(nil, FServices[i]);
      //Apply visibility according to filters
      FilterServices_Callback(Self.vtServices, Node, nil, Abort);
    end;

    //Invalidate existing services
    for i := 0 to Length(ServiceFound)-1 do
      if ServiceFound[i] then begin
        InvalidateService(FServices[i]); //but do not RefreshService since we already have its ServiceStatus

        //We will not make visible nodes invisible, for this is usually not what the user wants.
        //But if a service now matches the criteria, we will make it visible on Refresh
        Node := Self.FindServiceNode(FServices[i]);
        if not Self.vtServices.IsVisible[Node] then
          FilterServices_Callback(Self.vtServices, Node, nil, Abort);
      end;

    //Delete missing service entries
    for i := 0 to Length(ServiceFound)-1 do
      if not ServiceFound[i] then
        Self.DeleteServiceNode(FServices[i]);
  finally
    Self.EndUpdate;
  end;

  //Delete missing services - do this last, it'll screw with indexing
  for i := Length(ServiceFound)-1 downto 0 do
    if not ServiceFound[i] then
      FServices.Delete(i);
end;

procedure TMainServiceList.Refresh;
begin
  RefreshServiceList;
end;

procedure TMainServiceList.Clear;
begin
  inherited Clear;
  FServices.Clear;
end;


//Presents all services as a TServiceEntries list (some functions want this).
//Use FServices directly if you have a choice.
function TMainServiceList.AllServiceEntries(const ATypeFilter: cardinal = SERVICE_WIN32): TServiceEntries;
var i: integer;
begin
  SetLength(Result, 0);
  for i := 0 to FServices.Count-1 do
    if (FServices[i].Config <> nil) and (FServices[i].Config.dwServiceType and ATypeFilter <> 0) then begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := FServices[i];
    end;
end;

//True if the folder contains service.
//Handled outside of TNdFolderData inself because we need to support recursion and only the tree knows children.
function TMainServiceList.IsFolderContainsService(AFolder: PVirtualNode; AService: TServiceEntry; ARecursive: boolean = false): boolean;
var AFolderData: TNdFolderData;
begin
  AFolderData := MainForm.GetFolderData(AFolder);
  Result := AFolderData.ContainsService(AService.ServiceName);
  if (not Result) and ARecursive then
    Result := MainForm.vtFolders.IterateSubtree(AFolder, IsFolderContainsService_Callback, pointer(AService)) <> nil;
end;

//Servant function to IsFolderContainsService.
//Calls IsFolderContainsService on all nodes, returns first that succeeds. Called for all sub-nodes so no need to call IFCS with recursion
procedure TMainServiceList.IsFolderContainsService_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var AService: TServiceEntry absolute Data;
begin
  Abort := IsFolderContainsService(Node, AService, {ARecursive=}false);
end;


{
Details pane
Shows details for whatever is in FDetailsPaneFocusedService.
}

procedure TMainServiceList.SetDetailsPaneFocusedService(AService: TExtServiceEntry);
begin
  if AService = FDetailsPaneFocusedService then exit;
  FDetailsPaneFocusedService := AService;
  ReloadDetails;
end;

//Reloads all data in the details pane
procedure TMainServiceList.ReloadDetails;
begin
 //Only reload visible data, the rest is reloaded as we switch between tabs
  ReloadServiceInfo;
  if pcBottom.ActivePage = tsDependencies then
    ReloadServiceDependencies;
  if pcBottom.ActivePage = tsDependents then
    ReloadServiceDependents;
  if pcBottom.ActivePage = tsTriggers then
    ReloadTriggers;
end;

type
  TControlHack = class(TControl)
  end;

function GetControlFontHeight(Control: TWinControl): integer;
var DC: HDC;
  SaveFont : HFont;
  Metrics : TTextMetric;
begin
  DC := GetDC(Control.Handle);
  SaveFont := SelectObject(DC, TControlHack(Control).Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(Control.Handle, DC);
  Result := Metrics.tmHeight;
end;

procedure TMainServiceList.ReloadServiceInfo;
var service: TExtServiceEntry;
begin
  service := FDetailsPaneFocusedService;

  if service <> nil then
    mmDetails.Text := service.Description
  else
    mmDetails.Text := '';
//  mmDetails.Height := mmDetails.Lines.Count * GetControlFontHeight(mmDetails) + 8;

  if (service <> nil) and (service.Info <> nil) then
    mmNotes.Text := service.Info.Description
  else
    mmNotes.Text := '';
end;

function TMainServiceList.mmNotes: TRichEdit;
begin
  Result := NotesFrame.mmNotes;
end;

procedure TMainServiceList.tsDependenciesShow(Sender: TObject);
begin
  ReloadServiceDependencies;
end;

procedure TMainServiceList.ReloadServiceDependencies;
begin
  DependencySvcList.ReloadDependencies(FDetailsPaneFocusedService);
end;

procedure TMainServiceList.tsDependentsShow(Sender: TObject);
begin
  ReloadServiceDependents;
end;

procedure TMainServiceList.ReloadServiceDependents;
begin
  DependentsSvcList.ReloadDependents(FDetailsPaneFocusedService);
end;

//Triggers are Windows 7-introduced instructions to start/stop service on a certain system events.

procedure TMainServiceList.tsTriggersShow(Sender: TObject);
begin
  ReloadTriggers;
end;

procedure TMainServiceList.ReloadTriggers;
var service: TOsServiceEntry;
begin
  service := FDetailsPaneFocusedService;
  if service = nil then
    TriggerList.SetService('')
  else
    TriggerList.SetService(service.ServiceName, service.Handle);
end;



//Enters/exits notes edit mode for a currently focused service
procedure TMainServiceList.aEditServiceNotesExecute(Sender: TObject);
begin
  inherited;

  //If no service is focused, abort
  if aEditServiceNotes.Checked and (FDetailsPaneFocusedService = nil) then begin
    aEditServiceNotes.Checked := false;
    exit;
  end;

  //Enable note editing
  if aEditServiceNotes.Checked then begin
    mmNotes.Color := clWindow;
    //Auto-switch to the editor
    if pcBottom.ActivePage <> tsDescription then
      pcBottom.ActivePage := tsDescription;
    mmNotes.SetFocus;
  end else
    mmNotes.Color := clBtnFace;
  mmNotes.ReadOnly := not aEditServiceNotes.Checked;

  //Can't save notes unless we're editing
  aSaveNotes.Visible := aEditServiceNotes.Checked;
  aSaveNotes.Enabled := aEditServiceNotes.Checked;

  //Return to the list if the editor was focused (but not if something else)
  if mmNotes.Focused and not aEditServiceNotes.Checked
  and Self.Visible then
    Self.vtServices.SetFocus;
end;

function TMainServiceList.CanEditServiceInfo: boolean;
begin
  Result := aEditServiceNotes.Checked; //Edit mode is common for folders and service info
end;

procedure TMainServiceList.mmNotesExit(Sender: TObject);
begin
  SaveNotes;
end;

//Saves the service notes if they are currently being edited and there are changes
procedure TMainServiceList.SaveNotes;
var service: TExtServiceEntry;
begin
  if mmNotes.ReadOnly then exit; // couldn't have been changed

  service := FDetailsPaneFocusedService;
  if service = nil then exit;

  if (service.Info = nil) and (mmNotes.Text = '') then exit; //do not create a file unless there's a point

  if service.Info = nil then
    service.Info := MainForm.ServiceCat.Get(service.ServiceName);

  if service.Info.Description = mmNotes.Text then exit; //nothing changed

  service.Info.Description := mmNotes.Text;
  service.Info.SaveToMainFile;
end;

procedure TMainServiceList.aSaveNotesExecute(Sender: TObject);
begin
  inherited;
  //Ctrl-S shortcut for saving notes while editing
  if (not mmNotes.ReadOnly) and mmNotes.Focused then
    SaveNotes;
end;



procedure TMainServiceList.aShowDriversExecute(Sender: TObject);
begin
  inherited;
  MainForm.FilterFolders;
  MainForm.QuickFilterChanged;
end;

procedure TMainServiceList.aShowUserPrototypesExecute(Sender: TObject);
begin
  inherited;
  Self.FilterServices;
  MainForm.QuickFilterChanged;
end;

end.
