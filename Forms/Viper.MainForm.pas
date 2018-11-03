unit Viper.MainForm;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Actions, ActnList, ExtCtrls, ImgList, UiTypes, Menus, StdCtrls, ComCtrls, ActiveX, VirtualTrees,
  Generics.Collections, ServiceHelper, SvcEntry, SvcCat, Viper.ServiceList, Viper.TriggerList,
  Viper.DependencyList, Viper.ServiceTriggerList, Viper.RichEditEx;

type
  //Folder node data contains only a single pointer.
  //If it's < ntMax then it's TFolderNodeType, otherwise it's a TServiceFolder.
  TNdFolderData = TServiceFolder;
  PNdFolderData = ^TNdFolderData;

  TFolderNodeType = (
    //The order is important, folders are sorted by it
    ntNone = 0,               //no folder type OR information is assigned
    ntAllServicesDrivers = 1, //drivers and services together
    ntAllServices = 10,
    ntAllDrivers,
    ntTriggers = 20,
    ntFolder = 100,           //represents the other values when cast to TFolderNodeType, for sorting purposes
    ntUnknownServices = 200,  //unsorted
    ntRunningServices,
    ntRunningDrivers,
    ntMax = 255               //values higher than this are objects
  );

  TExtServiceEntry = class(TServiceEntry)
    Info: TServiceInfo;
    function GetEffectiveDisplayName: string; override;
    procedure GetIcon(out AImageList: TCustomImageList; out AIndex: integer); override;
  end;

  TMainForm = class(TForm)
    ActionList: TActionList;
    Splitter1: TSplitter;
    vtFolders: TVirtualStringTree;
    MainMenu: TMainMenu;
    cbHideEmptyFolders: TMenuItem;
    aHideEmptyFolders: TAction;
    pmFolders: TPopupMenu;
    miHideEmptyFolders: TMenuItem;
    pnlMain: TPanel;
    Splitter2: TSplitter;
    File1: TMenuItem;
    aShowDrivers: TAction;
    miShowDrivers: TMenuItem;
    miUseColors: TMenuItem;
    aRefresh: TAction;
    Refresh1: TMenuItem;
    pcBottom: TPageControl;
    tsDescription: TTabSheet;
    tsDependencies: TTabSheet;
    tsDependents: TTabSheet;
    tsOperations: TTabSheet;
    MainServiceList: TServiceList;
    N1: TMenuItem;
    Refresh2: TMenuItem;
    DependencySvcList: TDependencyList;
    DependentsSvcList: TDependencyList;
    tsTriggers: TTabSheet;
    TriggerList: TServiceTriggerList;
    aIncludeSubfolders: TAction;
    Includesubfolderscontents1: TMenuItem;
    aSaveAllServicesConfig: TAction;
    aRestoreServiceConfig: TAction;
    N2: TMenuItem;
    N3: TMenuItem;
    Saveserviceconfig1: TMenuItem;
    Restoreserviceconfig1: TMenuItem;
    aClose: TAction;
    Close1: TMenuItem;
    aSaveSelectedServicesConfig: TAction;
    Saveserviceconfig2: TMenuItem;
    OpenServiceConfigDialog: TOpenDialog;
    SaveServiceConfigDialog: TSaveDialog;
    Debug1: TMenuItem;
    miShowLog: TMenuItem;
    edtQuickFilter: TEdit;
    aRestartAsAdmin: TAction;
    Restartasadministrator1: TMenuItem;
    N4: TMenuItem;
    aAddFolder: TAction;
    aRenameFolder: TAction;
    aDeleteFolder: TAction;
    N5: TMenuItem;
    miAddFolder: TMenuItem;
    miRenameFolder: TMenuItem;
    miDeleteFolder: TMenuItem;
    aEditFolders: TAction;
    miEditFolders: TMenuItem;
    aRemoveServiceFromFolder: TAction;
    aRenameService: TAction;
    N6: TMenuItem;
    miRenameService: TMenuItem;
    miRemoveServiceFromFolder: TMenuItem;
    aSaveNotes: TAction;
    mmDetails: TMemo;
    Label1: TLabel;
    Splitter3: TSplitter;
    aEditServiceNotes: TAction;
    NotesFrame: TRichEditFrame;
    aConfigureColors: TAction;
    miConfigureColors: TMenuItem;
    miEdit: TMenuItem;
    Editfolders1: TMenuItem;
    Editnotes1: TMenuItem;
    aShowUserPrototypes: TAction;
    miShowPrototypes: TMenuItem;
    miView: TMenuItem;
    N8: TMenuItem;
    miTools: TMenuItem;
    N9: TMenuItem;
    miRunServicesMsc: TMenuItem;
    aRunServicesMsc: TAction;
    N7: TMenuItem;
    aSettings: TAction;
    miSettings: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure aCloseExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure vtFoldersGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vtFoldersInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vtFoldersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtFoldersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vtFoldersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtFoldersChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtFoldersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vtFoldersMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure vtFoldersCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure vtFoldersDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vtFoldersDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
      State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vtFoldersDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vtFoldersEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure vtFoldersNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      NewText: string);
    procedure vtFoldersEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure aEditFoldersExecute(Sender: TObject);
    procedure aAddFolderExecute(Sender: TObject);
    procedure aRenameFolderExecute(Sender: TObject);
    procedure aDeleteFolderExecute(Sender: TObject);
    procedure aHideEmptyFoldersExecute(Sender: TObject);
    procedure aShowDriversExecute(Sender: TObject);
    procedure MainServiceListvtServicesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure MainServiceListvtServicesDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure tsDependenciesShow(Sender: TObject);
    procedure tsDependentsShow(Sender: TObject);
    procedure tsTriggersShow(Sender: TObject);
    procedure aIncludeSubfoldersExecute(Sender: TObject);
    procedure aSaveAllServicesConfigExecute(Sender: TObject);
    procedure aSaveSelectedServicesConfigExecute(Sender: TObject);
    procedure aRestoreServiceConfigExecute(Sender: TObject);
    procedure miShowLogClick(Sender: TObject);
    procedure edtQuickFilterChange(Sender: TObject);
    procedure aRestartAsAdminExecute(Sender: TObject);
    procedure miTriggerBrowserClick(Sender: TObject);
    procedure miServiceBrowserClick(Sender: TObject);
    procedure aRemoveServiceFromFolderExecute(Sender: TObject);
    procedure mmNotesExit(Sender: TObject);
    procedure aSaveNotesExecute(Sender: TObject);
    procedure aRenameServiceExecute(Sender: TObject);
    procedure MainServiceListvtServicesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure MainServiceListvtServicesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; NewText: string);
    procedure MainServiceListvtServicesKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure MainServiceListvtServicesCreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; out EditLink: IVTEditLink);
    procedure aEditServiceNotesExecute(Sender: TObject);
    procedure aConfigureColorsExecute(Sender: TObject);
    procedure edtQuickFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MainServiceListvtServicesFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure aShowUserPrototypesExecute(Sender: TObject);
    procedure aRunServicesMscExecute(Sender: TObject);
    procedure aSettingsExecute(Sender: TObject);
    procedure vtFoldersCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);

  protected
    function GetFolderData(AFolderNode: PVirtualNode): TNdFolderData; inline;
    function IsSpecialFolder(AFolder: TNdFolderData): boolean; inline;
    function SpecialFolderType(AFolder: TNdFolderData): TFolderNodeType; inline;

  protected
    FServiceCat: TServiceCatalogue; //catalogue of static service information
    function vtFolders_Add(AParent: PVirtualNode; AInfo: TServiceFolder): PVirtualNode;
    function vtFolders_AddSpecial(AParent: PVirtualNode; AType: TFolderNodeType): PVirtualNode;
    procedure FilterFolders();
    procedure Folders_HideAll(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure Folders_ShowFiltered(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure NodeMarkVisible(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure CreateFolderNodes(AFolderNode: PVirtualNode; AFolderInfo: TServiceFolder);
  public
    procedure ReloadServiceTree;

  protected // Folder editing
    function CanEditFolders: boolean;
    procedure FoldersDropFolder(Sender: TBaseVirtualTree; SourceNode: PVirtualNode; Mode: TDropMode);
    procedure FoldersDropService(Sender: TBaseVirtualTree; Service: TServiceEntry; Mode: TDropMode); overload;

  protected
    FServices: TServiceEntryList;
    function AllServiceEntries(const ATypeFilter: cardinal = SERVICE_WIN32): TServiceEntries;
    procedure FilterServices();
    procedure FilterServices_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure FilterTriggers();
    procedure FilterTriggers_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    function IsFolderContainsService(AFolder: PVirtualNode; AService: TServiceEntry; ARecursive: boolean = false): boolean;
    procedure IsFolderContainsService_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);

  protected //Details pane
    FDetailsPaneFocusedService: TExtServiceEntry;
    procedure SetDetailsPaneFocusedService(AService: TExtServiceEntry);
    procedure ReloadDetails;
    procedure ReloadServiceInfo;
    procedure ReloadServiceDependencies;
    procedure ReloadServiceDependents;
    procedure ReloadTriggers;
    function mmNotes: TRichEdit; inline;

  protected //Service editing
    function CanEditServiceInfo: boolean;
    procedure SaveNotes;

  protected //Inplace trigger browser
    FTriggerBrowser: TTriggerList;
    procedure InitTriggerBrowser;
    procedure FreeTriggerBrowser;
    function TriggerBrowserGetFocusedService: TExtServiceEntry;
    procedure TriggerBrowserFocusChanged(Sender: TObject; const TriggerData: PNdTriggerData);
    procedure ShowServiceBrowser;
    procedure ShowTriggerBrowser;

  protected
    procedure RefreshServiceList;
  public
    procedure Refresh;
    procedure FullReload;

  end;

  TServiceEditLink = class(TStringEditLink)
  public
    function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; override; stdcall;
  end;


var
  MainForm: TMainForm;

implementation
uses FilenameUtils, CommCtrl, ShellApi, Clipbrd, WinApiHelper, ShellUtils, AclHelpers,
  CommonResources, Viper.RestoreServiceConfig, Viper.Log, TriggerUtils,
  Viper.StyleSettings, Viper.Settings;

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


procedure TMainForm.FormCreate(Sender: TObject);
begin
  FServiceCat := TServiceCatalogue.Create();
  FServices := TServiceEntryList.Create;
  DeviceInterfaceClassesFile := AppFolder()+'\DeviceInterfaceClasses.txt';
  RpcInterfacesFile := AppFolder()+'\RpcInterfaces.txt';
  TriggerList.Initialize;
  DependencySvcList.SetServiceList(FServices);
  DependentsSvcList.SetServiceList(FServices);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeTriggerBrowser;
  FreeAndNil(FServices);
  FreeAndNil(FServiceCat);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //Load settings
  SettingsForm.LoadSettings;

  aRestartAsAdmin.Visible := not IsUserAdmin();
  pcBottom.ActivePage := tsDescription;
  ReloadServiceTree;
  Refresh;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveNotes; //since OnExit won't happen
end;

procedure TMainForm.aCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.aRestartAsAdminExecute(Sender: TObject);
var err: integer;
begin
  err := RestartAsAdmin();
  if err = 0 then begin
    Application.Terminate;
    exit;
  end;
  if err <> ERROR_CANCELLED then
    RaiseLastOsError(err);
end;

procedure TMainForm.aSettingsExecute(Sender: TObject);
begin
  SettingsForm.ShowModal;
  MainServiceList.Invalidate;
end;

procedure TMainForm.aConfigureColorsExecute(Sender: TObject);
begin
  StyleSettingsForm.ShowModal;
  MainServiceList.Invalidate;
end;

{
Updates the list of services and their status.
To do a full reload, clear everything then refresh. Otherwise tries to keep changes to a minimum.
}
procedure TMainForm.RefreshServiceList;
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
        svc := TExtServiceEntry.CreateFromEnum(hSC, S);
        svc.Info := FServiceCat.Find(svc.ServiceName);
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

  FilterFolders; //service list changed, re-test which folders are empty

  //Update the UI

  MainServiceList.BeginUpdate;
  try
    //Add new services
    for i := Length(ServiceFound) to FServices.Count-1 do begin
      Abort := false;
      Node := MainServiceList.AddService(nil, FServices[i]);
      //Apply visibility according to filters
      FilterServices_Callback(MainServiceList.vtServices, Node, nil, Abort);
    end;

    //Invalidate existing services
    for i := 0 to Length(ServiceFound)-1 do
      if ServiceFound[i] then begin
        InvalidateService(FServices[i]); //but do not RefreshService since we already have its ServiceStatus

        //We will not make visible nodes invisible, for this is usually not what the user wants.
        //But if a service now matches the criteria, we will make it visible on Refresh
        Node := MainServiceList.FindServiceNode(FServices[i]);
        if not MainServiceList.vtServices.IsVisible[Node] then
          FilterServices_Callback(MainServiceList.vtServices, Node, nil, Abort);
      end;

    //Delete missing service entries
    for i := 0 to Length(ServiceFound)-1 do
      if not ServiceFound[i] then
        MainServiceList.DeleteServiceNode(FServices[i]);
  finally
    MainServiceList.EndUpdate;
  end;

  //Delete missing services - do this last, it'll screw with indexing
  for i := Length(ServiceFound)-1 downto 0 do
    if not ServiceFound[i] then
      FServices.Delete(i);
end;

procedure TMainForm.Refresh;
begin
  RefreshServiceList;
  //TriggerBrowser is dumber and currently always reloads fully
  if (FTriggerBrowser <> nil) and FTriggerBrowser.Visible then begin
    FTriggerBrowser.Reload;
    Self.FilterTriggers;
  end;
end;

procedure TMainForm.FullReload;
begin
  MainServiceList.Clear;
  FServices.Clear;
  if FTriggerBrowser <> nil then
    FTriggerBrowser.Clear;
  Refresh;
end;

procedure TMainForm.aRefreshExecute(Sender: TObject);
begin
  Self.Refresh;
end;

procedure TMainForm.miShowLogClick(Sender: TObject);
begin
  LogForm.Show;
end;

//Presents all services as a TServiceEntries list (some functions want this).
//Use FServices directly if you have a choice.
function TMainForm.AllServiceEntries(const ATypeFilter: cardinal = SERVICE_WIN32): TServiceEntries;
var i: integer;
begin
  SetLength(Result, 0);
  for i := 0 to FServices.Count-1 do
    if (FServices[i].Config <> nil) and (FServices[i].Config.dwServiceType and ATypeFilter <> 0) then begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := FServices[i];
    end;
end;

procedure TMainForm.FilterServices();
begin
  MainServiceList.ApplyFilter(FilterServices_Callback, nil);
end;

procedure TMainForm.FilterServices_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
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
  folderNode := vtFolders.GetFirstSelected();
  if folderNode <> nil then begin
    folderData := GetFolderData(folderNode);
    case SpecialFolderType(folderData) of
      ntNone: isVisible := false;
      ntAllServicesDrivers: isVisible := true;
      ntRunningServices: isVisible := isService and (svc.Status.dwCurrentState <> SERVICE_STOPPED);
      ntAllServices: isVisible :=  isService;
      ntUnknownServices: isVisible := isService and ((svc.Info = nil) or (Length(svc.Info.Folders) <= 0));
      ntRunningDrivers: isVisible := (not isService) and (svc.Status.dwCurrentState <> SERVICE_STOPPED);
      ntAllDrivers: isVisible := not isService;
      ntTriggers: isVisible := false;
    else //pointer
      isVisible := isService and IsFolderContainsService(folderNode, svc, {Recursive=}aIncludeSubfolders.Checked);
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
  filterText := AnsiLowerCase(string(edtQuickfilter.Text).Trim());
  if filterText <> '' then
    if not AnsiLowerCase(svc.ServiceName).Contains(filterText)
    and not AnsiLowerCase(svc.DisplayName).Contains(filterText) then
      isVisible := false;

  Sender.IsVisible[Node] := isVisible;
end;

procedure TMainForm.FilterTriggers();
begin
  //If it's invisible it's going to be filtered when it's reloaded
  if (FTriggerBrowser <> nil) and FTriggerBrowser.Visible then
    FTriggerBrowser.ApplyFilter(FilterTriggers_Callback, nil);
end;

procedure TMainForm.FilterTriggers_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var triggerData: PNdTriggerData;
  svc: TExtServiceEntry;
  isService: boolean;
  isVisible: boolean;
  filterText: string;
begin
  triggerData := Sender.GetNodeData(Node);

  svc := TExtServiceEntry(Self.FServices.Find(triggerData.ServiceName));
  if svc = nil then begin
    //Maybe someone created a service while we weren't reloading? Show for now.
    Sender.IsVisible[Node] := true;
    exit;
  end;

  isService := (svc.Status.dwServiceType and SERVICE_WIN32 <> 0);
  isVisible := true;

  //Filter out drivers if disabled
  if not aShowDrivers.Checked and not isService then
    isVisible := false;

  //Quickfilter
  filterText := AnsiLowerCase(string(edtQuickfilter.Text).Trim());
  if filterText <> '' then
    if not AnsiLowerCase(svc.ServiceName).Contains(filterText)
    and not AnsiLowerCase(svc.DisplayName).Contains(filterText)
    and not AnsiLowerCase(triggerData.Description).Contains(filterText)
    and not AnsiLowerCase(triggerData.Params).Contains(filterText)
    and not AnsiLowerCase(TriggerActionToString(triggerData.Action)).Contains(filterText) then
      isVisible := false;

  Sender.IsVisible[Node] := isVisible;
end;

//True if the folder contains service.
//Handled outside of TNdFolderData inself because we need to support recursion and only the tree knows children.
function TMainForm.IsFolderContainsService(AFolder: PVirtualNode; AService: TServiceEntry; ARecursive: boolean = false): boolean;
var AFolderData: TNdFolderData;
begin
  AFolderData := GetFolderData(AFolder);
  Result := AFolderData.ContainsService(AService.ServiceName);
  if (not Result) and ARecursive then
    Result := vtFolders.IterateSubtree(AFolder, IsFolderContainsService_Callback, pointer(AService)) <> nil;
end;

//Servant function to IsFolderContainsService.
//Calls IsFolderContainsService on all nodes, returns first that succeeds. Called for all sub-nodes so no need to call IFCS with recursion
procedure TMainForm.IsFolderContainsService_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var AService: TServiceEntry absolute Data;
begin
  Abort := IsFolderContainsService(Node, AService, {ARecursive=}false);
end;

procedure TMainForm.edtQuickFilterChange(Sender: TObject);
begin
  FilterServices();
  FilterTriggers();
end;

procedure TMainForm.edtQuickFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    edtQuickfilter.Text := '';
    edtQuickFilterChange(edtQuickfilter);
  end;

end;




resourcestring
  sFolderAllServicesDrivers = 'Everything';
  sFolderAllServices = 'Services';
  sFolderUnknownServices = 'Unknown';
  sFolderRunningServices = 'Running';
  sFolderAllDrivers = 'Drivers';
  sFolderRunningDrivers = 'Running';
  sFolderTriggers = 'Triggers';

//Reloads services and their folder structure
procedure TMainForm.ReloadServiceTree;
var section: PVirtualNode;
begin
  vtFolders.BeginUpdate;
  try
    FServiceCat.Clear;
    FServiceCat.Load(AppFolder+'\SvcData');
    vtFolders.Clear;
    vtFolders_AddSpecial(nil, ntAllServicesDrivers);
    section := vtFolders_AddSpecial(nil, ntAllServices);
    vtFolders_AddSpecial(section, ntRunningServices);
    CreateFolderNodes(section, nil);
    vtFolders_AddSpecial(section, ntUnknownServices);
    vtFolders.Expanded[section] := true;
    section := vtFolders_AddSpecial(nil, ntAllDrivers);
    vtFolders_AddSpecial(section, ntRunningDrivers);
    vtFolders.Expanded[section] := true;
    vtFolders_AddSpecial(nil, ntTriggers);
  finally
    vtFolders.EndUpdate;
  end;
end;

procedure TMainForm.CreateFolderNodes(AFolderNode: PVirtualNode; AFolderInfo: TServiceFolder);
var AFolderData: TServiceFolder;
  ASubFolderInfo: TServiceFolder;
  ASubNode: PVirtualNode;
begin
  AFolderData := GetFolderData(AFolderNode); //maybe nil
  for ASubFolderInfo in FServiceCat.Folders do begin
    if (ASubFolderInfo.Parent <> Self.GetFolderData(AFolderNode))
    and ((ASubFolderInfo.Parent <> nil) or not IsSpecialFolder(AFolderData)) then continue;
    ASubNode := vtFolders_Add(AFolderNode, ASubFolderInfo);
    CreateFolderNodes(ASubNode, ASubFolderInfo);
  end;
end;


function TMainForm.GetFolderData(AFolderNode: PVirtualNode): TNdFolderData;
begin
  if AFolderNode = nil then
    Result := nil
  else
    Result := TServiceFolder(vtFolders.GetNodeData(AFolderNode)^);
end;

function TMainForm.IsSpecialFolder(AFolder: TNdFolderData): boolean;
begin
  Result := NativeUInt(AFolder) < integer(ntMax);
end;

function TMainForm.SpecialFolderType(AFolder: TNdFolderData): TFolderNodeType;
begin
  //We can't just typecast to TFolderNodeType as this will only look at the lowest byte
  //But we also have to have a result for non-TFolderNodeType values. There's a special value for that
  if (NativeUInt(AFolder) > NativeUInt(ntMax)) then
    Result := ntFolder
  else
    Result := TFolderNodeType(NativeUInt(AFolder));
end;

function TMainForm.vtFolders_Add(AParent: PVirtualNode; AInfo: TServiceFolder): PVirtualNode;
var Data: PNdFolderData;
begin
  Result := vtFolders.AddChild(AParent);
  vtFolders.ValidateNode(Result, false);
  Data := vtFolders.GetNodeData(Result);
  Data^ := AInfo;
end;

function TMainForm.vtFolders_AddSpecial(AParent: PVirtualNode; AType: TFolderNodeType): PVirtualNode;
var Data: PNdFolderData;
begin
  Result := vtFolders.AddChild(AParent, nil);
  vtFolders.ValidateNode(Result, false);
  Data := vtFolders.GetNodeData(Result);
  Data^ := TServiceFolder(AType);
end;

procedure TMainForm.vtFoldersGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNdFolderData)
end;

procedure TMainForm.vtFoldersInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var Data: PNdFolderData;
begin
  Data := Sender.GetNodeData(Node);
//  Initialize(Data^); //no need for now
end;

procedure TMainForm.vtFoldersFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: PNdFolderData;
begin
  Data := Sender.GetNodeData(Node);
//  Finalize(Data^);  //no need for now
end;

procedure TMainForm.vtFoldersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var Data: TNdFolderData;
begin
  Data := GetFolderData(Node);
  if TextType <> ttNormal then exit;
  case Column of
    NoColumn, 0:
      case SpecialFolderType(Data) of
        ntNone: CellText := '';
        ntAllServicesDrivers: CellText := sFolderAllServicesDrivers;
        ntRunningServices: CellText := sFolderRunningServices;
        ntUnknownServices: CellText := sFolderUnknownServices;
        ntAllServices: CellText := sFolderAllServices;
        ntRunningDrivers: CellText := sFolderRunningDrivers;
        ntAllDrivers: CellText := sFolderAllDrivers;
        ntTriggers: CellText := sFolderTriggers;
      else CellText := Data.Name;
      end;
  end;
end;

procedure TMainForm.vtFoldersGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var Data: TNdFolderData;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  Data := GetFolderData(Node);
  if not IsSpecialFolder(Data) then
    ImageIndex := CommonRes.iFolder
  else
  case TFolderNodeType(Data) of
    ntAllServicesDrivers: ImageIndex := CommonRes.iService;
    ntRunningServices: ImageIndex := CommonRes.iStart;
    ntAllServices: ImageIndex := CommonRes.iService;
    ntRunningDrivers: ImageIndex := CommonRes.iStart;
    ntAllDrivers: ImageIndex := CommonRes.iDriver;
    ntTriggers: ImageIndex := CommonRes.iTrigger;
  else ImageIndex := CommonRes.iFolder;
  end;
end;

procedure TMainForm.vtFoldersCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var Data1, Data2: TNdFolderData;
begin
  Data1 := GetFolderData(Node1);
  Data2 := GetFolderData(Node2);

  if IsSpecialFolder(Data1) or IsSpecialFolder(Data2) then
    //This also handles the Special+Normal folder sorting:
    Result := NativeUInt(SpecialFolderType(Data1)) - NativeUInt(SpecialFolderType(Data2))
  else
    //Text comparison is only for Normal+Normal sorting
    Result := CompareText(Data1.Name, Data2.Name);
end;

procedure TMainForm.vtFoldersCollapsing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
begin
 //Top-level nodes have not [+] signs and should always remain expanded
  Allowed := (Node.Parent <> nil) and (Node.Parent <> Sender.RootNode);
end;


procedure TMainForm.vtFoldersMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var Node: PVirtualNode;
begin
 //Always focus or de-focus node on right-click so that we always get the correct right-click actions.

 //Focus is used for right-click actions, Selection for filtering the services

 //Would be better to do this on mouseup, but it only arrives after the popup.

  Node := vtFolders.GetNodeAt(X, Y);
  if vtFolders.FocusedNode <> Node then
    vtFolders.FocusedNode := Node;
end;

procedure TMainForm.vtFoldersDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var Data: TNdFolderData;
begin
  Data := GetFolderData(Node);
  Allowed := Self.CanEditFolders and not IsSpecialFolder(Data); //drag is only allowed for normal folders
end;

procedure TMainForm.vtFoldersDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var Node: PVirtualNode;
  Data: TNdFolderData;
begin
  Node := Sender.GetNodeAt(Pt.X, Pt.Y);
  if Node <> nil then
    Data := GetFolderData(Node)
  else Data := nil;

  Accept := Self.CanEditFolders
    and (Data <> nil) and not IsSpecialFolder(Data); //drop only allowed on normal folders
  if not Accept then exit;

  Accept := (Source = Sender)                 //accept folders
    or (Source = MainServiceList.vtServices); //accept services
end;

procedure TMainForm.vtFoldersDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
  var Effect: Integer; Mode: TDropMode);
var Service: TServiceEntry;
begin
  if not Self.CanEditFolders then exit;

  if Source = vtFolders then
    FoldersDropFolder(Sender, TVirtualStringTree(Source).FocusedNode, Mode)
  else
  if Source = MainServiceList.vtServices then
    for Service in MainServiceList.GetSelectedServices do
      FoldersDropService(Sender, Service, Mode);

 //else ignore
end;

//Called to handle a folder node dropped onto the folder tree
procedure TMainForm.FoldersDropFolder(Sender: TBaseVirtualTree; SourceNode: PVirtualNode; Mode: TDropMode);
var TargetNode: PVirtualNode;
  Folder, NewParent: TServiceFolder;
  attMode: TVTNodeAttachMode;
begin
  TargetNode := Sender.DropTargetNode;

  Folder := GetFolderData(SourceNode);
  NewParent := GetFolderData(TargetNode);

  case Mode of
    dmAbove: begin
      NewParent := NewParent.Parent;
      attMode := amInsertBefore;
    end;
    dmBelow: begin
      NewParent := NewParent.Parent;
      attMode := amInsertAfter;
    end;
    dmOnNode: attMode := amAddChildLast;
  else exit;
  end;

  FServiceCat.MoveFolder(Folder, NewParent); //throws on error;
  Sender.MoveTo(SourceNode, TargetNode, attMode, False);
end;

//Called to handle a service node dropped onto the folder tree
procedure TMainForm.FoldersDropService(Sender: TBaseVirtualTree; Service: TServiceEntry; Mode: TDropMode);
var TargetNode: PVirtualNode;
  SvcInfo: TServiceInfo;
  TargetFolder: TNdFolderData;
begin
  SvcInfo := FServiceCat.Get(Service.ServiceName); // Find or create one

  //We could've created SvcInfo just now, so update the field
  //Thankfully, all TServiceEntries are global between the ServiceList instances! So we only need to update in one place.
  if Service is TExtServiceEntry then
    TExtServiceEntry(Service).Info := SvcInfo;

  TargetNode := Sender.DropTargetNode;
  TargetFolder := GetFolderData(TargetNode);
  if IsSpecialFolder(TargetFolder) then exit;

  case Mode of
    dmAbove,
    dmBelow: TargetFolder := TargetFolder.Parent;
    dmOnNode: begin end;
  else exit;
  end;

  FServiceCat.MoveService(SvcInfo, TargetFolder); //throws on error
  Self.FilterServices;
end;

//Removes the selected services from exactly the active folders (not from every folder they're in)
procedure TMainForm.aRemoveServiceFromFolderExecute(Sender: TObject);
var Folder: TServiceFolder;
  SvcEntry: TServiceEntry;
  SvcInfo: TServiceInfo;
begin
  Folder := GetFolderData(vtFolders.GetFirstSelected());
  if (Folder = nil) or IsSpecialFolder(Folder) then exit; //TODO: shouldn't have even been visible!

  for SvcEntry in MainServiceList.GetSelectedServices do begin
    SvcInfo := FServiceCat.Find(SvcEntry.ServiceName);
    if SvcInfo <> nil then //those that are nil are already outside of everything
      FServiceCat.RemoveServiceFromFolder(SvcInfo, Folder);
  end;
  Self.FilterServices;
end;


procedure TMainForm.vtFoldersEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var Data: TNdFolderData;
begin
  Data := GetFolderData(Node);
  Allowed := Self.CanEditFolders and not IsSpecialFolder(Data);
end;

procedure TMainForm.vtFoldersNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
var Data: TNdFolderData;
begin
  Data := GetFolderData(Node);
  if not self.CanEditFolders or IsSpecialFolder(Data) then exit; //wtf

  if NewText = '' then exit;

  Data.Name := NewText; //Store NewText in Name for now, apply in OnEdited when we're out of Editing mode
end;

resourcestring
  eNameAlreadyExists = 'Folder with this name already exists';

procedure TMainForm.vtFoldersEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var Data: TNdFolderData;
  err: integer;
  errmsg: string;
begin
  Data := GetFolderData(Node);
  if not self.CanEditFolders or IsSpecialFolder(Data) then exit; //wtf

  if SameStr(Data.Name, ExtractFilename(Data.Path)) then
    exit; //nothing to change

  if not MoveFile(PChar(Data.Path), PChar(ExtractFilePath(Data.Path)+'\'+Data.Name)) then begin
    err := GetLastError;
    Data.Name := ExtractFilename(Data.Path);
    case err of
      ERROR_ALREADY_EXISTS: errmsg := eNameAlreadyExists; //common case
    else errmsg := SysErrorMessage(err);
    end;
    MessageBox(Self.Handle, PChar(errmsg), PChar(Self.Caption), MB_ICONERROR + MB_OK);
    exit;
  end;

  Data.Path := ExtractFilePath(Data.Path)+'\'+Data.Name;
end;


//Selection changed
procedure TMainForm.vtFoldersChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var Data: TNdFolderData;
begin
  Data := GetFolderData(Node);
  if IsSpecialFolder(Data) and (TFolderNodeType(Data) = ntTriggers) then begin
    ShowTriggerBrowser;
    FilterTriggers;
  end else begin
    ShowServiceBrowser;
    FilterServices;
  end;
end;

procedure TMainForm.vtFoldersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var Data: TNdFolderData;
begin
  if Node <> nil then
    Data := GetFolderData(Node)
  else Data := nil;

  aAddFolder.Enabled := (Node = nil) or not IsSpecialFolder(Data); //can only add children to normal folders and root
  aRenameFolder.Enabled := (Node <> nil) and not IsSpecialFolder(Data); //only for normal folders
  aDeleteFolder.Enabled := (Node <> nil) and not IsSpecialFolder(Data);
end;

procedure TMainForm.aHideEmptyFoldersExecute(Sender: TObject);
begin
  FilterFolders;
end;

procedure TMainForm.FilterFolders();
begin
 //Since we need to show not only folders which contain services, but also their empty parent folders,
 //we have to do it in two steps.
  vtFolders.BeginUpdate;
  try
   //First, hide all folders
    vtFolders.IterateSubtree(nil, Folders_HideAll, nil, [], {DoInit=}true);
   //Then mark each valid folder (has services) + all of its parents
    vtFolders.IterateSubtree(nil, Folders_ShowFiltered, nil, [], {DoInit=}true);
  finally
    vtFolders.EndUpdate;
  end;
end;

//Hides any node that's passed to it
procedure TMainForm.Folders_HideAll(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
begin
  Sender.IsVisible[Node] := false;
end;

//Shows any folder that must be visible according to current settings, and all its parent folders
procedure TMainForm.Folders_ShowFiltered(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var nd: TNdFolderData;
  HasAnyServices: boolean;
  service: TServiceEntry;
begin
  if Sender.IsVisible[Node] then exit; //don't waste time

  nd := GetFolderData(Node);

  if IsSpecialFolder(nd) then begin
    case SpecialFolderType(nd) of
      ntAllServicesDrivers,
      ntRunningDrivers,
      ntAllDrivers:
        if aShowDrivers.Checked then NodeMarkVisible(Sender, Node);
    else NodeMarkVisible(Sender, Node);
    end;
    exit;
  end;

  //This can be moved to service/folder list reloading and HasAnyServices flag cached
  HasAnyServices := false;
  for service in Self.FServices do
    if nd.ContainsService(service.ServiceName) then begin
      HasAnyServices := true;
      break;
    end;

  if (not aHideEmptyFolders.Checked) or aEditFolders.Checked or HasAnyServices then
    NodeMarkVisible(Sender, Node);
end;

//Makes a node and all of its parents Visible. If any of the parents is already visible, the higher-ups
//are assumed to be visible too.
procedure TMainForm.NodeMarkVisible(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  while (Node <> nil) and not Sender.IsVisible[Node] do begin
    Sender.IsVisible[Node] := true;
    Node := Sender.NodeParent[Node];
  end;
end;

procedure TMainForm.aIncludeSubfoldersExecute(Sender: TObject);
begin
  FilterServices();
end;

procedure TMainForm.aShowDriversExecute(Sender: TObject);
begin
  FilterFolders;
  FilterServices;
  FilterTriggers;
end;

procedure TMainForm.aShowUserPrototypesExecute(Sender: TObject);
begin
  FilterServices;
  FilterTriggers;
end;

procedure TMainForm.MainServiceListvtServicesFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  if OldNode <> NewNode then begin
    //Save notes on focus change
    if aEditServiceNotes.Checked then
      SaveNotes;
  end;
end;

procedure TMainForm.MainServiceListvtServicesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  MainServiceList.vtServicesFocusChanged(Sender, Node, Column); //inherited
  if MainServiceList.Visible then
    SetDetailsPaneFocusedService(TExtServiceEntry(MainServiceList.GetFocusedService));
  //Action availability checking is done in OnChanged, depends on Selection
end;

procedure TMainForm.MainServiceListvtServicesDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := CanEditFolders;
end;

procedure TMainForm.aRunServicesMscExecute(Sender: TObject);
begin
  ShellOpen('services.msc');
end;


{
Inplace trigger browser
The main window can switch between service list mode and trigger list mode.
Some controls such as details pane and quicksearch, as well as reload action,
should apply to both modes, so we have to:
1. Check which mode in their handlers.
2. Re-apply their effects every time we switch to a different mode.
}

//Initializes the trigger browser. Call before first showing the browser (or before all times -- no harm)
procedure TMainForm.InitTriggerBrowser;
begin
  if FTriggerBrowser <> nil then exit;
  FTriggerBrowser := TTriggerList.Create(nil);
  FTriggerBrowser.OnFocusChanged := Self.TriggerBrowserFocusChanged;
  FTriggerBrowser.Dock(pnlMain, MainServiceList.ClientRect);
  FTriggerBrowser.Align := alClient;
  FTriggerBrowser.TabOrder := MainServiceList.TabOrder;
end;

//Frees the browser windows. Call once, at termination.
procedure TMainForm.FreeTriggerBrowser;
begin
  if FTriggerBrowser = nil then exit;
  FreeAndNil(FTriggerBrowser);
  FTriggerBrowser := nil;
end;

//This returns our INTERNAL TServiceEntry matching the trigger entry currently
//focused in the TriggerBrowser
//Returns Nil if a matching service cannot be found which is a valid corner
//case so handle gracefully.
function TMainForm.TriggerBrowserGetFocusedService: TExtServiceEntry;
var TriggerData: PNdTriggerData;
begin
  if FTriggerBrowser = nil then begin
    Result := nil;
    exit;
  end;
  TriggerData := FTriggerBrowser.FocusedTrigger;
  if TriggerData = nil then
    Result := nil
  else
    //There can be some corner cases where the trigger has already been detected
    //by the TriggerBrowser but the matching new services is not yet in our local list.
    //Just deal with it for now. (Maybe later we'll rely on shared ServiceList in triggers too)
    Result := TExtServiceEntry(FServices.Find(TriggerData.ServiceName))
end;

procedure TMainForm.TriggerBrowserFocusChanged(Sender: TObject; const TriggerData: PNdTriggerData);
begin
  if (FTriggerBrowser = nil) or not FTriggerBrowser.Visible then exit; //we'll handle the TB focus when showing the TB
  //Show details for this service in details pane.
  SetDetailsPaneFocusedService(Self.TriggerBrowserGetFocusedService);
end;

procedure TMainForm.ShowServiceBrowser;
var WasFocused: boolean;
begin
  if FTriggerBrowser = nil then exit;
  if MainServiceList.Visible then exit; //already visible
  WasFocused := FTriggerBrowser.Focused;
  MainServiceList.Visible := true;
  FTriggerBrowser.Visible := false;
  SetDetailsPaneFocusedService(TExtServiceEntry(MainServiceList.GetFocusedService));
  if WasFocused then
    MainServiceList.SetFocus;
end;

procedure TMainForm.ShowTriggerBrowser;
var WasFocused: boolean;
begin
  if FTriggerBrowser = nil then
    InitTriggerBrowser
  else
    if FTriggerBrowser.Visible then exit; //already visible
  WasFocused := MainServiceList.Focused;
  FTriggerBrowser.Show;
  MainServiceList.Visible := false;
  FTriggerBrowser.Reload;
  SetDetailsPaneFocusedService(Self.TriggerBrowserGetFocusedService);
  if WasFocused then
    FTriggerBrowser.SetFocus;
end;

procedure TMainForm.miServiceBrowserClick(Sender: TObject);
begin
  ShowServiceBrowser;
end;

procedure TMainForm.miTriggerBrowserClick(Sender: TObject);
begin
  ShowTriggerBrowser;
end;



{
Details pane
Shows details for whatever is in FDetailsPaneFocusedService.
}

procedure TMainForm.SetDetailsPaneFocusedService(AService: TExtServiceEntry);
begin
  if AService = FDetailsPaneFocusedService then exit;
  FDetailsPaneFocusedService := AService;
  ReloadDetails;
end;

//Reloads all data in the details pane
procedure TMainForm.ReloadDetails;
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

procedure TMainForm.ReloadServiceInfo;
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

function TMainForm.mmNotes: TRichEdit;
begin
  Result := NotesFrame.mmNotes;
end;

procedure TMainForm.tsDependenciesShow(Sender: TObject);
begin
  ReloadServiceDependencies;
end;

procedure TMainForm.ReloadServiceDependencies;
begin
  DependencySvcList.ReloadDependencies(FDetailsPaneFocusedService);
end;

procedure TMainForm.tsDependentsShow(Sender: TObject);
begin
  ReloadServiceDependents;
end;

procedure TMainForm.ReloadServiceDependents;
begin
  DependentsSvcList.ReloadDependents(FDetailsPaneFocusedService);
end;

//Triggers are Windows 7-introduced instructions to start/stop service on a certain system events.

procedure TMainForm.tsTriggersShow(Sender: TObject);
begin
  ReloadTriggers;
end;

procedure TMainForm.ReloadTriggers;
var service: TServiceEntry;
begin
  service := FDetailsPaneFocusedService;
  if service = nil then
    TriggerList.SetService('')
  else
    TriggerList.SetService(service.ServiceName, service.Handle);
end;


procedure TMainForm.aSaveAllServicesConfigExecute(Sender: TObject);
begin
  if not SaveServiceConfigDialog.Execute then
    exit;

  SaveServiceConfig(Self.Handle, Self.AllServiceEntries, SaveServiceConfigDialog.FileName);
end;

procedure TMainForm.aSaveSelectedServicesConfigExecute(Sender: TObject);
var ASelectedServices: TServiceEntries;
begin
  ASelectedServices := MainServiceList.GetSelectedServices;
  if Length(ASelectedServices) <= 0 then exit;

  if not SaveServiceConfigDialog.Execute then
    exit;

  SaveServiceConfig(Self.Handle, ASelectedServices, SaveServiceConfigDialog.FileName);
end;

procedure TMainForm.aRestoreServiceConfigExecute(Sender: TObject);
var AForm: TRestoreServiceConfigForm;
begin
  if not OpenServiceConfigDialog.Execute then
    exit;

  AForm := TRestoreServiceConfigForm.Create(Self);
  try
    if IsPositiveResult(AForm.OpenRestore(OpenServiceConfigDialog.Filename)) then
      Refresh(); //all configurations could've changed
  finally
    FreeAndNil(AForm);
  end;
end;



// Edit mode

procedure TMainForm.aEditFoldersExecute(Sender: TObject);
begin
  if not aEditFolders.Checked then
    SaveNotes; //before turning them read-only

  aAddFolder.Visible := aEditFolders.Checked;
  aRenameFolder.Visible := aEditFolders.Checked;
  aDeleteFolder.Visible := aEditFolders.Checked;

  aHideEmptyFolders.Enabled := not aEditFolders.Checked; //Cannot hide empty folders in edit mode
  aHideEmptyFolders.Visible := not aEditFolders.Checked;

  FilterFolders; //Because "Hide empty folders" might implicitly change

 //By default, click+drag on services results in rectangle selection. Dragging interferes with this.
 //Also enable editing
  if aEditFolders.Checked then
    Self.MainServiceList.vtServices.TreeOptions.MiscOptions :=
      Self.MainServiceList.vtServices.TreeOptions.MiscOptions + [toFullRowDrag]
  else
    Self.MainServiceList.vtServices.TreeOptions.MiscOptions :=
      Self.MainServiceList.vtServices.TreeOptions.MiscOptions - [toFullRowDrag];

  //Services
  aRenameService.Visible := aEditFolders.Checked;
  aRemoveServiceFromFolder.Visible := aEditFolders.Checked;
end;

function TMainForm.CanEditFolders: boolean;
begin
  Result := Self.aEditFolders.Checked;
end;


// Folder tree manipulations

resourcestring
  sNewFolderName = 'New Folder';
  sNewFolderNameCtr = 'New Folder (%d)';
  sConfirmDeleteFolder = 'Do you really want to delete folder "%s"?';
  sCannotDeleteFolderNotEmpty = 'Cannot delete this folder because it is not empty.';

procedure TMainForm.aAddFolderExecute(Sender: TObject);
var ParentNode, Node: PVirtualNode;
  ParentData: TNdFolderData;
  Path, NewName: string;
  NewNameCtr: integer;
begin
  //Determine parent node and path
  ParentNode := vtFolders.FocusedNode;
  if ParentNode <> nil then begin
    ParentData := GetFolderData(ParentNode);
    if IsSpecialFolder(ParentData) then exit; //cannot create subfolders for system nodes
    Path := ParentData.Path;
  end else begin
    ParentData := nil;
    Path := FServiceCat.RootPath;
  end;
  if (Length(Path) < 1) or (Path[Length(Path)] <> '\') then
    Path := Path + '\';

  //Create a new folder with an unique name
  NewNameCtr := 0;
  NewName := sNewFolderName;
  while not CreateDirectory(PChar(Path+NewName), nil) do begin
    if GetLastError <> ERROR_ALREADY_EXISTS then
      RaiseLastOsError();
    Inc(NewNameCtr);
    NewName := Format(sNewFolderNameCtr, [NewNameCtr]);
  end;

  //Create a node for this folder
  Node := vtFolders_Add(ParentNode, FServiceCat.AddDir(ParentData, Path+NewName));

  //Auto-start editing
  vtFolders.EditNode(Node, NoColumn);
end;

procedure TMainForm.aRenameFolderExecute(Sender: TObject);
var Node: PVirtualNode;
  Data: TNdFolderData;
begin
  Node := vtFolders.FocusedNode;
  if Node = nil then exit;
  Data := GetFolderData(Node);
  if not IsSpecialFolder(Data) then
    vtFolders.EditNode(Node, NoColumn);
end;

procedure TMainForm.aDeleteFolderExecute(Sender: TObject);
var Node: PVirtualNode;
  Data: TNdFolderData;
begin
  Node := vtFolders.FocusedNode;
  if Node = nil then exit;
  Data := GetFolderData(Node);
  if IsSpecialFolder(Data) then exit;

  if (vtFolders.ChildCount[Node] > 0) or (Length(Data.Services) > 0) then begin
    MessageBox(Self.Handle, PChar(sCannotDeleteFolderNotEmpty), PChar(Self.Caption), MB_ICONERROR or MB_OK);
    exit;
  end;

  if MessageBox(Self.Handle, PChar(Format(sConfirmDeleteFolder, [Data.Name])), PChar(Self.Caption),
    MB_YESNO or MB_ICONQUESTION) <> ID_YES then
    exit;

  if not RemoveDirectory(PChar(Data.Path)) then
    RaiseLastOsError();

  vtFolders.DeleteNode(Node);
end;


// Service editing

//Enters/exits notes edit mode for a currently focused service
procedure TMainForm.aEditServiceNotesExecute(Sender: TObject);
begin
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
  and MainServiceList.Visible then
    MainServiceList.vtServices.SetFocus;
end;

function TMainForm.CanEditServiceInfo: boolean;
begin
  Result := aEditServiceNotes.Checked; //Edit mode is common for folders and service info
end;

procedure TMainForm.mmNotesExit(Sender: TObject);
begin
  SaveNotes;
end;

//Saves the service notes if they are currently being edited and there are changes
procedure TMainForm.SaveNotes;
var service: TExtServiceEntry;
begin
  if mmNotes.ReadOnly then exit; // couldn't have been changed

  service := FDetailsPaneFocusedService;
  if service = nil then exit;

  if (service.Info = nil) and (mmNotes.Text = '') then exit; //do not create a file unless there's a point

  if service.Info = nil then
    service.Info := FServiceCat.Get(service.ServiceName);

  if service.Info.Description = mmNotes.Text then exit; //nothing changed

  service.Info.Description := mmNotes.Text;
  service.Info.SaveToMainFile;
end;

procedure TMainForm.aSaveNotesExecute(Sender: TObject);
begin
  //Ctrl-S shortcut for saving notes while editing
  if (not mmNotes.ReadOnly) and mmNotes.Focused then
    SaveNotes;
end;

procedure TMainForm.aRenameServiceExecute(Sender: TObject);
var Node: PVirtualNode;
begin
  Node := MainServiceList.vtServices.FocusedNode;
  if Node = nil then exit;
  MainServiceList.vtServices.EditNode(Node, TServiceList.colDisplayName); //only Name column is editable
end;

procedure TMainForm.MainServiceListvtServicesKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
  var Shift: TShiftState; var DoDefault: Boolean);
begin
  if (CharCode = VK_F2) and (Shift=[]) then begin
   {
   VirtualTree tries to edit either Column 0 or whatever column is focused (with toExtendedFocus)
   We want to always edit the Display Name.
   Solution: Enable toExtendedFocus and move focus to Display Name column.
   }
    Sender.FocusedColumn := TServiceList.colDisplayName;
  end;
end;

procedure TMainForm.MainServiceListvtServicesEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column = TServiceList.colDisplayName;
end;

{
ServiceInfo.DisplayName can contain substitutions such as "Something something %1". These are
displayed processed, but when editing we want to edit the source.
We subclass TStringEditLink and replace the edit text at the last moment.
}
procedure TMainForm.MainServiceListvtServicesCreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);
begin
  EditLink := TServiceEditLink.Create;
end;

function TServiceEditLink.PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean;
var NodeData: TObject;
  service: TExtServiceEntry;
begin
  Result := inherited; //initialize the control
  if Result then begin
    NodeData := TObject(Tree.GetNodeData(Node)^);
    if not (NodeData is TExtServiceEntry) then begin //only allow editing for services themselves
      Result := false;
      exit;
    end;
    service := TExtServiceEntry(NodeData);
    if (service = nil) or (service.Info = nil) or (service.Info.DisplayName = '') then exit; //use the default value
    Self.Edit.Text := service.Info.DisplayName;
  end;
end;

procedure TMainForm.MainServiceListvtServicesNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
var service: TExtServiceEntry;
begin
  if (not CanEditServiceInfo) or (Column <> TServiceList.colDisplayName) then
    exit;
  service := TExtServiceEntry(MainServiceList.GetServiceEntry(Node));
  if service = nil then exit; //fail

  if service.Info = nil then
    service.Info := FServiceCat.Get(service.ServiceName);

  if SameStr(service.Info.DisplayName, NewText) then exit;

  service.Info.DisplayName := NewText;
  service.Info.SaveToMainFile;
  //We could also save this later in OnEdited, but there would be no way to check if DisplayName has really changed
end;

end.
