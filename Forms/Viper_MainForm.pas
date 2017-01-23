unit Viper_MainForm;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Actions, ActnList, ExtCtrls, ImgList, UiTypes, Menus, StdCtrls, ComCtrls, ActiveX, VirtualTrees,
  Generics.Collections, ServiceHelper, SvcEntry, SvcCat, Viper_ServiceList, Viper_TriggerList;

type
 //Folders are tracked only as folder nodes, we don't keep any middle layer.
  TFolderNodeType = (
    ntFolder,
    ntRunningServices,
    ntAllServices,
    ntUnknownServices,
    ntRunningDrivers,
    ntAllDrivers
  );

  TNdFolderData = record
    NodeType: TFolderNodeType;
    Name: string;                   // visible name
    Description: string;
    Services: array of string;
    Path: string;                   // on disk
    procedure LoadDescription(const AFilename: string);
    function ContainsService(AServiceName: string): boolean;
  end;
  PNdFolderData = ^TNdFolderData;

  TServiceFolders = array of PNdFolderData;
  PServiceFolders = ^TServiceFolders;


  TExtServiceEntry = class(TServiceEntry)
    Info: TServiceInfo;
    function GetEffectiveDisplayName: string; override;
    procedure GetIcon(out AImageList: TCustomImageList; out AIndex: integer); override;
  end;


  TMainForm = class(TForm)
    ActionList: TActionList;
    Splitter1: TSplitter;
    vtFolders: TVirtualStringTree;
    ilImages: TImageList;
    MainMenu: TMainMenu;
    Settings1: TMenuItem;
    cbHideEmptyFolders: TMenuItem;
    aHideEmptyFolders: TAction;
    pmFolders: TPopupMenu;
    Hideemptyfolders1: TMenuItem;
    pnlMain: TPanel;
    Splitter2: TSplitter;
    Colorize1: TMenuItem;
    Bystarttype1: TMenuItem;
    File1: TMenuItem;
    aShowDrivers: TAction;
    Showdrivers1: TMenuItem;
    Bystatus1: TMenuItem;
    aRefresh: TAction;
    Refresh1: TMenuItem;
    pcBottom: TPageControl;
    tsDescription: TTabSheet;
    mmDetails: TMemo;
    tsDependencies: TTabSheet;
    tsDependents: TTabSheet;
    tsOperations: TTabSheet;
    MainServiceList: TServiceList;
    N1: TMenuItem;
    Refresh2: TMenuItem;
    DependencyList: TServiceList;
    DependentsList: TServiceList;
    tsTriggers: TTabSheet;
    TriggerList: TTriggerList;
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
    Label1: TLabel;
    mmAdditionalInfo: TMemo;
    aRestartAsAdmin: TAction;
    Restartasadministrator1: TMenuItem;
    N4: TMenuItem;
    Alltriggers1: TMenuItem;
    aAddFolder: TAction;
    aRenameFolder: TAction;
    aDeleteFolder: TAction;
    N5: TMenuItem;
    pmAddFolder: TMenuItem;
    pmRenameFolder: TMenuItem;
    pmDeleteFolder: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure vtFoldersGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtFoldersInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtFoldersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtFoldersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtFoldersGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtFoldersFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtFoldersDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
    procedure vtFoldersDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
      State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure vtFoldersDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject;
      Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure aHideEmptyFoldersExecute(Sender: TObject);
    procedure MainServiceListvtServicesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
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
    procedure Alltriggers1Click(Sender: TObject);
    procedure aAddFolderExecute(Sender: TObject);
    procedure vtFoldersMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure aRenameFolderExecute(Sender: TObject);
    procedure vtFoldersEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure vtFoldersNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      NewText: string);
    procedure vtFoldersEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);

  protected
    FServiceCat: TServiceCatalogue; //catalogue of static service information
    function vtFolders_Add(AParent: PVirtualNode; AFolderPath: string): PVirtualNode;
    function vtFolders_AddSpecial(AParent: PVirtualNode; AType: TFolderNodeType; ATitle: string): PVirtualNode;
    procedure FilterFolders();
    procedure Folders_HideAll(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure Folders_ShowFiltered(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure NodeMarkVisible(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
    function ServiceCatRootPath: string;
    procedure ReloadServiceTree;
    procedure LoadServiceFolderContents(AFolderNode: PVirtualNode; AFolderPath: string);

  protected
    FServices: TServiceEntryList;
    function AllServiceEntries(const ATypeFilter: cardinal = SERVICE_WIN32): TServiceEntries;
    procedure FilterServices();
    procedure FilterServices_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    function IsFolderContainsService(AFolder: PVirtualNode; AService: TServiceEntry; ARecursive: boolean = false): boolean;
    procedure IsFolderContainsService_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    function GetServiceFolders(Service: TServiceEntry): TServiceFolders;
    procedure GetServiceFolders_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure ReloadServiceDependencies;
    procedure LoadServiceDependencyNodes(AService: TServiceEntry; AParentNode: PVirtualNode);
    procedure ReloadServiceDependents;
    procedure LoadServiceDependentsNodes(hSC: SC_HANDLE; AService: TServiceEntry; AParentNode: PVirtualNode);
    procedure ReloadTriggers;
  public
    procedure Refresh;
    procedure FullReload;

  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, CommCtrl, ShellApi, Clipbrd, WinApiHelper, ShellUtils, AclHelpers,
  CommonResources, Viper_RestoreServiceConfig, Viper.Log, TriggerUtils, Viper_MainTriggerList;

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
  AIndex := CommonRes.iService;
end;


//Loads folder description from description file
procedure TNdFolderData.LoadDescription(const AFilename: string);
var sl: TStringList;
begin
  sl := TStringList.Create();
  try
    sl.LoadFromFile(AFilename);
    Self.Description := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;

//True if folder contains the given service
function TNdFolderData.ContainsService(AServiceName: string): boolean;
var i: integer;
begin
  Result := false;
  AServiceName := LowerCase(AServiceName);
  for i := 0 to Length(Self.Services)-1 do
    if LowerCase(Self.Services[i]) = AServiceName then begin
      Result := true;
      break;
    end;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  FServiceCat := TServiceCatalogue.Create({OwnsObjects=}true);
  FServices := TServiceEntryList.Create;
  WellKnownDeviceInterfaceClasses.LoadFromFile(AppFolder()+'\DeviceInterfaceClasses.txt');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FServices);
  FreeAndNil(FServiceCat);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  aRestartAsAdmin.Visible := not IsUserAdmin();
  pcBottom.ActivePage := tsDescription;
  ReloadServiceTree;
  Refresh;
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

{
Updates the list of services and their status.
To do a full reload, clear everything then refresh. Otherwise tries to keep changes to a minimum.
}
procedure TMainForm.Refresh;
var hSC: SC_HANDLE;
  ServiceTypes: dword;
  Services, S: PEnumServiceStatus;
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
    if not EnumServicesStatus(hSC, ServiceTypes, SERVICE_STATE_ALL, Services, ServicesReturned) then
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

procedure TMainForm.FullReload;
begin
  MainServiceList.Clear;
  FServices.Clear;
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
    if (FServices[i].Config.dwServiceType and ATypeFilter <> 0) then begin
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
  folderData: PNdFolderData;
  svc: TServiceEntry;
  isService: boolean;
  isVisible: boolean;
  filterText: string;
begin
  svc := TServiceEntry(Sender.GetNodeData(Node)^);
  Assert(svc <> nil);

  isService := (svc.Status.dwServiceType and SERVICE_WIN32 <> 0);
  isVisible := true;

  //Filter by folder
  folderNode := vtFolders.FocusedNode;
  if folderNode <> nil then begin
    folderData := PNdFolderData(vtFolders.GetNodeData(folderNode));
    case folderData.NodeType of
      ntFolder: isVisible := isService and IsFolderContainsService(folderNode, svc, {Recursive=}aIncludeSubfolders.Checked);
      ntRunningServices: isVisible := isService and (svc.Status.dwCurrentState <> SERVICE_STOPPED);
      ntAllServices: isVisible :=  isService;
      ntUnknownServices: isVisible := isService and (Length(GetServiceFolders(svc)) <= 0);
      ntRunningDrivers: isVisible := (not isService) and (svc.Status.dwCurrentState <> SERVICE_STOPPED);
      ntAllDrivers: isVisible := not isService;
    end;
  end;

  //Filter out drivers if disabled
  if not isService and not aShowDrivers.Checked then
    isVisible := false;

  //Quickfilter
  filterText := AnsiLowerCase(string(edtQuickfilter.Text).Trim());
  if filterText <> '' then
    if not AnsiLowerCase(svc.ServiceName).Contains(filterText)
    and not AnsiLowerCase(svc.DisplayName).Contains(filterText) then
      isVisible := false;

  Sender.IsVisible[Node] := isVisible;
end;

//True if the folder contains service.
//Handled outside of TNdFolderData inself because we need to support recursion and only the tree knows children.
function TMainForm.IsFolderContainsService(AFolder: PVirtualNode; AService: TServiceEntry; ARecursive: boolean = false): boolean;
var AFolderData: PNdFolderData;
begin
  AFolderData := PNdFolderData(vtFolders.GetNodeData(AFolder));
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
end;




function TMainForm.ServiceCatRootPath: string;
begin
  Result := AppFolder+'\SvcData';
end;

resourcestring
  sFolderUnknownServices = 'Unknown';
  sFolderRunningServices = 'Running';
  sFolderAllServices = 'All';
  sFolderAllDrivers = 'Drivers';
  sFolderRunningDrivers = 'Running';

//Reloads services and their folder structure
procedure TMainForm.ReloadServiceTree;
var section: PVirtualNode;
begin
  vtFolders.BeginUpdate;
  try
    FServiceCat.Clear;
    vtFolders.Clear;
    LoadServiceFolderContents(nil, ServiceCatRootPath);
    vtFolders_AddSpecial(nil, ntUnknownServices, sFolderUnknownServices);
    vtFolders_AddSpecial(nil, ntRunningServices, sFolderRunningServices);
    vtFolders_AddSpecial(nil, ntAllServices, sFolderAllServices);
    section := vtFolders_AddSpecial(nil, ntAllDrivers, sFolderAllDrivers);
    vtFolders_AddSpecial(section, ntRunningDrivers, sFolderRunningDrivers);
  finally
    vtFolders.EndUpdate;
  end;
end;

procedure TMainForm.LoadServiceFolderContents(AFolderNode: PVirtualNode; AFolderPath: string);
var res: integer;
  sr: TSearchRec;
  node: PVirtualNode;
  FolderData: PNdFolderData;
begin
  if AFolderNode = nil then
    FolderData := nil
  else begin
    FolderData := PNdFolderData(vtFolders.GetNodeData(AFolderNode));
    try
      FolderData.LoadDescription(AFolderPath+'\desc');
    except
      on EFOpenError do begin end; //no description, okay
    end;
  end;

  res := FindFirst(AFolderPath+'\*.*', faAnyFile, sr);
  while res = 0 do begin
    if (sr.Name='.') or (sr.Name='..') then begin
      res := FindNext(sr);
      continue;
    end;

    if (sr.Attr and faDirectory) = faDirectory then begin
      node := vtFolders_Add(AFolderNode, AFolderPath+'\'+sr.Name);
      LoadServiceFolderContents(node, AFolderPath+'\'+sr.Name);
    end else
    if ExtractFileExt(sr.Name) = '.txt' then begin
      FServiceCat.LoadServiceInfo(AFolderPath + '\' + sr.Name);
      if FolderData <> nil then begin
        SetLength(FolderData.Services, Length(FolderData.Services)+1);
        FolderData.Services[Length(FolderData.Services)-1] := ChangeFileExt(sr.Name, '');
      end;
    end else
    if ExtractFileExt(sr.Name) = '.lnk' then begin
      if FolderData <> nil then begin
        SetLength(FolderData.Services, Length(FolderData.Services)+1);
        FolderData.Services[Length(FolderData.Services)-1] := ChangeFileExt(sr.Name, '');
      end;
    end;

    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;

function TMainForm.vtFolders_Add(AParent: PVirtualNode; AFolderPath: string): PVirtualNode;
var Data: PNdFolderData;
begin
  Result := vtFolders.AddChild(AParent);
  vtFolders.ValidateNode(Result, false);
  Data := vtFolders.GetNodeData(Result);
  Data.NodeType := ntFolder;
  Data.Name := ExtractFilename(AFolderPath);
  Data.Path := AFolderPath;
end;

function TMainForm.vtFolders_AddSpecial(AParent: PVirtualNode; AType: TFolderNodeType; ATitle: string): PVirtualNode;
var Data: PNdFolderData;
begin
  Result := vtFolders.AddChild(AParent, nil);
  vtFolders.ValidateNode(Result, false);
  Data := vtFolders.GetNodeData(Result);
  Data.NodeType := AType;
  Data.Name := ATitle;
  Data.Path := '';
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
  Initialize(Data^);
end;

procedure TMainForm.vtFoldersFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: PNdFolderData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TMainForm.vtFoldersGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var Data: PNdFolderData;
begin
  Data := Sender.GetNodeData(Node);
  if TextType <> ttNormal then exit;
  case Column of
    NoColumn, 0: CellText := Data.Name;
  end;
end;

procedure TMainForm.vtFoldersGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  ImageIndex := CommonRes.iFolder;
end;

procedure TMainForm.vtFoldersMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var Node: PVirtualNode;
begin
 //Always focus or de-focus node on right-click so that we always get the correct right-click actions.

 //We could've kept FocusedNode intact and introduced another variable to hold the "what was right-clicked",
 //but too much bother. Maybe later.

 //Or maybe we'll use Selection for services (can support multi-selection too) and Focus for actions.

 //Would be better to do this on mouseup, but it only arrives after the popup.

  Node := vtFolders.GetNodeAt(X, Y);
  if vtFolders.FocusedNode <> Node then
    vtFolders.FocusedNode := Node;
end;

procedure TMainForm.vtFoldersDragAllowed(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var Data: PNdFolderData;
begin
  Data := Sender.GetNodeData(Node);
  Allowed := Data.NodeType = ntFolder; //drag only allowed for normal folders
end;

procedure TMainForm.vtFoldersDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState;
  State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var Node: PVirtualNode;
  Data: PNdFolderData;
begin
  Node := Sender.GetNodeAt(Pt.X, Pt.Y);
  if Node <> nil then
    Data := Sender.GetNodeData(Node)
  else Data := nil;

  if (Data = nil) or (Data.NodeType <> ntFolder) then begin //drop only allowed on normal folders
    Accept := false;
    exit;
  end;

  Accept := (Source=Sender); //atm only accept nodes of this tree (later we'll also accept services)
end;

procedure TMainForm.vtFoldersDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint;
  var Effect: Integer; Mode: TDropMode);
var SourceNode, TargetNode: PVirtualNode;
  SourceData, TargetData: PNdFolderData;
  TargetPath: string;
  attMode: TVTNodeAttachMode;
begin
  SourceNode := TVirtualStringTree(Source).FocusedNode;
  TargetNode := Sender.DropTargetNode;

  SourceData := Sender.GetNodeData(SourceNode);
  TargetData := Sender.GetNodeData(TargetNode);

  if TargetNode <> nil then
    TargetPath := TargetData.Path
  else
    TargetPath := ServiceCatRootPath;

  case Mode of
    dmAbove: begin
      TargetPath := ExtractFilePath(TargetPath);
      attMode := amInsertBefore;
    end;
    dmBelow: begin
      TargetPath := ExtractFilePath(TargetPath);
      attMode := amInsertAfter;
    end;
    dmOnNode: attMode := amAddChildLast;
  else exit;
  end;

  if TargetPath[Length(TargetPath)] <> '\' then
    TargetPath := TargetPath + '\';
  TargetPath := TargetPath + ExtractFilename(SourceData.Path);

  if not SameText(TargetPath, SourceData.Path) then begin // otherwise just reordering
    if not MoveFile(PChar(SourceData.Path), PChar(TargetPath)) then
      RaiseLastOsError();
    SourceData.Path := TargetPath;
  end;

  Sender.MoveTo(SourceNode, TargetNode, attMode, False);
end;

procedure TMainForm.vtFoldersEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
var Data: PNdFolderData;
begin
  Data := Sender.GetNodeData(Node);
  Allowed := (Data.NodeType = ntFolder);
end;

procedure TMainForm.vtFoldersNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: string);
var Data: PNdFolderData;
begin
  Data := Sender.GetNodeData(Node);
  if Data.NodeType <> ntFolder then exit; //wtf
  if NewText = '' then exit;

  Data.Name := NewText; //Store NewText in Name for now, apply in OnEdited when we're out of Editing mode
end;

resourcestring
  eNameAlreadyExists = 'Folder with this name already exists';

procedure TMainForm.vtFoldersEdited(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var Data: PNdFolderData;
  err: integer;
  errmsg: string;
begin
  Data := Sender.GetNodeData(Node);
  if Data.NodeType <> ntFolder then exit; //wtf

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

procedure TMainForm.vtFoldersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
var Data: PNdFolderData;
begin
  FilterServices;

  if Node <> nil then
    Data := Sender.GetNodeData(Node)
  else Data := nil;

  aAddFolder.Enabled := (Node = nil) or (Data.NodeType = ntFolder); //can only add children to normal folders and root
  aRenameFolder.Enabled := (Node <> nil) and (Data.NodeType = ntFolder); //only for normal folders
  aDeleteFolder.Enabled := (Node <> nil) and (Data.NodeType = ntFolder);
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
var nd: PNdFolderData;
  HasAnyServices: boolean;
  service: TServiceEntry;
begin
  if Sender.IsVisible[Node] then exit; //don't waste time

  nd := PNdFolderData(Sender.GetNodeData(Node));
  Assert(nd <> nil);

  if nd.NodeType <> ntFolder then begin
    case nd.NodeType of
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

  if (not cbHideEmptyFolders.Checked) or HasAnyServices then
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

procedure TMainForm.MainServiceListvtServicesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var nd: TExtServiceEntry;
begin
  MainServiceList.vtServicesFocusChanged(Sender, Node, Column);

  //Most availability checking is done in OnChanged, depends on Selection
  if Node <> nil then
    nd := TExtServiceEntry(Sender.GetNodeData(Node)^)
  else
    nd := nil;


  if nd <> nil then
    mmDetails.Text := nd.Description
  else
    mmDetails.Text := '';

  if (nd <> nil) and (nd.Info <> nil) then
    mmAdditionalInfo.Text := nd.Info.Description
  else
    mmAdditionalInfo.Text := '';

  if pcBottom.ActivePage = tsDependencies then
    ReloadServiceDependencies;
  if pcBottom.ActivePage = tsDependents then
    ReloadServiceDependents;
  if pcBottom.ActivePage = tsTriggers then
    ReloadTriggers;
end;


type
  TGetServiceFoldersData = record
    Service: TServiceEntry;
    List: TServiceFolders;
  end;
  PGetServiceFoldersData = ^TGetServiceFoldersData;

//Returns a list of folders which contain the service
function TMainForm.GetServiceFolders(Service: TServiceEntry): TServiceFolders;
var Data: TGetServiceFoldersData;
begin
  Data.Service := Service;
  SetLength(Data.List, 0);
  vtFolders.IterateSubtree(nil, GetServiceFolders_Callback, @Data);
  Result := Data.List;
end;

procedure TMainForm.GetServiceFolders_Callback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var ScanData: PGetServiceFoldersData absolute Data;
  NodeData: PNdFolderData;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData = nil then exit;
  if NodeData.NodeType <> ntFolder then exit;
  if NodeData.ContainsService(ScanData.Service.ServiceName) then begin
    SetLength(ScanData.List, Length(ScanData.List)+1);
    ScanData.List[Length(ScanData.List)-1] := NodeData;
  end;
end;

procedure TMainForm.tsDependenciesShow(Sender: TObject);
begin
  ReloadServiceDependencies;
end;

procedure TMainForm.ReloadServiceDependencies;
var service: TServiceEntry;
begin
  service := MainServiceList.GetFocusedService;
  DependencyList.BeginUpdate;
  try
    DependencyList.Clear;
    if service <> nil then
      LoadServiceDependencyNodes(service, nil);
  finally
    DependencyList.EndUpdate;
  end;
end;

resourcestring
  eServiceDependencyNotFound = 'Service %s is not found in a general list.';
  eServiceDependentNotFound = 'Service %s is not found in a general list.';

//Works with DependencyList. Adds child nodes for service dependencies to the node given by ParentNode.
//Call ReloadServiceDependencies if you need to reload the whole DependencyList.
procedure TMainForm.LoadServiceDependencyNodes(AService: TServiceEntry; AParentNode: PVirtualNode);
var deps: TArray<string>;
  dep: string;
  depService: TServiceEntry;
  depNode: PVirtualNode;
begin
  if (AService = nil) or (AService.Config = nil) then
    exit;

  deps := SplitNullSeparatedList(AService.Config.lpDependencies);
  for dep in deps do begin
    if dep.StartsWith(SC_GROUP_IDENTIFIER) then //this is a dependency group, not service name
      continue;
    depService := FServices.Find(dep);
   //NOTE: Dependencies sometimes refer to drivers, so we either have to always load drivers
   //(as we do now), or to ignore failed matches.
    if depService = nil then
      raise Exception.CreateFmt(eServiceDependencyNotFound, [dep]);
    depNode := DependencyList.AddService(AParentNode, depService);
    LoadServiceDependencyNodes(depService, depNode);
  end;
end;

procedure TMainForm.tsDependentsShow(Sender: TObject);
begin
  ReloadServiceDependents;
end;

procedure TMainForm.ReloadServiceDependents;
var service: TServiceEntry;
  hSC: SC_HANDLE;
begin
  service := MainServiceList.GetFocusedService;
  if service = nil then begin
    DependentsList.Clear;
    exit;
  end;

  hSC := 0;
  DependentsList.BeginUpdate;
  try
    DependentsList.Clear;
    hSC := OpenSCManager(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
    LoadServiceDependentsNodes(hSC, service, nil);

  finally
    if hSC <> 0 then CloseServiceHandle(hSC);
    DependentsList.EndUpdate;
  end;
end;

procedure TMainForm.LoadServiceDependentsNodes(hSC: SC_HANDLE; AService: TServiceEntry; AParentNode: PVirtualNode);
var hSvc: SC_HANDLE;
  depnt: LPENUM_SERVICE_STATUS;
  depntCount: cardinal;
  depService: TServiceEntry;
  depNode: PVirtualNode;
begin
  if AService = nil then exit;

  hSvc := OpenService(hSC, AService.ServiceName, SERVICE_ENUMERATE_DEPENDENTS);
  if hSvc = 0 then exit;
  try
    depnt := EnumDependentServices(hSvc, SERVICE_STATE_ALL, depntCount);
    if depnt = nil then exit;

    while depntCount > 0 do begin
      depService := FServices.Find(depnt.lpServiceName);
      if depService = nil then
        raise Exception.CreateFmt(eServiceDependentNotFound, [depnt.lpServiceName]);
      depNode := DependentsList.AddService(AParentNode, depService);
      LoadServiceDependentsNodes(hSC, depService, depNode);

      Inc(depnt);
      Dec(depntCount);
    end;
  finally
    CloseServiceHandle(hSvc);
  end;
end;


//Triggers are Windows 7-introduced instructions to start/stop service on a certain system events.

procedure TMainForm.tsTriggersShow(Sender: TObject);
begin
  ReloadTriggers;
end;

procedure TMainForm.ReloadTriggers;
var service: TServiceEntry;
  triggers: PSERVICE_TRIGGER_INFO;
begin
  TriggerList.Clear;
  service := MainServiceList.GetFocusedService;
  if service = nil then
    exit;

  triggers := QueryServiceTriggers(service.Handle);
  if triggers = nil then exit;
  try
    while triggers.cTriggers > 0 do begin
      TriggerList.Add(triggers.pTriggers);
      Inc(triggers.pTriggers);
      Dec(triggers.cTriggers);
    end;

  finally
    FreeMem(triggers);
  end;
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
begin
  if not OpenServiceConfigDialog.Execute then
    exit;

  if IsPositiveResult(RestoreServiceConfigForm.OpenRestore(OpenServiceConfigDialog.Filename)) then
    Refresh(); //all configurations could've changed
end;

procedure TMainForm.Alltriggers1Click(Sender: TObject);
begin
  MainTriggerList.Services := Self.FServices;
  MainTriggerList.Show;
end;


// Folder tree manipulations

resourcestring
  sNewFolderName = 'New Folder';
  sNewFolderNameCtr = 'New Folder (%d)';

procedure TMainForm.aAddFolderExecute(Sender: TObject);
var ParentNode, Node: PVirtualNode;
  ParentData: PNdFolderData;
  Path, NewName: string;
  NewNameCtr: integer;
begin
  //Determine parent node and path
  ParentNode := vtFolders.FocusedNode;
  if ParentNode <> nil then begin
    ParentData := vtFolders.GetNodeData(ParentNode);
    if ParentData.NodeType <> ntFolder then exit; //cannot create subfolders for system nodes
    Path := ParentData.Path;
  end else
    Path := ServiceCatRootPath;
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
  Node := vtFolders_Add(ParentNode, Path+NewName);

  //Auto-start editing
  vtFolders.EditNode(Node, NoColumn);
end;

procedure TMainForm.aRenameFolderExecute(Sender: TObject);
var Node: PVirtualNode;
  Data: PNdFolderData;
begin
  Node := vtFolders.FocusedNode;
  if Node = nil then exit;
  Data := vtFolders.GetNodeData(Node);
  if Data.NodeType = ntFolder then
    vtFolders.EditNode(Node, NoColumn);
end;


end.
