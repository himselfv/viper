unit Viper_MainForm;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, VirtualTrees, Actions, ActnList, ExtCtrls,
  ImgList, UiTypes, Generics.Collections, Menus, ServiceHelper, StdCtrls, ComCtrls,
  SvcEntry, Viper_ServiceList, Viper_TriggerList;

type
 //Service information loaded from Catalogue
  TServiceFlag = (sfCritical, sfTelemetry);
  TServiceFlags = set of TServiceFlag;
  TServiceInfo = class
  public
    ServiceName: string;
    DisplayName: string;
    Description: string;
    CriticalText: string;
    Flags: TServiceFlags;
  end;

  TServiceCatalogue = class(TObjectList<TServiceInfo>)
  public
    function Find(const AServiceName: string): TServiceInfo;
  end;


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
    FolderName: string;
    Description: string;
    Services: array of string;
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
    aReload: TAction;
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
    Reload2: TMenuItem;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure aReloadExecute(Sender: TObject);
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
    procedure aHideEmptyFoldersExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure MainServiceListvtServicesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure tsDependenciesShow(Sender: TObject);
    procedure tsDependentsShow(Sender: TObject);
    procedure tsTriggersShow(Sender: TObject);
    procedure aIncludeSubfoldersExecute(Sender: TObject);
    procedure aCloseExecute(Sender: TObject);
    procedure aSaveAllServicesConfigExecute(Sender: TObject);
    procedure aSaveSelectedServicesConfigExecute(Sender: TObject);
    procedure aRestoreServiceConfigExecute(Sender: TObject);
    procedure miShowLogClick(Sender: TObject);
    procedure edtQuickFilterChange(Sender: TObject);

  protected
    FServiceCat: TServiceCatalogue; //catalogue of static service information
    function vtFolders_Add(AParent: PVirtualNode; AFolderPath: string): PVirtualNode;
    function vtFolders_AddSpecial(AParent: PVirtualNode; AType: TFolderNodeType; ATitle: string): PVirtualNode;
    procedure FilterFolders();
    procedure Folders_HideAll(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure Folders_ShowFiltered(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure NodeMarkVisible(Sender: TBaseVirtualTree; Node: PVirtualNode);
    function LoadServiceInfo(const AFilename: string): TServiceInfo;
  public
    procedure ReloadServiceTree;
    procedure LoadServiceFolder(AParentNode: PVirtualNode; AFolderPath: string);

  protected
    FServices: TServiceEntryList;
    function AllServiceEntries(const ATypeFilter: cardinal = SERVICE_WIN32): TServiceEntries;
    procedure FilterServices(); overload;
    procedure FilterServices(AFolder: PVirtualNode); overload;
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
    procedure Reload;
    procedure RefreshAllServices;

  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, CommCtrl, ShellApi, Clipbrd, WinApiHelper, ShellUtils,
  CommonResources, Viper_RestoreServiceConfig, Viper.Log;

{$R *.dfm}

function TServiceCatalogue.Find(const AServiceName: string): TServiceInfo;
var i: integer;
begin
  Result := nil;
  for i := 0 to Self.Count-1 do
    if SameText(Self[i].ServiceName, AServiceName) then begin
      Result := Self[i];
      break;
    end;
end;

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


procedure TMainForm.FormCreate(Sender: TObject);
begin
  FServiceCat := TServiceCatalogue.Create({OwnsObjects=}true);
  FServices := TServiceEntryList.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FServices);
  FreeAndNil(FServiceCat);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  pcBottom.ActivePage := tsDescription;
  ReloadServiceTree;
  Reload;
end;

procedure TMainForm.aCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.aReloadExecute(Sender: TObject);
begin
  Reload;
end;

//Loads the list of services with their status
procedure TMainForm.Reload;
var hSC: SC_HANDLE;
  ServiceTypes: dword;
  Services, S: PEnumServiceStatus;
  BytesNeeded,ServicesReturned,ResumeHandle: DWORD;
  i: integer;
  svc: TExtServiceEntry;
begin
  MainServiceList.Clear;
  FServices.Clear;

 //Let's load all services since we need all for dependencies, just make sure to handle
 //permission denials well.
  ServiceTypes := SERVICE_TYPE_ALL;

  hSC := OpenSCManager(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
  try
    Services := nil;
    ServicesReturned := 0;
    ResumeHandle := 0;
    if EnumServicesStatus(hSC, ServiceTypes, SERVICE_STATE_ALL,
       Services^,0, BytesNeeded,ServicesReturned,ResumeHandle) then Exit; //no services
    if GetLastError <> ERROR_MORE_DATA then RaiseLastOSError;
    GetMem(Services,BytesNeeded);
    try
      ServicesReturned := 0;
      ResumeHandle := 0;
      if not EnumServicesStatus(hSC, ServiceTypes, SERVICE_STATE_ALL,
        Services^,BytesNeeded,BytesNeeded,ServicesReturned,ResumeHandle) then
        RaiseLastOsError;
      S := Services;
      for i := 0 to ServicesReturned - 1 do begin
        svc := TExtServiceEntry.CreateFromEnum(hSC, S);
        svc.Info := FServiceCat.Find(svc.ServiceName);
        FServices.Add(svc);
        Inc(S);
      end;
    finally
      FreeMem(Services);
    end;

  finally
    CloseServiceHandle(hSC);
  end;

  FilterFolders; //service list changed, re-test which folders are empty

  MainServiceList.BeginUpdate;
  try
    MainServiceList.Clear;
    for i := 0 to FServices.Count-1 do
      MainServiceList.AddService(nil, FServices[i]);
    FilterServices();
  finally
    MainServiceList.EndUpdate;
  end;
end;


procedure TMainForm.RefreshAllServices;
var service: TServiceEntry;
begin
  for service in FServices do
    RefreshService(service);
end;


procedure TMainForm.aRefreshExecute(Sender: TObject);
begin
  Self.RefreshAllServices;
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
  FilterServices(vtFolders.FocusedNode);
end;

procedure TMainForm.FilterServices(AFolder: PVirtualNode);
begin
  MainServiceList.ApplyFilter(FilterServices_Callback, AFolder);
end;

procedure TMainForm.FilterServices_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var folderNode: PVirtualNode absolute Data;
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
    LoadServiceFolder(nil, AppFolder+'\SvcData');
    vtFolders_AddSpecial(nil, ntUnknownServices, sFolderUnknownServices);
    vtFolders_AddSpecial(nil, ntRunningServices, sFolderRunningServices);
    vtFolders_AddSpecial(nil, ntAllServices, sFolderAllServices);
    section := vtFolders_AddSpecial(nil, ntAllDrivers, sFolderAllDrivers);
    vtFolders_AddSpecial(section, ntRunningDrivers, sFolderRunningDrivers);
  finally
    vtFolders.EndUpdate;
  end;
end;

procedure TMainForm.LoadServiceFolder(AParentNode: PVirtualNode; AFolderPath: string);
var res: integer;
  sr: TSearchRec;
  node: PVirtualNode;
  nd: PNdFolderData;
begin
  if AParentNode = nil then
    nd := nil
  else begin
    nd := PNdFolderData(vtFolders.GetNodeData(AParentNode));
    try
      nd.LoadDescription(AFolderPath+'\desc');
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
      node := vtFolders_Add(AParentNode, AFolderPath+'\'+sr.Name);
      LoadServiceFolder(node, AFolderPath+'\'+sr.Name);
    end else
    if ExtractFileExt(sr.Name) = '.txt' then begin
      LoadServiceInfo(AFolderPath + '\' + sr.Name);
      if nd <> nil then begin
        SetLength(nd.Services, Length(nd.Services)+1);
        nd.Services[Length(nd.Services)-1] := ChangeFileExt(sr.Name, '');
      end;
    end else
    if ExtractFileExt(sr.Name) = '.lnk' then begin
      if nd <> nil then begin
        SetLength(nd.Services, Length(nd.Services)+1);
        nd.Services[Length(nd.Services)-1] := ChangeFileExt(sr.Name, '');
      end;
    end;

    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;

//Reads static service information file and registers a service in a catalogue (or extends it)
function TMainForm.LoadServiceInfo(const AFilename: string): TServiceInfo;
var ServiceName: string;
  sl: TStringList;
  ln: string;
  i: integer;
begin
  ServiceName := ChangeFileExt(ExtractFilename(AFilename), '');
  Result := FServiceCat.Find(ServiceName);
  if Result = nil then begin
    Result := TServiceInfo.Create;
    Result.ServiceName := ServiceName;
    FServiceCat.Add(Result);
  end;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    i := sl.Count;
    while i > 0 do begin
      Dec(i);
      ln := Trim(sl[i]);
      if (ln='') or (ln[1]='#') then begin
        sl.Delete(i);
        continue;
      end;
      if ln.StartsWith('TITLE:') then begin
        Result.DisplayName := Trim(Copy(ln, 7, MaxInt));
        sl.Delete(i);
        continue;
      end;
      if ln.StartsWith('CRITICAL') then begin
        Result.Flags := Result.Flags + [sfCritical];
        Result.CriticalText := Result.CriticalText + Trim(Copy(ln, 10, MaxInt)) + #13;
        sl.Delete(i);
        continue;
      end;
      if ln.StartsWith('TELEMETRY') then begin
        Result.Flags := Result.Flags + [sfTelemetry];
        sl.Delete(i);
        continue;
      end;
    end;
    Result.Description := Result.Description + Trim(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;

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

function TMainForm.vtFolders_Add(AParent: PVirtualNode; AFolderPath: string): PVirtualNode;
var Data: PNdFolderData;
begin
  Result := vtFolders.AddChild(AParent);
  vtFolders.ValidateNode(Result, false);
  Data := vtFolders.GetNodeData(Result);
  Data.NodeType := ntFolder;
  Data.FolderName := ExtractFilename(AFolderPath);
end;

function TMainForm.vtFolders_AddSpecial(AParent: PVirtualNode; AType: TFolderNodeType; ATitle: string): PVirtualNode;
var Data: PNdFolderData;
begin
  Result := vtFolders.AddChild(AParent, nil);
  vtFolders.ValidateNode(Result, false);
  Data := vtFolders.GetNodeData(Result);
  Data.NodeType := AType;
  Data.FolderName := ATitle;
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
    NoColumn, 0: CellText := Data.FolderName;
  end;
end;

procedure TMainForm.vtFoldersGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  ImageIndex := CommonRes.iFolder;
end;

procedure TMainForm.vtFoldersFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  FilterServices(Node);
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

  nd := TExtServiceEntry(Sender.GetNodeData(Node)^);
 //Most availability checking is done in OnChanged, depends on Selection

  mmDetails.Text := nd.Description;

  if nd.Info <> nil then
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
    Reload(); //all configurations could've changed
end;

end.
