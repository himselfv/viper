unit Viper.MainForm;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Actions, ActnList, ExtCtrls, ImgList, UiTypes, Menus, StdCtrls, ComCtrls, ActiveX, VirtualTrees,
  Generics.Collections, ServiceHelper, SvcEntry, SvcCat, Viper.ServiceList, Viper.TriggerList,
  Viper.RichEditEx;

type
  TFolderNodeType = (
    //The order is important, folders are sorted by it
    ntNone = 0,               //no folder type OR information is assigned
    ntRunningServices = 1,
    ntUnknownServices,
    ntAllServices,
    ntRunningDrivers,
    ntAllDrivers,
    ntMax = 255               //represents all the other values when cast to TFolderNodeType
  );

  //Folder node data contains only a single pointer.
  //If it's < 32 then it's TFolderNodeType, otherwise it's a TServiceFolder.
  TNdFolderData = TServiceFolder;
  PNdFolderData = ^TNdFolderData;

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
    miHideEmptyFolders: TMenuItem;
    pnlMain: TPanel;
    Splitter2: TSplitter;
    File1: TMenuItem;
    aShowDrivers: TAction;
    Showdrivers1: TMenuItem;
    Bystatus1: TMenuItem;
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
    aRestartAsAdmin: TAction;
    Restartasadministrator1: TMenuItem;
    N4: TMenuItem;
    Alltriggers1: TMenuItem;
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
    Configurecolors1: TMenuItem;
    N7: TMenuItem;
    miEdit: TMenuItem;
    Editfolders1: TMenuItem;
    Editnotes1: TMenuItem;
    aShowUserPrototypes: TAction;
    Showprototypes1: TMenuItem;
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
    procedure Alltriggers1Click(Sender: TObject);
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
    function IsFolderContainsService(AFolder: PVirtualNode; AService: TServiceEntry; ARecursive: boolean = false): boolean;
    procedure IsFolderContainsService_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);

  protected //Details pane
    procedure ReloadDetails;
    procedure ReloadServiceInfo;
    procedure ReloadServiceDependencies;
    procedure LoadServiceDependencyNodes(AService: TServiceEntry; AParentNode: PVirtualNode);
    procedure ReloadServiceDependents;
    procedure LoadServiceDependentsNodes(hSC: SC_HANDLE; AService: TServiceEntry; AParentNode: PVirtualNode);
    procedure ReloadTriggers;
    function mmNotes: TRichEdit; inline;

  protected //Service editing
    function CanEditServiceInfo: boolean;
    procedure SaveNotes;

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
  CommonResources, Viper.RestoreServiceConfig, Viper.Log, TriggerUtils, Viper.TriggerBrowser,
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
  AIndex := CommonRes.iService;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  FServiceCat := TServiceCatalogue.Create();
  FServices := TServiceEntryList.Create;
  WellKnownDeviceInterfaceClasses.LoadFromFile(AppFolder()+'\DeviceInterfaceClasses.txt');
  WellKnownRpcInterfaces.LoadFromFile(AppFolder()+'\RpcInterfaces.txt');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FServices);
  FreeAndNil(FServiceCat);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //Load settings
  SettingsForm.LoadSettings;
  StyleSettingsForm.LoadStyles;

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

procedure TMainForm.aConfigureColorsExecute(Sender: TObject);
begin
  StyleSettingsForm.ShowModal;
  MainServiceList.Invalidate;
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
  i, j, err: integer;
  svc: TExtServiceEntry;
  ServiceFound: array of boolean;
  Node: PVirtualNode;
  Abort: boolean;
begin
  //Let's load all services since we need all for dependencies, just make sure to handle
  //permission denials well.
  ServiceTypes := SERVICE_TYPE_ALL_W10;

  //Mark already known services for sweeping
  SetLength(ServiceFound, FServices.Count);
  for i := 0 to Length(ServiceFound)-1 do
    ServiceFound[i] := false;

  //Actualize the list of services
  Services := nil;
  hSC := OpenSCManager(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
  try
    if not EnumServicesStatus(hSC, ServiceTypes, SERVICE_STATE_ALL, Services, ServicesReturned) then begin
      err := GetLastError;
      if err <> ERROR_INVALID_PARAMETER then
        RaiseLastOsError(err);

     //Windows <= W7 will return error when asked for W10 service types, so try again
      ServiceTypes := SERVICE_TYPE_ALL_W7;
      if not EnumServicesStatus(hSC, ServiceTypes, SERVICE_STATE_ALL, Services, ServicesReturned) then
        RaiseLastOsError();
    end;

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
  svc: TExtServiceEntry;
  isService: boolean;
  isVisible: boolean;
  filterText: string;
begin
  svc := TExtServiceEntry(Sender.GetNodeData(Node)^);
  Assert(svc <> nil);

  isService := (svc.Status.dwServiceType and SERVICE_WIN32 <> 0);
  isVisible := true;

  //Filter by folder
  folderNode := vtFolders.GetFirstSelected();
  if folderNode <> nil then begin
    folderData := GetFolderData(folderNode);
    case SpecialFolderType(folderData) of
      ntNone: isVisible := false;
      ntRunningServices: isVisible := isService and (svc.Status.dwCurrentState <> SERVICE_STOPPED);
      ntAllServices: isVisible :=  isService;
      ntUnknownServices: isVisible := isService and ((svc.Info = nil) or (Length(svc.Info.Folders) <= 0));
      ntRunningDrivers: isVisible := (not isService) and (svc.Status.dwCurrentState <> SERVICE_STOPPED);
      ntAllDrivers: isVisible := not isService;
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
end;

procedure TMainForm.edtQuickFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    edtQuickfilter.Text := '';
    edtQuickFilterChange(edtQuickfilter);
  end;

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
    FServiceCat.Load(AppFolder+'\SvcData');
    vtFolders.Clear;
    CreateFolderNodes(nil, nil);
    vtFolders_AddSpecial(nil, ntUnknownServices);
    vtFolders_AddSpecial(nil, ntRunningServices);
    vtFolders_AddSpecial(nil, ntAllServices);
    section := vtFolders_AddSpecial(nil, ntAllDrivers);
    vtFolders_AddSpecial(section, ntRunningDrivers);
  finally
    vtFolders.EndUpdate;
  end;
end;

procedure TMainForm.CreateFolderNodes(AFolderNode: PVirtualNode; AFolderInfo: TServiceFolder);
var ASubFolderInfo: TServiceFolder;
  ASubNode: PVirtualNode;
begin
  for ASubFolderInfo in FServiceCat.Folders do begin
    if ASubFolderInfo.Parent <> Self.GetFolderData(AFolderNode) then continue;
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
  Result := NativeUInt(AFolder) < 32;
end;

function TMainForm.SpecialFolderType(AFolder: TNdFolderData): TFolderNodeType;
begin
  //We can't just typecast to TFolderNodeType as this will only look at the lowest byte
  //But we also have to have a result for non-TFolderNodeType values, so I'm creating ntMax for that
  if (NativeUInt(AFolder) > NativeUInt(ntMax)) then
    Result := ntMax
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
        ntRunningServices: CellText := sFolderRunningServices;
        ntUnknownServices: CellText := sFolderUnknownServices;
        ntAllServices: CellText := sFolderAllServices;
        ntRunningDrivers: CellText := sFolderRunningDrivers;
        ntAllDrivers: CellText := sFolderAllDrivers;
      else CellText := Data.Name;
      end;
  end;
end;

procedure TMainForm.vtFoldersGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  ImageIndex := CommonRes.iFolder;
end;

procedure TMainForm.vtFoldersCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var Data1, Data2: TNdFolderData;
begin
  Data1 := GetFolderData(Node1);
  Data2 := GetFolderData(Node2);

  if IsSpecialFolder(Data1) and IsSpecialFolder(Data2) then
    Result := NativeUInt(Data1) - NativeUInt(Data2)
  else
  if IsSpecialFolder(Data1) then
    Result := +1
  else
  if IsSpecialFolder(Data2) then
    Result := -1
  else
    Result := CompareText(Data1.Name, Data2.Name);
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


procedure TMainForm.vtFoldersChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  //Selection changed
  FilterServices;
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
  ReloadDetails;
  //Action availability checking is done in OnChanged, depends on Selection
end;

procedure TMainForm.MainServiceListvtServicesDragAllowed(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := CanEditFolders;
end;



// Details pane
// At the moment, always shows the details for MainServiceList.FocusedService

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
  service := TExtServiceEntry(MainServiceList.GetFocusedService);

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
begin
  TriggerList.Clear;
  service := MainServiceList.GetFocusedService;
  if service = nil then
    exit;
  TriggerList.Reload(service.Handle);
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

procedure TMainForm.Alltriggers1Click(Sender: TObject);
begin
  TriggerBrowserForm.Services := Self.FServices;
  TriggerBrowserForm.Show;
end;

procedure TMainForm.aShowDriversExecute(Sender: TObject);
begin
  FilterFolders;
  FilterServices;
end;

procedure TMainForm.aShowUserPrototypesExecute(Sender: TObject);
begin
  FilterServices;
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
  if aEditServiceNotes.Checked and (MainServiceList.GetFocusedService = nil) then begin
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
  if mmNotes.Focused and not aEditServiceNotes.Checked then
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

  service := TExtServiceEntry(MainServiceList.GetFocusedService);
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
var service: TExtServiceEntry;
begin
  Result := inherited; //initialize the control
  if Result then begin
    service := TExtServiceEntry(Tree.GetNodeData(Node)^);
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

  if service.Info = nil then
    service.Info := FServiceCat.Get(service.ServiceName);

  if SameStr(service.Info.DisplayName, NewText) then exit;

  service.Info.DisplayName := NewText;
  service.Info.SaveToMainFile;
  //We could also save this later in OnEdited, but there would be no way to check if DisplayName has really changed
end;

end.
