unit Viper_MainForm;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, VirtualTrees, Actions, ActnList, ExtCtrls,
  ImgList, UiTypes, Generics.Collections, Menus, ServiceHelper, StdCtrls, ComCtrls,
  SvcEntry, Viper_ServiceList;

type
 //Service information loaded from Catalogue
  TServiceInfo = class
  public
    ServiceName: string;
    DisplayName: string;
    Description: string;
  end;

  TServiceCatalogue = class(TObjectList<TServiceInfo>)
  public
    function Find(const AServiceName: string): TServiceInfo;
  end;


  TFolderNodeType = (
    ntFolder,
    ntRunningServices,
    ntAllServices,
    ntUnknownServices
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
    tsRequiredBy: TTabSheet;
    tsOperations: TTabSheet;
    MainServiceList: TServiceList;
    N1: TMenuItem;
    Refresh2: TMenuItem;
    DependencyList: TServiceList;
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

  protected
    function LoadIcon(const ALibName: string; AResId: integer): integer;

  protected
    FServiceCat: TServiceCatalogue; //catalogue of static service information
    function vtFolders_Add(AParent: PVirtualNode; AFolderPath: string): PVirtualNode;
    function vtFolders_AddSpecial(AType: TFolderNodeType; ATitle: string): PVirtualNode;
    procedure FilterFolders();
    procedure FilterFolders_Callback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    function LoadServiceInfo(const AFilename: string): TServiceInfo;
  public
    procedure ReloadServiceTree;
    procedure LoadServiceFolder(AParentNode: PVirtualNode; AFolderPath: string);

  protected
    FServices: TServiceEntryList;
    iFolder, iService: integer;
    procedure FilterServices(AFolder: PNdFolderData);
    procedure FilterServices_Callback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    function GetServiceFolders(Service: TServiceEntry): TServiceFolders;
    procedure GetServiceFolders_Callback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure ReloadServiceDependencies;
  public
    procedure Reload;
    procedure RefreshAllServices;


  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, CommCtrl, ShellApi, Clipbrd, WinApiHelper, ShellUtils;

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
    Result := Info.DisplayName
  else
    Result := inherited;
end;

procedure TExtServiceEntry.GetIcon(out AImageList: TCustomImageList; out AIndex: integer);
begin
 //TODO: Can make a common service imagelist
  AImageList := MainForm.ilImages;
  AIndex := MainForm.iService;
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
  iFolder := LoadIcon('shell32.dll', 4); //Иконка папки из эксплорера
  iService := LoadIcon('filemgmt.dll', 0); //Иконка службы из services.msc
  ReloadServiceTree;
  Reload;
end;

procedure TMainForm.aReloadExecute(Sender: TObject);
begin
  Reload;
end;

function TMainForm.LoadIcon(const ALibName: string; AResId: integer): integer;
var res: integer;
  hSmallIcon, hLargeIcon: HICON;
  icon: TIcon;
begin
  res := ExtractIconEx(PChar(ALibName), AResId, hLargeIcon, hSmallIcon, 1);
  if (res=1) or (res=2) then begin
    DestroyIcon(hLargeIcon);
  end else
    RaiseLastOsError();

 //Загружаем хитрым образом, чтобы сохранить прозрачность. Все более простые
 //ведут к "битым краям".
  icon := TIcon.Create;
  try
    icon.Handle := hSmallIcon;
    icon.Transparent := true;
   //ImageList's ColorDepth has to be 32 bit, DrawingStyle transparent.
    Result := ilImages.AddIcon(icon);
  finally
    FreeAndNil(icon);
  end;
end;

//Загружает список служб с их состояниями
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

  ServiceTypes := SERVICE_WIN32;
  if aShowDrivers.Checked then
    ServiceTypes := ServiceTypes or SERVICE_DRIVER;

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

  MainServiceList.BeginUpdate;
  try
    MainServiceList.Clear;
    for i := 0 to FServices.Count-1 do
      MainServiceList.AddService(nil, FServices[i]);
    FilterFolders; //service list changed, re-test which folders are empty
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


procedure TMainForm.FilterServices(AFolder: PNdFolderData);
begin
  MainServiceList.ApplyFilter(FilterServices_Callback, AFolder);
end;

procedure TMainForm.FilterServices_Callback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var nd: TServiceEntry;
begin
  if Data=nil then begin
    Sender.IsVisible[Node] := true;
    exit;
  end;

  nd := TServiceEntry(Sender.GetNodeData(Node)^);
  Assert(nd <> nil);

  case PNdFolderData(Data).NodeType of
    ntFolder: Sender.IsVisible[Node] := PNdFolderData(Data).ContainsService(nd.ServiceName);
    ntRunningServices: Sender.IsVisible[Node] := (nd.Status.dwCurrentState <> SERVICE_STOPPED);
    ntAllServices: Sender.IsVisible[Node] := true;
    ntUnknownServices: Sender.IsVisible[Node] := Length(GetServiceFolders(nd)) <= 0;
  end;
end;




resourcestring
  sFolderUnknownServices = 'Необычные';
  sFolderRunningServices = 'Работающие';
  sFolderAllServices = 'Все';

//Перезагружает структуру папок со службами
procedure TMainForm.ReloadServiceTree;
begin
  vtFolders.BeginUpdate;
  try
    FServiceCat.Clear;
    vtFolders.Clear;
    LoadServiceFolder(nil, AppFolder+'\SvcData');
    vtFolders_AddSpecial(ntUnknownServices, sFolderUnknownServices);
    vtFolders_AddSpecial(ntRunningServices, sFolderRunningServices);
    vtFolders_AddSpecial(ntAllServices, sFolderAllServices);
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
    if (sl.Count > 0) and (sl[0] <> '') and (Result.DisplayName = '') then begin
      Result.DisplayName := sl[0]; //TODO: This would better work per-folder
      sl.Delete(0);
    end;
    if Result.Description <> '' then
      Result.Description := Result.Description + #13;
    Result.Description := Result.Description + sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TMainForm.MainServiceListvtServicesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var nd: TServiceEntry;
begin
  MainServiceList.vtServicesFocusChanged(Sender, Node, Column);

  nd := TServiceEntry(Sender.GetNodeData(Node)^);
 //Most availability checking is done in OnChanged, depends on Selection

  mmDetails.Text := nd.Description;
  if pcBottom.ActivePage = tsDependencies then
    ReloadServiceDependencies;
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

function TMainForm.vtFolders_AddSpecial(AType: TFolderNodeType; ATitle: string): PVirtualNode;
var Data: PNdFolderData;
begin
  Result := vtFolders.AddChild(nil);
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
  ImageIndex := iFolder;
end;

procedure TMainForm.vtFoldersFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var nd: PNdFolderData;
begin
  if Node = nil then
    FilterServices(nil)
  else begin
    nd := PNdFolderData(Sender.GetNodeData(Node));
    FilterServices(nd);
  end;
end;

procedure TMainForm.aHideEmptyFoldersExecute(Sender: TObject);
begin
  FilterFolders;
end;

procedure TMainForm.FilterFolders();
begin
  vtFolders.BeginUpdate;
  try
    vtFolders.IterateSubtree(nil, FilterFolders_Callback, nil, [], {DoInit=}true);
  finally
    vtFolders.EndUpdate;
  end;
end;

procedure TMainForm.FilterFolders_Callback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var nd: PNdFolderData;
  HasAnyServices: boolean;
  service: TServiceEntry;
begin
  if Node=nil then begin
    Sender.IsVisible[Node] := true;
    exit;
  end;

  nd := PNdFolderData(Sender.GetNodeData(Node));
  Assert(nd <> nil);
  if nd.NodeType <> ntFolder then begin
    Sender.IsVisible[Node] := true;
    exit;
  end;

  //This can be moved to service/folder list reloading and HasAnyServices flag cached
  HasAnyServices := false;
  for service in Self.FServices do
    if nd.ContainsService(service.ServiceName) then begin
      HasAnyServices := true;
      break;
    end;

  Sender.IsVisible[Node] := (not cbHideEmptyFolders.Checked) or HasAnyServices;
end;


type
  TGetServiceFoldersData = record
    Service: TServiceEntry;
    List: TServiceFolders;
  end;
  PGetServiceFoldersData = ^TGetServiceFoldersData;

//Возвращает список всех папок, в которые включена служба
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
  hSC, hSvc: SC_HANDLE;
  depcy: LPENUM_SERVICE_STATUS;
  depcyCount: cardinal;
begin
  service := MainServiceList.GetFocusedService;
  if service = nil then exit;

  hSC := 0;
  hSvc := 0;
  DependencyList.BeginUpdate;
  try
    DependencyList.Clear;
    hSC := OpenSCManager(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
    hSvc := OpenService(hSC, service.ServiceName, SERVICE_READ_ACCESS);
    depcy := EnumDependentServices(hSvc, SERVICE_STATE_ALL, depcyCount);
    if depcy = nil then exit;

    while depcyCount > 0 do begin
      service := FServices.Find(depcy.lpServiceName);
      if service = nil then
        raise Exception.Create('Service '+string(depcy.lpServiceName)+' not found in a general list.');
      DependencyList.AddService(nil, service);
      Inc(depcy);
      Dec(depcyCount);
    end;

  finally
    if hSvc <> 0 then CloseServiceHandle(hSvc);
    if hSC <> 0 then CloseServiceHandle(hSC);
    DependencyList.EndUpdate;
  end;
end;

end.
