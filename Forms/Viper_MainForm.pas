unit Viper_MainForm;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, VirtualTrees, Actions, ActnList, Vcl.ExtCtrls,
  ImgList, UiTypes, Generics.Collections, Vcl.Menus, ServiceHelper, Vcl.StdCtrls;

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

 //Running service description
  TServiceEntry = class
    ServiceName: string;
    DisplayName: string;
    Description: string;
    Status: SERVICE_STATUS;
    Config: LPQUERY_SERVICE_CONFIG;
    ServiceDll: string;
    Info: TServiceInfo;
    destructor Destroy; override;
    function GetEffectiveDisplayName: string;
    function GetExecutableFilename: string;
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
    aStartService: TAction;
    aStopService: TAction;
    aPauseService: TAction;
    aResumeService: TAction;
    aRestartService: TAction;
    pmServices: TPopupMenu;
    Start1: TMenuItem;
    Stop1: TMenuItem;
    Pause1: TMenuItem;
    Resume1: TMenuItem;
    Restart1: TMenuItem;
    N1: TMenuItem;
    Reload1: TMenuItem;
    N2: TMenuItem;
    miStartType: TMenuItem;
    aStartTypeAutomatic: TAction;
    aStartTypeManual: TAction;
    aStartTypeDisabled: TAction;
    Automatic1: TMenuItem;
    Manual1: TMenuItem;
    Disabled1: TMenuItem;
    aDeleteService: TAction;
    aExportService: TAction;
    Advanced1: TMenuItem;
    Exporttoreg1: TMenuItem;
    Deleteservice1: TMenuItem;
    aHideEmptyFolders: TAction;
    pmFolders: TPopupMenu;
    Hideemptyfolders1: TMenuItem;
    pnlMain: TPanel;
    vtServices: TVirtualStringTree;
    pnlDetails: TPanel;
    Splitter2: TSplitter;
    mmDetails: TMemo;
    aColorByStartType: TAction;
    Colorize1: TMenuItem;
    Bystarttype1: TMenuItem;
    File1: TMenuItem;
    Reload2: TMenuItem;
    aShowDrivers: TAction;
    Showdrivers1: TMenuItem;
    aColorByStatus: TAction;
    Bystatus1: TMenuItem;
    Copy1: TMenuItem;
    aCopyServiceName: TAction;
    aCopyServiceID: TAction;
    aCopyServiceDescription: TAction;
    Ident1: TMenuItem;
    Name1: TMenuItem;
    Description1: TMenuItem;
    aCopyServiceShortSummary: TAction;
    aRefresh: TAction;
    Refresh1: TMenuItem;
    aCopyExecutableFilename: TAction;
    Executablefilename1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure aReloadExecute(Sender: TObject);
    procedure vtServicesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtServicesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtServicesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtServicesHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure vtServicesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtFoldersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtFoldersGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtFoldersInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtFoldersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vtFoldersGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtServicesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vtFoldersFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure vtServicesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure aHideEmptyFoldersExecute(Sender: TObject);
    procedure vtServicesBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure vtServicesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure aColorByStartTypeExecute(Sender: TObject);
    procedure aCopyServiceIDExecute(Sender: TObject);
    procedure aCopyServiceNameExecute(Sender: TObject);
    procedure aCopyServiceDescriptionExecute(Sender: TObject);
    procedure vtServicesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure aCopyServiceShortSummaryExecute(Sender: TObject);
    procedure aStopServiceExecute(Sender: TObject);
    procedure aStartServiceExecute(Sender: TObject);
    procedure aPauseServiceExecute(Sender: TObject);
    procedure aResumeServiceExecute(Sender: TObject);
    procedure aRestartServiceExecute(Sender: TObject);
    procedure aRefreshExecute(Sender: TObject);
    procedure aStartTypeAutomaticExecute(Sender: TObject);
    procedure aStartTypeManualExecute(Sender: TObject);
    procedure aStartTypeDisabledExecute(Sender: TObject);
    procedure aCopyExecutableFilenameExecute(Sender: TObject);

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
    iFolder, iService: integer;
    FServices: TObjectList<TServiceEntry>;
    function GetFocusedService: TServiceEntry;
    procedure FilterServices(AFolder: PNdFolderData);
    procedure FilterServices_Callback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    function GetServiceFolders(Service: TServiceEntry): TServiceFolders;
    procedure GetServiceFolders_Callback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure InvalidateServiceNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);

  public
    procedure Reload;
    procedure RefreshService(Service: TServiceEntry);
    procedure RefreshAllServices;

  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, CommCtrl, ShellApi, Clipbrd, WinApiHelper;

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

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FServiceCat := TServiceCatalogue.Create({OwnsObjects=}true);
  FServices := TObjectList<TServiceEntry>.Create;
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

procedure TMainForm.aRefreshExecute(Sender: TObject);
begin
  RefreshAllServices;
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

destructor TServiceEntry.Destroy;
begin
  if Self.Config <> nil then begin
    FreeMem(Self.Config);
    Self.Config := nil;
  end;
  inherited;
end;

function TServiceEntry.GetEffectiveDisplayName: string;
begin
  if (Info <> nil) and (Info.DisplayName <> '') then
    Result := Info.DisplayName
  else
    Result := Self.DisplayName;
end;

function TServiceEntry.GetExecutableFilename: string;
begin
  if Self.ServiceDll <> '' then
    Result := Self.ServiceDll
  else
  if Config <> nil then
    Result := Config.lpBinaryPathName
  else
    Result := '';
end;

//Загружает список служб с их состояниями
procedure TMainForm.Reload;
var hSC: SC_HANDLE;
  ServiceTypes: dword;
  Services, S: PEnumServiceStatus;
  BytesNeeded,ServicesReturned,ResumeHandle: DWORD;
  i: integer;
  svc: TServiceEntry;
begin
  vtServices.Clear;
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
        svc := TServiceEntry.Create;
        svc.ServiceName := S^.lpServiceName;
        svc.DisplayName := S^.lpDisplayName;
        svc.Status := S^.ServiceStatus;
        svc.Config := QueryServiceConfig(hSC, S^.lpServiceName);
        svc.Description := QueryServiceDescription(hSC, S^.lpServiceName);
        if svc.Config <> nil then begin
          if (pos('svchost.exe', svc.Config.lpBinaryPathName)>0)
          or (pos('lsass.exe', svc.Config.lpBinaryPathName)>0) then
         //^ this is not a surefire way to test it's running svchost.exe, but we don't need one
         // It would be too complicated to check that it really references svchost, and the one
         // from the system dir and not an impostor.
         // We just optimize away unneccessary registry checks.
            svc.ServiceDll := ExpandEnvironmentStrings(QueryServiceServiceDll(svc.ServiceName));
        end;
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

  vtServices.RootNodeCount := FServices.Count;
  FilterFolders; //service list changed, re-test which folders are empty
  vtServices.SortTree(vtServices.Header.SortColumn, vtServices.Header.SortDirection); //re-apply sort
end;

//Reload data for a specified service and repaint the node
procedure TMainForm.RefreshService(Service: TServiceEntry);
var hSC, hSvc: SC_HANDLE;
begin
  hSvc := 0;
  hSC := OpenSCManager(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
  try
    hSvc := OpenService(hSC, Service.ServiceName, SERVICE_READ_ACCESS);
    if hSvc <> 0 then begin
      Service.Status := QueryServiceStatus(hSvc);
      Service.Config := QueryserviceConfig(hSvc);
    end;
  finally
    CloseServiceHandle(hSvc);
    CloseServiceHandle(hSC);
  end;

  vtServices.IterateSubtree(nil, InvalidateServiceNode, Service);
end;

procedure TMainForm.InvalidateServiceNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
begin
  if TServiceEntry(Sender.GetNodeData(Node)^) = Data then
    Sender.InvalidateNode(Node);
end;

procedure TMainForm.RefreshAllServices;
var service: TServiceEntry;
begin
  for service in FServices do
    RefreshService(service);
end;

procedure TMainForm.vtServicesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TObject);
end;

procedure TMainForm.vtServicesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  PObject(Sender.GetNodeData(Node))^ := FServices[Node.Index];
end;

resourcestring
  sStatusStopped = 'Остановлена';
  sStatusStartPending = 'Запускается...';
  sStatusStopPending = 'Завершается...';
  sStatusRunning = 'Выполняется';
  sStatusContinuePending = 'Возобновляется...';
  sStatusPausePending = 'Приостанавливается...';
  sStatusPaused = 'Приостановлена';
  sStatusOther = 'Неясно (%d)';

  sStartTypeAuto = 'Автоматически';
  sStartTypeDemand = 'Вручную';
  sStartTypeDisabled = 'Отключена';
  sStartTypeBoot = 'Авто (загрузка)';
  sStartTypeSystem = 'Авто (система)';

procedure TMainForm.vtServicesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var Data: TServiceEntry;
begin
  Data := TServiceEntry(Sender.GetNodeData(Node)^);
  if TextType <> ttNormal then exit;
  case Column of
    NoColumn, 0: CellText := Data.ServiceName;
    1: CellText := Data.GetEffectiveDisplayName;
    2: case Data.Status.dwCurrentState of
         SERVICE_STOPPED: CellText := '';
         SERVICE_START_PENDING: CellText := sStatusStartPending;
         SERVICE_STOP_PENDING: CellText := sStatusStopPending;
         SERVICE_RUNNING: CellText := sStatusRunning;
         SERVICE_CONTINUE_PENDING: CellText := sStatusContinuePending;
         SERVICE_PAUSE_PENDING: CellText := sStatusPausePending;
         SERVICE_PAUSED: CellText := sStatusPaused;
       else CellText := Format(sStatusOther, [Data.Status.dwCurrentState]);
       end;
    3: if Data.Config = nil then
         CellText := ''
       else
       case Data.Config.dwStartType of
         SERVICE_AUTO_START: CellText := sStartTypeAuto;
         SERVICE_DEMAND_START: CellText := sStartTypeDemand;
         SERVICE_DISABLED: CellText := sStartTypeDisabled;
         SERVICE_BOOT_START: CellText := sStartTypeBoot;
         SERVICE_SYSTEM_START: CellText := sStartTypeSystem;
       else CellText := '';
       end;
    4: CellText := Data.Description;
    5: CellText := Data.GetExecutableFilename;
  end;
end;

//Customize the background
procedure TMainForm.vtServicesBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var Data: TServiceEntry;
begin
  Data := TServiceEntry(Sender.GetNodeData(Node)^);

  if (Data.Config <> nil) and aColorByStartType.Checked then
    case Data.Config.dwStartType of
      SERVICE_BOOT_START,
      SERVICE_SYSTEM_START,
      SERVICE_AUTO_START: begin
        EraseAction := eaColor;
        ItemColor := $00FFF7DD;//$00DDF5FF;
      end;
    end;
end;

 //Customize the font
procedure TMainForm.vtServicesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var Data: TServiceEntry;
begin
  Data := TServiceEntry(Sender.GetNodeData(Node)^);

  if aColorByStatus.Checked then
    case Data.Status.dwCurrentState of
      SERVICE_STOPPED: begin end;
    else
      TargetCanvas.Font.Style := [fsBold];
    end;

  if (Data.Config <> nil) and aColorByStartType.Checked then
    case Data.Config.dwStartType of
      SERVICE_BOOT_START,
      SERVICE_SYSTEM_START,
      SERVICE_AUTO_START: begin end;
      SERVICE_DISABLED:
        TargetCanvas.Font.Color := $AAAAAA;
    end;
end;

procedure TMainForm.vtServicesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: TServiceEntry;
begin
  Data1 := TServiceEntry(Sender.GetNodeData(Node1)^);
  Data2 := TServiceEntry(Sender.GetNodeData(Node2)^);
  case Column of
    0, NoColumn: Result := CompareText(Data1.ServiceName, Data2.ServiceName);
    1: Result := CompareText(Data1.GetEffectiveDisplayName, Data2.GetEffectiveDisplayName);
    2: Result := Data2.Status.dwCurrentState - Data1.Status.dwCurrentState;
    3: if Data1.Config = nil then
         if Data2.Config = nil then
           Result := 0
         else
           Result := 1
       else
         if Data2.Config = nil then
           Result := -1
         else
           Result := Data1.Config.dwStartType - Data2.Config.dwStartType;
    4: Result := CompareText(Data1.Description, Data2.Description);
    5: Result := CompareText(Data1.GetExecutableFilename, Data2.GetExecutableFilename);
  end;
end;

procedure TMainForm.vtServicesHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if Sender.SortColumn <> HitInfo.Column then begin
    Sender.SortColumn := HitInfo.Column;
    Sender.SortDirection := sdAscending;
  end else
    if Sender.SortDirection = sdAscending then
      Sender.SortDirection := sdDescending
    else
    if Sender.SortDirection = sdDescending then
      Sender.SortColumn := NoColumn;
  if Sender.SortColumn <> NoColumn then
    Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection);
end;

procedure TMainForm.vtServicesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;
  case Column of
    NoColumn, 0: ImageIndex := iService;
  end;
end;

function TMainForm.GetFocusedService: TServiceEntry;
begin
  if vtServices.FocusedNode = nil then begin
    Result := nil;
    exit;
  end;

  Result := TServiceEntry(vtServices.GetNodeData(vtServices.FocusedNode)^);
end;

procedure TMainForm.FilterServices(AFolder: PNdFolderData);
begin
  vtServices.BeginUpdate;
  try
    vtServices.IterateSubtree(nil, FilterServices_Callback, AFolder, [], {DoInit=}true);
  finally
    vtServices.EndUpdate;
  end;
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

procedure TMainForm.vtServicesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var nd: TServiceEntry;
begin
  nd := TServiceEntry(Sender.GetNodeData(Node)^);

  aStartService.Visible := (nd <> nil) and (nd.Status.dwCurrentState = SERVICE_STOPPED);
  aStopService.Visible := (nd <> nil) and (nd.Status.dwCurrentState <> SERVICE_STOPPED)
    and (nd.Status.dwCurrentState <> SERVICE_STOP_PENDING);
  aPauseService.Visible := (nd <> nil) and (nd.Status.dwCurrentState = SERVICE_RUNNING);
  aResumeService.Visible := (nd <> nil) and (nd.Status.dwCurrentState = SERVICE_PAUSED);
  aRestartService.Visible := (nd <> nil) and ((nd.Status.dwCurrentState = SERVICE_RUNNING)
    or (nd.Status.dwCurrentState = SERVICE_PAUSED));

  aStartTypeAutomatic.Checked := (nd <> nil) and (nd.Config <> nil) and (nd.Config.dwStartType = SERVICE_AUTO_START);
  aStartTypeManual.Checked := (nd <> nil) and (nd.Config <> nil) and (nd.Config.dwStartType = SERVICE_DEMAND_START);
  aStartTypeDisabled.Checked := (nd <> nil) and (nd.Config <> nil) and (nd.Config.dwStartType = SERVICE_DISABLED);

  aStartTypeAutomatic.Visible := (nd <> nil) and (nd.Config <> nil);
  aStartTypeManual.Visible := (nd <> nil) and (nd.Config <> nil);
  aStartTypeDisabled.Visible := (nd <> nil) and (nd.Config <> nil);
  miStartType.Visible := aStartTypeAutomatic.Visible or aStartTypeManual.Visible or aStartTypeDisabled.Visible;

  mmDetails.Text := nd.Description;
end;

procedure TMainForm.vtServicesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and ((Key=Ord('C')) or (Key=Ord('c'))) then
    aCopyServiceShortSummary.Execute;
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

procedure TMainForm.aColorByStartTypeExecute(Sender: TObject);
begin
  vtServices.Invalidate;
end;

procedure TMainForm.aCopyServiceIDExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  Clipboard.Open;
  try
    Clipboard.Clear;
    Clipboard.AsText := Service.ServiceName;
  finally
    Clipboard.Close;
  end;
end;

procedure TMainForm.aCopyServiceNameExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  Clipboard.AsText := Service.DisplayName;
end;

procedure TMainForm.aCopyServiceShortSummaryExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  Clipboard.AsText := Service.ServiceName + ' (' + Service.DisplayName + ')';
end;

procedure TMainForm.aCopyServiceDescriptionExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  Clipboard.AsText := Service.Description;
end;

procedure TMainForm.aCopyExecutableFilenameExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  if Service.Config <> nil then
    Clipboard.AsText := Service.Config.lpBinaryPathName
  else
    Clipboard.AsText := '';
end;



procedure TMainForm.aStartServiceExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  StartService(Service.ServiceName);
  RefreshService(Service);
end;

procedure TMainForm.aStopServiceExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  StopService(Service.ServiceName);
  RefreshService(Service);
end;

procedure TMainForm.aPauseServiceExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  PauseService(Service.ServiceName);
  RefreshService(Service);
end;

procedure TMainForm.aResumeServiceExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  ContinueService(Service.ServiceName);
  RefreshService(Service);
end;

procedure TMainForm.aRestartServiceExecute(Sender: TObject);
var Service: TServiceEntry;
  hSC, hSvc: SC_HANDLE;
  tmStart: cardinal;
  stat: SERVICE_STATUS;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);

  hSC := OpenSCManager();
  try
    hSvc := OpenService(hSC, Service.ServiceName);
    if hSvc = 0 then RaiseLastOsError();

    stat := StopService(hSvc);
    tmStart := GetTickCount();
    while stat.dwCurrentState <> SERVICE_STOPPED do begin
      Sleep(50);
      stat := QueryServiceStatus(hSvc);
      if GetTickCount() - tmStart > 5000 then
        raise Exception.Create('Cannot stop service');
    end;
    //TODO: Move ^^ into "Form.WaitForStatusChange()"

    StartService(hSvc);

  finally
    CloseServiceHandle(hSC);
  end;

  Reload();
end;


procedure TMainForm.aStartTypeAutomaticExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  ChangeServiceStartType(Service.ServiceName, SERVICE_AUTO_START);
  RefreshService(Service);
end;

procedure TMainForm.aStartTypeManualExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  ChangeServiceStartType(Service.ServiceName, SERVICE_DEMAND_START);
  RefreshService(Service);
end;

procedure TMainForm.aStartTypeDisabledExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFocusedService();
  Assert(Service <> nil);
  ChangeServiceStartType(Service.ServiceName, SERVICE_DISABLED);
  RefreshService(Service);
end;


end.
