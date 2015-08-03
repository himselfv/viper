unit Viper_MainForm;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, VirtualTrees, Actions, ActnList, Vcl.ExtCtrls,
  Vcl.ImgList;

type
  TNdServiceData = record
    ServiceName: string;
    DisplayName: string;
    Description: string;
    ServiceStatus: SERVICE_STATUS;
  end;
  PNdServiceData = ^TNdServiceData;

  TFolderNodeType = (
    ntFolder,
    ntRunningServices,
    ntAllServices
  );

  TNdFolderData = record
    NodeType: TFolderNodeType;
    FolderName: string;
    Services: array of string;
  end;
  PNdFolderData = ^TNdFolderData;

  TMainForm = class(TForm)
    vtServices: TVirtualStringTree;
    ActionList: TActionList;
    aReload: TAction;
    Splitter1: TSplitter;
    vtFolders: TVirtualStringTree;
    ilImages: TImageList;
    procedure FormShow(Sender: TObject);
    procedure aReloadExecute(Sender: TObject);
    procedure vtServicesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtServicesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
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
    procedure vtFoldersGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
  protected
    iFolder, iService: integer;
    function vtServices_Add(AServiceName, ADisplayName, ADescription: string;
      AServiceStatus: SERVICE_STATUS): PVirtualNode;
    function vtFolders_Add(AParent: PVirtualNode; AFolderPath: string): PVirtualNode;
    function vtFolders_AddSpecial(AType: TFolderNodeType; ATitle: string): PVirtualNode;
    function LoadIcon(const ALibName: string; AResId: integer): integer;
  public
    procedure Reload;
    procedure LoadServiceTree;
    procedure LoadServiceFolder(AParentNode: PVirtualNode; AFolderPath: string);
  end;

var
  MainForm: TMainForm;

implementation
uses FilenameUtils, CommCtrl, ShellApi;

{$R *.dfm}

procedure TMainForm.FormShow(Sender: TObject);
begin
  iFolder := LoadIcon('shell32.dll', 4);
//  iService := LoadIcon();
  LoadServiceTree;
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
  bmp: TBitmap;
begin
  res := ExtractIconEx(PChar(ALibName), AResId, hLargeIcon, hSmallIcon, 1);
  if (res=1) or (res=2) then begin
    DestroyIcon(hLargeIcon);
  end else
    RaiseLastOsError();

 //��������� ������ �������, ����� ��������� ������������. ��� ����� �������
 //����� � "����� �����".
  bmp := nil;
  icon := TIcon.Create;
  try
    icon.Handle := hSmallIcon;
    bmp := TBitmap.Create;
    bmp.Assign(icon);
    bmp.Transparent := false; //disable "mask transparency", use alpha
    Result := ilImages.Add(bmp, nil);
  finally
    FreeAndNil(icon);
    FreeAndNil(bmp);
  end;
end;

//��������� ������ ����� � �� �����������
procedure TMainForm.Reload;
var hSC: SC_HANDLE;
  Services, S: PEnumServiceStatus;
  BytesNeeded,ServicesReturned,ResumeHandle: DWORD;
  i: integer;
begin
  vtServices.Clear;
  hSC := OpenSCManager(nil, nil, SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
  if hSC = 0 then
    RaiseLastOsError;
  try
    Services := nil;
    ServicesReturned := 0;
    ResumeHandle := 0;
    if EnumServicesStatus(hSC, SERVICE_WIN32, SERVICE_ACTIVE or SERVICE_INACTIVE,
       Services^,0, BytesNeeded,ServicesReturned,ResumeHandle) then Exit; //no services
    if GetLastError <> ERROR_MORE_DATA then RaiseLastOSError;
    GetMem(Services,BytesNeeded);
    try
      ServicesReturned := 0;
      ResumeHandle := 0;
      if not EnumServicesStatus(hSC, SERVICE_WIN32, SERVICE_ACTIVE or SERVICE_INACTIVE,
        Services^,BytesNeeded,BytesNeeded,ServicesReturned,ResumeHandle) then
        RaiseLastOsError;
      S := Services;
      for i := 0 to ServicesReturned - 1 do begin
        vtServices_Add(S^.lpServiceName, S^.lpDisplayName, '', S^.ServiceStatus);
        Inc(S);
      end;
    finally
      FreeMem(Services);
    end;

  finally
    CloseServiceHandle(hSC);
  end;
end;

function TMainForm.vtServices_Add(AServiceName, ADisplayName, ADescription: string;
  AServiceStatus: SERVICE_STATUS): PVirtualNode;
var Data: PNdServiceData;
begin
  Result := vtServices.AddChild(nil);
  vtServices.ReinitNode(Result, false);
  Data := vtServices.GetNodeData(Result);
  Data.ServiceName := AServiceName;
  Data.DisplayName := ADisplayName;
  Data.Description := ADescription;
  Data.ServiceStatus := AServiceStatus;
end;

procedure TMainForm.vtServicesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNdServiceData);
end;

procedure TMainForm.vtServicesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var Data: PNdServiceData;
begin
  Data := Sender.GetNodeData(Node);
  Initialize(Data^);
end;

procedure TMainForm.vtServicesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: PNdServiceData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TMainForm.vtServicesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var Data: PNdServiceData;
begin
  Data := Sender.GetNodeData(Node);
  if TextType <> ttNormal then exit;
  case Column of
    NoColumn, 0:
      CellText := Data.ServiceName;
    1: CellText := Data.DisplayName;
    2: case Data.ServiceStatus.dwCurrentState of
         SERVICE_STOPPED: CellText := '';
         SERVICE_START_PENDING: CellText := '�����������...';
         SERVICE_STOP_PENDING: CellText := '�����������...';
         SERVICE_RUNNING: CellText := '�����������';
         SERVICE_CONTINUE_PENDING: CellText := '��������������...';
         SERVICE_PAUSE_PENDING: CellText := '������������������...';
         SERVICE_PAUSED: CellText := '��������������';
       else CellText := '������ ('+IntToStr(Data.ServiceStatus.dwCurrentState)+')';
       end;
  end;
end;

procedure TMainForm.vtServicesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: PNdServiceData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  case Column of
    0, NoColumn: Result := CompareText(Data1.ServiceName, Data2.ServiceName);
    1: Result := CompareText(Data1.DisplayName, Data2.DisplayName);
    2: Result := Data2.ServiceStatus.dwCurrentState - Data1.ServiceStatus.dwCurrentState;
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
      Sender.SortColumn := scNone;
  if Sender.SortColumn <> scNone then
    Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection);
end;



//��������� ��������� ����� �� ��������
procedure TMainForm.LoadServiceTree;
begin
  vtFolders.Clear;
  LoadServiceFolder(nil, AppFolder+'\SvcData');
  vtFolders_AddSpecial(ntRunningServices, '����������');
  vtFolders_AddSpecial(ntAllServices, '���');
end;

procedure TMainForm.LoadServiceFolder(AParentNode: PVirtualNode; AFolderPath: string);
var res: integer;
  sr: TSearchRec;
  node: PVirtualNode;
begin
  res := FindFirst(AFolderPath+'\*.*', faAnyFile, sr);
  while res = 0 do begin
    if (sr.Name='.') or (sr.Name='..') then begin
      res := FindNext(sr);
      continue;
    end;
    if (sr.Attr and faDirectory) = faDirectory then begin
      node := vtFolders_Add(AParentNode, AFolderPath+'\'+sr.Name);
      LoadServiceFolder(node, AFolderPath+'\'+sr.Name);
    end;
    //TODO: Also add services.
    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;

function TMainForm.vtFolders_Add(AParent: PVirtualNode; AFolderPath: string): PVirtualNode;
var Data: PNdFolderData;
begin
  Result := vtFolders.AddChild(AParent);
  vtFolders.ReinitNode(Result, false);
  Data := vtFolders.GetNodeData(Result);
  Data.NodeType := ntFolder;
  Data.FolderName := ExtractFilename(AFolderPath);
end;

function TMainForm.vtFolders_AddSpecial(AType: TFolderNodeType; ATitle: string): PVirtualNode;
var Data: PNdFolderData;
begin
  Result := vtFolders.AddChild(nil);
  vtFolders.ReinitNode(Result, false);
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




end.
