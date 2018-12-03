unit Viper.ServiceList;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Actions, ActnList, VirtualTrees, SvcEntry, ImgList, Vcl.Menus, CommonResources;

{
Clients populate the list manually. Each node is associated with an object
managed elsewhere.

NodeData is always a TObject. If the tree recognizes the object type it presents
it well:
- TServiceEntry is a service entry
- TSlCustomNode and its descendants provide customizable general purpose nodes (e.g. folders)

Clients can use TServiceEntry descendants.
}

type
  TNodeList = TArray<PVirtualNode>;
  PNodeList = ^TNodeList;
  TServiceList = class(TFrame)
    vtServices: TVirtualStringTree;
    ActionList: TActionList;
    aStartService: TAction;
    aForceStartService: TAction;
    aStopService: TAction;
    aPauseService: TAction;
    aResumeService: TAction;
    aRestartService: TAction;
    aStartTypeAutomatic: TAction;
    aStartTypeAutoDelayed: TAction;
    aStartTypeManual: TAction;
    aStartTypeDisabled: TAction;
    aDeleteService: TAction;
    aExportService: TAction;
    aImportServices: TAction;
    aUseColors: TAction;
    aCopyServiceID: TAction;
    aCopyServiceName: TAction;
    aCopyServiceDescription: TAction;
    aCopyServiceSummary: TAction;
    aCopyExecutableFilename: TAction;
    aJumpToBinary: TAction;
    aJumpToRegistry: TAction;
    pmServices: TPopupMenu;
    pmStartService: TMenuItem;
    pmForceStartService: TMenuItem;
    Stop1: TMenuItem;
    Pause1: TMenuItem;
    Resume1: TMenuItem;
    Restart1: TMenuItem;
    N2: TMenuItem;
    Jumptobinary1: TMenuItem;
    Openregistrykey1: TMenuItem;
    miCopySubmenu: TMenuItem;
    Ident1: TMenuItem;
    Name1: TMenuItem;
    Description1: TMenuItem;
    Executablefilename1: TMenuItem;
    N3: TMenuItem;
    miStartType: TMenuItem;
    miStartTypeAutomatic: TMenuItem;
    miStartTypeAutoDelayed: TMenuItem;
    miStartTypeManual: TMenuItem;
    miStartTypeDisabled: TMenuItem;
    miAdvancedSubmenu: TMenuItem;
    miExportService: TMenuItem;
    miImportServices: TMenuItem;
    miDeleteService: TMenuItem;
    aEditSecurity: TAction;
    aUnlockSecurity: TAction;
    miEditSecurity: TMenuItem;
    N1: TMenuItem;
    miProtectionType: TMenuItem;
    miSetProtectionNone: TMenuItem;
    miSetProtectionWindows: TMenuItem;
    miSetProtectionWindowsLight: TMenuItem;
    miSetProtectionAntimalwareLight: TMenuItem;
    aProtectionNone: TAction;
    aProtectionWindows: TAction;
    aProtectionWindowsLight: TAction;
    aProtectionAntimalwareLight: TAction;
    miSecuritySubmenu: TMenuItem;
    miUnlockSecurity: TMenuItem;
    N4: TMenuItem;
    Shortsummary1: TMenuItem;
    N5: TMenuItem;
    SaveRegFileDialog: TSaveDialog;
    OpenRegFileDialog: TOpenDialog;
    procedure vtServicesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtServicesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vtServicesHeaderClick(Sender: TVTHeader;
      HitInfo: TVTHeaderHitInfo);
    procedure vtServicesCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vtServicesBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure vtServicesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure vtServicesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vtServicesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure aCopyServiceIDExecute(Sender: TObject);
    procedure aCopyServiceNameExecute(Sender: TObject);
    procedure aCopyServiceDescriptionExecute(Sender: TObject);
    procedure aCopyServiceSummaryExecute(Sender: TObject);
    procedure aCopyExecutableFilenameExecute(Sender: TObject);
    procedure aStartServiceExecute(Sender: TObject);
    procedure aForceStartServiceExecute(Sender: TObject);
    procedure aStopServiceExecute(Sender: TObject);
    procedure aPauseServiceExecute(Sender: TObject);
    procedure aResumeServiceExecute(Sender: TObject);
    procedure aRestartServiceExecute(Sender: TObject);
    procedure aStartTypeAutomaticExecute(Sender: TObject);
    procedure aStartTypeAutoDelayedExecute(Sender: TObject);
    procedure aStartTypeManualExecute(Sender: TObject);
    procedure aStartTypeDisabledExecute(Sender: TObject);
    procedure aJumpToBinaryExecute(Sender: TObject);
    procedure aJumpToRegistryExecute(Sender: TObject);
    procedure vtServicesGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure pmServicesPopup(Sender: TObject);
    procedure vtServicesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure aColorByStartTypeExecute(Sender: TObject);
    procedure aProtectionNoneExecute(Sender: TObject);
    procedure aEditSecurityExecute(Sender: TObject);
    procedure aUnlockSecurityExecute(Sender: TObject);
    procedure vtServicesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtServicesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure aExportServiceExecute(Sender: TObject);
    procedure aImportServicesExecute(Sender: TObject);

  protected
    procedure Iterate_AddNodeToArray(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure Iterate_AddServiceDataToArray(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure InvalidateServiceNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure ServiceInvalidated(Sender: TObject);
    procedure SelectionChanged; virtual;
    function GetCommonStartType(const Services: TServiceEntries): DWORD;
    function GetCommonProtectionType(const Services: TServiceEntries): DWORD;
    procedure FindServiceNode_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
      var Abort: Boolean);
  public const
    colServiceName = 0;
    colDisplayName = 1;
    colStatus = 2;
    colStartMode = 3;
    colTriggers = 4;
    colDescription = 5;
    colFilename = 6;
    colProtection = 7;
    colType = 8;
    colPID = 9;
    colLoadOrderGroup = 10;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    function AddNode(Parent: PVirtualNode; Data: TObject): PVirtualNode;
    function AddService(Parent: PVirtualNode; Service: TServiceEntry): PVirtualNode; inline;
    procedure ApplyFilter(Callback: TVTGetNodeProc; Data: pointer);
    function GetServiceEntry(Node: PVirtualNode): TServiceEntry; inline;
    function GetFocusedService: TServiceEntry;
    function GetSelectedNodes: TNodeList;
    function GetSelectedServices: TServiceEntries;
    function GetFirstSelectedService: TServiceEntry;
    function FindServiceNode(Service: TServiceEntry): PVirtualNode;
    procedure DeleteServiceNode(Service: TServiceEntry);

  end;

  { Base class for custom nodes (e.g. folders)
  Used by clients to implement dependency groups and inline subfolders. }
  TSlCustomNode = class(TObject)
  protected
    FAutoDestroy: boolean; //if true, the list will dispose of the object on clear
    FName: string;
    //Override to provide customized information
    function GetName: string; virtual;
    function GetDisplayName: string; virtual;
    function GetTypeText: string; virtual;
  public
    constructor Create(const AName: string);
    procedure GetImageIndex(Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer; var ImageList: TCustomImageList); virtual;
    property Name: string read GetName;
    property DisplayName: string read GetDisplayName;
    property TypeText: string read GetTypeText;
    property AutoDestroy: boolean read FAutoDestroy write FAutoDestroy;
  end;

  //Base class for folders, only overrides the icon
  TSlFolderNode = class(TSlCustomNode)
  public
    procedure GetImageIndex(Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer; var ImageList: TCustomImageList); override;
  end;

resourcestring
  sServiceStatusStopped = 'Stopped';
  sServiceStatusStartPending = 'Starting...';
  sServiceStatusStopPending = 'Stopping...';
  sServiceStatusRunning = 'Running';
  sServiceStatusContinuePending = 'Resuming...';
  sServiceStatusPausePending = 'Pausing...';
  sServiceStatusPaused = 'Paused';
  sServiceStatusOther = 'Other (%d)';

  sServiceStartTypeAuto = 'Auto';
  sServiceStartTypeAutoDelayed = 'Delayed';
  sServiceStartTypeDemand = 'Manual';
  sServiceStartTypeDisabled = 'Disabled';
  sServiceStartTypeBoot = 'Auto (boot)';
  sServiceStartTypeSystem = 'Auto (system)';

  sServiceProtectionNone = '';
  sServiceProtectionWindows = 'OS (full)';
  sServiceProtectionWindowsLight = 'OS (light)';
  sServiceProtectionAntimalwareLight = 'Antimalware';

  sServiceTriggerCount = '%d triggers';

  sServiceTypeDriver = 'Driver';
  sServiceTypeService = 'Service';
  sServiceTypeUserService = 'User service';
  sServiceTypeUserProto = 'Prototype';


implementation
uses StrUtils, Clipbrd, ServiceHelper, ShellUtils, SecEdit, AclHelpers, AccCtrl,
  RegExport, Viper.StyleSettings, Viper.Log, Viper.ServiceImport;

{$R *.dfm}

constructor TSlCustomNode.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

function TSlCustomNode.GetName: string;
begin
  Result := FName;
end;

function TSlCustomNode.GetDisplayName: string;
begin
  Result := '';
end;

function TSlCustomNode.GetTypeText: string;
begin
  Result := '';
end;

procedure TSlCustomNode.GetImageIndex(Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
begin
  //Nothing.
end;

procedure TSlFolderNode.GetImageIndex(Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
begin
  if (Column <> NoColumn) and (Column <> TServiceList.colServiceName) then
    exit;
  case Kind of
    ikNormal, ikSelected: begin
      ImageList := CommonRes.ilImages;
      ImageIndex := CommonRes.iFolder;
    end;
  end;
end;

constructor TServiceList.Create(AOwner: TComponent);
begin
  inherited;
  OnServiceInvalidated.Add(ServiceInvalidated);
end;

destructor TServiceList.Destroy;
begin
  if OnServiceInvalidated <> nil then
    OnServiceInvalidated.Remove(ServiceInvalidated);
  inherited;
end;

procedure TServiceList.Clear;
begin
  vtServices.Clear;
  SelectionChanged;
end;

procedure TServiceList.BeginUpdate;
begin
  vtServices.BeginUpdate;
end;

procedure TServiceList.EndUpdate;
begin
  vtServices.SortTree(vtServices.Header.SortColumn, vtServices.Header.SortDirection); //re-apply sort
  vtServices.EndUpdate;
end;

function TServiceList.AddNode(Parent: PVirtualNode; Data: TObject): PVirtualNode;
begin
  Result := vtServices.AddChild(Parent, pointer(Data));
end;

function TServiceList.AddService(Parent: PVirtualNode; Service: TServiceEntry): PVirtualNode;
begin
  Result := AddNode(Parent, Service);
end;

procedure TServiceList.ApplyFilter(Callback: TVTGetNodeProc; Data: pointer);
begin
  vtServices.BeginUpdate;
  try
    vtServices.IterateSubtree(nil, Callback, Data, [], {DoInit=}true);
  finally
    vtServices.EndUpdate;
  end;
end;


procedure TServiceList.vtServicesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TObject);
end;

procedure TServiceList.vtServicesInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
//
end;

procedure TServiceList.vtServicesFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var Data: TObject;
begin
  Data := TObject(Sender.GetNodeData(Node)^);
  if Data = nil then exit;

  //We provide some courtesy auto destroy on client request
  if (Data is TSlCustomNode) and TSlCustomNode(Data).AutoDestroy then
    Data.Destroy;
end;


procedure TServiceList.vtServicesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var BaseData: TObject;
  Data: TServiceEntry;
begin
  BaseData := TObject(Sender.GetNodeData(Node)^);
  if BaseData is TServiceEntry then begin

    Data := TServiceEntry(BaseData);
    if TextType <> ttNormal then exit;
    case Column of
      NoColumn, colServiceName: CellText := Data.ServiceName;
      colDisplayName: CellText := Data.GetEffectiveDisplayName;
      colStatus: case Data.Status.dwCurrentState of
           SERVICE_STOPPED: CellText := '';
           SERVICE_START_PENDING: CellText := sServiceStatusStartPending;
           SERVICE_STOP_PENDING: CellText := sServiceStatusStopPending;
           SERVICE_RUNNING: CellText := sServiceStatusRunning;
           SERVICE_CONTINUE_PENDING: CellText := sServiceStatusContinuePending;
           SERVICE_PAUSE_PENDING: CellText := sServiceStatusPausePending;
           SERVICE_PAUSED: CellText := sServiceStatusPaused;
         else CellText := Format(sServiceStatusOther, [Data.Status.dwCurrentState]);
         end;
      colStartMode:
         case Data.StartTypeEx of
           SERVICE_AUTO_START: CellText := sServiceStartTypeAuto;
           SERVICE_DELAYED_AUTOSTART: CellText := sServiceStartTypeAutoDelayed;
           SERVICE_DEMAND_START: CellText := sServiceStartTypeDemand;
           SERVICE_DISABLED: CellText := sServiceStartTypeDisabled;
           SERVICE_BOOT_START: CellText := sServiceStartTypeBoot;
           SERVICE_SYSTEM_START: CellText := sServiceStartTypeSystem;
         else CellText := '';
         end;
      colDescription: CellText := Data.Description;
      colFilename: CellText := Data.GetImageFilename;
      colProtection: case Data.LaunchProtection of
           SERVICE_LAUNCH_PROTECTED_NONE: CellText := sServiceProtectionNone;
           SERVICE_LAUNCH_PROTECTED_WINDOWS: CellText := sServiceProtectionWindows;
           SERVICE_LAUNCH_PROTECTED_WINDOWS_LIGHT: CellText := sServiceProtectionWindowsLight;
           SERVICE_LAUNCH_PROTECTED_ANTIMALWARE_LIGHT: CellText := sServiceProtectionAntimalwareLight;
         else CellText := '';
         end;
      colTriggers: if Data.TriggerCount > 0 then
           CellText := IntToStr(Data.TriggerCount)
         else
           CellText := '';
      colType:
        if Data.Status.dwServiceType and SERVICE_USERSERVICE_INSTANCE <> 0 then
          CellText := sServiceTypeUserService
        else
        if Data.Status.dwServiceType and SERVICE_USER_SERVICE <> 0 then
          CellText := sServiceTypeUserProto
        else
        if Data.Status.dwServiceType and SERVICE_DRIVER <> 0 then
          CellText := sServiceTypeDriver
        else
          CellText := sServiceTypeService;
      colPID:
        if Data.Status.dwCurrentState = SERVICE_STOPPED then
          CellText := ''
        else
          CellText := IntToStr(Data.Status.dwProcessId);
      colLoadOrderGroup: CellText := Data.LoadOrderGroup;
    end;

  end else
  if BaseData is TSlCustomNode then begin

    if TextType <> ttNormal then exit;
    case Column of
      NoColumn, colServiceName: CellText := TSlCustomNode(BaseData).Name;
      colDisplayName: CellText := TSlCustomNode(BaseData).DisplayName;
      colType: CellText := TSlCustomNode(BaseData).TypeText;
    else CellText := '';
    end;

  end;
end;

//Customize the background
procedure TServiceList.vtServicesBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var BaseData: TObject;
  Data: TServiceEntry;
begin
  BaseData := TObject(Sender.GetNodeData(Node)^);

  if BaseData is TServiceEntry then begin
    Data := TServiceEntry(BaseData);

    if aUseColors.Checked then begin
      ItemColor := StyleSettingsForm.GetCombinedBgColor(Data);
      if ItemColor <> clWindow then
        EraseAction := eaColor;
    end;
  end;
end;

 //Customize the font
procedure TServiceList.vtServicesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var BaseData: TObject;
  Data: TServiceEntry;
begin
  BaseData := TObject(Sender.GetNodeData(Node)^);

  if BaseData is TServiceEntry then begin
    Data := TServiceEntry(BaseData);

    if aUseColors.Checked then begin
      StyleSettingsForm.GetCombinedFont(Data, TargetCanvas.Font);
    end;
  end;
end;

procedure TServiceList.vtServicesGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var BaseData: TObject;
  Service: TServiceEntry;
begin
  BaseData := TObject(Sender.GetNodeData(Node)^);

  if BaseData is TServiceEntry then begin
    Service := TServiceEntry(BaseData);
    case Kind of
    ikNormal, ikSelected:
      case Column of
        NoColumn, colServiceName: Service.GetIcon(ImageList, ImageIndex);
        colProtection: if Service.IsLaunchProtected then begin
          ImageList := CommonRes.ilImages;
          ImageIndex := CommonRes.iShield;
        end;
        colTriggers: if Service.TriggerCount > 0 then begin
          ImageList := CommonRes.ilImages;
          ImageIndex := CommonRes.iTrigger;
        end;

      end;
    ikOverlay: begin
      case Column of
        NoColumn, colServiceName: begin
          ImageList := CommonRes.ilOverlays;
          if Service.IsLaunchProtected then
            ImageIndex := CommonRes.iShieldOverlay;
        end;
      end;
    end;
    end;

  end else
  if BaseData is TSlCustomNode then
    TSlCustomNode(BaseData).GetImageIndex(Kind, Column, Ghosted, ImageIndex, ImageList);
end;

//Assigns ranks to service start types, for sorting
function RankServiceStartType(const AService: TServiceEntry): integer; inline;
begin
  if AService.Config = nil then
    Result := +MAXINT    //Unaccessible services all go to the bottom
  else begin
    Result := AService.StartTypeEx;
    //DELAYED needs to be moved to just after AUTO
    if Result = SERVICE_DELAYED_AUTOSTART then
      Result := SERVICE_AUTO_START*10 + 1
    else
      Result := Result*10;
  end;
end;

procedure TServiceList.vtServicesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var BaseData1, BaseData2: TObject;
  Data1, Data2: TServiceEntry;
  Text1, Text2: string;
begin
  //It's unclear how to compare objects of different types in a generalized fashion
  BaseData1 := TObject(Sender.GetNodeData(Node1)^);
  BaseData2 := TObject(Sender.GetNodeData(Node2)^);

  //Currently column-by-column comparisons are only possible for TServiceEntry
  //or for certain columns
  if (Column<>NoColumn) and (not Column in [colServiceName, colDisplayName])
  and (not (BaseData1 is TServiceEntry) or not (BaseData2 is TServiceEntry)) then begin
    Result := Integer(BaseData1.ClassType) - Integer(BaseData2.ClassType);
    exit; //that's it
  end;

  Data1 := TServiceEntry(BaseData1);
  Data2 := TServiceEntry(BaseData2);
  case Column of
    NoColumn, colServiceName, colDisplayName: begin
      //Compare by text, this way we handle any supported node types
      Text1 := vtServices.Text[Node1, Column];
      Text2 := vtServices.Text[Node2, Column];
      Result := CompareText(Text1, Text2);
      //Result := CompareText(Data1.ServiceName, Data2.ServiceName);
    end;
    colStatus: Result := Data2.Status.dwCurrentState - Data1.Status.dwCurrentState;
    colStartMode: Result := RankServiceStartType(Data1) - RankServiceStartType(Data2);
    colDescription: Result := CompareText(Data1.Description, Data2.Description);
    colTriggers: Result := Data2.TriggerCount - Data1.TriggerCount; //Triggers are by default sorted from biggest
    colFilename: Result := CompareText(Data1.GetImageFilename, Data2.GetImageFilename);
    colProtection: Result := Data2.LaunchProtection - Data1.LaunchProtection; //reverse order so that 0 goes last
    colType: Result := Data1.Status.dwServiceType - Data2.Status.dwServiceType;
    colPID: begin
      if Data1.Status.dwCurrentState = SERVICE_STOPPED then
        if Data2.Status.dwCurrentState = SERVICE_STOPPED then
          Result := 0
        else
          Result := 1 // STOPPED are sorted later
      else
        if Data2.Status.dwCurrentState = SERVICE_STOPPED then
          Result := -1
        else
          Result := Data1.Status.dwProcessId - Data2.Status.dwProcessId;
    end;
    colLoadOrderGroup: Result := CompareText(Data1.LoadOrderGroup, Data2.LoadOrderGroup); //though this puts empty strings to the top

  end;
end;

procedure TServiceList.vtServicesFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
//Kept just so the overrides automatically insert calls to us (in case we need it later)
end;

procedure TServiceList.vtServicesHeaderClick(Sender: TVTHeader;
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

procedure TServiceList.vtServicesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and ((Key=Ord('C')) or (Key=Ord('c'))) then
    aCopyServiceSummary.Execute;
end;

procedure TServiceList.vtServicesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  SelectionChanged;
end;


//Call when there's reason to suspect that node selection or the properties of those nodes
//could have changed;
procedure TServiceList.SelectionChanged;
var nodes: TNodeList;
  services: TServiceEntries;
  service: TServiceEntry;
  CanStart, CanForceStart, CanStop, CanPause, CanResume: boolean;
  CommonStartType: cardinal;
  CommonProtectionType: cardinal;
begin
  nodes := Self.GetSelectedNodes(); //all nodes
  services := Self.GetSelectedServices(); //only service entries

  aCopyServiceID.Visible := Length(nodes)>0;
  aCopyServiceName.Visible := Length(nodes)>0;
  aCopyServiceDescription.Visible := Length(services)>0;
  aCopyServiceSummary.Visible := Length(nodes)>0;
  aCopyExecutableFilename.Visible := Length(services)>0;
  miCopySubmenu.Visible := aCopyServiceID.Visible or aCopyServiceName.Visible
    or aCopyServiceDescription.Visible or aCopyServiceSummary.Visible
    or aCopyExecutableFilename.Visible;

 //Show an action if any of the selected services allows it
  CanStart := false;
  CanForceStart := false;
  CanStop := false;
  CanPause := false;
  CanResume := false;
  for service in services do begin
    if service.CanStart then CanStart := true;
    if service.CanForceStart then CanForceStart := true;
    if service.CanStop then CanStop := true;
    if service.CanPause then CanPause := true;
    if service.CanResume then CanResume := true;
  end;
  aStartService.Visible := CanStart;
  aForceStartService.Visible := CanForceStart;
  aStopService.Visible := CanStop;
  aPauseService.Visible := CanPause;
  aResumeService.Visible := CanResume;
  aRestartService.Visible := CanStop;

  aJumpToBinary.Visible := Length(services)=1;
  aJumpToRegistry.Visible := Length(services)=1;

 //Check start type item if all selected services share it
  CommonStartType := GetCommonStartType(services);
  aStartTypeAutomatic.Checked := (CommonStartType = SERVICE_AUTO_START);
  aStartTypeAutoDelayed.Checked := (CommonStartType = SERVICE_DELAYED_AUTOSTART);
  aStartTypeManual.Checked := (CommonStartType = SERVICE_DEMAND_START);
  aStartTypeDisabled.Checked := (CommonStartType = SERVICE_DISABLED);

  aStartTypeAutomatic.Visible := Length(services)>0;
  aStartTypeAutoDelayed.Visible := Length(services)>0;
  aStartTypeManual.Visible := Length(services)>0;
  aStartTypeDisabled.Visible := Length(services)>0;
  miStartType.Visible := aStartTypeAutomatic.Visible or aStartTypeAutoDelayed.Visible or aStartTypeManual.Visible or aStartTypeDisabled.Visible;

  CommonProtectionType := GetCommonProtectionType(services);
  aProtectionNone.Checked := (CommonProtectionType = SERVICE_LAUNCH_PROTECTED_NONE);
  aProtectionWindows.Checked := (CommonProtectionType = SERVICE_LAUNCH_PROTECTED_WINDOWS);
  aProtectionWindowsLight.Checked := (CommonProtectionType = SERVICE_LAUNCH_PROTECTED_WINDOWS_LIGHT);
  aProtectionAntimalwareLight.Checked := (CommonProtectionType = SERVICE_LAUNCH_PROTECTED_ANTIMALWARE_LIGHT);

  aProtectionNone.Visible := Length(services)>0;
  aProtectionWindows.Visible := Length(services)>0;
  aProtectionWindowsLight.Visible := Length(services)>0;
  aProtectionAntimalwareLight.Visible := Length(services)>0;
  miProtectionType.Visible := aProtectionNone.Visible or aProtectionWindows.Visible
    or aProtectionWindowsLight.Visible or aProtectionAntimalwareLight.Visible;

  aEditSecurity.Visible := Length(services)>0;
  aUnlockSecurity.Visible := Length(services)>0;
  miSecuritySubmenu.Visible := aEditSecurity.Visible or aUnlockSecurity.Visible
    or miProtectionType.Visible;

  aExportService.Visible := Length(services)>0;
  aDeleteService.Visible := Length(services)>0;
  aImportServices.Visible := true;
  miAdvancedSubmenu.Visible := aExportService.Visible or aImportServices.Visible
    or aDeleteService.Visible;
end;

//Returns a StartType common for all listed services, or -1 if there's variation
function TServiceList.GetCommonStartType(const Services: TServiceEntries): DWORD;
var service: TServiceEntry;
begin
  Result := cardinal(-1);
  for service in services do begin
    if service.Config = nil then begin
      Result := cardinal(-1);
      break; //no chance to find common
    end;

    if Result = cardinal(-1) then
      Result := service.StartTypeEx
    else
      if Result <> service.StartTypeEx then begin
        Result := cardinal(-1);
        break;
      end;
  end;
end;

//Returns a LaunchProtectionType common for all listed services, or -1 if there's variation
function TServiceList.GetCommonProtectionType(const Services: TServiceEntries): DWORD;
var service: TServiceEntry;
begin
  Result := cardinal(-1);
  for service in services do begin
    if Result = cardinal(-1) then
      Result := Service.LaunchProtection
    else
      if Result <> Service.LaunchProtection then begin
        Result := cardinal(-1);
        break;
      end;
  end;
end;


procedure TServiceList.ServiceInvalidated(Sender: TObject);
begin
  vtServices.IterateSubtree(nil, InvalidateServiceNode, TServiceEntry(Sender));
  SelectionChanged; //in case it was selected
end;

procedure TServiceList.InvalidateServiceNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
begin
  //We don't need to check whether it's TServiceData or not, all objects are unique pointers
  if TObject(Sender.GetNodeData(Node)^) = Data then
    Sender.InvalidateNode(Node);
  if Sender.Selected[Node] then
    SelectionChanged; //properties of one of the selected nodes changed
end;


//Can return nil if this is not a service node
function TServiceList.GetServiceEntry(Node: PVirtualNode): TServiceEntry;
var Data: TObject;
begin
  Data := TObject(vtServices.GetNodeData(Node)^);
  Data := TObject(vtServices.GetNodeData(vtServices.FocusedNode)^);
  if not (Data is TServiceEntry) then
    Result := nil
  else
    Result := TServiceEntry(Data);
end;

function TServiceList.GetFocusedService: TServiceEntry;
var Data: TObject;
begin
  if vtServices.FocusedNode = nil then begin
    Result := nil;
    exit;
  end;

  Data := TObject(vtServices.GetNodeData(vtServices.FocusedNode)^);
  if not (Data is TServiceEntry) then
    Result := nil
  else
    Result := TServiceEntry(Data);
end;

function TServiceList.GetSelectedNodes: TNodeList;
begin
  SetLength(Result, 0);
  vtServices.IterateSubtree(nil, Iterate_AddNodeToArray, @Result, [vsSelected]);
end;

procedure TServiceList.Iterate_AddNodeToArray(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var entries: PNodeList absolute Data;
begin
  SetLength(entries^, Length(entries^)+1);
  entries^[Length(entries^)-1] := Node;
end;

function TServiceList.GetSelectedServices: TServiceEntries;
begin
  SetLength(Result, 0);
  vtServices.IterateSubtree(nil, Iterate_AddServiceDataToArray, @Result, [vsSelected]);
end;

//Collects TServiceEntry for all compatible nodes
procedure TServiceList.Iterate_AddServiceDataToArray(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var entries: PServiceEntries absolute Data;
  nd: TObject;
begin
  nd := TObject(Sender.GetNodeData(Node)^);
  if not (nd is TServiceEntry) then
    exit;
  SetLength(entries^, Length(entries^)+1);
  entries^[Length(entries^)-1] := TServiceEntry(nd);
end;

function TServiceList.GetFirstSelectedService: TServiceEntry;
var services: TServiceEntries;
begin
  services := GetSelectedServices();
  if Length(services) < 0 then
    Result := nil
  else
    Result := services[0];
end;

//Also works for non-services really
function TServiceList.FindServiceNode(Service: TServiceEntry): PVirtualNode;
begin
  Result := vtServices.IterateSubtree(nil, FindServiceNode_Callback, Service);
end;

procedure TServiceList.FindServiceNode_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
begin
  //No need to check if it's TServiceEntry - objects are unique pointers
  Abort := (TServiceEntry(Sender.GetNodeData(Node)^) = Data);
end;

procedure TServiceList.DeleteServiceNode(Service: TServiceEntry);
var Node: PVirtualNode;
begin
  Node := FindServiceNode(Service);
  if Node <> nil then
    vtServices.DeleteNode(Node);
end;



procedure TServiceList.pmServicesPopup(Sender: TObject);
begin
  pmForceStartService.Visible := aForceStartService.Visible
    and (Windows.GetKeyState(VK_SHIFT) and $8000 <> 0); //Show extended action only if Shift is pressed
  pmStartService.Visible := aStartService.Visible
    and not pmForceStartService.Visible; //normal Start is overriden
end;



procedure TServiceList.aColorByStartTypeExecute(Sender: TObject);
begin
  vtServices.Invalidate;
end;

//Try to make at least some copy commands universal (available for non-service nodes)

procedure TServiceList.aCopyServiceIDExecute(Sender: TObject);
var node: PVirtualNode;
  str: string;
begin
  str := '';
  for node in GetSelectedNodes() do begin
    if str <> '' then str := str + #13;
    str := str + vtServices.Text[node, Self.colServiceName];
  end;
  Clipboard.AsText := str;
end;

procedure TServiceList.aCopyServiceNameExecute(Sender: TObject);
var node: PVirtualNode;
  str, val: string;
begin
  str := '';
  for node in GetSelectedNodes() do begin
    val := vtServices.Text[node, Self.colDisplayName];
    if val <> '' then begin
      if str <> '' then str := str + #13;
      str := str + val;
    end;
  end;
  Clipboard.AsText := str;
end;

procedure TServiceList.aCopyServiceSummaryExecute(Sender: TObject);
var node: PVirtualNode;
  str, disp: string;
begin
  str := '';
  for node in GetSelectedNodes() do begin
    if str <> '' then str := str + #13;
    str := str + vtServices.Text[node, Self.colServiceName];
    disp := vtServices.Text[node, Self.colDisplayName];
    if disp <> '' then
       str := str+ ' (' + disp + ')';
  end;
  Clipboard.AsText := str;
end;

procedure TServiceList.aCopyServiceDescriptionExecute(Sender: TObject);
var service: TServiceEntry;
  str: string;
begin
  str := '';
  for service in GetSelectedServices() do begin
    if str <> '' then str := str + #13;
    str := str + Service.Description;
  end;
  Clipboard.AsText := str;
end;

procedure TServiceList.aCopyExecutableFilenameExecute(Sender: TObject);
var service: TServiceEntry;
  str: string;
begin
  str := '';
  for service in GetSelectedServices() do begin
    if str <> '' then str := str + #13;
    if Service.Config <> nil then
      str := str + Service.Config.lpBinaryPathName;
  end;
  Clipboard.AsText := str;
end;

procedure TServiceList.aStartServiceExecute(Sender: TObject);
var service: TServiceEntry;
begin
  for service in GetSelectedServices() do
    if service.CanStart then begin
      StartService(Service.ServiceName);
      RefreshService(Service);
    end;
end;

procedure TServiceList.aForceStartServiceExecute(Sender: TObject);
var hSC, hSvc: SC_HANDLE;
  service: TServiceEntry;
  oldStartType: cardinal;
begin
  hSC := OpenSCManager();
  try
    for service in GetSelectedServices() do begin
      if service.CanStart then
        StartService(hSC, Service.ServiceName) //works even if Config==nil
      else
      if service.CanForceStart and (service.Config <> nil) then begin //or we wouldn't know what to restore
        hSvc := OpenService(hSC, service.ServiceName, SERVICE_CONTROL_ACCESS or SERVICE_WRITE_ACCESS);
        try
          service.Refresh; //we need actual latest config
          oldStartType := service.Config.dwStartType;
          ChangeServiceStartType(hSvc, SERVICE_DEMAND_START);
          StartService(hSvc);
        finally
          ChangeServiceStartType(hSvc, oldStartType);
          CloseServiceHandle(hSvc);
        end;
      end else
        continue;
      Service.Refresh;
      InvalidateService(Service);
    end;
  finally
    CloseServiceHandle(hSC);
  end;
end;

procedure TServiceList.aStopServiceExecute(Sender: TObject);
var service: TServiceEntry;
begin
  for service in GetSelectedServices() do
    if service.CanStop then begin
      StopService(Service.ServiceName);
      RefreshService(Service);
    end;
end;

procedure TServiceList.aPauseServiceExecute(Sender: TObject);
var service: TServiceEntry;
begin
  for service in GetSelectedServices() do
    if service.CanPause then begin
      PauseService(Service.ServiceName);
      RefreshService(Service);
    end;
end;

procedure TServiceList.aResumeServiceExecute(Sender: TObject);
var service: TServiceEntry;
begin
  for service in GetSelectedServices() do
    if service.CanResume then begin
      ContinueService(Service.ServiceName);
      RefreshService(Service);
    end;
end;

resourcestring
  eCouldNotStopSomeServices = 'Could not stop some services';

procedure TServiceList.aRestartServiceExecute(Sender: TObject);
var services: TServiceEntries;
  hSC: SC_HANDLE;
  hSvcs: array of record
    h: SC_HANDLE;
    waiting: boolean;
  end;
  tmStart: cardinal;
  i: integer;
  all_stopped: boolean;
begin
  services := GetSelectedServices();
  if Length(services) <= 0 then exit;

  hSC := OpenSCManager();
  try
    SetLength(hSvcs, Length(services));

    for i := 0 to Length(services)-1 do begin
      hSvcs[i].h := OpenService(hSC, services[i].ServiceName);
      if hSvcs[i].h = 0 then RaiseLastOsError();
      if services[i].CanStop then begin
        PServiceStatus(@services[i].Status)^ := StopService(hSvcs[i].h);
        RefreshService(services[i]);
        hSvcs[i].waiting := true;
      end else
        hSvcs[i].waiting := false;
    end;

    tmStart := GetTickCount();
    repeat
      all_stopped := true;
      for i := 0 to Length(hSvcs)-1 do
        if hSvcs[i].waiting then begin
          services[i].Status := QueryServiceStatusProcess(hSvcs[i].h);
          if services[i].Status.dwCurrentState = SERVICE_STOPPED then
            hSvcs[i].waiting := false
          else
            all_stopped := false;
        end;

      if all_stopped then break;
      Sleep(50);
    until GetTickCount() - tmStart > 5000;
    //TODO: Move ^^ into "Form.WaitForStatusChange()"

    for i := 0 to Length(hSvcs)-1 do
      if services[i].CanStart then begin
        StartService(hSvcs[i].h);
        RefreshService(services[i]);
      end;

    if not all_stopped then
      raise Exception.Create(eCouldNotStopSomeServices);
  finally
    for i := 0 to Length(hSvcs)-1 do
      CloseServiceHandle(hSvcs[i].h);
    CloseServiceHandle(hSC);
  end;
end;


procedure TServiceList.aStartTypeAutomaticExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  for Service in GetSelectedServices() do begin
    Service.SetStartTypeEx(SERVICE_AUTO_START);
    RefreshService(Service);
  end;
end;

procedure TServiceList.aStartTypeAutoDelayedExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  for Service in GetSelectedServices() do begin
    Service.SetStartTypeEx(SERVICE_DELAYED_AUTOSTART);
    RefreshService(Service);
  end;
end;

procedure TServiceList.aStartTypeManualExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  for Service in GetSelectedServices() do begin
    Service.SetStartTypeEx(SERVICE_DEMAND_START);
    RefreshService(Service);
  end;
end;

procedure TServiceList.aStartTypeDisabledExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  for Service in GetSelectedServices() do begin
    Service.SetStartTypeEx(SERVICE_DISABLED);
    RefreshService(Service);
  end;
end;


// Security

procedure TServiceList.aEditSecurityExecute(Sender: TObject);
var Service: TServiceEntry;
  hPriv: TPrivToken;
begin
  ClaimPrivilege(SE_SECURITY_NAME, hPriv); //it's okay if we don't
  try
    for Service in GetSelectedServices() do begin
      EditServiceSecurity(Self.Handle, Service.ServiceName, [efEditSacl]);
      RefreshService(Service);
    end;
  finally
    ReleasePrivilege(hPriv);
  end;
end;

resourcestring
  sConfirmSettingProtection = 'You''re about to set a launch protection for a service.'#13
    +'Once you protect a service, it cannot be reconfigured. You will not be able to uninstall it, '
    +'stop it or remove protection by normal means.'#13
    +'Do you still want to continue?';
  sProtectionNeedToReboot = 'You have overwritten the service protection level. You will likely '
    +'need to reboot before the changes are applied.';

procedure TServiceList.aProtectionNoneExecute(Sender: TObject);
var Services: TServiceEntries;
  Service: TServiceEntry;
  protectionValue, oldValue: cardinal;
begin
  Services := GetSelectedServices();
  if Length(Services) <= 0 then exit;

  case TComponent(Sender).Tag of
    0: protectionValue := SERVICE_LAUNCH_PROTECTED_NONE;
    1: protectionValue := SERVICE_LAUNCH_PROTECTED_WINDOWS;
    2: protectionValue := SERVICE_LAUNCH_PROTECTED_WINDOWS_LIGHT;
    3: protectionValue := SERVICE_LAUNCH_PROTECTED_ANTIMALWARE_LIGHT;
  else exit;
  end;

  oldValue := GetCommonProtectionType(services);

  if protectionValue <> SERVICE_LAUNCH_PROTECTED_NONE then
    if MessageBox(Self.Handle, PChar(sConfirmSettingProtection), PChar(Self.Caption),
      MB_ICONQUESTION or MB_YESNO) <> ID_YES then
      exit;

  for Service in GetSelectedServices() do begin
    if oldValue = SERVICE_LAUNCH_PROTECTED_NONE then
     //We can do this the supported way
      ChangeServiceLaunchProtected(Service.ServiceName, protectionValue)
    else
      OverwriteServiceLaunchProtection(Service.ServiceName, protectionValue);
    RefreshService(Service);
  end;

  MessageBox(Self.Handle, PChar(sProtectionNeedToReboot), PChar(Self.Caption), MB_OK + MB_ICONINFORMATION);
end;

resourcestring
  sUnlockNeedToReboot = 'Some of the services had launch protection. It has been disabled, but you need '
    +'to reboot and do unlock on them again to take ownership.';
  sUnlockDone = 'Done.';
  sNothingToChange = 'Nothing to change.';

procedure TServiceList.aUnlockSecurityExecute(Sender: TObject);
var Services: TServiceEntries;
  Service: TServiceEntry;
  hPriv: TPrivToken;
  pSidAdmin, pSidPreviousOwner: PSID;
  pPreviousDesc: PSECURITY_DESCRIPTOR;
  APreviousPermissions: cardinal;
  hadLaunchProt: boolean;
  hadOwnershipChanged: boolean;
  hadPermissionsChanged: boolean;
  err: integer;
begin
  Services := GetSelectedServices();
  if Length(Services) <= 0 then exit;

  hadLaunchProt := false;
  hadOwnershipChanged := false;
  hadPermissionsChanged := false;
  pSidAdmin := nil;

{$IFDEF DEBUG}
  AclHelpers.OnLog := LogForm.Log;
{$ENDIF}

  //Try to claim SE_TAKE_OWNERSHIP_NAME but tolerate if it's unavailable
  ClaimPrivilege(SE_TAKE_OWNERSHIP_NAME, hPriv);
  try

    pSidAdmin := AllocateSidBuiltinAdministrators();

    for Service in GetSelectedServices() do begin
      Log('Trying '+Service.ServiceName+'...');
      if Service.LaunchProtection <> SERVICE_LAUNCH_PROTECTED_NONE then begin
        Log('Resetting launch protection for '+Service.ServiceName+'...');
        OverwriteServiceLaunchProtection(Service.ServiceName, SERVICE_LAUNCH_PROTECTED_NONE);
        hadLaunchProt := true;
        continue; //Launch protection reset requires reboot, do not touch ownership or we'll fail with ACCESS_DENIED
      end;

      Log('Checking ownership for '+Service.ServiceName+'...');
      err := SwitchOwnership(Service.ServiceName, SE_SERVICE, pSidAdmin, pSidPreviousOwner, pPreviousDesc);
      if err <> 0 then
        RaiseLastOsError(err);

      if pSidPreviousOwner <> nil then begin
        Log('Ownership changed for '+Service.ServiceName+', giving the original owner all permissions...');
        hadOwnershipChanged := true;
        AddExplicitPermissions(Service.ServiceName, SE_SERVICE, pSidPreviousOwner, SERVICE_ALL_ACCESS);
        if pPreviousDesc <> nil then
          LocalFree(NativeUInt(pPreviousDesc));
      end;
      Log('Giving Administrators all permissions...');
      err := AddExplicitPermissions(Service.ServiceName, SE_SERVICE, pSidAdmin, SERVICE_ALL_ACCESS, @APreviousPermissions);
      if err = 0 then
        hadPermissionsChanged := APreviousPermissions <> SERVICE_ALL_ACCESS
      else begin
        Log('Cannot give permissions, error '+IntToStr(err));
        hadPermissionsChanged := false;
      end;

      RefreshService(Service);
    end;

  finally
    ReleasePrivilege(hPriv);
    if pSIDAdmin <> nil then
      FreeSid(pSIDAdmin);
  end;

  if hadLaunchProt then
    MessageBox(Self.Handle, PChar(sUnlockNeedToReboot), PChar(Self.Caption), MB_OK + MB_ICONINFORMATION)
  else
  if hadOwnershipChanged or hadPermissionsChanged then
    MessageBox(Self.Handle, PChar(sUnlockDone), PChar(Self.Caption), MB_OK + MB_ICONINFORMATION)
  else
    MessageBox(Self.Handle, PChar(sNothingToChange), PChar(Self.Caption), MB_OK + MB_ICONINFORMATION);
end;


// Misc

procedure TServiceList.aJumpToBinaryExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFirstSelectedService();
  Assert(Service <> nil);
  if Service.GetImageFilename <> '' then
    ExplorerAtFile(Service.GetImageFilename);
end;

procedure TServiceList.aJumpToRegistryExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFirstSelectedService();
  Assert(Service <> nil);
  RegeditAtKey('HKEY_LOCAL_MACHINE'+GetServiceKey(Service.ServiceName));
end;

resourcestring
  sExportServiceDialogTitle = 'Export "%s" to...';
  sExportMultipleServicesDialogTitle = 'Export %d services to...';

procedure TServiceList.aExportServiceExecute(Sender: TObject);
var exp: TRegistryExporter;
  Services: TServiceEntries;
  Service: TServiceEntry;
begin
  Services := GetSelectedServices();
  if Length(Services) <= 0 then exit;

  if Length(Services) = 1 then
    SaveRegFileDialog.Title := Format(sExportServiceDialogTitle, [Services[0].DisplayName])
  else
    SaveRegFileDialog.Title := Format(sExportMultipleServicesDialogTitle, [Length(Services)]);
  with SaveRegFileDialog do
    if not Execute then
      exit;

  exp := TRegistryExporter.Create();
  try
    for Service in Services do
      exp.ExportKey(HKEY_LOCAL_MACHINE, GetServiceKey(Service.ServiceName));
    exp.SaveToFile(SaveRegFileDialog.FileName);
  finally
    FreeAndNil(exp);
  end;
end;

procedure TServiceList.aImportServicesExecute(Sender: TObject);
begin
  with OpenRegFileDialog do
    if not Execute then
      exit;
  Viper.ServiceImport.ImportRegFile(Self, OpenRegFileDialog.FileName);
end;

end.
