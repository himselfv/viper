unit Viper.ServiceList;

interface

uses
  Windows, WinSvc, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  Actions, ActnList, VirtualTrees, SvcEntry, ImgList, Vcl.Menus, CommonResources;

type
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
    aStartTypeManual: TAction;
    aStartTypeDisabled: TAction;
    aDeleteService: TAction;
    aExportService: TAction;
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
    Copy1: TMenuItem;
    Ident1: TMenuItem;
    Name1: TMenuItem;
    Description1: TMenuItem;
    Executablefilename1: TMenuItem;
    N3: TMenuItem;
    miStartType: TMenuItem;
    Automatic1: TMenuItem;
    Manual1: TMenuItem;
    Disabled1: TMenuItem;
    Advanced1: TMenuItem;
    Exporttoreg1: TMenuItem;
    Deleteservice1: TMenuItem;
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
    Security1: TMenuItem;
    miUnlockSecurity: TMenuItem;
    N4: TMenuItem;
    Shortsummary1: TMenuItem;
    N5: TMenuItem;
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
    procedure miEditSecurityClick(Sender: TObject);
    procedure aProtectionNoneExecute(Sender: TObject);
    procedure miUnlockSecurityClick(Sender: TObject);

  protected
    procedure Iterate_AddNodeDataToArray(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
    procedure InvalidateServiceNode(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Data: Pointer; var Abort: Boolean);
    procedure ServiceInvalidated(Sender: TObject);
    procedure SelectionChanged;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    function AddService(Parent: PVirtualNode; Service: TServiceEntry): PVirtualNode;
    procedure ApplyFilter(Callback: TVTGetNodeProc; Data: pointer);
    function GetServiceEntry(Node: PVirtualNode): TServiceEntry; inline;
    function GetFocusedService: TServiceEntry;
    function GetSelectedServices: TServiceEntries;
    function GetFirstSelectedService: TServiceEntry;
    function FindServiceNode(Service: TServiceEntry): PVirtualNode;
    procedure DeleteServiceNode(Service: TServiceEntry);

  end;

implementation
uses StrUtils, Clipbrd, ServiceHelper, ShellUtils, SecEdit, AclHelpers, AccCtrl, Viper.StyleSettings,
  Viper.Log;

{$R *.dfm}

constructor TServiceList.Create(AOwner: TComponent);
begin
  inherited;
  OnServiceInvalidated.Remove(ServiceInvalidated);
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

function TServiceList.AddService(Parent: PVirtualNode; Service: TServiceEntry): PVirtualNode;
begin
  Result := vtServices.AddChild(Parent, pointer(Service));
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

resourcestring
  sStatusStopped = 'Stopped';
  sStatusStartPending = 'Starting...';
  sStatusStopPending = 'Stopping...';
  sStatusRunning = 'Running';
  sStatusContinuePending = 'Resuming...';
  sStatusPausePending = 'Pausing...';
  sStatusPaused = 'Paused';
  sStatusOther = 'Other (%d)';

  sStartTypeAuto = 'Auto';
  sStartTypeDemand = 'Manual';
  sStartTypeDisabled = 'Disabled';
  sStartTypeBoot = 'Auto (boot)';
  sStartTypeSystem = 'Auto (system)';

  sProtectionNone = '';
  sProtectionWindows = 'OS (full)';
  sProtectionWindowsLight = 'OS (light)';
  sProtectionAntimalwareLight = 'Antimalware';

  sTriggerCount = '%d triggers';

  sTypeDriver = 'Driver';
  sTypeService = 'Service';
  sTypeUserService = 'User service';
  sTypeUserProto = 'User service prototype';

procedure TServiceList.vtServicesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var Data: TServiceEntry;
begin
  Data := TServiceEntry(Sender.GetNodeData(Node)^);
  if TextType <> ttNormal then exit;
  case Column of
    NoColumn, colServiceName: CellText := Data.ServiceName;
    colDisplayName: CellText := Data.GetEffectiveDisplayName;
    colStatus: case Data.Status.dwCurrentState of
         SERVICE_STOPPED: CellText := '';
         SERVICE_START_PENDING: CellText := sStatusStartPending;
         SERVICE_STOP_PENDING: CellText := sStatusStopPending;
         SERVICE_RUNNING: CellText := sStatusRunning;
         SERVICE_CONTINUE_PENDING: CellText := sStatusContinuePending;
         SERVICE_PAUSE_PENDING: CellText := sStatusPausePending;
         SERVICE_PAUSED: CellText := sStatusPaused;
       else CellText := Format(sStatusOther, [Data.Status.dwCurrentState]);
       end;
    colStartMode: if Data.Config = nil then
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
    colDescription: CellText := Data.Description;
    colFilename: CellText := Data.GetExecutableFilename;
    colProtection: case Data.LaunchProtection of
         SERVICE_LAUNCH_PROTECTED_NONE: CellText := sProtectionNone;
         SERVICE_LAUNCH_PROTECTED_WINDOWS: CellText := sProtectionWindows;
         SERVICE_LAUNCH_PROTECTED_WINDOWS_LIGHT: CellText := sProtectionWindowsLight;
         SERVICE_LAUNCH_PROTECTED_ANTIMALWARE_LIGHT: CellText := sProtectionAntimalwareLight;
       else CellText := '';
       end;
    colTriggers: if Data.TriggerCount > 0 then
         CellText := IntToStr(Data.TriggerCount)
       else
         CellText := '';
    colType:
      if Data.Status.dwServiceType and SERVICE_USERSERVICE_INSTANCE <> 0 then
        CellText := sTypeUserService
      else
      if Data.Status.dwServiceType and SERVICE_USER_SERVICE <> 0 then
        CellText := sTypeUserProto
      else
      if Data.Status.dwServiceType and SERVICE_DRIVER <> 0 then
        CellText := sTypeDriver
      else
        CellText := sTypeService;

  end;
end;

//Customize the background
procedure TServiceList.vtServicesBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var Data: TServiceEntry;
begin
  Data := TServiceEntry(Sender.GetNodeData(Node)^);

  if aUseColors.Checked then begin
    ItemColor := StyleSettingsForm.GetCombinedBgColor(Data);
    if ItemColor <> clWindow then
      EraseAction := eaColor;
  end;

end;

 //Customize the font
procedure TServiceList.vtServicesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var Data: TServiceEntry;
begin
  Data := TServiceEntry(Sender.GetNodeData(Node)^);

  if aUseColors.Checked then begin
    StyleSettingsForm.GetCombinedFont(Data, TargetCanvas.Font);
  end;

end;

procedure TServiceList.vtServicesGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var Service: TServiceEntry;
begin
  Service := TServiceEntry(Sender.GetNodeData(Node)^);
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
end;

procedure TServiceList.vtServicesCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: TServiceEntry;
begin
  Data1 := TServiceEntry(Sender.GetNodeData(Node1)^);
  Data2 := TServiceEntry(Sender.GetNodeData(Node2)^);
  case Column of
    NoColumn, colServiceName: Result := CompareText(Data1.ServiceName, Data2.ServiceName);
    colDisplayName: Result := CompareText(Data1.GetEffectiveDisplayName, Data2.GetEffectiveDisplayName);
    colStatus: Result := Data2.Status.dwCurrentState - Data1.Status.dwCurrentState;
    colStartMode: if Data1.Config = nil then
         if Data2.Config = nil then
           Result := 0
         else
           Result := 1
       else
         if Data2.Config = nil then
           Result := -1
         else
           Result := Data1.Config.dwStartType - Data2.Config.dwStartType;
    colDescription: Result := CompareText(Data1.Description, Data2.Description);
    colFilename: Result := CompareText(Data1.GetExecutableFilename, Data2.GetExecutableFilename);
    colTriggers: Result := Data2.TriggerCount - Data1.TriggerCount; //Triggers are by default sorted from biggest
    colType: Result := Data2.Status.dwServiceType - Data1.Status.dwServiceType;
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
var services: TServiceEntries;
  service: TServiceEntry;
  CanStart, CanForceStart, CanStop, CanPause, CanResume: boolean;
  CommonStartType: cardinal;
  CommonProtectionType: cardinal;
begin
  services := Self.GetSelectedServices();

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
  aStartTypeManual.Checked := (CommonStartType = SERVICE_DEMAND_START);
  aStartTypeDisabled.Checked := (CommonStartType = SERVICE_DISABLED);

  aStartTypeAutomatic.Visible := Length(services)>0;
  aStartTypeManual.Visible := Length(services)>0;
  aStartTypeDisabled.Visible := Length(services)>0;
  miStartType.Visible := aStartTypeAutomatic.Visible or aStartTypeManual.Visible or aStartTypeDisabled.Visible;

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
      Result := service.Config.dwStartType
    else
      if Result <> service.Config.dwStartType then begin
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
  if TServiceEntry(Sender.GetNodeData(Node)^) = Data then
    Sender.InvalidateNode(Node);
  if Sender.Selected[Node] then
    SelectionChanged; //properties of one of the selected nodes changed
end;




function TServiceList.GetServiceEntry(Node: PVirtualNode): TServiceEntry;
begin
  Result := TServiceEntry(vtServices.GetNodeData(Node)^);
end;

function TServiceList.GetFocusedService: TServiceEntry;
begin
  if vtServices.FocusedNode = nil then begin
    Result := nil;
    exit;
  end;

  Result := TServiceEntry(vtServices.GetNodeData(vtServices.FocusedNode)^);
end;

function TServiceList.GetSelectedServices: TServiceEntries;
begin
  SetLength(Result, 0);
  vtServices.IterateSubtree(nil, Iterate_AddNodeDataToArray, @Result, [vsSelected]);
end;

procedure TServiceList.Iterate_AddNodeDataToArray(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var entries: PServiceEntries absolute Data;
  nd: TServiceEntry;
begin
  nd := TServiceEntry(Sender.GetNodeData(Node)^);
  SetLength(entries^, Length(entries^)+1);
  entries^[Length(entries^)-1] := nd;
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

function TServiceList.FindServiceNode(Service: TServiceEntry): PVirtualNode;
begin
  Result := vtServices.IterateSubtree(nil, FindServiceNode_Callback, Service);
end;

procedure TServiceList.FindServiceNode_Callback(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; var Abort: Boolean);
begin
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

procedure TServiceList.aCopyServiceIDExecute(Sender: TObject);
var service: TServiceEntry;
  str: string;
begin
  str := '';
  for service in GetSelectedServices() do begin
    if str <> '' then str := str + #13;
    str := str + Service.ServiceName;
  end;
  Clipboard.AsText := str;
end;

procedure TServiceList.aCopyServiceNameExecute(Sender: TObject);
var Service: TServiceEntry;
  str: string;
begin
  str := '';
  for service in GetSelectedServices() do begin
    if str <> '' then str := str + #13;
    str := str + Service.DisplayName;
  end;
  Clipboard.AsText := str;
end;

procedure TServiceList.aCopyServiceSummaryExecute(Sender: TObject);
var service: TServiceEntry;
  str: string;
begin
  str := '';
  for service in GetSelectedServices() do begin
    if str <> '' then str := str + #13;
    str := str + Service.ServiceName + ' (' + Service.DisplayName + ')';
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
        services[i].Status := StopService(hSvcs[i].h);
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
          services[i].Status := QueryServiceStatus(hSvcs[i].h);
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
    ChangeServiceStartType(Service.ServiceName, SERVICE_AUTO_START);
    RefreshService(Service);
  end;
end;

procedure TServiceList.aStartTypeManualExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  for Service in GetSelectedServices() do begin
    ChangeServiceStartType(Service.ServiceName, SERVICE_DEMAND_START);
    RefreshService(Service);
  end;
end;

procedure TServiceList.aStartTypeDisabledExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  for Service in GetSelectedServices() do begin
    ChangeServiceStartType(Service.ServiceName, SERVICE_DISABLED);
    RefreshService(Service);
  end;
end;


// Security

procedure TServiceList.miEditSecurityClick(Sender: TObject);
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

procedure TServiceList.miUnlockSecurityClick(Sender: TObject);
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
  if Service.GetExecutableFilename <> '' then
    ExplorerAtFile(Service.GetExecutableFilename);
end;

procedure TServiceList.aJumpToRegistryExecute(Sender: TObject);
var Service: TServiceEntry;
begin
  Service := GetFirstSelectedService();
  Assert(Service <> nil);
  RegeditAtKey('HKEY_LOCAL_MACHINE\System\CurrentControlSet\services\'+Service.ServiceName);
end;


end.
