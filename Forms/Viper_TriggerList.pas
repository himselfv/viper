unit Viper_TriggerList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, ImgList, WinSvc, ServiceHelper, Vcl.Menus, System.Actions, Vcl.ActnList;

//TODO: Fix Ctrl-C sharing between this and main service list!
//  Note that secondary service lists handle Ctrl-C fine (perhaps because trigger list is invisible
//  at that moment?)

//TODO: Fix Device Interface IDs (somehow they don't resolve to names)

//TODO: Parse Firewall Rule details (it's multistring)

//TODO: Choose better play/stop icons

type
  TNdTriggerData = record
    TriggerType: DWORD;
    TriggerSubtype: TGUID;
    EventDescription: string;
    Action: DWORD;
  end;
  PNdTriggerData = ^TNdTriggerData;

  TTriggerList = class(TFrame)
    vtTriggers: TVirtualStringTree;
    ilImages: TImageList;
    PopupMenu: TPopupMenu;
    ActionList: TActionList;
    aCopyText: TAction;
    Copy1: TMenuItem;
    procedure vtTriggersGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vtTriggersInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure vtTriggersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtTriggersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vtTriggersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure aCopyTextExecute(Sender: TObject);
    procedure vtTriggersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
  public
    procedure Clear;
    function Add(const ATrigger: PSERVICE_TRIGGER): PVirtualNode;
  end;

implementation
uses Clipbrd, SetupApiHelper;

{$R *.dfm}

procedure TTriggerList.vtTriggersGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNdTriggerData);
end;

procedure TTriggerList.vtTriggersInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var Data: PNdTriggerData;
begin
  Data := Sender.GetNodeData(Node);
  Initialize(Data^);
end;

procedure TTriggerList.vtTriggersFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var Data: PNdTriggerData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TTriggerList.vtTriggersGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var Data: PNdTriggerData;
begin
  if TextType <> ttNormal then exit;

  Data := Sender.GetNodeData(Node);
  if Data = nil then exit;

  case Column of
    NoColumn, 0:
      CellText := Data.EventDescription;
  end;
end;

procedure TTriggerList.vtTriggersGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var Data: PNdTriggerData;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;

  Data := Sender.GetNodeData(Node);
  if Data = nil then exit;

  case Column of
    NoColumn, 0:
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_START then
        ImageIndex := 0
      else
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_STOP then
        ImageIndex := 1;
  end;
end;

procedure TTriggerList.vtTriggersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  aCopyText.Visible := Node <> nil;
end;


resourcestring
  sTriggerDeviceInterfaceAvailable = 'Device available: %s';
  sTriggerDomainJoin = 'The computer joins a domain';
  sTriggerDomainLeave = 'The computer leaves a domain';
  sTriggerDomainUnusual = 'Unusual domain-related trigger';
  sTriggerFirewallPortOpen = 'Firewall port opens: %s';
  sTriggerFirewallPortClose = 'Firewall port closes: %s';
  sTriggerFirewallPortUnusual = 'Unusual firewall port-related trigger: %s';

  sUnknownDeviceInterfaceClass = 'Unknown Device Interface Class %s';

function ServiceTriggerToString(const ATrigger: PSERVICE_TRIGGER): string;
var tmp: string;
begin
 //NOTE: Triggers should further be subdivided with action GUIDS
 //See here: https://msdn.microsoft.com/en-us/library/windows/desktop/dd405512%28v=vs.85%29.aspx
  case ATrigger.dwTriggerType of
    SERVICE_TRIGGER_TYPE_DEVICE_INTERFACE_ARRIVAL: begin
     //A device of the specified device interface class arrives.
     //pTriggerSubtype specifies the device interface class GUID.
     //pDataItems specifies one or more hardware ID and compatible ID strings.
      if not TrySetupDiGetClassDescriptionStr(ATrigger.pTriggerSubtype^, tmp) then
        tmp := Format(sUnknownDeviceInterfaceClass, [GuidToString(ATrigger.pTriggerSubtype^)]);
      Result := Format(sTriggerDeviceInterfaceAvailable, [tmp]);
    end;

    SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY:
      Result := 'IP Address Availability';

    SERVICE_TRIGGER_TYPE_DOMAIN_JOIN: begin
     //The computer joins or leaves a domain.
     //pTriggerSubtype specifies DOMAIN_JOIN_GUID or DOMAIN_LEAVE_GUID.
      if ATrigger.pTriggerSubtype^ = DOMAIN_JOIN_GUID then
        Result := sTriggerDomainJoin
      else
      if ATrigger.pTriggerSubtype^ = DOMAIN_LEAVE_GUID then
        Result := sTriggerDomainLeave
      else
        Result := sTriggerDomainUnusual;
    end;

    SERVICE_TRIGGER_TYPE_FIREWALL_PORT_EVENT: begin
     //A firewall port is opened or approximately 60 seconds after the firewall port is closed.
     //pTriggerSubtype specifies FIREWALL_PORT_OPEN_GUID or FIREWALL_PORT_CLOSE_GUID.
     //The pDataItems specifies ..a SERVICE_TRIGGER_DATA_TYPE_STRING multi-string that specifies
     //the port, the protocol, and optionally the executable path and name of the service
      if ATrigger.cDataItems <= 0 then
        tmp := '(no data)'
      else
      if ATrigger.pDataItems^.dwDataType <> SERVICE_TRIGGER_DATA_TYPE_STRING then
        tmp := '(unusual data)'
      else
        tmp := PChar(ATrigger.pDataItems^.pData)^;

      if ATrigger.pTriggerSubtype^ = FIREWALL_PORT_OPEN_GUID then
        Result := Format(sTriggerFirewallPortOpen, [tmp])
      else
      if ATrigger.pTriggerSubtype^ = FIREWALL_PORT_CLOSE_GUID then
        Result := Format(sTriggerFirewallPortClose, [tmp])
      else
        Result := Format(sTriggerFirewallPortUnusual, [tmp]);
    end;

    SERVICE_TRIGGER_TYPE_GROUP_POLICY:
      Result := 'Group Policy';

    SERVICE_TRIGGER_TYPE_NETWORK_ENDPOINT:
      Result := 'Network Endpoint';

    SERVICE_TRIGGER_TYPE_CUSTOM:
      Result := 'Custom';

  else Result := 'Unusual trigger: '+IntToStr(ATrigger.dwTriggerType);
  end;
end;

procedure TTriggerList.Clear;
begin
  vtTriggers.Clear;
end;

function TTriggerList.Add(const ATrigger: PSERVICE_TRIGGER): PVirtualNode;
var Data: PNdTriggerData;
begin
  Result := vtTriggers.AddChild(nil);
  vtTriggers.ReinitNode(Result, false);
  Data := vtTriggers.GetNodeData(Result);

  Data.TriggerType := ATrigger^.dwTriggerType;
  Data.Action := ATrigger^.dwAction;
  Data.TriggerSubtype := ATrigger.pTriggerSubtype^;
  Data.EventDescription := ServiceTriggerToString(ATrigger);
end;

procedure TTriggerList.aCopyTextExecute(Sender: TObject);
var Data: PNdTriggerData;
begin
  if vtTriggers.FocusedNode = nil then exit;

  Data := vtTriggers.GetNodeData(vtTriggers.FocusedNode);
  if Data = nil then exit;

  Clipboard.AsText := Data.EventDescription;
end;

end.
