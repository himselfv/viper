unit Viper.TriggerList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, ImgList, WinSvc, ServiceHelper, Vcl.Menus, System.Actions, Vcl.ActnList;

//TODO: Fix Ctrl-C sharing between this and main service list!
//  Note that secondary service lists handle Ctrl-C fine (perhaps because trigger list is invisible
//  at that moment?)

//TODO: Parse Firewall Rule details (it's multistring)

type
  TNdTriggerData = record
    TriggerType: DWORD;
    TriggerSubtype: TGUID;
    Description: string;
    Params: string;
    Action: DWORD;
  end;
  PNdTriggerData = ^TNdTriggerData;

  TTriggerList = class(TFrame)
    vtTriggers: TVirtualStringTree;
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
    procedure aCopyTextExecute(Sender: TObject);
    procedure vtTriggersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure vtTriggersGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
  const
    colAction = 0;
    colTrigger = 1;
    colParams = 2;
  public
    procedure Clear;
    procedure Add(const ATrigger: PSERVICE_TRIGGER);
    procedure Reload(ServiceHandle: SC_HANDLE);
  end;

implementation
uses Clipbrd, CommonResources, TriggerUtils;

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
    NoColumn, colTrigger:
      CellText := Data.Description;
    colAction:
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_START then
        CellText := 'Start'
      else
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_STOP then
        CellText := 'Stop';
    colParams:
      CellText := Data.Params;
  end;
end;

procedure TTriggerList.vtTriggersGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var Data: PNdTriggerData;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;

  Data := Sender.GetNodeData(Node);
  if Data = nil then exit;

  ImageList := CommonRes.ilImages;
  case Column of
    NoColumn, colTrigger:
      case Data.TriggerType of
        SERVICE_TRIGGER_TYPE_DEVICE_INTERFACE_ARRIVAL: ImageIndex := CommonRes.iTriggerDevice;
        SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY: ImageIndex := CommonRes.iTriggerIp;
        SERVICE_TRIGGER_TYPE_DOMAIN_JOIN: ImageIndex := CommonRes.iTriggerDomain;
        SERVICE_TRIGGER_TYPE_FIREWALL_PORT_EVENT: ImageIndex := CommonRes.iTriggerFirewall;
        //TODO: The four types above would benefit from a separate "DIS-connected" versions
        SERVICE_TRIGGER_TYPE_GROUP_POLICY:
          if Data.TriggerSubtype = MACHINE_POLICY_PRESENT_GUID then
            ImageIndex := CommonRes.iTriggerMachinePolicy
          else
          if Data.TriggerSubtype = USER_POLICY_PRESENT_GUID then
            ImageIndex := CommonRes.iTriggerUserPolicy
          else
            ImageIndex := CommonRes.iTriggerGroupPolicy;
        SERVICE_TRIGGER_TYPE_NETWORK_ENDPOINT: ImageIndex := CommonRes.iTriggerNetwork;
        SERVICE_TRIGGER_TYPE_CUSTOM: ImageIndex := CommonRes.iTriggerEvent;
      else
        ImageIndex := CommonRes.iTrigger;
      end;

    colAction: begin
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_START then
        ImageIndex := 0
      else
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_STOP then
        ImageIndex := 1;
    end;
  end;
end;

procedure TTriggerList.vtTriggersFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex);
begin
  aCopyText.Visible := Node <> nil;
end;

procedure TTriggerList.Clear;
begin
  vtTriggers.Clear;
end;

procedure TTriggerList.Add(const ATrigger: PSERVICE_TRIGGER);
var Node: PVirtualNode;
  NodeData: PNdTriggerData;
  TriggerData: TTriggerData;
  Sources: TArray<string>;
  Source: string;
begin
  TriggerData := ParseTrigger(ATrigger);
  Sources := TriggerData.Sources;
  if Length(Sources) <= 0 then begin
    SetLength(Sources, 1);
    Sources[0] := '';
  end;

  for Source in Sources do begin
    Node := vtTriggers.AddChild(nil);
    vtTriggers.ReinitNode(Node, false);
    NodeData := vtTriggers.GetNodeData(Node);
    NodeData.TriggerType := ATrigger^.dwTriggerType;
    NodeData.Action := ATrigger^.dwAction;
    NodeData.TriggerSubtype := ATrigger.pTriggerSubtype^;
    if Source <> '' then
      NodeData.Description := TriggerData.Event + ' ('+Source+')'
    else
      NodeData.Description := TriggerData.Event;
    NodeData.Params := TriggerData.ParamsToString('; ');
  end;
end;

procedure TTriggerList.Reload(ServiceHandle: SC_HANDLE);
var triggers: PSERVICE_TRIGGER_INFO;
begin
  triggers := QueryServiceTriggers(ServiceHandle);
  if triggers = nil then exit;
  try
    while triggers.cTriggers > 0 do begin
      Self.Add(triggers.pTriggers);
      Inc(triggers.pTriggers);
      Dec(triggers.cTriggers);
    end;

  finally
    FreeMem(triggers);
  end;
end;

procedure TTriggerList.aCopyTextExecute(Sender: TObject);
var Data: PNdTriggerData;
begin
  if vtTriggers.FocusedNode = nil then exit;

  Data := vtTriggers.GetNodeData(vtTriggers.FocusedNode);
  if Data = nil then exit;

  if Data.Params <> '' then
    Clipboard.AsText := Data.Description + ' (' + Data.Params + ')'
  else
    Clipboard.AsText := Data.Description;
end;

end.
