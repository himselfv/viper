unit Viper_TriggerList;

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
    procedure Add(const ATrigger: PSERVICE_TRIGGER);
  end;

implementation
uses Clipbrd, TriggerUtils;

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
      CellText := Data.Description;
    1:
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_START then
        CellText := 'Start'
      else
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_STOP then
        CellText := 'Stop';
    2:
      CellText := Data.Params;
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
    NoColumn, 1:
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
