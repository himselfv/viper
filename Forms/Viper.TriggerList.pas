unit Viper.TriggerList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, ImgList, WinSvc, ServiceHelper, Vcl.Menus, System.Actions, Vcl.ActnList,
  TriggerUtils;

//TODO: Fix Ctrl-C sharing between this and main service list!
//  Note that secondary service lists handle Ctrl-C fine (perhaps because trigger list is invisible
//  at that moment?)

//TODO: Parse Firewall Rule details (it's multistring)

type
  TNdTriggerData = record
    TriggerType: DWORD;
    TriggerSubtype: TGUID;
    Description: string;
    Source: TTriggerSource;
    Params: string;
    Action: DWORD;
  end;
  PNdTriggerData = ^TNdTriggerData;

  TTriggerList = class(TFrame)
    Tree: TVirtualStringTree;
    PopupMenu: TPopupMenu;
    ActionList: TActionList;
    aCopySummary: TAction;
    miCopySummary: TMenuItem;
    aCopyTriggerText: TAction;
    aCopySourceData: TAction;
    aCopyParams: TAction;
    miCopy: TMenuItem;
    Sourcetext1: TMenuItem;
    SourceID1: TMenuItem;
    Additionalparams1: TMenuItem;
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure aCopySummaryExecute(Sender: TObject);
    procedure TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure aCopyTriggerTextExecute(Sender: TObject);
    procedure aCopySourceDataExecute(Sender: TObject);
    procedure aCopyParamsExecute(Sender: TObject);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  const
    colAction = 0;
    colTrigger = 1;
    colParams = 2;
  public
    procedure Clear;
    procedure Add(const ATrigger: PSERVICE_TRIGGER);
    procedure Reload(ServiceHandle: SC_HANDLE);
    function SelectedTriggers: TArray<PNdTriggerData>;

  end;

implementation
uses Clipbrd, CommonResources;

{$R *.dfm}

procedure TTriggerList.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNdTriggerData);
end;

procedure TTriggerList.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var Data: PNdTriggerData;
begin
  Data := Sender.GetNodeData(Node);
  Initialize(Data^);
end;

procedure TTriggerList.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var Data: PNdTriggerData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

resourcestring
  sActionStart = 'Start';
  sActionStop = 'Stop';
  sActionOther = 'Action (%d)';

function TriggerActionToString(const Action: cardinal): string; inline;
begin
  case Action of
    SERVICE_TRIGGER_ACTION_SERVICE_START: Result := sActionStart;
    SERVICE_TRIGGER_ACTION_SERVICE_STOP: Result := sActionStop;
  else Result := Format(sActionOther, [Action]);
  end;
end;

procedure TTriggerList.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
      CellText := TriggerActionToString(Data.Action);
    colParams:
      CellText := Data.Params;
  end;
end;

procedure TTriggerList.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
      ImageIndex := CommonRes.GetTriggerImageIndex(Data.TriggerType, Data.TriggerSubtype);

    colAction: begin
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_START then
        ImageIndex := 0
      else
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_STOP then
        ImageIndex := 1;
    end;
  end;
end;

procedure TTriggerList.TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
 //Selection changed
  aCopySummary.Visible := Tree.SelectedCount > 0;
  aCopyTriggerText.Visible := Tree.SelectedCount > 0;
  aCopySourceData.Visible := Tree.SelectedCount > 0;
  aCopyParams.Visible := Tree.SelectedCount > 0;
end;

procedure TTriggerList.Clear;
begin
  Tree.Clear;
end;

procedure TTriggerList.Add(const ATrigger: PSERVICE_TRIGGER);
var Node: PVirtualNode;
  NodeData: PNdTriggerData;
  TriggerData: TTriggerData;
  Sources: TArray<TTriggerSource>;
  Source: TTriggerSource;
begin
  TriggerData := ParseTrigger(ATrigger);
  Sources := TriggerData.Sources;
  if Length(Sources) <= 0 then begin
    SetLength(Sources, 1);
    Sources[0].DisplayText := '';
    Sources[0].Data := '';
  end;

  for Source in Sources do begin
    Node := Tree.AddChild(nil);
    Tree.ReinitNode(Node, false);
    NodeData := Tree.GetNodeData(Node);
    NodeData.TriggerType := ATrigger^.dwTriggerType;
    NodeData.Action := ATrigger^.dwAction;
    NodeData.TriggerSubtype := ATrigger.pTriggerSubtype^;
    NodeData.Source := Source;
    if Source.Data <> '' then
      NodeData.Description := TriggerData.Event + ' ('+Source.DisplayText+')'
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

function TTriggerList.SelectedTriggers: TArray<PNdTriggerData>;
var ANode: PVirtualNode;
begin
  SetLength(Result, 0);
  for ANode in Tree.SelectedNodes() do begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := Tree.GetNodeData(ANode);
  end;
end;

const
  sTriggerSummary = '%s on %s';
  sTriggerSummaryParams = '%s on %s (%s)';

procedure TTriggerList.aCopySummaryExecute(Sender: TObject);
var Data: PNdTriggerData;
  Result: string;
begin
  Result := '';
  for Data in SelectedTriggers do
    if Data.Params = '' then
      Result := Result + Format(sTriggerSummary, [TriggerActionToString(Data.Action), Data.Description]) + #13#10
    else
      Result := Result + Format(sTriggerSummaryParams, [TriggerActionToString(Data.Action), Data.Description, Data.Params]) + #13#10;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

procedure TTriggerList.aCopyTriggerTextExecute(Sender: TObject);
var Data: PNdTriggerData;
  Result: string;
begin
  Result := '';

  for Data in SelectedTriggers do
    Result := Result + Data.Description + #13#10;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

procedure TTriggerList.aCopySourceDataExecute(Sender: TObject);
var Data: PNdTriggerData;
  Result: string;
begin
  Result := '';

  for Data in SelectedTriggers do
    Result := Result + Data.Source.Data + #13#10;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

procedure TTriggerList.aCopyParamsExecute(Sender: TObject);
var Data: PNdTriggerData;
  Result: string;
begin
  Result := '';

  for Data in SelectedTriggers do
    Result := Result + Data.Params + #13#10;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

end.
