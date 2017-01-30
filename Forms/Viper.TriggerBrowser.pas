unit Viper.TriggerBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ImgList, VirtualTrees, Generics.Collections, WinSvc, SvcEntry,
  TriggerUtils;

type
 //We have nodes of 2 types: trigger groups (parent level) and actions (child level). Trigger groups have Action == 0
  TNdTriggerData = record
    TriggerType: DWORD;
    TriggerSubtype: TGUID;
    Description: string;
    Params: string;
    Action: DWORD;
    ServiceName: string;
  end;
  PNdTriggerData = ^TNdTriggerData;

  TTriggerBrowserForm = class(TForm)
    Tree: TVirtualStringTree;
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
  const
    colDescription = 0;
    colParams = 1;
  protected
    FServices: TServiceEntryList;
    FTriggerGroups: TDictionary<string, PVirtualNode>;
    procedure SetServices(const AValue: TServiceEntryList);
    function AddTriggerGroupNode(const AParent: PVirtualNode; const AGroup: string; ATriggerType: integer): PVirtualNode;
    function AddTriggerNode(const AParent: PVirtualNode; const ASource: string): PVirtualNode; overload;
    procedure AddTriggers(AService: TServiceEntry; AInfo: PSERVICE_TRIGGER);
    function GetGroupNode(ATriggerType: cardinal; const AEventType: string): PVirtualNode;
  public
    procedure Clear;
    procedure Reload;
    property Services: TServiceEntryList read FServices write SetServices;
  end;

var
  TriggerBrowserForm: TTriggerBrowserForm;

implementation
uses CommonResources, ServiceHelper;

{$R *.dfm}

procedure TTriggerBrowserForm.FormCreate(Sender: TObject);
begin
  FTriggerGroups := TDictionary<string, PVirtualNode>.Create;
end;

procedure TTriggerBrowserForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTriggerGroups);
end;

procedure TTriggerBrowserForm.FormShow(Sender: TObject);
begin
  Reload;
end;

procedure TTriggerBrowserForm.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNdTriggerData);
end;

procedure TTriggerBrowserForm.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var Data: PNdTriggerData;
begin
  Data := Sender.GetNodeData(Node);
  Initialize(Data^);
end;

procedure TTriggerBrowserForm.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var Data: PNdTriggerData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TTriggerBrowserForm.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var Data: PNdTriggerData;
begin
  if TextType <> ttNormal then exit;

  Data := Sender.GetNodeData(Node);
  if Data = nil then exit;

  case Column of
    NoColumn, colDescription:
      CellText := Data.Description;
    colParams:
      CellText := Data.Params;
  end;
end;

procedure TTriggerBrowserForm.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var Data: PNdTriggerData;
begin
  if not (Kind in [ikNormal, ikSelected]) then exit;

  Data := Sender.GetNodeData(Node);
  if Data = nil then exit;

  case Column of
    NoColumn, colDescription:
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_START then
        ImageIndex := 0
      else
      if Data.Action = SERVICE_TRIGGER_ACTION_SERVICE_STOP then
        ImageIndex := 1
      else
      if Data.Action = 0 then
        ImageIndex := CommonRes.GetTriggerImageIndex(Data.TriggerType, Data.TriggerSubtype);
  end;
end;

procedure TTriggerBrowserForm.TreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
  Column: TColumnIndex; var Result: Integer);
var Data1, Data2: PNdTriggerData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  Result := Data1.TriggerType - Data2.TriggerType;
  if Result = 0 then
    Result := CompareText(Data1.Description, Data2.Description);
end;

procedure TTriggerBrowserForm.TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if Sender.SortColumn <> HitInfo.Column then begin
    Sender.SortColumn := HitInfo.Column;
    Sender.SortDirection := sdAscending;
  end else
  if Sender.SortDirection = sdAscending then
    Sender.SortDirection := sdDescending
  else
    Sender.SortDirection := sdAscending;
  Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection);
end;

procedure TTriggerBrowserForm.SetServices(const AValue: TServiceEntryList);
begin
  if FServices = AValue then exit;
  FServices := AValue;
  Reload;
end;

procedure TTriggerBrowserForm.Clear;
begin
  FTriggerGroups.Clear;
  Tree.Clear;
end;

procedure TTriggerBrowserForm.Reload;
var service: TServiceEntry;
  triggers: PSERVICE_TRIGGER_INFO;
begin
  Clear;
  if FServices = nil then exit;

  Tree.BeginUpdate;
  try

    for service in Services do begin
      triggers := QueryServiceTriggers(service.Handle);
      if triggers = nil then exit;
      try
        while triggers.cTriggers > 0 do begin
          AddTriggers(service, triggers.pTriggers);
          Inc(triggers.pTriggers);
          Dec(triggers.cTriggers);
        end;

      finally
        FreeMem(triggers);
      end;
    end;

    Tree.SortTree(Tree.Header.SortColumn, Tree.Header.SortDirection);
  finally
    Tree.EndUpdate;
  end;
end;

procedure TTriggerBrowserForm.AddTriggers(AService: TServiceEntry; AInfo: PSERVICE_TRIGGER);
var ATriggerData: TTriggerData;
  ATriggerText: string;
  AActionText: string;
  AGroupNode: PVirtualNode;
  ANode: PVirtualNode;
  ANodeData: PNdTriggerData;
  ASources: TArray<TTriggerSource>;
  i: integer;
begin
  ATriggerData := ParseTrigger(AInfo);

  if AInfo.dwAction = SERVICE_TRIGGER_ACTION_SERVICE_START then
    AActionText := 'Start %s (%s)'
  else
  if AInfo.dwAction = SERVICE_TRIGGER_ACTION_SERVICE_STOP then
    AActionText := 'Stop %s (%s)'
  else
    AActionText := Format('Unknown action (%d)', [AInfo.dwAction])+' %s (%d)';
  AActionText := Format(AActionText, [AService.ServiceName, AService.DisplayName]);

  ASources := ATriggerData.Sources;
  if Length(ASources) <= 0 then begin
    SetLength(ASources, 1);
    ASources[0].Data := '';
    ASources[0].DisplayText := '';
  end;

  for i := 0 to Length(ASources)-1 do begin
    if ASources[i].Data <> '' then
      ATriggerText := ATriggerData.Event + ' ('+ASources[i].DisplayText+')'
    else
      ATriggerText := ATriggerData.Event;
    AGroupNode := GetGroupNode(AInfo.dwTriggerType, ATriggerText);

    ANode := AddTriggerNode(AGroupNode, AActionText);
    ANodeData := Tree.GetNodeData(ANode);
    ANodeData.TriggerType := AInfo.dwTriggerType;
    ANodeData.TriggerSubtype := AInfo.pTriggerSubtype^;
    ANodeData.Params := ATriggerData.ParamsToString('; ');
    ANodeData.ServiceName := AService.ServiceName;
    ANodeData.Action := AInfo.dwAction;
  end;

end;

function TTriggerBrowserForm.GetGroupNode(ATriggerType: cardinal; const AEventType: string): PVirtualNode;
begin
  if not FTriggerGroups.TryGetValue(AEventType, Result) then begin
    Result := AddTriggerGroupNode(nil, AEventType, ATriggerType);
    FTriggerGroups.Add(AEventType, Result);
  end;
end;

function TTriggerBrowserForm.AddTriggerGroupNode(const AParent: PVirtualNode; const AGroup: string; ATriggerType: integer): PVirtualNode;
var Data: PNdTriggerData;
begin
  Result := Tree.AddChild(AParent);
  Tree.ReinitNode(Result, false);
  Data := Tree.GetNodeData(Result);
  Data.TriggerType := ATriggerType;
  Data.Description := AGroup;
end;

function TTriggerBrowserForm.AddTriggerNode(const AParent: PVirtualNode; const ASource: string): PVirtualNode;
var Data: PNdTriggerData;
begin
  Result := Tree.AddChild(AParent);
  Tree.ReinitNode(Result, false);
  Data := Tree.GetNodeData(Result);
  Data.Description := ASource;
end;

end.
