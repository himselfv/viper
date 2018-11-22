unit Viper.TriggerList;
{
Trigger list for a service or multiple services.
Usage: Either
1. Reload() to load all triggers for all services.
2. Reimplement Reload() to Add() the desired triggers manually.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, ImgList, WinSvc, ServiceHelper, Vcl.Menus, System.Actions, Vcl.ActnList,
  Generics.Collections, TriggerUtils;

type
  {
  Our own copy of PSERVICE_TRIGGER + any additional Trigger-wide properties.
  Each copy must have Data assigned.
  }
  TNdTrigger = class
  protected
    FData: PSERVICE_TRIGGER;
    FOwnsData: boolean;
  public
    ServiceName: string;      //The service which stores this trigger
    Index: integer;           //The index of this trigger in the service's trigger list. 0-based
                              //WARNING: Will change if we delete/add triggers.
    RegistryPath: string;     //Registry source for this trigger, if created from one
    IsDisabled: boolean;      //True if the trigger is disabled
    constructor Create(AData: PSERVICE_TRIGGER; AOwnsData: boolean = true);
    destructor Destroy; override;
    property Data: PSERVICE_TRIGGER read FData;
  end;

  TNdTriggerList = class(TObjectList<TNdTrigger>)
  public
    constructor Create;
  end;

  {
  Triggers are split into trigger facets, e.g. multiple ports in the same trigger.
  They are listed separately but you can only edit or delete them together.
  }
  TNdTriggerFacet = record
    //Parent trigger for this Facet. Required.
    //List entries which spawn from the same trigger will have the same pointer.
    Trigger: TNdTrigger;
    //Cached details of this particular facet
    Description: string;
    Source: TTriggerSource;
    Params: string;
    TriggerType: DWORD;
    TriggerSubtype: TGUID;
    Action: DWORD;
    function Summary: string;
  end;
  PNdTriggerFacet = ^TNdTriggerFacet;

  TTriggerEvent = procedure(Sender: TObject; const TriggerData: PNdTriggerFacet) of object;


  {
  Enables or disables splitting some triggers into multiple trigger entries for clarity,
  e.g.
    Start on Port 100,101 available
  ->
    Start on Port 100 available
    Start on Port 101 available
  }
  TTriggerEntryMode = (
    emSingleEntry,      //One trigger = one entry
    emMultiEntries,     //One trigger = multiple entries
    emChildEntries      //One trigger = multiple entries, but additional ones as child nodes
  );

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
    miCopyTriggerText: TMenuItem;
    miCopySourceData: TMenuItem;
    miCopyParams: TMenuItem;
    aDisableTrigger: TAction;
    aExportTrigger: TAction;
    aExportAllTriggers: TAction;
    aDeleteTrigger: TAction;
    miExportTrigger: TMenuItem;
    miDisableTrigger: TMenuItem;
    miDeleteTrigger: TMenuItem;
    aEditTrigger: TAction;
    miEditTrigger: TMenuItem;
    aCopyTriggerRegDefinition: TAction;
    Registrydefinition1: TMenuItem;
    OpenTriggersDialog: TOpenDialog;
    SaveTriggersDialog: TSaveDialog;
    aImportTrigger: TAction;
    N2: TMenuItem;
    miImportTrigger: TMenuItem;
    aEnableTrigger: TAction;
    miEnableTrigger: TMenuItem;
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
    procedure TreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure TreeCollapsing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var Allowed: Boolean);
    procedure aCopyTriggerTextExecute(Sender: TObject);
    procedure aCopySourceDataExecute(Sender: TObject);
    procedure aCopyParamsExecute(Sender: TObject);
    procedure aDeleteTriggerExecute(Sender: TObject);
    procedure aEditTriggerExecute(Sender: TObject);
    procedure aCopyTriggerRegDefinitionExecute(Sender: TObject);
    procedure aExportTriggerExecute(Sender: TObject);
    procedure aExportAllTriggersExecute(Sender: TObject);
    procedure aImportTriggerExecute(Sender: TObject);
    procedure aDisableTriggerExecute(Sender: TObject);
    procedure aEnableTriggerExecute(Sender: TObject);
  const
    colTrigger = 0;
    colAction = 1;
    colService = 2;
    colParams = 3;
  protected
    FTriggers: TNdTriggerList;
    FEntryMode: TTriggerEntryMode;
    FOnFocusChanged: TTriggerEvent;
    function NodesToTriggers(const ANodes: TVTVirtualNodeEnumeration): TArray<TNdTrigger>;
    procedure TryExportTriggers(const Sel: TArray<TNdTrigger>);
    procedure TryImportTriggers(const AServiceName: string = '');
    procedure LoadTriggersForService(const AScmHandle: SC_HANDLE; const AServiceName: string); overload;
    procedure LoadTriggersForService(const AServiceName: string; const AServiceHandle: SC_HANDLE); overload;
    procedure LoadDisabledTriggersForService(const AServiceName: string); overload;
    procedure HandleTriggerListChanged(Sender: TObject; const AService: string);
    procedure SetEntryMode(const Value: TTriggerEntryMode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Reload; virtual;
    function Add(const ATrigger: TNdTrigger): PVirtualNode;
    function AddFromScm(const AServiceName: string; AIndex: integer; const ATrigger: PSERVICE_TRIGGER): PVirtualNode;
    function AddFromRegistry(const AServiceName: string; const AKeyPath: string; ATrigger: PSERVICE_TRIGGER): PVirtualNode;
    procedure ApplyFilter(Callback: TVTGetNodeProc; Data: pointer);

    function GetFacetData(ANode: PVirtualNode): PNdTriggerFacet; inline;
    function GetTriggerData(ANode: PVirtualNode): TNdTrigger; inline;

    function FocusedFacet: PNdTriggerFacet;
    function SelectedFacets: TArray<PNdTriggerFacet>;
    function AllFacets: TArray<PNdTriggerFacet>;
    function SelectedTriggers: TArray<TNdTrigger>;
    function AllTriggers: TArray<TNdTrigger>;

    property EntryMode: TTriggerEntryMode read FEntryMode write SetEntryMode;
    property OnFocusChanged: TTriggerEvent read FOnFocusChanged write FOnFocusChanged;

  end;


implementation
uses UITypes, Clipbrd, CommonResources, TriggerExport,
  RegFile, RegExport, Viper.TriggerEditor, Viper.TriggerImport, Registry;

{$R *.dfm}

{ Triggers }

constructor TNdTrigger.Create(AData: PSERVICE_TRIGGER; AOwnsData: boolean = true);
begin
  inherited Create;
  Self.FData := AData;
  Self.FOwnsData := AOwnsData;
end;

destructor TNdTrigger.Destroy;
begin
  if FOwnsData and (FData <> nil) then begin
    FreeMem(FData);
    FData := nil;
  end;
  inherited;
end;

constructor TNdTriggerList.Create;
begin
  //Strictly owns-objects=true, the TNdTriggers themselves are wrappers
  //and can choose if they need to destroy the underlying data.
  inherited Create({OwnsObjects=}true);
end;


{ Trigger facets }

const
  sTriggerFacetSummary = '%s %s on %s';
  sTriggerFacetSummaryParams = '%s %s on %s (%s)';

//Contains all important parts of the trigger, used to compactly refer to it like in confirmation dialogs
function TNdTriggerFacet.Summary: string;
begin
  if Self.Params = '' then
    Result := Format(sTriggerFacetSummary, [TriggerActionToString(Self.Action), Self.Trigger.ServiceName, Self.Description])
  else
    Result := Format(sTriggerFacetSummaryParams, [TriggerActionToString(Self.Action), Self.Trigger.ServiceName, Self.Description, Self.Params]);
end;


{ The form }

constructor TTriggerList.Create(AOwner: TComponent);
begin
  inherited;
  FTriggers := TNdTriggerList.Create;
  OnTriggerListChanged.Add(Self.HandleTriggerListChanged);
  FEntryMode := emMultiEntries; //by default
end;

destructor TTriggerList.Destroy;
begin
  if OnTriggerListChanged <> nil then
    OnTriggerListChanged.Remove(Self.HandleTriggerListChanged);
  FreeAndNil(FTriggers);
  inherited;
end;

procedure TTriggerList.TreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNdTriggerFacet);
end;

procedure TTriggerList.TreeInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var Data: PNdTriggerFacet;
begin
  Data := Sender.GetNodeData(Node);
  Initialize(Data^);
end;

procedure TTriggerList.TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var Data: PNdTriggerFacet;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TTriggerList.TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var Data: PNdTriggerFacet;
begin
  if TextType <> ttNormal then exit;

  Data := Sender.GetNodeData(Node);
  if Data = nil then exit;

  case Column of
    NoColumn, colTrigger:
      CellText := Data.Description;
    colAction:
      CellText := TriggerActionToString(Data.Action);
    colService:
      CellText := Data.Trigger.ServiceName;
    colParams:
      CellText := Data.Params;
  end;
end;

procedure TTriggerList.TreeGetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
var Data: PNdTriggerFacet;
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
    colService:
      ImageIndex := CommonRes.iService;
  end;
end;

procedure TTriggerList.TreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var Data: PNdTriggerFacet;
begin
  Data := Sender.GetNodeData(Node);

  if Data.Trigger.IsDisabled then begin
    TargetCanvas.Font.Color := clGray;
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsStrikeOut];
  end;
end;

//Checks for disabled/enabled triggers in the list
//ADisabled: value to check for.
function HaveDisabledTriggers(const sel: TArray<PNdTriggerFacet>; const ADisabled: boolean): boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to Length(sel)-1 do
    if sel[i].Trigger.IsDisabled = ADisabled then begin
      Result := true;
      break;
    end;
end;

procedure TTriggerList.TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var sel: TArray<PNdTriggerFacet>;
  HaveSCM, HaveDisabled: boolean;
begin
 //Selection changed
  sel := SelectedFacets();
  HaveSCM := HaveDisabledTriggers(sel, false);
  HaveDisabled := HaveDisabledTriggers(sel, true);

  aCopySummary.Visible := Tree.SelectedCount > 0;
  aCopyTriggerText.Visible := Tree.SelectedCount > 0;
  aCopySourceData.Visible := Tree.SelectedCount > 0;
  aCopyParams.Visible := Tree.SelectedCount > 0;
  miCopy.Visible := aCopySummary.Visible or aCopyTriggerText.Visible
    or aCopySourceData.Visible or aCopyParams.Visible;

  aEditTrigger.Visible := (Tree.SelectedCount = 1) and HaveSCM; //"Edit" only works on live triggers
  aExportTrigger.Visible := Tree.SelectedCount > 0;
  aExportAllTriggers.Visible := (Tree.RootNode.ChildCount > 0); //"Export all" is available if we have any triggers
  aDeleteTrigger.Visible := (Tree.SelectedCount > 0);
  aImportTrigger.Visible := true;

  aDisableTrigger.Visible := HaveSCM;
  aEnableTrigger.Visible := HaveDisabled;
end;

procedure TTriggerList.TreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var Data: PNdTriggerFacet;
begin
  if Node = nil then
    Data := nil
  else
    Data := Sender.GetNodeData(Node);
  if Assigned(Self.FOnFocusChanged) then
    Self.FOnFocusChanged(Self, Data);
end;

procedure TTriggerList.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and ((Key=Ord('C')) or (Key=Ord('c'))) then
    aCopySummary.Execute;
end;

procedure TTriggerList.TreeCollapsing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
begin
 //The only tree structure currently is child nodes in emChildEntries mode, don't collapse that:
  Allowed := false;
end;

procedure TTriggerList.TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var Data1, Data2: PNdTriggerFacet;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  if (Data1 = nil) or (Data2 = nil) then begin
    if Data1 = nil then
      if Data2 = nil then
        Result := 0
      else
        Result := -1
    else
      Result := +1;
    exit;
  end;

  case Column of
    NoColumn, colTrigger: begin
      Result := Data1.TriggerType - Data2.TriggerType;
      if Result = 0 then
        Result := CompareText(Data1.Description, Data2.Description);
    end;
    colAction:
      Result := Data1.Action - Data2.Action;
    colService:
      Result := CompareText(Data1.Trigger.ServiceName, Data2.Trigger.ServiceName);
    colParams:
      Result := CompareText(Data1.Params, Data2.Params);
  end;
end;

procedure TTriggerList.TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Button = mbLeft then begin
    if HitInfo.Column <> Sender.SortColumn then begin
      Sender.SortColumn := HitInfo.Column;
      Sender.SortDirection := sdAscending;
    end else
      case Sender.SortDirection of
        sdAscending: Sender.SortDirection := sdDescending;
        sdDescending: Sender.SortColumn := NoColumn;
      end;
    Sender.Treeview.Sort(nil, Sender.SortColumn, Sender.SortDirection);
  end;
end;


procedure TTriggerList.Clear;
begin
  Tree.Clear;
  FTriggers.Clear;
  //Reset the popup menu
  //We'd like to put this into FormCreate, but there's no FormCreates for TFrames.
  TreeChange(Tree, nil);
end;

{
Adds all trigger facet nodes for the given TNdTrigger.
This is the lowest level Add(). TNdTrigger will be automatically destroyed on Clear().
Returns the virtual node added. In multi-facet mode returns the first of such nodes.
}
function TTriggerList.Add(const ATrigger: TNdTrigger): PVirtualNode;
var Node: PVirtualNode;
  NodeData: PNdTriggerFacet;
  TriggerData: TTriggerData;
  Sources: TArray<TTriggerSource>;
  Source, NewSource: TTriggerSource;
begin
  FTriggers.Add(ATrigger);

  TriggerData := ParseTrigger(ATrigger.Data);
  Sources := TriggerData.Sources;
  if Length(Sources) <= 0 then begin
    SetLength(Sources, 1);
    Sources[0].DisplayText := '';
    Sources[0].Data := '';
  end;

  if (Self.FEntryMode = emSingleEntry) and (Length(Sources) > 0) then begin
    //Ugly. Squash all sources together into a single source if in multi-mode
    NewSource.DisplayText := '';
    NewSource.Data := '';
    for Source in Sources do begin
      NewSource.DisplayText := NewSource.DisplayText + Source.DisplayText + ', ';
      NewSource.Data := NewSource.Data + Source.DisplayText + ', ';
    end;
    SetLength(NewSource.DisplayText, Length(NewSource.DisplayText)-2);
    SetLength(NewSource.Data, Length(NewSource.Data)-2);
    //Replace the Sources list
    SetLength(Sources, 1);
    Sources[0] := NewSource;
    //Handle the rest normally
  end;

  Result := nil;
  for Source in Sources do begin
    if (Self.FEntryMode = emChildEntries) and (Result <> nil) then
      Node := Tree.AddChild(Result)
    else begin
      Node := Tree.AddChild(nil);
      Result := Node;
    end;
    Tree.ReinitNode(Node, false);
    NodeData := Tree.GetNodeData(Node);
    NodeData.Trigger := ATrigger;
    NodeData.TriggerType := ATrigger.Data^.dwTriggerType;
    NodeData.Action := ATrigger.Data^.dwAction;
    NodeData.TriggerSubtype := ATrigger.Data^.pTriggerSubtype^;
    NodeData.Source := Source;
    if Source.Data <> '' then
      NodeData.Description := TriggerData.Event + ' ('+Source.DisplayText+')'
    else
      NodeData.Description := TriggerData.Event;
    NodeData.Params := TriggerData.ParamsToString('; ');
  end;

  if Self.FEntryMode = emChildEntries then
    Self.Tree.Expanded[Result] := true; //auto-expand
end;

{
Adds the trigger object and all its facet nodes for a PSERVICE_TRIGGER from the
local PC's SCM.
Trigger data is copied. The original trigger data can be freed.
AIndex:
  Trigger index in the parent Service's trigger list.
}
function TTriggerList.AddFromScm(const AServiceName: string; AIndex: integer; const ATrigger: PSERVICE_TRIGGER): PVirtualNode;
var Trigger: TNdTrigger;
begin
  Trigger := TNdTrigger.Create(CopyTrigger(ATrigger^), {OwnsData=}true);
  Trigger.ServiceName := AServiceName;
  Trigger.Index := AIndex;
  Result := Self.Add(Trigger);
end;

//Adds an entry for a disabled trigger stored in the registry by a given key path
//Trigger data ownership is transferred. The caller must not use ATrigger anymore.
function TTriggerList.AddFromRegistry(const AServiceName: string; const AKeyPath: string; ATrigger: PSERVICE_TRIGGER): PVirtualNode;
var Trigger: TNdTrigger;
begin
  Trigger := TNdTrigger.Create(ATrigger, {OwnsData=}true);
  Trigger.ServiceName := AServiceName;
  Trigger.RegistryPath := AKeyPath;
  Trigger.IsDisabled := true;
  Result := Self.Add(Trigger);
end;


procedure TTriggerList.Reload;
var Services, S: PEnumServiceStatusProcess;
  ServicesReturned: cardinal;
  i: integer;
begin
  Clear;

  with OpenScm(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE) do try
    if not ShEnumServicesStatusEx(ScmHandle, SERVICE_TYPE_ALL, SERVICE_STATE_ALL, nil, Services, ServicesReturned) then
      RaiseLastOsError();

    S := Services;
    for i := 0 to ServicesReturned - 1 do begin
      LoadTriggersForService(ScmHandle, S.lpServiceName);
      Inc(S);
    end;
  finally
    if Services <> nil then
      FreeMem(Services);
  end;

  //Reset the popup menu
  //We'd like to put this into FormCreate, but there's no FormCreates for TFrames.
  TreeChange(Tree, nil);
end;

//Adds all triggers for a given service to the current list.
//Does not refresh the presentation: intended to be used in batch operations.
procedure TTriggerList.LoadTriggersForService(const AServiceName: string; const AServiceHandle: SC_HANDLE);
var triggers: PSERVICE_TRIGGER_INFO;
  i: integer;
begin
  triggers := QueryServiceTriggers(AServiceHandle);
  if triggers = nil then exit;
  try
    i := 0;
    while triggers.cTriggers > 0 do begin
      Self.AddFromScm(AServiceName, i, triggers.pTriggers);
      Inc(triggers.pTriggers);
      Dec(triggers.cTriggers);
      Inc(i);
    end;
  finally
    FreeMem(triggers);
  end;

  //Also load disabled triggers
  LoadDisabledTriggersForService(AServiceName);
end;

//Same but automatically opens and closes the service handle
procedure TTriggerList.LoadTriggersForService(const AScmHandle: SC_HANDLE; const AServiceName: string);
begin
  with OpenService2(AScmHandle, AServiceName, SERVICE_QUERY_CONFIG) do
    if SvcHandle <> 0 then
      LoadTriggersForService(AServiceName, SvcHandle);
end;

procedure TTriggerList.ApplyFilter(Callback: TVTGetNodeProc; Data: pointer);
begin
  Tree.BeginUpdate;
  try
    Tree.IterateSubtree(nil, Callback, Data, [], {DoInit=}true);
  finally
    Tree.EndUpdate;
  end;
end;

//Called when the trigger list for a given service changes
//This is not a guaranteed notification so we should be ready for changes without it
procedure TTriggerList.HandleTriggerListChanged(Sender: TObject; const AService: string);
begin
  Self.Reload; //For now, reload everything
end;

procedure TTriggerList.SetEntryMode(const Value: TTriggerEntryMode);
begin
  if FEntryMode = Value then exit;
  FEntryMode := Value;
  Self.Reload;
end;


function TTriggerList.GetFacetData(ANode: PVirtualNode): PNdTriggerFacet;
begin
  Result := Tree.GetNodeData(ANode);
end;

function TTriggerList.GetTriggerData(ANode: PVirtualNode): TNdTrigger;
begin
  Result := Self.GetFacetData(ANode).Trigger;
end;

function TTriggerList.FocusedFacet: PNdTriggerFacet;
begin
  if Tree.FocusedNode = nil then
    Result := nil
  else
    Result := Tree.GetNodeData(Tree.FocusedNode);
end;

//Returns selected trigger node list as a TArray<>
function TTriggerList.SelectedFacets: TArray<PNdTriggerFacet>;
var ANode: PVirtualNode;
begin
  SetLength(Result, 0);
  for ANode in Tree.SelectedNodes() do begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := Tree.GetNodeData(ANode);
  end;
end;

function TTriggerList.AllFacets: TArray<PNdTriggerFacet>;
var ANode: PVirtualNode;
begin
  SetLength(Result, 0);
  for ANode in Tree.ChildNodes(Tree.RootNode) do begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := Tree.GetNodeData(ANode);
  end;
end;

//A single trigger can result in multiple list entries (e.g. if it lists several ports)
//This returns a list of unique triggers for the selected nodes.
function TTriggerList.NodesToTriggers(const ANodes: TVTVirtualNodeEnumeration): TArray<TNdTrigger>;
var ANode: PVirtualNode;
  AData: PNdTriggerFacet;
  i: integer;
  found: boolean;
begin
  SetLength(Result, 0);
  for ANode in ANodes do begin
    AData := Tree.GetNodeData(ANode);

    found := false;
    for i := 0 to Length(Result)-1 do
      if Result[i]=AData.Trigger then begin
        found := true;
        break;
      end;
    if found then
      continue;

    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := AData.Trigger;
  end;
end;

function TTriggerList.SelectedTriggers: TArray<TNdTrigger>;
begin
  Result := Self.NodesToTriggers(Tree.SelectedNodes());
end;

function TTriggerList.AllTriggers: TArray<TNdTrigger>;
begin
  Result := Self.NodesToTriggers(Tree.ChildNodes(Tree.RootNode));
end;

procedure TTriggerList.aCopySummaryExecute(Sender: TObject);
var Data: PNdTriggerFacet;
  Result: string;
begin
  Result := '';
  for Data in SelectedFacets do
    Result := Result + Data.Summary + #13#10;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

procedure TTriggerList.aCopyTriggerTextExecute(Sender: TObject);
var Data: PNdTriggerFacet;
  Result: string;
begin
  Result := '';

  for Data in SelectedFacets do
    Result := Result + Data.Description + #13#10;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

procedure TTriggerList.aCopySourceDataExecute(Sender: TObject);
var Data: PNdTriggerFacet;
  Result: string;
begin
  Result := '';

  for Data in SelectedFacets do
    Result := Result + Data.Source.Data + #13#10;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

procedure TTriggerList.aCopyParamsExecute(Sender: TObject);
var Data: PNdTriggerFacet;
  Result: string;
begin
  Result := '';

  for Data in SelectedFacets do
    Result := Result + Data.Params + #13#10;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

procedure TTriggerList.aCopyTriggerRegDefinitionExecute(Sender: TObject);
var Data: PNdTriggerFacet;
  Result: string;
  i: integer;
begin
  Result := '';

  i := 0;
  for Data in SelectedFacets do begin
    Result := Result
      + ExportTrigger(Data.Trigger.Data^, IntToStr(i)).ExportToString + #13#10;
    Inc(i);
  end;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

procedure TTriggerList.aEditTriggerExecute(Sender: TObject);
var Sel: TArray<PNdTriggerFacet>;
  EditForm: TTriggerEditorForm;
  TriggerData: PSERVICE_TRIGGER;
begin
  Sel := Self.SelectedFacets;
  if Length(Sel) <> 1 then exit;

  //"Edit" only works on live triggers
  if Sel[0].Trigger.IsDisabled then exit;

  TriggerData := nil;

  EditForm := TTriggerEditorForm.Create(Self);
  try
   //Make our own copy so as not to edit Node copy directly
    TriggerData := CopyTrigger(Sel[0].Trigger.Data^);
    if not IsPositiveResult(EditForm.EditTrigger(TriggerData)) then
      exit; //TriggerData is freed in Finally

   //Store the edited trigger in the service config
    with OpenService2(Sel[0].Trigger.ServiceName,
      STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
      SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
    begin
      ChangeServiceTrigger(SvcHandle, Sel[0].Trigger.Data^, TriggerData^);
      TriggerUtils.TriggerListChanged(Self, Sel[0].Trigger.ServiceName);
    end;

  finally
    FreeAndNil(EditForm);
    if TriggerData <> nil then
      FreeMem(TriggerData);
  end;

 //Reload the list of triggers
  {
  This is needed since our single trigger could have had resulted in several
  entries in the list, and our edited trigger can result in a different set of those.

  We could delete all related nodes (with the same TriggerCopy) and re-create
  all appropriate ones from new data, but for now lets keep it simple.
  (+ we would need to insert new nodes at the position of the old ones,
  and we don't yet have proper ordering)
  }
  Self.Reload;
end;


procedure DeleteTriggerRegistryKeys(const ATriggerKeys: array of string); forward;

resourcestring
  sConfirmTriggerDeletionCaption = 'Confirm deletion';
  sConfirmTriggerDeletion = 'Do you really want to delete these triggers?';

procedure TTriggerList.aDeleteTriggerExecute(Sender: TObject);
var Sel: TArray<PNdTriggerFacet>;
  ConfirmationText: string;
  ScmTriggers: array of PSERVICE_TRIGGER;
  RegTriggers: array of string;
  i: integer;
begin
  Sel := SelectedFacets;
  if Length(Sel) <= 0 then exit;

  ConfirmationText := sConfirmTriggerDeletion;
  for i := 0 to Length(Sel)-1 do
    ConfirmationText := ConfirmationText + #13 + '* ' + Sel[i].Summary;
  if MessageBox(Self.Handle, PChar(ConfirmationText),
    PChar(sConfirmTriggerDeletionCaption),
    MB_ICONQUESTION or MB_YESNO) <> ID_YES then
    exit;

  //We delete both live and registry-key triggers, but by different means

  //Split into arrays by type:
  SetLength(ScmTriggers, 0);
  SetLength(RegTriggers, 0);
  for i := 0 to Length(Sel)-1 do
    if Sel[i].Trigger.IsDisabled then begin
      SetLength(RegTriggers, Length(RegTriggers)+1);
      RegTriggers[Length(RegTriggers)-1] := Sel[i].Trigger.RegistryPath;
    end else begin
      SetLength(ScmTriggers, Length(ScmTriggers)+1);
      ScmTriggers[Length(ScmTriggers)-1] := Sel[i].Trigger.Data;
    end;

  //Delete SCM triggers
  with OpenService2(Sel[0].Trigger.ServiceName,
    STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
    SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
  begin
    DeleteServiceTriggers(SvcHandle, ScmTriggers);
  end;

  //Delete registry triggers
  DeleteTriggerRegistryKeys(RegTriggers);

  TriggerUtils.TriggerListChanged(Self, '');
  Reload;
end;


//Exports triggers for all given nodes
procedure TTriggerList.TryExportTriggers(const Sel: TArray<TNdTrigger>);
var sl: TStringList;
  Trigger: TNdTrigger;
begin
  if Length(Sel) <= 0 then exit;

  with SaveTriggersDialog do
    if not Execute then
      exit;

  sl := TStringList.Create;
  try
    for Trigger in Sel do
      //All triggers are exported under their full registry path + their real index
      ExportTrigger(
        Trigger.Data^,
        GetTriggerKeyFull(Trigger.ServiceName) + '\' + IntToStr(Trigger.Index)
      ).ExportToStrings(sl);

    sl.SaveToFile(SaveTriggersDialog.FileName);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TTriggerList.aExportTriggerExecute(Sender: TObject);
begin
  TryExportTriggers(Self.SelectedTriggers);
end;

//"Export All" is available but not included in the menus by default, because it
//makes little sense for multi-service trigger lists. Descendants can include it.
procedure TTriggerList.aExportAllTriggersExecute(Sender: TObject);
begin
  TryExportTriggers(Self.AllTriggers);
end;

//Queries the file name and starts the trigger import process.
//Supports both single-trigger version (pass AServiceName) and multi-trigger one (pass '')
procedure TTriggerList.TryImportTriggers(const AServiceName: string = '');
begin
  with OpenTriggersDialog do
    if not Execute then
      exit;
  Viper.TriggerImport.ImportTriggers(Self, AServiceName, OpenTriggersDialog.FileName);
  Self.Reload;
end;

procedure TTriggerList.aImportTriggerExecute(Sender: TObject);
begin
  //Multi-trigger version
  Self.TryImportTriggers('');
end;


{ Disable / enable triggers }

const
  sDisabledServicesKey = '\Software\Viper\DisabledServices'; //in HKLM

function GetDisabledServiceKey(const AServiceName: string): string;
begin
  Result := sDisabledServicesKey+'\'+AServiceName;
end;

function GetDisabledTriggersKey(const AServiceName: string): string;
begin
  Result := GetDisabledServiceKey(AServiceName) + '\TriggerInfo'
end;

//Generates a new, unused key name in DisabledServices\ServiceName\Triggers\
function GetNewDisabledTriggerKey(const AServiceName: string): string;
var guid: TGuid;
begin
  //Real SERVICES key stores triggers under integer indexes,
  //but DisabledServices is not meant to be export-compatible so we don't have to.
  CreateGuid(guid);
  Result := GetDisabledTriggersKey(AServiceName) + '\' + GuidToString(guid);
end;

{
Writes a given trigger to the given registry key (possibly unrelated).
Trigger is written in a format identical to the one used by the SCM.
FullKeyPath must include HKEY_* root key.
}
procedure WriteTriggerToRegistryKey(reg: TRegistry; Trigger: PSERVICE_TRIGGER;
  const FullKeyPath: string);
var key: TRegFileKey;
begin
  //Export the trigger to Reg file format structure + import the structure
  key := ExportTrigger(Trigger^, FullKeyPath);
  WriteToRegistry(reg, key);
end;

{
Reads a SCM compatible trigger definition from a currently open registry key
and creates a new PSERVICE_TRIGGER out of it.
Returns
  PSERVICE_TRIGGER which has to be freed by the caller.
  nil if the key does not contain a valid trigger registry definition
}
function CreateTriggerFromRegistryKey(reg: TRegistry): PSERVICE_TRIGGER;
var regf: TRegFile;
begin
  //Export + parse as trigger in standard way
  Result := nil;
  regf := ExportRegistryKey(reg);
  if regf = nil then
    exit;
  try
    //The trigger key itself is flat. Someone could have manually created subfolders,
    //but we are going to ignore them
    if regf.Count < 1 then
      exit;
    Result := ImportTrigger(regf[0]);
  finally
    FreeAndNil(regf);
  end;
end;

//Each key: a registry path without HKEY
procedure DeleteTriggerRegistryKeys(const ATriggerKeys: array of string);
var reg: TRegistry;
  key: string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    for key in ATriggerKeys do
      reg.DeleteKey(key);
  finally
    FreeAndNil(reg);
  end;
end;

//Loads additional disabled triggers from the special registry key
procedure TTriggerList.LoadDisabledTriggersForService(const AServiceName: string);
var reg: TRegistry;
  subkeys: TStringList;
  subkey: string;
  trig: PSERVICE_TRIGGER;
begin
  subkeys := nil;
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if not reg.OpenKeyReadOnly(GetDisabledTriggersKey(AServiceName)) then
      exit; //no disabled ones

    subkeys := TStringList.Create;
    reg.GetKeyNames(subkeys);
    for subkey in subkeys do begin
      if not reg.OpenKeyReadOnly(GetDisabledTriggersKey(AServiceName)+'\'+subkey) then
        continue; //cannot read, don't complain
      trig := CreateTriggerFromRegistryKey(reg);
      Self.AddFromRegistry(AServiceName, reg.CurrentPath, trig); //takes over the data
    end;
  finally
    FreeAndNil(reg);
    FreeAndNil(subkeys);
  end;
end;

procedure TTriggerList.aDisableTriggerExecute(Sender: TObject);
var Sel: TArray<TNdTrigger>;
  Trigger: TNdTrigger;
  reg: TRegistry;
begin
  Sel := SelectedTriggers;
  if Length(Sel) <= 0 then exit;

  reg := nil;
  try
    reg := TRegistry.Create;
    reg.RootKey := HKEY_LOCAL_MACHINE;

    for Trigger in Sel do begin
      if Trigger.IsDisabled then continue; //already disabled

      //Create the disabled version
      WriteTriggerToRegistryKey(
        reg,
        Trigger.Data,
        sHkeyLocalMachine + GetNewDisabledTriggerKey(Trigger.ServiceName)
      );

      //Delete the live version
      with OpenService2(Trigger.ServiceName, SC_MANAGER_CONNECT, SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
        DeleteServiceTriggers(SvcHandle, [Trigger.Data]);

      //TODO: Maybe aggregate changes and edit each service only once?
    end;
  finally
    FreeAndNil(reg);
  end;

  TriggerUtils.TriggerListChanged(Self, ''); //triggers reload
end;

procedure TTriggerList.aEnableTriggerExecute(Sender: TObject);
var Sel: TArray<TNdTrigger>;
  Trigger: TNdTrigger;
  reg: TRegistry;
  trig: PSERVICE_TRIGGER;
begin
  Sel := SelectedTriggers;
  if Length(Sel) <= 0 then exit;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    for Trigger in Sel do begin
      if not Trigger.IsDisabled then continue;
      if Trigger.RegistryPath = '' then continue;

      if not reg.OpenKeyReadOnly(Trigger.RegistryPath) then
        continue; //cannot read, don't complain

      trig := CreateTriggerFromRegistryKey(reg);
      try
        with OpenService2(Trigger.ServiceName, SC_MANAGER_CONNECT,
          SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG)
        do
          AddServiceTriggers(SvcHandle, [Trigger.Data^]);
      finally
        FreeMem(trig);
      end;

      //If the above worked, delete the disabled instance
      reg.DeleteKey('\'+reg.CurrentPath);
    end;

  finally
    FreeAndNil(reg);
  end;

  TriggerUtils.TriggerListChanged(Self, ''); //triggers reload
end;

end.
