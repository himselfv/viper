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
  TriggerUtils;

type
  TNdTriggerData = record
    ServiceName: string;   //The service which stores this trigger
    TriggerIndex: integer; //The index of this trigger in the service's trigger list. 0-based
                           //WARNING: Will change if we delete/add triggers.
    //Our own trigger copy, stored in the main form.
    //List entries which spawn from the same trigger will have the same pointer.
    TriggerCopy: PSERVICE_TRIGGER;
    //Cached details of this particular entry
    Description: string;
    Source: TTriggerSource;
    Params: string;
    TriggerType: DWORD;
    TriggerSubtype: TGUID;
    Action: DWORD;
    IsDisabled: boolean;
    function Summary: string;
  end;
  PNdTriggerData = ^TNdTriggerData;

  TTriggerEvent = procedure(Sender: TObject; const TriggerData: PNdTriggerData) of object;

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
    Enable1: TMenuItem;
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
    FOwnTriggerCopies: TArray<PSERVICE_TRIGGER>;
    FEntryMode: TTriggerEntryMode;
    FOnFocusChanged: TTriggerEvent;
    function NodesToUniqueTriggers(const ANodes: TVTVirtualNodeEnumeration): TArray<PSERVICE_TRIGGER>;
    procedure TryExportTriggers(const Sel: TArray<PNdTriggerData>);
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
    function Add(const AServiceName: string; AIndex: integer; const ATrigger: PSERVICE_TRIGGER): PVirtualNode;
    procedure ApplyFilter(Callback: TVTGetNodeProc; Data: pointer);

    function SelectedTriggers: TArray<PNdTriggerData>;
    function AllTriggers: TArray<PNdTriggerData>;
    function SelectedUniqueTriggers: TArray<PSERVICE_TRIGGER>;
    function AllUniqueTriggers: TArray<PSERVICE_TRIGGER>;
    function FocusedTrigger: PNdTriggerData;
    property EntryMode: TTriggerEntryMode read FEntryMode write SetEntryMode;
    property OnFocusChanged: TTriggerEvent read FOnFocusChanged write FOnFocusChanged;

  end;


implementation
uses UITypes, Clipbrd, Generics.Collections, CommonResources, TriggerExport,
  RegFile, RegExport, Viper.TriggerEditor, Viper.TriggerImport, Registry;

{$R *.dfm}

const
  sTriggerSummary = '%s %s on %s';
  sTriggerSummaryParams = '%s %s on %s (%s)';

//Contains all important parts of the trigger, used to compactly refer to it like in confirmation dialogs
function TNdTriggerData.Summary: string;
begin
  if Self.Params = '' then
    Result := Format(sTriggerSummary, [TriggerActionToString(Self.Action), Self.ServiceName, Self.Description])
  else
    Result := Format(sTriggerSummaryParams, [TriggerActionToString(Self.Action), Self.ServiceName, Self.Description, Self.Params]);
end;


//We often need to do something only once for each PTRIGGER_DATA.
//This structure makes it easier to keep track
type
  TTriggerSet = TArray<PSERVICE_TRIGGER>;
  TTriggerSetHelper = record helper for TTriggerSet
    function Contains(ATrigger: PSERVICE_TRIGGER): boolean;
    function UniqueAdd(ATrigger: PSERVICE_TRIGGER): boolean;
  end;

function TTriggerSetHelper.Contains(ATrigger: PSERVICE_TRIGGER): boolean;
var i: integer;
begin
  Result := false;
  for i := Low(Self) to High(Self) do
    if Self[i] = ATrigger then begin
      Result := true;
      break;
    end;
end;

//True if uniquely added to the list, False if already present
function TTriggerSetHelper.UniqueAdd(ATrigger: PSERVICE_TRIGGER): boolean;
begin
  Result := not Contains(ATrigger);
  if Result then begin
    SetLength(Self, Length(Self)+1);
    Self[Length(Self)-1] := ATrigger;
  end;
end;


constructor TTriggerList.Create(AOwner: TComponent);
begin
  inherited;
  OnTriggerListChanged.Add(Self.HandleTriggerListChanged);
  FEntryMode := emMultiEntries; //by default
end;

destructor TTriggerList.Destroy;
begin
  if OnTriggerListChanged <> nil then
    OnTriggerListChanged.Remove(Self.HandleTriggerListChanged);
  inherited;
end;

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
    colService:
      CellText := Data.ServiceName;
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
    colService:
      ImageIndex := CommonRes.iService;
  end;
end;

procedure TTriggerList.TreePaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var Data: PNdTriggerData;
begin
  Data := Sender.GetNodeData(Node);

  if Data.IsDisabled then begin
    TargetCanvas.Font.Color := clGray;
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsStrikeOut];
  end;
end;

//Checks for disabled/enabled triggers in the list
//ADisabled: value to check for.
function HaveDisabledTriggers(const sel: TArray<PNdTriggerData>; const ADisabled: boolean): boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to Length(sel)-1 do
    if sel[i].IsDisabled = ADisabled then begin
      Result := true;
      break;
    end;
end;

procedure TTriggerList.TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var sel: TArray<PNdTriggerData>;
begin
 //Selection changed
  aCopySummary.Visible := Tree.SelectedCount > 0;
  aCopyTriggerText.Visible := Tree.SelectedCount > 0;
  aCopySourceData.Visible := Tree.SelectedCount > 0;
  aCopyParams.Visible := Tree.SelectedCount > 0;
  miCopy.Visible := aCopySummary.Visible or aCopyTriggerText.Visible
    or aCopySourceData.Visible or aCopyParams.Visible;
  aEditTrigger.Visible := Tree.SelectedCount = 1;
  aExportTrigger.Visible := Tree.SelectedCount > 0;
  aExportAllTriggers.Visible := (Tree.RootNode.ChildCount > 0); //"Export all" is available if we have any triggers
  aDeleteTrigger.Visible := Tree.SelectedCount > 0;
  aImportTrigger.Visible := true;

  sel := SelectedTriggers();
  aDisableTrigger.Visible := HaveDisabledTriggers(sel, false);
  aEnableTrigger.Visible := HaveDisabledTriggers(sel, true);
end;

procedure TTriggerList.TreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var Data: PNdTriggerData;
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
var Data1, Data2: PNdTriggerData;
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
      Result := CompareText(Data1.ServiceName, Data2.ServiceName);
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
var i: integer;
begin
  Tree.Clear;
  for i := 0 to Length(FOwnTriggerCopies)-1 do
    FreeMem(FOwnTriggerCopies[i]);
  SetLength(FOwnTriggerCopies, 0);
  //Reset the popup menu
  //We'd like to put this into FormCreate, but there's no FormCreates for TFrames.
  TreeChange(Tree, nil);
end;

{
Adds a trigger to the list.
Trigger data is copied. The original trigger data can be freed.
Returns the virtual node added. In single-trigger-multi-node mode returns the first of such nodes.

AIndex:
  Trigger index in the parent Service's trigger list. <0 if the trigger is disabled.
}
function TTriggerList.Add(const AServiceName: string; AIndex: integer; const ATrigger: PSERVICE_TRIGGER): PVirtualNode;
var Node: PVirtualNode;
  NodeData: PNdTriggerData;
  TriggerData: TTriggerData;
  Sources: TArray<TTriggerSource>;
  Source, NewSource: TTriggerSource;
  TriggerCopy: PSERVICE_TRIGGER;
begin
  TriggerData := ParseTrigger(ATrigger);
  Sources := TriggerData.Sources;
  if Length(Sources) <= 0 then begin
    SetLength(Sources, 1);
    Sources[0].DisplayText := '';
    Sources[0].Data := '';
  end;

  TriggerCopy := CopyTrigger(ATrigger^);
  SetLength(FOwnTriggerCopies, Length(FOwnTriggerCopies)+1);
  FOwnTriggerCopies[Length(FOwnTriggerCopies)-1] := TriggerCopy;

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
    NodeData.ServiceName := AServiceName;
    NodeData.TriggerIndex := AIndex;
    NodeData.TriggerType := ATrigger^.dwTriggerType;
    NodeData.Action := ATrigger^.dwAction;
    NodeData.TriggerSubtype := ATrigger.pTriggerSubtype^;
    NodeData.Source := Source;
    if Source.Data <> '' then
      NodeData.Description := TriggerData.Event + ' ('+Source.DisplayText+')'
    else
      NodeData.Description := TriggerData.Event;
    NodeData.Params := TriggerData.ParamsToString('; ');
    NodeData.TriggerCopy := TriggerCopy;
    NodeData.IsDisabled := AIndex < 0;
  end;

  if Self.FEntryMode = emChildEntries then
    Self.Tree.Expanded[Result] := true; //auto-expand
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
      Self.Add(AServiceName, i, triggers.pTriggers);
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


function TTriggerList.FocusedTrigger: PNdTriggerData;
begin
  if Tree.FocusedNode = nil then
    Result := nil
  else
    Result := Tree.GetNodeData(Tree.FocusedNode);
end;

//Returns selected trigger node list as a TArray<>
function TTriggerList.SelectedTriggers: TArray<PNdTriggerData>;
var ANode: PVirtualNode;
begin
  SetLength(Result, 0);
  for ANode in Tree.SelectedNodes() do begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := Tree.GetNodeData(ANode);
  end;
end;

function TTriggerList.AllTriggers: TArray<PNdTriggerData>;
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
function TTriggerList.NodesToUniqueTriggers(const ANodes: TVTVirtualNodeEnumeration): TArray<PSERVICE_TRIGGER>;
var ANode: PVirtualNode;
  AData: PNdTriggerData;
  i: integer;
  found: boolean;
begin
  SetLength(Result, 0);
  for ANode in ANodes do begin
    AData := Tree.GetNodeData(ANode);

    found := false;
    for i := 0 to Length(Result)-1 do
      if Result[i]=AData.TriggerCopy then begin
        found := true;
        break;
      end;
    if found then
      continue;

    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := AData.TriggerCopy;
  end;
end;

function TTriggerList.SelectedUniqueTriggers: TArray<PSERVICE_TRIGGER>;
begin
  Result := Self.NodesToUniqueTriggers(Tree.SelectedNodes());
end;

function TTriggerList.AllUniqueTriggers: TArray<PSERVICE_TRIGGER>;
begin
  Result := Self.NodesToUniqueTriggers(Tree.ChildNodes(Tree.RootNode));
end;

procedure TTriggerList.aCopySummaryExecute(Sender: TObject);
var Data: PNdTriggerData;
  Result: string;
begin
  Result := '';
  for Data in SelectedTriggers do
    Result := Result + Data.Summary + #13#10;

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

procedure TTriggerList.aCopyTriggerRegDefinitionExecute(Sender: TObject);
var Data: PNdTriggerData;
  Result: string;
  i: integer;
begin
  Result := '';

  i := 0;
  for Data in SelectedTriggers do begin
    Result := Result
      + ExportTrigger(Data.TriggerCopy^, IntToStr(i)).ExportToString + #13#10;
    Inc(i);
  end;

  if Length(Result) >= 2 then
    SetLength(Result, Length(Result)-2);
  Clipboard.AsText := Result;
end;

procedure TTriggerList.aEditTriggerExecute(Sender: TObject);
var Sel: TArray<PNdTriggerData>;
  EditForm: TTriggerEditorForm;
  TriggerData: PSERVICE_TRIGGER;
begin
  Sel := Self.SelectedTriggers;
  if Length(Sel) <> 1 then exit;

  TriggerData := nil;

  EditForm := TTriggerEditorForm.Create(Self);
  try
   //Make our own copy so as not to edit Node copy directly
    TriggerData := CopyTrigger(Sel[0].TriggerCopy^);
    if not IsPositiveResult(EditForm.EditTrigger(TriggerData)) then
      exit; //TriggerData is freed in Finally

   //Store the edited trigger in the service config
    with OpenService2(Sel[0].ServiceName,
      STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
      SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
    begin
      ChangeServiceTrigger(SvcHandle, Sel[0].TriggerCopy^, TriggerData^);
      TriggerUtils.TriggerListChanged(Self, Sel[0].ServiceName);
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

resourcestring
  sConfirmTriggerDeletionCaption = 'Confirm deletion';
  sConfirmTriggerDeletion = 'Do you really want to delete these triggers?';


procedure TTriggerList.aDeleteTriggerExecute(Sender: TObject);
var Sel: TArray<PNdTriggerData>;
  SelData: array of SERVICE_TRIGGER;
  i: integer;
  ConfirmationText: string;
begin
  Sel := SelectedTriggers;
  if Length(Sel) <= 0 then exit;

  ConfirmationText := sConfirmTriggerDeletion;
  for i := 0 to Length(Sel)-1 do
    ConfirmationText := ConfirmationText + #13 + '* ' + Sel[i].Summary;
  if MessageBox(Self.Handle, PChar(ConfirmationText),
    PChar(sConfirmTriggerDeletionCaption),
    MB_ICONQUESTION or MB_YESNO) <> ID_YES then
    exit;

  SetLength(SelData, Length(Sel));
  for i := 0 to Length(Sel)-1 do
    SelData[i] := Sel[i].TriggerCopy^;
  with OpenService2(Sel[0].ServiceName,
    STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
    SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
  begin
    DeleteServiceTriggers(SvcHandle, SelData);
    TriggerUtils.TriggerListChanged(Self, '');
  end;
  Reload;
end;


//Exports triggers for all given nodes
procedure TTriggerList.TryExportTriggers(const Sel: TArray<PNdTriggerData>);
var nl: TTriggerSet;
  sl: TStringList;
  Node: PNdTriggerData;
begin
  if Length(Sel) <= 0 then exit;

  with SaveTriggersDialog do
    if not Execute then
      exit;

  sl := TStringList.Create;
  try
    for Node in Sel do begin
      if not nl.UniqueAdd(Node.TriggerCopy) then continue;

      //All triggers are exported under their full registry path + their real index
      ExportTrigger(
        Node.TriggerCopy^,
        GetTriggerKeyFull(Node.ServiceName) + '\' + IntToStr(Node.TriggerIndex)
      ).ExportToStrings(sl);
    end;

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
      try
        Self.Add(AServiceName, -1, trig); //copies the data
      finally
        FreeMem(trig);
      end;
    end;
  finally
    FreeAndNil(reg);
    FreeAndNil(subkeys);
  end;
end;

procedure TTriggerList.aDisableTriggerExecute(Sender: TObject);
var Sel: TArray<PNdTriggerData>;
  Node: PNdTriggerData;
  nl: TTriggerSet;
  reg: TRegistry;
begin
  Sel := SelectedTriggers;
  if Length(Sel) <= 0 then exit;

  reg := nil;
  try
    reg := TRegistry.Create;
    reg.RootKey := HKEY_LOCAL_MACHINE;

    for Node in Sel do begin
      if Node.IsDisabled then continue; //already disabled
      if not nl.UniqueAdd(Node.TriggerCopy) then continue;

      WriteTriggerToRegistryKey(
        reg,
        Node.TriggerCopy,
        sHkeyLocalMachine + GetNewDisabledTriggerKey(Node.ServiceName)
      );

      //TODO: Delete the trigger from the service
      //TODO: Maybe aggregate changes and edit each service only once?

    end;
  finally
    FreeAndNil(reg);
  end;

  TriggerUtils.TriggerListChanged(Self, ''); //triggers reload
end;

procedure TTriggerList.aEnableTriggerExecute(Sender: TObject);
var Sel: TArray<PNdTriggerData>;
  Node: PNdTriggerData;
  nl: TTriggerSet;
  reg: TRegistry;
  trig: PSERVICE_TRIGGER;
begin
  Sel := SelectedTriggers;
  if Length(Sel) <= 0 then exit;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    for Node in Sel do begin
      if Node.IsDisabled then exit; //already disabled
      if not nl.UniqueAdd(Node.TriggerCopy) then continue;

      trig := CreateTriggerFromRegistryKey(reg);
      try
        //TODO: Import as trigger
      finally
        FreeMem(trig);
      end;

      //TODO: Delete key node
    end;

  finally
    FreeAndNil(reg);
  end;

  TriggerUtils.TriggerListChanged(Self, ''); //triggers reload
end;

end.
