unit Viper.TriggerList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, ImgList, WinSvc, ServiceHelper, Vcl.Menus, System.Actions, Vcl.ActnList,
  TriggerUtils;

type
  TNdTriggerData = record
    TriggerType: DWORD;
    TriggerSubtype: TGUID;
    Description: string;
    Source: TTriggerSource;
    Params: string;
    Action: DWORD;
    ServiceName: string;
    //Our own trigger copy, stored in the main form.
    //List entries which spawn from the same trigger will have the same pointer.
    TriggerCopy: PSERVICE_TRIGGER;
    function Summary: string;
  end;
  PNdTriggerData = ^TNdTriggerData;

  TTriggerEvent = procedure(Sender: TObject; const TriggerData: PNdTriggerData) of object;

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
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure aCopyTriggerTextExecute(Sender: TObject);
    procedure aCopySourceDataExecute(Sender: TObject);
    procedure aCopyParamsExecute(Sender: TObject);
    procedure aDeleteTriggerExecute(Sender: TObject);
    procedure aEditTriggerExecute(Sender: TObject);
    procedure aCopyTriggerRegDefinitionExecute(Sender: TObject);
    procedure aExportTriggerExecute(Sender: TObject);
    procedure aExportAllTriggersExecute(Sender: TObject);
    procedure TreeCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure TreeHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure TreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
  const
    colTrigger = 0;
    colAction = 1;
    colService = 2;
    colParams = 3;
  protected
    FOwnTriggerCopies: TArray<PSERVICE_TRIGGER>;
    FOnFocusChanged: TTriggerEvent;
    procedure Add(const AServiceName: string; const ATrigger: PSERVICE_TRIGGER);
    function NodesToUniqueTriggers(const ANodes: TVTVirtualNodeEnumeration): TArray<PSERVICE_TRIGGER>;
    procedure TryExportTriggers(const Sel: TArray<PSERVICE_TRIGGER>);
    procedure LoadTriggersForService(const AScmHandle: SC_HANDLE; const AServiceName: string); overload;
    procedure LoadTriggersForService(const AServiceName: string; const AServiceHandle: SC_HANDLE); overload;
    procedure HandleTriggerListChanged(Sender: TObject; const AService: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Reload; virtual;
    procedure ApplyFilter(Callback: TVTGetNodeProc; Data: pointer);
    function SelectedTriggers: TArray<PNdTriggerData>;
    function SelectedUniqueTriggers: TArray<PSERVICE_TRIGGER>;
    function AllUniqueTriggers: TArray<PSERVICE_TRIGGER>;
    function FocusedTrigger: PNdTriggerData;
    property OnFocusChanged: TTriggerEvent read FOnFocusChanged write FOnFocusChanged;

  end;


implementation
uses UITypes, Clipbrd, CommonResources, TriggerExport, Viper.TriggerEditor;

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


constructor TTriggerList.Create(AOwner: TComponent);
begin
  inherited;
  OnTriggerListChanged.Add(Self.HandleTriggerListChanged);
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

procedure TTriggerList.TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
 //Selection changed
  aCopySummary.Visible := Tree.SelectedCount > 0;
  aCopyTriggerText.Visible := Tree.SelectedCount > 0;
  aCopySourceData.Visible := Tree.SelectedCount > 0;
  aCopyParams.Visible := Tree.SelectedCount > 0;
  miCopy.Visible := aCopySummary.Visible or aCopyTriggerText.Visible
    or aCopySourceData.Visible or aCopyParams.Visible;
  aEditTrigger.Visible := Tree.SelectedCount = 1;
  aDisableTrigger.Visible := Tree.SelectedCount > 0;
  aExportTrigger.Visible := Tree.SelectedCount > 0;
  aExportAllTriggers.Visible := (Tree.RootNode.ChildCount > 0); //"Export all" is available if we have any triggers
  aDeleteTrigger.Visible := Tree.SelectedCount > 0;
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

//Adds a trigger to the list. Trigger data is not ours and should be copied if needed.
procedure TTriggerList.Add(const AServiceName: string; const ATrigger: PSERVICE_TRIGGER);
var Node: PVirtualNode;
  NodeData: PNdTriggerData;
  TriggerData: TTriggerData;
  Sources: TArray<TTriggerSource>;
  Source: TTriggerSource;
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

  for Source in Sources do begin
    Node := Tree.AddChild(nil);
    Tree.ReinitNode(Node, false);
    NodeData := Tree.GetNodeData(Node);
    NodeData.ServiceName := AServiceName;
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
  end;
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
begin
  triggers := QueryServiceTriggers(AServiceHandle);
  if triggers = nil then exit;
  try
    while triggers.cTriggers > 0 do begin
      Self.Add(AServiceName, triggers.pTriggers);
      Inc(triggers.pTriggers);
      Dec(triggers.cTriggers);
    end;
  finally
    FreeMem(triggers);
  end;
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


function TTriggerList.FocusedTrigger: PNdTriggerData;
begin
  if Tree.FocusedNode = nil then
    Result := nil
  else
    Result := Tree.GetNodeData(Tree.FocusedNode);
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
      + string(ExportTrigger(Data.TriggerCopy^, AnsiString(IntToStr(i)))) + #13#10;
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


procedure TTriggerList.TryExportTriggers(const Sel: TArray<PSERVICE_TRIGGER>);
var Text: AnsiString;
  sl: TStringList;
begin
  if Length(Sel) <= 0 then exit;

  with SaveTriggersDialog do
    if not Execute then
      exit;

  Text := ExportTriggers(Sel);

  sl := TStringList.Create;
  try
    sl.SetText(PChar(string(Text)));
    sl.SaveToFile(SaveTriggersDialog.FileName);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TTriggerList.aExportTriggerExecute(Sender: TObject);
begin
  TryExportTriggers(Self.SelectedUniqueTriggers);
end;

//"Export All" is available but not included in the menus by default, because it
//makes little sense for multi-service trigger lists. Descendants can include it.
procedure TTriggerList.aExportAllTriggersExecute(Sender: TObject);
begin
  TryExportTriggers(Self.AllUniqueTriggers);
end;


end.
