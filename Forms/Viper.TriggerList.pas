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
    //Our own trigger copy, stored in the main form.
    //List entries which spawn from the same trigger will have the same pointer.
    TriggerCopy: PSERVICE_TRIGGER;
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
    miCopyTriggerText: TMenuItem;
    miCopySourceData: TMenuItem;
    miCopyParams: TMenuItem;
    aAddTrigger: TAction;
    aDisableTrigger: TAction;
    aImportTrigger: TAction;
    aExportTrigger: TAction;
    aExportAllTriggers: TAction;
    aDeleteTrigger: TAction;
    miAddTrigger: TMenuItem;
    miImportTrigger: TMenuItem;
    miExportTrigger: TMenuItem;
    miExportAllTriggers: TMenuItem;
    miDisableTrigger: TMenuItem;
    miDeleteTrigger: TMenuItem;
    aEditTrigger: TAction;
    miEditTrigger: TMenuItem;
    N1: TMenuItem;
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
    procedure aAddTriggerExecute(Sender: TObject);
    procedure aEditTriggerExecute(Sender: TObject);
    procedure aCopyTriggerRegDefinitionExecute(Sender: TObject);
    procedure aImportTriggerExecute(Sender: TObject);
    procedure aExportTriggerExecute(Sender: TObject);
    procedure aExportAllTriggersExecute(Sender: TObject);
  const
    colAction = 0;
    colTrigger = 1;
    colParams = 2;
  protected
    //We need to be able to reload and edit
    FServiceName: string;
    FServiceHandle: SC_HANDLE;
    FScmHandle: SC_HANDLE;
    FOwnsServiceHandle: boolean;
    FOwnTriggerCopies: TArray<PSERVICE_TRIGGER>;
    procedure Add(const ATrigger: PSERVICE_TRIGGER);
    function NodesToUniqueTriggers(const ANodes: TVTVirtualNodeEnumeration): TArray<PSERVICE_TRIGGER>;
    procedure TryExportTriggers(const Sel: TArray<PSERVICE_TRIGGER>);
  public
    procedure SetService(ServiceName: string; ServiceHandle: SC_HANDLE = 0);
    procedure Clear;
    procedure Reload;
    function SelectedTriggers: TArray<PNdTriggerData>;
    function SelectedUniqueTriggers: TArray<PSERVICE_TRIGGER>;
    function AllUniqueTriggers: TArray<PSERVICE_TRIGGER>;

  end;

implementation
uses UITypes, Clipbrd, CommonResources, TriggerExport, Viper.TriggerEditor;

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
var HaveService: boolean;
begin
  HaveService := Self.FServiceName <> '';

 //Selection changed
  aCopySummary.Visible := Tree.SelectedCount > 0;
  aCopyTriggerText.Visible := Tree.SelectedCount > 0;
  aCopySourceData.Visible := Tree.SelectedCount > 0;
  aCopyParams.Visible := Tree.SelectedCount > 0;
  miCopy.Visible := aCopySummary.Visible or aCopyTriggerText.Visible
    or aCopySourceData.Visible or aCopyParams.Visible;
  aAddTrigger.Visible := HaveService;
  aImportTrigger.Visible := HaveService;
  aEditTrigger.Visible := Tree.SelectedCount = 1;
  aDisableTrigger.Visible := Tree.SelectedCount > 0;
  aExportTrigger.Visible := Tree.SelectedCount > 0;
  aExportAllTriggers.Visible := (Tree.RootNode.ChildCount > 0); //"Export all" is available if we have any triggers
  aDeleteTrigger.Visible := Tree.SelectedCount > 0;
end;

procedure TTriggerList.TreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and ((Key=Ord('C')) or (Key=Ord('c'))) then
    aCopySummary.Execute;
end;

procedure TTriggerList.SetService(ServiceName: string; ServiceHandle: SC_HANDLE = 0);
begin
  if Self.FServiceHandle <> 0 then begin
    if Self.FOwnsServiceHandle then begin
      CloseServiceHandle(Self.FServiceHandle);
      CloseServiceHandle(Self.FScmHandle);
    end;
    Self.FServiceHandle := 0;
    Self.FScmHandle := 0;
  end;

  Clear;

  Self.FServiceName := ServiceName;
  if FServiceName = '' then
    exit;

  if ServiceHandle <> 0 then begin
    Self.FServiceHandle := ServiceHandle;
    Self.FOwnsServiceHandle := false;
  end else begin
    OpenScmAndService(Self.FScmHandle, Self.FServiceHandle, ServiceName);
    Self.FOwnsServiceHandle := true;
  end;

  Reload;
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
procedure TTriggerList.Add(const ATrigger: PSERVICE_TRIGGER);
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
var triggers: PSERVICE_TRIGGER_INFO;
begin
  Clear;

  triggers := QueryServiceTriggers(FServiceHandle);
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

  //Reset the popup menu
  //We'd like to put this into FormCreate, but there's no FormCreates for TFrames.
  TreeChange(Tree, nil);
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

procedure TTriggerList.aAddTriggerExecute(Sender: TObject);
var EditForm: TTriggerEditorForm;
  TriggerData: PSERVICE_TRIGGER;
begin
  EditForm := TTriggerEditorForm.Create(Self);
  try
    TriggerData := nil;
    if not IsPositiveResult(EditForm.EditTrigger(TriggerData))
    or (TriggerData = nil) //safety
      then exit;
  finally
    FreeAndNil(EditForm);
  end;

  //Add the given trigger
  try
    //We only have a handle with read access, so reopen
    with OpenService2(Self.FServiceName,
      STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
      SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
      AddServiceTriggers(SvcHandle, [TriggerData^]);
  finally
    FreeMem(TriggerData);
  end;

  //Reload the triggers
  //We could just parse the newly created trigger, but lets KISS for now
  Self.Reload;
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
    if not IsPositiveResult(EditForm.EditTrigger(TriggerData)) then begin
      FreeMem(TriggerData);
      exit;
    end;

   //Store the edited trigger in the service config
    with OpenService2(Self.FServiceName,
      STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
      SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
      ChangeServiceTrigger(SvcHandle, Sel[0].TriggerCopy^, TriggerData^);

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

procedure TTriggerList.aDeleteTriggerExecute(Sender: TObject);
var Sel: TArray<PNdTriggerData>;
  SelData: array of SERVICE_TRIGGER;
  i: integer;
begin
  Sel := SelectedTriggers;
  SetLength(SelData, Length(Sel));
  for i := 0 to Length(Sel)-1 do
    SelData[i] := Sel[i].TriggerCopy^;
  with OpenService2(Self.FServiceName,
    STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
    SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
    DeleteServiceTriggers(SvcHandle, SelData);
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

procedure TTriggerList.aExportAllTriggersExecute(Sender: TObject);
begin
  TryExportTriggers(Self.AllUniqueTriggers);
end;

procedure TTriggerList.aImportTriggerExecute(Sender: TObject);
var Triggers: TArray<PSERVICE_TRIGGER>;
  Triggers2: TArray<SERVICE_TRIGGER>;
  Status: TTriggerImportStatus;
  i: integer;
begin
  with OpenTriggersDialog do
    if not Execute then
      exit;

  SetLength(Triggers, 0);
  try
    ImportTriggers(OpenTriggersDialog.FileName, Triggers, Status);

    //TODO: Show the dialog to inform the user of the malformed file (potential missed triggers)
    // + let them choose the triggers to import.

    SetLength(Triggers2, Length(Triggers));
    for i := 0 to Length(Triggers)-1 do
      Triggers2[i] := Triggers[i]^;

    //Add these triggers
    //We only have a handle with read access, so reopen
    with OpenService2(Self.FServiceName,
      STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
      SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
      AddServiceTriggers(SvcHandle, Triggers2, {UniqueOnly=}true);

  finally
    for i := 0 to Length(Triggers)-1 do
      FreeMem(Triggers[i]);
  end;

  Self.Reload;
end;

end.
