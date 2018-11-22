unit Viper.TriggerImport;
{
Trigger import form.
Lists the triggers found in the registry export file. Allows user to choose
which ones to import.

Has two modes:
1. Universal: Import triggers into their respective services. Triggers for
  services that aren't found will be grayed out.
2. Single service: Import all available triggers into the specified service.
  Ignores the service names from the file.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, VirtualTrees, CommonResources, TriggerExport,
  Viper.TriggerList, Viper.TriggerImportList;

type
  TTriggerImportForm = class(TForm)
    lblPrompt: TLabel;
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    TriggerList: TTriggerImportList;
  protected
    FServiceName: string;
    FTriggers: TArray<TRegTriggerEntry>;
    procedure SetServiceName(const Value: string);
    procedure SetTriggers(Value: TArray<TRegTriggerEntry>);
    procedure ReloadTriggerList;
    procedure GetFilteredTriggers_Callback(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
  public
    function GetFilteredTriggers: TArray<TRegTriggerEntry>;
    property ServiceName: string read FServiceName write SetServiceName;
    property Triggers: TArray<TRegTriggerEntry> read FTriggers write SetTriggers;
  end;

var
  TriggerImportForm: TTriggerImportForm;

function ImportTriggers(AOwner: TWinControl; const AServiceName: string;
  const AFilename: string): TModalResult; overload;
function ImportTriggers(AOwner: TComponent; const AServiceName: string;
  const ATriggers: TArray<TRegTriggerEntry>): TModalResult; overload;


implementation
uses UITypes, WinSvc, ServiceHelper, TriggerUtils;

{$R *.dfm}

procedure ProcessTriggers(const ATriggers: TArray<TRegTriggerEntry>; const AServiceName: string); forward;

resourcestring
  sNoTriggersToImport = 'No triggers found in this file.';

//Reads the triggers from the specified file and imports them
function ImportTriggers(AOwner: TWinControl; const AServiceName: string;
  const AFilename: string): TModalResult;
var Triggers: TArray<TRegTriggerEntry>;
  Status: TTriggerImportStatus;
  i: integer;
begin
  SetLength(Triggers, 0);
  try
    TriggerExport.LoadTriggersFromFile(AFilename, Triggers, Status);

    if Length(Triggers) <= 0 then begin
      MessageBox(AOwner.Handle, PChar(sNoTriggersToImport), PChar('Import triggers'),
        MB_OK + MB_ICONINFORMATION);
      Result := mrCancel;
      exit;
    end;

    Result := ImportTriggers(AOwner, AServiceName, Triggers);
  finally
    for i := 0 to Length(Triggers)-1 do
      Triggers[i].ReleaseTriggerData;
  end;
end;

//Lets the user choose any triggers from the list and imports them
function ImportTriggers(AOwner: TComponent; const AServiceName: string;
  const ATriggers: TArray<TRegTriggerEntry>): TModalResult;
var Form: TTriggerImportForm;
  AFilteredTriggers: TArray<TRegTriggerEntry>;
begin
  Form := TTriggerImportForm.Create(AOwner);
  try
    Form.SetServiceName(AServiceName);
    Form.SetTriggers(ATriggers);
    Result := Form.ShowModal;
    if not IsPositiveResult(Result) then
      exit;
    AFilteredTriggers := Form.GetFilteredTriggers;
  finally
    FreeAndNil(Form);
  end;

  ProcessTriggers(AFilteredTriggers, AServiceName);
end;

//Adds given triggers to the local machine
//If AServiceName is given, that service recieves all the triggers, otherwise
//each trigger goes to its appropriate service.
procedure ProcessTriggers(const ATriggers: TArray<TRegTriggerEntry>; const AServiceName: string);
var i: integer;
  AThisServiceName: string;
begin
  for i := 0 to Length(ATriggers)-1 do begin
    if AServiceName <> '' then
      AThisServiceName := AServiceName
    else
      AThisServiceName := ATriggers[i].ServiceName;

    with OpenService2(AThisServiceName,
      STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
      SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
    begin
      AddServiceTriggers(SvcHandle, [ATriggers[i].Trigger^], {UniqueOnly=}false);
    end;
  end;

  TriggerUtils.TriggerListChanged(nil, AServiceName);
end;


resourcestring
  sMultiTriggerPrompt = 'Choose triggers to import into respective services:';
  sSingleTriggerPrompt = 'Choose triggers to import into "%s":';

procedure TTriggerImportForm.SetServiceName(const Value: string);
var col: TVirtualTreeColumn;
begin
  FServiceName := Value;
  if FServiceName = '' then
    lblPrompt.Caption := sMultiTriggerPrompt
  else
    lblPrompt.Caption := Format(sSingleTriggerPrompt, [Value]);
  //Show Service column only when it matters
  col := TriggerList.Tree.Header.Columns[TriggerList.colService];
  if FServiceName <> '' then
    col.Options := col.Options - [coVisible]
  else
    col.Options := col.Options + [coVisible];
end;

//Copies the array of service triggers.
//Trigger data is used by reference and must remain valid while this window is active.
procedure TTriggerImportForm.SetTriggers(Value: TArray<TRegTriggerEntry>);
begin
  FTriggers := Value;
  ReloadTriggerList;
end;

//Repopulates TVirtualTreeview with triggers from FTriggers.
procedure TTriggerImportForm.ReloadTriggerList;
var Trigger: TRegTriggerEntry;
  GrayedOut: boolean;
begin
  TriggerList.Tree.BeginUpdate;
  try
    TriggerList.Clear;
    for Trigger in FTriggers do begin
      GrayedOut := (FServiceName = '') and not ServiceExists(Trigger.ServiceName);
      TriggerList.Add(Trigger, GrayedOut);
    end;
  finally
    TriggerList.Tree.EndUpdate;
  end;
  btnOk.Enabled := (TriggerList.Tree.ChildCount[nil] > 0);
end;

//Returns the list of all triggers which are checked.
//Uses the same PSERVICE_TRIGGER pointers given to SetTriggers()
function TTriggerImportForm.GetFilteredTriggers: TArray<TRegTriggerEntry>;
begin
  SetLength(Result, 0);
  TriggerList.Tree.IterateSubtree(nil, GetFilteredTriggers_Callback, @Result);
end;

procedure TTriggerImportForm.GetFilteredTriggers_Callback(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var List: ^TArray<TRegTriggerEntry> absolute Data;
  Trigger: TNdTrigger;
  Entry: PRegTriggerEntry;
begin
  if Sender.CheckState[Node] <> csCheckedNormal then exit;
  Trigger := TriggerList.GetTriggerData(Node);
  SetLength(List^, Length(List^)+1);
  Entry := @List^[Length(List^)-1];
  Entry^.ServiceName := Trigger.ServiceName;
  Entry^.Index := Trigger.Index;
  Entry^.Trigger := Trigger.Data; //TriggerImportList uses the same pointers we've given to it
end;

end.
