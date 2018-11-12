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
  public
    property ServiceName: string read FServiceName write SetServiceName;
    property Triggers: TArray<TRegTriggerEntry> read FTriggers write SetTriggers;
  end;

var
  TriggerImportForm: TTriggerImportForm;

function ImportTriggers(AOwner: TComponent; const AServiceName: string;
  const ATriggers: TArray<TRegTriggerEntry>): TModalResult;


implementation
uses ServiceHelper;

{$R *.dfm}

function ImportTriggers(AOwner: TComponent; const AServiceName: string;
  const ATriggers: TArray<TRegTriggerEntry>): TModalResult;
var Form: TTriggerImportForm;
begin
  Form := TTriggerImportForm.Create(AOwner);
  try
    Form.SetServiceName(AServiceName);
    Form.SetTriggers(ATriggers);
    Result := Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
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
//Trigger data is copied by reference and must remain valid while this window is active.
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
end;

end.
