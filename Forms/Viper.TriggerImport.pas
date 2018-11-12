unit Viper.TriggerImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, VirtualTrees, CommonResources, TriggerExport,
  Viper.TriggerList;

type
  TTriggerImportForm = class(TForm)
    lblPrompt: TLabel;
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    TriggerList: TTriggerList;
    procedure FormCreate(Sender: TObject);
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

procedure TTriggerImportForm.FormCreate(Sender: TObject);
begin
  inherited;
  Self.TriggerList.FEntryMode := emChildEntries;
end;

resourcestring
  sMultiTriggerPrompt = 'Choose triggers to import into respective services:';
  sSingleTriggerPrompt = 'Choose triggers to import into "%s":';

procedure TTriggerImportForm.SetServiceName(const Value: string);
begin
  FServiceName := Value;
  if FServiceName = '' then
    lblPrompt.Caption := sMultiTriggerPrompt
  else
    lblPrompt.Caption := Format(sSingleTriggerPrompt, [Value]);
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
  Node: PVirtualNode;
begin
  TriggerList.Tree.BeginUpdate;
  try
    TriggerList.Clear;
    for Trigger in FTriggers do begin
      Node := TriggerList.Add(Trigger.ServiceName, Trigger.Index, Trigger.Trigger);
      TriggerList.Tree.CheckType[Node] := ctCheckBox;
      TriggerList.Tree.CheckState[Node] := csCheckedNormal;
    end;
  finally
    TriggerList.Tree.EndUpdate;
  end;
end;

end.
