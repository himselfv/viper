unit Viper.TriggerImport;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, VirtualTrees, CommonResources;

type
  TTriggerImportForm = class(TForm)
    lblPrompt: TLabel;
    pnlButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    vtTriggers: TVirtualStringTree;
  end;

var
  TriggerImportForm: TTriggerImportForm;

implementation

{$R *.dfm}

resourcestring
  sMultiTriggerPrompt = 'Choose triggers to import into respective services:';
  sSingleTriggerPrompt = 'Choose triggers to import into "%s":';

end.
