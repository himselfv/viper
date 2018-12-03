unit Viper.ServiceImport;
{
Imports a .reg file.
As a baseline this is supposed to work like a double-click import. Any improvements
are to be carefully considered.

This means "add missing + update existing":
- Missing services are automatically added in their entirety.
- Existing services are updated, all properties are replaced, any additional
  properties are left untouched.

We add a number of improvements to this:
- You can choose services to import / skip adding new ones entirely
- If triggers are replaced they are replaced consistently (double-click import
  produces weird merges).

We may add more improvements in the future, e.g. the ability to "merge triggers",
but:
OUR BASELINE IS DOUBLE-CLICK IMPORT.

There are too many ways to import a complicated structure. We need to start
somewhere. We start with a mode that's least surprising.

Q: How to do a "full replace"?
A: Delete the service and import it. Maybe in the future we'll make an automated
  option.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, VirtualTrees, RegFile;

type
  TServiceImportForm = class(TForm)
    pcPages: TPageControl;
    pnlBottom: TPanel;
    btnNext: TButton;
    btnCancel: TButton;
    tsServices: TTabSheet;
    tsSettings: TTabSheet;
    Label1: TLabel;
    rbAddAndUpdateServices: TRadioButton;
    rbUpdateServicesOnly: TRadioButton;
    VirtualStringTree1: TVirtualStringTree;
    Label2: TLabel;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox1: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    FFile: TRegFile;
  public
    procedure LoadFromFile(const AFilename: string);
  end;

var
  ServiceImportForm: TServiceImportForm;

function ImportRegFile(AOwner: TComponent; const AFilename: string): TModalResult;


implementation
uses RegExport;

{$R *.dfm}

//Loads the given file, lets the user select which services and settings to import
//and imports them.
function ImportRegFile(AOwner: TComponent; const AFilename: string): TModalResult;
var Form: TServiceImportForm;
begin
  Form := TServiceImportForm.Create(AOwner);
  try
    Form.LoadFromFile(AFilename);
    Result := Form.ShowModal;
  finally
    FreeAndNil(Form);
  end;
end;


procedure TServiceImportForm.FormCreate(Sender: TObject);
begin
  FFile := TRegFile.Create;
end;

procedure TServiceImportForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFile);
end;

procedure TServiceImportForm.FormShow(Sender: TObject);
begin
  pcPages.ActivePage := Self.tsServices;
end;

//Loads the registry export file
procedure TServiceImportForm.LoadFromFile(const AFilename: string);
begin
  FFile.LoadFromFile(AFilename);

{
Next we have to analyze the file contents:

1. Build the list of services (keys that start with HKLM\System\CCS\Services)

2. Mark which info each service has. We'll probably need a flag set for all possible
  important fields.

3. Mark the services for which we have the minimal info; these we can add,
  others only update.

4. Compile the "what's available" to disable the check boxes for settings which
  aren't in the file.

5. Reload the services list.

We probably need some kind of "import service structure" with the flag set
and everything.
}

//  ReloadServices; //TODO
end;



end.
