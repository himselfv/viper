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

Limitations:
- We don't support deletion commands from .reg.

Q: How to do a "full replace"?
A: Delete the service and import it. Maybe in the future we'll make an automated
  option.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, VirtualTrees, Generics.Collections,
  RegFile, RegFileSvcEntry;

type
  //The list of all params which may or may not be available for the service
  TImportServiceParam = (
    //Minimal set:
    spImagePath,
    spType,             //Driver or Service
    spStartType,        //Includes DelayedAutostart
    //The rest can be grouped arbitrarily as we need
    spOtherBasicInfo,
    spTriggers
  );
  TImportServiceParams = set of TImportServiceParam;

const
  //A minimal set of service params which allows us to create the service
  RequiredServiceParams = [spImagePath];

type
  TImportServiceList = TRegFileServiceList;

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
    FServices: TImportServiceList;
  public
    procedure LoadFromFile(const AFilename: string);
  end;

var
  ServiceImportForm: TServiceImportForm;

function ImportRegFile(AOwner: TComponent; const AFilename: string): TModalResult;


implementation
uses RegExport, TriggerExport, WinApiHelper;

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
  FServices := TImportServiceList.Create;
end;

procedure TServiceImportForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FServices);
  FreeAndNil(FFile);
end;

procedure TServiceImportForm.FormShow(Sender: TObject);
begin
  pcPages.ActivePage := Self.tsServices;
end;


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

Rules of thumb for now:

- Ignore keys and params "marked for deletion" - they don't contain anything useful

- Likewise ignore params that we can't parse, but for starters we throw on that,
  and then may catch and ignore.

}

//Loads the registry export file
procedure TServiceImportForm.LoadFromFile(const AFilename: string);
var key: TRegFileKey;
  ServiceName: string;
  Subkey: string;
  Service: TRegFileServiceEntry;
begin
  FFile.LoadFromFile(AFilename);
  FServices.Clear;

  for key in FFile do begin
    if key.Delete then
      continue; //we don't support deletion commands here
    ServiceName := '';
    Subkey := '';
    if not SplitServiceRegistryPath(key.Name, ServiceName, Subkey) then
      continue;

    Service := Self.FServices.Get(ServiceName);


  end;


//  ReloadServices; //TODO
end;


end.
