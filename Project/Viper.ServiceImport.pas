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
uses RegExport, TriggerExport;

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


type
  TRegFileServices = class(TDictionary<string, TRegFileKeys>)
  protected
    procedure ValueNotify(const Value: TRegFileKeys; Action: TCollectionNotification); override;
  public
    function Get(const AServiceName: string): TRegFileKeys;
    procedure AddKeys(var AKeys: TRegFileKeys);
  end;

function TRegFileServices.Get(const AServiceName: string): TRegFileKeys;
begin
  if Self.TryGetValue(AServiceName, Result) then exit;
  Result := TRegFileKeys.Create;
  Self.Add(AServiceName, Result);
end;

procedure TRegFileServices.ValueNotify(const Value: TRegFileKeys; Action: TCollectionNotification);
begin
  case Action of
    cnRemoved: Value.Destroy;
  end;
end;

{
Parses all keys in the list and groups them by the target service, based on their
registry path.
Leaves only the keys which do not belong to any service in AKeys.
}
procedure TRegFileServices.AddKeys(var AKeys: TRegFileKeys);
var i: integer;
  ServiceName: string;
  Subkey: string;
  Key: PRegFileKey;
begin
  i := 0;
  while i < AKeys.Count do begin
    ServiceName := '';
    Subkey := '';
    if not SplitServiceRegistryPath(AKeys[i].Name, ServiceName, Subkey)
    or (ServiceName = '') then begin
      Inc(i);
      continue;
    end;

    //Move the key to the service's personal key list
    Key := AKeys[i];
    AKeys.Extract(Key);
    Key.Name := Subkey; //trim the base part from the name
    Self.Get(ServiceName).Add(Key);
    //Don't increment the i
  end;
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
var SvcList: TRegFileServices;
  ServiceName: string;
  Service: TRegFileServiceEntry;
begin
  FFile.LoadFromFile(AFilename);

  //Split keys by services
  SvcList := TRegFileServices.Create;
  try
    SvcList.AddKeys(TRegFileKeys(FFile));
    //TODO: If anything is left in FFile, that's unparsable keys - tell the user

    for ServiceName in SvcList.Keys do begin
      Service := Self.FServices.Get(ServiceName);
      //Pass TRegFileKeys as is to avoid copying:
      Service.LoadFromKeys(SvcList.ExtractPair(ServiceName).Value, {owns=}true);
    end;
  finally
    FreeAndNil(SvcList);
  end;

//  ReloadServices; //TODO
end;


end.
