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
  RegFile, MemSvcEntry;

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
  TImportService = class(TMemServiceEntry)
  protected
    FRootKey: TRegFileKey;
    FKeys: TList<TRegFileKey>; //except for root
  public
    constructor Create;
    destructor Destroy; override;
    property Keys: TList<TRegFileKey> read FKeys;
  end;
  TImportServiceList = class(TDictionary<string, TImportService>)
  protected
    procedure ValueNotify(const Value: TImportService; Action: TCollectionNotification); override;
  public
    function Get(const AServiceName: string): TImportService;
  end;

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
    procedure ParseBasicEntry(AService: TImportService; AEntry: TRegFileEntry);
    procedure ParseParametersEntry(AService: TImportService; AEntry: TRegFileEntry);
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


{
Temporary storage for service information loaded from the reg file
}

constructor TImportService.Create;
begin
  inherited;
  FKeys := TList<TRegFileKey>.Create;
end;

destructor TImportService.Destroy;
begin
  FreeAndNil(FKeys);
  inherited;
end;


procedure TImportServiceList.ValueNotify(const Value: TImportService; Action: TCollectionNotification);
begin
  case Action of
    cnRemoved: Value.Free;
  end;
end;

function TImportServiceList.Get(const AServiceName: string): TImportService;
begin
  if Self.TryGetValue(AServiceName, Result) then
    exit;
  Result := TImportService.Create;
  Result.ServiceName := AServiceName;
  Self.Add(AServiceName, Result);
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
  entry: TRegFileEntry;
  ServiceName: string;
  Subkey: string;
  Service: TImportService;
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

    if Subkey = '' then begin
      for entry in key.Entries do
        ParseBasicEntry(Service, entry);
    end else
    if Subkey = 'Parameters' then begin
      for entry in key.Entries do
        ParseParametersEntry(Service, entry);
    end else
      //All the other keys should be stored as is
      Service.Keys.Add(key);
  end;





//  ReloadServices; //TODO
end;

//Parses the service top level key and extracts params we know how to handle.
procedure TServiceImportForm.ParseBasicEntry(AService: TImportService; AEntry: TRegFileEntry);
begin
  //QueryServiceConfig() parameters

  if AEntry.Name = 'Type' then begin
    AService.FConfig.dwServiceType := AEntry.DwordValue;
  end else
  if AEntry.Name = 'Start' then begin
    AService.FConfig.dwStartType := AEntry.DwordValue;
  end else
  if AEntry.Name = 'ErrorControl' then begin
    AService.FConfig.dwErrorControl := AEntry.DwordValue;
  end else
  if AEntry.Name = 'ImagePath' then begin
    AService.CBinaryPathName := AEntry.StringValue;
  end else
  if AEntry.Name = 'Group' then begin
    AService.CLoadOrderGroup := AEntry.StringValue;
  end else
  if AEntry.Name = 'Tag' then begin
    AService.FConfig.dwTagId := AEntry.DwordValue;
  end else
  if AEntry.Name = 'DependOnService' then begin
//    AService.FDependencies := AEntry.StringValue
  //TODO
  end else
  if AEntry.Name = 'DependOnGroup' then begin
  //TODO
  end else
  if AEntry.Name = 'ObjectName' then begin
    AService.CServiceStartName := AEntry.StringValue;
  end else
  //TODO: What to do with Password? We need it to re-set the ObjectName read from the registry,
  //but there's no Password in the registry.
  //And we can only avoid passing it by also writing in the registry, so this possibility
  //should be optional.
  if AEntry.Name = 'DisplayName' then begin
    AService.CDisplayName := AEntry.StringValue;
  end else

  //TODO: Other properties

  //Add the rest as a root key to the service object
  AService.FRootKey.AddEntry(AEntry);
end;

procedure TServiceImportForm.ParseParametersEntry(AService: TImportService; AEntry: TRegFileEntry);
begin

end;



end.
