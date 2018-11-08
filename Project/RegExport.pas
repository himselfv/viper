unit RegExport;
{
Exports a given registry key and all of its subkeys to the .reg file format.
}

interface
uses SysUtils, Windows, Registry, RegFile;

type
  ERegistryExportError = class(Exception);


type
  //TRegistry simplifies various data types into its own enumeration - undesirable
  TRegNativeDataInfo = record
    RegData: integer;
    DataSize: Integer;
  end;
  TRegistryHelper = class helper for TRegistry
  public
    function GetDataInfoNative(const ValueName: string; var Value: TRegNativeDataInfo): Boolean;
  end;

{
Usage:
  Override to provide custom key/value filtering.
  Construct a copy, pass an existing TRegFile if needed.
  Export all required keys.
  Save to file.
}
type
  TRegistryExporter = class
  protected
    FRegFile: TRegFile;
    FOwnsRegFile: boolean;
    FRegistry: TRegistry;
    function GetRegistry: TRegistry; inline;
    function FilterKey(ARegistry: TRegistry; const AName: string): boolean; overload;
    procedure FilterKey(ARegistry: TRegistry; const AName: string; var SkipKey: boolean); virtual; overload;
    function FilterValue(ARegistry: TRegistry; const AName: string): boolean; overload;
    procedure FilterValue(ARegistry: TRegistry; const AName: string; var SkipValue: boolean); virtual;
  public
    constructor Create(ARegFile: TRegFile = nil; AOwnsRegFile: boolean = false);
    destructor Destroy; override;
    procedure SaveToFile(const AFilename: string);
    procedure ExportKey(const ARootKey: HKEY; const APath: string; const Recursive: boolean = true); overload;
    procedure ExportKey(ARegistry: TRegistry; const Recursive: boolean = true); overload;
    property RegFile: TRegFile read FRegFile;
  end;


implementation
uses Classes;

//Retrieves size and type data for the value
//On error returns false and sets LastError
function TRegistryHelper.GetDataInfoNative(const ValueName: string; var Value: TRegDataInfo): Boolean;
begin
  FillChar(Value, SizeOf(TRegDataInfo), 0);
  Result := CheckResult(RegQueryValueEx(CurrentKey, PChar(ValueName), nil, @Value.DataType, nil,
    @Value.DataSize));
end;

resourcestring
  eCannotOpenKey = 'Cannot open registry key %s';

constructor TRegistryExporter.Create(ARegFile: TRegFile; AOwnsRegFile: boolean);
begin
  inherited Create;
  if ARegFile = nil then begin
    FRegFile := TRegFile.Create;
    FOwnsRegFile := true;
  end else begin
    FRegFile := ARegFile;
    FOwnsRegFile := AOwnsRegFile;
  end;
  FRegistry := nil;
end;

destructor TRegistryExporter.Destroy;
begin
  if FOwnsRegFile then
    FreeAndNil(FRegFile);
  FreeAndNil(FRegistry);
  inherited;
end;

//Returns an internal TRegistry object if we haven't been given one by the user
function TRegistryExporter.GetRegistry: TRegistry;
begin
  if FRegistry = nil then
    FRegistry := TRegistry.Create;
  Result := FRegistry;
end;

procedure TRegistryExporter.SaveToFile(const AFilename: string);
begin
  FRegFile.SaveToFile(AFilename);
end;

//Exports a given key
//Raises an exception if the key does not exist
procedure TRegistryExporter.ExportKey(const ARootKey: HKEY; const APath: string;
  const Recursive: boolean = true);
begin
  if FRegistry = nil then
    FRegistry := TRegistry.Create;
  FRegistry.RootKey := ARootKey;
  if not FRegistry.OpenKey(APath, false) then
    raise ERegistryExportError.Create(eCannotOpenKey, [RootKeyToStr(ARootKey)+'\'+APath]);
  ExportKey(FRegistry, Recursive);
end;

//Exports a key opened in a given TRegistry object
procedure TRegistryExporter.ExportKey(ARegistry: TRegistry; const Recursive: boolean = true);
var Names: TStringList;
  Name: string;
  KeyEntry: TRegFileKey;
  ValueEntry: TRegFileEntry;
  ValueInfo: TRegNativeDataInfo;
  BasePath: string;
begin
  Names := TStringList.Create; //need a new one for each recursion
  try
    KeyEntry.Name := RootKeyToStr(ARegistry.RootKey) + '\' + ARegistry.CurrentPath;
    KeyEntry.Delete := false;
    ARegistry.GetValueNames(Names);
    for Name in Names do begin
      if self.FilterValue(ARegistry, Name) then
        continue;
      ValueEntry.Name := Name;
      if not ARegistry.GetDataInfo(Name, ValueInfo) then
        RaiseLastOsError(ARegistry.LastError);
      ValueEntry.DataType := ValueInfo.RegData;
      //TODO: Read + encode/decode value depending on its type
    end;
    Self.FRegFile.Add(KeyEntry);

    if not Recursive then exit;

    BasePath := ARegistry.CurrentPath;
    ARegistry.GetKeyNames(names);
    for Name in Names do begin
      if self.FilterKey(ARegistry, Name) then
        continue;
      ARegistry.OpenKey(BasePath + '\' + Name, false);
      ExportKey(ARegistry, Recursive);
    end;
    if ARegistry.CurrentPath <> BasePath then
      ARegistry.CurrentPath := BasePath;
  finally
    FreeAndNil(Names);
  end;
end;


//Override the following functions to provide key and value filtering
//The Registry object MUST be in the original state on return
//The Name is always the leaf name, the rest of the path is in Registry.CurrentPath

function TRegistryExporter.FilterKey(ARegistry: TRegistry; const AName: string): boolean;
begin
  Result := false;
  FilterKey(ARegistry, AName, Result);
end;

procedure TRegistryExporter.FilterKey(ARegistry: TRegistry; const AName: string; var SkipKey: boolean);
begin
end;

function TRegistryExporter.FilterValue(ARegistry: TRegistry; const AName: string): boolean;
begin
  Result := false;
  FilterValue(ARegistry, AName, Result);
end;

procedure TRegistryExporter.FilterValue(ARegistry: TRegistry; const AName: string; var SkipValue: boolean);
begin
end;


end.
