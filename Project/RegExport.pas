unit RegExport;
{
Exports a given registry key and all of its subkeys to the .reg file format.
Imports a given .reg file / its keys.
}

interface
uses SysUtils, Windows, Registry, RegFile;

type
  ERegistryExportError = class(Exception);

type
  TRegNativeDataInfo = record
    DataType: integer;
    DataSize: Integer;
  end;
  TRegistryHelper = class helper for TRegistry
  public
    //TRegistry simplifies various data types into its own enumeration - undesirable
    function GetDataInfoNative(const ValueName: string; var Value: TRegNativeDataInfo): Boolean;
    //TRegistry doesn't give us a way to read flat data (ReadBinary checks data type)
    function GetData(const Name: string; Buffer: Pointer; BufSize: Integer; var DataType: integer): Integer;
    procedure PutData(const Name: string; Buffer: Pointer; BufSize: Integer; DataType: integer);
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
    procedure FilterKey(ARegistry: TRegistry; const AName: string; var SkipKey: boolean); overload; virtual;
    function FilterValue(ARegistry: TRegistry; const AName: string): boolean; overload;
    procedure FilterValue(ARegistry: TRegistry; const AName: string; var SkipValue: boolean); overload; virtual;
    function ExportValue(ARegistry: TRegistry; const AName: string): TRegFileEntry;
  public
    constructor Create(ARegFile: TRegFile = nil; AOwnsRegFile: boolean = false);
    destructor Destroy; override;
    procedure SaveToFile(const AFilename: string);
    procedure ExportKey(const ARootKey: HKEY; const APath: string; const Recursive: boolean = true); overload;
    procedure ExportKey(ARegistry: TRegistry; const Recursive: boolean = true); overload;
    property RegFile: TRegFile read FRegFile;
  end;

{
Export shortcuts
}
function ExportRegistryKey(ARegistry: TRegistry; const Recursive: boolean = true): TRegFile;

{
Import
To filter the RegFile simply remove the unnecessary parts before importing.
}
procedure WriteToRegistry(const file_: TRegFile); overload;
procedure WriteToRegistry(reg: TRegistry; const file_: TRegFile); overload;
procedure WriteToRegistry(reg: TRegistry; const key: TRegFileKey); overload;
procedure WriteToRegistry(reg: TRegistry; const entry: TRegFileEntry); overload;

{
Utility functions with no better place to be
}
resourcestring
  eUnacceptableKeyPath = 'Unacceptable key path for operation: "%s"';

function RegNormalizePath(const AKeyPath: string): string;
function RegGetDepth(AKeyPath: string): integer;
procedure RegAssertNonEmpty(const AKeyPath: string); inline;
procedure RegAssertDepth(const AKeyPath: string; const ADepth: integer); inline;


implementation
uses Classes;

{
Normalilzes the registry path:
* Removes all instances of multiple \\\\ by collapsing them
* \..\s are unsupported for registry paths
* Forward slashes / are valid in the key names so shouldn't be reversed
https://docs.microsoft.com/en-us/windows/desktop/SysInfo/structure-of-the-registry
}
function RegNormalizePath(const AKeyPath: string): string;
var i: integer;
  slash: boolean;
begin
  Result := '';
  slash := false;
  for i := 1 to Length(AKeyPath) do
    if AKeyPath[i] = '\' then
      if not slash then begin
        Result := Result + AKeyPath[i];
        slash := true;
      end else begin
        //skip
      end
    else begin
      Result := Result + AKeyPath[i];
      slash := false;
    end;
end;

//Returns the depth of the given registry key. 0 = empty path, 1 = first level, etc.
function RegGetDepth(AKeyPath: string): integer;
var i: integer;
begin
  AKeyPath := RegNormalizePath(AKeyPath); //Prevent things like Software\\Test

  Result := 0;
  if (AKeyPath = '') or (AKeyPath='\') then
    exit;

  if AKeyPath[1] <> '\' then
    i := 1
  else
    i := 2;
  repeat
    Inc(Result);
    i := pos('\', AKeyPath, i);
  until i <= 0;
end;

//Makes sure the given path is not empty, nor references "all registry key"
procedure RegAssertNonEmpty(const AKeyPath: string);
begin
  if (Length(AKeyPath) <= 2)  //so important that we'll check directly
  or (RegGetDepth(AKeyPath) < 1) then //covers the empty case
    raise EAssertionFailed.CreateFmt(eUnacceptableKeyPath, [AKeyPath]);
end;

//Makes sure the given path is of at least the given depth
procedure RegAssertDepth(const AKeyPath: string; const ADepth: integer);
begin
  if RegGetDepth(AKeyPath) < ADepth then
    raise EAssertionFailed.CreateFmt(eUnacceptableKeyPath, [AKeyPath]);
end;


//Retrieves size and type data for the value
//On error returns false and sets LastError
function TRegistryHelper.GetDataInfoNative(const ValueName: string; var Value: TRegNativeDataInfo): Boolean;
begin
  FillChar(Value, SizeOf(TRegDataInfo), 0);
  Result := CheckResult(RegQueryValueEx(CurrentKey, PChar(ValueName), nil, @Value.DataType, nil,
    @Value.DataSize));
end;

function TRegistryHelper.GetData(const Name: string; Buffer: Pointer; BufSize: Integer; var DataType: integer): Integer;
begin
  DataType := REG_NONE;
  if not CheckResult(RegQueryValueEx(CurrentKey, PChar(Name), nil, @DataType, PByte(Buffer), @BufSize)) then
    RaiseLastOsError(Self.LastError);
  Result := BufSize;
end;

procedure TRegistryHelper.PutData(const Name: string; Buffer: Pointer; BufSize: Integer; DataType: integer);
begin
  if not CheckResult(RegSetValueEx(CurrentKey, PChar(Name), 0, DataType, Buffer, BufSize)) then
    RaiseLastOsError(Self.LastError);
end;

function ExportRegistryKey(ARegistry: TRegistry; const Recursive: boolean = true): TRegFile;
var exp: TRegistryExporter;
begin
  exp := nil;
  Result := TRegFile.Create;
  try
    exp := TRegistryExporter.Create(Result, {OwnsRegFile=}false);
    exp.ExportKey(ARegistry, Recursive);
    FreeAndNil(exp);
  except
    FreeAndNil(Result);
    FreeAndNil(exp);
    raise;
  end;
end;

//Processes all instructions contained in this TRegFile (adding and deleting keys)
//and applies them to local registry
procedure WriteToRegistry(const file_: TRegFile);
var reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    WriteToRegistry(reg, file_);
  finally
    FreeAndNil(reg);
  end;
end;

//Processes all instructions contained in this TRegFile (adding and deleting keys)
//and applies them to registry
//The registry object needs not to be opened on any particular key
procedure WriteToRegistry(reg: TRegistry; const file_: TRegFile);
var key: TRegFileKey;
begin
  for key in file_ do
    WriteToRegistry(reg, key);
end;

//Processes the instructions contained in this RegFileKey (adding and deleting keys)
//and applies them to registry
//The registry object needs not to be opened on any particular key
procedure WriteToRegistry(reg: TRegistry; const key: TRegFileKey);
var KeyPath: string;
  Entry: TRegFileEntry;
  RootKey: HKEY;
begin
  GetAbsoluteKeyPath(key.Name, KeyPath, RootKey);
  reg.RootKey := RootKey;
  if key.Delete then begin
    //Safety: NEVER delete '' (empty string) or top-level keys.
    KeyPath := RegNormalizePath(KeyPath);
    RegAssertDepth(KeyPath, 2);
    reg.DeleteKey(KeyPath);
    exit;
  end;

  if not reg.OpenKey(KeyPath, true) then
    RaiseLastOsError(reg.LastError);

  for Entry in key.Entries do
    WriteToRegistry(reg, Entry);
end;

//Processes the instructions contained in this RegFileKey (adding and deleting keys)
//and applies them to registry
//The registry object must be opened on the appropriate key
procedure WriteToRegistry(reg: TRegistry; const entry: TRegFileEntry);
var data: TBytes;
begin
  if entry.DataType = REG_DELETE then begin
    reg.DeleteValue(entry.Name);
    exit;
  end;

  data := entry.GetAsBytes();
  reg.PutData(entry.Name, data, Length(data), entry.DataType);
end;



resourcestring
  eCannotOpenKey = 'Cannot open registry key %s, error %d';

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
  FRegistry.CloseKey; //just in case
  FRegistry.RootKey := ARootKey;
  if not FRegistry.OpenKeyReadOnly(APath) then
    raise ERegistryExportError.CreateFmt(eCannotOpenKey, [RootKeyToStr(ARootKey)+'\'+APath, FRegistry.LastError]);
  ExportKey(FRegistry, Recursive);
  FRegistry.CloseKey;
end;

//Exports a key opened in a given TRegistry object
procedure TRegistryExporter.ExportKey(ARegistry: TRegistry; const Recursive: boolean = true);
var Names: TStringList;
  Name: string;
  KeyEntry: TRegFileKey;
  ValueEntry: TRegFileEntry;
  BasePath, NewPath: string;
begin
  Names := TStringList.Create; //need a new one for each recursion
  try
    KeyEntry.Name := RootKeyToStr(ARegistry.RootKey) + '\' + ARegistry.CurrentPath;
    KeyEntry.Delete := false;
    ARegistry.GetValueNames(Names);
    for Name in Names do begin
      if self.FilterValue(ARegistry, Name) then
        continue;
      ValueEntry := Self.ExportValue(ARegistry, Name);
      KeyEntry.AddValueEntry(ValueEntry);
    end;
    Self.FRegFile.Add(KeyEntry);

    if not Recursive then exit;

    BasePath := ARegistry.CurrentPath;
    ARegistry.GetKeyNames(names);
    for Name in Names do begin
      if self.FilterKey(ARegistry, Name) then
        continue;
      ARegistry.CloseKey; //must close sibling key
      NewPath := BasePath+'\'+Name;
      if not ARegistry.OpenKeyReadOnly(NewPath) then
        raise ERegistryExportError.CreateFmt(eCannotOpenKey, [RootKeyToStr(FRegistry.RootKey)+'\'+NewPath, FRegistry.LastError]);
      ExportKey(ARegistry, Recursive);
    end;
    if ARegistry.CurrentPath <> BasePath then begin
      ARegistry.CloseKey; //=> start from root, whether BasePath is relative or not
      ARegistry.OpenKeyReadOnly(BasePath);
    end;
  finally
    FreeAndNil(Names);
  end;
end;

function TRegistryExporter.ExportValue(ARegistry: TRegistry; const AName: string): TRegFileEntry;
var ValueInfo: TRegNativeDataInfo;
  Buf: array of byte;
begin
  Result.Name := AName;
  if not ARegistry.GetDataInfoNative(AName, ValueInfo) then
    RaiseLastOsError(ARegistry.LastError);
  Result.DataType := ValueInfo.DataType;

  //Read and convert the data depending on its type
  //Both the read method and the text representation might differ
  case Result.DataType of
  REG_SZ:    Result.AsString := ARegistry.ReadString(AName);
  REG_DWORD: Result.AsDword := ARegistry.ReadInteger(AName);
  else //Everything else is stored as hex
    SetLength(Buf, ValueInfo.DataSize);
    ARegistry.GetData(AName, @Buf[0], ValueInfo.DataSize, ValueInfo.DataType);
    Result.SetOtherValue(@Buf[0], ValueInfo.DataSize);
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
