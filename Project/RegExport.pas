unit RegExport;
{
Exports a given registry key and all of its subkeys to the .reg file format.
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


implementation
uses Classes;

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
