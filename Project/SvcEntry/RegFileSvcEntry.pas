unit RegFileSvcEntry;
{
TServiceEntry loaded from REG file.
Keys are exactly the same as they would be in the registry.

The implementation is based on TMemServiceEntry but for our purposes it's important
to track which properties have been read and which are undefined.
So for each property this module has a way to express that it's undefined.

For most properties this is just a flag in TServiceQueryBits, but some queries
consist of multiple fields which may be available independently.
}

interface
uses SysUtils, Generics.Collections, RegFile, SvcEntry, MemSvcEntry;

type
{
To load a TRegFileServiceEntry from a reg file, iterate through all its keys,
find appropriate service for each and pass the key to its ParseKey().
}
  TRegFileServiceEntry = class(TMemServiceEntry)
  protected //Additional parsed keys
    FRootKey: TRegFileKey;
    FKeys: TList<TRegFileKey>; //except for root
  protected
    procedure ParseBasicEntry(AEntry: TRegFileEntry);
    procedure ParseParametersEntry(AEntry: TRegFileEntry);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ParseKey(const APath: string; AKey: TRegFileKey);
    property Keys: TList<TRegFileKey> read FKeys;
  end;

  TRegFileServiceList = class(TDictionary<string, TRegFileServiceEntry>)
  protected
    procedure ValueNotify(const Value: TRegFileServiceEntry; Action: TCollectionNotification); override;
  public
    function Get(const AServiceName: string): TRegFileServiceEntry;
  end;


implementation
uses WinSvc;

procedure TRegFileServiceList.ValueNotify(const Value: TRegFileServiceEntry; Action: TCollectionNotification);
begin
  case Action of
    cnRemoved: Value.Free;
  end;
end;

function TRegFileServiceList.Get(const AServiceName: string): TRegFileServiceEntry;
begin
  if Self.TryGetValue(AServiceName, Result) then
    exit;
  Result := TRegFileServiceEntry.Create;
  Result.ServiceName := AServiceName;
  Self.Add(AServiceName, Result);
end;


constructor TRegFileServiceEntry.Create;
begin
  inherited;
  FKeys := TList<TRegFileKey>.Create;
end;

destructor TRegFileServiceEntry.Destroy;
begin
  FreeAndNil(FKeys);
  inherited;
end;

//Parses a reg file key related to this service, with a given subpath,
//and either updates some service parameters or stores this key in additional keys.
procedure TRegFileServiceEntry.ParseKey(const APath: string; AKey: TRegFileKey);
var entry: TRegFileEntry;
begin
  if APath = '' then begin
    for entry in AKey.Entries do
      ParseBasicEntry(entry);
  end else
  if APath = 'Parameters' then begin
    for entry in AKey.Entries do
      ParseParametersEntry(entry);
  end else
  //TODO: Security
    //All the other keys should be stored as is
    Self.Keys.Add(AKey);
end;

//Parses the service top level key and extracts params we know how to handle.
procedure TRegFileServiceEntry.ParseBasicEntry(AEntry: TRegFileEntry);
var tmp_str: string;
begin
  //QueryServiceConfig() parameters

  if AEntry.Name = 'Type' then begin
    Self.FConfig.dwServiceType := AEntry.DwordValue;
  end else
  if AEntry.Name = 'Start' then begin
    Self.FConfig.dwStartType := AEntry.DwordValue;
  end else
  if AEntry.Name = 'ErrorControl' then begin
    Self.FConfig.dwErrorControl := AEntry.DwordValue;
  end else
  if AEntry.Name = 'ImagePath' then begin
    Self.CBinaryPathName := AEntry.StringValue;
  end else
  if AEntry.Name = 'Group' then begin
    Self.CLoadOrderGroup := AEntry.StringValue;
  end else
  if AEntry.Name = 'Tag' then begin
    Self.FConfig.dwTagId := AEntry.DwordValue;
  end else
  if AEntry.Name = 'DependOnService' then begin
    for tmp_str in AEntry.MultiStrValue do
      Self.AddCDependency(tmp_str);
  end else
  if AEntry.Name = 'DependOnGroup' then begin
    for tmp_str in AEntry.MultiStrValue do
      Self.AddCDependency(SC_GROUP_IDENTIFIER+tmp_str);
  end else
  if AEntry.Name = 'ObjectName' then begin
    Self.CServiceStartName := AEntry.StringValue;
  end else
  if AEntry.Name = 'Tag' then begin
    //Note: TagId cannot be set via SCM, you can only ask it for a new one.
    //But our in-memory service entry is okay with that.
    Self.FConfig.dwTagId := AEntry.DwordValue;
  end else

  //TODO: What to do with Password? We need it to re-set the ObjectName read from the registry,
  //but there's no Password in the registry.
  //And we can only avoid passing it by also writing in the registry, so this possibility
  //should be optional.
  if AEntry.Name = 'DisplayName' then begin
    Self.CDisplayName := AEntry.StringValue;
  end else

  //QueryServiceConfig2() / extended properties

  if AEntry.Name = 'Description' then begin
    Self.Description := AEntry.StringValue;
  end else

  //TODO: FailureActions

  if AEntry.Name = 'FailureActionsOnNonCrashFailures' then begin
    Self.FailureActionsOnNonCrashFailures := AEntry.DwordValue <> 0;
  end else
  if AEntry.Name = 'DelayedAutoStart' then begin
    Self.DelayedAutostart := AEntry.DwordValue <> 0;
  end else
  if AEntry.Name = 'ServiceSidType' then begin
    Self.SidType := AEntry.DwordValue;
  end else
  if AEntry.Name = 'RequiredPrivileges' then begin
    Self.RequiredPrivileges := AEntry.MultiStrValue;
  end else
  if AEntry.Name = 'PreshutdownTimeout' then begin
    Self.PreshutdownTimeout := AEntry.DwordValue;
    //TODO: This entry NOT existing might have the active meaning that the timeout is not set.
    //Though this may be true for other properties too.
  end else

  //TODO: Triggers (TriggerInfo subkey)

  if AEntry.Name = 'PreferredNode' then begin
    Self.PreferredNode := AEntry.DwordValue;
  end else

  //TODO: ServiceDll, ServiceDllUnloadOnStop, ServiceMain

  //Add the rest as a root key to the service object
  Self.FRootKey.AddEntry(AEntry);
end;

procedure TRegFileServiceEntry.ParseParametersEntry(AEntry: TRegFileEntry);
begin
  //TODO: ServiceDll, ServiceDllUnloadOnStop, ServiceMain
end;

end.
