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
uses SysUtils, Generics.Collections, WinSvc, RegFile, SvcEntry, MemSvcEntry;

type
{
To load a TRegFileServiceEntry from a reg file, iterate through all its keys,
find appropriate service for each and pass the key to its ParseKey().
}
  TRegFileServiceEntry = class(TMemServiceEntry)
  protected //Additional parsed keys
    FKeys: TRegFileKeys;
    FMyTriggers: array of PSERVICE_TRIGGER;
    procedure FreeMyTriggers;
    procedure FinalizeTriggers;
  protected
    function ParseBasicEntry(AEntry: TRegFileEntry): boolean;
    function ParseParametersEntry(AEntry: TRegFileEntry): boolean;
    function ParseTriggerKey(APath: string; AKey: PRegFileKey): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromKeys(const AKeys: TRegFileKeys; const AOwnsKeys: boolean);
    property Keys: TRegFileKeys read FKeys;
  end;

  TRegFileServiceList = class(TDictionary<string, TRegFileServiceEntry>)
  protected
    procedure ValueNotify(const Value: TRegFileServiceEntry; Action: TCollectionNotification); override;
  public
    function Get(const AServiceName: string): TRegFileServiceEntry;
  end;


implementation
uses WinApiHelper, TriggerExport;

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
  FKeys := TRegFileKeys.Create;
end;

destructor TRegFileServiceEntry.Destroy;
begin
  FreeMyTriggers;
  FreeAndNil(FKeys);
  inherited;
end;

procedure TRegFileServiceEntry.Clear;
begin
  FreeMyTriggers; //if left from the previous loading
  FreeAndNil(FKeys);
end;

{
Loads the service configuration from the given list of registry keys.
All keys must be subkeys of this service's registry key and all paths must be
stripped of the service's base registry path.
In other words:
  HKLM\System\CurrentControlSet\Services\MyService\TriggerInfo\1
  -->
  TriggerInfo\1
Any recognized keys in the wrong format will trigger parsing failures atm.
Any unrecognized keys will be available in the Keys field of this object.
Deletion instructions will be treated as unrecognized keys.
}
procedure TRegFileServiceEntry.LoadFromKeys(const AKeys: TRegFileKeys; const AOwnsKeys: boolean);
var i, j: integer;
  AKey: PRegFileKey;
begin
  Clear;

  FreeAndNil(FKeys); //if we had any
  if AOwnsKeys then
    Self.FKeys := AKeys
  else
    Self.FKeys := TRegFileKeys.Create(AKeys); //make own copy

  i := 0;
  while i < AKeys.Count do begin
    AKey := AKeys[i];

    if AKey.Delete then begin
      Inc(i);
      continue; //unsupported
    end;

    if AKey.Name='' then begin
      j := 0;
      while j < Length(AKey.Entries) do
        if ParseBasicEntry(AKey.Entries[j]) then
          AKey.DeleteEntry(j)
        else
          Inc(j);
      //Delete key if empty
      if AKey.EntryCount <= 0 then
        AKeys.Delete(i)
      else
        Inc(i);
      continue;
    end;

    if AKey.Name='Parameters' then begin
      j := 0;
      while j < Length(AKey.Entries) do
        if ParseParametersEntry(AKey.Entries[j]) then
          AKey.DeleteEntry(j)
        else
          Inc(j);
      //Delete key if empty
      if AKey.EntryCount <= 0 then
        AKeys.Delete(i)
      else
        Inc(i);
      continue;
    end;

    if AKey.Name.StartsWith('TriggerInfo') then begin
      if Self.ParseTriggerKey(Copy(AKey.Name, Length('TriggerInfo')+1, MaxInt), AKey)
      and (AKey.EntryCount <= 0) then
        //Delete if recognized/parsed AND empty
        AKeys.Delete(i)
      else
        Inc(i);
      continue;
    end;

    //TODO: Security

    //Keep this key
    Inc(i);
  end;

  //Finalize
  FinalizeTriggers;
end;

//Parses the service top level key and extracts params we know how to handle.
function TRegFileServiceEntry.ParseBasicEntry(AEntry: TRegFileEntry): boolean;
var tmp_str: string;
begin
  Result := true;

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
  end else

  if AEntry.Name = 'PreferredNode' then begin
    Self.PreferredNode := AEntry.DwordValue;
  end else

  if AEntry.Name = 'ServiceDll' then begin
    Self.FImageInformation.ServiceDll := AEntry.StringValue;
  end else
  if AEntry.Name = 'ServiceDllUnloadOnStop' then begin
    Self.FImageInformation.ServiceDllUnloadOnStop := AEntry.DwordValue <> 0;
  end else
  if AEntry.Name = 'ServiceMain' then begin
    Self.FImageInformation.ServiceMain := AEntry.StringValue;
  end else

    Result := false;
end;

function TRegFileServiceEntry.ParseParametersEntry(AEntry: TRegFileEntry): boolean;
begin
  Result := true;

  if AEntry.Name = 'ServiceDll' then begin
    Self.FImageInformation.ServiceDll := AEntry.StringValue;
  end else
  if AEntry.Name = 'ServiceDllUnloadOnStop' then begin
    Self.FImageInformation.ServiceDllUnloadOnStop := AEntry.DwordValue <> 0;
  end else
  if AEntry.Name = 'ServiceMain' then begin
    Self.FImageInformation.ServiceMain := AEntry.StringValue;
  end else

    Result := false;
end;

function TRegFileServiceEntry.ParseTriggerKey(APath: string; AKey: PRegFileKey): boolean;
var LTrigger: PSERVICE_TRIGGER;
begin
  if APath = '' then begin
    //nothing in the root TriggerInfo
    Result := false;
    exit;
  end;

  if APath[1] = '\' then
    Delete(APath, 1, 1);

  //Parse the trigger params and delete them from the key
  LTrigger := CreateTriggerFromSectionVar(AKey^);
  Result := LTrigger <> nil;

  //We could simply AddTrigger but let's be more efficient
  SetLength(FMyTriggers, Length(FMyTriggers)+1);
  FMyTriggers[Length(FMyTriggers)-1] := LTrigger;

  Result := true;
end;

//Frees the temporarily allocated trigger structures in MyTriggers
procedure TRegFileServiceEntry.FreeMyTriggers;
var i: integer;
begin
  for i := 0 to Length(FMyTriggers)-1 do
    FreeMem(FMyTriggers[i]);
  SetLength(FMyTriggers, 0);
end;

procedure TRegFileServiceEntry.FinalizeTriggers;
var trigHead: SERVICE_TRIGGER_INFO;
  trigData: array of SERVICE_TRIGGER;
  i: integer;
begin
  try
    trigHead.cTriggers := Length(Self.FMyTriggers);
    trigHead.pReserved := nil;

    SetLength(trigData, trigHead.cTriggers);
    for i := 0 to trigHead.cTriggers-1 do
      trigData[i] := Self.FMyTriggers[i]^;
    if trigHead.cTriggers > 0 then
      trigHead.pTriggers := @trigData[0]
    else
      trigHead.pTriggers := nil;
    Self.SetTriggers(@trigHead);
  finally
    FreeMyTriggers; //since we need to free them anyway
  end;
end;


end.
