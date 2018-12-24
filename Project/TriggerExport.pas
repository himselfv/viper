unit TriggerExport;
{
Trigger import/export.

Triggers are stored in the registry, in the service key's TriggerInfo subkey.
When exported to a .reg file format, they assume a certain form which we here
reproduce.
  [Section name]
  "Type"=dword:00000004
  "Action"=dword:00000001
  "GUID"=hex:07,9e,56,b7,21,...
  "Data0"=hex:31,00,33,00,39,00,...
  "DataType0"=dword:00000002
  "Data1"=hex:31,00,33,00,37,00,...
  "DataType1"=dword:00000002

This is exactly the same format as in a direct registry export.
- Regedit can import these files
- We can import trigger sections from service keys exported by Regedit

Section names should always store trigger's full registry path, including its
index in the list of triggers:
  HKEY_LOCAL_MACHINE\...\ServiceName\TriggerInfo\1
Those who don't need this information can ignore it on import.
}

interface
uses Windows, WinSvc, RegFile, Registry;

type
  //Trigger entry in the .reg file
  TRegTriggerEntry = record
    ServiceName: string;
    Index: integer;
    Trigger: PSERVICE_TRIGGER;
    procedure ReleaseTriggerData;
  end;
  PRegTriggerEntry = ^TRegTriggerEntry;


{
Export to .reg format
}
function SectionFromTrigger(const tr: SERVICE_TRIGGER; const KeyName: string = ''): TRegFileKey; overload;
function SectionFromTrigger(const tr: TRegTriggerEntry): TRegFileKey; overload;

procedure WriteTriggersToFile(reg: TRegFile; const tri: TArray<TRegTriggerEntry>); overload;

//Note: These functions do not know the trigger's full registry path unless you
//  explicitly give it in KeyPrefix
function SaveTriggersToRegString(const tri: SERVICE_TRIGGER_INFO; const KeyPrefix: string = ''): string; overload;
function SaveTriggersToRegString(const tri: TArray<PSERVICE_TRIGGER>; const KeyPrefix: string = ''): string; overload;


{
Import from .reg format
Parses available .reg file sections and decodes those which seem to contain triggers
}
type
  TTriggerImportStatusFlag = (
    sfMalformedFile
  );
  TTriggerImportStatus = set of TTriggerImportStatusFlag;

procedure LoadTriggersFromFile(const AFilename: string; out Triggers: TArray<TRegTriggerEntry>;
  out Status: TTriggerImportStatus);

function SplitServiceRegistryPath(KeyPath: string; out ServiceName: string; out Subkey: string): boolean;
function TryDecodeTriggerSectionName(SectionName: string; out ServiceName: string; out Index: integer): boolean;
function CreateTriggerFromSection(rk: TRegFileKey): PSERVICE_TRIGGER;


{
Direct access to trigger registry keys (both SCM and unrelated ones)
}
function CreateTriggerFromRegistryKey(reg: TRegistry): PSERVICE_TRIGGER;
procedure WriteTriggerToRegistryKey(reg: TRegistry; Trigger: PSERVICE_TRIGGER; const FullKeyPath: string);
procedure DeleteTriggerRegistryKeys(RootKey: HKEY; const KeyPaths: array of string);


{
Disabled triggers
When the trigger is disabled it's deleted from SCM and moved to an unrelated
registry key.
Eventually this might need to be moved to a separate unit.
}
function GetDisabledServiceKey(const AServiceName: string): string;
function GetDisabledTriggersKey(const AServiceName: string): string; inline;
function GetDisabledTriggerKey(const AServiceName: string; const AId: string): string; inline;
function GetNewDisabledTriggerKey(const AServiceName: string): string;

type
  TDisabledTrigger = record
    Id: string;
    Trigger: PSERVICE_TRIGGER;
  end;

function LoadDisabledTriggers(const AServiceName: string): TArray<TDisabledTrigger>;
procedure DisableTrigger(const AServiceName: string; ATrigger: PSERVICE_TRIGGER);
function EnableTrigger(const AServiceName: string; AId: string; ATrigger: PSERVICE_TRIGGER = nil): boolean;
function ReadDisabledTrigger(const AServiceName: string; const AId: string): PSERVICE_TRIGGER;
procedure WriteDisabledTrigger(const AServiceName: string; const AId: string; ATrigger: PSERVICE_TRIGGER);
procedure DeleteDisabledTriggers(const AKeyPaths: array of string);



implementation
uses SysUtils, Classes, UniStrUtils, ServiceHelper, TriggerUtils, RegExport;

procedure TRegTriggerEntry.ReleaseTriggerData;
begin
  if Self.Trigger <> nil then begin
    FreeMem(Self.Trigger);
    Self.Trigger := nil;
  end;
end;


{
Exports a single trigger as a single .reg file section.
KeyName:
  A name of this trigger's section in the file.
Example:
  KeyName=HKLM\Test\1
Export:
  [HKLM\Test\1]
}
function SectionFromTrigger(const tr: SERVICE_TRIGGER; const KeyName: string = ''): TRegFileKey;
var i: integer;
  pDataItem: PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
  IndexStr: string;
begin
  Result.Clear;
  Result.Name := KeyName;
  Result.Delete := false;
  Result.AddDwordValue('Type', REG_DWORD, tr.dwTriggerType);
  Result.AddDwordValue('Action', REG_DWORD, tr.dwAction);
  Result.AddOtherValue('GUID', REG_BINARY, PByte(tr.pTriggerSubtype), SizeOf(TGUID));
  pDataItem := tr.pDataItems;
  for i := 1 to tr.cDataItems do begin
    IndexStr := IntToStr(i-1);
    Result.AddOtherValue('Data'+IndexStr, REG_BINARY, pDataItem^.pData, pDataItem^.cbData);
    Result.AddDwordValue('DataType'+IndexStr, REG_DWORD, pDataItem^.dwDataType);
    Inc(pDataItem);
  end;
end;

//Exports a single trigger with the proper KeyName for the given service and index
function SectionFromTrigger(const tr: TRegTriggerEntry): TRegFileKey;
begin
  Result := SectionFromTrigger(tr.Trigger^, sScmHKEY+GetTriggerKey(tr.ServiceName, tr.Index));
end;

//Exports a number of triggers together to a given TRegFile
procedure WriteTriggersToFile(reg: TRegFile; const tri: TArray<TRegTriggerEntry>);
var i: integer;
begin
  for i := 0 to Length(tri)-1 do
    reg.Add(SectionFromTrigger(tri[i]));
end;


{
Exports all triggers for a given service as a set of exported registry keys.
KeyPrefix:
  A registry path to prepend to trigger key names.
Example:
  KeyPrefix=HKLM\Test1\
Export:
  [HKLM\Test1\0]
  ...
  [HKLM\Test1\1]
  ...
Pass KeyPrefix=='' to export a generic trigger list (non service-bound).
}
function SaveTriggersToRegString(const tri: SERVICE_TRIGGER_INFO; const KeyPrefix: string = ''): string;
var exp: TRegFile;
  key: TRegFileKey;
  i: integer;
  ptr: PSERVICE_TRIGGER;
begin
  exp := TRegFile.Create;
  try
    ptr := tri.pTriggers;
    for i := 1 to tri.cTriggers do begin
      key := SectionFromTrigger(ptr^);
      key.Name := KeyPrefix + IntToStr(i-1);
      exp.Add(key);
      Inc(ptr);
    end;
    Result := exp.ExportToString;
  finally
    FreeAndNil(exp);
  end;
end;

//Same, but the list of triggers is explicitly passed as an array.
function SaveTriggersToRegString(const tri: TArray<PSERVICE_TRIGGER>; const KeyPrefix: string = ''): string; overload;
var exp: TRegFile;
  key: TRegFileKey;
  i: integer;
begin
  exp := TRegFile.Create;
  try
    for i := 0 to Length(tri)-1 do begin
      key := SectionFromTrigger(tri[i]^);
      key.Name := KeyPrefix + IntToStr(i);
      exp.Add(key);
    end;
  finally
    FreeAndNil(exp);
  end;
end;


{
Imports a .reg file containing exported trigger definitions.

You can also pass other registry export files which contain trigger definitions,
including Viper's service configuration exports, and this function
will try to load as much as it can.
}
procedure LoadTriggersFromFile(const AFilename: string; out Triggers: TArray<TRegTriggerEntry>;
  out Status: TTriggerImportStatus);
var reg: TRegFile;
  rk: TRegFileKey;
  i: integer;
  st: TRegTriggerEntry;
begin
  Status := [];
  SetLength(Triggers, 0);

  reg := TRegFile.Create;
  try
    reg.LoadFromFile(AFilename);

    try
      for i := 0 to reg.Count-1 do begin
        rk := reg[i];

        if rk.Delete then begin
          Status := Status + [sfMalformedFile];
          continue;
        end;

        //The section name is our way of checking that this is a trigger export indeed.
        //Do NOT try to load sections with incompatible names - they may have a similar
        //set of parameters but that doesn't mean they're really triggers.
        if not TryDecodeTriggerSectionName(rk.Name, st.ServiceName, st.Index) then begin
          Status := Status + [sfMalformedFile];
          continue;
        end;

        st.Trigger := CreateTriggerFromSection(rk);
        if st.Trigger = nil then begin
          Status := Status + [sfMalformedFile];
          continue;
        end;

        SetLength(Triggers, Length(Triggers)+1);
        Triggers[Length(Triggers)-1] := st;
      end;
    except
      //Release what we have allocated
      for i := 0 to Length(Triggers)-1 do
        Triggers[i].ReleaseTriggerData;
      raise;
    end;

  finally
    FreeAndNil(reg);
  end;
end;

//Parses an absolute registry path.
//Returns true if it leads to some service key's subkey, returns this services'
//name and subkey. Returns false otherwise.
function SplitServiceRegistryPath(KeyPath: string; out ServiceName: string; out Subkey: string): boolean;
var i_pos: integer;
begin
  //Support both HKEY-based and simple paths.
  if KeyPath.StartsWith(sScmHKEY) then
    Delete(KeyPath, 1, Length(sScmHKEY))
    //This leaves us with initial \
  else
  if (Length(KeyPath) > 0) and (KeyPath[1] <> '\') then
    //sScmBasePath requires initial \
    KeyPath := '\' + KeyPath;

  if not KeyPath.StartsWith(sScmBasePath) then begin
    Result := false;
    exit;
  end;
  Delete(KeyPath, 1, 1+Length(sScmBasePath)); //with the "\"

  i_pos := pos('\', KeyPath);
  if i_pos <= 0 then begin
    ServiceName := KeyPath;
    Subkey := '';
  end else begin
    ServiceName := Copy(KeyPath, 1, i_pos-1);
    Delete(KeyPath, 1, i_pos); //with the "\"
    Subkey := KeyPath;
  end;

  Result := true;
end;

//Tries to decode the trigger section name in the default format
//Returns false on failure
function TryDecodeTriggerSectionName(SectionName: string; out ServiceName: string;
  out Index: integer): boolean;
begin
  if not SplitServiceRegistryPath(SectionName, ServiceName, SectionName) then begin
    ServiceName := '';
    Index := -1;
    Result := false;
    exit;
  end;

  if not SectionName.StartsWith(sTriggersSubkey) then begin
    Result := false;
    exit;
  end;
  Delete(SectionName, 1, Length(sTriggersSubkey)+1); //with the "\"

  Result := TryStrToInt(SectionName, Index); //all that should remain is the index
end;


const
  //We don't support more data items cause it would be weird (and eat memory).
  MAX_DATAITEMS = 64;

{
Temporary list to hold the list of data items being constructed.
This is complicated since they may go out of order, we have to track which
parameters are filled for each of them.
}
type
  TDataItemEntry = record
    item: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
    haveType: boolean;
    haveData: boolean;
    procedure Reset;
    procedure Free;
  end;
  PDataItemEntry = ^TDataItemEntry;
  TDataItemListBuilder = class
  protected
    FItems: TArray<TDataItemEntry>;
    procedure Grow(ACount: integer);
  public
    destructor Destroy; override;
    function Get(AIndex: integer): PDataItemEntry;
    procedure Pack;
    function AsList: TArray<SERVICE_TRIGGER_SPECIFIC_DATA_ITEM>;
  end;

procedure TDataItemEntry.Reset;
begin
  Self.haveType := false;
  Self.haveData := false;
  Self.item.dwDataType := 0;
  Self.item.cbData := 0;
  Self.item.pData := nil;
end;

procedure TDataItemEntry.Free;
begin
  if Self.item.pData <> nil then
    FreeMem(Self.item.pData);
end;

destructor TDataItemListBuilder.Destroy;
var i: integer;
begin
  for i := 0 to Length(FItems)-1 do
    FItems[i].Free;
  inherited;
end;

function TDataItemListBuilder.Get(AIndex: integer): PDataItemEntry;
begin
  if Length(FItems) <= AIndex then
    Grow(AIndex-Length(FItems)+1);
  Result := @FItems[AIndex];
end;

//Adds ACount zero-initialized elements at the end.
procedure TDataItemListBuilder.Grow(ACount: integer);
var pe: PDataItemEntry;
begin
  SetLength(FItems, Length(FItems)+ACount);
  while ACount > 0 do begin
    pe := @FItems[Length(FItems)-ACount];
    pe.Reset;
    Dec(ACount);
  end;
end;

//Remove any data items which have not been filled.
procedure TDataItemListBuilder.Pack;
var i, shift: integer;
begin
  i := 0;
  shift := 0;
  while i + shift < Length(FItems) do begin
    if not FItems[i+shift].haveType or not FItems[i+shift].haveData then begin
      FItems[i+shift].Free;
      Inc(shift);
    end else begin
      if shift > 0 then
        FItems[i] := FItems[i+shift];
      Inc(i);
    end;
  end;
  if shift > 0 then
    SetLength(FItems, Length(FItems)-shift);
end;

//Constructs an array of STS_DATA_ITEM suitable for using in a SERVICE_TRIGGER record.
//The data itself is still owned by the builder and will be freed on destruction.
function TDataItemListBuilder.AsList: TArray<SERVICE_TRIGGER_SPECIFIC_DATA_ITEM>;
var i: integer;
begin
  SetLength(Result, Length(FItems));
  for i := 0 to Length(FItems)-1 do
    Result[i] := FItems[i].item;
end;

{
Tries to parse a single reg file key as a trigger description.
Returns a new trigger contents or nil, if there was no trigger data in this key.
NOTE: You have to free the returned PSERVICE_TRIGGER.
}
function CreateTriggerFromSection(rk: TRegFileKey): PSERVICE_TRIGGER;
var st: SERVICE_TRIGGER;
  guid: TGuid;
  haveType: boolean;
  haveAction: boolean;
  //haveGuid is determined by st.pTriggerSubtype being set
  //We will collect data items in this structure:
  data: TDataItemListBuilder;
  dataPtrs: TArray<SERVICE_TRIGGER_SPECIFIC_DATA_ITEM>;
  //Temporary variables
  i, idx, tmp: integer;
  re: TRegFileEntry;
  pde: PDataItemEntry;
begin
  st.dwTriggerType := 0;
  st.dwAction := 0;
  st.pTriggerSubtype := nil;
  st.cDataItems := 0;
  st.pDataItems := nil;

  haveType := false;
  haveAction := false;

  data := TDataItemListBuilder.Create;
  try

    for i := 0 to Length(rk.Entries)-1 do begin
      re := rk.Entries[i];
      if re.DataType = REG_DELETE then
        continue;

      if SameText(re.Name, 'Type') and TryStrToInt('$'+re.Data, tmp) then begin
        haveType := true;
        st.dwTriggerType := tmp;
        continue;
      end;

      if SameText(re.Name, 'Action') and TryStrToInt('$'+re.Data, tmp) then begin
        haveAction := true;
        st.dwAction := tmp;
        continue;
      end;

      if SameText(re.Name, 'Guid') then begin
        st.pTriggerSubtype := @guid;
        RegHexToBin(AnsiString(re.Data), @guid, SizeOf(guid));
        continue;
      end;

      if re.Name.StartsWith('DataType') and TryStrToInt(re.Name.Substring(8), idx)
      and (idx < MAX_DATAITEMS)
      and (re.DataType = REG_DWORD) and TryStrToInt('$'+re.Data, tmp) then begin
        pde := data.Get(idx);
        pde.item.dwDataType := tmp;
        pde.haveType := true;
        continue;
      end;

      if re.Name.StartsWith('Data') and TryStrToInt(re.Name.Substring(4), idx)
      and (idx < MAX_DATAITEMS) then begin
        pde := data.Get(idx);
        pde.item.pData := RegHexToBin(AnsiString(re.Data), @pde.item.cbData);
        pde.haveData := true;
        continue;
      end;

      //The rest we can't decode

    end;


    //We have to decide whether this is a trigger definition at all.
    //This shall be determined by the presence of the three properties:
    //  Type, Action and GUID.
    if not haveType or not haveAction or (st.pTriggerSubtype = nil) then begin
      Result := nil;
      exit;
    end;

    //Remove unused data item slots
    data.Pack;

    //Fill all SERVICE_TRIGGER fields
    dataPtrs := data.AsList;
    st.cDataItems := Length(dataPtrs);
    if st.cDataItems <> 0 then
      st.pDataItems := @dataPtrs[0]
    else
      st.pDataItems := nil;

    //Now copy the structure to a single flat one which the caller should free
    Result := CopyTrigger(st);

  finally
    //Free any temporary memory we've allocated for data items
    FreeAndNil(data);
  end;
end;



{
Reads a SCM compatible trigger definition from a currently open registry key
and creates a new PSERVICE_TRIGGER out of it.
Returns
  PSERVICE_TRIGGER which has to be freed by the caller.
  nil if the key does not contain a valid trigger registry definition
}
function CreateTriggerFromRegistryKey(reg: TRegistry): PSERVICE_TRIGGER;
var regf: TRegFile;
begin
  //Export + parse as trigger in standard way
  Result := nil;
  regf := ExportRegistryKey(reg);
  if regf = nil then
    exit;
  try
    //The trigger key itself is flat. Someone could have manually created subfolders,
    //but we are going to ignore them
    if regf.Count < 1 then
      exit;
    Result := CreateTriggerFromSection(regf[0]);
  finally
    FreeAndNil(regf);
  end;
end;

{
Writes a given trigger to the given registry key (possibly unrelated).
Trigger is written in a format identical to the one used by the SCM.
FullKeyPath must include HKEY_* root key.
}
procedure WriteTriggerToRegistryKey(reg: TRegistry; Trigger: PSERVICE_TRIGGER;
  const FullKeyPath: string);
var key: TRegFileKey;
begin
  //Export the trigger to Reg file format structure + import the structure
  key := SectionFromTrigger(Trigger^, FullKeyPath);
  WriteToRegistry(reg, key);
end;

//Each key: a registry path without HKEY
procedure DeleteTriggerRegistryKeys(RootKey: HKEY; const KeyPaths: array of string);
var reg: TRegistry;
  key: string;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := RootKey;
    for key in KeyPaths do begin
      //Safety: NEVER delete '' (empty string) or top-level keys.
      RegAssertDepth(key, 2);
      reg.DeleteKey(key);
    end;
  finally
    FreeAndNil(reg);
  end;
end;


{
Disable / enable triggers
}
const
  sDisabledServicesKey = '\Software\Viper\DisabledServices'; //in HKLM

function GetDisabledServiceKey(const AServiceName: string): string;
begin
  Result := sDisabledServicesKey+'\'+AServiceName;
end;

function GetDisabledTriggersKey(const AServiceName: string): string;
begin
  Result := GetDisabledServiceKey(AServiceName) + '\TriggerInfo'
end;

function GetDisabledTriggerKey(const AServiceName: string; const AId: string): string;
begin
  Result := GetDisabledTriggersKey(AServiceName) + '\' + AId;
end;

//Generates a new, unused key name in DisabledServices\ServiceName\Triggers\
function GetNewDisabledTriggerKey(const AServiceName: string): string;
var guid: TGuid;
begin
  //Real SERVICES key stores triggers under integer indexes,
  //but DisabledServices is not meant to be export-compatible so we don't have to.
  CreateGuid(guid);
  Result := GetDisabledTriggersKey(AServiceName) + '\' + GuidToString(guid);
end;

//Reads the list of disabled PSERVICE_TRIGGER entries for the given service
//The caller has to free the entries
function LoadDisabledTriggers(const AServiceName: string): TArray<TDisabledTrigger>;
var reg: TRegistry;
  subkeys: TStringList;
  subkey: string;
  i: integer;
  trig: PSERVICE_TRIGGER;
begin
  SetLength(Result, 0);

  try
    subkeys := nil;
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      if not reg.OpenKeyReadOnly(GetDisabledTriggersKey(AServiceName)) then
        exit; //no disabled ones

      subkeys := TStringList.Create;
      reg.GetKeyNames(subkeys);
      for subkey in subkeys do begin
        if not reg.OpenKeyReadOnly(GetDisabledTriggerKey(AServiceName, subkey)) then
          continue; //cannot read, don't complain
        trig := CreateTriggerFromRegistryKey(reg);
        //if loaded, store
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1].Id := subkey;
        Result[Length(Result)-1].Trigger := trig;
      end;
    finally
      FreeAndNil(reg);
      FreeAndNil(subkeys);
    end;
  except
    //Do not leak memory
    for i := 0 to Length(Result)-1 do
      FreeMem(Result[i].Trigger);
    raise;
  end;
end;

//Creates a new instance of PSERVICE_TRIGGER populated with the data of the disabled
//trigger with a given Id
//Returns nil if no such trigger exists.
function ReadDisabledTrigger(const AServiceName: string; const AId: string): PSERVICE_TRIGGER;
var reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if not reg.OpenKeyReadOnly(GetDisabledTriggerKey(AServiceName, AId)) then begin
      Result := nil;
      exit;
    end;
    Result := CreateTriggerFromRegistryKey(reg);
  finally
    FreeAndNil(reg);
  end;
end;

//Updates disabled trigger contents with the data from the given PSERVICE_TRIGGER
procedure WriteDisabledTrigger(const AServiceName: string; const AId: string; ATrigger: PSERVICE_TRIGGER);
var reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    WriteTriggerToRegistryKey(
      reg,
      ATrigger,
      sHkeyLocalMachine + GetDisabledTriggerKey(AServiceName, AId)
    );
  finally
    FreeAndNil(reg);
  end;
end;

procedure DisableTrigger(const AServiceName: string; ATrigger: PSERVICE_TRIGGER);
var reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;

    //Create the disabled version
    WriteTriggerToRegistryKey(
      reg,
      ATrigger,
      sHkeyLocalMachine + GetNewDisabledTriggerKey(AServiceName)
    );

    //Delete the live version
    with OpenService2(AServiceName, SC_MANAGER_CONNECT,
      SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG)
    do
      DeleteServiceTriggers(SvcHandle, [ATrigger]);
  finally
    FreeAndNil(reg);
  end;
end;

//If you pass ATrigger it'll be used, otherwise it'll be read anew from registry
function EnableTrigger(const AServiceName: string; AId: string; ATrigger: PSERVICE_TRIGGER = nil): boolean;
var reg: TRegistry;
  trig: PSERVICE_TRIGGER;
  KeyPath: string;
begin
  Result := false;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    KeyPath := GetDisabledTriggerKey(AServiceName, AId);

    if ATrigger <> nil then
      trig := ATrigger
    else begin
      if not reg.OpenKeyReadOnly(KeyPath) then
        exit; //cannot read source, don't continue
      trig := CreateTriggerFromRegistryKey(reg);
      if trig = nil then
        exit;
    end;

    try
      with OpenService2(AServiceName, SC_MANAGER_CONNECT,
        SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG)
      do
        AddServiceTriggers(SvcHandle, [ATrigger^]);
    finally
      if ATrigger = nil then //trig created internally
        FreeMem(trig);
    end;

    //If the above worked, delete the disabled instance
    //Safety: NEVER delete '' (empty string) or top-level keys.
    RegAssertDepth(KeyPath, 2);
    reg.DeleteKey(KeyPath);
  finally
    FreeAndNil(reg);
  end;
end;

procedure DeleteDisabledTriggers(const AKeyPaths: array of string);
begin
  DeleteTriggerRegistryKeys(HKEY_LOCAL_MACHINE, AKeyPaths);
end;


end.
