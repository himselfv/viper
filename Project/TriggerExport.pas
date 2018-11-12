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
uses WinSvc, RegFile;

type
  //Trigger entry in the export file
  TRegTriggerEntry = record
    ServiceName: string;
    Index: integer;
    Trigger: PSERVICE_TRIGGER;
    procedure ReleaseTriggerData;
  end;


// Export

function ExportTrigger(const tr: SERVICE_TRIGGER; const KeyName: string = ''): TRegFileKey; overload;
function ExportTrigger(const tr: TRegTriggerEntry): TRegFileKey; overload; inline;

procedure ExportTriggers(reg: TRegFile; const tri: TArray<TRegTriggerEntry>); overload;
function ExportTriggers(const tri: TArray<TRegTriggerEntry>): string; overload;

//Note: These functions do not know the trigger's full registry path unless you
//  explicitly give it in KeyPrefix
function ExportTriggers(const tri: SERVICE_TRIGGER_INFO; const KeyPrefix: string = ''): string; overload;
function ExportTriggers(const tri: TArray<SERVICE_TRIGGER>; const KeyPrefix: string = ''): string; overload;
function ExportTriggers(const tri: TArray<PSERVICE_TRIGGER>; const KeyPrefix: string = ''): string; overload;


// Import
// Parses available registry file sections and decodes those which seem to contain triggers

function TryDecodeTriggerSectionName(SectionName: string; out ServiceName: string; out Index: integer): boolean;
function ImportTrigger(rk: TRegFileKey): PSERVICE_TRIGGER;

type
  TTriggerImportStatusFlag = (
    sfMalformedFile
  );
  TTriggerImportStatus = set of TTriggerImportStatusFlag;

procedure ImportTriggers(const AFilename: string; out Triggers: TArray<TRegTriggerEntry>;
  out Status: TTriggerImportStatus);


implementation
uses SysUtils, Windows, UniStrUtils, ServiceHelper, TriggerUtils;

procedure TRegTriggerEntry.ReleaseTriggerData;
begin
  if Self.Trigger <> nil then begin
    FreeMem(Self.Trigger);
    Self.Trigger := nil;
  end;
end;


{
Exports a single trigger as a set of exported registry values.
KeyName:
  A name of this trigger's section in the file.
Example:
  KeyName=HKLM\Test\1
Export:
  [HKLM\Test\1]
}
function ExportTrigger(const tr: SERVICE_TRIGGER; const KeyName: string = ''): TRegFileKey;
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
function ExportTrigger(const tr: TRegTriggerEntry): TRegFileKey;
begin
  Result := ExportTrigger(tr.Trigger^, GetTriggerKeyFull(tr.ServiceName) + '\' + IntToStr(tr.Index));
end;


//Exports a number of triggers together to a given TRegFile
procedure ExportTriggers(reg: TRegFile; const tri: TArray<TRegTriggerEntry>);
var i: integer;
begin
  for i := 0 to Length(tri)-1 do
    reg.Add(ExportTrigger(tri[i]));
end;

//Exports a number of triggers together to a string
function ExportTriggers(const tri: TArray<TRegTriggerEntry>): string;
var exp: TRegFile;
begin
  exp := TRegFile.Create;
  try
    ExportTriggers(exp, tri);
    Result := exp.ExportToString;
  finally
    FreeAndNil(exp);
  end;
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
function ExportTriggers(const tri: SERVICE_TRIGGER_INFO;
  const KeyPrefix: string = ''): string;
var exp: TRegFile;
  key: TRegFileKey;
  i: integer;
  ptr: PSERVICE_TRIGGER;
begin
  exp := TRegFile.Create;
  try
    ptr := tri.pTriggers;
    for i := 1 to tri.cTriggers do begin
      key := ExportTrigger(ptr^);
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
function ExportTriggers(const tri: TArray<SERVICE_TRIGGER>;
  const KeyPrefix: string = ''): string; overload;
var exp: TRegFile;
  key: TRegFileKey;
  i: integer;
begin
  exp := TRegFile.Create;
  try
    for i := 0 to Length(tri)-1 do begin
      key := ExportTrigger(tri[i]);
      key.Name := KeyPrefix + IntToStr(i);
      exp.Add(key);
    end;
    Result := exp.ExportToString;
  finally
    FreeAndNil(exp);
  end;
end;

function ExportTriggers(const tri: TArray<PSERVICE_TRIGGER>;
  const KeyPrefix: string = ''): string; overload;
var exp: TRegFile;
  key: TRegFileKey;
  i: integer;
begin
  exp := TRegFile.Create;
  try
    for i := 0 to Length(tri)-1 do begin
      key := ExportTrigger(tri[i]^);
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
procedure ImportTriggers(const AFilename: string; out Triggers: TArray<TRegTriggerEntry>;
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

        st.Trigger := ImportTrigger(rk);
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

//Tries to decode the trigger section name in the default format
//Returns false on failure
function TryDecodeTriggerSectionName(SectionName: string; out ServiceName: string;
  out Index: integer): boolean;
var i_pos: integer;
begin
  ServiceName := '';
  Index := -1;

  if not SectionName.StartsWith(sHkeyLocalMachine+sBaseScmKey) then begin
    Result := false;
    exit;
  end;
  Delete(SectionName, 1, Length(sHkeyLocalMachine)+Length(sBaseScmKey)+1); //with the "\"

  i_pos := pos('\', SectionName);
  if i_pos <= 0 then begin
    Result := false;
    exit;
  end;

  ServiceName := Copy(SectionName, 1, i_pos-1);
  Delete(SectionName, 1, i_pos); //with the "\"

  if not SectionName.StartsWith(sTriggerSubkey) then begin
    Result := false;
    exit;
  end;
  Delete(SectionName, 1, Length(sTriggerSubkey)+1); //with the "\"

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
function ImportTrigger(rk: TRegFileKey): PSERVICE_TRIGGER;
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
    st.pDataItems := @dataPtrs[0];
    st.cDataItems := Length(dataPtrs);

    //Now copy the structure to a single flat one which the caller should free
    Result := CopyTrigger(st);

  finally
    //Free any temporary memory we've allocated for data items
    FreeAndNil(data);
  end;

end;

end.
