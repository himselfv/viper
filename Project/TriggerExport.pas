unit TriggerExport;
{
Trigger import/export.

Triggers are stored in the registry, in the service key's TriggerInfo subkey.
When exported to a .reg file format, they assume a certain form which we here
reproduce.
  "Type"=dword:00000004
  "Action"=dword:00000001
  "GUID"=hex:07,9e,56,b7,21,...
  "Data0"=hex:31,00,33,00,39,00,...
  "DataType0"=dword:00000002
  "Data1"=hex:31,00,33,00,37,00,...
  "DataType1"=dword:00000002
}

interface
uses WinSvc, RegFile;

function ExportTriggers(const tri: SERVICE_TRIGGER_INFO; const KeyPrefix: AnsiString = ''): AnsiString; overload;

function ExportTrigger(const tri: TArray<SERVICE_TRIGGER>; const KeyPrefix: AnsiString = ''): AnsiString; overload;

function ExportTrigger(const tr: SERVICE_TRIGGER; const KeyName: AnsiString = ''): AnsiString; overload;

function ExportTriggerDataItem(const item: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
  const IndexStr: AnsiString): AnsiString;

type
  TTriggerImportStatusFlag = (
    sfMalformedFile
  );
  TTriggerImportStatus = set of TTriggerImportStatusFlag;

procedure ImportTriggers(const AFilename: string; out Triggers: TArray<PSERVICE_TRIGGER>;
  out Status: TTriggerImportStatus);

function ImportTrigger(rk: TRegFileKey): PSERVICE_TRIGGER;

implementation
uses SysUtils, Windows, UniStrUtils, ServiceHelper, TriggerUtils;

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
  const KeyPrefix: AnsiString = ''): AnsiString;
var i: integer;
  ptr: PSERVICE_TRIGGER;
begin
  ptr := tri.pTriggers;
  for i := 1 to tri.cTriggers do begin
    Result := Result
      + '['+KeyPrefix+AnsiString(IntToStr(i-1))+']'#13
      + ExportTrigger(ptr^) + #13;
    Inc(ptr);
  end;
  if Length(Result) > 0 then
    SetLength(Result, Length(Result)-1);
end;

//Same, but the list of triggers is explicitly passed as an array.
function ExportTrigger(const tri: TArray<SERVICE_TRIGGER>;
  const KeyPrefix: AnsiString = ''): AnsiString; overload;
var i: integer;
begin
  for i := 0 to Length(tri)-1 do begin
    Result := Result
      + '['+KeyPrefix+AnsiString(IntToStr(i))+']'#13
      + ExportTrigger(tri[i]) + #13;
  end;
  if Length(Result) > 0 then
    SetLength(Result, Length(Result)-1);
end;

//Exports a single trigger as a set of exported registry values (without a key)
//If KeyName is specified, that key header will be added at the beginning.
function ExportTrigger(const tr: SERVICE_TRIGGER;
  const KeyName: AnsiString = ''): AnsiString;
var i: integer;
  pDataItem: PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
begin
  if KeyName = '' then
    Result := ''
  else
    Result := '[' + KeyName + ']'#13;
  Result := Result +
     '"Type"=dword:'+AnsiString(IntToHex(tr.dwTriggerType, 8))+#13
    +'"Action"=dword:'+AnsiString(IntToHex(tr.dwAction, 8))+#13
    +'"GUID"=hex:'+BinToRegHex(PByte(tr.pTriggerSubtype), SizeOf(TGUID));
  pDataItem := tr.pDataItems;
  for i := 1 to tr.cDataItems do begin
    Result := Result + #13 + ExportTriggerDataItem(pDataItem^, AnsiString(IntToStr(i-1)));
    Inc(pDataItem);
  end;
end;

//Returns a .reg-file formatted chunk of several strings defining the keys
//which fully describe this trigger data item.
function ExportTriggerDataItem(const item: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
  const IndexStr: AnsiString): AnsiString;
begin
  Result := '"Data'+IndexStr+'"=hex:'+BinToRegHex(item.pData, item.cbData)+#13
    +'"DataType'+IndexStr+'"=dword:'+AnsiString(IntToHex(item.dwDataType, 8));
end;


{
Imports a .reg file containing exported trigger definitions.

A well-formed Viper registry export file is service-neutral and looks like this:
  [0]
  //service params
  [1]
  //service params
  //etc...

You can also pass other registry export files which contain trigger definitions,
including Viper's service configuration exports, and this function
will try to load as much as it can.
}
procedure ImportTriggers(const AFilename: string; out Triggers: TArray<PSERVICE_TRIGGER>;
  out Status: TTriggerImportStatus);
var reg: TRegFile;
  rk: TRegFileKey;
  i: integer;
  st: PSERVICE_TRIGGER;
begin
  Status := [];
  SetLength(Triggers, 0);

  reg := TRegFile.Create;
  try
    reg.LoadFromFile(AFilename);

    try
      for i := 0 to reg.Count-1 do begin
        rk := reg[i];
        if rk.Name <> '['+IntToStr(i)+']' then
          Status := Status + [sfMalformedFile];

        if rk.Delete then begin
          Status := Status + [sfMalformedFile];
          continue;
        end;

        st := ImportTrigger(rk);
        if st = nil then begin
          Status := Status + [sfMalformedFile];
          continue;
        end;

        SetLength(Triggers, Length(Triggers)+1);
        Triggers[Length(Triggers)-1] := st;
      end;
    except
      //Release what we have allocated
      for i := 0 to Length(Triggers)-1 do
        FreeMem(Triggers[i]);
      raise;
    end;

  finally
    FreeAndNil(reg);
  end;
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
  TDataItemListConstructor = class
  protected
    FItems: TArray<TDataItemEntry>;
    procedure Grow(ACount: integer);
  public
    destructor Destroy; override;
    function Get(AIndex: integer): PDataItemEntry;
    procedure Pack;
    function GetPointerArray: TArray<PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM>;
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

destructor TDataItemListConstructor.Destroy;
var i: integer;
begin
  for i := 0 to Length(FItems)-1 do
    FItems[i].Free;
  inherited;
end;

function TDataItemListConstructor.Get(AIndex: integer): PDataItemEntry;
begin
  if Length(FItems) <= AIndex then
    Grow(Length(FItems)-AIndex+1);
  Result := @FItems[AIndex];
end;

//Adds ACount zero-initialized elements at the end.
procedure TDataItemListConstructor.Grow(ACount: integer);
var pe: PDataItemEntry;
begin
  SetLength(FItems, Length(FItems)+ACount);
  while ACount > 0 do begin
    pe := @FItems[Length(FItems)-ACount];
    pe.Reset;
  end;
end;

//Remove any data items which have not been filled.
procedure TDataItemListConstructor.Pack;
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

function TDataItemListConstructor.GetPointerArray: TArray<PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM>;
var i: integer;
begin
  SetLength(Result, Length(FItems));
  for i := 0 to Length(FItems)-1 do
    Result[i] := @FItems[i].item;
end;


//Tries to parse a single reg file key as a trigger description.
//Returns a new trigger contents (which you have to free), or nil, if there
//was no trigger data in this key.
function ImportTrigger(rk: TRegFileKey): PSERVICE_TRIGGER;
var st: SERVICE_TRIGGER;
  guid: TGuid;
  haveType: boolean;
  haveAction: boolean;
  //haveGuid is determined by st.pTriggerSubtype being set
  //We will collect data items in this structure:
  data: TDataItemListConstructor;
  dataPtrs: TArray<PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM>;
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

  data := TDataItemListConstructor.Create;
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

      if re.Name.StartsWith('DataType') and TryStrToInt(re.Name.Substring(9), idx)
      and (idx < MAX_DATAITEMS)
      and (re.DataType = REG_DWORD) and TryStrToInt('$'+re.Data, tmp) then begin
        pde := data.Get(idx);
        pde.item.dwDataType := tmp;
        pde.haveType := true;
        continue;
      end;

      if re.Name.StartsWith('Data') and TryStrToInt(re.Name.Substring(5), idx)
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
    dataPtrs := data.GetPointerArray;
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
