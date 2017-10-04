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
uses WinSvc;

function ExportTriggers(const tri: SERVICE_TRIGGER_INFO;
  const KeyPrefix: AnsiString = ''): AnsiString; overload;

function ExportTrigger(const tri: TArray<SERVICE_TRIGGER>;
  const KeyPrefix: AnsiString = ''): AnsiString; overload;

function ExportTrigger(const tr: SERVICE_TRIGGER;
  const KeyName: AnsiString = ''): AnsiString; overload;

function ExportTriggerDataItem(const item: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
  const IndexStr: AnsiString): AnsiString;

implementation
uses SysUtils, UniStrUtils, ServiceHelper, TriggerUtils;

//Converts binary data to "A1,00,3F,25" hex representation
function BinToRegHex(pb: PByte; cb: cardinal): AnsiString;
var i: integer;
  pc: PAnsiChar;
begin
  SetLength(Result, cb*3);
  if cb > 0 then begin
    pc := @Result[1];
    for i := 1 to cb do begin
      ByteToHex(pb^, (pc+0)^, (pc+1)^);
      (pc+2)^ := ',';
      Inc(pb);
      Inc(pc, 3);
    end;
    SetLength(Result, Length(Result)-1);
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


end.
