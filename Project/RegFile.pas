unit RegFile;
{
Registry export format parser/
Reg file format specifications:
  http://support.microsoft.com/kb/310516
  https://msdn.microsoft.com/en-us/library/gg469889.aspx

We support a LIMITED subset of functionality that our own export uses.
Limitations:
- Only 5.00 format is supported
- Multiline values not supported
- No key name escapting (there seems to be none)
- We don't preserve comments or formatting on load-save
- Not all data types are supported

We support:
- Empty lines
- Comments
- Default values (@)
- Entry name and value escaping
- Key and value removal
}

interface
uses SysUtils, Classes, Windows, Generics.Collections;

const //Additional data types
  REG_QWORD  = 11;  //Missing in Winapi.Windows
  REG_DELETE = -1;  //Custom type to store deletion marker


resourcestring
  sRegFileFormat500 = 'Windows Registry Editor Version 5.00';

type
  TRegFileEntry = record
    Name: string;
    DataType: integer;
    Data: string; {
      In the appropriate data type:
        REG_SZ: string, unescaped
        DWORD: hex dword, 8 characters
      Everything else, including:
        REG_BINARY
        REG_EXPAND_SZ
        REG_MULTI_SZ
        REG_QWORD:  hex string
    }
    //Use these setters
    procedure SetDwordValue(const val: dword); inline;
    procedure SetStringValue(const val: string); inline;
    procedure SetOtherValue(const pb: PByte; cb: cardinal); inline;
    property AsDword: dword write SetDwordValue;
    property AsString: string write SetStringValue;
    //Export
    function ExportToString: string;
  end;

  TRegFileKey = record
    Name: string;
    Delete: boolean;
    Entries: TArray<TRegFileEntry>;
    procedure AddValueEntry(const Entry: TRegFileEntry);
    //Shortcuts
    procedure AddStringValue(const Name: string; DataType: integer; const Data: string);
    procedure AddDwordValue(const Name: string; DataType: integer; const Data: dword);
    procedure AddOtherValue(const Name: string; DataType: integer; const Data: PByte; const Size: integer);
    //Export
    procedure ExportToStrings(sl: TStrings);
    function ExportToString: string;
  end;

  TRegFile = class(TList<TRegFileKey>)
  protected
    function ParseNameValue(const line: string; out re: TRegFileEntry): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToFile(const AFilename: string);
    procedure SaveToStrings(sl: TStrings);
    function ExportToString: string;
  end;

  ERegFileFormatError = class(Exception);


//Convert binary data to hex string and back
function RegBinToHex(pb: PByte; cb: cardinal): AnsiString;
procedure RegHexToBin(const hex: AnsiString; pb: PByte; cb: cardinal); overload;
function RegHexToBin(const hex: AnsiString; cb: PCardinal = nil): PByte; overload;

//Escape - do not apply to values manually
function RegEscapeString(const val: string): string;
function RegUnescapeString(const val: string): string;


// Root keys

const
  RootKeyNames: array[HKEY_CLASSES_ROOT..HKEY_DYN_DATA] of string = (
    'HKEY_CLASSES_ROOT',
    'HKEY_CURRENT_USER',
    'HKEY_LOCAL_MACHINE',
    'HKEY_USERS',
    'HKEY_PERFORMANCE_DATA',
    'HKEY_CURRENT_CONFIG',
    'HKEY_DYN_DATA');

const
  RootShortKeyNames: array[HKEY_CLASSES_ROOT..HKEY_DYN_DATA] of string = (
    'HKCR', 'HKCU', 'HKLM', 'HKU', 'HKPD', 'HKCC', 'HKDD');

function RootKeyToStr(const AKey: HKEY): string;
function RootKeyToShortStr(const AKey: HKEY): string;


implementation
uses UniStrUtils;
{
Short registry format description.
Overall format:
  <Format version str>

  [KEY PATH]
  name=value
  name=value

  ;comment
  [KEY PATH]
  name=value

Format versions:
  Only 5.00 is supported (see below)

Key names:
  Non-encoded, must not contain []s, \s treated as subkey transitions.
  [-KEY] means delete KEY.

Names and values are encoded: '\' => '\\'; '"' => '\"'
Various formats are stored like this:

[HKEY_LOCAL_MACHINE\SOFTWARE\Temp]
"stringParamNoValue"=""
"stringParam"="some value"
"binaryParamNoValue"=hex:
"binaryParam"=hex:ab,cd,12,34,55,66,77,88,99
"dwordParam"=dword:00000123
"qwordParam"=hex(b):23,01,00,00,00,00,00,00
"multiStrParam"=hex(7):61,00,73,00,64,00,00,00,62,00,73,00,64,00,00,00,6c,00,\
  69,00,6e,00,65,00,33,00,00,00,6c,00,69,00,6e,00,65,00,34,00,00,00,6c,00,69,\
  00,6e,00,65,00,36,00,00,00,00,00
"expandStrParam"=hex(2):65,00,78,00,70,00,61,00,6e,00,64,00,20,00,74,00,65,00,\
  78,00,74,00,00,00
"deleteParam"=-
}


//Converts root HKEYs such as HKEY_LOCAL_MACHINE to their string constant names
function RootKeyToStr(const AKey: HKEY): string;
begin
  if (AKey >= Low(RootKeyNames)) and (AKey <= High(RootKeyNames)) then
    Result := RootKeyNames[AKey]
  else
    Result := 'HKEY('+IntToStr(AKey)+')';
end;

//Converts root HKEYs such as HKEY_LOCAL_MACHINE to their short string names such as HKLM
function RootKeyToShortStr(const AKey: HKEY): string;
begin
  if (AKey >= Low(RootShortKeyNames)) and (AKey <= High(RootShortKeyNames)) then
    Result := RootShortKeyNames[AKey]
  else
    Result := 'HKEY('+IntToStr(AKey)+')';
end;


//Sets TRegFileEntry value in dword format
procedure TRegFileEntry.SetDwordValue(const val: dword);
begin
  Self.Data := IntToHex(val, 8);
end;

procedure TRegFileEntry.SetStringValue(const val: string);
begin
  //Do not escape the string here, we'll do it on writing
  Self.Data := val;
end;

procedure TRegFileEntry.SetOtherValue(const pb: PByte; cb: cardinal);
begin
  //All the other values are simply presented as hex
  Self.Data := string(RegBinToHex(pb, cb));
end;

//Compiles the exported string that represents this name=value pair in the .reg file
function TRegFileEntry.ExportToString: string;
var nameStr: string;
begin
  //Entry format:
  //  "Name"=datatype:value
  //  "Name"="string value"
  //  "Name"=-

  if Self.Name = '' then //default value
    nameStr := '@='
  else
    nameStr := '"'+RegEscapeString(Self.Name)+'"=';

  case Self.DataType of
    REG_DELETE: Result := nameStr+'-';
    REG_SZ: Result := nameStr+'"'+RegEscapeString(Self.Data)+'"';
    REG_BINARY: Result := nameStr+'hex:'+Self.Data;
    REG_DWORD: Result := nameStr+'dword:'+Self.Data;
  else
   //All the other types are stored as hex!
    Result := nameStr + 'hex('+IntToHex(Self.DataType, 1)+'):' + Self.Data;
  end;
end;


procedure TRegFileKey.AddValueEntry(const Entry: TRegFileEntry);
begin
  SetLength(Self.Entries, Length(Self.Entries)+1);
  Self.Entries[Length(Self.Entries)-1] := Entry;
end;

procedure TRegFileKey.AddStringValue(const Name: string; DataType: integer; const Data: string);
var Entry: TRegFileEntry;
begin
  Entry.Name := Name;
  Entry.DataType := DataType;
  Entry.SetStringValue(Data);
  Self.AddValueEntry(Entry);
end;

procedure TRegFileKey.AddDwordValue(const Name: string; DataType: integer; const Data: dword);
var Entry: TRegFileEntry;
begin
  Entry.Name := Name;
  Entry.DataType := DataType;
  Entry.SetDwordValue(Data);
  Self.AddValueEntry(Entry);
end;

procedure TRegFileKey.AddOtherValue(const Name: string; DataType: integer; const Data: PByte; const Size: integer);
var Entry: TRegFileEntry;
begin
  Entry.Name := Name;
  Entry.DataType := DataType;
  Entry.SetOtherValue(Data, Size);
  Self.AddValueEntry(Entry);
end;

//Appends the exported set of string that represents this section in the .reg file
procedure TRegFileKey.ExportToStrings(sl: TStrings);
var i: integer;
begin
  if not Self.Delete then
    sl.Add('['+Self.Name+']')
  else
    sl.Add('[-'+Self.Name+']');

  for i := 0 to Length(Self.Entries)-1 do
    sl.Add(Self.Entries[i].ExportToString);
end;

function TRegFileKey.ExportToString: string;
var sl: TStringList;
begin
  sl := TStringList.Create;
  try
    Self.ExportToStrings(sl);
    Result := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;


//Converts binary data to "A1,00,3F,25" hex representation
function RegBinToHex(pb: PByte; cb: cardinal): AnsiString;
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

procedure RegHexToBin(const hex: AnsiString; pb: PByte; cb: cardinal);
var tmp: AnsiString;
begin
  tmp := AnsiReplaceStr(hex, ',', '');
  HexToBin(tmp, pb, cb);
end;

//Allocate the required amount of memory with GetMem and decode the data
function RegHexToBin(const hex: AnsiString; cb: PCardinal): PByte;
var tcb: cardinal;
begin
  try
    tcb := Length(hex) div 3;
    GetMem(Result, tcb);
    RegHexToBin(hex, Result, tcb);
    if cb <> nil then
      cb^ := tcb;
  except
    FreeMem(Result);
  end;
end;

function RegEscapeString(const val: string): string;
begin
  Result := val.Replace('\', '\\').Replace('"', '\"');
end;

function RegUnescapeString(const val: string): string;
var pc: PChar;
  cnt: integer;
  specSymbol: boolean;
begin
  if val = '' then begin
    Result := '';
    exit;
  end;

  pc := @val[1];
  cnt := Length(val);
  specSymbol := false;
  while cnt > 0 do begin
    if specSymbol then begin
      Result := Result + pc^;
      specSymbol := false;
    end else
    if pc^ = '\' then
      specSymbol := true
    else
      Result := Result + pc^;
    Dec(cnt);
  end;
end;

//Reads a potentially escaped text starting at a character pc^ until the
//first unescaped occurence of brch, or until the end (if the text is quote-wrapped).
//Advances the pointer to after the end of the parameter.
//Handles wrapping quotes, if present, and also de-encodes the value.
function RegReadEscapedValue(var pc: PChar; const brch: char; out wasEscaped: boolean): string;
var haveQuotes, specSymbol: boolean;
begin
  haveQuotes := (pc^='"');
  if haveQuotes then
    Inc(pc);

  specSymbol := false;
  while pc^ <> #00 do begin
    if haveQuotes and not specSymbol and (pc^='"') then begin
     //Wrapper quotes ended
      Inc(pc);
      break;
    end;

    if not haveQuotes and (pc^=brch) then
      break;

    if specSymbol then begin
      Result := Result + pc^;
      specSymbol := false;
    end else
    if (pc^ = '\') and haveQuotes then //specsymbols only work in quotes
      specSymbol := true
    else
      Result := Result + pc^;

    Inc(pc);
  end;
end;

constructor TRegFile.Create;
begin
  inherited;
end;

destructor TRegFile.Destroy;
begin
  inherited;
end;

resourcestring
  eRegUnsupportedFormat = 'Unsupported registry export file format: %s';
  eRegInvalidSectionHeader = 'Invalid section header: %s';
  eRegInvalidFreeLine = 'Invalid line outside of any section: %s';
  eRegInvalidSectionLine = 'Invalid line: %s';
  eRegInvalidEntryLineFormat = 'Invalid name=value line format: %s';
  eRegUnsupportedDataType = 'Unsupported data type: %s';

procedure TRegFile.LoadFromFile(const AFilename: string);
var sl: TStringList;
  i: integer;
  line: string;
  rk: TRegFileKey;
  re: TRegFileEntry;
  haveRk, haveFormat: boolean;
begin
  Self.Clear;

  sl := TStringList.Create;
  try
    haveRk := false;
    haveFormat := false;

    sl.LoadFromFile(AFilename);

    for i := 0 to sl.Count-1 do begin
      line := Trim(sl[i]);
      if (line='') or (line[1]=';') then
        continue;

      //New section
      if (line[1]='[') then begin
        if (Length(line)<2) or (line[Length(line)]<>']') then
          raise ERegFileFormatError.CreateFmt(eRegInvalidSectionHeader, [line]);
        //Store the collected section
        if haveRk then
          Self.Add(rk)
        else
          haveRk := true;
        //Anyway, reset the rk
        rk.Name := line.Substring(1, Length(line)-2);
        rk.Delete := false;
        SetLength(rk.Entries, 0);
        continue;
      end;

      //Non-empty string can only be format declaration
      if not haveRk then begin
        if haveFormat then
          raise ERegFileFormatError.CreateFmt(eRegInvalidFreeLine, [line]);
        if not SameText(line, sRegFileFormat500) then
          raise ERegFileFormatError.CreateFmt(eRegUnsupportedFormat, [line]);
        haveFormat := true;
        continue;
      end;

      //Otherwise must be name=value
      if not ParseNameValue(line, re) then
        raise ERegFileFormatError.CreateFmt(eRegInvalidSectionLine, [line]);
      SetLength(rk.Entries, Length(rk.Entries)+1);
      rk.Entries[Length(rk.Entries)-1] := re;
    end;

    //Store final section
    if haveRk then
      Self.Add(rk);

  finally
    FreeAndNil(sl);
  end;
end;

//Parses a name=value reg file entry
//Throws an exception on format error, returns false if this is obviously not a
//name=value line.
function TRegFile.ParseNameValue(const line: string; out re: TRegFileEntry): boolean;
var pc: PChar;
  wasEscaped: boolean;
  datatypeStr: string;
begin
  if line = '' then begin
    Result := false;
    exit;
  end;
  Result := true;

  pc := @line[1];
  re.Name := RegReadEscapedValue(pc, '=', wasEscaped);
  //A special case: spaces are allowed after quote-terminated name: "name" = "value"
  while pc^=' ' do Inc(pc);
  //Otherwise name must go right until =, and there must be a =
  if pc^ <> '=' then
    raise ERegFileFormatError.CreateFmt(eRegInvalidEntryLineFormat, [line]);
  re.Name := Trim(re.Name);
  if (re.Name = '@') and not wasEscaped then
    re.Name := '';
  Inc(pc);

  re.Data := RegReadEscapedValue(pc, #00, wasEscaped);
  while pc^=' ' do Inc(pc);
  //Value must go until the end of the line
  if pc^ <> #00 then
    raise ERegFileFormatError.CreateFmt(eRegInvalidEntryLineFormat, [line]);
  if wasEscaped then begin
   //Escaped values are always strings
    re.DataType := REG_SZ;
    exit;
  end;

  //Special case: Entry deletion marker (must be the only thing in the value)
  if Trim(re.Data) = '-' then begin
    re.DataType := REG_DELETE;
    re.Data := '';
    exit;
  end;

  //Non-escaped value must be in the form "datatype:value"
  datatypeStr := '';
  pc := ReadUpToNext(@re.Data[1], ':', datatypeStr);
  datatypeStr := Trim(datatypeStr).ToLower();
  //There must be a ':' after datatype, even with empty data
  if pc^ <> ':' then
     raise ERegFileFormatError.CreateFmt(eRegInvalidEntryLineFormat, [line]);
  Inc(pc);
  re.Data := pc; //trim the datatype
  if SameStr(datatypeStr, 'hex') then begin
    re.DataType := REG_BINARY;
  end else
  if SameStr(datatypeStr, 'dword') then begin
    re.DataType := REG_DWORD;
  end else
  //These are the only ones we support
  if SameStr(datatypeStr, 'hex(b)') then begin
    re.DataType := $0b; //REG_QWORD
  end else
  if SameStr(datatypeStr, 'hex(7)') then begin
    re.DataType := REG_MULTI_SZ;
  end else
  if SameStr(datatypeStr, 'hex(2)') then begin
    re.DataType := REG_EXPAND_SZ;
  end else
    raise ERegFileFormatError.CreateFmt(eRegUnsupportedDataType, [datatypeStr]);
end;

procedure TRegFile.SaveToFile(const AFilename: string);
var sl: TStringList;
begin
  sl := TStringList.Create;
  try
    Self.SaveToStrings(sl);
    sl.SaveToFile(AFilename);
  finally
    FreeAndNil(sl);
  end;
end;

procedure TRegFile.SaveToStrings(sl: TStrings);
var i: integer;
begin
  sl.Add(sRegFileFormat500);
  sl.Add('');

  for i := 0 to Self.Count-1 do begin
    Self.Items[i].ExportToStrings(sl);
    sl.Add('');
  end;
end;

function TRegFile.ExportToString: string;
var sl: TStringList;
begin
  sl := TStringList.Create;
  try
    Self.SaveToStrings(sl);
    Result := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;


end.
