unit RegFile;
{
Registry export format parser/
Reg file format specifications:
  http://support.microsoft.com/kb/310516
  https://msdn.microsoft.com/en-us/library/gg469889.aspx

We support a LIMITED subset of functionality that our own export uses.
Limitations:
- Only 5.00 format is supported
- No key name escaping (there seems to be none)
- We don't preserve comments or formatting on load-save
- Not all data types are supported on import

We support:
- Empty lines
- Comments
- Default values (@)
- Entry name and value escaping
- Key and value removal
- Multiline values
- All data types on export
}

interface
uses SysUtils, Classes, Windows, Generics.Collections, Registry;

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
    //Set the Data in the respective format, ignoring the DataType
    function GetDwordData: dword; inline;
    function GetStringData: string; inline;
    function GetOtherData: TBytes; inline;
    procedure SetDwordData(const val: dword); inline;
    procedure SetStringData(const val: string); inline;
    procedure SetOtherData(const pb: PByte; cb: cardinal); inline;
    property DataAsDword: dword read GetDwordData write SetDwordData;
    property DataAsString: string read GetStringData write SetStringData;

    //Sets the DataType and the Data // verifies the DataType and retrieves the Data
    procedure AssertType(const ADataType: integer); inline;
    function GetDwordValue: dword; inline;
    function GetStringValue: string; inline;
    function GetExpandStrValue: string; inline;
    function GetMultiStrValue: TArray<string>; inline;
    procedure SetDwordValue(const AValue: dword); inline;
    procedure SetStringValue(const AValue: string); inline;
    procedure SetExpandStrValue(const AValue: string); inline;
    procedure SetMultiStrValue(const AValue: TArray<string>); inline;
    property DwordValue: dword read GetDwordValue write SetDwordValue;
    property StringValue: string read GetStringValue write SetStringValue;
    property ExpandStrValue: string read GetExpandStrValue write SetExpandStrValue;
    property MultiStrValue: TArray<string> read GetMultiStrValue write SetMultiStrValue;

    //Convert the Data to the given format
    //Read any data as TBytes
    function GetAsBytes: TBytes;

    //Export
    function ExportToString: string;
  end;

  TRegFileKey = record
    Name: string;
    Delete: boolean;
    Entries: TArray<TRegFileEntry>;
    constructor Create(const AFrom: TRegFileKey);
    procedure Clear;
    procedure AddEntry(const Entry: TRegFileEntry);
    procedure DeleteEntry(const AIndex: integer);
    function EntryCount: integer; inline;
    //Shortcuts
    procedure AddStringValue(const Name: string; DataType: integer; const Data: string);
    procedure AddDwordValue(const Name: string; DataType: integer; const Data: dword);
    procedure AddOtherValue(const Name: string; DataType: integer; const Data: PByte; const Size: integer);
    //Export
    procedure ExportToStrings(sl: TStrings);
    function ExportToString: string;
  end;
  PRegFileKey = ^TRegFileKey;

  TRegFileKeys = class(TList<PRegFileKey>)
  protected
    procedure Notify(const Item: PRegFileKey; Action: TCollectionNotification); override;
  public
    constructor Create; overload;
    constructor Create(const AFrom: TRegFileKeys); overload;
    procedure Add(const AKey: TRegFileKey); overload;
    function GetKey(const AName: string): PRegFileKey;
  end;

  TRegFile = class(TRegFileKeys)
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

resourcestring
  eInvalidEntryDataType = 'Invalid REG entry data type';

//Convert binary data to hex string and back
function RegBinToHex(pb: PByte; cb: cardinal): AnsiString;
procedure RegHexToBin(const hex: AnsiString; pb: PByte; cb: cardinal); overload;
function RegHexToBin(const hex: AnsiString; cb: PCardinal): PByte; overload;
function RegHexToBin(const hex: AnsiString): TBytes; overload;

//Escape - do not apply to values manually
function RegEscapeString(const val: string): string;
function RegUnescapeString(const val: string): string;


// Root keys
// Need to be uppercase

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
function ExtractRootKey(var KeyPath: string): HKEY;
procedure GetAbsoluteKeyPath(const KeyPath: string; out AbsolutePath: string; out RootKey: HKEY);


implementation
uses UniStrUtils, WinApiHelper;
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

//Extracts one of HKEY_* constants from the beginning of the string and returns the constant
//The string is modified to start with first \ after the HKEY_*
//Returns 0 if the string does not start with any of these constants
function ExtractRootKey(var KeyPath: string): HKEY;
var KeyPathUpcase: string;
  key: integer;
begin
  KeyPathUpcase := KeyPath.ToUpper;

  for key := Low(RootKeyNames) to High(RootKeyNames) do
    if KeyPathUpcase.StartsWith(RootKeyNames[key]) then begin
      Delete(KeyPath, 1, Length(RootKeynames[key]));
      Result := key;
      exit;
    end;

  for key := Low(RootShortKeyNames) to High(RootShortKeyNames) do
    if KeyPathUpcase.StartsWith(RootShortKeyNames[key]) then begin
      Delete(KeyPath, 1, Length(RootShortKeyNames[key]));
      Result := key;
      exit;
    end;

  Result := 0;
end;

//Extracts the HKEY_* from the beginning of the string, if present, or otherwise
//transforms the path into a pair of root HKEY + absolute path starting with \
procedure GetAbsoluteKeyPath(const KeyPath: string; out AbsolutePath: string; out RootKey: HKEY);
begin
  AbsolutePath := KeyPath;
  RootKey := ExtractRootKey(AbsolutePath);
  if RootKey <> 0 then exit; //successs

  //By default we have no clue and assume the HKEY_ part is missing
  RootKey := HKEY_CURRENT_USER;
  if (AbsolutePath = '') or (AbsolutePath[1] <> '\') then
    AbsolutePath := '\' + AbsolutePath; //we promised to return something starting with \
end;


function TRegFileEntry.GetAsBytes: TBytes;
begin
  case Self.DataType of
    REG_DELETE: SetLength(Result, 0);
    REG_SZ: begin
      SetLength(Result, SizeOf(WideChar)*(Length(Self.Data)+1));
      if Length(Self.Data) > 0 then
        Move(Self.Data[1], Result[0], SizeOf(WideChar)*Length(Self.Data));
      Result[Length(Result)-1] := 00;
      Result[Length(Result)-0] := 00;
    end;
    REG_DWORD: begin
      SetLength(Result, 4);
      PInteger(@Result[0])^ := StrToInt('$'+Self.Data); //it's hex!
    end
  else
   //All the other types are stored as hex!
    Result := RegHexToBin(AnsiString(Self.Data));
  end;
end;


{ Gets or sets the Data in the respective format. Does not verify the DataType.
 Throws if the interpretation is impossible. }

function TRegFileEntry.GetDwordData: dword;
begin
  Result := StrToInt('$'+Self.Data);
end;

function TRegFileEntry.GetStringData: string;
begin
  Result := Self.Data;
end;

function TRegFileEntry.GetOtherData: TBytes;
begin
  Result := RegHexToBin(AnsiString(Self.Data));
end;

//Sets TRegFileEntry value in dword format
procedure TRegFileEntry.SetDwordData(const val: dword);
begin
  Self.Data := IntToHex(val, 8);
end;

procedure TRegFileEntry.SetStringData(const val: string);
begin
  //Do not escape the string here, we'll do it on writing
  Self.Data := val;
end;

procedure TRegFileEntry.SetOtherData(const pb: PByte; cb: cardinal);
begin
  //All the other values are simply presented as hex
  Self.Data := string(RegBinToHex(pb, cb));
end;

{ Sets the DataType and the Data // verifies the DataType and retrieves the Data }

procedure TRegFileEntry.AssertType(const ADataType: integer);
begin
  if Self.DataType <> ADataType then
    raise EConvertError.Create(eInvalidEntryDataType);
end;

function TRegFileEntry.GetDwordValue: dword;
begin
  AssertType(REG_DWORD);
  Result := Self.DataAsDword;
end;

//Each of GetStringValue, GetExpandStrValue read both, but Set* sets the appropriate type.
function TRegFileEntry.GetStringValue: string;
var LData: TBytes;
begin
  case Self.DataType of
    REG_SZ: Result := Self.DataAsString;
    REG_EXPAND_SZ: begin
      LData := Self.GetOtherData;
      //Can't deal with half chars
      if Length(LData) mod SizeOf(char) <> 0 then
        SetLength(LData, Length(LData) - Length(LData) mod SizeOf(char));
      //Empty string
      if Length(LData) <= SizeOf(Char) then
        Result := ''
      else begin
        //Add forced term null
        SetLength(LData, Length(LData)+SizeOf(PChar)); //no half chars so ok
        PChar(@LData[Length(LData)-SizeOf(PChar)])^ := #00;
        //convert
        Result := PChar(@LData[0]);
      end;
    end

  else
    raise EConvertError.Create(eInvalidEntryDataType);
  end;
end;

function TRegFileEntry.GetExpandStrValue: string;
begin
  Result := GetStringValue;
end;

function TRegFileEntry.GetMultiStrValue: TArray<string>;
var LMerged: TBytes;
begin
  AssertType(REG_MULTI_SZ);
  SetLength(Result, 0);
  LMerged := Self.GetOtherData;
  if Length(LMerged) <= SizeOf(Char) then exit; //empty

  Result := SplitNullSeparatedList(PChar(@LMerged[0]));
end;

procedure TRegFileEntry.SetDwordValue(const AValue: dword);
begin
  Self.DataType := REG_DWORD;
  Self.DataAsDword := AValue;
end;

procedure TRegFileEntry.SetStringValue(const AValue: string);
begin
  Self.DataType := REG_SZ;
  Self.DataAsString := AValue;
end;

procedure TRegFileEntry.SetExpandStrValue(const AValue: string);
begin
  Self.DataType := REG_EXPAND_SZ;
  Self.DataAsString := AValue;
end;

procedure TRegFileEntry.SetMultiStrValue(const AValue: TArray<string>);
var LMerged: string;
  i: integer;
begin
  Self.DataType := REG_MULTI_SZ;
  SetLength(LMerged, 0);
  for i := 0 to Length(AValue)-1 do
    LMerged := LMerged + AValue[i] + #00;
  //Final additional #00 is provided by the string
  //But we have to have at least one base #00 + also avoid nil
  if Length(LMerged) <= 0 then
    LMerged := ''#00;
  Self.SetOtherData(@LMerged[1], (Length(LMerged)+1)*SizeOf(Char));
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


//Creates a copy of the given TRegFileKey. Dynamic arrays in Delphi do not COW
//so simply assigning is not enough if you want to edit your copy
constructor TRegFileKey.Create(const AFrom: TRegFileKey);
begin
  Self := AFrom;
  Self.Entries := Copy(AFrom.Entries);
end;

procedure TRegFileKey.Clear;
begin
  SetLength(Self.Entries, 0);
end;

procedure TRegFileKey.AddEntry(const Entry: TRegFileEntry);
begin
  SetLength(Self.Entries, Length(Self.Entries)+1);
  Self.Entries[Length(Self.Entries)-1] := Entry;
end;

procedure TRegFileKey.DeleteEntry(const AIndex: integer);
var i: integer;
begin
  for i := AIndex+1 to Length(Self.Entries)-1 do
    Self.Entries[i-1] := Self.Entries[i];
  SetLength(Self.Entries, Length(Self.Entries)-1);
end;

function TRegFileKey.EntryCount: integer;
begin
  Result := Length(Self.Entries);
end;

procedure TRegFileKey.AddStringValue(const Name: string; DataType: integer; const Data: string);
var Entry: TRegFileEntry;
begin
  Entry.Name := Name;
  Entry.DataType := DataType;
  Entry.SetStringValue(Data);
  Self.AddEntry(Entry);
end;

procedure TRegFileKey.AddDwordValue(const Name: string; DataType: integer; const Data: dword);
var Entry: TRegFileEntry;
begin
  Entry.Name := Name;
  Entry.DataType := DataType;
  Entry.SetDwordValue(Data);
  Self.AddEntry(Entry);
end;

procedure TRegFileKey.AddOtherValue(const Name: string; DataType: integer; const Data: PByte; const Size: integer);
var Entry: TRegFileEntry;
begin
  Entry.Name := Name;
  Entry.DataType := DataType;
  Entry.SetOtherData(Data, Size);
  Self.AddEntry(Entry);
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
var tmp: AnsiString;
  tcb: cardinal;
begin
  tmp := AnsiReplaceStr(hex, ',', '');
  tmp := AnsiReplaceStr(tmp, ' ', '');
  try
    tcb := Length(tmp) div 2;
    GetMem(Result, tcb);
    RegHexToBin(tmp, Result, tcb);
    if cb <> nil then
      cb^ := tcb;
  except
    FreeMem(Result);
    raise;
  end;
end;

function RegHexToBin(const hex: AnsiString): TBytes;
var tmp: AnsiString;
begin
  tmp := AnsiReplaceStr(hex, ',', '');
  tmp := AnsiReplaceStr(tmp, ' ', '');
  SetLength(Result, Length(tmp) div 2);
  if Length(Result) > 0 then
    RegHexToBin(tmp, @Result[0], Length(Result));
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
function RegReadEscapedValue(var pc: PChar; const brch: char; out haveQuotes: boolean): string;
var specSymbol: boolean;
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


constructor TRegFileKeys.Create;
begin
  inherited;
end;

//Creates a copy of the given TRegFileKeys
constructor TRegFileKeys.Create(const AFrom: TRegFileKeys);
var LKey: PRegFileKey;
begin
  inherited Create;
  for LKey in AFrom do
    Self.Add(TRegFileKey.Create(LKey^));
end;

procedure TRegFileKeys.Notify(const Item: PRegFileKey; Action: TCollectionNotification);
begin
  case Action of
    cnRemoved: Dispose(Item);
  end;
end;

//Adds a copy of the given key structure
procedure TRegFileKeys.Add(const AKey: TRegFileKey);
var LPKey: PRegFileKey;
begin
  New(LPKey);
  LPKey^ := AKey;
  Self.Add(LPKey);
end;

//Finds and returns a key for the given name (path)
function TRegFileKeys.GetKey(const AName: string): PRegFileKey;
var i: integer;
begin
  Result := nil;
  for i := 0 to Self.Count-1 do
    if Self[i]^.Name = AName then begin
      Result := Self[i];
      exit;
    end;
  New(Result);
  Result^.Name := AName;
  Self.Add(Result);
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
  line, line2: string;
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

    i := 0;
    while i <= sl.Count-1 do begin
      line := Trim(sl[i]);
      if (line='') or (line[1]=';') then begin
        Inc(i);
        continue;
      end;

      //New section
      if (line[1]='[') then begin
        if (Length(line)<3) or (line[Length(line)]<>']') then // [] and non-empty
          raise ERegFileFormatError.CreateFmt(eRegInvalidSectionHeader, [line]);
        //Store the collected section
        if haveRk then
          Self.Add(rk)
        else
          haveRk := true;
        //Anyway, reset the rk
        rk.Name := copy(line, 2, Length(line)-2);
        rk.Delete := (rk.Name[1]='-'); //available per check above
        if rk.Delete then begin
          rk.Name := copy(rk.Name, 2, MaxInt);
          if rk.Name = '' then //"-" sign without key name
            raise ERegFileFormatError.CreateFmt(eRegInvalidSectionHeader, [line]);
        end;
        SetLength(rk.Entries, 0);
        Inc(i);
        continue;
      end;

      //Non-empty string can only be format declaration
      if not haveRk then begin
        if haveFormat then
          raise ERegFileFormatError.CreateFmt(eRegInvalidFreeLine, [line]);
        if not SameText(line, sRegFileFormat500) then
          raise ERegFileFormatError.CreateFmt(eRegUnsupportedFormat, [line]);
        haveFormat := true;
        Inc(i);
        continue;
      end;

      //Otherwise must be name=value

      //Name=value strings might have continuations:
      //   Line1\
      //   Line2\
      //   Line3
      line2 := line;
      while (Length(line2) > 0) and (line2[Length(line2)] = '\') and (i <= sl.Count-1) do begin
        Inc(i);
        line2 := Trim(sl[i]);
        System.Delete(line, Length(line), 1); //remove the previous \
        line := line + line2;       //add new chunk
      end;

      //Now parse it
      if not ParseNameValue(line, re) then
        raise ERegFileFormatError.CreateFmt(eRegInvalidSectionLine, [line]);
      SetLength(rk.Entries, Length(rk.Entries)+1);
      rk.Entries[Length(rk.Entries)-1] := re;
      Inc(i);
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
  haveQuotes: boolean;
  datatypeStr: string;
begin
  if line = '' then begin
    Result := false;
    exit;
  end;
  Result := true;

  pc := @line[1];
  re.Name := RegReadEscapedValue(pc, '=', haveQuotes);
  //A special case: spaces are allowed after quote-terminated name: "name" = "value"
  while pc^=' ' do Inc(pc);
  //Otherwise name must go right until =, and there must be a =
  if pc^ <> '=' then
    raise ERegFileFormatError.CreateFmt(eRegInvalidEntryLineFormat, [line]);
  re.Name := Trim(re.Name);
  if (re.Name = '@') and not haveQuotes then
    re.Name := '';
  Inc(pc);

  re.Data := RegReadEscapedValue(pc, #00, haveQuotes);
  while pc^=' ' do Inc(pc);
  //Value must go until the end of the line
  if pc^ <> #00 then
    raise ERegFileFormatError.CreateFmt(eRegInvalidEntryLineFormat, [line]);
  if haveQuotes then begin
   //Values in quotes are always strings
    re.DataType := REG_SZ;
    exit;
  end;

  //Special case: Entry deletion marker (must be the only thing in the value)
  if Trim(re.Data) = '-' then begin
    re.DataType := REG_DELETE;
    re.Data := '';
    exit;
  end;

  //Value without quotes must be in the form "datatype:value"
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
