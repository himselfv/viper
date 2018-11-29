unit WnfUtils;
{
Windows Notification Facility utils.
See https://blog.quarkslab.com/playing-with-the-windows-notification-facility-wnf.html
}

interface
uses SysUtils, Generics.Collections;

type
  TWnfStateName = int64;

const
  //WNF Scope markers
  WNF_SCOPE_MAP_CODE          = $901;
  WNF_SCOPE_INSTANCE_CODE     = $902;
  WNF_NAME_INSTANCE_CODE      = $903;
  WNF_STATE_DATA_CODE         = $904;
  WNF_SUBSCRIPTION_CODE       = $905;
  WNF_PROCESS_CONTEXT_CODE    = $906;

const
  WNF_XOR_KEY = $41C64E6DA3BC0074;

  //WNF State Name format:
  WNF_SN_VERSION_MASK = $0F;          //ULONG64 Version : 4;
  WNF_SN_LIFETIME_MASK = $03 shl 4;   //ULONG64 Lifetime : 2;
  WNF_SN_DATASCOPE_MASK = $0F shl 6;  //ULONG64 DataScope : 4;
  WNF_SN_PERMANENT_MASK = $01 shl 10; //ULONG64 IsPermanent : 1;
  WNF_SN_UNIQUE_SHIFT = 11;           //ULONG64 Unique : 53;      //the rest

  WNF_DATA_SCOPE_SYSTEM = 0;
  WNF_DATA_SCOPE_SESSION = 1;
  WNF_DATA_SCOPE_USER = 2;
  WNF_DATA_SCOPE_PROCESS = 3;
  WNF_DATA_SCOPE_MACHINE = 4;

function WnfSnGetVersion(const WnfSn: TWnfStateName): byte; inline
function WnfSnGetLifetime(const WnfSn: TWnfStateName): byte; inline;
function WnfSnGetDataScope(const WnfSn: TWnfStateName): byte; inline;
function WnfSnGetPermanent(const WnfSn: TWnfStateName): boolean; inline;
function WnfSnGetUniquePart(const WnfSn: TWnfStateName): int64; inline;

function XorShift(const AValue: int64; AMask: int64): int64;

type
  TWnfSnInfo = record
    Name: string;
    Desc: string;
  end;
  TWnfSnDict = class(TDictionary<TWnfStateName, TWnfSnInfo>)
  public
    procedure LoadFromFile(const AFilename: string);
  end;

  EWnfSnDictParseError = class(Exception);

implementation
uses Classes;

function XorShift(const AValue: int64; AMask: int64): int64;
begin
  Result := AValue;
  while AMask and 1 = 0 do begin
    AMask := AMask shr 1;
    Result := Result shr 1;
  end;
  Result := Result and AMask;
end;

function WnfSnGetVersion(const WnfSn: TWnfStateName): byte;
begin
  Result := XorShift(WnfSn, WNF_SN_VERSION_MASK);
end;

function WnfSnGetLifetime(const WnfSn: TWnfStateName): byte;
begin
  Result := XorShift(WnfSn, WNF_SN_LIFETIME_MASK);
end;

function WnfSnGetDataScope(const WnfSn: TWnfStateName): byte;
begin
  Result := XorShift(WnfSn, WNF_SN_DATASCOPE_MASK);
end;

function WnfSnGetPermanent(const WnfSn: TWnfStateName): boolean;
begin
  Result := ((WnfSn xor WNF_XOR_KEY) and WNF_SN_PERMANENT_MASK) <> 0;
end;

function WnfSnGetUniquePart(const WnfSn: TWnfStateName): int64;
begin
  Result := (WnfSn xor WNF_XOR_KEY) shr WNF_SN_UNIQUE_SHIFT;
end;

resourcestring
  eInvalidWfnSn = 'Invalid WNF State Name: %s';

procedure TWnfSnDict.LoadFromFile(const AFilename: string);
var lines: TStringList;
  line: string;
  i, i_pos: integer;
  sn_str: string;
  sn: TWnfStateName;
  sn_info: TWnfSnInfo;
begin
  lines := TStringList.Create;
  try
    lines.LoadFromFile(AFilename);
    for i := 0 to lines.Count-1 do begin
      line := Trim(lines[i]);
      if (line = '') or (line[1] = '#') then
        continue;

      i_pos := pos(#09, line);
      if i_pos <= 0 then
        continue; //Bad line!

      sn_str := copy(line, 1, i_pos-1);
      delete(line, 1, i_pos);
      if not TryStrToInt64('$'+sn_str, sn) then
        raise EWnfSnDictParseError.CreateFmt(eInvalidWfnSn, [sn_str]);

      //WNF SNs are stored lowest byte first, but StrToInt64 is HBF, reverse:
      sn := int64(PByte(@sn)[7])
        + int64(PByte(@sn)[6]) shl 8
        + int64(PByte(@sn)[5]) shl 16
        + int64(PByte(@sn)[4]) shl 24
        + int64(PByte(@sn)[3]) shl 32
        + int64(PByte(@sn)[2]) shl 40
        + int64(PByte(@sn)[1]) shl 48
        + int64(PByte(@sn)[0]) shl 56;

      i_pos := pos(#09, line);
      if i_pos <= 0 then begin
        sn_info.Name := line;
        sn_info.Desc := '';
      end else begin
        sn_info.Name := copy(line, 1, i_pos-1);
        delete(line, 1, i_pos);
        sn_info.Desc := line; //remainder
      end;

      Self.AddOrSetValue(sn, sn_info);
    end;
  finally
    FreeAndNil(lines);
  end;
end;

end.
