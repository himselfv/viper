unit GuidDict;
{
Resolves GUIDs to strings
}

interface
uses SysUtils, Generics.Collections;

type
  TGuidDictionary = class(TDictionary<TGuid, string>)
  public
    procedure LoadFromFile(const AFilename: string);
  end;

  EInvalidFormat = class(Exception);

implementation
uses Classes;

procedure TGuidDictionary.LoadFromFile(const AFilename: string);
var lines: TStringList;
  ln: string;
  i, i_pos: integer;
  guid: TGuid;
begin
  lines := TStringList.Create;
  try
    lines.LoadFromFile(AFilename);
    for i := 0 to lines.Count-1 do begin
      ln := lines[i];

      //Remove comments
      i_pos := pos('#', ln);
      if i_pos > 0 then
        delete(ln, i_pos, MaxInt);
      ln := Trim(ln);
      if ln = '' then continue;

      //Split
      i_pos := pos('=', ln);
      if i_pos <= 0 then
        raise EInvalidFormat.Create('');

      guid := StringToGuid(Trim(copy(ln, i_pos+1, MaxInt)));
      ln := Trim(copy(ln, 1, i_pos-1));

      Self.AddOrSetValue(guid, ln);
    end;
  finally
    FreeAndNil(lines);
  end;
end;

end.
