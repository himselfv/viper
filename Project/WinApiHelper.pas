unit WinApiHelper;

interface
uses Windows;

function ExpandEnvironmentStrings(const AString: string): string;

implementation
uses SysUtils;

function ExpandEnvironmentStrings(const AString: string): string;
var sz: integer;
begin
  if AString = '' then begin
    Result := '';
    exit;
  end;
  sz := Length(AString)+1; //probably at least this
  repeat
    SetLength(Result, sz-1);
    sz := Windows.ExpandEnvironmentStrings(PChar(AString), PChar(Result), Length(Result)+1);
    if sz = 0 then RaiseLastOsError(); //normally at least 1 (null term)
  until sz <= Length(Result)+1;
  if sz < Length(Result)+1 then
    SetLength(Result, sz-1);
end;

end.
