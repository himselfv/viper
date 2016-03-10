unit WinApiHelper;

interface
uses Windows;

function ExpandEnvironmentStrings(const AString: string): string;

function SplitNullSeparatedList(AList: PChar): TArray<string>;

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

//Null-separated lists end with double-null:
//  EntryA #00 EntryB #00 EntryC #00 #00
function SplitNullSeparatedList(AList: PChar): TArray<string>;
var i: integer;
begin
  SetLength(Result, 0);
  if AList = nil then exit;

  i := 0;
  while AList^ <> #00 do begin
    SetLength(Result, i+1);
    Result[i] := string(AList);
    Inc(AList, Length(Result[i])+1); //skip string and terminating null
    Inc(i);
  end;
end;

end.
