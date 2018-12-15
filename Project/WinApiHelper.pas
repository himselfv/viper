unit WinApiHelper;

interface
uses Windows;

function ExpandEnvironmentStrings(const AString: string): string;

function SplitNullSeparatedList(AList: PChar): TArray<string>;
function JoinNullSeparatedList(const AList: TArray<string>): string;
procedure CopyNullSeparatedList(const AList: PChar; out ACopy: string); overload;


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

function JoinNullSeparatedList(const AList: TArray<string>): string;
var i: integer;
begin
  Result := '';
  for i := 0 to Length(AList)-1 do
    Result := Result + AList[i] + #00;
end;

//Creates a copy of a given list in a string variable. Simply assigning the variable
//would not work since only the text until the first #00 would be assigned.
procedure CopyNullSeparatedList(const AList: PChar; out ACopy: string);
var ptr: PChar;
begin
  if AList = nil then begin
    SetLength(ACopy, 0);
    exit;
  end;
  ptr := AList;
  //Find terminating #00#00
  while (ptr[0] <> #00) or (ptr[1] <> #00) do
    Inc(ptr);
  Inc(ptr); //we need the first #00 explicitly
  SetLength(ACopy, NativeUInt(ptr)-NativeUint(AList));
  Move(AList^, ACopy[1], SizeOf(char)*Length(ACopy));
end;

end.
