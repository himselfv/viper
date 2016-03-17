unit SetupApiHelper;
//Delphi lacks SetupApi conversion, so we port what we need explicitly.

interface
uses Windows;

const
  setupapi = 'SetupApi.dll';

function SetupDiGetClassDescriptionW(ClassGuid: PGUID; ClassDescription: PChar;
  ClassDescriptionSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall; external setupapi;

function TrySetupDiGetClassDescriptionStr(const ClassGuid: TGUID; out Description: string): boolean;
function SetupDiGetClassDescriptionStr(const ClassGuid: TGUID): string;

implementation
uses SysUtils;

function TrySetupDiGetClassDescriptionStr(const ClassGuid: TGUID; out Description: string): boolean;
var sz: DWORD;
begin
  sz := 255;
  SetLength(Description, sz);

  if not SetupDiGetClassDescriptionW(@ClassGuid, PChar(Description), sz, @sz) then begin
    if GetLastError <> ERROR_MORE_DATA then begin
      Result := false;
      exit;
    end;
    SetLength(Description, sz);
    if not SetupDiGetClassDescriptionW(@ClassGuid, PChar(Description), sz, @sz) then begin
      Result := false;
      exit;
    end;
  end;
  SetLength(Description, sz);
  Result := true;
end;

function SetupDiGetClassDescriptionStr(const ClassGuid: TGUID): string;
begin
  if not TrySetupDiGetClassDescriptionStr(ClassGuid, Result) then
    RaiseLastOsError();
end;

end.
