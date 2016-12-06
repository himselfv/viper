unit ShellUtils;

interface

function RestartAsAdmin: integer;

procedure ShellOpen(const sCommand: string; const sParams: string = '');
procedure ExplorerAtFile(const AFilename: string);
procedure RegeditAtKey(const key: string);

implementation
uses SysUtils, Windows, ShellAPI, Registry, FilenameUtils;

function RunAsAdmin(const aFile: string; const aParameters: string = ''; Handle: HWND = 0): integer;
var sei: TShellExecuteInfo;
begin
  FillChar(sei, SizeOf(sei), 0);

  sei.cbSize := SizeOf(sei);
  sei.Wnd := Handle;
  sei.fMask := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpVerb := 'runas';
  sei.lpFile := PChar(aFile);
  sei.lpParameters := PChar(aParameters);
  sei.nShow := SW_SHOWNORMAL;

  if not ShellExecuteEx(@sei) then
    Result := GetLastError()
  else
    Result := 0;
end;

//Caller has to manually terminate this instance as we don't know if it prefers System.Exit()
//or Application.Terminate() here.
function RestartAsAdmin: integer;
begin
  Result := RunAsAdmin(AppFilename(), GetCommandLine());
end;


procedure ShellOpen(const sCommand: string; const sParams: string = '');
begin
  ShellExecute(0, 'open', PChar(sCommand), PChar(sParams), '', SW_SHOW);
end;

procedure ExplorerAtFile(const AFilename: string);
begin
  ShellExecute(0, '', PChar('explorer.exe'), PChar('/select,"'+AFilename+'"'),
    '', SW_SHOW);
end;

//Opens registry editor at the specific key
procedure RegeditAtKey(const key: string);
var reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.OpenKey('Software', true);
    reg.OpenKey('Microsoft', true);
    reg.OpenKey('Windows', true);
    reg.OpenKey('CurrentVersion', true);
    reg.OpenKey('Applets', true);
    reg.OpenKey('Regedit', true);
    reg.WriteString('Lastkey', key);
    WinExec(PAnsiChar(AnsiString('regedit.exe')), SW_SHOW);
  finally
    FreeAndNil(reg);
  end;
end;

end.
