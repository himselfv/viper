unit ShellUtils;

interface

procedure ShellOpen(const sCommand: string; const sParams: string = '');
procedure ExplorerAtFile(const AFilename: string);
procedure RegeditAtKey(const key: string);

implementation
uses SysUtils, Windows, ShellAPI, Registry;

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
