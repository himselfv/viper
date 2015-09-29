unit ServiceHelper;

interface
uses WinSvc;

//Result has to be freed
function QueryServiceConfig(hSC: SC_HANDLE; const AServiceName: string): LPQUERY_SERVICE_CONFIG;


implementation
uses SysUtils, Windows;

function QueryServiceConfig(hSC: SC_HANDLE; const AServiceName: string): LPQUERY_SERVICE_CONFIG;
var lastSize, bufSize: cardinal;
  hSvc: SC_HANDLE;
  err: integer;
begin
  hSvc := OpenService(hSC, PChar(AServiceName), SC_MANAGER_ALL_ACCESS);
  if hSvc = 0 then begin
    err := GetLastError();
    if err = ERROR_ACCESS_DENIED then begin
      Result := nil;
      exit;
    end;
    RaiseLastOsError(err);
  end;
  try
    Result := nil;
    bufSize := 0;
    lastSize := 0;
    while not WinSvc.QueryServiceConfig(hSvc, Result, lastSize, bufSize) do begin
      err := GetLastError();
      if err <> ERROR_INSUFFICIENT_BUFFER then
        RaiseLastOsError(err);
      if bufSize <= lastSize then //avoid infinite cycle
        RaiseLastOsError(err);
      ReallocMem(Result, bufSize);
      lastSize := bufSize;
    end;
  finally
    CloseServiceHandle(hSvc);
  end;
end;

end.
