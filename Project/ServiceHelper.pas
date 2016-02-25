unit ServiceHelper;

interface
uses WinSvc;

//Result has to be freed
function QueryServiceConfig(hSC: SC_HANDLE; hSvc: SC_HANDLE): LPQUERY_SERVICE_CONFIG; overload;
function QueryServiceConfig(hSC: SC_HANDLE; const AServiceName: string): LPQUERY_SERVICE_CONFIG; overload;

//Result has to be freed
function QueryServiceConfig2(hSC: SC_HANDLE; dwInfoLevel: cardinal; hSvc: SC_HANDLE): PByte; overload;
function QueryServiceConfig2(hSC: SC_HANDLE; dwInfoLevel: cardinal; const AServiceName: string): PByte; overload;

function QueryServiceDescription(hSC: SC_HANDLE; const AServiceName: string): string;


implementation
uses SysUtils, Windows;

function QueryServiceConfig(hSC: SC_HANDLE; hSvc: SC_HANDLE): LPQUERY_SERVICE_CONFIG;
var lastSize, bufSize: cardinal;
  err: integer;
begin
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
end;

function QueryServiceConfig(hSC: SC_HANDLE; const AServiceName: string): LPQUERY_SERVICE_CONFIG;
var hSvc: SC_HANDLE;
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
    Result := QueryServiceConfig(hSC, hSvc);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

function QueryServiceConfig2(hSC: SC_HANDLE; dwInfoLevel: cardinal; hSvc: SC_HANDLE): PByte;
var lastSize, bufSize: cardinal;
  err: integer;
begin
  Result := nil;
  bufSize := 0;
  lastSize := 0;
  while not WinSvc.QueryServiceConfig2(hSvc, dwInfoLevel, Result, lastSize, @bufSize) do begin
    err := GetLastError();
    if err <> ERROR_INSUFFICIENT_BUFFER then
      RaiseLastOsError(err);
    if bufSize <= lastSize then //avoid infinite cycle
      RaiseLastOsError(err);
    ReallocMem(Result, bufSize);
    lastSize := bufSize;
  end;
end;

function QueryServiceConfig2(hSC: SC_HANDLE; dwInfoLevel: cardinal; const AServiceName: string): PByte;
var hSvc: SC_HANDLE;
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
    Result := QueryServiceConfig2(hSC, dwInfoLevel, hSvc);
  finally
    CloseServiceHandle(hSvc);
  end;
end;


function QueryServiceDescription(hSC: SC_HANDLE; const AServiceName: string): string;
var buf: LPSERVICE_DESCRIPTION;
begin
  buf := LPSERVICE_DESCRIPTION(QueryServiceConfig2(hSC, SERVICE_CONFIG_DESCRIPTION, AServiceName));
  if buf = nil then begin
    Result := '';
    exit;
  end;
  Result := buf.lpDescription;
  FreeMem(buf);
end;


end.
