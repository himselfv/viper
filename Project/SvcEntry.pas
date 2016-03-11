unit SvcEntry;

interface
uses SysUtils, Classes, Windows, WinSvc, ServiceHelper, Generics.Collections, ImgList;

type
 //Running service description
  TServiceEntry = class
    ServiceName: string;
    DisplayName: string;
    Description: string;
    Status: SERVICE_STATUS;
    Config: LPQUERY_SERVICE_CONFIG;
    ServiceDll: string;
    constructor CreateFromEnum(hSC: SC_HANDLE; const S: PEnumServiceStatus);
    destructor Destroy; override;
    procedure Refresh(); overload;
    procedure RefreshFromManager(hSC: SC_HANDLE);
    procedure RefreshFromHandle(hSvc: SC_HANDLE);
    function GetEffectiveDisplayName: string; virtual;
    function GetExecutableFilename: string;
    procedure GetIcon(out AImageList: TCustomImageList; out AIndex: integer); virtual;
    function CanStart: boolean; inline;
    function CanForceStart: boolean; inline;
    function CanStop: boolean; inline;
    function CanPause: boolean; inline;
    function CanResume: boolean; inline;
  end;
  PServiceEntry = ^TServiceEntry;

  TServiceEntries = array of TServiceEntry;
  PServiceEntries = ^TServiceEntries;

  TServiceEntryList = class(TObjectList<TServiceEntry>)
  public
    function Find(const AServiceName: string): TServiceEntry;
  end;


//Subscribe to be notified of changes
var
  OnServiceInvalidated: TList<TNotifyEvent>;


//Shortcuts
procedure RefreshService(Service: TServiceEntry);
procedure InvalidateService(Service: TServiceEntry);


implementation
uses WinapiHelper;

constructor TServiceEntry.CreateFromEnum(hSC: SC_HANDLE; const S: PEnumServiceStatus);
var hSvc: SC_HANDLE;
begin
  inherited Create;
  Self.ServiceName := S^.lpServiceName;
  Self.DisplayName := S^.lpDisplayName;
  Self.Status := S^.ServiceStatus;
  hSvc := OpenService(hSC, S^.lpServiceName, SERVICE_QUERY_CONFIG);
  try
    Self.Config := QueryServiceConfig(hSvc);
    Self.Description := QueryServiceDescription(hSvc);
  finally
    CloseServiceHandle(hSvc);
  end;
  if Self.Config <> nil then begin
    if (pos('svchost.exe', Self.Config.lpBinaryPathName)>0)
    or (pos('lsass.exe', Self.Config.lpBinaryPathName)>0) then
   //^ this is not a surefire way to test it's running svchost.exe, but we don't need one
   // It would be too complicated to check that it really references svchost, and the one
   // from the system dir and not an impostor.
   // We just optimize away unneccessary registry checks.
      Self.ServiceDll := ExpandEnvironmentStrings(QueryServiceServiceDll(Self.ServiceName));
  end;
end;

destructor TServiceEntry.Destroy;
begin
  if Self.Config <> nil then begin
    FreeMem(Self.Config);
    Self.Config := nil;
  end;
  inherited;
end;

procedure TServiceEntry.Refresh();
var hSC: SC_HANDLE;
begin
  hSC := OpenSCManager(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
  try
    RefreshFromManager(hSC);
  finally
    CloseServiceHandle(hSC);
  end;
end;

procedure TServiceEntry.RefreshFromManager(hSC: SC_HANDLE);
var hSvc: SC_HANDLE;
begin
  hSvc := OpenService(hSC, Self.ServiceName, SERVICE_READ_ACCESS);
  try
    if hSvc <> 0 then
      RefreshFromHandle(hSvc);
  finally
    CloseServiceHandle(hSvc);
  end;
end;

procedure TServiceEntry.RefreshFromHandle(hSvc: SC_HANDLE);
begin
  Self.Status := QueryServiceStatus(hSvc);
  Self.Config := QueryServiceConfig(hSvc);
end;

procedure TServiceEntry.GetIcon(out AImageList: TCustomImageList; out AIndex: integer);
begin
  AImageList := nil;
  AIndex := -1;
end;

function TServiceEntry.GetEffectiveDisplayName: string;
begin
  Result := Self.DisplayName;
end;

function TServiceEntry.GetExecutableFilename: string;
begin
  if Self.ServiceDll <> '' then
    Result := Self.ServiceDll
  else
  if Config <> nil then
    Result := Config.lpBinaryPathName
  else
    Result := '';
end;

function TServiceEntry.CanStart: boolean;
begin
  Result := (Status.dwCurrentState = SERVICE_STOPPED)
    and ((Config = nil) or (Config.dwStartType <> SERVICE_DISABLED));
end;

function TServiceEntry.CanForceStart: boolean;
begin
  Result := Status.dwCurrentState = SERVICE_STOPPED;
end;

function TServiceEntry.CanStop: boolean;
begin
  Result := (Status.dwCurrentState <> SERVICE_STOPPED)
    and (Status.dwCurrentState <> SERVICE_STOP_PENDING);
end;

function TServiceEntry.CanPause: boolean;
begin
  Result := Status.dwCurrentState = SERVICE_RUNNING;
end;

function TServiceEntry.CanResume: boolean;
begin
  Result := Status.dwCurrentState = SERVICE_PAUSED;
end;


function TServiceEntryList.Find(const AServiceName: string): TServiceEntry;
var i: integer;
begin
  Result := nil;
  for i := 0 to Self.Count-1 do
    if SameText(Self[i].ServiceName, AServiceName) then begin
      Result := Self[i];
      break;
    end;
end;


//Reload data for a specified service and repaint the nodes
procedure RefreshService(Service: TServiceEntry);
begin
  Service.Refresh();
  InvalidateService(Service);
end;

procedure InvalidateService(Service: TServiceEntry);
var event: TNotifyEvent;
begin
  for event in OnServiceInvalidated do
    event(Service);
end;


initialization
  OnServiceInvalidated := TList<TNotifyEvent>.Create;

finalization
{$IFDEF DEBUG}
  FreeAndNil(OnServiceInvalidated);
{$ENDIF}

end.
