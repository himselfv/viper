unit SvcCat;
// Service catalogue

interface
uses Generics.Collections;

type
 //Service information loaded from Catalogue
  TServiceFlag = (sfCritical, sfTelemetry);
  TServiceFlags = set of TServiceFlag;

  TServiceInfo = class
  public
    ServiceName: string;
    DisplayName: string;
    Description: string;
    CriticalText: string;
    Flags: TServiceFlags;
    procedure Reset;
    procedure LoadFromFile(const AFilename: string);
  end;

  TServiceCatalogue = class(TObjectList<TServiceInfo>)
  public
    function Find(const AServiceName: string): TServiceInfo;
    function LoadServiceInfo(const AFilename: string): TServiceInfo;
  end;

implementation
uses SysUtils, Classes;

procedure TServiceInfo.Reset;
begin
  DisplayName := '';
  Description := '';
  CriticalText := '';
  Flags := [];
end;

procedure TServiceInfo.LoadFromFile(const AFilename: string);
var sl: TStringList;
  ln: string;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    i := sl.Count;
    while i > 0 do begin
      Dec(i);
      ln := Trim(sl[i]);
      if (ln='') or (ln[1]='#') then begin
        sl.Delete(i);
        continue;
      end;
      if ln.StartsWith('TITLE:') then begin
        Self.DisplayName := Trim(Copy(ln, 7, MaxInt));
        sl.Delete(i);
        continue;
      end;
      if ln.StartsWith('CRITICAL') then begin
        Self.Flags := Self.Flags + [sfCritical];
        Self.CriticalText := Self.CriticalText + Trim(Copy(ln, 10, MaxInt)) + #13;
        sl.Delete(i);
        continue;
      end;
      if ln.StartsWith('TELEMETRY') then begin
        Self.Flags := Self.Flags + [sfTelemetry];
        sl.Delete(i);
        continue;
      end;
    end;
    Self.Description := Self.Description + Trim(sl.Text);
  finally
    FreeAndNil(sl);
  end;
end;


function TServiceCatalogue.Find(const AServiceName: string): TServiceInfo;
var i: integer;
begin
  Result := nil;
  for i := 0 to Self.Count-1 do
    if SameText(Self[i].ServiceName, AServiceName) then begin
      Result := Self[i];
      break;
    end;
end;

//Reads static service information file and registers a service in a catalogue (or extends it)
function TServiceCatalogue.LoadServiceInfo(const AFilename: string): TServiceInfo;
var ServiceName: string;
begin
  ServiceName := ChangeFileExt(ExtractFilename(AFilename), '');
  Result := Self.Find(ServiceName);
  if Result = nil then begin
    Result := TServiceInfo.Create;
    Result.ServiceName := ServiceName;
    Self.Add(Result);
  end;

  Result.LoadFromFile(AFilename);
end;


end.
