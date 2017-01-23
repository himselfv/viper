unit SvcCat;
// Service catalogue

interface
uses Generics.Collections;

type
 //Service information loaded from Catalogue
  TServiceFlag = (sfCritical, sfTelemetry);
  TServiceFlags = set of TServiceFlag;

  TServiceFolder = class;

  TServiceInfo = class
  public
    ServiceName: string;
    DisplayName: string;
    Description: string;
    CriticalText: string;
    Flags: TServiceFlags;
    Folders: array of TServiceFolder;
    procedure Reset;
    procedure LoadFromFile(const AFilename: string);
    function GetFolderIndex(AFolder: TServiceFolder): integer;
    procedure AddFolder(AFolder: TServiceFolder);
    procedure RemoveFolder(AFolder: TServiceFolder);
  end;

  TServiceFolder = class
  public
    Name: string;
    Description: string;
    Services: array of string;
    Path: string;
    Parent: TServiceFolder;
    constructor Create(const APath: string);
    procedure LoadDescription(const AFilename: string);
    function ContainsService(AServiceName: string): boolean;
  end;

  TServiceCatalogue = class(TObjectList<TServiceInfo>)
  protected
    FRootPath: string;
    FFolders: TObjectList<TServiceFolder>;
    procedure LoadDirContents(const ADir: TServiceFolder; const APath: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    procedure Load(const ARootPath: string);
    function AddDir(AParent: TServiceFolder; const APath: string): TServiceFolder;
    function Find(const AServiceName: string): TServiceInfo;
    function LoadServiceInfo(const AFilename: string): TServiceInfo;
    property RootPath: string read FRootPath;
    property Folders: TObjectList<TServiceFolder> read FFolders;
  end;

implementation
uses SysUtils, Classes, FilenameUtils;


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

function TServiceInfo.GetFolderIndex(AFolder: TServiceFolder): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(Self.Folders)-1 do
    if Self.Folders[i]=AFolder then begin
      Result := i;
      break;
    end;
end;

procedure TServiceInfo.AddFolder(AFolder: TServiceFolder);
begin
  SetLength(Folders, Length(Folders)+1);
  Folders[Length(Folders)-1] := AFolder;
end;

procedure TServiceInfo.RemoveFolder(AFolder: TServiceFolder);
var i: integer;
begin
  i := GetFolderIndex(AFolder);
  if i < 0 then exit;

  Move(Folders[i+1], Folders[i], (Length(Folders)-i-1)*SizeOf(AFolder));
  SetLength(Folders, Length(Folders)-1);
end;


constructor TServiceFolder.Create(const APath: string);
begin
  inherited Create;
  Self.Name := ExtractFilename(APath);
  Self.Path := APath;

  //Try to load the directory descrtiption
  try
    Self.LoadDescription(APath+'\desc');
  except
    on EFOpenError do begin end; //no description, okay
  end;
end;

//Loads folder description from description file
procedure TServiceFolder.LoadDescription(const AFilename: string);
var sl: TStringList;
begin
  sl := TStringList.Create();
  try
    sl.LoadFromFile(AFilename);
    Self.Description := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;

//True if folder contains the given service
function TServiceFolder.ContainsService(AServiceName: string): boolean;
var i: integer;
begin
  Result := false;
  AServiceName := LowerCase(AServiceName);
  for i := 0 to Length(Self.Services)-1 do
    if LowerCase(Self.Services[i]) = AServiceName then begin
      Result := true;
      break;
    end;
end;


constructor TServiceCatalogue.Create;
begin
  inherited Create({OwnsObjects=}true);
  FFolders := TObjectList<TServiceFolder>.Create({OwnsObjects=}true);
end;

destructor TServiceCatalogue.Destroy;
begin
  FreeAndNil(FFolders);
  inherited;
end;

procedure TServiceCatalogue.Clear;
begin
  inherited;
  FFolders.Clear;
end;

procedure TServiceCatalogue.Load(const ARootPath: string);
begin
  FRootPath := ARootPath;
  LoadDirContents(nil, ARootPath);
end;

procedure TServiceCatalogue.LoadDirContents(const ADir: TServiceFolder; const APath: string);
var res: integer;
  sr: TSearchRec;
  ASubdir: TServiceFolder;
  AService: TServiceInfo;
begin

  res := FindFirst(APath+'\*.*', faAnyFile, sr);
  while res = 0 do begin
    if (sr.Name='.') or (sr.Name='..') then begin
      res := FindNext(sr);
      continue;
    end;

    if (sr.Attr and faDirectory) = faDirectory then begin
      ASubdir := AddDir(ADir, APath+'\'+sr.Name);
      LoadDirContents(ASubdir, APath+'\'+sr.Name);
    end else
    if ExtractFileExt(sr.Name) = '.txt' then begin
      AService := Self.LoadServiceInfo(APath+'\'+sr.Name);
      AService.AddFolder(ADir);
      if ADir <> nil then begin
        SetLength(ADir.Services, Length(ADir.Services)+1);
        ADir.Services[Length(ADir.Services)-1] := ChangeFileExt(sr.Name, '');
      end;
    end else
    if ExtractFileExt(sr.Name) = '.lnk' then begin
      if ADir <> nil then begin
        SetLength(ADir.Services, Length(ADir.Services)+1);
        ADir.Services[Length(ADir.Services)-1] := ChangeFileExt(sr.Name, '');
      end;
    end;

    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;

function TServiceCatalogue.AddDir(AParent: TServiceFolder; const APath: string): TServiceFolder;
begin
  Result := TServiceFolder.Create(APath);
  Result.Parent := AParent;
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
