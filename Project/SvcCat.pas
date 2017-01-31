unit SvcCat;
// Service catalogue

interface
uses Generics.Collections;

type
 //Service information loaded from Catalogue
  TServiceFlag = (sfCritical, sfTelemetry);
  TServiceFlags = set of TServiceFlag;

  TServiceFolder = class;

 {
  At the moment, every service may have at most one .txt entry and any number of .lnk entries.
  .txt entry is stored in FMainFile, .lnk files only add entries to Folders.

  Service MainFile may also be located in the root folder, in which case it's not added to Folders.

  Moving the services moves its MainFile, removes the old folder from Folders, adds new one,
  removes the service from old Folder object and inserts into new one.
  All the rest of Folders are unaffected.

  Moving the MainFile where there is already an .lnk is ignored, but if we are to proceed,
  the latter is unaffected. So there can be both .lnk and .txt in a folder.
  For this reason, Load shall check IsInFolder before doing another AddFolder.
 }
  TServiceInfo = class
  protected
    FMainFile: string;
    FMainFolder: TServiceFolder;
    FComments: string;
  public
    ServiceName: string;
    DisplayName: string;
    Description: string;
    CriticalText: string;
    Flags: TServiceFlags;
    procedure Reset;
    procedure LoadFromFile(const AFilename: string);
    procedure SaveToMainFile;

  protected
    procedure AddFolder(AFolder: TServiceFolder);
    procedure RemoveFolder(AFolder: TServiceFolder);
  public
    Folders: array of TServiceFolder;
    function IsInFolder(AFolder: TServiceFolder): boolean;
    function GetFolderIndex(AFolder: TServiceFolder): integer;

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
    function IsDescendantOf(AParentFolder: TServiceFolder): boolean;

  protected
    procedure AddService(AService: TServiceInfo);
    procedure RemoveService(AService: TServiceInfo);
  public
    function GetServiceIndex(AServiceName: string): integer;
    function ContainsService(AServiceName: string): boolean; inline;

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
    function Get(const AServiceName: string): TServiceInfo;
    property RootPath: string read FRootPath;
    property Folders: TObjectList<TServiceFolder> read FFolders;

  public
    procedure MoveFolder(Folder: TServiceFolder; NewParent: TServiceFolder);
    procedure MoveService(Service: TServiceInfo; NewFolder: TServiceFolder);
    procedure RemoveServiceFromFolder(Service: TServiceInfo; Folder: TServiceFolder);

  end;

implementation
uses SysUtils, Classes, Windows, FilenameUtils;

procedure TServiceInfo.Reset;
begin
  DisplayName := '';
  Description := '';
  CriticalText := '';
  Flags := [];
end;

{
Service description files are simple text files which are displayed intact,
except for the special commands which are extracted and processed, and put at the top on saving.
}
procedure TServiceInfo.LoadFromFile(const AFilename: string);
var sl: TStringList;
  ln: string;
  i: integer;
begin
  if FMainFile <> '' then
    raise Exception.CreateFmt('Service %s: Data already loaded', [Self.ServiceName]);
  FMainFile := AFilename;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    i := sl.Count;
    while i > 0 do begin  //process lines in the reverse order
      Dec(i);
      ln := Trim(sl[i]);
      if (ln <> '') and (ln[1]='#') then begin
        FComments := ln + #13#10 + FComments;   // remember, reverse order
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

    if FComments <> '' then
      SetLength(FComments, Length(FComments)-Length(#13#10));
    Self.Description := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TServiceInfo.SaveToMainFile;
var sl: TStringList;
begin
  if FMainFile = '' then
    raise Exception.CreateFmt('Service %s: File not assigned', [Self.ServiceName]);

  sl := TStringList.Create;
  try
    //TODO: Properly save everything, preferably exactly as it were

    if FComments <> '' then
      sl.Add(FComments);

    if Self.DisplayName <> '' then
      sl.Add('TITLE:'+Self.DisplayName);

    if sfCritical in Self.Flags then
      if Self.CriticalText <> '' then
        sl.Add('CRITICAL:'+Self.CriticalText)
      else
        sl.Add('CRITICAL');

    if sfTelemetry in Self.Flags then
      sl.Add('TELEMETRY');

    if Self.Description <> '' then
      sl.Add(Self.Description);
    sl.SaveToFile(FMainFile);
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

function TServiceInfo.IsInFolder(AFolder: TServiceFolder): boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to Length(Self.Folders)-1 do
    if Self.Folders[i] = AFolder then begin
      Result := true;
      exit;
    end;
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

function TServiceFolder.GetServiceIndex(AServiceName: string): integer;
var i: integer;
begin
  Result := -1;
  AServiceName := LowerCase(AServiceName);
  for i := 0 to Length(Self.Services)-1 do
    if LowerCase(Self.Services[i]) = AServiceName then begin
      Result := i;
      break;
    end;
end;

//True if folder contains the given service
function TServiceFolder.ContainsService(AServiceName: string): boolean;
begin
  Result := (GetServiceIndex(AServiceName) >= 0);
end;

function TServiceFolder.IsDescendantOf(AParentFolder: TServiceFolder): boolean;
var this: TServiceFolder;
begin
  Result := false;
  this := Self;
  while this <> AParentFolder do begin
    if this = nil then exit;
    this := this.Parent;
  end;
  Result := true;
end;

procedure TServiceFolder.AddService(AService: TServiceInfo);
begin
  SetLength(Self.Services, Length(Self.Services)+1);
  Self.Services[Length(Self.Services)-1] := ChangeFileExt(AService.ServiceName, '');
end;

procedure TServiceFolder.RemoveService(AService: TServiceInfo);
var i: integer;
begin
  i := GetServiceIndex(AService.ServiceName);
  if i < 0 then exit;

  Services[i] := '';
  Move(Services[i+1], Services[i], (Length(Services)-i-1)*SizeOf(Services));
  PPointer(@Services[Length(Services)-1])^ := nil; //zero without dereferencing
  SetLength(Services, Length(Services)-1);
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
      AService := Self.Get(ChangeFileExt(sr.Name, ''));
      AService.LoadFromFile(APath+'\'+sr.Name);
      AService.FMainFolder := ADir;
      if ADir <> nil then begin
        AService.AddFolder(ADir);
        ADir.AddService(AService);
      end;
    end else
    if ExtractFileExt(sr.Name) = '.lnk' then begin
      AService := Self.Get(ChangeFileExt(sr.Name, ''));
      if ADir <> nil then begin
        AService.AddFolder(ADir);
        ADir.AddService(AService);
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
  Self.Folders.Add(Result);
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

//Finds a service info record or creates a new one
function TServiceCatalogue.Get(const AServiceName: string): TServiceInfo;
begin
  Result := Self.Find(AServiceName);
  if Result = nil then begin
    Result := TServiceInfo.Create;
    Result.ServiceName := AServiceName;
    Self.Add(Result);
  end;
end;



resourcestring
  eCannotMoveIntoItself = 'Cannot move a folder into itself or one of its descendants';

//Moves a given folder to a new parent folder. Throws on errors.
procedure TServiceCatalogue.MoveFolder(Folder: TServiceFolder; NewParent: TServiceFolder);
var TargetPath: string;
begin
  if Folder.Parent = NewParent then exit; //already there

  if NewParent <> nil then
    TargetPath := NewParent.Path
  else
    TargetPath := Self.RootPath;

  if TargetPath[Length(TargetPath)] <> '\' then
    TargetPath := TargetPath + '\';
  TargetPath := TargetPath + ExtractFilename(Folder.Path);

  if NewParent.IsDescendantOf(Folder) then
    raise EArgumentException.Create(eCannotMoveIntoItself);

  if not MoveFile(PChar(Folder.Path), PChar(TargetPath)) then
    RaiseLastOsError();

  Folder.Path := TargetPath;
end;

//Moves a given service to a new host folder. Throws on errors.
//See the comments at the beginning of the file on how the .txt and .lnk entries are handled.
procedure TServiceCatalogue.MoveService(Service: TServiceInfo; NewFolder: TServiceFolder);
var TargetPath: string;
  OldFolder: TServiceFolder;
begin
  if Service.IsInFolder(NewFolder) then exit;

  if NewFolder <> nil then
    TargetPath := NewFolder.Path
  else
    TargetPath := Self.RootPath;

  if TargetPath[Length(TargetPath)] <> '\' then
    TargetPath := TargetPath + '\';
  TargetPath := TargetPath + Service.ServiceName + '.txt';

 //1. If MainFile is missing, create one at the destination.
 //2. If it's present,
 //  1. Move it from its present location.
 //  2. Remove its present location from Folders.

  if Service.FMainFile = '' then begin
    Service.FMainFile := TargetPath;
    Service.FMainFolder := NewFolder;
    Service.SaveToMainFile;
  end else begin

    if not MoveFile(PChar(Service.FMainFile), PChar(TargetPath)) then
      RaiseLastOsError;
    Service.FMainFile := TargetPath;

    OldFolder := Service.FMainFolder;
    if OldFolder <> nil then begin
      Service.RemoveFolder(OldFolder);
      OldFolder.RemoveService(Service);
    end;
  end;

  if NewFolder <> nil then begin
    Service.AddFolder(NewFolder);
    NewFolder.AddService(Service);
  end;
end;

procedure TServiceCatalogue.RemoveServiceFromFolder(Service: TServiceInfo; Folder: TServiceFolder);
begin
  if Folder = nil then exit; //can't be removed from there
  if not Service.IsInFolder(Folder) then exit;

  //First delete lnks
  if FileExists(Folder.Path+'\'+Service.ServiceName+'.lnk') then
    if not DeleteFile(PChar(Folder.Path+'\'+Service.ServiceName+'.lnk')) then
      RaiseLastOsError();

  //Next txts. We don't like to delete txts, so we'll move it somewhere
  if Service.FMainFolder = Folder then
    MoveService(Service, nil); //could've moved anywhere but root is as good as anything else
end;


end.
