unit SecEditTasks;
{
Security editor interface for services.
Usage:
  EditTaskSecurity(ParentHwnd, IRegisteredTask);
}

interface
uses Windows, AclUi, AclHelpers, WinSvc, ServiceHelper, SvcEntry, SecEdit, taskschd;

type
  TTaskSecurityInformation = class(TInterfacedObject, ISecurityInformation)
  class var
    AccessMasks: array of SI_ACCESS;
    AccessNames: array of string;
    class constructor Create;
  protected
    FTask: IRegisteredTask;
    FFlags: TEditSecurityFlags;
  public
    constructor Create(const ATask: IRegisteredTask);
    destructor Destroy; override;
    property Flags: TEditSecurityFlags read FFlags write FFlags;

  public
    function GetObjectInformation(out pObjectInfo: SI_OBJECT_INFO): HRESULT; stdcall;
    function GetSecurity(RequestedInformation: SECURITY_INFORMATION;
      out ppSecurityDescriptor: PSECURITY_DESCRIPTOR; fDefault: BOOL): HRESULT; stdcall;
    function SetSecurity(SecurityInformation: SECURITY_INFORMATION;
      pSecurityDescriptor: PSECURITY_DESCRIPTOR): HRESULT; stdcall;
    function GetAccessRights(pguidObjectType: PGUID; dwFlags: DWORD;
      out ppAccess: PSI_ACCESS; out pcAccesses, piDefaultAccess: ULONG): HRESULT; stdcall;
    function MapGeneric(pguidObjectType: PGUID; pAceFlags: PUCHAR;
      pMask: PACCESS_MASK): HRESULT; stdcall;
    function GetInheritTypes(out ppInheritTypes: PSI_INHERIT_TYPE;
      out pcInheritTypes: ULONG): HRESULT; stdcall;
    function PropertySheetPageCallback(hwnd: HWND; uMsg: UINT;
      uPage: SI_PAGE_TYPE): HRESULT; stdcall;
  end;

function EditTaskSecurity(hwndOwner: HWND; const Task: IRegisteredTask; Flags: TEditSecurityFlags = []): boolean;


const
  //I don't know where's the list of task-specific permissions, and if there's one.
  //In Scheduler 1.0 these had been simply file/folder permissions on Windows\Tasks subfolders.
  //So we'll use file/folder permissions for now.
  STANDARD_RIGHTS_EXTRA = READ_CONTROL or WRITE_DAC or WRITE_OWNER or _DELETE or SYNCHRONIZE;

  FILE_GENERIC_READ = STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_EA or FILE_READ_ATTRIBUTES;
  FILE_GENERIC_WRITE = STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or FILE_APPEND_DATA or FILE_WRITE_EA or FILE_WRITE_ATTRIBUTES;
  FILE_GENERIC_EXECUTE = STANDARD_RIGHTS_EXECUTE or FILE_EXECUTE or FILE_READ_ATTRIBUTES;
  FILE_ALL_ACCESS = FILE_GENERIC_READ or FILE_GENERIC_WRITE or FILE_GENERIC_EXECUTE or STANDARD_RIGHTS_EXTRA;
  FILE_ACCESS_MAP: TGenericMapping = (
    GenericRead: FILE_GENERIC_READ;
    GenericWrite: FILE_GENERIC_WRITE;
    GenericExecute: FILE_GENERIC_EXECUTE;
    GenericAll: FILE_ALL_ACCESS;
  );

  DIR_GENERIC_READ = STANDARD_RIGHTS_READ or FILE_LIST_DIRECTORY or FILE_READ_EA or FILE_READ_ATTRIBUTES;
  DIR_GENERIC_WRITE = STANDARD_RIGHTS_WRITE or FILE_ADD_FILE or FILE_ADD_SUBDIRECTORY or FILE_WRITE_EA or FILE_WRITE_ATTRIBUTES or FILE_DELETE_CHILD;
  DIR_GENERIC_EXECUTE = STANDARD_RIGHTS_EXECUTE or FILE_TRAVERSE or FILE_READ_ATTRIBUTES;
  DIR_ALL_ACCESS = DIR_GENERIC_READ or DIR_GENERIC_WRITE or DIR_GENERIC_EXECUTE or STANDARD_RIGHTS_EXTRA;
  DIR_ACCESS_MAP: TGenericMapping = (
    GenericRead: DIR_GENERIC_READ;
    GenericWrite: DIR_GENERIC_WRITE;
    GenericExecute: DIR_GENERIC_EXECUTE;
    GenericAll: DIR_ALL_ACCESS;
  );


implementation
uses SysUtils, Viper.Log, JwaSddl, JwaWinNT;

resourcestring
  sTaskAccessAll       = 'Full access';
  sTaskAccessRead      = 'Read';
  sTaskAccessExecute   = 'Start, stop and pause';
  sTaskAccessWrite     = 'Write';

  sFileReadData        = 'Read data';
  sFileWriteData       = 'Write data';
  sFileAppendData      = 'Append data';
  sFileReadEa          = 'Read extended attributes';
  sFileWriteEa         = 'Write extended attributes';
  sFileExecute         = 'Execute';
  sFileReadAttributes  = 'Read attributes';
  sFileWriteAttributes = 'Write attributes';


class constructor TTaskSecurityInformation.Create;
var i: integer;
  procedure Push(m: ACCESS_MASK; const t: string; f: DWORD);
  begin
    AccessNames[i] := t; //we have to keep this somewhere or it'll be freed
    AccessMasks[i].pguid := nil;
    AccessMasks[i].mask := m;
    AccessMasks[i].pszName := PChar(AccessNames[i]);
    AccessMasks[i].dwFlags := f;
    Inc(i);
  end;

begin
  SetLength(AccessNames, 17);
  SetLength(AccessMasks, 17);
  i := 0;
 //We'd prefer to have a static precompiled table, but alas, resourcestrings cannot be referenced correctly in static
  Push(FILE_ALL_ACCESS,            sTaskAccessAll,            SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC);

  Push(FILE_GENERIC_READ,          sTaskAccessRead,           SI_ACCESS_GENERAL);
  Push(FILE_GENERIC_EXECUTE,       sTaskAccessExecute,        SI_ACCESS_GENERAL);
  Push(FILE_GENERIC_WRITE,         sTaskAccessWrite,          SI_ACCESS_GENERAL);

  Push(FILE_READ_DATA,             sFileReadData,             SI_ACCESS_SPECIFIC);
  Push(FILE_WRITE_DATA,            sFileWriteData,            SI_ACCESS_SPECIFIC);
  Push(FILE_APPEND_DATA,           sFileAppendData,           SI_ACCESS_SPECIFIC);
  Push(FILE_READ_EA,               sFileReadEa,               SI_ACCESS_SPECIFIC);
  Push(FILE_WRITE_EA,              sFileWriteEa,              SI_ACCESS_SPECIFIC);
  Push(FILE_EXECUTE,               sFileExecute,              SI_ACCESS_SPECIFIC);
  Push(FILE_READ_ATTRIBUTES,       sFileReadAttributes,       SI_ACCESS_SPECIFIC);
  Push(FILE_WRITE_ATTRIBUTES,      sFileWriteAttributes,      SI_ACCESS_SPECIFIC);

  Push(_DELETE,                    sCmnAccessDelete,          SI_ACCESS_GENERAL or SI_ACCESS_SPECIFIC);
  Push(READ_CONTROL,               sCmnAccessRead,            SI_ACCESS_SPECIFIC);
  Push(WRITE_DAC,                  sCmnAccessWriteDac,        SI_ACCESS_SPECIFIC);
  Push(WRITE_OWNER,                sCmnAccessWriteOwner,      SI_ACCESS_SPECIFIC);
end;

constructor TTaskSecurityInformation.Create(const ATask: IRegisteredTask);
begin
  inherited Create;
  Log('SecurityInformation.Create('+ATask.Name+')');
  FTask := ATask;
end;

destructor TTaskSecurityInformation.Destroy;
begin
  Log('SecurityInformation.Destroy');
  inherited;
end;

function TTaskSecurityInformation.GetObjectInformation(out pObjectInfo: SI_OBJECT_INFO): HRESULT;
begin
  Log('SecurityInformation.GetObjectInformation');
  pObjectInfo.dwFlags := SI_ADVANCED or SI_EDIT_ALL;
  pObjectInfo.pszObjectName := FTask.Name;
  Result := S_OK;
end;

function TTaskSecurityInformation.GetSecurity(RequestedInformation: SECURITY_INFORMATION;
  out ppSecurityDescriptor: Windows.PSECURITY_DESCRIPTOR; fDefault: BOOL): HRESULT;
var psddl: PWideChar;
  sdsize: DWORD;
  ppSecurityDescriptor2: PSECURITY_DESCRIPTOR;
begin
  Log('SecurityInformation.GetSecurity');
  Result := Self.FTask.GetSecurityDescriptor(RequestedInformation, psddl);
  if FAILED(Result) then begin
    Log('IRegisteredTask.GetSecurityDescriptor failed: 0x'+IntToHex(Result, 8));
    exit;
  end;

  ppSecurityDescriptor2 := nil; //Convert* needs "var" and ISecurityInformation gives us "out"
  if not ConvertStringSecurityDescriptorToSecurityDescriptor(
       psddl,
       SDDL_REVISION_1,
       JwaWinNT.PSECURITY_DESCRIPTOR(ppSecurityDescriptor2),
       @SDSize)
  then begin
    Log('SecurityInformation.GetSecurity: SDDLFromString failed.');
    Result := E_FAIL;
    exit;
  end;
  ppSecurityDescriptor := ppSecurityDescriptor2;
  //The caller will LocalFree the ppSecurityDescriptor as Convert* expects.

  Result := S_OK;
end;


function TTaskSecurityInformation.SetSecurity(SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: Windows.PSECURITY_DESCRIPTOR): HRESULT;
var psddl: PWideChar;
begin
  Log('SecurityInformation.SetSecurity...');
  if not ConvertSecurityDescriptorToStringSecurityDescriptor(
    pSecurityDescriptor,
    SDDL_REVISION_1,
    SecurityInformation,
    psddl,
    nil)
  then begin
    Log('SecurityInformation.SetSecurity: SDDLToString failed.');
    Result := E_FAIL;
    exit;
  end;

  Result := Self.FTask.SetSecurityDescriptor(psddl, 0);
  if FAILED(Result) then begin
    Log('IRegisteredTask.SetSecurityDescriptor failed: 0x'+IntToHex(Result, 8));
    //But continue to free the buffer
  end;
end;

function TTaskSecurityInformation.GetAccessRights(pguidObjectType: PGUID; dwFlags: DWORD;
  out ppAccess: PSI_ACCESS; out pcAccesses, piDefaultAccess: ULONG): HRESULT;
begin
  Log('SecurityInformation.GetAccessRights');
  ppAccess := @Self.AccessMasks[0];
  pcAccesses := Length(Self.AccessMasks);
  piDefaultAccess := 0; //index of TASK_ALL_ACCESS
  Result := S_OK;
end;

function TTaskSecurityInformation.MapGeneric(pguidObjectType: PGUID; pAceFlags: PUCHAR;
  pMask: Windows.PACCESS_MASK): HRESULT;
begin
  Log('SecurityInformation.MapGeneric');
  MapGenericMask(pMask^, FILE_ACCESS_MAP);
  Result := S_OK;
end;

function TTaskSecurityInformation.GetInheritTypes(out ppInheritTypes: PSI_INHERIT_TYPE;
  out pcInheritTypes: ULONG): HRESULT;
begin
  Log('SecurityInformation.GetInheritTypes');
  ppInheritTypes := nil;
  pcInheritTypes := 0;
  Result := E_NOTIMPL;
end;

function TTaskSecurityInformation.PropertySheetPageCallback(hwnd: HWND; uMsg: UINT;
  uPage: SI_PAGE_TYPE): HRESULT;
begin
  Log('SecurityInformation.PropertySheetPageCallback');
  Result := S_OK;
end;

function EditTaskSecurity(hwndOwner: HWND; const Task: IRegisteredTask; Flags: TEditSecurityFlags): boolean;
var secInfo: TTaskSecurityInformation;
begin
  secInfo := TTaskSecurityInformation.Create(Task);
  secInfo.Flags := Flags;
  Result := EditSecurity(hwndOwner, secInfo);
end;

end.
