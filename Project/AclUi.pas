unit AclUi;

interface
uses Windows, Messages;

type
  PSI_OBJECT_INFO = ^SI_OBJECT_INFO;
  _SI_OBJECT_INFO = record
    dwFlags: DWORD;
    hInstance: HINST;  // resources (e.g. strings) reside here
    pszServerName: LPWSTR; // must be present
    pszObjectName: LPWSTR; // must be present
    pszPageTitle: LPWSTR;  // only valid if SI_PAGE_TITLE is set
    guidObjectType: TGUID;  // only valid if SI_OBJECT_GUID is set
  end;
  SI_OBJECT_INFO = _SI_OBJECT_INFO;

  TSiObjectInfo = SI_OBJECT_INFO;
  PSiObjectInfo = PSI_OBJECT_INFO;

// SI_OBJECT_INFO flags

const
  SI_EDIT_PERMS      = $00000000; // always implied
  SI_EDIT_OWNER      = $00000001;
  SI_EDIT_AUDITS     = $00000002;
  SI_CONTAINER       = $00000004;
  SI_READONLY        = $00000008;
  SI_ADVANCED        = $00000010;
  SI_RESET           = $00000020;
  SI_OWNER_READONLY  = $00000040;
  SI_EDIT_PROPERTIES = $00000080;
  SI_OWNER_RECURSE   = $00000100;
  SI_NO_ACL_PROTECT  = $00000200;
  SI_NO_TREE_APPLY   = $00000400;
  SI_PAGE_TITLE      = $00000800;
  SI_SERVER_IS_DC    = $00001000;
  SI_RESET_DACL_TREE = $00004000;
  SI_RESET_SACL_TREE = $00008000;
  SI_OBJECT_GUID     = $00010000;

  SI_EDIT_ALL = SI_EDIT_PERMS or SI_EDIT_OWNER or SI_EDIT_AUDITS;

type
  PSI_ACCESS = ^SI_ACCESS;
  _SI_ACCESS = record
    pguid: PGUID;
    mask: ACCESS_MASK;
    pszName: LPCWSTR; // may be resource ID
    dwFlags: DWORD;
  end;
  SI_ACCESS = _SI_ACCESS;

  TSiAccess = SI_ACCESS;
  PSiAccess = PSI_ACCESS;

// SI_ACCESS flags

const
  SI_ACCESS_SPECIFIC  = $00010000;
  SI_ACCESS_GENERAL   = $00020000;
  SI_ACCESS_CONTAINER = $00040000; // general access, container-only
  SI_ACCESS_PROPERTY  = $00080000;

// ACE inheritance flags (CONTAINER_INHERIT_ACE, etc.) may also be set.
// They will be used as the inheritance when an access is turned on.

type
  PSI_INHERIT_TYPE = ^SI_INHERIT_TYPE;
  _SI_INHERIT_TYPE = record
    pguid: PGUID;
    dwFlags: ULONG;
    pszName: LPCWSTR; // may be resource ID
  end;
  SI_INHERIT_TYPE = _SI_INHERIT_TYPE;

  TSiInheritType = SI_INHERIT_TYPE;
  PSiInheritType = PSI_INHERIT_TYPE;

// SI_INHERIT_TYPE flags are a combination of INHERIT_ONLY_ACE,
// CONTAINER_INHERIT_ACE, and OBJECT_INHERIT_ACE.

  _SI_PAGE_TYPE = (SI_PAGE_PERM, SI_PAGE_ADVPERM, SI_PAGE_AUDIT, SI_PAGE_OWNER);
  SI_PAGE_TYPE = _SI_PAGE_TYPE;

  TSiPageType = _SI_PAGE_TYPE;

// Message to PropertySheetPageCallback (in addition to
// PSPCB_CREATE and PSPCB_RELEASE)

const
  PSPCB_SI_INITDIALOG = WM_USER + 1;

const
  IID_ISecurityInformation: TGUID = (
    D1:$965fc360; D2:$16ff; D3:$11d0; D4:($91, $cb, $0, $aa, $0, $bb, $b7, $23));
  IID_ISecurityInformation2: TGUID = (
    D1:$c3ccfdb4; D2:$6f88; D3:$11d2; D4:($a3, $ce, $0, $c0, $4f, $b1, $78, $2a));

  SID_ISecurityInformation  = '{965FC360-16FF-11d0-91CB-00AA00BBB723}';
  SID_ISecurityInformation2 = '{c3ccfdb4-6f88-11d2-a3ce-00c04fb1782a}';

type
  ISecurityInformation = interface (IUnknown)
  [SID_ISecurityInformation]
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

  LPSECURITYINFO = ISecurityInformation;

  PPSID = ^PSID;

  ISecurityInformation2 = interface (IUnknown)
  [SID_ISecurityInformation]
    function IsDaclCanonical(pDacl: PACL): BOOL; stdcall;
    function LookupSids(cSids: ULONG; rgpSids: PPSID;
      out ppdo: Pointer{*LPDATAOBJECT}): HRESULT; stdcall;
  end;

  LPSECURITYINFO2 = ISecurityInformation2;

// HGLOBAL containing SID_INFO_LIST returned by ISecurityInformation2::LookupSids

const
  CFSTR_ACLUI_SID_INFO_LIST = 'CFSTR_ACLUI_SID_INFO_LIST';

// Data structures corresponding to CFSTR_ACLUI_SID_INFO_LIST

type
  PSID_INFO = ^SID_INFO;
  _SID_INFO = record
    pSid: PSID;
    pwzCommonName: PWideChar;
    pwzClass: PWideChar; // Used for selecting icon, e.g. "User" or "Group"
    pwzUPN: PWideChar;   // Optional, may be NULL
  end;
  SID_INFO = _SID_INFO;

  TSidInfo = SID_INFO;
  PSidInfo = PSID_INFO;

  PSID_INFO_LIST = ^SID_INFO_LIST;
  _SID_INFO_LIST = record
    cItems: ULONG;
    aSidInfo: array [0..0] of SID_INFO;
  end;
  SID_INFO_LIST = _SID_INFO_LIST;

  TSidInfoList = SID_INFO_LIST;
  PSidInfoList = PSID_INFO_LIST;

type
  HPROPSHEETPAGE = Pointer;

function CreateSecurityPage(psi: LPSECURITYINFO): HPROPSHEETPAGE; stdcall;
function EditSecurity(hwndOwner: HWND; psi: LPSECURITYINFO): BOOL; stdcall;

implementation

const
  aclui_lib = 'aclui.dll';

function CreateSecurityPage; external aclui_lib name 'CreateSecurityPage';
function EditSecurity; external aclui_lib name 'EditSecurity';

end.
