unit SetupApiHelper;
//Delphi lacks SetupApi conversion, so we port what we need explicitly.

interface
uses Windows;

const
  setupapi = 'SetupApi.dll';

type
   HDEVINFO = Pointer;

const
  ERROR_INVALID_CLASS = $E0000206; //often returned by Di*


// Flags controlling what is included in the device information set built
// by SetupDiGetClassDevs
const
  DIGCF_DEFAULT         = $00000001; // only valid with DIGCF_DEVICEINTERFACE
  DIGCF_PRESENT         = $00000002;
  DIGCF_ALLCLASSES      = $00000004;
  DIGCF_PROFILE         = $00000008;
  DIGCF_DEVICEINTERFACE = $00000010;

function SetupDiGetClassDevsW(ClassGuid: PGUID; const Enumerator: PWideChar;
  hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall; external setupapi;

function SetupDiDestroyDeviceInfoList(DeviceInfoSet: HDEVINFO): BOOL; stdcall; external setupapi;

type
  TSPDevInfoData = packed record
    cbSize: NativeUInt;
    ClassGuid: TGUID;
    DevInst: DWORD; // DEVINST handle
    Reserved: ULONG_PTR;
  end;
  PSPDevInfoData = ^TSPDevInfoData;

function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO;
  MemberIndex: DWORD; var DeviceInfoData: TSPDevInfoData): BOOL; stdcall; external setupapi;

type
  TSPDeviceInterfaceData = packed record
    cbSize: NativeUInt;
    InterfaceClassGuid: TGUID;
    Flags: DWORD;
    Reserved: ULONG_PTR;
  end;
  PSPDeviceInterfaceData = ^TSPDeviceInterfaceData;

function SetupDiEnumDeviceInterfaces(DeviceInfoSet: HDEVINFO;
  DeviceInfoData: PSPDevInfoData; const InterfaceClassGuid: TGUID;
  MemberIndex: DWORD; var DeviceInterfaceData: TSPDeviceInterfaceData): BOOL; stdcall; external setupapi;


function SetupDiGetDeviceInterfaceClassDescription(const DiClassGuid: TGUID; out Description: string): boolean;


function SetupDiGetClassDescriptionW(ClassGuid: PGUID; ClassDescription: PChar;
  ClassDescriptionSize: DWORD; RequiredSize: PDWORD): BOOL; stdcall; external setupapi;

function TrySetupDiGetClassDescriptionStr(const ClassGuid: TGUID; out Description: string): boolean;
function SetupDiGetClassDescriptionStr(const ClassGuid: TGUID): string;

implementation
uses SysUtils;

{
See:
  https://msdn.microsoft.com/en-us/windows/hardware/drivers/install/accessing-device-interface-class-properties
  https://www.codeproject.com/articles/14412/enumerating-windows-device
}

function SetupDiGetDeviceInterfaceClassDescription(const DiClassGuid: TGUID; out Description: string): boolean;
var hDi: HDEVINFO;
  did: TSPDevInfoData;
begin
  //Query the default interface for the device interface class
  hDi := SetupDiGetClassDevsW(@DiClassGuid, nil, 0, DIGCF_DEVICEINTERFACE);
  if hDi = HDEVINFO(INVALID_HANDLE_VALUE) then begin
    Result := false;
    exit;
  end;
  try
    //Query the first device in the result set
    did.cbSize := sizeof(did);
    if not SetupDiEnumDeviceInfo(hDi, 0, did) then begin
      Result := false;
      exit;
    end;

    Result := TrySetupDiGetClassDescriptionStr(did.ClassGuid, Description);
  finally
    SetupDiDestroyDeviceInfoList(hDi);
  end;
end;


function TrySetupDiGetClassDescriptionStr(const ClassGuid: TGUID; out Description: string): boolean;
var sz: DWORD;
begin
  sz := 255;
  SetLength(Description, sz);

  if not SetupDiGetClassDescriptionW(@ClassGuid, PChar(Description), sz, @sz) then begin
    if GetLastError <> ERROR_MORE_DATA then begin
      Result := false;
      exit;
    end;
    SetLength(Description, sz);
    if not SetupDiGetClassDescriptionW(@ClassGuid, PChar(Description), sz, @sz) then begin
      Result := false;
      exit;
    end;
  end;
  SetLength(Description, sz);
  Result := true;
end;

function SetupDiGetClassDescriptionStr(const ClassGuid: TGUID): string;
begin
  if not TrySetupDiGetClassDescriptionStr(ClassGuid, Result) then
    RaiseLastOsError();
end;

end.
