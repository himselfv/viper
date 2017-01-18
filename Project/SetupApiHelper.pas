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

type
  TDeviceInterfaceClassDesc = record
    c: TGuid;
    n: string;
  end;
  PDeviceInterfaceClassDesc = ^TDeviceInterfaceClassDesc;

var
 {
  Sources:
    https://msdn.microsoft.com/en-us/library/windows/hardware/ff553412(v=vs.85).aspx
 }

  WellKnownDeviceInterfaceClasses: array[0..46] of TDeviceInterfaceClassDesc = (
   //1394 and 61883 Devices
   (c: '{6BDD1FC1-810F-11d0-BEC7-08002BE2092F}'; n: 'BUS1394'),
   (c: '{7EBEFBC0-3200-11d2-B4C2-00A0C9697D07}'; n: '61883'),
   //Battery and ACPI Devices
   (c: '{629758EE-986E-4D9E-8E47-DE27F8AB054D}'; n: 'APPLICATIONLAUNCH_BUTTON'),
   (c: '{72631E54-78A4-11D0-BCF7-00AA00B7B32A}'; n: 'BATTERY'),
   (c: '{4AFA3D52-74A7-11d0-be5e-00A0C9062857}'; n: 'LID'),
   (c: '{3FD0F03D-92E0-45FB-B75C-5ED8FFB01021}'; n: 'MEMORY'),
   (c: '{CD48A365-FA94-4CE2-A232-A1B764E5D8B4}'; n: 'MESSAGE_INDICATOR'),
   (c: '{97FADB10-4E33-40AE-359C-8BEF029DBDD0}'; n: 'PROCESSOR'),
   (c: '{4AFA3D53-74A7-11d0-be5e-00A0C9062857}'; n: 'SYS_BUTTON'),
   (c: '{4AFA3D51-74A7-11d0-be5e-00A0C9062857}'; n: 'THERMAL_ZONE'),
  //Bluetooth Devices
   (c: '{0850302A-B344-4fda-9BE9-90576B8D46F0}'; n: 'BTHPORT'),
  //Display and Image Devices
   (c: '{FDE5BBA4-B3F9-46FB-BDAA-0728CE3100B4}'; n: 'BRIGHTNESS'),
   (c: '{5B45201D-F2F2-4F3B-85BB-30FF1F953599}'; n: 'DISPLAY_ADAPTER'),
   (c: '{2564AA4F-DDDB-4495-B497-6AD4A84163D7}'; n: 'I2C'),       //Display adapters that can communicate with monitors via I2C
   (c: '{6BDD1FC6-810F-11D0-BEC7-08002BE2092F}'; n: 'IMAGE'),     //Scanners and cameras
   (c: '{E6F07B5F-EE97-4a90-B076-33F57BF4EAA7}'; n: 'MONITOR'),
   (c: '{BF4672DE-6B4E-4BE4-A325-68A91EA49C09}'; n: 'OPM'),       //Display adapters that support output protection management
   (c: '{1AD9E4F0-F88D-4360-BAB9-4C2D55E564CD}'; n: 'VIDEO_OUTPUT_ARRIVAL'),     //No functions, registered to notify of display availability
   (c: '{1CA05180-A699-450A-9A0C-DE4FBE3DDD89}'; n: 'DISPLAY_DEVICE_ARRIVAL'),   //No functions, registered to notify of display availability
  //Interactive Input Devices
   (c: '{4D1E55B2-F16F-11CF-88CB-001111000030}'; n: 'HID'),   //Keyboard, mouse etc. There were older ones for these specifically.
   (c: '{884b96c3-56ef-11d1-bc8c-00a0c91405dd}'; n: 'KEYBOARD'),
   (c: '{378DE44C-56EF-11D1-BC8C-00A0C91405DD}'; n: 'MOUSE'),
  //Modem Devices
   (c: '{2C7089AA-2E0E-11D1-B114-00C04FC2AAE4}'; n: 'MODEM'),
  //Network Devices
   (c: '{CAC88484-7515-4C03-82E6-71A87ABAC361}'; n: 'NET'),
  //Sensor Devices
   (c: '{BA1BB692-9B7A-4833-1E9A-525ED134E7E2}'; n: 'SENSOR'),
  //Serial and Parallel Port Devices
   (c: '{86E0D1E0-8089-11D0-9CE4-08003E301F73}'; n: 'COMPORT'),
   (c: '{97F76EF0-F883-11D0-AF1F-0000F800845C}'; n: 'PARALLEL'),  //Parallel port
   (c: '{811FC6A5-F728-11D0-A537-0000F8753ED1}'; n: 'PARCLASS'),  //Device attached to parallel port
   (c: '{4D36E978-E325-11CE-BFC1-08002BE10318}'; n: 'SERENUM_BUS_ENUMERATOR'),  //Plug and play serial port
  //Storage Devices
   (c: '{53F56312-B6BF-11D0-94F2-00A0C91EFB8B}'; n: 'CDCHANGER'),
   (c: '{53F56308-B6BF-11D0-94F2-00A0C91EFB8B}'; n: 'CDROM'),
   (c: '{53F56307-B6BF-11D0-94F2-00A0C91EFB8B}'; n: 'DISK'),  //Hard disk
   (c: '{53F56311-B6BF-11D0-94F2-00A0C91EFB8B}'; n: 'FLOPPY'),
   (c: '{53F56310-B6BF-11D0-94F2-00A0C91EFB8B}'; n: 'MEDIUMCHANGER'),
   (c: '{53F5630A-B6BF-11D0-94F2-00A0C91EFB8B}'; n: 'PARTITION'),
   (c: '{2ACCFE60-C130-11D2-B082-00A0C91EFB8B}'; n: 'STORAGEPORT'),
   (c: '{53F5630B-B6BF-11D0-94F2-00A0C91EFB8B}'; n: 'TAPE'),
   (c: '{53F5630D-B6BF-11D0-94F2-00A0C91EFB8B}'; n: 'IO_VOLUME'),  //Disk volume
   (c: '{53F5630C-B6BF-11D0-94F2-00A0C91EFB8B}'; n: 'WRITEONCEDISK'),
  //Kernel Streaming Media Devices
   (c: '{095780C3-48A1-4570-BD95-46707F78C2DC}'; n: 'AVC'),  //Audio / Video Control devices
   (c: '{616EF4D0-23CE-446D-A568-C31EB01913D0}'; n: 'VIRTUAL_AVC'),
   //KSCATEGORIES not listed because they're not really devices and there's too much of them
  //USB Devices
   (c: '{A5DCBF10-6530-11D2-901F-00C04FB951ED}'; n: 'USB_DEVICE'),
   (c: '{3ABF6F2D-71C4-462A-8A92-1E6861E6AF27}'; n: 'USB_HOST_CONTROLLER'),
   (c: '{F18A0E88-C30C-11D0-8815-00A0C906BED8}'; n: 'USB_HUB'),
  //Windows Portable Devices
   (c: '{6AC27878-A6FA-4155-BA85-F98F491D4F33}'; n: 'WPD'), //Windows Portable Device
   (c: '{BA0C718F-4DED-49B7-BDD3-FABE28661211}'; n: 'WPD_PRIVATE'),
  //Windows SideShow Devices
   (c: '{152E5811-FEB9-4B00-90F4-D32947AE1681}'; n: 'SIDESHOW')
  );

function GetWellKnownDeviceInterfaceClass(const ClassGuid: TGuid): PDeviceInterfaceClassDesc;
function GetWellKnownDeviceInterfaceClassName(const ClassGuid: TGuid): string;

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
  hDi := SetupDiGetClassDevsW(@DiClassGuid, nil, 0, DIGCF_DEVICEINTERFACE or DIGCF_DEFAULT);
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

function GetWellKnownDeviceInterfaceClass(const ClassGuid: TGuid): PDeviceInterfaceClassDesc;
var i: integer;
begin
  Result := nil;
  for i := 0 to Length(WellKnownDeviceInterfaceClasses)-1 do
    if WellKnownDeviceInterfaceClasses[i].c = ClassGuid then begin
      Result := @WellKnownDeviceInterfaceClasses[i];
      break;
    end;
end;

function GetWellKnownDeviceInterfaceClassName(const ClassGuid: TGuid): string;
var desc: PDeviceInterfaceClassDesc;
begin
  desc := GetWellKnownDeviceInterfaceClass(ClassGuid);
  if desc <> nil then
    Result := desc.n
  else
    Result := '';
end;

end.
