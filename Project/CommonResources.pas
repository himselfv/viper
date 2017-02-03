unit CommonResources;

interface

uses
  SysUtils, Classes, Windows, ImgList, Controls;

type
  TCommonRes = class(TDataModule)
    ilImages: TImageList;
    ilOverlays: TImageList;
    procedure DataModuleCreate(Sender: TObject);
  protected
    function LoadIconResource(const ALibName: string; const AResName: string; AW, AH: integer): HICON;
    function LoadIconResource16_OldWay(const ALibName: string; AResId: integer): HICON;
  public const
    iTrigger: integer = 2;
    iTriggerFirewall: integer = 3;
    iTriggerDevice: integer = 4; //5
    iTriggerDomain: integer = 6;
    iTriggerIp: integer = 7;
    iTriggerEvent: integer = 8;
    iTriggerGroupPolicy: integer = 9;
    iTriggerUserPolicy: integer = 10;
    iTriggerMachinePolicy: integer = 11;
    iTriggerNetwork: integer = 12;
    function GetTriggerImageIndex(TriggerType: cardinal; const TriggerSubtype: TGUID): integer;
  public
    iFolder: integer;
    iService: integer;
    iShield: integer;
    iShieldOverlay: integer;
    iDriver: integer;
    function LoadIcon16(const ALibName: string; AResId: integer): integer; overload;
    function LoadIcon16(const ALibName: string; const AResName: string): integer; overload;
    function LoadIcon8(const ALibName: string; const AResName: string): integer;
  end;

var
  CommonRes: TCommonRes;

implementation
uses Graphics, ShellApi, WinSvc, ServiceHelper;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TCommonRes.DataModuleCreate(Sender: TObject);
var i: integer;
begin
 //Overlays need to start at index 16 or VirtualTreeview will fail to render them
  for i := 0 to 15 do
    ilOverlays.Add(nil, nil);

  iFolder := LoadIcon16('shell32.dll', 4); //Folder icon from Explorer
  iDriver := LoadIcon16('shell32.dll', 12); //Chip icon from Explorer
  iService := LoadIcon16('filemgmt.dll', 0); //Service icon in services.msc
  iShield := LoadIcon16('imageres.dll', 'SHIDI_SHIELD_INTERNAL');
  iShieldOverlay := LoadIcon8('imageres.dll', 'SHIDI_SHIELD_INTERNAL');
end;

function TCommonRes.LoadIconResource(const ALibName: string; const AResName: string; AW, AH: integer): HICON;
var hLib: HMODULE;
begin
  hLib := LoadLibrary(PChar(ALibName));
  if (hLib = 0) or (hLib = INVALID_HANDLE_VALUE) then
    RaiseLastOsError();
  try
    Result := LoadImage(hLib, PChar(AResName), IMAGE_ICON, AW, AH, LR_DEFAULTCOLOR or LR_SHARED);
    if Result = 0 then
      RaiseLastOsError();
  finally
    FreeLibrary(hLib);
  end;
end;

function TCommonRes.LoadIconResource16_OldWay(const ALibName: string; AResId: integer): HICON;
var res: integer;
  hLargeIcon: HICON;
begin
  res := ExtractIconEx(PChar(ALibName), AResId, hLargeIcon, Result, 1);
  if (res=1) or (res=2) then begin
    DestroyIcon(hLargeIcon);
  end else
    RaiseLastOsError();
end;


function TCommonRes.LoadIcon16(const ALibName: string; AResId: integer): integer;
var hSmallIcon: HICON;
   icon: TIcon;
begin
  hSmallIcon := LoadIconResource16_OldWay(ALibName, AResId);

 //«агружаем хитрым образом, чтобы сохранить прозрачность. ¬се более простые
 //ведут к "битым кра€м".
  icon := TIcon.Create;
  try
    icon.Handle := hSmallIcon;
    icon.Transparent := true;
   //ImageList's ColorDepth has to be 32 bit, DrawingStyle transparent.
    Result := ilImages.AddIcon(icon);
  finally
    FreeAndNil(icon);
  end;
end;

function TCommonRes.LoadIcon16(const ALibName: string; const AResName: string): integer;
var hSmallIcon: HICON;
   icon: TIcon;
begin
  hSmallIcon := LoadIconResource(ALibName, AResName, 16, 16);

 //«агружаем хитрым образом, чтобы сохранить прозрачность. ¬се более простые
 //ведут к "битым кра€м".
  icon := TIcon.Create;
  try
    icon.Handle := hSmallIcon;
    icon.Transparent := true;
   //ImageList's ColorDepth has to be 32 bit, DrawingStyle transparent.
    Result := ilImages.AddIcon(icon);
  finally
    FreeAndNil(icon);
  end;
end;

function TCommonRes.LoadIcon8(const ALibName: string; const AResName: string): integer;
var hSmallIcon: HICON;
  icon: TIcon;
begin
  hSmallIcon := LoadIconResource(ALibName, AResName, 8, 8);
  icon := TIcon.Create;
  try
    icon.Handle := hSmallIcon;
    icon.Transparent := true;
   //ImageList's ColorDepth has to be 32 bit, DrawingStyle transparent.
    Result := ilOverlays.AddIcon(icon);
  finally
    FreeAndNil(icon);
  end;
end;


function TCommonRes.GetTriggerImageIndex(TriggerType: cardinal; const TriggerSubtype: TGUID): integer;
begin
  case TriggerType of
    SERVICE_TRIGGER_TYPE_DEVICE_INTERFACE_ARRIVAL: Result := Self.iTriggerDevice;
    SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY: Result := Self.iTriggerIp;
    SERVICE_TRIGGER_TYPE_DOMAIN_JOIN: Result := Self.iTriggerDomain;
    SERVICE_TRIGGER_TYPE_FIREWALL_PORT_EVENT: Result := Self.iTriggerFirewall;
    //TODO: The four types above would benefit from a separate "DIS-connected" versions
    SERVICE_TRIGGER_TYPE_GROUP_POLICY:
      if TriggerSubtype = MACHINE_POLICY_PRESENT_GUID then
        Result := Self.iTriggerMachinePolicy
      else
      if TriggerSubtype = USER_POLICY_PRESENT_GUID then
        Result := Self.iTriggerUserPolicy
      else
        Result := Self.iTriggerGroupPolicy;
    SERVICE_TRIGGER_TYPE_NETWORK_ENDPOINT: Result := Self.iTriggerNetwork;
    SERVICE_TRIGGER_TYPE_CUSTOM: Result := Self.iTriggerEvent;
  else
    Result := Self.iTrigger;
  end;
end;

end.
