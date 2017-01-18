unit TriggerUtils;

interface
uses WinSvc;

function TriggerDataItemsToStr(ATrigger: PSERVICE_TRIGGER): string;


resourcestring
  sTriggerDeviceInterfaceAvailable = 'Device available: %s';

  sTriggerIpFirstAvailable = 'First IP address available';
  sTriggerIpLastLost = 'Last IP address lost';
  sTriggerIpOther = 'IP Address Avaibility (unknown exact event)';

  sTriggerDomainJoin = 'This computer joins a domain';
  sTriggerDomainLeave = 'This computer leaves a domain';
  sTriggerDomainOther = 'Unusual domain-related trigger';

  sTriggerFirewallPortOpen = 'Firewall port opens';
  sTriggerFirewallPortClose = 'Firewall port closes';
  sTriggerFirewallPortUnusual = 'Unusual firewall-related trigger';

  sTriggerGroupPolicyChangedMachine = 'Group Policy Changed (Machine)';
  sTriggerGroupPolicyChangedUser = 'Group Policy Changed (User)';
  sTriggerGroupPolicyChangedOther = 'Group Policy Changed (Other)';

  sUnknownDeviceInterfaceClass = 'Class %s';

type
  TTriggerParam = SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
  PTriggerParam = PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM;

  TTriggerParamHelper = record helper for TTriggerParam
    function ToString: string;
    function HexValue: string; inline;
    function StringValue: string; inline;
  end;

  TTriggerData = record
    Event: string;
    Sources: TArray<string>;
    Params: TArray<PTriggerParam>;
    procedure AddSource(const ASource: string); inline;
    function ExtractParamByType(const dwType: cardinal): PTriggerParam; overload;
    function ExtractParamByType(const dwType: cardinal; out AParam: PTriggerParam): boolean; overload;
    function SourcesToString(const ASep: string = #13): string;
    function ParamsToString(const ASep: string = #13): string;
  end;

//Parses trigger data, extracting its exact action as a string, a set of sources (such as ports
//or devices) to watch for that action, and a set of remaining params.
function ParseTrigger(const ATrigger: PSERVICE_TRIGGER): TTriggerData;

implementation
uses SysUtils, UniStrUtils, ServiceHelper, SetupApiHelper;

type
  PInt64 = ^Int64;

function TTriggerParamHelper.ToString: string;
begin
  case Self.dwDataType of
  SERVICE_TRIGGER_DATA_TYPE_BINARY:
    Result := 'Binary: '+Self.HexValue;
  SERVICE_TRIGGER_DATA_TYPE_STRING:
    Result := 'String: '+Self.StringValue;
  SERVICE_TRIGGER_DATA_TYPE_LEVEL:
    Result := 'Level: '+IntToStr(PByte(Self.pData)^);
  //Keyword filters for ETW
  //TODO: Can be further explained by querying the ETW provider
  SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ANY:
    Result := 'Keyword (any): '+IntToStr(PInt64(Self.pData)^);
  SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ALL:
    Result := 'Keyword (all): '+IntToStr(PInt64(Self.pData)^);
  else
    Result := 'Unknown data: '+Self.HexValue;
  end;
end;

function TTriggerParamHelper.HexValue: string;
begin
  Result := string(BinToHex(Self.pData, Self.cbData));
end;

function TTriggerParamHelper.StringValue: string;
begin
  Result := StrFromBuf(PWideChar(Self.pData), Self.cbData);
end;

function TriggerDataItemsToStr(ATrigger: PSERVICE_TRIGGER): string;
var cnt: cardinal;
  ptr: PTriggerParam;
begin
  Result := '';
  ptr := ATrigger.pDataItems;
  cnt := ATrigger.cDataItems;
  while cnt > 0 do begin
    Result := Result + ptr.ToString + #13;
    Inc(ptr);
    Dec(cnt);
  end;
  if Result <> '' then
    SetLength(Result, Length(Result)-1);
end;


//
// A device of the specified device interface class arrives.
// pTriggerSubtype specifies the device interface class GUID.
// pDataItems specifies one or more hardware ID and compatible ID strings.
//
function ParseDeviceInterfaceTrigger(const ATrigger: PSERVICE_TRIGGER): TTriggerData;
var param: PTriggerParam;
  tmp: string;
begin
{
SetupApi has two types of classes: Setup Class and Device Interface Class.

Setup Classes are categories shown in Device Manager. They have names and icons which can be
retrieved.

Device Interfaces are sets of functions that the device provides. A device can provide several
of these. Each Interface conforms to some Device Interface Class.
Device Interface Classes are simply guids associated with a particular set of functions.

The OS provides no names or icons for the device interface classes. There are a few well-known ones,
but in general, any GUID might mean something for someone.

There are some ways to access properties of the device interface class via it's default implementation:
  https://msdn.microsoft.com/en-us/windows/hardware/drivers/install/accessing-device-interface-class-properties
Unfortunately, default implementations for most classes are unavailable as well.
}

  tmp := GetWellKnownDeviceInterfaceClassName(ATrigger.pTriggerSubtype^);
  if tmp = '' then
    tmp := Format(sUnknownDeviceInterfaceClass, [GuidToString(ATrigger.pTriggerSubtype^)]);
  Result.Event := Format(sTriggerDeviceInterfaceAvailable, [tmp]);

  while Result.ExtractParamByType(SERVICE_TRIGGER_DATA_TYPE_STRING, param) do
    Result.AddSource(param.StringValue);
end;


{$POINTERMATH ON}

function ParseTrigger(const ATrigger: PSERVICE_TRIGGER): TTriggerData;
var i: integer;
  param: PTriggerParam;
begin
  SetLength(Result.Sources, 0);

  SetLength(Result.Params, ATrigger.cDataItems);
  for i := 0 to Length(Result.Params)-1 do
    Result.Params[i] := ATrigger.pDataItems + i;

 //See here: https://msdn.microsoft.com/en-us/library/windows/desktop/dd405512%28v=vs.85%29.aspx
  case ATrigger.dwTriggerType of

   // TODO:
   //
   // A device of the specified device interface class arrives.
   // pTriggerSubtype specifies the device interface class GUID.
   // pDataItems specifies one or more hardware ID and compatible ID strings.
   //
   SERVICE_TRIGGER_TYPE_DEVICE_INTERFACE_ARRIVAL:
     Result := ParseDeviceInterfaceTrigger(ATrigger);


   // DONE.
   //
   // The event is triggered when the first IP address on the TCP/IP networking stack becomes
   // available or the last IP address on the stack becomes unavailable.
   //
   // The pTriggerSubtype member specifies NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID
   // or NETWORK_MANAGER_LAST_IP_ADDRESS_REMOVAL_GUID.
   //
   // The pDataItems member is not used.
   //
    SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY:
      if ATrigger.pTriggerSubtype^ = NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID then
        Result.Event := sTriggerIpFirstAvailable
      else
      if ATrigger.pTriggerSubtype^ = NETWORK_MANAGER_LAST_IP_ADDRESS_REMOVAL_GUID then
        Result.Event := sTriggerIpLastLost
      else
        Result.Event := sTriggerIpOther;


   // DONE.
   //
   // The event is triggered when the computer joins or leaves a domain.
   // The pTriggerSubtype member specifies DOMAIN_JOIN_GUID or DOMAIN_LEAVE_GUID.
   // The pDataItems member is not used.
   //
    SERVICE_TRIGGER_TYPE_DOMAIN_JOIN:
      if ATrigger.pTriggerSubtype^ = DOMAIN_JOIN_GUID then
        Result.Event := sTriggerDomainJoin
      else
      if ATrigger.pTriggerSubtype^ = DOMAIN_LEAVE_GUID then
        Result.Event := sTriggerDomainLeave
      else
        Result.Event := sTriggerDomainOther;


   // TODO: Proper parsing of Firewall rules (it's multistring)
   //
   // The event is triggered when a firewall port is opened or approximately 60 seconds after
   // the firewall port is closed.
   //
   // The pTriggerSubtype member specifies FIREWALL_PORT_OPEN_GUID or FIREWALL_PORT_CLOSE_GUID.
   //
   // The pDataItems member specifies the port, the protocol, and optionally the executable path
   // and user information (SID string or name) of the service listening on the event.
   // The "RPC" token can be used in place of the port to specify any listening socket used by RPC.
   // The "system" token can be used in place of the executable path to specify ports created by
   // and listened on by the Windows kernel.
   //
   // The event is triggered only if all strings match. For example, if MyService hosted inside
   // MyServiceProcess.exe is to be trigger-started when port UDP 5001 opens, the trigger-specific
   // data would be the Unicode representation of
   // "5001\0UDP\0%programfiles%\MyApplication\MyServiceProcess.exe\0MyService\0\0".
   //
    SERVICE_TRIGGER_TYPE_FIREWALL_PORT_EVENT: begin
      if ATrigger.pTriggerSubtype^ = FIREWALL_PORT_OPEN_GUID then
        Result.Event := sTriggerFirewallPortOpen
      else
      if ATrigger.pTriggerSubtype^ = FIREWALL_PORT_CLOSE_GUID then
        Result.Event := sTriggerFirewallPortClose
      else
        Result.Event := sTriggerFirewallPortUnusual;

      while Result.ExtractParamByType(SERVICE_TRIGGER_DATA_TYPE_STRING, param) do
        Result.AddSource(param.StringValue);
    end;


   // DONE.
   //
   // The event is triggered when a machine policy or user policy change occurs.
   // The pTriggerSubtype member specifies MACHINE_POLICY_PRESENT_GUID or USER_POLICY_PRESENT_GUID.
   // The pDataItems member is not used.
   //
    SERVICE_TRIGGER_TYPE_GROUP_POLICY:
      if ATrigger.pTriggerSubtype^ = MACHINE_POLICY_PRESENT_GUID then
        Result.Event := sTriggerGroupPolicyChangedMachine
      else
      if ATrigger.pTriggerSubtype^ = USER_POLICY_PRESENT_GUID then
        Result.Event := sTriggerGroupPolicyChangedUser
      else
        Result.Event := sTriggerGroupPolicyChangedOther;


    // TODO: Incorporate additional data into the description
    //
    // The event is triggered when a packet or request arrives on a particular network protocol.
    // This request is commonly used to start a service that has stopped itself after an idle
    // time-out when there is no work to do.
    //
    // The pTriggerSubtype member specifies one of the following values:
    // RPC_INTERFACE_EVENT_GUID, NAMED_PIPE_EVENT_GUID, TCP_PORT_EVENT_GUID, or UDP_EVENT_PORT_GUID.
    //
    // The pDataItems member specifies an endpoint or interface GUID. The string must be Unicode.
    // The event triggers if the string is an exact match.
    //
    // The dwAction member must be SERVICE_TRIGGER_ACTION_SERVICE_START.
    //
    SERVICE_TRIGGER_TYPE_NETWORK_ENDPOINT: begin
      if ATrigger.pTriggerSubtype^ = RPC_INTERFACE_EVENT_GUID then
        Result.Event := 'RPC request'
      else
      if ATrigger.pTriggerSubtype^ = NAMED_PIPE_EVENT_GUID then
        Result.Event := 'Named pipe request'
      else
        Result.Event := Format('Network request via %s', [GuidToString(ATrigger.pTriggerSubtype^)]);
      //NOTE: From the docs, there are also TCP_PORT_EVENT_GUID and UDP_EVENT_PORT_GUID,
      //   but their values are not documented.

      while Result.ExtractParamByType(SERVICE_TRIGGER_DATA_TYPE_STRING, param) do
        Result.AddSource(param.StringValue);
    end;

   // TODO:
   //
   // The event is a custom event generated by an Event Tracing for Windows (ETW) provider.
   //
   // The pTriggerSubtype member specifies the event provider's GUID.
   //
   // The pDataItems member specifies trigger-specific data defined by the provider.
   //
    SERVICE_TRIGGER_TYPE_CUSTOM: begin
     //TODO: Get the provider name from GUID
      Result.Event := Format('ETW Event from %s', [GuidToString(ATrigger.pTriggerSubtype^)]);
    end

  else
    Result.Event := Format('Unusual trigger: %d / %s',
      [ATrigger.dwTriggerType, GuidToString(ATrigger.pTriggerSubtype^)]);
  end;
end;


procedure TTriggerData.AddSource(const ASource: string);
begin
  SetLength(Self.Sources, Length(Self.Sources)+1);
  Self.Sources[Length(Self.Sources)-1] := ASource;
end;

//Extracts first param with a given type
function TTriggerData.ExtractParamByType(const dwType: cardinal): PTriggerParam;
begin
  if not ExtractParamByType(dwType, Result) then
    Result := nil;
end;

function TTriggerData.ExtractParamByType(const dwType: cardinal; out AParam: PTriggerParam): boolean;
var i: integer;
begin
  Result := false;
  for i := 0 to Length(Self.Params)-1 do
    if Self.Params[i].dwDataType = dwType then begin
      AParam := Self.Params[i];
      Move(Self.Params[i+1], Self.Params[i], (Length(Self.Params)-i-1)*SizeOf(Self.Params[i]));
      SetLength(Self.Params, Length(Self.Params)-1);
      Result := true;
      break;
    end;
end;

function TTriggerData.SourcesToString(const ASep: string): string;
var i: integer;
begin
  Result := '';
  for i := 0 to Length(Self.Sources)-1 do
    Result := Result + Self.Sources[i] + ASep;
  if Result <> '' then
    SetLength(Result, Length(Result) - Length(ASep));
end;

function TTriggerData.ParamsToString(const ASep: string): string;
var i: integer;
begin
  Result := '';
  for i := 0 to Length(Self.Params)-1 do
    Result := Result + Self.Params[i].ToString + ASep;
  if Result <> '' then
    SetLength(Result, Length(Result) - Length(ASep));
end;

end.
