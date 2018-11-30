unit TriggerUtils;

{$DEFINE QUERYLOCALRPC}
// Query local RPC interfaces and use this to decode the RPC interface IDs.
// Lets you decode some IDs but might make the app start a bit slower.

interface
uses WinSvc, Generics.Collections, UniStrUtils, GuidDict, WnfUtils;

{
Trigger related events. Please call when you're making appropriate changes.
Some are only valid for the changes made throughout the application. Handle them,
but also expect event-less changes.
}
type
  TTriggerListEvent = procedure(Sender: TObject; const AService: string) of object;

var
  //The trigger list for a given service has been changed
  OnTriggerListChanged: TList<TTriggerListEvent>;

procedure TriggerListChanged(Sender: TObject; const AService: string);


{
Trigger printing functions
}
resourcestring
  sTriggerActionStart = 'Start';
  sTriggerActionStop = 'Stop';
  sTriggerActionOther = 'Action (%d)';

function TriggerActionToString(const Action: cardinal): string; inline;

resourcestring
  sTriggerDataTypeBinary = 'Binary';
  sTriggerDataTypeString = 'String';
  sTriggerDataTypeLevel = 'Level';
  sTriggerDataTypeKeywordAny = 'Keyword (any)';
  sTriggerDataTypeKeywordAll = 'Keyword (all)';
  sTriggerDataTypeUnknown = 'Unknown (%d)';
  sTriggerDataTypeUnknownConst = 'Unknown';

function TriggerDataTypeToStr(const dwDataType: cardinal): string;

type
  TTriggerParam = SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
  PTriggerParam = PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM;

  TTriggerParamHelper = record helper for SERVICE_TRIGGER_SPECIFIC_DATA_ITEM
    function ToString: string; inline;
    function DataTypeToString: string; inline;
    function ValueToString: string;
    //The following functions only read the data but do not ensure the buffer size is enough
    function HexValue: string; inline;
    function StringValue: string; inline;
    function RawStringValue: string; inline;
    function MultistringValue: TStringArray;
    function ByteValue: byte; inline;
    function Int64Value: int64; inline;
    //Changes the size and reallocates the associated memory. Only use for the standalone structures!
    procedure Resize(const cbNewSize: cardinal);
    //The following functions reallocate memory and store data!
    procedure SetHexValue(const Value: string); inline;
    procedure SetStringValue(const Value: string); inline;
    procedure SetRawStringValue(const Value: string); inline;
    procedure SetMultistringValue(const Value: TStringArray);
    procedure SetByteValue(const Value: byte); inline;
    procedure SetInt64Value(const Value: int64); inline;
  end;

  TTriggerHelper = record helper for SERVICE_TRIGGER
  end;

function TriggerDataItemsToStr(ATrigger: PSERVICE_TRIGGER): string;



resourcestring
  sTriggerDeviceInterfaceAvailable = 'Device available: %s';
  sTriggerDeviceInterfaceAvailableConst = 'Device available';

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

  sTriggerSystemStateChange = 'System State Change';
  sTriggerSystemStateChangeParam = 'System State Change: %s';
  sTriggerSystemStateChangeUnusual = 'System State Change: %s';

  sTriggerNetworkEndpointRpc = 'RPC request';
  sTriggerNetworkEndpointNamedPipe = 'Named pipe request';
  sTriggerNetworkEndpointOther = 'Network request via %s';

  sTriggerEtwEvent = 'ETW Event from %s';
  sTriggerEtwEventConst = 'ETW Event';

  sTriggerAggregateEvent = 'Aggregate event: %s';
  sTriggerAggregateEventConst = 'Aggregate event';

  sUnknownDeviceInterfaceClass = 'Class %s';

type
 //Trigger source is stuff like ETW queue name, Device class, RPC interface GUID
 //Sometimes we can decode it into something user-friendly, then DisplayText is different from OriginalData;
  TTriggerSource = record
    DisplayText: string;
    Data: string;
  end;

  TTriggerData = record
    Event: string;
    Sources: TArray<TTriggerSource>;
    Params: TArray<PTriggerParam>;
    procedure AddSource(const ADisplayText, AData: string); overload; inline;
    procedure AddSource(const AData: string); overload; inline;
    function ExtractParamByType(const dwType: cardinal): PTriggerParam; overload;
    function ExtractParamByType(const dwType: cardinal; out AParam: PTriggerParam): boolean; overload;
    function SourcesToString(const ASep: string = #13): string;
    function ParamsToString(const ASep: string = #13): string;
  end;

//Parses trigger data, extracting its exact action as a string, a set of sources (such as ports
//or devices) to watch for that action, and a set of remaining params.
function ParseTrigger(const ATrigger: PSERVICE_TRIGGER): TTriggerData;


{
Some trigger types have extensible lists of possible GUID values.
We either extract these from the system or allow loading from the text files.
}
var
 //Set by external code on load to point to files to load on first demand
  DeviceInterfaceClassesFile: string = '';
  RpcInterfacesFile: string = '';
  WnfStateNamesFile: string = '';

function GetWellKnownDeviceInterfaceClasses: TGuidDictionary;
function GetWellKnownDeviceInterfaceClassName(const ClassGuid: TGuid): string;

function GetWellKnownRpcInterfaces: TGuidDictionary;

function GetEtwProviders: TGUIDDictionary;
function TryGetEtwProviderName(const Guid: TGuid; out AName: string): boolean;
function GetEtwProviderName(const Guid: TGuid): string; inline;

function GetLocalRpcInterfaces: TGUIDDictionary;
function TryGetLocalRpcInterfaceName(const Guid: TGuid; out AName: string): boolean;
function GetLocalRpcInterfaceName(const Guid: TGuid): string; inline;

function GetWnfStateNames: TWnfSnDict;
function TryGetWnfStateNameInfo(const WnfSn: TWnfStateName; out AInfo: TWnfSnInfo): boolean;
function GetWnfStateNameInfo(const WnfSn: TWnfStateName): TWnfSnInfo; inline;


implementation
uses SysUtils, Classes, Windows, ServiceHelper, SetupApiHelper,
  EtwUtils, {$IFDEF QUERYLOCALRPC}JwaRpc, JwaRpcDce,{$ENDIF} Viper.Log;

procedure TriggerListChanged(Sender: TObject; const AService: string);
var event: TTriggerListEvent;
begin
  for event in OnTriggerListChanged do
    event(Sender, AService);
end;

function TriggerActionToString(const Action: cardinal): string; inline;
begin
  case Action of
    SERVICE_TRIGGER_ACTION_SERVICE_START: Result := sTriggerActionStart;
    SERVICE_TRIGGER_ACTION_SERVICE_STOP: Result := sTriggerActionStop;
  else Result := Format(sTriggerActionOther, [Action]);
  end;
end;

function TriggerDataTypeToStr(const dwDataType: cardinal): string;
begin
  case dwDataType of
  SERVICE_TRIGGER_DATA_TYPE_BINARY: Result := sTriggerDataTypeBinary;
  SERVICE_TRIGGER_DATA_TYPE_STRING: Result := sTriggerDataTypeString;
  SERVICE_TRIGGER_DATA_TYPE_LEVEL: Result := sTriggerDataTypeLevel;
  SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ANY: Result := sTriggerDataTypeKeywordAny;
  SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ALL: Result := sTriggerDataTypeKeywordAll;
  else
    Result := Format(sTriggerDataTypeUnknown, [dwDataType]);
  end;
end;

//Formats the trigger content in the readable way
function TTriggerParamHelper.ToString: string;
begin
  Result := DataTypeToString() + ': ' + ValueToString();
end;

function TTriggerParamHelper.DataTypeToString: string;
begin
  Result := TriggerDataTypeToStr(Self.dwDataType);
end;

//Formats the trigger value in the appropriate way
function TTriggerParamHelper.ValueToString: string;
begin
  case Self.dwDataType of
  SERVICE_TRIGGER_DATA_TYPE_BINARY: Result := Self.HexValue;
  SERVICE_TRIGGER_DATA_TYPE_STRING: Result := Self.StringValue;
  SERVICE_TRIGGER_DATA_TYPE_LEVEL: Result := IntToStr(Self.ByteValue);
  //Keyword filters for ETW
  //TODO: Can be further explained by querying the ETW provider
  SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ANY: Result := IntToStr(Self.Int64Value);
  SERVICE_TRIGGER_DATA_TYPE_KEYWORD_ALL: Result := IntToStr(Self.Int64Value);
  else
    Result := Self.HexValue;
  end;
end;

//Returns the entire contents of the buffer formatted as hex data. Reversible.
function TTriggerParamHelper.HexValue: string;
begin
  Result := string(UniStrUtils.BinToHex(Self.pData, Self.cbData));
end;

//Returns the contents of the buffer treated and formatted as a string value.
//Note that many STRING type entries are in reality MULTISZ and will be reformatted,
//so the result will not be reversible.
function TTriggerParamHelper.StringValue: string;
var list: TStringArray;
  i: integer;
begin
  //Most STRING values are really MULTISZ and we better be prepared
  list := Self.MultistringValue;
  //Concatenate the values with ';', but know this is not the true value
  if Length(list) > 0 then
    Result := list[0];
  for i := 1 to Length(list)-1 do
    Result := Result+';'+list[i];
end;

//Returns the entire contents of the buffer as a raw string value (without any processing).
//Note that many STRING type entries are in reality MULTISZ and contain zeroes.
function TTriggerParamHelper.RawStringValue: string;
begin
  SetLength(Result, Self.cbData div SizeOf(WideChar));
  if Length(Result) > 0 then
    Move(Self.pData, Result[1], Length(Result)*SizeOf(WideChar));
end;

//Returns the entire contents of the buffer treated as a STRING or multistring (MULTISZ) value.
//Handles various edge and malformed cases.
function TTriggerParamHelper.MultistringValue: TStringArray;
begin
  Result := MultiszDecode(PWideChar(Self.pData), Self.cbData div SizeOf(WideChar));
end;

function TTriggerParamHelper.ByteValue: byte;
begin
  Result := PByte(Self.pData)^;
end;

function TTriggerParamHelper.Int64Value: int64;
begin
  Result := PInt64(Self.pData)^;
end;

//Adjusts cbSize and reallocates memory if needed.
//WARNING: Only use if this instance was allocated by us AND is not part of any bigger structure!
procedure TTriggerParamHelper.Resize(const cbNewSize: cardinal);
begin
  if Self.cbData = cbNewSize then exit;
  Self.cbData := cbNewSize;
  ReallocMem(Self.pData, Self.cbData);
end;

procedure TTriggerParamHelper.SetHexValue(const Value: string);
begin
  Resize(Length(Value) div 2);
  UniStrUtils.HexToBin(AnsiString(Value), Self.pData, Self.cbData);
end;

procedure TTriggerParamHelper.SetStringValue(const Value: string);
var list: TStringArray;
begin
 //Tries to automatically detect a converted multistring, but will misinterpret
 //your normal ;s. Use SetRawStringValue if you handle multistrings properly.
  list := SepSplit(Value, ';');
  if Length(list) > 1 then
    Self.SetMultistringValue(list)
  else
    SetRawStringValue(list[0]);
end;

//Copies the raw contents (incl. any #00s) from the given string + the final #00 (implied).
//The final #00 is auto-copied because all supported string data types require #00.
procedure TTriggerParamHelper.SetRawStringValue(const Value: string);
begin
  Resize((Length(Value)+1)*SizeOf(WideChar));
  if Length(Value) > 0 then
    Move(Value[1], Self.pData^, (Length(Value)+1)*SizeOf(WideChar));
end;

procedure TTriggerParamHelper.SetMultistringValue(const Value: TStringArray);
begin
  Self.SetRawStringValue(MultiszEncode(Value));
end;

procedure TTriggerParamHelper.SetByteValue(const Value: byte);
begin
  Resize(1);
  PByte(Self.pData)^ := Value;
end;

procedure TTriggerParamHelper.SetInt64Value(const Value: int64);
begin
  Resize(8);
  PInt64(Self.pData)^ := Value;
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
procedure ParseDeviceInterfaceTrigger(const ATrigger: PSERVICE_TRIGGER; var Result: TTriggerData);
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



function ParseRPCRequestSource(const ASource: string): string;
var srcGuidStr: string;
  srcGuid: TGuid;
  i_pos: integer;
begin
  Result := ASource;

 //RPC sources are sometimes
  i_pos := pos(':', ASource);
  if i_pos > 0 then
    srcGuidStr := copy(ASource, 1, i_pos-1)
  else
    srcGuidStr := ASource;
  try
    srcGuid := StringToGuid('{'+srcGuidStr+'}');
  except
    on EConvertError do begin //can't decode guid
      Result := ASource;
      exit;
    end;
  end;

  if not GetWellKnownRpcInterfaces.TryGetValue(srcGuid, Result)
  and not TryGetLocalRpcInterfaceName(srcGuid, Result) then begin
    Result := ASource;
    exit;
  end;

  if Result = '' then begin
    Result := ASource;
    exit;
  end;

  if i_pos > 0 then
    //Append the unparsed part to the translated string
    Result := Result + ':' + copy(ASource, i_pos+1, MaxInt);
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

  //Mostly DONE.
   SERVICE_TRIGGER_TYPE_DEVICE_INTERFACE_ARRIVAL:
     ParseDeviceInterfaceTrigger(ATrigger, Result);


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
      if ATrigger.pTriggerSubtype^ = RPC_INTERFACE_EVENT_GUID then begin
        Result.Event := sTriggerNetworkEndpointRpc;
        while Result.ExtractParamByType(SERVICE_TRIGGER_DATA_TYPE_STRING, param) do
          Result.AddSource(ParseRpcRequestSource(param.StringValue), param.StringValue);
      end else begin
        if ATrigger.pTriggerSubtype^ = NAMED_PIPE_EVENT_GUID then
          Result.Event := sTriggerNetworkEndpointNamedPipe
        else
          Result.Event := Format(sTriggerNetworkEndpointOther, [GuidToString(ATrigger.pTriggerSubtype^)]);
        //NOTE: From the docs, there are also TCP_PORT_EVENT_GUID and UDP_EVENT_PORT_GUID,
        //   but their values are not documented.

        while Result.ExtractParamByType(SERVICE_TRIGGER_DATA_TYPE_STRING, param) do
          Result.AddSource(param.StringValue);
      end;
    end;

   //
   // The event is a Windows Notification Facility event (WNF State Name)
   //
    SERVICE_TRIGGER_TYPE_CUSTOM_SYSTEM_STATE_CHANGE:
      if ATrigger.pTriggerSubtype^ = CUSTOM_SYSTEM_STATE_CHANGE_EVENT_GUID then begin
        if Result.ExtractParamByType(SERVICE_TRIGGER_DATA_TYPE_BINARY, param) then
          Result.Event := Format(sTriggerSystemStateChangeParam, [GetWnfStateNameInfo(PInt64(param.pData)^).Name])
        else
          Result.Event := Format(sTriggerSystemStateChange, []);
      end else
        Result.Event := Format(sTriggerSystemStateChangeUnusual, [GuidToString(ATrigger.pTriggerSubtype^)]);

   //
   // The event is a custom event generated by an Event Tracing for Windows (ETW) provider.
   //
   // The pTriggerSubtype member specifies the event provider's GUID.
   //
   // The pDataItems member specifies trigger-specific data defined by the provider.
   //
    SERVICE_TRIGGER_TYPE_CUSTOM:
      Result.Event := Format(sTriggerEtwEvent, [GetEtwProviderName(ATrigger.pTriggerSubtype^)]);

   // Not much is known about this type of trigger
    SERVICE_TRIGGER_TYPE_AGGREGATE:
      Result.Event := Format(sTriggerAggregateEvent, [GuidToString(ATrigger.pTriggerSubtype^)]);

  else
    Result.Event := Format('Unusual trigger: %d / %s',
      [ATrigger.dwTriggerType, GuidToString(ATrigger.pTriggerSubtype^)]);
  end;
end;


procedure TTriggerData.AddSource(const ADisplayText, AData: string);
var i: integer;
begin
  i := Length(Self.Sources);
  SetLength(Self.Sources, i+1);
  Self.Sources[i].DisplayText := ADisplayText;
  Self.Sources[i].Data := AData;
end;

procedure TTriggerData.AddSource(const AData: string);
begin
  Self.AddSource(AData, AData);
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
    Result := Result + Self.Sources[i].DisplayText + ASep;
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


{
Well known Device Interface and RPC GUIDs.
Loaded from files on first access. Silently skipped if no files are found.
}

var
  WellKnownDeviceInterfaceClasses: TGuidDictionary = nil;
  WellKnownRpcInterfaces: TGuidDictionary = nil;

procedure __LoadWellKnownDeviceInterfaceClasses;
begin
  WellKnownDeviceInterfaceClasses := TGuidDictionary.Create;
  try
    WellKnownDeviceInterfaceClasses.LoadFromFile(DeviceInterfaceClassesFile);
  except
    on E: EFOpenError do begin end;
  end;
end;

procedure __NeedWellKnownDeviceInterfaceClasses; inline;
begin
  if WellKnownDeviceInterfaceClasses <> nil then exit;
  __LoadWellKnownDeviceInterfaceClasses;
end;

function GetWellKnownDeviceInterfaceClasses: TGuidDictionary;
begin
  __NeedWellKnownDeviceInterfaceClasses;
  Result := WellKnownDeviceInterfaceClasses;
end;

function GetWellKnownDeviceInterfaceClassName(const ClassGuid: TGuid): string;
begin
  __NeedWellKnownDeviceInterfaceClasses;
  if not WellKnownDeviceInterfaceClasses.TryGetValue(ClassGuid, Result) then
    Result := '';
end;


procedure __LoadWellKnownRpcInterfaces;
begin
  WellKnownRpcInterfaces := TGuidDictionary.Create;
  try
    WellKnownRpcInterfaces.LoadFromFile(RpcInterfacesFile);
  except
    on E: EFOpenError do begin end;
  end;
end;

procedure __NeedWellKnownRpcInterfaces; inline;
begin
  if WellKnownRpcInterfaces <> nil then exit;
  __LoadWellKnownRpcInterfaces;
end;

function GetWellKnownRpcInterfaces: TGuidDictionary;
begin
  __NeedWellKnownRpcInterfaces;
  Result := WellKnownRpcInterfaces;
end;


{
ETW providers
Created and populated on first access.
}

var
  EtwProviders: TGuidDictionary = nil;

procedure __PopulateEtwProviders(AList: TGuidDictionary);
var res: cardinal;
  buf: PPROVIDER_ENUMERATION_INFO;
  bufSize, newBufSize: NativeUInt;
  info: PTRACE_PROVIDER_INFO;
  i: integer;
begin
  if not LoadEtw or (@TdhEnumerateProviders = nil) then exit;

  bufSize := 8192; //for starters
  GetMem(buf, bufSize);
  try
    newBufSize := bufSize;
    res := TdhEnumerateProviders(buf, @newBufSize);
    while res = ERROR_INSUFFICIENT_BUFFER do begin
      if newBufSize <= bufSize then
        RaiseLastOsError(); //wtf though
      bufSize := newBufSize;
      ReallocMem(buf, bufSize);
      res := TdhEnumerateProviders(buf, @newBufSize);
    end;
    if res <> 0 then
      RaiseLastOsError();

    for i := 0 to integer(buf.NumberOfProviders)-1 do begin
      info := buf.TraceProviderInfo[i];
      AList.AddOrSetValue(info.ProviderGuid, PWideChar(NativeUInt(buf)+info.ProviderNameOffset));
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure __LoadEtwProviders;
var AProv: TGuidDictionary;
begin
  AProv := TGuidDictionary.Create();
  try
    __PopulateEtwProviders(AProv);
  except
    FreeAndNil(AProv);
    raise;
  end;

  if InterlockedCompareExchangePointer(pointer(EtwProviders), AProv, nil) <> nil then
    FreeAndNil(AProv);
end;

//Inline version for internal consumption
procedure __NeedEtwProviders; inline;
begin
  if EtwProviders <> nil then exit;
  __LoadEtwProviders;
end;

//Exported version - can't be inline
function GetEtwProviders: TGUIDDictionary;
begin
  __NeedEtwProviders;
  Result := EtwProviders;
end;

//Returns ETW provider textual name if available
function TryGetEtwProviderName(const Guid: TGuid; out AName: string): boolean;
begin
  __NeedEtwProviders;
  Result := EtwProviders.TryGetValue(Guid, AName);
end;

//Returns ETW provider name (textual if available, or GUID)
function GetEtwProviderName(const Guid: TGuid): string;
begin
  if not TryGetEtwProviderName(Guid, Result) then
    Result := GuidToString(Guid); //use guid as a name
end;


{
Local RPC interfaces (not all are available).
Created and populated on first access.
}

{$IFDEF QUERYLOCALRPC}
var
  LocalRpcInterfaces: TGuidDictionary = nil;

procedure PopulateLocalRpcInterfaces(AList: TGuidDictionary; AProtocol, AServer: PChar); overload;
var hRpc: RPC_BINDING_HANDLE;
  hInq: RPC_EP_INQ_HANDLE;
  pStringBinding: PWideChar;
  pAnnot: PWideChar;
  ifId: RPC_IF_ID;
begin
  pStringBinding := nil;
  hRPC := nil;
  hInq := nil;
  pAnnot := nil;

  try
    if RpcStringBindingCompose(nil, AProtocol, nil, AServer, nil, &pStringBinding) <> 0 then
      exit;
    if RpcBindingFromStringBinding(pStringBinding, hRpc) <> 0 then
      exit;
    if RpcMgmtEpEltInqBegin(hRpc, RPC_C_EP_ALL_ELTS, nil, 0, nil, hInq) <> 0 then
      exit;

    while RpcMgmtEpEltInqNext(hInq, ifId, nil, nil, pAnnot) = 0 do begin
      AList.AddOrSetValue(ifId.Uuid, pAnnot);
      RpcStringFree(pAnnot);
      pAnnot := nil;
    end;
  finally
    if pAnnot <> nil then RpcStringFree(pAnnot);
    if hInq <> nil then RpcMgmtEpEltInqDone(hInq);
    if hRPC <> nil then RpcBindingFree(hRpc);
    if pStringBinding <> nil then RpcStringFree(pStringBinding);
  end;
end;

var
 //Possible protocol sequence constants:
 //  https://msdn.microsoft.com/en-us/library/windows/desktop/aa374395(v=vs.85).aspx
 //We don't scan some either because they're slow and rare (_http, _mq) or obsolete (_ipx, _spx, etc)
  LocalRpcProtocols: array[0..4] of string = (
    'ncalrpc',       //local procedure call
    'ncacn_ip_tcp',  //tcp
    'ncadg_ip_udp',  //udp
    'ncacn_np',      //named pipes
    'ncacn_nb_tcp'   //netbios over tcp
  );

procedure PopulateLocalRpcInterfaces(AList: TGuidDictionary); overload;
var i: integer;
begin
  for i := 0 to Length(LocalRpcProtocols)-1 do
    PopulateLocalRpcInterfaces(AList, PChar(LocalRpcProtocols[i]), nil);
  Log(intToStr(Alist.Count));
end;

procedure __LoadLocalRpcInterfaces;
var AList: TGuidDictionary;
begin
  AList := TGuidDictionary.Create;
  try
    PopulateLocalRpcInterfaces(AList);
  except
    FreeAndNil(AList);
    raise;
  end;

  if InterlockedCompareExchangePointer(pointer(LocalRpcInterfaces), AList, nil) <> nil then
    FreeAndNil(AList);
end;

procedure LoadLocalRpcInterfaces; inline;
begin
  if LocalRpcInterfaces <> nil then exit;
  __LoadLocalRpcInterfaces;
end;
{$ENDIF}

function GetLocalRpcInterfaces: TGUIDDictionary;
begin
  LoadLocalRpcInterfaces;
  Result := LocalRpcInterfaces;
end;

function TryGetLocalRpcInterfaceName(const Guid: TGuid; out AName: string): boolean;
begin
 {$IFDEF QUERYLOCALRPC}
  LoadLocalRpcInterfaces;
  Result := LocalRpcInterfaces.TryGetValue(Guid, AName);
 {$ELSE}
  Result := false;
 {$ENDIF}
end;

function GetLocalRpcInterfaceName(const Guid: TGuid): string;
begin
  if not TryGetLocalRpcInterfaceName(Guid, Result) then
    Result := '';
end;


{
WNF State Names are used as IDs in CUSTOM_SYSTEM_STATE_CHANGE triggers
}
var
  WnfStateNames: TWnfSnDict = nil;

procedure __LoadWnfStateNames;
begin
  WnfStateNames := TWnfSnDict.Create;
  try
    WnfStateNames.LoadFromFile(WnfStateNamesFile);
  except
    on E: EFOpenError do begin end;
  end;
end;

procedure __NeedWnfStateNames; inline;
begin
  if WnfStateNames <> nil then exit;
  __LoadWnfStateNames;
end;

function GetWnfStateNames: TWnfSnDict;
begin
  __NeedWnfStateNames;
  Result := WnfStateNames;
end;

function TryGetWnfStateNameInfo(const WnfSn: TWnfStateName; out AInfo: TWnfSnInfo): boolean;
begin
  __NeedWnfStateNames;
  Result := WnfStateNames.TryGetValue(WnfSn, AInfo);
end;

function GetWnfStateNameInfo(const WnfSn: TWnfStateName): TWnfSnInfo; inline;
begin
  if not TryGetWnfStateNameInfo(WnfSn, Result) then begin
    Result.Name := string(UniStrUtils.BinToHex(@WnfSn, SizeOf(WnfSn)));
    Result.Desc := '';
  end;
end;



initialization
  OnTriggerListChanged := TList<TTriggerListEvent>.Create;

finalization
 {$IFDEF DEBUG}
  FreeAndNil(OnTriggerListChanged);
 {$IFDEF QUERYLOCALRPC}
  FreeAndNil(LocalRpcInterfaces);
 {$ENDIF}
  FreeAndNil(WellKnownRpcInterfaces);
  FreeAndNil(WellKnownDeviceInterfaceClasses);
  FreeAndNil(EtwProviders);
 {$ENDIF}

end.
