unit EtwUtils;
// Event Tracing for Windows

interface
uses Windows;

const
  TDH_DLL = 'tdh.dll';

type
  TRACE_PROVIDER_INFO = packed record
    ProviderGuid: TGUID;
    SchemaSource: ULONG;
    ProviderNameOffset: ULONG;
  end;
  PTRACE_PROVIDER_INFO = ^TRACE_PROVIDER_INFO;

  PROVIDER_ENUMERATION_INFO = packed record
    NumberOfProviders: ULONG;
    Padding: ULONG;
    function GetTraceProviderInfo(const Index: integer): PTRACE_PROVIDER_INFO; inline;
    property TraceProviderInfo[const Index: integer]: PTRACE_PROVIDER_INFO read GetTraceProviderInfo;
  end;
  PPROVIDER_ENUMERATION_INFO = ^PROVIDER_ENUMERATION_INFO;

type
  TTdhEnumerateProviders = function(pBuffer: PPROVIDER_ENUMERATION_INFO; pBufferSize: PULONG): ULONG; stdcall;

var
  TdhEnumerateProviders: TTdhEnumerateProviders = nil;

function LoadEtw: boolean;

implementation

function PROVIDER_ENUMERATION_INFO.GetTraceProviderInfo(const Index: integer): PTRACE_PROVIDER_INFO;
begin
  Result := PTRACE_PROVIDER_INFO(NativeUInt(@Self)+SizeOf(Self)+Index*SizeOf(TRACE_PROVIDER_INFO));
end;

var
  hTdhDll: HMODULE = 0;

function LoadEtw: boolean;
var hLib: HMODULE;
begin
  if hTdhDll <> 0 then begin
    Result := true;
    exit;
  end;

  hLib := LoadLibrary(TDH_DLL);
  if hLib = 0 then begin
    Result := false;
    exit;
  end;

  //Load functions.
  //It's not a problem if someone is loading these in parallel: the values will be the same
  TdhEnumerateProviders := GetProcAddress(hlib, 'TdhEnumerateProviders');

{$IFDEF WIN64}
  if InterlockedCompareExchange64(NativeUInt(hTdhDll), hLib, 0) <> 0 then
{$ELSE}
  if InterlockedCompareExchange(Integer(hTdhDll), hLib, 0) <> 0 then
{$ENDIF}
    FreeLibrary(hLib);
  Result := true;
end;

end.
