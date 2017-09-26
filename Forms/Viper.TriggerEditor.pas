unit Viper.TriggerEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, WinSvc, ServiceHelper, TriggerUtils, Vcl.ComCtrls,
  Vcl.Samples.Spin, Vcl.ExtCtrls, VirtualTrees;

type
  TTriggerEditorForm = class(TForm)
    cbAction: TComboBox;
    lblActionToType: TLabel;
    cbTypePreset: TComboBox;
    pcPresetDetails: TPageControl;
    tsPresetGeneric: TTabSheet;
    tsPresetDevice: TTabSheet;
    tsPresetETW: TTabSheet;
    lblCustomType: TLabel;
    lblCustomSubtype: TLabel;
    edtCustomSubtype: TEdit;
    edtCustomType: TSpinEdit;
    cbDeviceInterfaceClass: TComboBox;
    lblDeviceInterfaceClass: TLabel;
    lblEtwEventSource: TLabel;
    cbEtwEventSource: TComboBox;
    pnlData: TPanel;
    lblDataCaption: TLabel;
    vtDataEntries: TVirtualStringTree;
    btnOk: TButton;
    btnCancel: TButton;
    btnDataEntryAdd: TButton;
    btnDataEntryEdit: TButton;
    btnDataEntryDelete: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cbTypePresetChange(Sender: TObject);
  protected
    FTrigger: PSERVICE_TRIGGER;
    FTypePresetsLoaded: boolean;
    FDeviceInterfacesLoaded: boolean;
    FDeviceInterfaces: TArray<TGUID>;
    FEtwSourcesLoaded: boolean;
    FEtwSources: TArray<TGUID>;
    procedure ReloadTypePresets;
    procedure ReloadData;
    procedure UpdatePresetDevicePage;
    procedure UpdatePresetEtwPage;
  public
    function EditTrigger(var Data: PSERVICE_TRIGGER): TModalResult;
    function TriggerData: PSERVICE_TRIGGER;
    procedure SetTriggerData(const Data: PSERVICE_TRIGGER);

  end;

var
  TriggerEditorForm: TTriggerEditorForm;

type
  TTypePreset = record
    t: DWORD;   //type
    st: PGUID;  //subtype
    d: string;  //description
    p: integer; //properties page type
  end;

const
 //Available propety pages
  PP_NONE                  = 0; //no additional properties
  PP_GENERIC               = 1; //generic type/subtype input + properties
  PP_GENERICSUBTYPE        = 2; //generic subtype input + properties
  PP_DEVICETYPE            = 11; //device type selection (for device interface availability)
  PP_FIREWALLPORTS         = 12; //firewall ports selection
  PP_ETWEVENT              = 13; //ETW event (category selection)

const
 //The presets listed in this table will be presented in the nice neat way,
 //all unhandled combinations will be given generic treatment.

  TYPE_PRESETS: array[0..11] of TTypePreset = (
    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerDeviceInterfaceAvailableConst;
     p: PP_DEVICETYPE),

    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerIpFirstAvailable;
     p: PP_NONE),
    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerIpLastLost;
     p: PP_NONE),

    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerDomainJoin;
     p: PP_NONE),
    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerDomainLeave;
     p: PP_NONE),

    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerFirewallPortOpen;
     p: PP_FIREWALLPORTS),
    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerFirewallPortClose;
     p: PP_FIREWALLPORTS),

    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerGroupPolicyChangedMachine;
     p: PP_NONE),
    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerGroupPolicyChangedUser;
     p: PP_NONE),

    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerSystemStateChange;
     p: PP_GENERICSUBTYPE),

    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerEtwEventConst;
     p: PP_ETWEVENT),

    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerAggregateEventConst;
     p: PP_GENERICSUBTYPE)
  );

implementation
uses UITypes, GuidDict;

{$R *.dfm}

procedure TTriggerEditorForm.FormCreate(Sender: TObject);
begin
  FTrigger := nil;
  FTypePresetsLoaded := false;
  FDeviceInterfacesLoaded := false;
  FEtwSourcesLoaded := false;
end;

procedure TTriggerEditorForm.FormDestroy(Sender: TObject);
begin
  if FTrigger <> nil then
   //Free our own copy of trigger data
    SetTriggerData(nil);
end;

procedure TTriggerEditorForm.FormShow(Sender: TObject);
begin
  ReloadTypePresets;
end;

resourcestring
  sCustomTypePreset = 'Custom condition';

//Populates type preset selection combobox
procedure TTriggerEditorForm.ReloadTypePresets;
var i, oldId: integer;
begin
  //Associated object field stores the index into the TYPE_PRESETS table.
  //None selected == -1
  //Custom preset == -2

  if cbTypePreset.ItemIndex >= 0 then
    oldId := integer(cbTypePreset.Items.Objects[cbTypePreset.ItemIndex])
  else
    oldId := -1;

  cbTypePreset.Clear;
  for i := Low(TYPE_PRESETS) to High(TYPE_PRESETS) do
    cbTypePreset.AddItem(TYPE_PRESETS[i].d, TObject(i));
  cbTypePreset.AddItem(sCustomTypePreset, TObject(-2));

  for i := 0 to cbTypePreset.Items.Count-1 do
    if integer(cbTypePreset.Items.Objects[i]) = oldId then begin
      cbTypePreset.ItemIndex := i;
      break;
    end;

  FTypePresetsLoaded := true;
end;

//Provides access to a copy of trigger data stored in this form.
//You MUST copy it with CopyTrigger if you want to continue using it.
function TTriggerEditorForm.TriggerData: PSERVICE_TRIGGER;
begin
  Result := FTrigger;
end;

//Lets this form copy trigger data. The source data remains in your custody.
procedure TTriggerEditorForm.SetTriggerData(const Data: PSERVICE_TRIGGER);
begin
 //Free any existing internal data
  if Self.FTrigger <> nil then begin
    FreeMem(Self.FTrigger);
    Self.FTrigger := nil;
  end;

  if Data <> nil then
    FTrigger := CopyTrigger(Data^);
  ReloadData;
end;

//Helper function. Opens a modal window where lets the user edit the provided trigger data.
//FREES YOUR POINTER and returns a DIFFERENT POINTER with the resulting data,
//or nothing if the user cancels the operation.
//Pass nil to edit a new trigger.
function TTriggerEditorForm.EditTrigger(var Data: PSERVICE_TRIGGER): TModalResult;
begin
  Self.SetTriggerData(Data);
  Result := Self.ShowModal;
  if IsPositiveResult(Result) then begin
    if Data <> nil then
      FreeMem(Data);
    Data := CopyTrigger(Self.TriggerData^);
  end;
end;

procedure TTriggerEditorForm.btnOkClick(Sender: TObject);
begin
 //TODO: Save all changes into FTrigger structure.
  ModalResult := mrOk;
end;

//Reloads all data on this form from the stored trigger data.
procedure TTriggerEditorForm.ReloadData;
begin
  //Action
  if (FTrigger = nil) or (FTrigger.dwAction = SERVICE_TRIGGER_ACTION_SERVICE_START) then
    cbAction.ItemIndex := 0
  else
  if FTrigger.dwAction = SERVICE_TRIGGER_ACTION_SERVICE_STOP then
    cbAction.ItemIndex := 1
  else
    //That's the only custom thing we don't support right now
    raise Exception.CreateFmt('Unsupported service action: %d', [FTrigger.dwAction]);

  //This may be called before type presets are automatically loaded
  if not FTypePresetsLoaded then
    ReloadTypePresets;

  //Select the type preset
  cbTypePreset.ItemIndex := -1; //for now
  cbTypePresetChange(cbTypePreset); //wouldn't be called automatically


end;

procedure TTriggerEditorForm.cbTypePresetChange(Sender: TObject);
var idx: integer;

  //Custom type edit on the generic details page can be enabled or disabled
  //depending on the preset type.
  procedure SetGenericCustomTypeEnabled(const Value: boolean);
  begin
    edtCustomType.Enabled := Value;
    edtCustomType.Visible := Value;
    lblCustomType.Enabled := Value;
    lblCustomType.Visible := Value;
  end;

begin
  //TODO: Shouldn't we save current page details? E.g. we've changed the device type
  //on one page, we should see the guid on the generic page!

  idx := cbTypePreset.ItemIndex;
  if idx < 0 then begin
    //No preset selected, this is the default state for new triggers - show nothing
    pcPresetDetails.ActivePage := nil;
    exit;
  end;

  //Henceforth choose by TYPE_PRESETS index
  idx := integer(cbTypePreset.Items.Objects[idx]);
  if idx < 0 then begin
   //Custom preset
    pcPresetDetails.ActivePage := tsPresetGeneric;
    SetGenericCustomTypeEnabled(true);
    exit;
  end;

  //Otherwise work by data stored in a TYPE_PRESET

  //Configure the generic page in case we switch to it later
   edtCustomType.Value := TYPE_PRESETS[idx].t;
   if TYPE_PRESETS[idx].st <> nil then
     edtCustomSubtype.Text := GuidToString(TYPE_PRESETS[idx].st^)
   else
     edtCustomSubtype.Text := '';

   //Choose the appropriate propety page (maybe not generic)
   case TYPE_PRESETS[idx].p of
   PP_NONE: pcPresetDetails.ActivePage := nil;
   PP_GENERICSUBTYPE: begin
     pcPresetDetails.ActivePage := tsPresetGeneric;
     SetGenericCustomTypeEnabled(false);
   end;
   PP_DEVICETYPE: begin
     pcPresetDetails.ActivePage := tsPresetDevice;
     //update the page configuration as the guid could have changed
     UpdatePresetDevicePage();
   end;
   PP_FIREWALLPORTS: begin
    //No additional details except for properties
     pcPresetDetails.ActivePage := nil;
   end;
   PP_ETWEVENT: begin
     pcPresetDetails.ActivePage := tsPresetEtw;
     //update the page configuration as the guid could have changed
     UpdatePresetEtwPage();
   end;
   else //PP_GENERIC and everything undefined
     pcPresetDetails.ActivePage := tsPresetGeneric;
     SetGenericCustomTypeEnabled(true);
   end;
end;

//Actualizes the information on the PresetDevice page
procedure TTriggerEditorForm.UpdatePresetDevicePage;
var classes: TGUIDDictionary;
  i: integer;
  pg: PGUID;
  found: boolean;
begin
  classes := GetWellKnownDeviceInterfaceClasses;

 //Load the Device Interface List on first access
  if not Self.FDeviceInterfacesLoaded then begin
    cbDeviceInterfaceClass.Clear;
    //TGuidDictionary is not indexed, so we need to store some indexed array locally
    FDeviceInterfaces := classes.Keys.ToArray();
    //Associated object points to the key guid
    for i := 0 to Length(FDeviceInterfaces)-1 do begin
      pg := @FDeviceInterfaces[i];
      cbDeviceInterfaceClass.AddItem(classes[pg^], TObject(pg));
    end;
    cbDeviceInterfaceClass.Sorted := true;
    Self.FDeviceInterfacesLoaded := true;
  end;

 //New triggers start with no data
  if FTrigger = nil then begin
    cbDeviceInterfaceClass.ItemIndex := -1;
    cbDeviceInterfaceClass.Text := '';
    exit;
  end;

 //Select the value based on the data we have
  found := false;
  for i := 0 to cbDeviceInterfaceClass.Items.Count-1 do begin
    pg := PGUID(cbDeviceInterfaceClass.Items.Objects[i]);
    if FTrigger.pTriggerSubtype^ = pg^ then begin
      cbDeviceInterfaceClass.ItemIndex := i;
      found := true;
      break;
    end;
  end;

  if not found then begin
    cbDeviceInterfaceClass.ItemIndex := -1;
    cbDeviceInterfaceClass.Text := GuidToString(FTrigger.pTriggerSubtype^);
  end;
end;


procedure TTriggerEditorForm.UpdatePresetEtwPage;
var providers: TGUIDDictionary;
  i: integer;
  pg: PGUID;
  found: boolean;
begin
  providers := GetEtwProviders;

 //Load the ETW Source List on first access
  if not Self.FEtwSourcesLoaded then begin
    cbEtwEventSource.Clear;
    //TGuidDictionary is not indexed, so we need to store some indexed array locally
    FEtwSources := providers.Keys.ToArray();
    //Associated object points to the key guid
    for i := 0 to Length(FEtwSources)-1 do begin
      pg := @FEtwSources[i];
      cbEtwEventSource.AddItem(providers[pg^], TObject(pg));
    end;
    cbEtwEventSource.Sorted := true;
    Self.FEtwSourcesLoaded := true;
  end;

 //New triggers start with no data
  if FTrigger = nil then begin
    cbEtwEventSource.ItemIndex := -1;
    cbEtwEventSource.Text := '';
    exit;
  end;

 //Select the value based on the data we have
  found := false;
  for i := 0 to cbEtwEventSource.Items.Count-1 do begin
    pg := PGUID(cbEtwEventSource.Items.Objects[i]);
    if FTrigger.pTriggerSubtype^ = pg^ then begin
      cbEtwEventSource.ItemIndex := i;
      found := true;
      break;
    end;
  end;

  if not found then begin
    cbEtwEventSource.ItemIndex := -1;
    cbEtwEventSource.Text := GuidToString(FTrigger.pTriggerSubtype^);
  end;
end;


end.
