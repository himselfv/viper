unit Viper.TriggerEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, WinSvc, ServiceHelper, TriggerUtils, ComCtrls,
  Vcl.Samples.Spin, ExtCtrls, VirtualTrees;

type
  TDataEntryNodeData = record
    Item: SERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
    ValueText: string;
    TypeText: string;
  end;
  PDataEntryNodeData = ^TDataEntryNodeData;

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
    procedure vtDataEntriesGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure vtDataEntriesInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure vtDataEntriesFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure vtDataEntriesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  protected
    FTrigger: PSERVICE_TRIGGER;

  protected
    FTypePresetsLoaded: boolean;
    procedure ReloadTypePresets;
    function SelectTypePreset(const Preset: pointer; NotifyChanged: boolean = true): boolean;

  protected
    procedure ReloadDataEntries;

  protected
    FDeviceInterfacesLoaded: boolean;
    FDeviceInterfaces: TArray<TGUID>;
    FEtwSourcesLoaded: boolean;
    FEtwSources: TArray<TGUID>;
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
  PTypePreset = ^TTypePreset;

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

  TYPE_PRESETS: array[0..13] of TTypePreset = (
    (t: SERVICE_TRIGGER_TYPE_DEVICE_INTERFACE_ARRIVAL;
    st: nil;
     d: sTriggerDeviceInterfaceAvailableConst;
     p: PP_DEVICETYPE),

    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_FIRST_IP_ADDRESS_ARRIVAL_GUID;
     d: sTriggerIpFirstAvailable;
     p: PP_NONE),
    (t: SERVICE_TRIGGER_TYPE_IP_ADDRESS_AVAILABILITY;
    st: @NETWORK_MANAGER_LAST_IP_ADDRESS_REMOVAL_GUID;
     d: sTriggerIpLastLost;
     p: PP_NONE),

    (t: SERVICE_TRIGGER_TYPE_DOMAIN_JOIN;
    st: @DOMAIN_JOIN_GUID;
     d: sTriggerDomainJoin;
     p: PP_NONE),
    (t: SERVICE_TRIGGER_TYPE_DOMAIN_JOIN;
    st: @DOMAIN_LEAVE_GUID;
     d: sTriggerDomainLeave;
     p: PP_NONE),

    (t: SERVICE_TRIGGER_TYPE_FIREWALL_PORT_EVENT;
    st: @FIREWALL_PORT_OPEN_GUID;
     d: sTriggerFirewallPortOpen;
     p: PP_FIREWALLPORTS),
    (t: SERVICE_TRIGGER_TYPE_FIREWALL_PORT_EVENT;
    st: @FIREWALL_PORT_CLOSE_GUID;
     d: sTriggerFirewallPortClose;
     p: PP_FIREWALLPORTS),

    (t: SERVICE_TRIGGER_TYPE_GROUP_POLICY;
    st: @MACHINE_POLICY_PRESENT_GUID;
     d: sTriggerGroupPolicyChangedMachine;
     p: PP_NONE),
    (t: SERVICE_TRIGGER_TYPE_GROUP_POLICY;
    st: @USER_POLICY_PRESENT_GUID;
     d: sTriggerGroupPolicyChangedUser;
     p: PP_NONE),

    (t: SERVICE_TRIGGER_TYPE_NETWORK_ENDPOINT;
    st: @RPC_INTERFACE_EVENT_GUID;
     d: sTriggerNetworkEndpointRpc;
     p: PP_NONE),
    (t: SERVICE_TRIGGER_TYPE_NETWORK_ENDPOINT;
    st: @NAMED_PIPE_EVENT_GUID;
     d: sTriggerNetworkEndpointNamedPipe;
     p: PP_NONE),

    (t: SERVICE_TRIGGER_TYPE_CUSTOM_SYSTEM_STATE_CHANGE;
    st: nil;
     d: sTriggerSystemStateChange;
     p: PP_GENERICSUBTYPE),

    (t: SERVICE_TRIGGER_TYPE_CUSTOM;
    st: nil;
     d: sTriggerEtwEventConst;
     p: PP_ETWEVENT),

    (t: SERVICE_TRIGGER_TYPE_AGGREGATE;
    st: nil;
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

const
  //Additional type preset for "no preset" (fully customizable values)
  CustomTypePreset: TTypePreset =
    (t: 0;
     st: nil;
     d: sCustomTypePreset;
     p: PP_GENERIC);

//Populates type preset selection combobox
procedure TTriggerEditorForm.ReloadTypePresets;
var i: integer;
  oldPreset: pointer;
begin
  //Associated object field stores the pointer to the TTypePreset.

  if cbTypePreset.ItemIndex >= 0 then
    oldPreset := cbTypePreset.Items.Objects[cbTypePreset.ItemIndex]
  else
    oldPreset := nil;

  cbTypePreset.Clear;
  for i := Low(TYPE_PRESETS) to High(TYPE_PRESETS) do
    cbTypePreset.AddItem(TYPE_PRESETS[i].d, TObject(@TYPE_PRESETS[i]));
  cbTypePreset.AddItem(sCustomTypePreset, TObject(@CustomTypePreset));
  FTypePresetsLoaded := true;

  //Re-select old preset but do not notify of change
  SelectTypePreset(oldPreset, false);
end;

//Selects the type preset in the drop down list by the given PTypePreset pointer (nil = select nothing).
//If NotifyChanged is true, notifies the control's OnChanged handler.
//Returns false if no entry with this Preset has been found (should not happen).
function TTriggerEditorForm.SelectTypePreset(const Preset: pointer; NotifyChanged: boolean = true): boolean;
var i: integer;
begin
  if Preset = nil then begin
    cbTypePreset.ItemIndex := -1;
    Result := true;
  end else begin
    Result := false;
    for i := 0 to cbTypePreset.Items.Count-1 do
      if cbTypePreset.Items.Objects[i] = Preset then begin
        cbTypePreset.ItemIndex := i;
        Result := true;
        break;
      end;
  end;
  if Result and NotifyChanged then
    if Assigned(cbTypePreset.OnChange) then
      cbTypePreset.OnChange(cbTypePreset);
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
var i: integer;
  typePreset: PTypePreset;
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
  typePreset := nil;
  if FTrigger <> nil then begin
   //Find the most appropriate match
    for i := Low(TYPE_PRESETS) to High(TYPE_PRESETS) do
      if (TYPE_PRESETS[i].t=FTrigger.dwTriggerType)
      and ((TYPE_PRESETS[i].st=nil) or ((FTrigger.pTriggerSubtype<>nil) and (TYPE_PRESETS[i].st^=FTrigger.pTriggerSubtype^))) then begin
        typePreset := @TYPE_PRESETS[i];
        break;
      end;
    if typePreset = nil then
      typePreset := @CustomTypePreset;
  end;
  SelectTypePreset(typePreset, true);

  ReloadDataEntries;
end;

procedure TTriggerEditorForm.cbTypePresetChange(Sender: TObject);
var preset: PTypePreset;

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

  if cbTypePreset.ItemIndex < 0 then begin
    //No preset selected, this is the default state for new triggers - show nothing
    pcPresetDetails.ActivePage := nil;
    exit;
  end;

  preset := PTypePreset(cbTypePreset.Items.Objects[cbTypePreset.ItemIndex]);
  if preset = @CustomTypePreset then begin
   //Custom handling for "No preset" - enable the type editing
    pcPresetDetails.ActivePage := tsPresetGeneric;
    SetGenericCustomTypeEnabled(true);
    exit;
  end;

  //Otherwise work by data stored in a TYPE_PRESET

  //Configure the generic page in case we switch to it later
   edtCustomType.Value := preset.t;
   if preset.st <> nil then
     edtCustomSubtype.Text := GuidToString(preset.st^)
   else
     edtCustomSubtype.Text := '';

   //Choose the appropriate propety page (maybe not generic)
   case preset.p of
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


// Data Entries / Properties

procedure TTriggerEditorForm.ReloadDataEntries;
var item: PSERVICE_TRIGGER_SPECIFIC_DATA_ITEM;
  itemCnt: cardinal;
  node: PVirtualNode;
  nodeData: PDataEntryNodeData;
begin
  vtDataEntries.Clear;
  if FTrigger = nil then exit;

  itemCnt := FTrigger.cDataItems;
  item := FTrigger.pDataItems;
  while itemCnt > 0 do begin
    node := vtDataEntries.AddChild(nil);
    vtDataEntries.ReinitNode(node, false);
    nodeData := vtDataEntries.GetNodeData(node);
    nodeData.Item := CopyTriggerDataItem(item^);

    nodeData.TypeText := item^.DataTypeToString();
    nodeData.ValueText := item^.ValueToString();

    Inc(item);
    Dec(itemCnt);
  end;
end;

procedure TTriggerEditorForm.vtDataEntriesGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TDataEntryNodeData);
end;

procedure TTriggerEditorForm.vtDataEntriesInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
  var Data: PDataEntryNodeData;
begin
  Data := Sender.GetNodeData(Node);
  Initialize(Data^);
  Data^.Item.pData := nil;
end;

procedure TTriggerEditorForm.vtDataEntriesFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var Data: PDataEntryNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if Data.Item.pData <> nil then begin
    FreeStandaloneTriggerDataItem(Data.Item);
    Data.Item.pData := nil;
  end;
  Finalize(Data^);
end;

procedure TTriggerEditorForm.vtDataEntriesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var Data: PDataEntryNodeData;
begin
  if not (TextType in [ttNormal, ttStatic]) then exit;
  Data := Sender.GetNodeData(Node);
  case Column of
   0, NoColumn: CellText := Data.ValueText;
   1: CellText := Data.TypeText;
  end;
end;


end.