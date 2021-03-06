unit Viper.DependencyList;
{
Implements dependencies and dependents trees for a given service.
Can only host one or the other.
}

interface
{
A driver/service can depend on another driver/service or a group. Groups simply
reference driver/service's assigned group:
  LoadOrderGroup  in SCM's SERVICE_CONFIG.
  Group           in the registry

In the SCM dependency list groups are indicated by SC_GROUP_IDENTIFIER ("+").
In registry they are listed separately:
  DependOnGroup
  DependOnService

There's apparent separation between driver and service groups. Services use one
set of groups, drivers use another.
  Control/GroupOrderList        contains only driver groups
  Control/ServiceGroupOrder     contains both service and driver groups

Some groups are listed in SafeBoot/, always as "type=Driver Group". Most of them
are, but "NetworkProvider" and "TDI" are only used by services and don't appear in
GroupOrderList, hinting that the distinction is superficial.
}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Actions, ActnList, ImgList, VirtualTrees, Viper.ServiceList,
  WinSvc, SvcEntry;

{
The list needs TServiceEntryList from the main form.
Reload the list manually on any changes.
}
type
  TDependencyList = class(TServiceList)
    procedure vtServicesBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure vtServicesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  protected
    FServices: TServiceEntryList;
    function AddBrokenDependencyNode(AParent: PVirtualNode; const AName: string): PVirtualNode;
    function AddServiceDependencyNode(AParent: PVirtualNode; const AService: TServiceEntry): PVirtualNode;
    procedure LoadServiceDependencyNodes(AParentNode: PVirtualNode; const AService: TServiceEntry);
    function AddGroupDependencyNode(AParent: PVirtualNode; const AGroup: string): PVirtualNode;
    procedure LoadGroupDependencyContentNodes(AParentNode: PVirtualNode; const AGroup: string);

    function AddServiceDependentNode(AParent: PVirtualNode; const AService: TServiceEntry): PVirtualNode;
    procedure LoadServiceDependentsNodes(AParentNode: PVirtualNode; const AService: TServiceEntry);
    function AddGroupDependentNode(AParent: PVirtualNode; const AGroup: string): PVirtualNode;
    procedure LoadGroupDependentContentNodes(AParentNode: PVirtualNode; const AGroup: string);
  public
    procedure SetServiceList(AServices: TServiceEntryList);
    procedure ReloadDependencies(const AService: TServiceEntry);
    procedure ReloadDependents(const AService: TServiceEntry);
  end;

  //Dependency group node. Any service from this group is required.
  TSlDependencyGroupNode = class(TSlFolderNode)
  protected
    function GetDisplayName: string; override;
    function GetTypeText: string; override;
  end;

  //A broken dependency node. The service or driver are missing on this system.
  TSlBrokenDependencyNode = class(TSlCustomNode)
  protected
    function GetDisplayName: string; override;
  public
    procedure GetImageIndex(Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer; var ImageList: TCustomImageList); override;
  end;


implementation
uses UITypes, WinApiHelper, ServiceHelper, CommonResources, Viper.StyleSettings;

{$R *.dfm}

resourcestring
  sServiceTypeDependencyGroup = 'Group';
    //Type text for the dependency group entry in service list
    //Windows Registry calls these "Driver Groups" (Control/SafeBoot) even when they
    //are service groups.

function TSlDependencyGroupNode.GetDisplayName: string;
begin
  Result := Self.FName;
end;

function TSlDependencyGroupNode.GetTypeText: string;
begin
  Result := sServiceTypeDependencyGroup;
end;


resourcestring
  sServiceMissing = 'Dependency missing';

function TSlBrokenDependencyNode.GetDisplayName: string;
begin
  Result := Format(sServiceMissing, [Self.Name]);
end;

procedure TSlBrokenDependencyNode.GetImageIndex(Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
begin
  if (Column <> NoColumn) and (Column <> TServiceList.colServiceName) then
    exit;
  case Kind of
    ikNormal, ikSelected: begin
      //We don't really know if it's a service or a driver! Tentatively show it as a service.
      ImageList := CommonRes.ilImages;
      ImageIndex := CommonRes.iBrokenDependency;
      Ghosted := true; //try this for now
    end;
  end;
end;


procedure TDependencyList.SetServiceList(AServices: TServiceEntryList);
begin
  Self.FServices := AServices;
end;

procedure TDependencyList.vtServicesBeforeItemErase(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
  var ItemColor: TColor; var EraseAction: TItemEraseAction);
var BaseData: TObject;
  BrokenDepData: TSlBrokenDependencyNode;
  Style: TPaintStyle;
begin
  inherited;
  BaseData := TObject(Sender.GetNodeData(Node)^);
  if BaseData is TSlBrokenDependencyNode then begin
    BrokenDepData := TSlBrokenDependencyNode(BaseData);

    if aUseColors.Checked then begin
      Style.BgColor := clWindow;
      StyleSettingsForm.GetStyleForStartType(SERVICE_DISABLED, Style, [seBgColor]);
      ItemColor := Style.BgColor;
      if ItemColor <> clWindow then
        EraseAction := eaColor;
    end;
  end;
end;

procedure TDependencyList.vtServicesPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var BaseData: TObject;
  BrokenDepData: TSlBrokenDependencyNode;
  Style: TPaintStyle;
begin
  inherited;
  BaseData := TObject(Sender.GetNodeData(Node)^);
  if BaseData is TSlBrokenDependencyNode then begin
    BrokenDepData := TSlBrokenDependencyNode(BaseData);

    if aUseColors.Checked then begin
      Style.Font := TargetCanvas.Font; //by reference
      StyleSettingsForm.GetStyleForStartType(SERVICE_DISABLED, Style, [seFont]);
    end;
  end;
end;


//Loads the dependency tree for the given service
procedure TDependencyList.ReloadDependencies(const AService: TServiceEntry);
begin
  Self.BeginUpdate;
  try
    Self.Clear;
    if AService <> nil then
      LoadServiceDependencyNodes(nil, AService);
  finally
    Self.EndUpdate;
  end;
end;

//Adds a node for a broken dependency/dependent
function TDependencyList.AddBrokenDependencyNode(AParent: PVirtualNode; const AName: string): PVirtualNode;
var broken: TSlBrokenDependencyNode;
begin
  broken := TSlBrokenDependencyNode.Create(AName);
  broken.AutoDestroy := true;
  Result := Self.AddNode(AParent, broken);
end;

//Adds a service node with its further dependencies
function TDependencyList.AddServiceDependencyNode(AParent: PVirtualNode; const AService: TServiceEntry): PVirtualNode;
begin
  Result := Self.AddService(AParent, AService);
  LoadServiceDependencyNodes(Result, AService);
end;

//Adds child nodes for service dependencies to the node given by ParentNode.
//Call ReloadDependencies if you need to reload the whole list.
procedure TDependencyList.LoadServiceDependencyNodes(AParentNode: PVirtualNode; const AService: TServiceEntry);
var deps: TArray<string>;
  dep: string;
  depService: TServiceEntry;
begin
  if (AService = nil) or (AService.Config = nil) then
    exit;

  deps := SplitNullSeparatedList(AService.Config.lpDependencies);
  for dep in deps do begin
    if dep.StartsWith(SC_GROUP_IDENTIFIER) then begin
      //This is a dependency group, not a service name.
      AddGroupDependencyNode(AParentNode, Copy(dep, 2, MaxInt));
      continue;
    end;

    depService := FServices.Find(dep);
   //Dependencies sometimes refer to drivers, so we either have to always load drivers
   //(as we do now), or to ignore failed matches.

   //Dependencies do sometimes contain failed matches, we should not crash on that!
   //Show that as a valid, but broken, dependency.
    if depService = nil then begin
      AddBrokenDependencyNode(AParentNode, dep);
      continue;
    end;

    AddServiceDependencyNode(AParentNode, depService);
  end;
end;

//Adds a group dependency with its entry list
function TDependencyList.AddGroupDependencyNode(AParent: PVirtualNode; const AGroup: string): PVirtualNode;
var group: TSlDependencyGroupNode;
begin
  group := TSlDependencyGroupNode.Create(AGroup);
  group.AutoDestroy := true;
  Result := Self.AddNode(AParent, group);
  Self.LoadGroupDependencyContentNodes(Result, group.Name);
end;

//Adds child nodes for all services in a given group to the node given by ParentNode.
//Used to populate a dependency group node
procedure TDependencyList.LoadGroupDependencyContentNodes(AParentNode: PVirtualNode; const AGroup: string);
var service: TServiceEntry;
begin
  if AGroup = '' then exit;

  //We could ask SCM but it's quicker to check our own list. Saves us opening
  //SCM on each call too or passing the handle
  for service in FServices do
    if SameText(service.LoadOrderGroup, AGroup) then
      AddServiceDependencyNode(AParentNode, service);
end;


//Loads the dependents tree for the given service
procedure TDependencyList.ReloadDependents(const AService: TServiceEntry);
begin
  if AService = nil then begin
    Self.Clear;
    exit;
  end;

  Self.BeginUpdate;
  try
    Self.Clear;
    LoadServiceDependentsNodes(nil, AService);
  finally
    Self.EndUpdate;
  end;
end;

function TDependencyList.AddServiceDependentNode(AParent: PVirtualNode; const AService: TServiceEntry): PVirtualNode;
begin
  Result := Self.AddService(AParent, AService);
  LoadServiceDependentsNodes(Result, AService);
end;

procedure TDependencyList.LoadServiceDependentsNodes(AParentNode: PVirtualNode; const AService: TServiceEntry);
var depService: TServiceEntry;
begin
  if AService = nil then exit;

  //We could ask SCM but it's quicker to check our own list. Saves us opening
  //SCM on each call too or passing the handle
  for depService in FServices do
    if depService.DependsOn(AService.ServiceName) then
      AddServiceDependentNode(AParentNode, depService);

  //Add the group to which this service belongs -- someone may depend on that too
  AddGroupDependentNode(AParentNode, AService.LoadOrderGroup);
end;

function TDependencyList.AddGroupDependentNode(AParent: PVirtualNode; const AGroup: string): PVirtualNode;
var group: TSlDependencyGroupNode;
  depService: TServiceEntry;
  Found: boolean;
begin
  //We're going to add this node only if someone depends on it.
  //For most groups this is not true so no point in showing them
  Found := false;
  for depService in FServices do
    if depService.DependsOn(SC_GROUP_IDENTIFIER+AGroup) then begin
      Found := true;
      break;
    end;
  if not Found then begin
    Result := nil;
    exit;
  end;

  group := TSlDependencyGroupNode.Create(AGroup);
  group.AutoDestroy := true;
  Result := Self.AddNode(AParent, group);
  Self.LoadGroupDependencyContentNodes(Result, group.Name);
end;

procedure TDependencyList.LoadGroupDependentContentNodes(AParentNode: PVirtualNode; const AGroup: string);
var depService: TServiceEntry;
begin
  if AGroup = '' then exit;

  //We could ask SCM but it's quicker to check our own list. Saves us opening
  //SCM on each call too or passing the handle
  for depService in FServices do
    if depService.DependsOn(SC_GROUP_IDENTIFIER+AGroup) then
      AddServiceDependentNode(AParentNode, depService);
end;


end.
