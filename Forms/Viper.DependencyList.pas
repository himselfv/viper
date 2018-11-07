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
    procedure LoadServiceDependentsNodes(hSC: SC_HANDLE; const AService: TServiceEntry; AParentNode: PVirtualNode);
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
  sServiceMissing = '%s (missing)';

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
      ImageIndex := CommonRes.iService;
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
var hSC: SC_HANDLE;
begin
  if AService = nil then begin
    Self.Clear;
    exit;
  end;

  hSC := 0;
  Self.BeginUpdate;
  try
    Self.Clear;
    hSC := OpenSCManager(SC_MANAGER_CONNECT or SC_MANAGER_ENUMERATE_SERVICE);
    LoadServiceDependentsNodes(hSC, AService, nil);

  finally
    if hSC <> 0 then CloseServiceHandle(hSC);
    Self.EndUpdate;
  end;
end;

procedure TDependencyList.LoadServiceDependentsNodes(hSC: SC_HANDLE; const AService: TServiceEntry; AParentNode: PVirtualNode);
var hSvc: SC_HANDLE;
  depnt: LPENUM_SERVICE_STATUS;
  depntCount: cardinal;
  depService: TServiceEntry;
  depNode: PVirtualNode;
begin
  if AService = nil then exit;

  hSvc := OpenService(hSC, AService.ServiceName, SERVICE_ENUMERATE_DEPENDENTS);
  if hSvc = 0 then exit;
  try
    depnt := EnumDependentServices(hSvc, SERVICE_STATE_ALL, depntCount);
    if depnt = nil then exit;

    while depntCount > 0 do begin
      depService := FServices.Find(depnt.lpServiceName);

     //Weird: SCM gives us a valid service but we don't know it.
     //Maybe our list is stale. Not crashing is still better - let's reuse the broken dependency mechanics.
      if depService = nil then begin
        AddBrokenDependencyNode(AParentNode, depnt.lpServiceName);
        continue;
      end;

      depNode := Self.AddService(AParentNode, depService);
      LoadServiceDependentsNodes(hSC, depService, depNode);

      Inc(depnt);
      Dec(depntCount);
    end;
  finally
    CloseServiceHandle(hSvc);
  end;
end;


end.
