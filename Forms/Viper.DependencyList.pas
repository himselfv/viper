unit Viper.DependencyList;
{
Implements dependencies and dependents trees for a given service.
Can only host one or the other.
}

interface

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
    procedure LoadServiceDependencyNodes(const AService: TServiceEntry; AParentNode: PVirtualNode);
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
      LoadServiceDependencyNodes(AService, nil);
  finally
    Self.EndUpdate;
  end;
end;

resourcestring
  eServiceDependentNotFound = 'Service %s is not found in a general list.';

//Adds child nodes for service dependencies to the node given by ParentNode.
//Call ReloadDependencies if you need to reload the whole list.
procedure TDependencyList.LoadServiceDependencyNodes(const AService: TServiceEntry; AParentNode: PVirtualNode);
var deps: TArray<string>;
  dep: string;
  depService: TServiceEntry;
  depNode: PVirtualNode;
  folder: TSlDependencyGroupNode;
  broken: TSlBrokenDependencyNode;
begin
  if (AService = nil) or (AService.Config = nil) then
    exit;

  deps := SplitNullSeparatedList(AService.Config.lpDependencies);
  for dep in deps do begin
    if dep.StartsWith(SC_GROUP_IDENTIFIER) then begin
      //this is a dependency group, not service name
      //for now we'll just create a folder with this name
      //TODO: Also add the contents, all services from this group
      folder := TSlDependencyGroupNode.Create(Copy(dep, 2, MaxInt));
      folder.AutoDestroy := true;
      Self.AddNode(AParentNode, folder);
      continue;
    end;
    depService := FServices.Find(dep);
   //NOTE: Dependencies sometimes refer to drivers, so we either have to always load drivers
   //(as we do now), or to ignore failed matches.
   //NOTE: Dependencies DO sometimes contain failed matches, we should not crash on that!
   //Show that as a valid, but broken, dependency.
    if depService = nil then begin
      broken := TSlBrokenDependencyNode.Create(dep);
      broken.AutoDestroy := true;
      Self.AddNode(AParentNode, broken);
      continue;
    end;
    depNode := Self.AddService(AParentNode, depService);
    LoadServiceDependencyNodes(depService, depNode);
  end;
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
      if depService = nil then
        raise Exception.CreateFmt(eServiceDependentNotFound, [depnt.lpServiceName]);
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
