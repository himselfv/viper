unit Viper.DependencyList;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, Actions, ActnList, ImgList, VirtualTrees, Viper.ServiceList;

type
  TDependencyList = class(TServiceList)
    procedure vtServicesBeforeItemErase(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure vtServicesPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
  end;

  //Dependency group node for ServiceList
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

var
  DependencyList: TDependencyList;

implementation
uses UITypes, WinSvc, CommonResources, Viper.StyleSettings;

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

end.
