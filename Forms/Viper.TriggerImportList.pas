unit Viper.TriggerImportList;
{
TriggerList for TriggerImport form.
We have to inherit from it to reimplement Reload(), or it'll reload the live
triggers.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Actions, ActnList, Menus, VirtualTrees, TriggerExport,
  Viper.TriggerList;

type
  //Additional data associated with the nodes. Goes after the inherited node data.
  TNdTriggerImportData = record
    GrayedOut: boolean;
    Entry: PRegTriggerEntry;
  end;
  PNdTriggerImportData = ^TNdTriggerImportData;


  TTriggerImportList = class(TTriggerList)
    procedure TreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
  protected
    FInheritedNodeDataSize: integer;
  public
    constructor Create(AOwner: TComponent); override;
    function GetTriggerImportData(Node: PVirtualNode): PNdTriggerImportData; inline;
    procedure Reload; override;
    function Add(const ATrigger: TRegTriggerEntry; AGrayedOut: boolean = false): PVirtualNode; reintroduce;
  end;

var
  TriggerImportList: TTriggerImportList;

implementation

{$R *.dfm}

constructor TTriggerImportList.Create(AOwner: TComponent);
begin
  inherited;
  Self.FEntryMode := emChildEntries;
end;

procedure TTriggerImportList.TreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  inherited;
  Self.FInheritedNodeDataSize := NodeDataSize;
  Inc(NodeDataSize, SizeOf(TNdTriggerImportData));
end;

//Retrieves additional data this module associates with the nodes. It goes after the inherited node data.
function TTriggerImportList.GetTriggerImportData(Node: PVirtualNode): PNdTriggerImportData;
begin
  Result := Tree.GetNodeData(Node);
  if Result <> nil then
    Result := PNdTriggerImportData(NativeUint(Result) + FInheritedNodeDataSize);
end;

procedure TTriggerImportList.Reload;
begin
  //Do nothing -- we're reloaded from the outside.
end;

resourcestring
  sServiceNotFound = '%s - not found';

//Adds a new imported trigger entry. Internal data is copied.
function TTriggerImportList.Add(const ATrigger: TRegTriggerEntry; AGrayedOut: boolean): PVirtualNode;
var ServiceName: string;
  ChildNode: PVirtualNode;
begin
  if not AGrayedOut then
    ServiceName := ATrigger.ServiceName
  else
    ServiceName := Format(sServiceNotFound, [ATrigger.ServiceName]);
  Result := inherited AddFromScm(ServiceName, ATrigger.Index, ATrigger.Trigger);
  with GetTriggerImportData(Result)^ do begin
    GrayedOut := AGrayedOut;
    Entry := @ATrigger;
  end;
  for ChildNode in Tree.ChildNodes(Result) do //multi-entry triggers may spawn child nodes
    with GetTriggerImportData(ChildNode)^ do begin
      GrayedOut := AGrayedOut;
      Entry := @ATrigger;
    end;
  Tree.CheckType[Result] := ctCheckBox;
  if AGrayedOut then
    Tree.CheckState[Result] := csUncheckedNormal
  else
    Tree.CheckState[Result] := csCheckedNormal;
  Tree.IsDisabled[Result] := AGrayedOut;
end;


end.
