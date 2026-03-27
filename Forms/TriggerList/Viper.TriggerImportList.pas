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
  Viper.TriggerList, Vcl.ExtCtrls;

type
  //Additional data associated with the nodes. Goes after the inherited node data.
  TNdTriggerImportData = record
    //Turns out we don't need anything special
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

//Adds a new imported trigger entry. The underlying PSERVICE_TRIGGER must remain valid until Clear()
function TTriggerImportList.Add(const ATrigger: TRegTriggerEntry; AGrayedOut: boolean): PVirtualNode;
var Trigger: TNdTrigger;
  ChildNode: PVirtualNode;
begin
  //Do not copy the data
  Trigger := TNdTrigger.Create(ATrigger.Trigger, {OwnsData=}false);
  if not AGrayedOut then
    Trigger.ServiceName := ATrigger.ServiceName
  else
    Trigger.ServiceName := Format(sServiceNotFound, [ATrigger.ServiceName]);
  Trigger.Index := ATrigger.Index;

  Result := inherited Add(Trigger);

  Tree.IsDisabled[Result] := AGrayedOut;
  Tree.CheckType[Result] := ctCheckBox;
  if AGrayedOut then
    Tree.CheckState[Result] := csUncheckedNormal
  else
    Tree.CheckState[Result] := csCheckedNormal;
  for ChildNode in Tree.ChildNodes(Result) do //multi-entry triggers may spawn child nodes
    Tree.IsDisabled[ChildNode] := AGrayedOut;
end;


end.
