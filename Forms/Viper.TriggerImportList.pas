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
  TTriggerImportList = class(TTriggerList)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Reload; override;
    function Add(const ATrigger: TRegTriggerEntry): PVirtualNode; reintroduce;
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

procedure TTriggerImportList.Reload;
begin
  //Do nothing -- we're reloaded from the outside.
end;

//Adds a new imported trigger entry. Internal data is copied.
function TTriggerImportList.Add(const ATrigger: TRegTriggerEntry): PVirtualNode;
begin
  Result := inherited Add(ATrigger.ServiceName, ATrigger.Index, ATrigger.Trigger);
  Tree.CheckType[Result] := ctCheckBox;
  Tree.CheckState[Result] := csCheckedNormal;
end;


end.
