unit Viper.TriggerImportList;
{
TriggerList for TriggerImport form.
We have to inherit from it to reimplement Reload(), or it'll reload the live
triggers.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Viper.TriggerList, Actions, ActnList, Menus, VirtualTrees;

type
  TTriggerImportList = class(TTriggerList)
  public
    procedure Reload; override;
  end;

var
  TriggerImportList: TTriggerImportList;

implementation

{$R *.dfm}

procedure TTriggerImportList.Reload;
begin
  //Do nothing -- we're reloaded from the outside.
end;

end.
