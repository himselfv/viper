unit Viper.ServiceTriggerList;
{
Trigger list for a particular service only.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualTrees, ImgList, WinSvc, ServiceHelper, Vcl.Menus, System.Actions, Vcl.ActnList,
  TriggerUtils, Viper.TriggerList, Vcl.ExtCtrls;

type
  TServiceTriggerList = class(TTriggerList)
    miAddTrigger: TMenuItem;
    N1: TMenuItem;
    aAddTrigger: TAction;
    miExportAllTriggers: TMenuItem;
    procedure aAddTriggerExecute(Sender: TObject);
    procedure TreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure aImportTriggerExecute(Sender: TObject);
  protected
    //We need to be able to reload and edit
    FServiceName: string;
    FScmHandle: SC_HANDLE;
    FServiceHandle: SC_HANDLE;
    FOwnsServiceHandle: boolean;
  public
    procedure Initialize;
    destructor Destroy; override;
    procedure SetService(ServiceName: string; ServiceHandle: SC_HANDLE = 0);
    procedure Reload; override;
  end;

var
  ServiceTriggerList: TServiceTriggerList;

implementation
uses UITypes, TriggerExport, Viper.TriggerEditor, Viper.TriggerImport;

{$R *.dfm}

procedure TServiceTriggerList.Initialize;
begin
  //Hide service name column and move action column to the first place
  //We can't put this in Create because this needs to happen after the property loading.
  Self.Tree.Header.Columns[Self.colService].Options :=
    Self.Tree.Header.Columns[Self.colService].Options - [coVisible];
  Self.Tree.Header.Columns[Self.colAction].Position := 0;
end;

destructor TServiceTriggerList.Destroy;
begin
  //Free service handle if we own it:
  SetService('', 0);
  inherited;
end;

procedure TServiceTriggerList.SetService(ServiceName: string; ServiceHandle: SC_HANDLE = 0);
begin
  if Self.FServiceHandle <> 0 then begin
    if Self.FOwnsServiceHandle then begin
      CloseServiceHandle(Self.FServiceHandle);
      CloseServiceHandle(Self.FScmHandle);
    end;
    Self.FServiceHandle := 0;
    Self.FScmHandle := 0;
  end;

  Self.FServiceName := ServiceName;
  if FServiceName = '' then begin
    Self.FServiceHandle := 0;
    Self.FOwnsServiceHandle := false;
  end else
  if ServiceHandle <> 0 then begin
    Self.FServiceHandle := ServiceHandle;
    Self.FOwnsServiceHandle := false;
  end else begin
    OpenScmAndService(Self.FScmHandle, Self.FServiceHandle, ServiceName);
    Self.FOwnsServiceHandle := true;
  end;

  //Do reload even if service==nil because we need to update popup commands
  Reload;
end;

procedure TServiceTriggerList.Reload;
begin
  Clear;

  //Here we need triggers for exactly one service
  if FServiceName <> '' then
    LoadTriggersForService(FServiceName, FServiceHandle);

  //Reset the popup menu
  //We'd like to put this into FormCreate, but there's no FormCreates for TFrames.
  TreeChange(Tree, nil);
end;

procedure TServiceTriggerList.TreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var HaveService: boolean;
begin
  inherited;
  HaveService := Self.FServiceName <> '';
  aAddTrigger.Visible := HaveService;
  aImportTrigger.Visible := HaveService;
end;

procedure TServiceTriggerList.aAddTriggerExecute(Sender: TObject);
var EditForm: TTriggerEditorForm;
  TriggerData: PSERVICE_TRIGGER;
begin
  EditForm := TTriggerEditorForm.Create(Self);
  try
    TriggerData := nil;
    if not IsPositiveResult(EditForm.EditTrigger(TriggerData))
    or (TriggerData = nil) //safety
      then exit;
  finally
    FreeAndNil(EditForm);
  end;

  //Add the given trigger
  try
    //We only have a handle with read access, so reopen
    with OpenService2(Self.FServiceName,
      STANDARD_RIGHTS_REQUIRED or SC_MANAGER_CONNECT,
      SERVICE_QUERY_CONFIG or SERVICE_CHANGE_CONFIG) do
    begin
      AddServiceTriggers(SvcHandle, [TriggerData^]);
      TriggerUtils.TriggerListChanged(Self, Self.FServiceName);
    end;
  finally
    FreeMem(TriggerData);
  end;

  //Reload the triggers
  //We could just parse the newly created trigger, but lets KISS for now
  Self.Reload;
end;

procedure TServiceTriggerList.aImportTriggerExecute(Sender: TObject);
begin
  //Single trigger version
  Self.TryImportTriggers(Self.FServiceName);
end;


end.
