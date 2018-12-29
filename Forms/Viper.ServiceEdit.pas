unit Viper.ServiceEdit;
{
A form to view/edit certain service parameters.
Works on any TServiceEntry descendant. Tries to be smart and update things
in batches.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, SvcEntry;

type
  TServiceEditForm = class(TForm)
    pnlFooter: TPanel;
    pcPages: TPageControl;
    tsGeneral: TTabSheet;
    btnOk: TButton;
    btnCancel: TButton;
    btnApply: TButton;
    edtServiceDLL: TEdit;
    cbServiceDllUnloadOnStop: TCheckBox;
    edtServiceMain: TEdit;
    lblServiceMain: TLabel;
    lblServiceDLL: TLabel;
    lblExecutableFile: TLabel;
    edtExecutableFile: TEdit;
    edtDescription: TMemo;
    lblDescription: TLabel;
    lblDisplayName: TLabel;
    edtDisplayName: TEdit;
    edtServiceName: TEdit;
    lblServiceName: TLabel;
    lblServiceType: TLabel;
    cbServiceType: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  protected
    FService: TServiceEntry;
    procedure Reload;

  protected
    FHasChanges: boolean;
    procedure SetHasChanges(const AValue: boolean);
    procedure SaveChanges;

  public
    class function EditService(AOwner: TComponent; const AService: TServiceEntry): TModalResult;
    property HasChanges: boolean read FHasChanges write SetHasChanges;

  end;

var
  ServiceEditForm: TServiceEditForm;

implementation

{$R *.dfm}

{
  SERVICE_KERNEL_DRIVER         = $00000001;
  SERVICE_FILE_SYSTEM_DRIVER    = $00000002;
  SERVICE_ADAPTER               = $00000004;
  SERVICE_RECOGNIZER_DRIVER     = $00000008;
  SERVICE_DRIVER                = (SERVICE_KERNEL_DRIVER or
                                   SERVICE_FILE_SYSTEM_DRIVER or
                                   SERVICE_RECOGNIZER_DRIVER);

  SERVICE_WIN32_OWN_PROCESS     = $00000010;
  SERVICE_WIN32_SHARE_PROCESS   = $00000020;
  SERVICE_WIN32                 = (SERVICE_WIN32_OWN_PROCESS or
                                   SERVICE_WIN32_SHARE_PROCESS);

  SERVICE_INTERACTIVE_PROCESS   = $00000100;

  SERVICE_USER_SERVICE          = $00000040;                        //set for user service prototypes AND instances
  SERVICE_USERSERVICE_INSTANCE  = $00000080;                        //set only for instances

  SERVICE_USER_SHARE_PROCESS    = (SERVICE_USER_SERVICE or
                                   SERVICE_WIN32_SHARE_PROCESS);
  SERVICE_USER_OWN_PROCESS      = (SERVICE_USER_SERVICE or
                                   SERVICE_WIN32_OWN_PROCESS);

  SERVICE_INTERACTIVE_PROCESS   = $00000100;
  SERVICE_PKG_SERVICE           = $00000200;

  Kernel driver
  File system driver
  Recognizer driver
  Adapter

  Service (standalone)
  Service (shared)

  [ ] Interactive
  [ ] Per-user


}


class function TServiceEditForm.EditService(AOwner: TComponent; const AService: TServiceEntry): TModalResult;
var Instance: TServiceEditForm;
begin
  Instance := TServiceEditForm.Create(AOwner);
  try
    Instance.FService := AService;
    Result := Instance.ShowModal;
  finally
    FreeAndNil(Instance);
  end;
end;

procedure TServiceEditForm.FormShow(Sender: TObject);
begin
  Self.Reload;
end;

//Reloads all data from FService and discards any changes
procedure TServiceEditForm.Reload;
var ii: TServiceImageInformation;
begin
  edtServiceName.Text := FService.ServiceName;
  edtDisplayName.Text := FService.DisplayName;
  edtDescription.Text := FService.Description;

  ii := FService.ImageInformation;
  edtExecutableFile.Text := ii.ImagePath;
  edtServiceDLL.Text := ii.ServiceDll;

  Self.HasChanges := false;
end;

procedure TServiceEditForm.SetHasChanges(const AValue: boolean);
begin
  FHasChanges := AValue;
  Self.btnApply.Enabled := AValue;
end;

procedure TServiceEditForm.btnApplyClick(Sender: TObject);
begin
  SaveChanges;
end;

procedure TServiceEditForm.btnOkClick(Sender: TObject);
begin
  SaveChanges;
  ModalResult := mrOk;
end;

procedure TServiceEditForm.SaveChanges;
begin
  //TODO: Save changes.
  Self.HasChanges := false;
end;



end.
