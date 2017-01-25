unit Viper.RichEditEx;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TRichEditFrame = class(TFrame)
    mmNotes: TRichEdit;
  protected
    procedure CreateWnd; override;
    procedure WmNotify(var Msg: TWMNotify); message WM_NOTIFY;
  end;

implementation
uses RichEdit, ShellUtils;

{$R *.dfm}

procedure TRichEditFrame.CreateWnd;
var mask: Integer;
begin
  inherited;
  mask := SendMessage(mmNotes.Handle, EM_GETEVENTMASK, 0, 0);
  SendMessage(mmNotes.Handle, EM_SETEVENTMASK, 0, mask or ENM_LINK);
  SendMessage(mmNotes.Handle, EM_AUTOURLDETECT, Integer(True), 0);
end;

procedure TRichEditFrame.WmNotify(var Msg: TWMNotify);
var p: TENLink;
  strURL: string;
begin
  if Msg.NMHdr^.code = EN_LINK then begin
    p := TENLink(Pointer(TWMNotify(Msg).NMHdr)^);
    if p.msg = WM_LBUTTONDOWN then begin
      SendMessage(mmNotes.Handle, EM_EXSETSEL, 0, LongInt(@(p.chrg)));
      strURL := mmNotes.SelText;
      ShellUtils.ShellOpen(strURL); //could've also just called some event
    end
  end;
  inherited;
end;


end.
