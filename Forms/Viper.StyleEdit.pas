unit Viper.StyleEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TStyleEditForm = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    lblBgColor: TLabel;
    lblFontColor: TLabel;
    cbBgColor: TColorBox;
    cbFontColor: TColorBox;
    cbBold: TCheckBox;
    cbItalic: TCheckBox;
    cbStrikeOut: TCheckBox;
    cbUnderline: TCheckBox;
  protected
    function GetFontStyles: TFontStyles;
    procedure SetFontStyles(const Value: TFontStyles);
  public
    property FontStyles: TFontStyles read GetFontStyles write SetFontStyles;
  end;

implementation

{$R *.dfm}

function TStyleEditForm.GetFontStyles: TFontStyles;
begin
  Result := [];
  if cbBold.Checked then Result := Result + [fsBold];
  if cbItalic.Checked then Result := Result + [fsItalic];
  if cbUnderline.Checked then Result := Result + [fsUnderline];
  if cbStrikeOut.Checked then Result := Result + [fsStrikeOut];
end;

procedure TStyleEditForm.SetFontStyles(const Value: TFontStyles);
begin
  cbBold.Checked := (fsBold in Value);
  cbItalic.Checked := (fsItalic in Value);
  cbUnderline.Checked := (fsUnderline in Value);
  cbStrikeOut.Checked := (fsStrikeOut in Value);
end;

end.
