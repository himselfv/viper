unit Viper.Log;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TLogForm = class(TForm)
    memo: TMemo;
  public
    procedure Log(const msg: string); overload; inline;
  end;

var
  LogForm: TLogForm;

procedure Log(const msg: string); overload; inline;

implementation

{$R *.dfm}

procedure Log(const msg: string);
begin
  LogForm.Log(msg);
end;

procedure TLogForm.Log(const msg: string);
begin
  memo.Lines.Add(msg);
end;

end.
