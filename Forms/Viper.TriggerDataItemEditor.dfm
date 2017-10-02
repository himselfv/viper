object TriggerDataItemEditor: TTriggerDataItemEditor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Edit data item'
  ClientHeight = 123
  ClientWidth = 344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    344
    123)
  PixelsPerInch = 96
  TextHeight = 13
  object lblValue: TLabel
    Left = 8
    Top = 21
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object lblDataType: TLabel
    Left = 8
    Top = 48
    Width = 52
    Height = 13
    Caption = 'Data type:'
  end
  object btnOk: TButton
    Left = 180
    Top = 90
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = btnOkClick
    ExplicitLeft = 208
    ExplicitTop = 172
  end
  object btnCancel: TButton
    Left = 261
    Top = 90
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 289
    ExplicitTop = 172
  end
  object cbType: TComboBox
    Left = 180
    Top = 45
    Width = 156
    Height = 21
    Style = csDropDownList
    Anchors = [akTop, akRight]
    TabOrder = 1
  end
  object edtValue: TEdit
    Left = 56
    Top = 18
    Width = 280
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
end
