object StyleEditForm: TStyleEditForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Style editor'
  ClientHeight = 199
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    290
    199)
  PixelsPerInch = 96
  TextHeight = 13
  object lblBgColor: TLabel
    Left = 8
    Top = 11
    Width = 60
    Height = 13
    Caption = 'Background:'
  end
  object lblFontColor: TLabel
    Left = 8
    Top = 41
    Width = 26
    Height = 13
    Caption = 'Font:'
  end
  object btnOk: TButton
    Left = 90
    Top = 166
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    ExplicitLeft = 144
    ExplicitTop = 341
  end
  object btnCancel: TButton
    Left = 193
    Top = 166
    Width = 89
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 247
    ExplicitTop = 341
  end
  object cbBgColor: TColorBox
    Left = 111
    Top = 8
    Width = 171
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    ExplicitWidth = 150
  end
  object cbFontColor: TColorBox
    Left = 111
    Top = 38
    Width = 171
    Height = 22
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames, cbCustomColors]
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
    ExplicitWidth = 150
  end
  object cbBold: TCheckBox
    Left = 111
    Top = 66
    Width = 171
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Bold'
    TabOrder = 4
    ExplicitWidth = 150
  end
  object cbItalic: TCheckBox
    Left = 111
    Top = 89
    Width = 171
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Italic'
    TabOrder = 5
    ExplicitWidth = 150
  end
  object cbStrikeOut: TCheckBox
    Left = 111
    Top = 135
    Width = 171
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Strike-out'
    TabOrder = 7
    ExplicitWidth = 149
  end
  object cbUnderline: TCheckBox
    Left = 111
    Top = 112
    Width = 171
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Underline'
    TabOrder = 6
    ExplicitWidth = 149
  end
end
