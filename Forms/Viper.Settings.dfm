object SettingsForm: TSettingsForm
  Left = 0
  Top = 0
  Caption = 'Settings'
  ClientHeight = 124
  ClientWidth = 348
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
    348
    124)
  PixelsPerInch = 96
  TextHeight = 13
  object lblPortableModeDesc: TLabel
    Left = 16
    Top = 31
    Width = 324
    Height = 26
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'If set, settings will be stored in an ini file in the applicatio' +
      'n folder.'
    WordWrap = True
  end
  object cbPortableMode: TCheckBox
    Left = 8
    Top = 8
    Width = 332
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Store settings in a portable way'
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 265
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 184
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
end
