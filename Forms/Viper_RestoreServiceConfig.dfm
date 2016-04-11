object RestoreServiceConfigForm: TRestoreServiceConfigForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Restore Service Config'
  ClientHeight = 308
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    386
    308)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 355
    Height = 26
    Caption = 
      'You are ready to restore the startup configuration for the follo' +
      'wing list of services:'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 244
    Width = 121
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Do you want to proceed?'
  end
  object lbList: TListBox
    Left = 8
    Top = 40
    Width = 370
    Height = 198
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btnOK: TButton
    Left = 222
    Top = 275
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 303
    Top = 275
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
end
