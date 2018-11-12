object TriggerImportForm: TTriggerImportForm
  Left = 0
  Top = 0
  Caption = 'Import triggers'
  ClientHeight = 343
  ClientWidth = 517
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 5
  Padding.Top = 5
  Padding.Right = 5
  Padding.Bottom = 5
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblPrompt: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 501
    Height = 13
    Margins.Bottom = 9
    Align = alTop
    Caption = 'Choose triggers to import:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    ExplicitWidth = 148
  end
  object pnlButtons: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 312
    Width = 507
    Height = 26
    Margins.Left = 0
    Margins.Top = 9
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      507
      26)
    object btnOk: TButton
      Left = 348
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Import'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 429
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  inline TriggerList: TTriggerImportList
    Left = 5
    Top = 30
    Width = 507
    Height = 273
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 5
    ExplicitTop = 30
    ExplicitWidth = 507
    ExplicitHeight = 273
    inherited Tree: TVirtualStringTree
      Width = 507
      Height = 273
      ExplicitTop = 3
      ExplicitWidth = 507
      ExplicitHeight = 273
    end
  end
end
