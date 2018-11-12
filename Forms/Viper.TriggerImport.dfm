object TriggerImportForm: TTriggerImportForm
  Left = 0
  Top = 0
  Caption = 'Import triggers'
  ClientHeight = 343
  ClientWidth = 516
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
  PixelsPerInch = 96
  TextHeight = 13
  object lblPrompt: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 500
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
    ExplicitLeft = 5
    ExplicitTop = 5
    ExplicitWidth = 148
  end
  object pnlButtons: TPanel
    AlignWithMargins = True
    Left = 5
    Top = 312
    Width = 506
    Height = 26
    Margins.Left = 0
    Margins.Top = 9
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 360
    DesignSize = (
      506
      26)
    object btnOk: TButton
      Left = 347
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Import'
      Default = True
      ModalResult = 1
      TabOrder = 0
      ExplicitLeft = 201
    end
    object btnCancel: TButton
      Left = 428
      Top = 0
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 282
    end
  end
  object vtTriggers: TVirtualStringTree
    Left = 5
    Top = 30
    Width = 506
    Height = 273
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = CommonRes.ilImages
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
    ExplicitWidth = 360
    Columns = <
      item
        Position = 0
        Width = 342
        WideText = 'Trigger'
      end
      item
        Position = 1
        Width = 160
        WideText = 'Service'
      end>
  end
end
