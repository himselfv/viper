object TriggerEditorForm: TTriggerEditorForm
  Left = 0
  Top = 0
  Caption = 'Edit trigger'
  ClientHeight = 285
  ClientWidth = 552
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
  OnShow = FormShow
  DesignSize = (
    552
    285)
  PixelsPerInch = 96
  TextHeight = 13
  object lblActionToType: TLabel
    Left = 100
    Top = 11
    Width = 72
    Height = 13
    Alignment = taCenter
    Caption = 'the service on:'
  end
  object cbAction: TComboBox
    Left = 8
    Top = 8
    Width = 81
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 0
    Text = 'Start'
    Items.Strings = (
      'Start'
      'Stop')
  end
  object cbTypePreset: TComboBox
    Left = 200
    Top = 8
    Width = 344
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = cbTypePresetChange
  end
  object pcPresetDetails: TPageControl
    Left = 8
    Top = 35
    Width = 536
    Height = 62
    ActivePage = tsPresetDevice
    Anchors = [akLeft, akTop, akRight]
    Style = tsFlatButtons
    TabOrder = 2
    object tsPresetGeneric: TTabSheet
      Caption = 'Generic'
      TabVisible = False
      DesignSize = (
        528
        52)
      object lblCustomType: TLabel
        Left = 3
        Top = 6
        Width = 28
        Height = 13
        Caption = 'Type:'
      end
      object lblCustomSubtype: TLabel
        Left = 3
        Top = 33
        Width = 44
        Height = 13
        Caption = 'Subtype:'
      end
      object edtCustomSubtype: TEdit
        Left = 64
        Top = 30
        Width = 461
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object edtCustomType: TSpinEdit
        Left = 64
        Top = 3
        Width = 121
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 0
      end
    end
    object tsPresetDevice: TTabSheet
      Caption = 'Device'
      ImageIndex = 1
      TabVisible = False
      DesignSize = (
        528
        52)
      object lblDeviceInterfaceClass: TLabel
        Left = 3
        Top = 6
        Width = 115
        Height = 13
        Caption = 'Device Interface Class: '
      end
      object cbDeviceInterfaceClass: TComboBox
        Left = 136
        Top = 3
        Width = 389
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
    object tsPresetETW: TTabSheet
      Caption = 'ETW'
      ImageIndex = 2
      TabVisible = False
      DesignSize = (
        528
        52)
      object lblEtwEventSource: TLabel
        Left = 3
        Top = 6
        Width = 96
        Height = 13
        Caption = 'ETW Event Source: '
      end
      object cbEtwEventSource: TComboBox
        Left = 136
        Top = 3
        Width = 389
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
    end
  end
  object pnlDataItems: TPanel
    Left = 8
    Top = 103
    Width = 536
    Height = 138
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      536
      138)
    object lblDataItemsCaption: TLabel
      Left = 7
      Top = 0
      Width = 27
      Height = 13
      Caption = 'Data:'
    end
    object vtDataItems: TVirtualStringTree
      Left = 7
      Top = 19
      Width = 441
      Height = 110
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderWidth = 1
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
      TabOrder = 0
      TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
      TreeOptions.PaintOptions = [toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect]
      OnFocusChanged = vtDataItemsFocusChanged
      OnFreeNode = vtDataItemsFreeNode
      OnGetText = vtDataItemsGetText
      OnGetNodeDataSize = vtDataItemsGetNodeDataSize
      OnInitNode = vtDataItemsInitNode
      Columns = <
        item
          Position = 0
          Width = 295
          WideText = 'Value'
        end
        item
          Position = 1
          Width = 140
          WideText = 'Data type'
        end>
    end
    object btnDataItemAdd: TButton
      Left = 454
      Top = 19
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Add...'
      TabOrder = 1
      OnClick = btnDataItemAddClick
    end
    object btnDataItemEdit: TButton
      Left = 454
      Top = 50
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Edit...'
      Enabled = False
      TabOrder = 2
      OnClick = btnDataItemEditClick
    end
    object btnDataItemDelete: TButton
      Left = 454
      Top = 81
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Delete'
      Enabled = False
      TabOrder = 3
      OnClick = btnDataItemDeleteClick
    end
  end
  object btnOk: TButton
    Left = 354
    Top = 252
    Width = 87
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 456
    Top = 252
    Width = 88
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
