object TriggerEditorForm: TTriggerEditorForm
  Left = 0
  Top = 0
  Caption = 'Edit trigger'
  ClientHeight = 298
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
    298)
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
  object pcPresetDetails: TPageControl
    Left = 8
    Top = 35
    Width = 536
    Height = 78
    ActivePage = tsPresetWNF
    Anchors = [akLeft, akTop, akRight]
    Style = tsFlatButtons
    TabOrder = 2
    object tsPresetGeneric: TTabSheet
      Caption = 'Generic'
      TabVisible = False
      ExplicitHeight = 52
      DesignSize = (
        528
        68)
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
      ExplicitHeight = 52
      DesignSize = (
        528
        68)
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
      ExplicitHeight = 52
      DesignSize = (
        528
        68)
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
    object tsPresetRPC: TTabSheet
      Caption = 'RPC'
      ImageIndex = 3
      TabVisible = False
      ExplicitHeight = 52
      DesignSize = (
        528
        68)
      object lblRpcInterface: TLabel
        Left = 3
        Top = 6
        Width = 75
        Height = 13
        Caption = 'RPC Interface: '
      end
      object lblRpcInterfaceHint: TLabel
        Left = 3
        Top = 30
        Width = 162
        Height = 13
        Caption = 'Stored as the first Data property.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object cbRpcInterface: TComboBox
        Left = 136
        Top = 3
        Width = 389
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = cbRpcInterfaceChange
      end
    end
    object tsPresetWNF: TTabSheet
      Caption = 'WNF'
      ImageIndex = 4
      TabVisible = False
      ExplicitHeight = 61
      DesignSize = (
        528
        68)
      object lblWnfEvent: TLabel
        Left = 3
        Top = 6
        Width = 61
        Height = 13
        Caption = 'WNF Event: '
      end
      object lblWnfEventHint: TLabel
        Left = 3
        Top = 30
        Width = 522
        Height = 38
        Anchors = [akLeft, akTop, akRight, akBottom]
        AutoSize = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        ExplicitHeight = 22
      end
      object cbWnfEvent: TComboBox
        Left = 136
        Top = 3
        Width = 389
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        OnChange = cbWnfEventChange
      end
    end
  end
  object pnlDataItems: TPanel
    Left = 8
    Top = 119
    Width = 536
    Height = 135
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 3
    DesignSize = (
      536
      135)
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
      Height = 107
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
      ExplicitHeight = 110
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
    Top = 265
    Width = 87
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = btnOkClick
    ExplicitTop = 252
  end
  object btnCancel: TButton
    Left = 456
    Top = 265
    Width = 88
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    ExplicitTop = 252
  end
  object cbAction: TComboBoxEx
    Left = 8
    Top = 8
    Width = 81
    Height = 22
    ItemsEx = <
      item
        Caption = 'Start'
        ImageIndex = 0
        SelectedImageIndex = 0
      end
      item
        Caption = 'Stop'
        ImageIndex = 1
        SelectedImageIndex = 1
      end>
    Style = csExDropDownList
    TabOrder = 0
    Images = CommonRes.ilImages
  end
  object cbTypePreset: TComboBoxEx
    Left = 200
    Top = 8
    Width = 344
    Height = 22
    ItemsEx = <>
    Style = csExDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    OnChange = cbTypePresetChange
    Images = CommonRes.ilImages
  end
end
