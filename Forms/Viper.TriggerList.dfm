object TriggerList: TTriggerList
  Left = 0
  Top = 0
  Width = 792
  Height = 238
  TabOrder = 0
  object Tree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 792
    Height = 238
    Align = alClient
    BorderWidth = 1
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoAutoResize, hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = CommonRes.ilImages
    PopupMenu = PopupMenu
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect, toSimpleDrawSelection]
    OnChange = TreeChange
    OnCollapsing = TreeCollapsing
    OnCompareNodes = TreeCompareNodes
    OnFocusChanged = TreeFocusChanged
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnPaintText = TreePaintText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnGetNodeDataSize = TreeGetNodeDataSize
    OnHeaderClick = TreeHeaderClick
    OnInitNode = TreeInitNode
    OnKeyDown = TreeKeyDown
    Columns = <
      item
        Position = 0
        Width = 236
        WideText = 'Trigger'
      end
      item
        Position = 1
        Width = 100
        WideText = 'Action'
      end
      item
        Position = 2
        Width = 150
        WideText = 'Service'
      end
      item
        Position = 3
        Width = 300
        WideText = 'Params'
      end>
  end
  object PopupMenu: TPopupMenu
    Left = 16
    Top = 64
    object miCopy: TMenuItem
      Caption = 'Copy'
      object miCopySummary: TMenuItem
        Action = aCopySummary
      end
      object miCopyTriggerText: TMenuItem
        Action = aCopyTriggerText
      end
      object miCopySourceData: TMenuItem
        Action = aCopySourceData
      end
      object miCopyParams: TMenuItem
        Action = aCopyParams
      end
      object miCopyRegistryDefinition: TMenuItem
        Action = aCopyRegistryDefinition
      end
    end
    object miJumpToRegistry: TMenuItem
      Action = aJumpToRegistry
    end
    object miEditTrigger: TMenuItem
      Action = aEditTrigger
    end
    object miEnableTrigger: TMenuItem
      Action = aEnableTrigger
    end
    object miExportTrigger: TMenuItem
      Action = aExportTrigger
    end
    object miDisableTrigger: TMenuItem
      Action = aDisableTrigger
    end
    object miDeleteTrigger: TMenuItem
      Action = aDeleteTrigger
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miImportTrigger: TMenuItem
      Action = aImportTrigger
    end
  end
  object ActionList: TActionList
    Left = 16
    Top = 8
    object aCopySummary: TAction
      Category = 'Copy'
      Caption = 'Summary'
      OnExecute = aCopySummaryExecute
    end
    object aCopyTriggerText: TAction
      Category = 'Copy'
      Caption = 'Trigger text'
      OnExecute = aCopyTriggerTextExecute
    end
    object aCopySourceData: TAction
      Category = 'Copy'
      Caption = 'Source reference'
      OnExecute = aCopySourceDataExecute
    end
    object aCopyParams: TAction
      Category = 'Copy'
      Caption = 'Additional params'
      OnExecute = aCopyParamsExecute
    end
    object aDisableTrigger: TAction
      Category = 'Modify'
      Caption = 'Disable'
      OnExecute = aDisableTriggerExecute
    end
    object aEnableTrigger: TAction
      Category = 'Modify'
      Caption = 'Enable'
      OnExecute = aEnableTriggerExecute
    end
    object aExportTrigger: TAction
      Category = 'Modify'
      Caption = 'Export'
      OnExecute = aExportTriggerExecute
    end
    object aExportAllTriggers: TAction
      Category = 'Modify'
      Caption = 'Export all'
      OnExecute = aExportAllTriggersExecute
    end
    object aDeleteTrigger: TAction
      Category = 'Modify'
      Caption = 'Delete'
      OnExecute = aDeleteTriggerExecute
    end
    object aEditTrigger: TAction
      Category = 'Modify'
      Caption = 'Edit'
      OnExecute = aEditTriggerExecute
    end
    object aCopyRegistryDefinition: TAction
      Category = 'Copy'
      Caption = 'Registry definition'
      OnExecute = aCopyRegistryDefinitionExecute
    end
    object aImportTrigger: TAction
      Category = 'Modify'
      Caption = 'Import...'
      OnExecute = aImportTriggerExecute
    end
    object aJumpToRegistry: TAction
      Caption = 'Jump to key'
      Hint = 'Jump to registry key associated with the trigger'
      OnExecute = aJumpToRegistryExecute
    end
  end
  object OpenTriggersDialog: TOpenDialog
    DefaultExt = '*.reg'
    Filter = 'Triggers export files (*.reg)|*.reg|All files (*.*)|*.*'
    Title = 'Open Triggers Export...'
    Left = 184
    Top = 8
  end
  object SaveTriggersDialog: TSaveDialog
    DefaultExt = '*.reg'
    Filter = 'Triggers export files (*.reg)|*.reg|All files (*.*)|*.*'
    Title = 'Save Triggers...'
    Left = 184
    Top = 64
  end
  object ReloadTimer: TTimer
    Enabled = False
    Interval = 250
    OnTimer = ReloadTimerTimer
    Left = 296
    Top = 8
  end
end
