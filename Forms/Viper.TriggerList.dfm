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
    Header.AutoSizeIndex = 1
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
    OnFreeNode = TreeFreeNode
    OnGetText = TreeGetText
    OnGetImageIndexEx = TreeGetImageIndexEx
    OnGetNodeDataSize = TreeGetNodeDataSize
    OnInitNode = TreeInitNode
    OnKeyDown = TreeKeyDown
    Columns = <
      item
        Position = 0
        Width = 80
        WideText = 'Action'
      end
      item
        Position = 1
        Width = 406
        WideText = 'Trigger'
      end
      item
        Position = 2
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
      object Sourcetext1: TMenuItem
        Action = aCopyTriggerText
      end
      object SourceID1: TMenuItem
        Action = aCopySourceData
      end
      object Additionalparams1: TMenuItem
        Action = aCopyParams
      end
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
  end
end
