object TriggerList: TTriggerList
  Left = 0
  Top = 0
  Width = 792
  Height = 238
  TabOrder = 0
  object vtTriggers: TVirtualStringTree
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
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect, toSimpleDrawSelection]
    OnFocusChanged = vtTriggersFocusChanged
    OnFreeNode = vtTriggersFreeNode
    OnGetText = vtTriggersGetText
    OnGetImageIndexEx = vtTriggersGetImageIndexEx
    OnGetNodeDataSize = vtTriggersGetNodeDataSize
    OnInitNode = vtTriggersInitNode
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
    object Copy1: TMenuItem
      Action = aCopyText
    end
  end
  object ActionList: TActionList
    Left = 16
    Top = 8
    object aCopyText: TAction
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = aCopyTextExecute
    end
  end
end
