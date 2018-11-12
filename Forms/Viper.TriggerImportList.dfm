inherited TriggerImportList: TTriggerImportList
  inherited Tree: TVirtualStringTree
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    PopupMenu = nil
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    Columns = <
      item
        Position = 0
        Width = 250
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
end
