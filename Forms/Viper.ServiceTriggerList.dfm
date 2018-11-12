inherited ServiceTriggerList: TServiceTriggerList
  inherited Tree: TVirtualStringTree
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
  inherited PopupMenu: TPopupMenu
    object miAddTrigger: TMenuItem [0]
      Action = aAddTrigger
    end
    object miImportTrigger: TMenuItem [1]
      Action = aImportTrigger
    end
    object N1: TMenuItem [2]
      Caption = '-'
    end
    object miExportAllTriggers: TMenuItem [6]
      Action = aExportAllTriggers
    end
  end
  inherited ActionList: TActionList
    object aAddTrigger: TAction
      Category = 'Modify'
      Caption = 'Add...'
      OnExecute = aAddTriggerExecute
    end
    object aImportTrigger: TAction
      Category = 'Modify'
      Caption = 'Import...'
      OnExecute = aImportTriggerExecute
    end
  end
end
