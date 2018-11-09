inherited ServiceTriggerList: TServiceTriggerList
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
