inherited ServiceTriggerList: TServiceTriggerList
  inherited PopupMenu: TPopupMenu
    object miAddTrigger: TMenuItem [0]
      Action = aAddTrigger
    end
    inherited miImportTrigger: TMenuItem [1]
    end
    object N1: TMenuItem [2]
      Caption = '-'
    end
    inherited miCopy: TMenuItem [3]
    end
    inherited miEditTrigger: TMenuItem [4]
    end
    inherited miExportTrigger: TMenuItem [5]
    end
    object miExportAllTriggers: TMenuItem [6]
      Action = aExportAllTriggers
    end
    inherited miEnableTrigger: TMenuItem [7]
    end
    inherited miDisableTrigger: TMenuItem [8]
    end
    inherited miDeleteTrigger: TMenuItem [9]
    end
    inherited N2: TMenuItem [10]
      Visible = False
    end
  end
  inherited ActionList: TActionList
    object aAddTrigger: TAction [10]
      Category = 'Modify'
      Caption = 'Add...'
      OnExecute = aAddTriggerExecute
    end
  end
end
