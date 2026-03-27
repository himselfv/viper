inherited MainServiceList: TMainServiceList
  Height = 640
  ExplicitHeight = 640
  object Splitter2: TSplitter [0]
    Left = 0
    Top = 397
    Width = 746
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 13
  end
  inherited vtServices: TVirtualStringTree
    Height = 397
    OnFocusChanging = vtServicesFocusChanging
    ExplicitHeight = 13
  end
  object pcBottom: TPageControl [2]
    Left = 0
    Top = 400
    Width = 746
    Height = 240
    ActivePage = tsDescription
    Align = alBottom
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object tsDescription: TTabSheet
      Caption = 'Info'
      ImageIndex = -1
      object Label1: TLabel
        Left = 0
        Top = 57
        Width = 738
        Height = 13
        Align = alTop
        Caption = 'Additional notes:'
        Font.Charset = RUSSIAN_CHARSET
        Font.Color = clGrayText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 89
      end
      object Splitter3: TSplitter
        Left = 0
        Top = 54
        Width = 738
        Height = 3
        Cursor = crVSplit
        Align = alTop
        ExplicitLeft = -1
        ExplicitTop = 51
        ExplicitWidth = 849
      end
      object mmDetails: TMemo
        Left = 0
        Top = 0
        Width = 738
        Height = 54
        Align = alTop
        BorderStyle = bsNone
        ParentColor = True
        ReadOnly = True
        TabOrder = 0
      end
      inline NotesFrame: TRichEditFrame
        Left = 0
        Top = 70
        Width = 738
        Height = 138
        Align = alClient
        TabOrder = 1
        ExplicitTop = 70
        ExplicitWidth = 738
        ExplicitHeight = 138
        inherited mmNotes: TRichEdit
          Width = 738
          Height = 138
          Font.Height = -13
          Font.Name = 'Segoe UI'
          OnExit = mmNotesExit
          ExplicitWidth = 738
          ExplicitHeight = 138
        end
      end
    end
    object tsDependencies: TTabSheet
      Caption = 'Depends on'
      ImageIndex = -1
      OnShow = tsDependenciesShow
      inline DependencySvcList: TDependencyList
        Left = 0
        Top = 0
        Width = 738
        Height = 208
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 738
        ExplicitHeight = 208
        inherited vtServices: TVirtualStringTree
          Width = 738
          Height = 208
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
          ExplicitWidth = 738
          ExplicitHeight = 208
        end
      end
    end
    object tsDependents: TTabSheet
      Caption = 'Required by'
      ImageIndex = -1
      OnShow = tsDependentsShow
      inline DependentsSvcList: TDependencyList
        Left = 0
        Top = 0
        Width = 738
        Height = 208
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 738
        ExplicitHeight = 208
        inherited vtServices: TVirtualStringTree
          Width = 738
          Height = 208
          TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
          ExplicitWidth = 738
          ExplicitHeight = 208
        end
      end
    end
    object tsTriggers: TTabSheet
      Caption = 'Triggers'
      ImageIndex = 2
      OnShow = tsTriggersShow
      inline TriggerList: TServiceTriggerList
        Left = 0
        Top = 0
        Width = 738
        Height = 208
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 738
        ExplicitHeight = 208
        inherited Tree: TVirtualStringTree
          Width = 738
          Height = 208
          ExplicitWidth = 738
          ExplicitHeight = 208
          Columns = <
            item
              Position = 0
              Width = 182
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
    end
    object tsOperations: TTabSheet
      Caption = 'Operations'
      ImageIndex = -1
      TabVisible = False
    end
  end
  inherited ActionList: TActionList
    object aEditServiceNotes: TAction
      AutoCheck = True
      Caption = 'Edit notes'
      ShortCut = 115
      OnExecute = aEditServiceNotesExecute
    end
    object aSaveNotes: TAction
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = aSaveNotesExecute
    end
    object aShowDrivers: TAction
      Category = 'Filters'
      AutoCheck = True
      Caption = 'Show drivers'
      OnExecute = aShowDriversExecute
    end
    object aShowUserPrototypes: TAction
      Category = 'Filters'
      AutoCheck = True
      Caption = 'Show prototypes'
      Hint = 'Show prototype services for user services'
      OnExecute = aShowUserPrototypesExecute
    end
  end
end
