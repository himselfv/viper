object ScheduledTasksForm: TScheduledTasksForm
  Left = 0
  Top = 0
  Caption = 'Scheduled Tasks'
  ClientHeight = 480
  ClientWidth = 640
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 305
    Width = 640
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ExplicitTop = 291
    ExplicitWidth = 478
  end
  object vtTasks: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 640
    Height = 305
    Align = alClient
    BorderWidth = 1
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    PopupMenu = pmPopup
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toRightClickSelect]
    OnCompareNodes = vtTasksCompareNodes
    OnFocusChanged = vtTasksFocusChanged
    OnFreeNode = vtTasksFreeNode
    OnGetText = vtTasksGetText
    OnGetNodeDataSize = vtTasksGetNodeDataSize
    OnHeaderClick = vtTasksHeaderClick
    OnInitNode = vtTasksInitNode
    Columns = <
      item
        Position = 0
        Width = 172
        WideText = 'Name'
      end
      item
        Position = 1
        Width = 256
        WideText = 'Path'
      end
      item
        Position = 2
        WideText = 'State'
      end
      item
        Position = 3
        WideText = 'Flags'
      end
      item
        Position = 4
        WideText = 'Source'
      end
      item
        Position = 5
        WideText = 'GUID'
      end
      item
        Position = 6
        WideText = 'Buckets'
      end
      item
        Position = 7
        WideText = 'Presence'
      end
      item
        Position = 8
        WideText = 'LastRunTime'
      end
      item
        Position = 9
        WideText = 'NextRunTime'
      end>
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 308
    Width = 640
    Height = 172
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object mmDetails: TMemo
      Left = 0
      Top = 0
      Width = 640
      Height = 172
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object alActions: TActionList
    Left = 24
    Top = 32
    object aReload: TAction
      Caption = 'Refresh'
      ShortCut = 116
      OnExecute = aReloadExecute
    end
    object aStart: TAction
      Category = 'Tasks'
      Caption = 'Start'
      OnExecute = aStartExecute
    end
    object aStop: TAction
      Category = 'Tasks'
      Caption = 'Stop'
      OnExecute = aStopExecute
    end
    object aEnable: TAction
      Category = 'Tasks'
      Caption = 'Enable'
      OnExecute = aEnableExecute
    end
    object aDisable: TAction
      Category = 'Tasks'
      Caption = 'Disable'
      OnExecute = aDisableExecute
    end
    object aExit: TAction
      Caption = 'Exit'
    end
    object aOpenSchedulerRegistry: TAction
      Category = 'Links'
      Caption = 'HKLM\Schedule\TaskCache'
      OnExecute = aOpenSchedulerRegistryExecute
    end
    object aOpenSchedulerFolder: TAction
      Category = 'Links'
      Caption = 'System32\Tasks'
      OnExecute = aOpenSchedulerFolderExecute
    end
    object aOpenSchedulerMMC: TAction
      Category = 'Links'
      Caption = 'taskschd.mmc'
      OnExecute = aOpenSchedulerMMCExecute
    end
    object aJumpToRegPlain: TAction
      Category = 'Tasks'
      Caption = 'Open registry key'
      OnExecute = aJumpToRegPlainExecute
    end
    object aJumpToRegTree: TAction
      Category = 'Tasks'
      Caption = 'Open registry key (Tree)'
      OnExecute = aJumpToRegTreeExecute
    end
    object aJumpToSystem32Tasks: TAction
      Category = 'Tasks'
      Caption = 'Jump to System32\Tasks'
      OnExecute = aJumpToSystem32TasksExecute
    end
  end
  object pmPopup: TPopupMenu
    Left = 24
    Top = 88
    object Start1: TMenuItem
      Action = aStart
    end
    object Stop1: TMenuItem
      Action = aStop
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
    end
    object RegistryPlainkey1: TMenuItem
      Action = aJumpToRegPlain
    end
    object RegistryTreekey1: TMenuItem
      Action = aJumpToRegTree
    end
    object System32Taskskey1: TMenuItem
      Action = aJumpToSystem32Tasks
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Enable1: TMenuItem
      Action = aEnable
    end
    object Disable1: TMenuItem
      Action = aDisable
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object aReload1: TMenuItem
      Action = aReload
    end
  end
end
