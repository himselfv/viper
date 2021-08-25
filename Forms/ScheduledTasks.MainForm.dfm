object ScheduledTasksMainForm: TScheduledTasksMainForm
  Left = 0
  Top = 0
  Caption = 'ScheduledTasksMainForm'
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
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
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
      Caption = 'aReload'
      ShortCut = 116
      OnExecute = aReloadExecute
    end
  end
end
