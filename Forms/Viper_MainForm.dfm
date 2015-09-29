object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = #1057#1083#1091#1078#1073#1099
  ClientHeight = 609
  ClientWidth = 1040
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 200
    Top = 0
    Height = 609
    ExplicitLeft = 194
  end
  object vtServices: TVirtualStringTree
    Left = 203
    Top = 0
    Width = 837
    Height = 609
    Align = alClient
    BorderWidth = 1
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -13
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 24
    Header.Options = [hoColumnResize, hoDrag, hoHotTrack, hoShowSortGlyphs, hoVisible]
    Images = ilImages
    ParentFont = False
    PopupMenu = pmServices
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toHideFocusRect, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toRightClickSelect, toSimpleDrawSelection]
    OnCompareNodes = vtServicesCompareNodes
    OnFocusChanged = vtServicesFocusChanged
    OnGetText = vtServicesGetText
    OnGetImageIndex = vtServicesGetImageIndex
    OnGetNodeDataSize = vtServicesGetNodeDataSize
    OnHeaderClick = vtServicesHeaderClick
    OnInitNode = vtServicesInitNode
    Columns = <
      item
        Position = 0
        Width = 150
        WideText = 'ID'
      end
      item
        Position = 1
        Width = 500
        WideText = #1053#1072#1079#1074#1072#1085#1080#1077
      end
      item
        Position = 2
        Width = 150
        WideText = #1057#1086#1089#1090#1086#1103#1085#1080#1077
      end
      item
        Position = 3
        Width = 150
        WideText = #1058#1080#1087' '#1079#1072#1087#1091#1089#1082#1072
      end>
  end
  object vtFolders: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 200
    Height = 609
    Align = alLeft
    BorderWidth = 1
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Images = ilImages
    PopupMenu = pmFolders
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnFocusChanged = vtFoldersFocusChanged
    OnFreeNode = vtFoldersFreeNode
    OnGetText = vtFoldersGetText
    OnGetImageIndex = vtFoldersGetImageIndex
    OnGetNodeDataSize = vtFoldersGetNodeDataSize
    OnInitNode = vtFoldersInitNode
    Columns = <>
  end
  object ActionList: TActionList
    Left = 24
    Top = 16
    object aReload: TAction
      Caption = 'Reload'
      ShortCut = 116
      OnExecute = aReloadExecute
    end
    object aStartService: TAction
      Category = 'ServiceControl'
      Caption = 'Start'
    end
    object aStopService: TAction
      Category = 'ServiceControl'
      Caption = 'Stop'
    end
    object aPauseService: TAction
      Category = 'ServiceControl'
      Caption = 'Pause'
    end
    object aResumeService: TAction
      Category = 'ServiceControl'
      Caption = 'Resume'
    end
    object aRestartService: TAction
      Category = 'ServiceControl'
      Caption = 'Restart'
    end
    object aStartTypeAutomatic: TAction
      Category = 'StartType'
      AutoCheck = True
      Caption = 'Automatic'
      GroupIndex = 1
    end
    object aStartTypeManual: TAction
      Category = 'StartType'
      AutoCheck = True
      Caption = 'Manual'
      GroupIndex = 1
    end
    object aStartTypeDisabled: TAction
      Category = 'StartType'
      AutoCheck = True
      Caption = 'Disabled'
      GroupIndex = 1
    end
    object aDeleteService: TAction
      Category = 'ServiceSetup'
      Caption = 'Delete service'
    end
    object aExportService: TAction
      Category = 'ServiceSetup'
      Caption = 'Export to .reg...'
    end
    object aHideEmptyFolders: TAction
      AutoCheck = True
      Caption = 'Hide empty folders'
      Checked = True
      OnExecute = aHideEmptyFoldersExecute
    end
  end
  object ilImages: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 24
    Top = 72
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 128
    object Settings1: TMenuItem
      Caption = 'Settings'
      object cbHideEmptyFolders: TMenuItem
        Action = aHideEmptyFolders
        AutoCheck = True
      end
    end
  end
  object pmServices: TPopupMenu
    Left = 232
    Top = 40
    object Start1: TMenuItem
      Action = aStartService
    end
    object Stop1: TMenuItem
      Action = aStopService
    end
    object Pause1: TMenuItem
      Action = aPauseService
    end
    object Resume1: TMenuItem
      Action = aResumeService
    end
    object Restart1: TMenuItem
      Action = aRestartService
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miStartType: TMenuItem
      Caption = 'Start type'
      object Automatic1: TMenuItem
        Action = aStartTypeAutomatic
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
      object Manual1: TMenuItem
        Action = aStartTypeManual
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
      object Disabled1: TMenuItem
        Action = aStartTypeDisabled
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
    end
    object Advanced1: TMenuItem
      Caption = 'Advanced'
      object Exporttoreg1: TMenuItem
        Action = aExportService
      end
      object Deleteservice1: TMenuItem
        Action = aDeleteService
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Reload1: TMenuItem
      Action = aReload
    end
  end
  object pmFolders: TPopupMenu
    Left = 24
    Top = 184
    object Hideemptyfolders1: TMenuItem
      Action = aHideEmptyFolders
      AutoCheck = True
    end
  end
end
