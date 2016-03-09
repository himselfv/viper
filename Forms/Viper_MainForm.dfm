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
    TabOrder = 0
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
  object pnlMain: TPanel
    Left = 203
    Top = 0
    Width = 837
    Height = 609
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 453
      Width = 837
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 568
    end
    object vtServices: TVirtualStringTree
      Left = 0
      Top = 0
      Width = 837
      Height = 453
      Align = alClient
      BorderWidth = 1
      ChangeDelay = 50
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
      TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toMultiSelect, toRightClickSelect, toSimpleDrawSelection]
      OnBeforeItemErase = vtServicesBeforeItemErase
      OnChange = vtServicesChange
      OnCompareNodes = vtServicesCompareNodes
      OnFocusChanged = vtServicesFocusChanged
      OnGetText = vtServicesGetText
      OnPaintText = vtServicesPaintText
      OnGetImageIndex = vtServicesGetImageIndex
      OnGetNodeDataSize = vtServicesGetNodeDataSize
      OnHeaderClick = vtServicesHeaderClick
      OnInitNode = vtServicesInitNode
      OnKeyDown = vtServicesKeyDown
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
        end
        item
          Position = 4
          Width = 200
          WideText = #1054#1087#1080#1089#1072#1085#1080#1077
        end
        item
          Position = 5
          Width = 200
          WideText = #1048#1089#1087#1086#1083#1085#1103#1077#1084#1099#1081' '#1092#1072#1081#1083
        end>
    end
    object pnlDetails: TPanel
      Left = 0
      Top = 456
      Width = 837
      Height = 153
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object mmDetails: TMemo
        Left = 0
        Top = 0
        Width = 837
        Height = 153
        Align = alClient
        ReadOnly = True
        TabOrder = 0
      end
    end
  end
  object ActionList: TActionList
    Left = 24
    Top = 16
    object aReload: TAction
      Caption = 'Reload'
      OnExecute = aReloadExecute
    end
    object aStartService: TAction
      Category = 'ServiceControl'
      Caption = 'Start'
      OnExecute = aStartServiceExecute
    end
    object aForceStartService: TAction
      Category = 'ServiceControl'
      Caption = 'Start (force)'
      Hint = 'Start services even when they are disabled'
      OnExecute = aForceStartServiceExecute
    end
    object aStopService: TAction
      Category = 'ServiceControl'
      Caption = 'Stop'
      OnExecute = aStopServiceExecute
    end
    object aPauseService: TAction
      Category = 'ServiceControl'
      Caption = 'Pause'
      OnExecute = aPauseServiceExecute
    end
    object aResumeService: TAction
      Category = 'ServiceControl'
      Caption = 'Resume'
      OnExecute = aResumeServiceExecute
    end
    object aRestartService: TAction
      Category = 'ServiceControl'
      Caption = 'Restart'
      OnExecute = aRestartServiceExecute
    end
    object aStartTypeAutomatic: TAction
      Category = 'StartType'
      AutoCheck = True
      Caption = 'Automatic'
      GroupIndex = 1
      OnExecute = aStartTypeAutomaticExecute
    end
    object aStartTypeManual: TAction
      Category = 'StartType'
      AutoCheck = True
      Caption = 'Manual'
      GroupIndex = 1
      OnExecute = aStartTypeManualExecute
    end
    object aStartTypeDisabled: TAction
      Category = 'StartType'
      AutoCheck = True
      Caption = 'Disabled'
      GroupIndex = 1
      OnExecute = aStartTypeDisabledExecute
    end
    object aDeleteService: TAction
      Category = 'ServiceSetup'
      Caption = 'Delete service'
    end
    object aExportService: TAction
      Category = 'ServiceSetup'
      Caption = 'Export to .reg...'
    end
    object aRefresh: TAction
      Caption = 'Refresh'
      ShortCut = 116
      OnExecute = aRefreshExecute
    end
    object aHideEmptyFolders: TAction
      AutoCheck = True
      Caption = 'Hide empty folders'
      Checked = True
      OnExecute = aHideEmptyFoldersExecute
    end
    object aColorByStartType: TAction
      Category = 'Coloring'
      AutoCheck = True
      Caption = 'By start type'
      Checked = True
      OnExecute = aColorByStartTypeExecute
    end
    object aShowDrivers: TAction
      AutoCheck = True
      Caption = 'Show drivers'
      OnExecute = aReloadExecute
    end
    object aColorByStatus: TAction
      Category = 'Coloring'
      AutoCheck = True
      Caption = 'By status'
      Checked = True
      OnExecute = aColorByStartTypeExecute
    end
    object aCopyServiceID: TAction
      Category = 'Copy'
      Caption = 'Name'
      OnExecute = aCopyServiceIDExecute
    end
    object aCopyServiceName: TAction
      Category = 'Copy'
      Caption = 'Visible name'
      OnExecute = aCopyServiceNameExecute
    end
    object aCopyServiceDescription: TAction
      Category = 'Copy'
      Caption = 'Description'
      OnExecute = aCopyServiceDescriptionExecute
    end
    object aCopyServiceShortSummary: TAction
      Category = 'Copy'
      Caption = 'Short summary'
      OnExecute = aCopyServiceShortSummaryExecute
    end
    object aCopyExecutableFilename: TAction
      Category = 'Copy'
      Caption = 'Executable file name'
      OnExecute = aCopyExecutableFilenameExecute
    end
    object aJumpToBinary: TAction
      Category = 'JumpTo'
      Caption = 'Jump to file'
      OnExecute = aJumpToBinaryExecute
    end
    object aJumpToRegistry: TAction
      Category = 'JumpTo'
      Caption = 'Open registry key'
      OnExecute = aJumpToRegistryExecute
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
    object File1: TMenuItem
      Caption = 'File'
      object Refresh1: TMenuItem
        Action = aRefresh
      end
      object Reload2: TMenuItem
        Action = aReload
      end
    end
    object Settings1: TMenuItem
      Caption = 'Settings'
      object Showdrivers1: TMenuItem
        Action = aShowDrivers
        AutoCheck = True
      end
      object cbHideEmptyFolders: TMenuItem
        Action = aHideEmptyFolders
        AutoCheck = True
      end
      object Colorize1: TMenuItem
        Caption = 'Colorize'
        object Bystarttype1: TMenuItem
          Action = aColorByStartType
          AutoCheck = True
        end
        object Bystatus1: TMenuItem
          Action = aColorByStatus
          AutoCheck = True
        end
      end
    end
  end
  object pmServices: TPopupMenu
    OnPopup = pmServicesPopup
    Left = 232
    Top = 40
    object pmStartService: TMenuItem
      Action = aStartService
    end
    object pmForceStartService: TMenuItem
      Action = aForceStartService
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
    object Jumptobinary1: TMenuItem
      Action = aJumpToBinary
    end
    object Openregistrykey1: TMenuItem
      Action = aJumpToRegistry
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
      object Ident1: TMenuItem
        Action = aCopyServiceID
      end
      object Name1: TMenuItem
        Action = aCopyServiceName
      end
      object Description1: TMenuItem
        Action = aCopyServiceDescription
      end
      object Executablefilename1: TMenuItem
        Action = aCopyExecutableFilename
      end
    end
    object N3: TMenuItem
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
      Action = aRefresh
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
