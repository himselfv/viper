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
    Left = 180
    Top = 0
    Height = 609
    ExplicitLeft = 194
  end
  object vtFolders: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 180
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
    Left = 183
    Top = 0
    Width = 857
    Height = 609
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 413
      Width = 857
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 3
      ExplicitTop = 251
      ExplicitWidth = 837
    end
    object pcBottom: TPageControl
      Left = 0
      Top = 416
      Width = 857
      Height = 193
      ActivePage = tsTriggers
      Align = alBottom
      TabOrder = 0
      object tsDescription: TTabSheet
        Caption = 'Info'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object mmDetails: TMemo
          Left = 0
          Top = 0
          Width = 849
          Height = 165
          Align = alClient
          ReadOnly = True
          TabOrder = 0
        end
      end
      object tsDependencies: TTabSheet
        Caption = 'Depends on'
        ImageIndex = 1
        OnShow = tsDependenciesShow
        inline DependencyList: TServiceList
          Left = 0
          Top = 0
          Width = 849
          Height = 165
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 849
          ExplicitHeight = 165
          inherited vtServices: TVirtualStringTree
            Width = 849
            Height = 165
            TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
            ExplicitWidth = 849
            ExplicitHeight = 165
          end
        end
      end
      object tsDependents: TTabSheet
        Caption = 'Required by'
        ImageIndex = 2
        OnShow = tsDependentsShow
        inline DependentsList: TServiceList
          Left = 0
          Top = 0
          Width = 849
          Height = 165
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 849
          ExplicitHeight = 165
          inherited vtServices: TVirtualStringTree
            Width = 849
            Height = 165
            TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
            ExplicitWidth = 849
            ExplicitHeight = 165
          end
        end
      end
      object tsTriggers: TTabSheet
        Caption = 'Triggers'
        ImageIndex = 4
        OnShow = tsTriggersShow
        object lbTriggers: TListBox
          Left = 0
          Top = 0
          Width = 849
          Height = 165
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object tsOperations: TTabSheet
        Caption = 'Operations'
        ImageIndex = 3
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
      end
    end
    inline MainServiceList: TServiceList
      Left = 0
      Top = 0
      Width = 857
      Height = 413
      Align = alClient
      TabOrder = 1
      ExplicitWidth = 857
      ExplicitHeight = 413
      inherited vtServices: TVirtualStringTree
        Width = 857
        Height = 413
        OnFocusChanged = MainServiceListvtServicesFocusChanged
        ExplicitWidth = 857
        ExplicitHeight = 413
      end
      inherited pmServices: TPopupMenu
        object N1: TMenuItem
          Caption = '-'
        end
        object Refresh2: TMenuItem
          Action = aRefresh
        end
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
    object aShowDrivers: TAction
      AutoCheck = True
      Caption = 'Show drivers'
      OnExecute = aReloadExecute
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
          Action = MainServiceList.aColorByStartType
          AutoCheck = True
        end
        object Bystatus1: TMenuItem
          Action = MainServiceList.aColorByStatus
          AutoCheck = True
        end
      end
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
