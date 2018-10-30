object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Services'
  ClientHeight = 648
  ClientWidth = 1040
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 180
    Top = 0
    Height = 648
    ExplicitLeft = 194
    ExplicitHeight = 609
  end
  object vtFolders: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 180
    Height = 648
    Align = alLeft
    BorderWidth = 1
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Images = CommonRes.ilImages
    PopupMenu = pmFolders
    StateImages = CommonRes.ilOverlays
    TabOrder = 1
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnChange = vtFoldersChange
    OnCompareNodes = vtFoldersCompareNodes
    OnDragAllowed = vtFoldersDragAllowed
    OnDragOver = vtFoldersDragOver
    OnDragDrop = vtFoldersDragDrop
    OnEdited = vtFoldersEdited
    OnEditing = vtFoldersEditing
    OnFocusChanged = vtFoldersFocusChanged
    OnFreeNode = vtFoldersFreeNode
    OnGetText = vtFoldersGetText
    OnGetImageIndex = vtFoldersGetImageIndex
    OnGetNodeDataSize = vtFoldersGetNodeDataSize
    OnInitNode = vtFoldersInitNode
    OnMouseDown = vtFoldersMouseDown
    OnNewText = vtFoldersNewText
    Columns = <>
  end
  object pnlMain: TPanel
    Left = 183
    Top = 0
    Width = 857
    Height = 648
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter2: TSplitter
      Left = 0
      Top = 405
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
      Top = 408
      Width = 857
      Height = 240
      ActivePage = tsDescription
      Align = alBottom
      Font.Charset = RUSSIAN_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Segoe UI'
      Font.Style = []
      Images = CommonRes.ilImages
      ParentFont = False
      TabOrder = 2
      object tsDescription: TTabSheet
        Caption = 'Info'
        ImageIndex = -1
        object Label1: TLabel
          Left = 0
          Top = 57
          Width = 849
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
          Width = 849
          Height = 3
          Cursor = crVSplit
          Align = alTop
          ExplicitLeft = -1
          ExplicitTop = 51
        end
        object mmDetails: TMemo
          Left = 0
          Top = 0
          Width = 849
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
          Width = 849
          Height = 138
          Align = alClient
          TabOrder = 1
          ExplicitTop = 70
          ExplicitWidth = 849
          ExplicitHeight = 138
          inherited mmNotes: TRichEdit
            Width = 849
            Height = 138
            Font.Height = -13
            Font.Name = 'Segoe UI'
            ExplicitWidth = 849
            ExplicitHeight = 138
          end
        end
      end
      object tsDependencies: TTabSheet
        Caption = 'Depends on'
        ImageIndex = -1
        OnShow = tsDependenciesShow
        inline DependencyList: TServiceList
          Left = 0
          Top = 0
          Width = 849
          Height = 208
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 849
          ExplicitHeight = 208
          inherited vtServices: TVirtualStringTree
            Width = 849
            Height = 208
            TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
            ExplicitWidth = 849
            ExplicitHeight = 208
          end
        end
      end
      object tsDependents: TTabSheet
        Caption = 'Required by'
        ImageIndex = -1
        OnShow = tsDependentsShow
        inline DependentsList: TServiceList
          Left = 0
          Top = 0
          Width = 849
          Height = 208
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 849
          ExplicitHeight = 208
          inherited vtServices: TVirtualStringTree
            Width = 849
            Height = 208
            TreeOptions.PaintOptions = [toHideFocusRect, toShowButtons, toShowRoot, toThemeAware, toUseBlendedImages]
            ExplicitWidth = 849
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
          Width = 849
          Height = 208
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 849
          ExplicitHeight = 208
          inherited Tree: TVirtualStringTree
            Width = 849
            Height = 208
            ExplicitWidth = 849
            ExplicitHeight = 208
            Columns = <
              item
                Position = 0
                Width = 293
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
    inline MainServiceList: TServiceList
      Left = 0
      Top = 21
      Width = 857
      Height = 384
      Align = alClient
      TabOrder = 1
      ExplicitTop = 21
      ExplicitWidth = 857
      ExplicitHeight = 384
      inherited vtServices: TVirtualStringTree
        Width = 857
        Height = 384
        TreeOptions.MiscOptions = [toEditable, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
        OnCreateEditor = MainServiceListvtServicesCreateEditor
        OnDragAllowed = MainServiceListvtServicesDragAllowed
        OnEditing = MainServiceListvtServicesEditing
        OnFocusChanged = MainServiceListvtServicesFocusChanged
        OnFocusChanging = MainServiceListvtServicesFocusChanging
        OnKeyAction = MainServiceListvtServicesKeyAction
        OnNewText = MainServiceListvtServicesNewText
        ExplicitWidth = 857
        ExplicitHeight = 384
      end
      inherited pmServices: TPopupMenu
        object miRenameService: TMenuItem [0]
          Action = aRenameService
        end
        object miRemoveServiceFromFolder: TMenuItem [1]
          Action = aRemoveServiceFromFolder
        end
        object N6: TMenuItem [2]
          Caption = '-'
        end
        object N1: TMenuItem [16]
          Caption = '-'
        end
        inherited Advanced1: TMenuItem
          object Saveserviceconfig2: TMenuItem [0]
            Action = aSaveSelectedServicesConfig
          end
        end
        object Refresh2: TMenuItem
          Action = aRefresh
        end
      end
    end
    object edtQuickFilter: TEdit
      Left = 0
      Top = 0
      Width = 857
      Height = 21
      Align = alTop
      TabOrder = 0
      OnChange = edtQuickFilterChange
      OnKeyDown = edtQuickFilterKeyDown
    end
  end
  object ActionList: TActionList
    Left = 24
    Top = 16
    object aEditFolders: TAction
      Category = 'Folders'
      AutoCheck = True
      Caption = 'Edit folders'
      ShortCut = 16497
      OnExecute = aEditFoldersExecute
    end
    object aClose: TAction
      Caption = 'Close'
      OnExecute = aCloseExecute
    end
    object aRefresh: TAction
      Caption = 'Refresh'
      ShortCut = 116
      OnExecute = aRefreshExecute
    end
    object aRestartAsAdmin: TAction
      Caption = 'Restart as administrator...'
      OnExecute = aRestartAsAdminExecute
    end
    object aHideEmptyFolders: TAction
      Category = 'Folders'
      AutoCheck = True
      Caption = 'Hide empty folders'
      Checked = True
      OnExecute = aHideEmptyFoldersExecute
    end
    object aShowDrivers: TAction
      Category = 'Service filter'
      AutoCheck = True
      Caption = 'Show drivers'
      OnExecute = aShowDriversExecute
    end
    object aIncludeSubfolders: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Include subfolders contents'
      Hint = 'Show each folder with all of its subfolders contents included'
      OnExecute = aIncludeSubfoldersExecute
    end
    object aSaveAllServicesConfig: TAction
      Category = 'View'
      Caption = 'Save service config...'
      Hint = 'Save the startup configuration of services on this computer'
      OnExecute = aSaveAllServicesConfigExecute
    end
    object aSaveSelectedServicesConfig: TAction
      Caption = 'Save service config...'
      Hint = 'Save the startup configuration of selected services'
      OnExecute = aSaveSelectedServicesConfigExecute
    end
    object aRestoreServiceConfig: TAction
      Category = 'View'
      Caption = 'Restore service config...'
      Hint = 'Restore the startup configuration of services to a saved one'
      OnExecute = aRestoreServiceConfigExecute
    end
    object aAddFolder: TAction
      Category = 'Folders'
      Caption = 'Add...'
      Visible = False
      OnExecute = aAddFolderExecute
    end
    object aRenameFolder: TAction
      Category = 'Folders'
      Caption = 'Rename'
      Visible = False
      OnExecute = aRenameFolderExecute
    end
    object aDeleteFolder: TAction
      Category = 'Folders'
      Caption = 'Delete'
      Visible = False
      OnExecute = aDeleteFolderExecute
    end
    object aRenameService: TAction
      Category = 'Services'
      Caption = 'Rename'
      Visible = False
      OnExecute = aRenameServiceExecute
    end
    object aRemoveServiceFromFolder: TAction
      Category = 'Services'
      Caption = 'Remove from folder'
      Visible = False
      OnExecute = aRemoveServiceFromFolderExecute
    end
    object aSaveNotes: TAction
      Category = 'Services'
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = aSaveNotesExecute
    end
    object aEditServiceNotes: TAction
      Category = 'Services'
      AutoCheck = True
      Caption = 'Edit notes'
      ShortCut = 115
      OnExecute = aEditServiceNotesExecute
    end
    object aConfigureColors: TAction
      Category = 'View'
      Caption = 'Configure colors'
      OnExecute = aConfigureColorsExecute
    end
    object aShowUserPrototypes: TAction
      Category = 'Service filter'
      AutoCheck = True
      Caption = 'Show prototypes'
      Hint = 'Show prototype services for user services'
      OnExecute = aShowUserPrototypesExecute
    end
    object aRunServicesMsc: TAction
      Caption = 'services.msc'
      OnExecute = aRunServicesMscExecute
    end
    object aSettings: TAction
      Caption = 'Settings'
      OnExecute = aSettingsExecute
    end
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 72
    object File1: TMenuItem
      Caption = 'File'
      object Refresh1: TMenuItem
        Action = aRefresh
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Restartasadministrator1: TMenuItem
        Action = aRestartAsAdmin
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Saveserviceconfig1: TMenuItem
        Action = aSaveAllServicesConfig
      end
      object Restoreserviceconfig1: TMenuItem
        Action = aRestoreServiceConfig
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Close1: TMenuItem
        Action = aClose
      end
    end
    object miEdit: TMenuItem
      Caption = 'Edit'
      object Editfolders1: TMenuItem
        Action = aEditFolders
        AutoCheck = True
      end
      object Editnotes1: TMenuItem
        Action = aEditServiceNotes
        AutoCheck = True
      end
    end
    object miView: TMenuItem
      Caption = 'View'
      object miShowDrivers: TMenuItem
        Action = aShowDrivers
        AutoCheck = True
        GroupIndex = 10
      end
      object miShowPrototypes: TMenuItem
        Action = aShowUserPrototypes
        AutoCheck = True
        GroupIndex = 10
      end
      object miUseColors: TMenuItem
        Action = MainServiceList.aUseColors
        AutoCheck = True
        GroupIndex = 10
      end
      object N7: TMenuItem
        Caption = '-'
        GroupIndex = 10
      end
      object miConfigureColors: TMenuItem
        Action = aConfigureColors
        GroupIndex = 10
      end
      object N8: TMenuItem
        Caption = '-'
        GroupIndex = 10
      end
      object cbHideEmptyFolders: TMenuItem
        Action = aHideEmptyFolders
        AutoCheck = True
        GroupIndex = 10
      end
      object Includesubfolderscontents1: TMenuItem
        Action = aIncludeSubfolders
        AutoCheck = True
        GroupIndex = 10
      end
    end
    object miTools: TMenuItem
      Caption = 'Tools'
      object miSettings: TMenuItem
        Action = aSettings
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object miRunServicesMsc: TMenuItem
        Action = aRunServicesMsc
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object miShowLog: TMenuItem
        Caption = 'Show log'
        OnClick = miShowLogClick
      end
    end
  end
  object pmFolders: TPopupMenu
    Left = 24
    Top = 128
    object miAddFolder: TMenuItem
      Action = aAddFolder
    end
    object miRenameFolder: TMenuItem
      Action = aRenameFolder
    end
    object miDeleteFolder: TMenuItem
      Action = aDeleteFolder
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object miEditFolders: TMenuItem
      Action = aEditFolders
      AutoCheck = True
    end
    object miHideEmptyFolders: TMenuItem
      Action = aHideEmptyFolders
      AutoCheck = True
    end
  end
  object OpenServiceConfigDialog: TOpenDialog
    DefaultExt = '*.svcconf'
    Filter = 
      'Service configuration files (*.svcconf)|*.svcconf|All files (*.*' +
      ')|*.*'
    Title = 'Open service configuration file'
    Left = 24
    Top = 184
  end
  object SaveServiceConfigDialog: TSaveDialog
    DefaultExt = '*.svcconf'
    Filter = 
      'Service configuration files (*.svcconf)|*.svcconf|All files (*.*' +
      ')|*.*'
    Title = 'Save service configuration file'
    Left = 24
    Top = 240
  end
end
