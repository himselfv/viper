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
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toThemeAware, toUseBlendedImages, toUseExplorerTheme]
    TreeOptions.SelectionOptions = [toFullRowSelect, toRightClickSelect]
    OnChange = vtFoldersChange
    OnCollapsing = vtFoldersCollapsing
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
    object pnlViewHost: TPanel
      Left = 0
      Top = 21
      Width = 857
      Height = 627
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      inline MainServiceList: TMainServiceList
        Left = 0
        Top = 0
        Width = 857
        Height = 627
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 857
        ExplicitHeight = 627
        inherited Splitter2: TSplitter
          Top = 384
          Width = 857
          ExplicitTop = 0
        end
        inherited vtServices: TVirtualStringTree
          Width = 857
          Height = 384
          TreeOptions.MiscOptions = [toEditable, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
          OnCreateEditor = MainServiceListvtServicesCreateEditor
          OnDragAllowed = MainServiceListvtServicesDragAllowed
          OnEditing = MainServiceListvtServicesEditing
          OnKeyAction = MainServiceListvtServicesKeyAction
          OnNewText = MainServiceListvtServicesNewText
          ExplicitWidth = 857
          ExplicitHeight = 0
        end
        inherited pcBottom: TPageControl
          Top = 387
          Width = 857
          inherited tsDescription: TTabSheet
            ExplicitWidth = 849
            inherited Label1: TLabel
              Width = 849
            end
            inherited Splitter3: TSplitter
              Width = 849
            end
            inherited mmDetails: TMemo
              Width = 849
            end
            inherited NotesFrame: TRichEditFrame
              Width = 849
              ExplicitWidth = 849
              inherited mmNotes: TRichEdit
                Width = 849
                ExplicitWidth = 849
              end
            end
          end
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
          inherited miAdvancedSubmenu: TMenuItem
            object Saveserviceconfig2: TMenuItem [0]
              Action = aSaveSelectedServicesConfig
            end
          end
          object Refresh2: TMenuItem
            Action = aRefresh
          end
        end
      end
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
    object aConfigureColors: TAction
      Category = 'View'
      Caption = 'Configure colors'
      OnExecute = aConfigureColorsExecute
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
      object miImportServices: TMenuItem
        Action = MainServiceList.aImportServices
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
        Action = MainServiceList.aEditServiceNotes
        AutoCheck = True
      end
    end
    object miView: TMenuItem
      Caption = 'View'
      object miShowDrivers: TMenuItem
        Action = MainServiceList.aShowDrivers
        AutoCheck = True
        GroupIndex = 10
      end
      object miShowPrototypes: TMenuItem
        Action = MainServiceList.aShowUserPrototypes
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
      object miQueryLocalRPC: TMenuItem
        Caption = 'Get local RPC interface name'
        OnClick = miQueryLocalRPCClick
      end
      object miQueryLocalCOM: TMenuItem
        Caption = 'Get local COM interface name'
        OnClick = miQueryLocalCOMClick
      end
      object miDumpLocalRPC: TMenuItem
        Caption = 'Dump local RPC'
        OnClick = miDumpLocalRPCClick
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
