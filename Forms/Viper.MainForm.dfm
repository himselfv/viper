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
        inline TriggerList: TTriggerList
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
                Width = 137
                WideText = 'Action'
              end
              item
                Position = 1
                Width = 412
                WideText = 'Trigger'
              end
              item
                Position = 2
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
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
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
      AutoCheck = True
      Caption = 'Include subfolders contents'
      Hint = 'Show each folder with all of its subfolders contents included'
      OnExecute = aIncludeSubfoldersExecute
    end
    object aSaveAllServicesConfig: TAction
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
  end
  object ilImages: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 24
    Top = 72
    Bitmap = {
      494C010102000800940010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000018572800164F
      2500164F2500164F2500154C2300154C2300154C2300154C2300154C2300154C
      2300154C23000000000000000000000000000000000000000000000000000000
      000038386A002F2F85002727790027277900272779002F2F85001A1A59000000
      0000FDFDFD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001C672F0080B48E00CBEF
      D400B8E9C500B8E9C500B8E9C500B8E9C500A1E2B200A1E2B200A1E2B200AFE6
      BD0080B48E00154C230000000000000000000000000000000000000000003F3F
      5F007777ED006565FD005F5FF1005F5FF1005F5FF1005F5FF1006565FD002727
      790000000000FDFDFD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001C672F00F4FBF6004DD1
      77004BCF740049CD710047CB6D0044C9690041C665003EC25F003EC25F003EC2
      5F00AFE6BD00154C2300000000000000000000000000000000003F3F5F007777
      ED005656D8005353D4005353D4005353D4005353D4005353D4005353D4006A6A
      FE00272779000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000021743600E8F8EC004FD3
      7A004DD177004BCF740049CD710047CB6D0041C6650041C665003EC25F003EC2
      5F00A1E2B200154C2300000000000000000000000000424267009595FC005555
      D7005555D7005555D7005555D7005555D7005555D7005555D7005656D8005353
      D4006A6AFE001A1A590000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000026873E00E8F8EC0052D5
      7D004FD37A004DD177000000000049CD710044C9690044C9690041C665003EC2
      5F00A1E2B200154C230000000000000000000000000042426700B4B4F6006363
      E1006363E100000000007878FF005E5EDE005E5EDE00000000005E5EDE005E5E
      DE006A6AFE001A1A590000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000026873E00E8F8EC0054D6
      800052D57D004FD37A00000000000000000047CB6D0044C9690041C6650041C6
      6500A1E2B200154C230000000000000000000000000042426700B4B4F6006A6A
      E6006363E1003B3BD800000000007878FF00000000005E5EDE005E5EDE005E5E
      DE006A6AFE001A1A590000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002CA14900E8F8EC0056D9
      850056D9850054D6800000000000000000000000000049CD710047CB6D0044C9
      6900B8E9C500164F250000000000000000000000000042426700B4B4F6006A6A
      E6006A6AE6006A6AE6003B3BD800000000007878FF006363E1006363E1005E5E
      DE006A6AFE001A1A590000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002CA14900E8F8EC0059DB
      890056D9850056D9850000000000000000004DD177004BCF740049CD710047CB
      6D00B8E9C500164F250000000000000000000000000042426700B4B4F6006F6F
      E8006F6FE8006F6FE800000000003B3BD800000000007878FF006363E1006363
      E1006A6AFE0038386A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002FAA4E00E8F8EC005ADC
      8A0059DB890056D985000000000054D680004FD37A004DD177004BCF740049CD
      7100B8E9C5001857280000000000000000000000000042426700B4B4F6007474
      EB007474EB00000000006F6FE8006F6FE8003B3BD800000000006363E1006363
      E1007878FF0038386A0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002FAA4E00E8F8EC005CDE
      8E005ADC8A0059DB890056D9850056D9850052D57D004FD37A004DD177004BCF
      7400B8E9C5001857280000000000000000000000000042426700D8D8FE007777
      ED007777ED007474EB007474EB006F6FE8006F6FE8006A6AE6006A6AE6006363
      E1007878FF001A1A590000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002FAA4E00F4FBF6005CDE
      8E005CDE8E005ADC8A0059DB890056D9850054D6800052D57D004FD37A004DD1
      7700CBEFD4001C672F00000000000000000000000000000000005C5C8B00D8D8
      FE007777ED007777ED007474EB007474EB006F6FE8006F6FE8006A6AE6007878
      FF002F2F85000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002FAA4E0080B48E00F4FB
      F600E8F8EC00E8F8EC00E8F8EC00E8F8EC00E8F8EC00E8F8EC00E8F8EC00F4FB
      F600A1E2B2002174360000000000000000000000000000000000000000005C5C
      8B00D8D8FE00C9C9FF00C9C9FF00A6A6FE009595FC009595FC009595FC002F2F
      8500000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000002FAA4E002FAA
      4E002FAA4E002FAA4E002FAA4E002FAA4E002CA149002CA14900289444002894
      440026873E000000000000000000000000000000000000000000000000000000
      000046467A008C8C98008C8C98008C8C98006D6D93006D6D930046467A000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000FFFFFFFF00000000
      C007F017000000008003E00B000000008003C007000000008003800300000000
      8203844300000000830382830000000083838103000000008303828300000000
      820384430000000080038003000000008003C007000000008003E00F00000000
      C007F01F00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object MainMenu: TMainMenu
    Left = 24
    Top = 128
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
    object Settings1: TMenuItem
      Caption = 'Settings'
      object Showdrivers1: TMenuItem
        Action = aShowDrivers
        AutoCheck = True
      end
      object Showprototypes1: TMenuItem
        Action = aShowUserPrototypes
        AutoCheck = True
      end
      object cbHideEmptyFolders: TMenuItem
        Action = aHideEmptyFolders
        AutoCheck = True
      end
      object Includesubfolderscontents1: TMenuItem
        Action = aIncludeSubfolders
        AutoCheck = True
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Bystatus1: TMenuItem
        Action = MainServiceList.aUseColors
        AutoCheck = True
      end
      object Configurecolors1: TMenuItem
        Action = aConfigureColors
      end
    end
    object Debug1: TMenuItem
      Caption = 'Debug'
      object miShowLog: TMenuItem
        Caption = 'Show log'
        OnClick = miShowLogClick
      end
      object Alltriggers1: TMenuItem
        Caption = 'Trigger browser'
        OnClick = Alltriggers1Click
      end
    end
  end
  object pmFolders: TPopupMenu
    Left = 24
    Top = 184
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
    Top = 240
  end
  object SaveServiceConfigDialog: TSaveDialog
    DefaultExt = '*.svcconf'
    Filter = 
      'Service configuration files (*.svcconf)|*.svcconf|All files (*.*' +
      ')|*.*'
    Title = 'Save service configuration file'
    Left = 24
    Top = 296
  end
end
