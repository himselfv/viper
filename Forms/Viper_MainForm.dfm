object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Services'
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
    Images = CommonRes.ilImages
    PopupMenu = pmFolders
    StateImages = CommonRes.ilOverlays
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
  object pnlMain: TPanel
    Left = 183
    Top = 0
    Width = 857
    Height = 609
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
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
      ActivePage = tsDescription
      Align = alBottom
      TabOrder = 2
      object tsDescription: TTabSheet
        Caption = 'Info'
        object Label1: TLabel
          Left = 0
          Top = 49
          Width = 849
          Height = 13
          Align = alTop
          Caption = 'Additional info:'
          ExplicitWidth = 72
        end
        object mmDetails: TMemo
          Left = 0
          Top = 0
          Width = 849
          Height = 49
          Align = alTop
          BorderStyle = bsNone
          ParentColor = True
          ReadOnly = True
          TabOrder = 0
        end
        object mmAdditionalInfo: TMemo
          Left = 0
          Top = 62
          Width = 849
          Height = 103
          Align = alClient
          BorderStyle = bsNone
          ParentColor = True
          ReadOnly = True
          TabOrder = 1
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
        inline TriggerList: TTriggerList
          Left = 0
          Top = 0
          Width = 849
          Height = 165
          Align = alClient
          TabOrder = 0
          ExplicitWidth = 849
          ExplicitHeight = 165
          inherited vtTriggers: TVirtualStringTree
            Width = 849
            Height = 165
            ExplicitWidth = 849
            ExplicitHeight = 165
          end
          inherited ilImages: TImageList
            Bitmap = {
              494C010102000800680010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
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
      Top = 21
      Width = 857
      Height = 392
      Align = alClient
      TabOrder = 1
      ExplicitTop = 21
      ExplicitWidth = 857
      ExplicitHeight = 392
      inherited vtServices: TVirtualStringTree
        Width = 857
        Height = 392
        OnFocusChanged = MainServiceListvtServicesFocusChanged
        ExplicitWidth = 857
        ExplicitHeight = 392
      end
      inherited pmServices: TPopupMenu
        object N1: TMenuItem [13]
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
    end
  end
  object ActionList: TActionList
    Left = 24
    Top = 16
    object aClose: TAction
      Caption = 'Close'
      OnExecute = aCloseExecute
    end
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
  end
  object ilImages: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 24
    Top = 72
    Bitmap = {
      494C010102000800600010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
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
      object Reload2: TMenuItem
        Action = aReload
      end
      object N2: TMenuItem
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
      object Includesubfolderscontents1: TMenuItem
        Action = aIncludeSubfolders
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
    Top = 184
    object Hideemptyfolders1: TMenuItem
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
