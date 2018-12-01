object ServiceList: TServiceList
  Left = 0
  Top = 0
  Width = 746
  Height = 447
  TabOrder = 0
  object vtServices: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 746
    Height = 447
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
    Images = CommonRes.ilImages
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
    OnFreeNode = vtServicesFreeNode
    OnGetText = vtServicesGetText
    OnPaintText = vtServicesPaintText
    OnGetImageIndexEx = vtServicesGetImageIndexEx
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
        WideText = 'Name'
      end
      item
        Position = 2
        Width = 125
        WideText = 'Status'
      end
      item
        Position = 3
        Width = 125
        WideText = 'Start mode'
      end
      item
        Position = 4
        Width = 65
        WideText = 'Triggers'
      end
      item
        Position = 5
        Width = 200
        WideText = 'Description'
      end
      item
        Position = 6
        Width = 200
        WideText = 'Filename'
      end
      item
        Position = 7
        Width = 100
        WideText = 'Protection'
      end
      item
        Position = 8
        Width = 100
        WideText = 'Type'
      end
      item
        Position = 9
        Width = 65
        WideText = 'PID'
      end
      item
        Position = 10
        Width = 125
        WideText = 'Load order group'
      end>
  end
  object ActionList: TActionList
    Left = 24
    Top = 16
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
    object aStartTypeAutoDelayed: TAction
      Category = 'StartType'
      Caption = 'Automatic (Delayed)'
      OnExecute = aStartTypeAutoDelayedExecute
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
      Hint = 'Save all registry configuration for this service in a .reg file'
      OnExecute = aExportServiceExecute
    end
    object aUseColors: TAction
      Category = 'Coloring'
      AutoCheck = True
      Caption = 'Use colors'
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
    object aCopyServiceSummary: TAction
      Category = 'Copy'
      Caption = 'Summary'
      OnExecute = aCopyServiceSummaryExecute
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
    object aProtectionNone: TAction
      Category = 'Protection'
      AutoCheck = True
      Caption = 'None'
      GroupIndex = 2
      OnExecute = aProtectionNoneExecute
    end
    object aProtectionWindows: TAction
      Tag = 1
      Category = 'Protection'
      AutoCheck = True
      Caption = 'Windows'
      GroupIndex = 2
      OnExecute = aProtectionNoneExecute
    end
    object aProtectionWindowsLight: TAction
      Tag = 2
      Category = 'Protection'
      AutoCheck = True
      Caption = 'Windows (Light)'
      GroupIndex = 2
      OnExecute = aProtectionNoneExecute
    end
    object aProtectionAntimalwareLight: TAction
      Tag = 3
      Category = 'Protection'
      AutoCheck = True
      Caption = 'Antimalware (Light)'
      GroupIndex = 2
      OnExecute = aProtectionNoneExecute
    end
    object aEditSecurity: TAction
      Category = 'Protection'
      Caption = 'Edit security'
      Hint = 'Edit the service access permissions'
      OnExecute = aEditSecurityExecute
    end
    object aUnlockSecurity: TAction
      Category = 'Protection'
      Caption = 'Unlock security'
      Hint = 
        'Remove protection and take ownership as neccessary to access the' +
        ' service'
      OnExecute = aUnlockSecurityExecute
    end
  end
  object pmServices: TPopupMenu
    OnPopup = pmServicesPopup
    Left = 24
    Top = 72
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
    object miCopySubmenu: TMenuItem
      Caption = 'Copy'
      object Shortsummary1: TMenuItem
        Action = aCopyServiceSummary
      end
      object N5: TMenuItem
        Caption = '-'
      end
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
      Caption = 'Start mode'
      object miStartTypeAutomatic: TMenuItem
        Action = aStartTypeAutomatic
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
      object miStartTypeAutoDelayed: TMenuItem
        Action = aStartTypeAutoDelayed
        GroupIndex = 1
      end
      object miStartTypeManual: TMenuItem
        Action = aStartTypeManual
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
      object miStartTypeDisabled: TMenuItem
        Action = aStartTypeDisabled
        AutoCheck = True
        GroupIndex = 1
        RadioItem = True
      end
    end
    object miSecuritySubmenu: TMenuItem
      Caption = 'Security'
      object miEditSecurity: TMenuItem
        Action = aEditSecurity
        Caption = 'Edit...'
      end
      object miProtectionType: TMenuItem
        Caption = 'Protection'
        object miSetProtectionNone: TMenuItem
          Action = aProtectionNone
          AutoCheck = True
          GroupIndex = 2
          RadioItem = True
        end
        object miSetProtectionWindows: TMenuItem
          Tag = 1
          Action = aProtectionWindows
          AutoCheck = True
          GroupIndex = 2
          RadioItem = True
        end
        object miSetProtectionWindowsLight: TMenuItem
          Tag = 2
          Action = aProtectionWindowsLight
          AutoCheck = True
          GroupIndex = 2
          RadioItem = True
        end
        object miSetProtectionAntimalwareLight: TMenuItem
          Tag = 3
          Action = aProtectionAntimalwareLight
          AutoCheck = True
          GroupIndex = 2
          RadioItem = True
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miUnlockSecurity: TMenuItem
        Action = aUnlockSecurity
        Caption = 'Unlock'
      end
    end
    object miAdvancedSubmenu: TMenuItem
      Caption = 'Advanced'
      object miExportService: TMenuItem
        Action = aExportService
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miDeleteService: TMenuItem
        Action = aDeleteService
      end
    end
  end
  object SaveRegFileDialog: TSaveDialog
    DefaultExt = '.reg'
    Filter = 'Registry Export Files (*.reg)|*.reg|All files (*.*)|*.*'
    Left = 88
    Top = 16
  end
end
