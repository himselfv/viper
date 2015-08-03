object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = #1057#1083#1091#1078#1073#1099
  ClientHeight = 609
  ClientWidth = 997
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
    Width = 794
    Height = 609
    Align = alClient
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Images = ilImages
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnCompareNodes = vtServicesCompareNodes
    OnFreeNode = vtServicesFreeNode
    OnGetText = vtServicesGetText
    OnGetNodeDataSize = vtServicesGetNodeDataSize
    OnHeaderClick = vtServicesHeaderClick
    OnInitNode = vtServicesInitNode
    Columns = <
      item
        Position = 0
        Width = 100
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
      end>
  end
  object vtFolders: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 200
    Height = 609
    Align = alLeft
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Images = ilImages
    TabOrder = 1
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toFullRepaintOnResize, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowRoot, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect]
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
  end
  object ilImages: TImageList
    Left = 24
    Top = 72
  end
end
