object ServiceImportForm: TServiceImportForm
  Left = 0
  Top = 0
  Caption = 'Import services'
  ClientHeight = 354
  ClientWidth = 477
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pcPages: TPageControl
    Left = 0
    Top = 0
    Width = 477
    Height = 313
    ActivePage = tsServices
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 0
    ExplicitWidth = 573
    ExplicitHeight = 326
    object tsServices: TTabSheet
      Caption = 'tsServices'
      TabVisible = False
      ExplicitWidth = 565
      ExplicitHeight = 316
      DesignSize = (
        469
        303)
      object Label1: TLabel
        Left = 3
        Top = 3
        Width = 121
        Height = 13
        Caption = 'Select services to import:'
      end
      object rbAddAndUpdateServices: TRadioButton
        Left = 3
        Top = 22
        Width = 460
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Add services and update existing services'
        Checked = True
        TabOrder = 0
        TabStop = True
        ExplicitWidth = 556
      end
      object rbUpdateServicesOnly: TRadioButton
        Left = 3
        Top = 45
        Width = 460
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Update existing services only'
        TabOrder = 1
        ExplicitWidth = 556
      end
      object VirtualStringTree1: TVirtualStringTree
        Left = 3
        Top = 68
        Width = 463
        Height = 233
        Anchors = [akLeft, akTop, akRight, akBottom]
        Header.AutoSizeIndex = 0
        Header.Font.Charset = DEFAULT_CHARSET
        Header.Font.Color = clWindowText
        Header.Font.Height = -11
        Header.Font.Name = 'Tahoma'
        Header.Font.Style = []
        Header.MainColumn = -1
        TabOrder = 2
        ExplicitWidth = 559
        ExplicitHeight = 246
        Columns = <>
      end
    end
    object tsSettings: TTabSheet
      Caption = 'tsSettings'
      ImageIndex = 1
      TabVisible = False
      ExplicitWidth = 565
      ExplicitHeight = 316
      DesignSize = (
        469
        303)
      object Label2: TLabel
        Left = 3
        Top = 3
        Width = 89
        Height = 13
        Caption = 'Settings to import:'
      end
      object CheckBox2: TCheckBox
        Left = 16
        Top = 54
        Width = 450
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Basic service information'
        TabOrder = 0
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 146
        Width = 450
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Additional service parameters'
        TabOrder = 1
      end
      object CheckBox4: TCheckBox
        Left = 16
        Top = 77
        Width = 450
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Start type'
        TabOrder = 2
      end
      object CheckBox5: TCheckBox
        Left = 16
        Top = 100
        Width = 450
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Triggers'
        TabOrder = 3
      end
      object CheckBox6: TCheckBox
        Left = 16
        Top = 123
        Width = 450
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Failure actions'
        TabOrder = 4
      end
      object CheckBox1: TCheckBox
        Left = 16
        Top = 30
        Width = 450
        Height = 17
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Everything'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 313
    Width = 477
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 64
    ExplicitTop = 328
    ExplicitWidth = 185
    DesignSize = (
      477
      41)
    object btnNext: TButton
      Left = 304
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Next >'
      Default = True
      TabOrder = 0
      ExplicitLeft = 400
    end
    object btnCancel: TButton
      Left = 392
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 488
    end
  end
end
