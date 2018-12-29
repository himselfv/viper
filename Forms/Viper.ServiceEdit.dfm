object ServiceEditForm: TServiceEditForm
  Left = 0
  Top = 0
  Caption = 'Service Properties'
  ClientHeight = 518
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 5
  Padding.Top = 5
  Padding.Right = 5
  Padding.Bottom = 5
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlFooter: TPanel
    Left = 5
    Top = 478
    Width = 418
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 517
    DesignSize = (
      418
      35)
    object btnOk: TButton
      Left = 177
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      Default = True
      TabOrder = 0
      OnClick = btnOkClick
      ExplicitLeft = 276
    end
    object btnCancel: TButton
      Left = 258
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 357
    end
    object btnApply: TButton
      Left = 339
      Top = 6
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Apply'
      TabOrder = 2
      OnClick = btnApplyClick
      ExplicitLeft = 438
    end
  end
  object pcPages: TPageControl
    Left = 5
    Top = 5
    Width = 418
    Height = 473
    ActivePage = tsGeneral
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 527
    ExplicitHeight = 477
    object tsGeneral: TTabSheet
      AlignWithMargins = True
      Caption = 'General'
      ExplicitLeft = 4
      ExplicitTop = 24
      ExplicitWidth = 519
      ExplicitHeight = 449
      DesignSize = (
        404
        439)
      object lblServiceMain: TLabel
        Left = 3
        Top = 323
        Width = 62
        Height = 26
        Caption = 'Main service function:'
        Layout = tlCenter
        WordWrap = True
      end
      object lblServiceDLL: TLabel
        Left = 3
        Top = 281
        Width = 59
        Height = 13
        Caption = 'Service DLL:'
        Layout = tlCenter
        WordWrap = True
      end
      object lblExecutableFile: TLabel
        Left = 3
        Top = 260
        Width = 74
        Height = 13
        Caption = 'Executable file:'
        Layout = tlCenter
        WordWrap = True
      end
      object lblDescription: TLabel
        Left = 3
        Top = 60
        Width = 57
        Height = 13
        Caption = 'Description:'
        WordWrap = True
      end
      object lblDisplayName: TLabel
        Left = 3
        Top = 33
        Width = 67
        Height = 13
        Caption = 'Display name:'
        Layout = tlCenter
        WordWrap = True
      end
      object lblServiceName: TLabel
        Left = 3
        Top = 6
        Width = 68
        Height = 13
        Caption = 'Service name:'
        Layout = tlCenter
        WordWrap = True
      end
      object lblServiceType: TLabel
        Left = 3
        Top = 142
        Width = 68
        Height = 13
        Caption = 'Service name:'
        Layout = tlCenter
        WordWrap = True
      end
      object edtServiceDLL: TEdit
        Left = 103
        Top = 281
        Width = 121
        Height = 21
        TabOrder = 0
      end
      object cbServiceDllUnloadOnStop: TCheckBox
        Left = 103
        Top = 302
        Width = 97
        Height = 17
        Caption = 'Unload service DLL on stop'
        TabOrder = 1
      end
      object edtServiceMain: TEdit
        Left = 103
        Top = 323
        Width = 100
        Height = 21
        TabOrder = 2
      end
      object edtExecutableFile: TEdit
        Left = 103
        Top = 260
        Width = 121
        Height = 21
        TabOrder = 3
      end
      object edtDescription: TMemo
        Left = 99
        Top = 57
        Width = 302
        Height = 60
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
      end
      object edtDisplayName: TEdit
        Left = 99
        Top = 30
        Width = 302
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
      end
      object edtServiceName: TEdit
        Left = 99
        Top = 3
        Width = 302
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 6
      end
      object cbServiceType: TComboBox
        Left = 99
        Top = 139
        Width = 302
        Height = 21
        Style = csDropDownList
        TabOrder = 7
      end
    end
  end
end
