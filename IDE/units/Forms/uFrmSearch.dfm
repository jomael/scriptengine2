object SearchDialog: TSearchDialog
  Left = 401
  Top = 313
  ActiveControl = cbSearchText
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Search text'
  ClientHeight = 282
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lbTitle: TLabel
    Left = 0
    Top = 0
    Width = 402
    Height = 17
    Align = alTop
    AutoSize = False
    Caption = '    Search for'
    Color = clWhite
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Layout = tlCenter
  end
  object lbTitleBlue: TLabel
    Left = 0
    Top = 17
    Width = 402
    Height = 1
    Align = alTop
    AutoSize = False
    Color = clNavy
    ParentColor = False
  end
  object MainPanel: TPanel
    Left = 9
    Top = 26
    Width = 384
    Height = 223
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = 15987699
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object lbSearchTitle: TLabel
      Left = 8
      Top = 12
      Width = 54
      Height = 13
      Caption = '&Search for:'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object gbOptions: TGroupBox
      Left = 8
      Top = 40
      Width = 180
      Height = 81
      Caption = 'Options'
      Color = 15987699
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 1
      object cbCaseSensitive: TCheckBox
        Left = 9
        Top = 20
        Width = 164
        Height = 17
        Caption = 'C&ase sensitivity'
        TabOrder = 0
      end
      object cbWholeWords: TCheckBox
        Left = 9
        Top = 38
        Width = 164
        Height = 17
        Caption = '&Whole words only'
        TabOrder = 1
      end
      object cbRegularExpression: TCheckBox
        Left = 9
        Top = 55
        Width = 164
        Height = 17
        Caption = '&Regular expression'
        TabOrder = 2
      end
    end
    object cbSearchText: TComboBox
      Left = 92
      Top = 8
      Width = 283
      Height = 21
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemHeight = 13
      ParentFont = False
      TabOrder = 0
      OnKeyDown = cbSearchTextKeyDown
      OnKeyPress = cbSearchTextKeyPress
    end
    object rgSearchDirection: TRadioGroup
      Left = 197
      Top = 40
      Width = 180
      Height = 81
      Caption = 'Direction'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        '&Forward'
        '&Backward')
      ParentFont = False
      TabOrder = 2
    end
    object rgAreaSelection: TRadioGroup
      Left = 8
      Top = 122
      Width = 180
      Height = 55
      Caption = 'Area'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Whole text'
        'Selected text only')
      ParentFont = False
      TabOrder = 3
    end
    object rgSearchContent: TRadioGroup
      Left = 197
      Top = 122
      Width = 180
      Height = 55
      Caption = 'Search start'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 1
      Items.Strings = (
        'Cursor position'
        'Start of text')
      ParentFont = False
      TabOrder = 4
    end
  end
  object btOk: TButton
    Left = 318
    Top = 254
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object btCancel: TButton
    Left = 240
    Top = 254
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object cbListSearch: TCheckBox
    Left = 9
    Top = 256
    Width = 224
    Height = 17
    Caption = 'List complete search result'
    TabOrder = 1
  end
end
