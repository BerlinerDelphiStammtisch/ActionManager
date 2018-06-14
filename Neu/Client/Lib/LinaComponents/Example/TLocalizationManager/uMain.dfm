object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'LinaComponents "TScriptManager" Example'
  ClientHeight = 211
  ClientWidth = 353
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object laEncoding: TLabel
    Left = 8
    Top = 132
    Width = 65
    Height = 13
    AutoSize = False
    Caption = 'Encoding:'
  end
  object pnButtons: TPanel
    Left = 0
    Top = 170
    Width = 353
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btClose: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = btCloseClick
    end
    object btHello: TButton
      Left = 270
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Hello!'
      TabOrder = 1
      OnClick = btHelloClick
    end
  end
  object gbLaguage: TGroupBox
    Left = 8
    Top = 8
    Width = 337
    Height = 115
    Caption = 'Language'
    TabOrder = 1
    object lbLanguage: TListBox
      Left = 16
      Top = 24
      Width = 305
      Height = 81
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbLanguageClick
    end
  end
  object coEncoding: TComboBox
    Left = 79
    Top = 129
    Width = 90
    Height = 21
    Style = csDropDownList
    ItemIndex = 2
    TabOrder = 2
    Text = 'Unicode'
    OnChange = coEncodingChange
    Items.Strings = (
      'ANSI'
      'UTF8'
      'Unicode')
  end
  object LocalizationManager: TLocalizationManager
    Localizations = <>
    References = <
      item
        Indent = 'HelloWorld'
      end
      item
        Component = gbLaguage
        Indent = 'LanguageSelect'
        Field = 'Caption'
      end
      item
        Component = btHello
        Section = 'Buttons'
        Indent = 'Hello'
        Field = 'Caption'
      end
      item
        Component = btClose
        Section = 'Buttons'
        Indent = 'Close'
        Field = 'Caption'
      end
      item
        Component = laEncoding
        Section = 'Other'
        Indent = 'Coding'
        Field = 'Caption'
      end>
    Left = 280
    Top = 24
  end
end
