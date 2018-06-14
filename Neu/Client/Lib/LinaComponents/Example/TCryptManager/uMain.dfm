object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LinaComponents "TCryptManager" Example'
  ClientHeight = 281
  ClientWidth = 761
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
  object mmDecrypted: TMemo
    Left = 8
    Top = 8
    Width = 330
    Height = 265
    Lines.Strings = (
      'This is a sample test to show the usability of the'
      '"TCryptManager" component from Lina Components'
      'for Delphi.')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object mmEncrypted: TMemo
    Left = 425
    Top = 8
    Width = 330
    Height = 265
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object btEncrypt: TButton
    Left = 344
    Top = 87
    Width = 75
    Height = 25
    Caption = 'Encrypt '#187
    TabOrder = 2
    OnClick = btEncryptClick
  end
  object rgMode: TRadioGroup
    Left = 344
    Top = 8
    Width = 75
    Height = 73
    Caption = 'Mode'
    Items.Strings = (
      'Xor'
      'Caesar'
      'Vigenere')
    TabOrder = 3
    OnClick = rgModeClick
  end
  object btDecrypt: TButton
    Left = 344
    Top = 118
    Width = 75
    Height = 25
    Caption = #171' Decrypt'
    TabOrder = 4
    OnClick = btDecryptClick
  end
  object leKey: TLabeledEdit
    Left = 344
    Top = 221
    Width = 75
    Height = 21
    CharCase = ecUpperCase
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Key:'
    TabOrder = 5
    OnChange = leKeyChange
    OnContextPopup = leKeyContextPopup
    OnKeyPress = leKeyKeyPress
  end
  object brGenerate: TButton
    Left = 344
    Top = 248
    Width = 75
    Height = 25
    Caption = 'Generate'
    TabOrder = 6
    OnClick = brGenerateClick
  end
  object CryptManager: TCryptManager
    Left = 704
    Top = 8
  end
end
