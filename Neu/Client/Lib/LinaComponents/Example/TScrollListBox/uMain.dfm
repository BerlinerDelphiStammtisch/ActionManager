object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LinaComponents "TScrollListBox" Example'
  ClientHeight = 329
  ClientWidth = 434
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbLines: TScrollListBox
    Left = 0
    Top = 0
    Width = 264
    Height = 329
    Align = alClient
    ItemHeight = 13
    Items.Strings = (
      
        '----------------------------------------------------------------' +
        '---'
      'Welcome to the TScrollBar example program of LinaComponents!'
      
        '----------------------------------------------------------------' +
        '---'
      'How does it work?'
      'Simply change the "oversize view" setting and see what happens.')
    ScrollWidth = 100
    TabOrder = 0
    TopIndex = 0
  end
  object pnButtons: TPanel
    Left = 264
    Top = 0
    Width = 170
    Height = 329
    Align = alRight
    TabOrder = 1
    object laOversizeView: TLabel
      Left = 6
      Top = 39
      Width = 155
      Height = 13
      AutoSize = False
      Caption = 'Oversize view:'
    end
    object btAdd: TButton
      Left = 6
      Top = 8
      Width = 155
      Height = 25
      Caption = 'Add...'
      TabOrder = 0
      OnClick = btAddClick
    end
    object coOversizeView: TComboBox
      Left = 6
      Top = 58
      Width = 155
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'None'
      OnChange = coOversizeViewChange
      Items.Strings = (
        'None'
        'Horizontal scroll bar'
        'Word wrapping')
    end
  end
end
