object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LinaComponents "TParamDefiner" Example'
  ClientHeight = 429
  ClientWidth = 434
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
  object pnButtons: TPanel
    Left = 0
    Top = 369
    Width = 434
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btRestart: TButton
      Left = 350
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Restart'
      TabOrder = 1
      OnClick = btRestartClick
    end
    object btRefresh: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = btRefreshClick
    end
  end
  object gbFormat: TGroupBox
    Left = 8
    Top = 8
    Width = 417
    Height = 65
    Caption = 'Format settings'
    TabOrder = 1
    object lePrefix: TLabeledEdit
      Left = 16
      Top = 33
      Width = 113
      Height = 21
      EditLabel.Width = 32
      EditLabel.Height = 13
      EditLabel.Caption = 'Prefix:'
      TabOrder = 0
      OnChange = lePrefixChange
    end
    object leSuffix: TLabeledEdit
      Left = 288
      Top = 33
      Width = 113
      Height = 21
      EditLabel.Width = 32
      EditLabel.Height = 13
      EditLabel.Caption = 'Prefix:'
      TabOrder = 1
      OnChange = leSuffixChange
    end
    object leSeparator: TLabeledEdit
      Left = 152
      Top = 33
      Width = 113
      Height = 21
      EditLabel.Width = 52
      EditLabel.Height = 13
      EditLabel.Caption = 'Separator:'
      TabOrder = 2
      OnChange = leSeparatorChange
    end
  end
  object ListView: TListView
    Left = 8
    Top = 79
    Width = 418
    Height = 284
    Columns = <
      item
        Caption = 'Identifier'
        Width = 150
      end
      item
        Caption = 'Value'
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 410
    Width = 434
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object ParamDefiner: TParamDefiner
    References = <>
    Left = 32
    Top = 88
  end
end
