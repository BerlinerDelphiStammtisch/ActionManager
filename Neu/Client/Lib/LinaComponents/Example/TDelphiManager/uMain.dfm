object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LinaComponents "TDelphiManager" Example'
  ClientHeight = 219
  ClientWidth = 474
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
  object rgVersions: TRadioGroup
    Left = 8
    Top = 8
    Width = 371
    Height = 185
    Caption = 'Installed Delphi versions'
    TabOrder = 0
  end
  object pnButtons: TPanel
    Left = 385
    Top = 0
    Width = 89
    Height = 200
    Align = alRight
    TabOrder = 1
    object btLaunch: TButton
      Left = 8
      Top = 168
      Width = 75
      Height = 25
      Caption = 'Launch'
      Default = True
      TabOrder = 2
      OnClick = btLaunchClick
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
    object btInfo: TButton
      Left = 8
      Top = 137
      Width = 75
      Height = 25
      Caption = 'Info'
      TabOrder = 1
      OnClick = btInfoClick
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 200
    Width = 474
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object DelphiManager: TDelphiManager
    Left = 40
    Top = 32
  end
end
