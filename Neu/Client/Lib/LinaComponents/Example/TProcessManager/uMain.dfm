object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'LinaComponents "TProcessManager" Example'
  ClientHeight = 419
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnButtons: TPanel
    Left = 0
    Top = 359
    Width = 478
    Height = 41
    Align = alBottom
    TabOrder = 0
    ExplicitTop = 369
    ExplicitWidth = 434
    DesignSize = (
      478
      41)
    object btTerminate: TButton
      Left = 394
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Terminate'
      TabOrder = 1
      OnClick = btTerminateClick
      ExplicitLeft = 350
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
  object ListView: TListView
    Left = 0
    Top = 0
    Width = 478
    Height = 359
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'ID'
      end
      item
        Caption = 'Priority'
      end
      item
        Caption = 'Memory'
        Width = 100
      end
      item
        Caption = 'Name'
        Width = 100
      end>
    GridLines = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    ExplicitWidth = 434
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 400
    Width = 478
    Height = 19
    Panels = <>
    SimplePanel = True
    ExplicitTop = 410
    ExplicitWidth = 434
  end
  object MainMenu: TMainMenu
    Left = 392
    Top = 8
    object miProcess: TMenuItem
      Caption = 'Process'
      object miProcessLaunch: TMenuItem
        Caption = 'Launch...'
        OnClick = miProcessLaunchClick
      end
      object miProcessTerminate: TMenuItem
        Caption = 'Terminate'
        OnClick = miProcessTerminateClick
      end
    end
    object miInterval: TMenuItem
      Caption = 'Interval'
      object miIntervalNever: TMenuItem
        AutoCheck = True
        Caption = 'No refresh'
        RadioItem = True
        OnClick = miIntervalNeverClick
      end
      object miInterval1000: TMenuItem
        AutoCheck = True
        Caption = '1000 ms'
        RadioItem = True
        OnClick = miInterval1000Click
      end
      object miInterval500: TMenuItem
        AutoCheck = True
        Caption = '500 ms'
        RadioItem = True
        OnClick = miInterval500Click
      end
      object miInterval200: TMenuItem
        AutoCheck = True
        Caption = '200 ms'
        RadioItem = True
        OnClick = miInterval200Click
      end
      object miInterval50: TMenuItem
        AutoCheck = True
        Caption = '50 ms'
        RadioItem = True
        OnClick = miInterval50Click
      end
    end
  end
  object ProcessManager: TProcessManager
    OnUpdate = ProcessManagerUpdate
    Left = 320
    Top = 8
  end
end
