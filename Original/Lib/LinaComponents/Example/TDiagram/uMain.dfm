object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LinaComponents "TDiagram" Example'
  ClientHeight = 371
  ClientWidth = 594
  Color = clBtnFace
  Constraints.MinHeight = 400
  Constraints.MinWidth = 600
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Diagram: TDiagram
    Left = 8
    Top = 8
    Width = 578
    Height = 355
    Align = alClient
    Caption.Font.Charset = DEFAULT_CHARSET
    Caption.Font.Color = clSilver
    Caption.Font.Height = -16
    Caption.Font.Name = 'Tahoma'
    Caption.Font.Style = []
    Values = <>
    Padding.Top = 20
    Padding.Bottom = 20
    Padding.Left = 10
    Padding.Right = 10
    Scale.Values.Font.Charset = DEFAULT_CHARSET
    Scale.Values.Font.Color = clNone
    Scale.Values.Font.Height = -11
    Scale.Values.Font.Name = 'Tahoma'
    Scale.Values.Font.Style = []
    TrendLine.Width = 0
    ExplicitLeft = -248
    ExplicitTop = 32
  end
  object MainMenu: TMainMenu
    Left = 552
    Top = 8
    object miFile: TMenuItem
      Caption = 'File'
      OnClick = miFileClick
      object miFileNew: TMenuItem
        Caption = 'New'
        ShortCut = 16462
        OnClick = miFileNewClick
      end
      object miFileOpen: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = miFileOpenClick
      end
      object miFileSave: TMenuItem
        Caption = 'Save'
        ShortCut = 16467
        OnClick = miFileSaveClick
      end
      object miFileSaveAs: TMenuItem
        Caption = 'Save as...'
        OnClick = miFileSaveAsClick
      end
      object miFileClose: TMenuItem
        Caption = 'Close'
        ShortCut = 16472
        OnClick = miFileCloseClick
      end
    end
    object miValues: TMenuItem
      Caption = 'Values'
      OnClick = miValuesClick
      object miValuesValues: TMenuItem
        Caption = 'Values'
        SubMenuImages = ImageList
      end
      object miValuesAdd: TMenuItem
        Caption = 'Add...'
        ShortCut = 16449
        OnClick = miValuesAddClick
      end
      object miValuesImport: TMenuItem
        Caption = 'Import...'
        ShortCut = 16457
        OnClick = miValuesImportClick
      end
      object miValuesExport: TMenuItem
        Caption = 'Export...'
        ShortCut = 16453
        OnClick = miValuesExportClick
      end
    end
    object miView: TMenuItem
      Caption = 'View'
      OnClick = miViewClick
      object miColor: TMenuItem
        Caption = 'Background color...'
        ShortCut = 16450
        OnClick = miColorClick
      end
      object miViewGrid: TMenuItem
        Caption = 'Grid lines'
        OnClick = miViewGridClick
      end
      object miViewBars: TMenuItem
        Caption = 'Bars'
        OnClick = miViewBarsClick
      end
      object miViewArtLines: TMenuItem
        Caption = 'Artificial lines'
        OnClick = miViewArtLinesClick
      end
      object miViewValues: TMenuItem
        Caption = 'Value numbers'
        OnClick = miViewValuesClick
      end
    end
    object miTools: TMenuItem
      Caption = 'Tools'
      OnClick = miToolsClick
      object miToolsCaption: TMenuItem
        Caption = 'Caption...'
        OnClick = miToolsCaptionClick
      end
      object miToolsTrendLine: TMenuItem
        Caption = 'Trend line'
        ShortCut = 16468
        OnClick = miToolsTrendLineClick
      end
    end
    object miInfo: TMenuItem
      Caption = 'Info'
      object miInfoAbout: TMenuItem
        Caption = 'About...'
        ShortCut = 112
        OnClick = miInfoAboutClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 496
    Top = 8
  end
  object SaveDialog: TSaveDialog
    Left = 432
    Top = 8
  end
  object ColorDialog: TColorDialog
    Left = 368
    Top = 8
  end
  object ImageList: TImageList
    Masked = False
    Left = 312
    Top = 8
  end
end
