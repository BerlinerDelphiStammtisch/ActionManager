object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LinaComponents "THtmlDocument" Example'
  ClientHeight = 417
  ClientWidth = 657
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object tvStructure: TTreeView
    Left = 0
    Top = 0
    Width = 169
    Height = 417
    Align = alLeft
    Indent = 19
    ReadOnly = True
    TabOrder = 0
    OnClick = tvStructureClick
  end
  object pnProperties: TPanel
    Left = 169
    Top = 0
    Width = 488
    Height = 417
    Align = alClient
    TabOrder = 1
    object lvParams: TListView
      Left = 1
      Top = 1
      Width = 486
      Height = 150
      Align = alTop
      Columns = <
        item
          Caption = 'Name'
          Width = 200
        end
        item
          Caption = 'Value'
          Width = 200
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
    object mmLines: TSynMemo
      Left = 1
      Top = 151
      Width = 486
      Height = 265
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Courier New'
      Font.Style = []
      PopupMenu = pmLines
      TabOrder = 1
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWindowText
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Courier New'
      Gutter.Font.Style = []
      Highlighter = SynHTMLSyn
      ReadOnly = True
      FontSmoothing = fsmNone
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'Hypertext markup files (*.html)|*.html|Extended markup files (*.' +
      'xml)|*.xml'
    Title = 'Select an HTML/XML file anywhere on your system'
    Left = 608
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 552
    Top = 8
    object miFile: TMenuItem
      Caption = 'File'
      OnClick = miFileClick
      object miOpen: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = miOpenClick
      end
      object miClose: TMenuItem
        Caption = 'Close'
        OnClick = miCloseClick
      end
    end
    object miEdit: TMenuItem
      Caption = 'Edit'
      OnClick = miEditClick
      object miSelectAll: TMenuItem
        Caption = 'Select all'
        ShortCut = 16449
        OnClick = miSelectAllClick
      end
      object miSelectNone: TMenuItem
        Caption = 'Select none'
        OnClick = miSelectNoneClick
      end
      object miSepEdit: TMenuItem
        Caption = '-'
      end
      object miCopy: TMenuItem
        Caption = 'Copy'
        ShortCut = 16451
        OnClick = miCopyClick
      end
    end
  end
  object SynHTMLSyn: TSynHTMLSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 489
    Top = 8
  end
  object pmLines: TPopupMenu
    OnPopup = pmLinesPopup
    Left = 432
    Top = 8
    object miLinesSelectAll: TMenuItem
      Caption = 'Select all'
      OnClick = miLinesSelectAllClick
    end
    object miLinesSelectNone: TMenuItem
      Caption = 'Select none'
      OnClick = miLinesSelectNoneClick
    end
    object miLinesSep: TMenuItem
      Caption = '-'
    end
    object miLinesCopy: TMenuItem
      Caption = 'Copy'
      OnClick = miLinesCopyClick
    end
  end
end
