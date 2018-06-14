object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'LinaComponents "TScriptManager" Example'
  ClientHeight = 369
  ClientWidth = 707
  Color = clBtnFace
  Constraints.MinHeight = 360
  Constraints.MinWidth = 670
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
  object SynEdit: TSynEdit
    Left = 0
    Top = 0
    Width = 707
    Height = 304
    Align = alClient
    ActiveLineColor = clInfoBk
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = PopupMenu
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Highlighter = SynPasSyn
    Lines.Strings = (
      'program Example;'
      ''
      'var'
      '  Frm: TForm;'
      ''
      'begin'
      '  //Show form'
      '  Frm := TForm.Create(Application);'
      '  Frm.Position := poScreenCenter;'
      '  Frm.BorderStyle := bsDialog;'
      '  Frm.Caption := '#39'Demo'#39';'
      '  Frm.Show;'
      'end.')
    WantTabs = True
    FontSmoothing = fsmNone
  end
  object ListBox: TListBox
    Left = 0
    Top = 304
    Width = 707
    Height = 65
    Align = alBottom
    BevelKind = bkSoft
    BevelWidth = 4
    ItemHeight = 13
    TabOrder = 1
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 8
    object miFile: TMenuItem
      Caption = 'File'
      object miFileOpen: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = miFileOpenClick
      end
      object miFileSave: TMenuItem
        Caption = 'Save...'
        ShortCut = 16467
        OnClick = miFileSaveClick
      end
      object miFileSep: TMenuItem
        Caption = '-'
      end
      object miFileClose: TMenuItem
        Caption = 'Close'
        OnClick = miFileCloseClick
      end
    end
    object miEdit: TMenuItem
      Caption = 'Edit'
      OnClick = miEditClick
      object miEditUndo: TMenuItem
        Caption = 'Undo'
        ShortCut = 16474
        OnClick = miEditUndoClick
      end
      object miEditRedo: TMenuItem
        Caption = 'Redo'
        ShortCut = 16473
        OnClick = miEditRedoClick
      end
      object miEditSep: TMenuItem
        Caption = '-'
      end
      object miEditSelectAll: TMenuItem
        Caption = 'Select all'
        OnClick = miEditSelectAllClick
      end
    end
    object miRun: TMenuItem
      Caption = 'Run'
      object miRunCompileExecute: TMenuItem
        Caption = 'Compile && execute'
        ShortCut = 16466
        OnClick = miRunCompileExecuteClick
      end
      object miRunLibrary: TMenuItem
        AutoCheck = True
        Caption = 'Load libraries'
        Checked = True
        OnClick = miRunLibraryClick
      end
    end
    object miInfo: TMenuItem
      Caption = 'Info'
      object miInfoAbout: TMenuItem
        Caption = 'About...'
        OnClick = miInfoAboutClick
      end
    end
  end
  object SynPasSyn: TSynPasSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    AsmAttri.Foreground = clMaroon
    CommentAttri.Foreground = clGreen
    DirectiveAttri.Foreground = clPurple
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clTeal
    FloatAttri.Foreground = clTeal
    HexAttri.Foreground = clTeal
    StringAttri.Foreground = clTeal
    CharAttri.Foreground = clTeal
    SymbolAttri.Foreground = clGray
    Left = 40
    Top = 8
  end
  object ScriptManager: TScriptManager
    OnReturn = ScriptManagerReturn
    ReturnMode = srAll
    ReturnStyle = srName
    VarSelf = Owner
    Libraries = [slClasses, slControls, slStdCtrls, slForms, slDateUtils, slComObj, slDB, slCustom]
    UsePreProcessor = True
    Left = 40
    Top = 40
  end
  object OpenTextFileDialog: TOpenTextFileDialog
    Filter = 'Source Files (*.pas)|*.pas|Include Files (*.inc)|*.inc'
    Left = 72
    Top = 8
  end
  object SaveTextFileDialog: TSaveTextFileDialog
    Filter = 'Source Files (*.pas)|*.pas|Include Files (*.inc)|*.inc'
    Left = 72
    Top = 40
  end
  object PopupMenu: TPopupMenu
    Left = 8
    Top = 40
    object miPopupSelectAll: TMenuItem
      Caption = 'Select all'
      ShortCut = 16449
      OnClick = miPopupSelectAllClick
    end
    object miPopupSep: TMenuItem
      Caption = '-'
    end
    object miPopupCopy: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = miPopupCopyClick
    end
    object miPopupCut: TMenuItem
      Caption = 'Cut'
      ShortCut = 16472
      OnClick = miPopupCutClick
    end
    object miPopupPaste: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = miPopupPasteClick
    end
  end
end
