object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'LinaComponents "TListBoxManager" Example'
  ClientHeight = 371
  ClientWidth = 663
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnButtons: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 371
    Align = alLeft
    TabOrder = 0
    DesignSize = (
      201
      371)
    object laFilter: TLabel
      Left = 16
      Top = 8
      Width = 169
      Height = 13
      AutoSize = False
      Caption = 'Search filter:'
    end
    object btAdd: TButton
      Left = 16
      Top = 327
      Width = 169
      Height = 32
      Anchors = [akLeft, akBottom]
      Caption = 'Add item'
      TabOrder = 2
      OnClick = btAddClick
    end
    object edFilter: TEdit
      Left = 16
      Top = 27
      Width = 169
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnChange = edFilterChange
    end
    object cbCase: TCheckBox
      Left = 16
      Top = 57
      Width = 169
      Height = 17
      Caption = 'Ignore char case'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = cbCaseClick
    end
  end
  object lbItems: TListBox
    Left = 201
    Top = 0
    Width = 462
    Height = 371
    Align = alClient
    ItemHeight = 13
    Items.Strings = (
      'Lina Components: uFrmCtrls.TListBoxManager Example'
      
        '----------------------------------------------------------------' +
        '-------------------'
      'Copyright 2014 Dennis G'#246'hlert a.o.'
      
        '----------------------------------------------------------------' +
        '-------------------'
      
        '----------------------------------------------------------------' +
        '-------------------'
      'How to use?'
      
        '-> Simply edit the text on the left in the "Search filter" edit ' +
        'field')
    TabOrder = 1
  end
  object ListBoxManager: TListBoxManager
    Target = lbItems
    SourceEdit = edFilter
    Mode = lmmEdit
    FilterOptions = [sfoForceTrim, sfoDefaultVisible]
    Left = 232
    Top = 8
  end
end
