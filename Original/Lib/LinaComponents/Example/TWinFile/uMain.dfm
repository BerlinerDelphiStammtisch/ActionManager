object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LinaComponents "TWinFile" Example'
  ClientHeight = 257
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
  object laOtherFiles: TLabel
    Left = 8
    Top = 144
    Width = 162
    Height = 13
    AutoSize = False
    Caption = 'PAS && DPR files in this directory:'
  end
  object laOtherFolders: TLabel
    Left = 176
    Top = 144
    Width = 162
    Height = 13
    AutoSize = False
    Caption = 'Folders in this directory:'
  end
  object edPath: TEdit
    Left = 8
    Top = 8
    Width = 249
    Height = 21
    TabOrder = 0
    OnChange = edPathChange
  end
  object miBrowse: TButton
    Left = 263
    Top = 8
    Width = 75
    Height = 21
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = miBrowseClick
  end
  object lbOtherFiles: TListBox
    Left = 8
    Top = 163
    Width = 162
    Height = 86
    ItemHeight = 13
    TabOrder = 2
  end
  object gbProperties: TGroupBox
    Left = 8
    Top = 35
    Width = 325
    Height = 94
    Caption = 'File properties'
    TabOrder = 3
    object leFileName: TLabeledEdit
      Left = 63
      Top = 24
      Width = 106
      Height = 21
      EditLabel.Width = 49
      EditLabel.Height = 13
      EditLabel.Caption = 'File name:'
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 0
    end
    object leFileExt: TLabeledEdit
      Left = 268
      Top = 24
      Width = 45
      Height = 21
      EditLabel.Width = 70
      EditLabel.Height = 13
      EditLabel.Caption = 'File extension:'
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 1
    end
    object leFolderName: TLabeledEdit
      Left = 63
      Top = 59
      Width = 106
      Height = 21
      EditLabel.Width = 51
      EditLabel.Height = 13
      EditLabel.Caption = 'File folder:'
      LabelPosition = lpLeft
      ReadOnly = True
      TabOrder = 2
    end
  end
  object lbOtherFolders: TListBox
    Left = 176
    Top = 163
    Width = 162
    Height = 86
    ItemHeight = 13
    TabOrder = 4
  end
  object pnButtons: TPanel
    Left = 345
    Top = 0
    Width = 89
    Height = 257
    Align = alRight
    TabOrder = 5
    object laExecuteMode: TLabel
      Left = 8
      Top = 144
      Width = 73
      Height = 13
      AutoSize = False
      Caption = 'Mode:'
    end
    object imIcon: TImage
      Left = 8
      Top = 8
      Width = 73
      Height = 73
      Center = True
    end
    object btExecute: TButton
      Left = 8
      Top = 224
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 0
      OnClick = btExecuteClick
    end
    object cbExecuteSafe: TCheckBox
      Left = 8
      Top = 201
      Width = 73
      Height = 17
      Caption = 'Safe exec.'
      TabOrder = 1
    end
    object coExecuteMode: TComboBox
      Left = 8
      Top = 163
      Width = 73
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Open'
      Items.Strings = (
        'Open'
        'Edit')
    end
  end
  object OpenDialog: TOpenDialog
    Title = 'Select a file anywhere on your system'
    Left = 384
    Top = 88
  end
end
