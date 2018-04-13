object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LinaComponents "TContextMenu" Example'
  ClientHeight = 259
  ClientWidth = 449
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
    Top = 218
    Width = 449
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btClose: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = btCloseClick
    end
    object btApply: TButton
      Left = 364
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 2
      OnClick = btApplyClick
    end
    object btRefresh: TButton
      Left = 283
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = btRefreshClick
    end
  end
  object gbMenuItems: TGroupBox
    Left = 8
    Top = 8
    Width = 433
    Height = 204
    Caption = 'Context menu items for *.pas files'
    TabOrder = 1
    object lwMenuItems: TListView
      Left = 16
      Top = 24
      Width = 401
      Height = 169
      Columns = <
        item
          Caption = 'Name'
        end
        item
          Caption = 'Caption'
        end
        item
          Caption = 'Command'
        end
        item
          Caption = 'Icon'
        end>
      RowSelect = True
      PopupMenu = pmMenuItems
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lwMenuItemsChange
    end
  end
  object ContextMenu: TContextMenu
    Items = <>
    Ext = 'pas'
    Left = 48
    Top = 40
  end
  object pmMenuItems: TPopupMenu
    OnPopup = pmMenuItemsPopup
    Left = 120
    Top = 40
    object miAdd: TMenuItem
      Caption = 'Add'
      OnClick = miAddClick
    end
    object miDelete: TMenuItem
      Caption = 'Delete'
      OnClick = miDeleteClick
    end
    object miSeparator: TMenuItem
      Caption = '-'
    end
    object miEditCaption: TMenuItem
      Caption = 'Caption...'
      OnClick = miEditCaptionClick
    end
    object miEditCommand: TMenuItem
      Caption = 'Command...'
      OnClick = miEditCommandClick
    end
    object miEditIcon: TMenuItem
      Caption = 'Icon...'
      OnClick = miEditIconClick
    end
  end
  object odIcon: TOpenDialog
    Filter = 'Icons (*.ico)|*.ico|Executables (*.exe)|*.exe'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofNoTestFileCreate, ofNoNetworkButton, ofNoDereferenceLinks, ofEnableSizing, ofDontAddToRecent]
    Left = 184
    Top = 40
  end
end
