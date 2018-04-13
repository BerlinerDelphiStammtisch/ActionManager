object GDOLoginDialog: TGDOLoginDialog
  Left = 396
  Top = 277
  ActiveControl = Password
  BorderStyle = bsDialog
  Caption = 'Login'
  ClientHeight = 147
  ClientWidth = 290
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 100
    Top = 114
    Width = 85
    Height = 24
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelButton: TButton
    Left = 194
    Top = 114
    Width = 85
    Height = 24
    Cancel = True
    Caption = 'Abbrechen'
    ModalResult = 2
    TabOrder = 1
  end
  object Panel: TPanel
    Left = 8
    Top = 7
    Width = 272
    Height = 98
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Label3: TLabel
      Left = 6
      Top = 6
      Width = 13
      Height = 13
      Caption = 'DB'
    end
    object DatabaseName: TLabel
      Left = 108
      Top = 6
      Width = 3
      Height = 13
    end
    object Bevel: TBevel
      Left = 1
      Top = 24
      Width = 270
      Height = 9
      Shape = bsTopLine
    end
    object Panel1: TPanel
      Left = 2
      Top = 31
      Width = 268
      Height = 65
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 22
        Height = 13
        Caption = 'User'
        FocusControl = UserName
      end
      object Label2: TLabel
        Left = 8
        Top = 36
        Width = 16
        Height = 13
        Caption = 'Pss'
        FocusControl = Password
      end
      object UserName: TEdit
        Left = 107
        Top = 5
        Width = 153
        Height = 21
        MaxLength = 31
        TabOrder = 0
      end
      object Password: TEdit
        Left = 107
        Top = 33
        Width = 153
        Height = 21
        MaxLength = 31
        PasswordChar = '*'
        TabOrder = 1
      end
    end
  end
end
