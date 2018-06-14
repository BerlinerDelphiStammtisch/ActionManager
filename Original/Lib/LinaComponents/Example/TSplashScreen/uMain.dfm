object fmMain: TfmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'LinaComponents "TSplashScreen" Example'
  ClientHeight = 219
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object gbForm: TGroupBox
    Left = 8
    Top = 8
    Width = 233
    Height = 161
    Caption = 'Form settings'
    TabOrder = 0
    object laWidth: TLabel
      Left = 16
      Top = 21
      Width = 105
      Height = 21
      AutoSize = False
      Caption = 'Width:'
      Layout = tlCenter
    end
    object laHeight: TLabel
      Left = 16
      Top = 48
      Width = 105
      Height = 21
      AutoSize = False
      Caption = 'Height:'
      Layout = tlCenter
    end
    object laCaption: TLabel
      Left = 16
      Top = 75
      Width = 105
      Height = 21
      AutoSize = False
      Caption = 'Caption:'
      Layout = tlCenter
    end
    object laColor: TLabel
      Left = 16
      Top = 102
      Width = 105
      Height = 21
      AutoSize = False
      Caption = 'Color:'
      Layout = tlCenter
    end
    object edWidth: TEdit
      Left = 127
      Top = 21
      Width = 90
      Height = 21
      TabOrder = 0
      OnChange = edWidthChange
    end
    object edHeight: TEdit
      Left = 127
      Top = 48
      Width = 90
      Height = 21
      TabOrder = 1
      OnChange = edHeightChange
    end
    object edCaption: TEdit
      Left = 127
      Top = 75
      Width = 90
      Height = 21
      TabOrder = 2
      OnChange = edCaptionChange
    end
    object edAlpha: TEdit
      Left = 127
      Top = 129
      Width = 90
      Height = 21
      TabOrder = 3
      OnChange = edAlphaChange
    end
    object coColor: TColorBox
      Left = 127
      Top = 102
      Width = 90
      Height = 22
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
      TabOrder = 4
    end
    object cbAlpha: TCheckBox
      Left = 16
      Top = 129
      Width = 105
      Height = 21
      Caption = 'Alpha Blend'
      TabOrder = 5
      OnClick = cbAlphaClick
    end
  end
  object gbImage: TGroupBox
    Left = 247
    Top = 8
    Width = 233
    Height = 161
    Caption = 'Image settings'
    TabOrder = 1
    object imImage: TImage
      Left = 16
      Top = 21
      Width = 201
      Height = 103
      Center = True
      Stretch = True
    end
    object edImage: TEdit
      Left = 16
      Top = 129
      Width = 169
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = edImageChange
      OnExit = edImageExit
    end
    object btImage: TButton
      Left = 191
      Top = 129
      Width = 26
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = btImageClick
    end
  end
  object pnButtons: TPanel
    Left = 0
    Top = 178
    Width = 489
    Height = 41
    Align = alBottom
    TabOrder = 2
    object laAnimation: TLabel
      Left = 8
      Top = 8
      Width = 65
      Height = 17
      AutoSize = False
      Caption = 'Animation:'
    end
    object btShow: TButton
      Left = 405
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Show...'
      TabOrder = 0
      OnClick = btShowClick
    end
    object coAnimation: TComboBox
      Left = 79
      Top = 8
      Width = 98
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'None'
      Items.Strings = (
        'None'
        'Shallow')
    end
  end
  object SplashScreen: TSplashScreen
    OnTimer = SplashScreenTimer
    SplashForm.BorderStyle = bsDialog
    SplashForm.BorderIcons = [biSystemMenu]
    SplashProgressBar.Position = 60
    SplashTimer.Enabled = True
    SplashTimer.Interval = 500
    Left = 368
    Top = 186
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Alle (*.bmp; *.gif; *.jpg; *.jpeg)|*.bmp;*.gif;*.jpg;*.jpeg'
    Options = [ofReadOnly, ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofNoTestFileCreate, ofNoNetworkButton, ofNoDereferenceLinks, ofDontAddToRecent]
    OptionsEx = [ofExNoPlacesBar]
    Left = 336
    Top = 186
  end
end
