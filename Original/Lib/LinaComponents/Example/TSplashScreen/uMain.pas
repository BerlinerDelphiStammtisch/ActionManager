unit uMain;

//////////////////////////////////////
///  Lina Splash Screen Example    ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, uFrmCtrls, uBase, uSysTools,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg, Vcl.ExtDlgs;

type
  TfmMain = class(TForm)
    gbForm: TGroupBox;
    gbImage: TGroupBox;
    pnButtons: TPanel;
    btShow: TButton;
    laWidth: TLabel;
    edWidth: TEdit;
    edHeight: TEdit;
    laHeight: TLabel;
    edCaption: TEdit;
    laCaption: TLabel;
    laColor: TLabel;
    edAlpha: TEdit;
    coColor: TColorBox;
    cbAlpha: TCheckBox;
    imImage: TImage;
    edImage: TEdit;
    btImage: TButton;
    laAnimation: TLabel;
    coAnimation: TComboBox;
    SplashScreen: TSplashScreen;
    OpenPictureDialog: TOpenPictureDialog;
    procedure btShowClick(Sender: TObject);
    procedure SplashScreenTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edWidthChange(Sender: TObject);
    procedure edHeightChange(Sender: TObject);
    procedure edAlphaChange(Sender: TObject);
    procedure edCaptionChange(Sender: TObject);
    procedure cbAlphaClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btImageClick(Sender: TObject);
    procedure edImageExit(Sender: TObject);
    procedure edImageChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btImageClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute = True then
  begin
    if FileExists(OpenPictureDialog.FileName) = True then
    begin
      edImage.Text := OpenPictureDialog.FileName;
    end;
  end;
end;

procedure TfmMain.btShowClick(Sender: TObject);
begin
  SplashScreen.SplashForm.Width := StrToInt(edWidth.Text);
  SplashScreen.SplashForm.Height := StrToInt(edHeight.Text);
  SplashScreen.SplashForm.Caption := edCaption.Text;
  SplashScreen.SplashForm.Color := coColor.Selected;
  SplashScreen.SplashForm.AlphaBlend := cbAlpha.Checked;
  SplashScreen.SplashForm.AlphaBlendValue := StrToInt(edAlpha.Text);
  SplashScreen.SplashImage.Picture.Assign(imImage.Picture);
  if coAnimation.ItemIndex = 0 then
  begin
    SplashScreen.Animation := ssaNone;
  end else
  begin
    SplashScreen.Animation := ssaShallow;
  end;

  if SplashScreen.Visible = True then
  begin
    SplashScreen.Hide;
    btShow.Caption := 'Show...';
  end else
  begin
    SplashScreen.Show;
    btShow.Caption := 'Hide';
  end;
end;

procedure TfmMain.cbAlphaClick(Sender: TObject);
begin
  edAlpha.Enabled := cbAlpha.Checked;
end;

procedure TfmMain.edAlphaChange(Sender: TObject);
begin
  if (StrIsInt(edAlpha.Text) = False) or (not (StrToInt(edAlpha.Text) in [0..255])) then
  begin
    MessageDlg('Unvalid value',mtError,[mbCancel],0);
    edAlpha.Text := IntToStr(SplashScreen.SplashForm.AlphaBlendValue);
  end;
end;

procedure TfmMain.edCaptionChange(Sender: TObject);
begin
  if Length(edCaption.Text) > 255 then
  begin
    MessageDlg('Unvalid value',mtError,[mbCancel],0);
    edCaption.Text := SplashScreen.SplashForm.Caption;
  end;
end;

procedure TfmMain.edHeightChange(Sender: TObject);
begin
  if StrIsInt(edHeight.Text) = False then
  begin
    MessageDlg('Unvalid value',mtError,[mbCancel],0);
    edHeight.Text := IntToStr(SplashScreen.SplashForm.Height);
  end;
end;

procedure TfmMain.edImageChange(Sender: TObject);
begin
  if edImage.Text = 'Background.jpg' then
  begin
    edImage.Font.Color := clGray;
  end else
  begin
    edImage.Font.Color := clWindowText;
  end;
end;

procedure TfmMain.edImageExit(Sender: TObject);
begin
  try
    imImage.Picture.LoadFromFile(edImage.Text);
  except
    edImage.Text := 'Background.jpg';
    imImage.Picture.LoadFromFile(edImage.Text);
  end;
end;

procedure TfmMain.edWidthChange(Sender: TObject);
begin
  if StrIsInt(edWidth.Text) = False then
  begin
    MessageDlg('Unvalid value',mtError,[mbCancel],0);
    edWidth.Text := IntToStr(SplashScreen.SplashForm.Width);
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  SplashScreen.SplashImage.Picture.LoadFromFile('Background.jpg');
  edImage.Text := 'Background.jpg';
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  edWidth.Text := IntToStr(SplashScreen.SplashForm.Width);
  edHeight.Text := IntToStr(SplashScreen.SplashForm.Height);
  edCaption.Text := SplashScreen.SplashForm.Caption;
  coColor.Selected := SplashScreen.SplashForm.Color;
  cbAlpha.Checked := SplashScreen.SplashForm.AlphaBlend;
  edAlpha.Enabled := cbAlpha.Checked;
  edAlpha.Text := IntToStr(SplashScreen.SplashForm.AlphaBlendValue);
  imImage.Picture.Assign(SplashScreen.SplashImage.Picture);
  if SplashScreen.Animation = ssaNone then
  begin
    coAnimation.ItemIndex := 0;
  end else
  begin
    coAnimation.ItemIndex := 1;
  end;
end;

procedure TfmMain.SplashScreenTimer(Sender: TObject);
begin
  if SplashScreen.SplashProgressBar.Position < SplashScreen.SplashProgressBar.Max then
  begin
    SplashScreen.SplashProgressBar.Position := SplashScreen.SplashProgressBar.Position + 10;
  end else
  begin
    SplashScreen.SplashProgressBar.Position := 0;
    SplashScreen.Hide;
  end;
  //SplashScreen.ApplyChanges;
end;

end.
