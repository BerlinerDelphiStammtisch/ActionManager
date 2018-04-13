unit uMain;

//////////////////////////////////////
///  Lina Cryption Example         ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, uBase,
  uSysCtrls, uSysTools;

type
  TfmMain = class(TForm)
    mmDecrypted: TMemo;
    mmEncrypted: TMemo;
    btEncrypt: TButton;
    rgMode: TRadioGroup;
    btDecrypt: TButton;
    leKey: TLabeledEdit;
    CryptManager: TCryptManager;
    brGenerate: TButton;
    procedure rgModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btEncryptClick(Sender: TObject);
    procedure btDecryptClick(Sender: TObject);
    procedure leKeyChange(Sender: TObject);
    procedure leKeyKeyPress(Sender: TObject; var Key: Char);
    procedure leKeyContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure brGenerateClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btEncryptClick(Sender: TObject);
begin
  CryptManager.Decrypted.Assign(mmDecrypted.Lines);
  mmEncrypted.Lines.Assign(CryptManager.Encrypted);
end;

procedure TfmMain.brGenerateClick(Sender: TObject);
begin
  leKey.Text := CryptManager.GenerateKey;
end;

procedure TfmMain.btDecryptClick(Sender: TObject);
begin
  CryptManager.Encrypted.Assign(mmEncrypted.Lines);
  mmDecrypted.Lines.Assign(CryptManager.Decrypted);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  rgMode.ItemIndex := 0;
end;

procedure TfmMain.leKeyChange(Sender: TObject);
begin
  CryptManager.Key := leKey.Text;
end;

procedure TfmMain.leKeyContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  Handled := True;
end;

procedure TfmMain.leKeyKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key in Numbers + Letters) and (CryptManager.ValidKey(leKey.Text + Key) = False) then
  begin
    Key := #0;
  end;
end;

procedure TfmMain.rgModeClick(Sender: TObject);
begin
  leKey.Clear;
  leKey.NumbersOnly := rgMode.ItemIndex = 0;
  leKey.MaxLength := Abs(BoolToInt(rgMode.ItemIndex = 1));
  case rgMode.ItemIndex of
  0: CryptManager.Mode := cmXor;
  1: CryptManager.Mode := cmCaesar;
  2: CryptManager.Mode := cmVigenere;
  end;
end;

end.
