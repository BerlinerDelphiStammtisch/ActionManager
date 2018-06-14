unit uMain;

//////////////////////////////////////
///  Lina Local. Manager Example   ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uBase, uLocalMgr, Vcl.StdCtrls,
  Vcl.ExtCtrls, uFileTools, uSysTools;

type
  TForm1 = class(TForm)
    LocalizationManager: TLocalizationManager;
    pnButtons: TPanel;
    btClose: TButton;
    btHello: TButton;
    gbLaguage: TGroupBox;
    lbLanguage: TListBox;
    laEncoding: TLabel;
    coEncoding: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure lbLanguageClick(Sender: TObject);
    procedure btHelloClick(Sender: TObject);
    procedure btCloseClick(Sender: TObject);
    procedure coEncodingChange(Sender: TObject);
  private
    { Private-Deklarationen }
    HelloWorld: String;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.btHelloClick(Sender: TObject);
//var g: TLocalizationGenerator;
begin
//g := TLocalizationGenerator.Create(LocalizationManager.Data);
//  ShowMessage(HelloWorld);
//showmessage(g.Localization.Lines.Text);
//g.Free;
showmessage((localizationmanager.Localizations.Items[LocalizationManager.Current] as TLocalization).Name);
end;

procedure TForm1.coEncodingChange(Sender: TObject);
var
  Index: Integer;
begin
  for Index := 0 to lbLanguage.Items.Count - 1 do
  begin
    case coEncoding.ItemIndex of
    0: (LocalizationManager.Localizations.Items[Index] as TLocalization).Encoding := ceANSI;
    1: (LocalizationManager.Localizations.Items[Index] as TLocalization).Encoding := ceUTF8;
    2: (LocalizationManager.Localizations.Items[Index] as TLocalization).Encoding := ceUnicode;
    end;
    (LocalizationManager.Localizations.Items[Index] as TLocalization).Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + ChangeFileExt(lbLanguage.Items.Strings[Index],'.loc'));
  end;
  LocalizationManager.Current := lbLanguage.ItemIndex;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Index: Integer;
 // LanguageFiles: TStrings;
begin
  LocalizationManager.References.Add;
  HelloWorld := 'Hello,' + #10 + 'World!';
  (LocalizationManager.References.Items[0] as TLocalizationReference).Reference := @HelloWorld;
  {LanguageFiles := TStringList.Create;
  ListFiles(ExtractFileDir(Application.ExeName),LanguageFiles,['*.loc']);
  lbLanguage.Items.Assign(LanguageFiles);
  for Index := 0 to LanguageFiles.Count - 1 do
  begin
      (LocalizationManager.Localizations.Add as TLocalization).Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + ChangeFileExt(LanguageFiles.Strings[Index],'.loc'));
   // (LocalizationManager.Localizations.Add as TLocalization).Encoding := ceUnicode;
  //  (LocalizationManager.Localizations.Items[LocalizationManager.Localizations.Count - 1] as TLocalization).Lines.LoadFromFile(ExtractFilePath(Application.ExeName) + ChangeFileExt(LanguageFiles.Strings[Index],'.loc'));
   // // (LocalizationManager.Localizations.Items[Index] as TLocalization).Format.Indent := '+';
   //   if (LocalizationManager.Localizations.Items[Index] as TLocalization).Lines.DefaultEncoding = TEncoding.ANSI then showmessage('');
//     TLocalization(LocalizationManager.Localizations.Items[Index]).Encoding := ceANSI;
  end;
  //LocalizationManager.Applier.ApplyMode := laMainForm;  }
  LocalizationManager.Localizations.LoadFromDirectory(ExtractFileDir(Application.ExeName),['*.loc']);
  for Index := 0 to LocalizationManager.Localizations.Count - 1 do
  begin
    lbLanguage.Items.Add((LocalizationManager.Localizations.Items[Index] as TLocalization).Name);
  end;
end;

procedure TForm1.lbLanguageClick(Sender: TObject);
var Indents: TStrings;
begin
  // Showmessage((LocalizationManager.Localizations.Items[0] as TLocalization).Lines.Text);
  LocalizationManager.Current := lbLanguage.ItemIndex;
//  // Showmessage(booltostr(LocalizationManager.Data.IndentExists('','HelloWorld'),true));
 // indents := TStringList.Create;
 // LocalizationManager.Data.ReadValues('buttons',indents);
 // showmessage(indents.Text);
// indents.Free;

end;

end.
