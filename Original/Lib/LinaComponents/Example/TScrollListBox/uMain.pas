unit uMain;

//////////////////////////////////////
///  Lina File Tools Example       ///
///  ****************************  ///
///  (c) 2014 Dennis G�hlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

{ Dies ist ein Beispielprojekt, um den Umgang und die m�gliche Verwendeng der
  TScrollListBox-Komponents zu veranschaulichen.
  Es darf als Grundlage f�r eingene Projekte ohne vorherige Einwilligung
  verwendet werden, solange mindestens eine LinaComponents-Unit im Projekt
  verarbeitet wird.

  WICHTIG: Im Gegensatz zur Entwicklung der LinaComponents selbst wurde bei
           diesem Beispielprojekt keinerlei R�cksicht auf Abw�rtskompatibilit�t
           genommen. Dieses Beispielprogramm wurde in Embarcadero Delphi XE5
           erstellt, entwickelt und kompiliert und k�nnte bei �lteren Delphi-
           Versionen zu Kompilierungsfehlern f�hren. }

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uBase, Vcl.StdCtrls, uAdvCtrls,
  Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    lbLines: TScrollListBox;
    pnButtons: TPanel;
    btAdd: TButton;
    laOversizeView: TLabel;
    coOversizeView: TComboBox;
    procedure btAddClick(Sender: TObject);
    procedure coOversizeViewChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btAddClick(Sender: TObject);
var
  NewLine: String;
begin
  InputQuery('New line','Text:',NewLine);
  NewLine := Trim(NewLine);
  if Length(NewLine) <> 0 then
  begin
    lbLines.Items.Add(NewLine);
  end;
end;

procedure TfmMain.coOversizeViewChange(Sender: TObject);
begin
  case coOversizeView.ItemIndex of
  0: begin
       lbLines.Style := lbStandard;
       lbLines.HorizontalScrollBar := False;
       lbLines.WordWrap := False;
     end;
  1: begin
       lbLines.WordWrap := False;
       lbLines.Style := lbStandard;
       lbLines.HorizontalScrollBar := True;
     end;
  2: begin
       lbLines.HorizontalScrollBar := False;
       lbLines.WordWrap := True;
     end;
  end;
end;

end.
