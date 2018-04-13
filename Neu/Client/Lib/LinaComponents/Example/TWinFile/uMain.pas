unit uMain;

//////////////////////////////////////
///  Lina Win File Example         ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

{ Dies ist ein Beispielprojekt, um den Umgang und die mögliche Verwendeng der
  TWinFile-Klasse und einiger uFileTools-Methoden von LinaComponents zu
  veranschaulichen.
  Es darf als Grundlage für eingene Projekte ohne vorherige Einwilligung
  verwendet werden, solange mindestens eine LinaComponents-Unit im Projekt
  verarbeitet wird.

  WICHTIG: Im Gegensatz zur Entwicklung der LinaComponents selbst wurde bei
           diesem Beispielprojekt keinerlei Rücksicht auf Abwärtskompatibilität
           genommen. Dieses Beispielprogramm wurde in Embarcadero Delphi XE5
           erstellt, entwickelt und kompiliert und könnte bei älteren Delphi-
           Versionen zu Kompilierungsfehlern führen. }

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uFileTools, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    edPath: TEdit;
    miBrowse: TButton;
    lbOtherFiles: TListBox;
    gbProperties: TGroupBox;
    leFileName: TLabeledEdit;
    leFileExt: TLabeledEdit;
    leFolderName: TLabeledEdit;
    laOtherFiles: TLabel;
    lbOtherFolders: TListBox;
    laOtherFolders: TLabel;
    pnButtons: TPanel;
    btExecute: TButton;
    cbExecuteSafe: TCheckBox;
    OpenDialog: TOpenDialog;
    coExecuteMode: TComboBox;
    laExecuteMode: TLabel;
    imIcon: TImage;
    procedure edPathChange(Sender: TObject);
    procedure miBrowseClick(Sender: TObject);
    procedure btExecuteClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btExecuteClick(Sender: TObject);
var
  CurrentFile: TWinFile;
begin
  if FileExists(edPath.Text) then
  begin
    CurrentFile := TWinFile.Create(edPath.Text);
    try
      case coExecuteMode.ItemIndex of
        0: CurrentFile.ExecuteMode := feOpen;
        1: CurrentFile.ExecuteMode := feEdit;
      end;
      if cbExecuteSafe.Checked = True then
      begin
        CurrentFile.SafeExecute;
      end else
      begin
        CurrentFile.Execute;
      end;
    finally
      CurrentFile.Free;
    end;
  end;
end;

procedure TfmMain.edPathChange(Sender: TObject);
var
  CurrentFile: TWinFile;
  List: TStrings;
begin
  if FileExists(edPath.Text) then
  begin
    CurrentFile := TWinFile.Create(edPath.Text);
    List := TStringList.Create;
    try
      leFileName.Text := CurrentFile.GetFileName(False);
      leFileExt.Text := CurrentFile.GetExtension(False);
      leFolderName.Text := CurrentFile.GetFolderName;
      List.Clear;
      ListFolders(CurrentFile.GetPath,List);
      lbOtherFolders.Items.Assign(List);
      List.Clear;
      ListFiles(CurrentFile.GetPath,List,['*.pas','*.dpr'],[fnExtension]);
      lbOtherFiles.Items.Assign(List);
      imIcon.Picture.Graphic := TIcon.Create;
      imIcon.Picture.Graphic.Assign(CurrentFile.GetIcon);
    finally
      CurrentFile.Free;
      List.Free;
    end;
  end;
end;

procedure TfmMain.miBrowseClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    edPath.Text := OpenDialog.FileName;
  end;
end;

end.
