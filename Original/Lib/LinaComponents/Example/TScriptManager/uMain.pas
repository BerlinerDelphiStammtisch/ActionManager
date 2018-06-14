unit uMain;

//////////////////////////////////////
///  Lina Script Manager Example   ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

{ Dies ist ein Beispielprojekt, um den Umgang und die mögliche Verwendeng der
  TScriptManager-Komponente von LinaComponents zu veranschaulichen.
  Es benötigt die SynEdit-Komponenten, um kompiliert werden zu können.
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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, SynEdit, SynEditHighlighter,
  SynHighlighterPas, Vcl.StdCtrls, uBase, uScriptMgr, Vcl.ExtDlgs;

type
  TfmMain = class(TForm)
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileSep: TMenuItem;
    miFileClose: TMenuItem;
    miRun: TMenuItem;
    miInfo: TMenuItem;
    miInfoAbout: TMenuItem;
    SynEdit: TSynEdit;
    SynPasSyn: TSynPasSyn;
    ScriptManager: TScriptManager;
    ListBox: TListBox;
    miEdit: TMenuItem;
    miEditUndo: TMenuItem;
    miEditRedo: TMenuItem;
    OpenTextFileDialog: TOpenTextFileDialog;
    SaveTextFileDialog: TSaveTextFileDialog;
    miRunCompileExecute: TMenuItem;
    miEditSep: TMenuItem;
    miEditSelectAll: TMenuItem;
    miRunLibrary: TMenuItem;
    PopupMenu: TPopupMenu;
    miPopupSelectAll: TMenuItem;
    miPopupCopy: TMenuItem;
    miPopupPaste: TMenuItem;
    miPopupCut: TMenuItem;
    miPopupSep: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure miInfoAboutClick(Sender: TObject);
    procedure miFileCloseClick(Sender: TObject);
    procedure miEditClick(Sender: TObject);
    procedure miEditUndoClick(Sender: TObject);
    procedure miEditRedoClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miRunCompileExecuteClick(Sender: TObject);
    procedure miEditSelectAllClick(Sender: TObject);
    procedure miRunLibraryClick(Sender: TObject);
    procedure miPopupSelectAllClick(Sender: TObject);
    procedure miPopupCopyClick(Sender: TObject);
    procedure miPopupCutClick(Sender: TObject);
    procedure miPopupPasteClick(Sender: TObject);
    procedure ScriptManagerReturn(Sender: TObject; const Msg: string);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  ScriptManager.VarApplication := Application;
  ScriptManager.ReturnTarget := ListBox.Items;
end;

procedure TfmMain.miInfoAboutClick(Sender: TObject);
begin
  ScriptManager.About.AboutDlg;
end;

procedure TfmMain.miPopupCopyClick(Sender: TObject);
begin
  SynEdit.CopyToClipboard;
end;

procedure TfmMain.miPopupCutClick(Sender: TObject);
begin
  SynEdit.CutToClipboard;
end;

procedure TfmMain.miPopupPasteClick(Sender: TObject);
begin
  SynEdit.PasteFromClipboard;
end;

procedure TfmMain.miPopupSelectAllClick(Sender: TObject);
begin
  miEditSelectAll.Click;
end;

procedure TfmMain.miFileCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.miRunCompileExecuteClick(Sender: TObject);
begin
  ListBox.Items.Clear;
  ScriptManager.Code.Assign(SynEdit.Lines);
  if miRunLibrary.Checked = False then
  begin
    ScriptManager.VarSelf := nil;
    ScriptManager.VarApplication := nil;
  end;
  ScriptManager.CompileAndExecute;
end;

procedure TfmMain.miEditClick(Sender: TObject);
begin
  miEditUndo.Enabled := (SynEdit.UndoList.ItemCount > 0);
  miEditRedo.Enabled := (SynEdit.RedoList.ItemCount > 0);
end;

procedure TfmMain.miRunLibraryClick(Sender: TObject);
begin
  if miRunLibrary.Checked = True then
  begin
    ScriptManager.Libraries := [slClasses,slControls,slStdCtrls,slForms,slDateUtils,slComObj,slDB,slCustom];
  end else
  begin
    ScriptManager.Libraries := [];
  end;
end;

procedure TfmMain.ScriptManagerReturn(Sender: TObject; const Msg: string);
begin
  ListBox.TopIndex := ListBox.Items.Count - 1;
end;

procedure TfmMain.miFileOpenClick(Sender: TObject);
begin
  if OpenTextFileDialog.Execute then
  begin
    SynEdit.Lines.LoadFromFile(OpenTextFileDialog.FileName);
    SynEdit.UndoList.Clear;
    SynEdit.RedoList.Clear;
  end;
end;

procedure TfmMain.miEditRedoClick(Sender: TObject);
begin
  SynEdit.Redo;
end;

procedure TfmMain.miFileSaveClick(Sender: TObject);
begin
  if SaveTextFileDialog.Execute then
  begin
    SynEdit.Lines.SaveToFile(SaveTextFileDialog.FileName);
    SynEdit.UndoList.Clear;
    SynEdit.RedoList.Clear;
  end;
end;

procedure TfmMain.miEditSelectAllClick(Sender: TObject);
begin
  SynEdit.SelectAll;
end;

procedure TfmMain.miEditUndoClick(Sender: TObject);
begin
  SynEdit.Undo;
end;

end.
