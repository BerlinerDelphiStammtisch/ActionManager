unit uMain;

//////////////////////////////////////
///  Lina File Tools Example       ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

{ Dies ist ein Beispielprojekt, um den Umgang und die mögliche Verwendeng der
  TDiagram-Komponente zu veranschaulichen.
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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uBase, Vcl.StdCtrls, uAdvCtrls,
  Vcl.ExtCtrls, Vcl.Menus, uSysTools, uFrmCtrls, uValue, Vcl.ImgList;

type
  TfmMain = class(TForm)
    Diagram: TDiagram;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miValues: TMenuItem;
    miView: TMenuItem;
    miInfo: TMenuItem;
    miFileNew: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileClose: TMenuItem;
    miValuesValues: TMenuItem;
    miValuesAdd: TMenuItem;
    miValuesImport: TMenuItem;
    miInfoAbout: TMenuItem;
    miViewGrid: TMenuItem;
    miViewBars: TMenuItem;
    miViewArtLines: TMenuItem;
    miViewValues: TMenuItem;
    miTools: TMenuItem;
    miToolsTrendLine: TMenuItem;
    miFileSaveAs: TMenuItem;
    miToolsCaption: TMenuItem;
    miColor: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    miValuesExport: TMenuItem;
    ColorDialog: TColorDialog;
    ImageList: TImageList;
    procedure miValuesValuesValueClick(Sender: TObject);
    procedure miInfoAboutClick(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure miFileNewClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miToolsCaptionClick(Sender: TObject);
    procedure miToolsTrendLineClick(Sender: TObject);
    procedure miViewGridClick(Sender: TObject);
    procedure miViewBarsClick(Sender: TObject);
    procedure miColorClick(Sender: TObject);
    procedure miViewArtLinesClick(Sender: TObject);
    procedure miViewValuesClick(Sender: TObject);
    procedure miValuesImportClick(Sender: TObject);
    procedure miValuesExportClick(Sender: TObject);
    procedure miFileCloseClick(Sender: TObject);
    procedure miValuesClick(Sender: TObject);
    procedure miValuesAddClick(Sender: TObject);
    procedure miViewClick(Sender: TObject);
    procedure miToolsClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
    Modified: Boolean;
    FileName: String;
  public
    { Public-Deklarationen }
    procedure Load;
    procedure Save;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormShow(Sender: TObject);
begin
  miFileNew.Click;
end;

procedure TfmMain.Load;
var
  NewDiagram: TDiagram;
begin
  if Length(FileName) = 0 then
  begin
    NewDiagram := TDiagram.Create(Self);
    NewDiagram := TDiagram.Create(Self);
    NewDiagram.Parent := Diagram.Parent;
    NewDiagram.Align := Diagram.Align;
    NewDiagram.Padding.Assign(Diagram.Padding);
    Diagram.Free;
    Diagram := NewDiagram;
  end else
  begin
    ComponentLoadFromFile(FileName, Diagram);
  end;
end;

procedure TfmMain.Save;
begin
  ComponentSaveToFile(FileName, Diagram);
end;

procedure TfmMain.miColorClick(Sender: TObject);
begin
  ColorDialog.Color := Diagram.Color;
  if ColorDialog.Execute then
  begin
    Diagram.Color := ColorDialog.Color;
  end;
end;

procedure TfmMain.miFileClick(Sender: TObject);
begin
  if (not Modified) or (MessageDlg('There exist unsaved changes, are you sure you want to close?',mtWarning,mbYesNo,0) = mrYes) then
  begin
    Close;
  end;
end;

procedure TfmMain.miFileCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.miFileNewClick(Sender: TObject);
begin
  SetLength(FileName,0);
  Load;
end;

procedure TfmMain.miFileOpenClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  OpenDialog.Title := 'Open diagram';
  OpenDialog.DefaultExt := '.dgm';
  OpenDialog.Filter := 'Diagram files (*.dgm)|*.dgm';
  if OpenDialog.Execute then
  begin
    FileName := OpenDialog.FileName;
    Load;
  end;
end;

procedure TfmMain.miFileSaveAsClick(Sender: TObject);
begin
  SaveDialog.FileName := '';
  SaveDialog.Title := 'Save diagram';
  SaveDialog.DefaultExt := '.dgm';
  SaveDialog.Filter := 'Diagram files (*.dgm)|*.dgm';
  if SaveDialog.Execute then
  begin
    FileName := SaveDialog.FileName;
    miFileSave.Click;
  end;
end;

procedure TfmMain.miFileSaveClick(Sender: TObject);
begin
  if Length(FileName) = 0 then
  begin
    miFileSaveAs.Click;
  end else
  begin
    Save;
  end;
end;

procedure TfmMain.miValuesValuesValueClick(Sender: TObject);
var
  BMP: TBitmap;
begin
  fmValue.Data := (Diagram.Values.Items[miValuesValues.IndexOf(Sender as TMenuItem)] as TDiagramValue).Data;
  if fmValue.ShowModal = mrOk then
  begin
    (Diagram.Values.Items[miValuesValues.IndexOf(Sender as TMenuItem)] as TDiagramValue).Data := fmValue.Data;
    ImageList.Delete(miValuesValues.IndexOf(Sender as TMenuItem));
    BMP := TBitmap.Create;
    try
      BMP.SetSize(ImageList.Width, ImageList.Height);
      BMP.Canvas.Brush.Color := fmValue.Data.Color;
      BMP.Canvas.FillRect(BmpRect(BMP));
      ImageList.Insert(miValuesValues.IndexOf(Sender as TMenuItem), BMP, nil);
    finally
      BMP.Free;
    end;
  end;
end;

procedure TfmMain.miInfoAboutClick(Sender: TObject);
begin
  Diagram.About.AboutDlg;
end;

procedure TfmMain.miToolsCaptionClick(Sender: TObject);
var
  DiagramCaption: String;
begin
  DiagramCaption := Diagram.Caption.Text;
  if InputQuery('Diagram caption', 'Caption for this diagram:', DiagramCaption) then
  begin
    Diagram.Caption.Text := DiagramCaption;
  end;
end;

procedure TfmMain.miToolsClick(Sender: TObject);
begin
  miToolsTrendLine.Checked := Diagram.TrendLine.Visible;
end;

procedure TfmMain.miToolsTrendLineClick(Sender: TObject);
begin
  Diagram.TrendLine.Visible := not Diagram.TrendLine.Visible;
end;

procedure TfmMain.miValuesAddClick(Sender: TObject);
var
  BMP: TBitmap;
begin
  fmValue.Data := Default(TDiagramValueData);
  if fmValue.ShowModal = mrOk then
  begin
    (Diagram.Values.Add as TDiagramValue).Data := fmValue.Data; BMP := TBitmap.Create;
    try
      BMP.SetSize(ImageList.Width, ImageList.Height);
      BMP.Canvas.Brush.Color := fmValue.Data.Color;
      BMP.Canvas.FillRect(BmpRect(BMP));
      ImageList.Add(BMP, nil);
    finally
      BMP.Free;
    end;
  end;
end;

procedure TfmMain.miValuesClick(Sender: TObject);
var
  Index: Integer;
begin
  miValuesValues.Clear;
  for Index := 0 to Diagram.Values.Count - 1 do
  begin
    miValuesValues.Add(TMenuItem.Create(miValuesValues));
    miValuesValues.Items[Index].Caption := (Diagram.Values.Items[Index] as TDiagramValue).Name + '...';
    miValuesValues.Items[Index].OnClick := miValuesValuesValueClick;
    miValuesValues.Items[Index].ImageIndex := Index;
  end;
  miValuesValues.Enabled := miValuesValues.Count <> 0;
end;

procedure TfmMain.miValuesExportClick(Sender: TObject);
begin
  SaveDialog.FileName := '';
  SaveDialog.Title := 'Export values';
  SaveDialog.DefaultExt := '.dvd';
  SaveDialog.Filter := 'Diagram value data files (*.dvd)|*.dvd';
  if SaveDialog.Execute then
  begin
    Diagram.Values.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TfmMain.miValuesImportClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  SaveDialog.Title := 'Import values';
  OpenDialog.DefaultExt := '.dvd';
  OpenDialog.Filter := 'Diagram value data files (*.dvd)|*.dvd';
  if OpenDialog.Execute then
  begin
    Diagram.Values.ImportFromFile(OpenDialog.FileName);
  end;
end;

procedure TfmMain.miViewArtLinesClick(Sender: TObject);
begin
  Diagram.Scale.ArtLines.Cursor.Visible := not Diagram.Scale.ArtLines.Cursor.Visible;
end;

procedure TfmMain.miViewBarsClick(Sender: TObject);
begin
  Diagram.Scale.Bar.Visible := not Diagram.Scale.Bar.Visible;
end;

procedure TfmMain.miViewClick(Sender: TObject);
begin
  miViewGrid.Checked := Diagram.Scale.Grid.Visible;
  miViewBars.Checked := Diagram.Scale.Bar.Visible;
  miViewArtLines.Checked := Diagram.Scale.ArtLines.Cursor.Visible;
end;

procedure TfmMain.miViewGridClick(Sender: TObject);
begin
  Diagram.Scale.Grid.Visible := not Diagram.Scale.Grid.Visible;
end;

procedure TfmMain.miViewValuesClick(Sender: TObject);
begin
  Diagram.Scale.Values.Visible := not Diagram.Scale.Values.Visible;
end;

end.
