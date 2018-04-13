unit uMain;

//////////////////////////////////////
///  Lina Web Document Example     ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

{ Dies ist ein Beispielprojekt, um den Umgang und die mögliche Verwendeng der
  THtmlDocument-Klasse und einiger uWebCtrls-Methoden von LinaComponents zu
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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.ComCtrls, SynEditHighlighter, SynHighlighterHtml, SynEdit, Vcl.Menus,
  uWebCtrls, SynMemo;

type
  TfmMain = class(TForm)
    OpenDialog: TOpenDialog;
    tvStructure: TTreeView;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miOpen: TMenuItem;
    miClose: TMenuItem;
    miEdit: TMenuItem;
    miSelectAll: TMenuItem;
    miSelectNone: TMenuItem;
    miSepEdit: TMenuItem;
    miCopy: TMenuItem;
    pnProperties: TPanel;
    lvParams: TListView;
    SynHTMLSyn: TSynHTMLSyn;
    mmLines: TSynMemo;
    pmLines: TPopupMenu;
    miLinesSelectAll: TMenuItem;
    miLinesSelectNone: TMenuItem;
    miLinesSep: TMenuItem;
    miLinesCopy: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure tvStructureClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure miEditClick(Sender: TObject);
    procedure pmLinesPopup(Sender: TObject);
    procedure miLinesSelectAllClick(Sender: TObject);
    procedure miLinesSelectNoneClick(Sender: TObject);
    procedure miLinesCopyClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miSelectNoneClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
  private
    { Private-Deklarationen }
    Opened: Boolean;
    Nodes: TObjectDictionary<THtmlDocumentTag,TTreeNode>;
    Tags: TObjectDictionary<TTreeNode,THtmlDocumentTag>;
    HtmlDocument: THtmlDocument;
    procedure HtmlDocumentParseSuccess(Sender: TObject);
    procedure AddTagNode(Tag: THtmlDocumentTag);
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Nodes := TObjectDictionary<THtmlDocumentTag,TTreeNode>.Create([]);
  Tags := TObjectDictionary<TTreeNode,THtmlDocumentTag>.Create([]);
  HtmlDocument := THtmlDocument.Create;
  HtmlDocument.OnParseSuccess := HtmlDocumentParseSuccess;
  miClose.Click;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  Nodes.Free;
  Tags.Free;
  HtmlDocument.Free;
end;

procedure TfmMain.miCloseClick(Sender: TObject);
begin
  Opened := False;
  Nodes.Clear;
  Tags.Clear;
  tvStructure.Items.Clear;
  tvStructure.Enabled := False;
  lvParams.Items.Clear;
  lvParams.Enabled := False;
  mmLines.Lines.Clear;
  mmLines.Enabled := False;
end;

procedure TfmMain.miCopyClick(Sender: TObject);
begin
  mmLines.CopyToClipboard;
end;

procedure TfmMain.miCutClick(Sender: TObject);
begin
  mmLines.CutToClipboard;
end;

procedure TfmMain.miEditClick(Sender: TObject);
begin
  miSelectAll.Enabled := Opened and (ActiveControl = mmLines) and (mmLines.SelLength <> Length(mmLines.Lines.Text));
  miSelectNone.Enabled := Opened and (ActiveControl = mmLines) and (mmLines.SelLength <> 0);
  miCopy.Enabled := Opened and (ActiveControl = mmLines) and (mmLines.SelLength <> 0);
end;

procedure TfmMain.miFileClick(Sender: TObject);
begin
  miClose.Enabled := Opened;
end;

procedure TfmMain.miLinesCopyClick(Sender: TObject);
begin
  miCopy.Click;
end;

procedure TfmMain.miLinesSelectAllClick(Sender: TObject);
begin
  miSelectAll.Click;
end;

procedure TfmMain.miLinesSelectNoneClick(Sender: TObject);
begin
  miSelectNone.Click;
end;

procedure TfmMain.miOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    miClose.Click;
    Opened := True;
    tvStructure.Enabled := True;
    tvStructure.Items.Add(nil,'(root)');
    lvParams.Enabled := True;
    mmLines.Enabled := True;
    Nodes.Add(nil,tvStructure.Items.Item[0]);
    Tags.Add(tvStructure.Items.Item[0],nil);
    HtmlDocument.Lines.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TfmMain.miPasteClick(Sender: TObject);
begin
  mmLines.PasteFromClipboard;
end;

procedure TfmMain.miSelectAllClick(Sender: TObject);
begin
  mmLines.SelectAll;
end;

procedure TfmMain.miSelectNoneClick(Sender: TObject);
begin
  mmLines.SelLength := 0;
end;

procedure TfmMain.pmLinesPopup(Sender: TObject);
begin
  miEdit.Click;
  miLinesSelectAll.Enabled := miSelectAll.Enabled;
  miLinesSelectNone.Enabled := miSelectNone.Enabled;
  miLinesCopy.Enabled := miCopy.Enabled;
end;

procedure TfmMain.tvStructureClick(Sender: TObject);
var
  Index: Integer;
  Param: TListItem;
begin
  if tvStructure.SelectionCount <> 0 then
  begin
    lvParams.Items.Clear;
    if Tags.Items[tvStructure.Selected] = nil then
    begin
      mmLines.Lines.Assign(HtmlDocument.Lines);
    end else
    begin
      mmLines.Lines.Assign(Tags.Items[tvStructure.Selected].Lines);
      for Index := 0 to Tags.Items[tvStructure.Selected].ParamCount - 1 do
      begin
        Param := lvParams.Items.Add;
        Param.Caption := Tags.Items[tvStructure.Selected].Params[Index].Name;
        Param.SubItems.Add(Tags.Items[tvStructure.Selected].Params[Index].Name);
      end;
    end;
  end;
end;

procedure TfmMain.HtmlDocumentParseSuccess(Sender: TObject);
var
  Index: Integer;
begin
  for Index := 0 to HtmlDocument.TagCount - 1 do
  begin
    AddTagNode(HtmlDocument.Tags[Index]);
  end;
end;

procedure TfmMain.AddTagNode(Tag: THtmlDocumentTag);
var
  Index: Integer;
  Node: TTreeNode;
begin
  Node := tvStructure.Items.AddChild(Nodes.Items[Tag.Parent],Tag.Name);
  Nodes.Add(Tag,Node);
  Tags.Add(Node,Tag);
  for Index := 0 to Tag.TagCount - 1 do
  begin
    AddTagNode(Tag.Tags[Index]);
  end;
end;

end.
