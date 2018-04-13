unit uMain;

//////////////////////////////////////
///  Lina Param Definer Example    ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, ShellAPI, uBase, uFrmCtrls;

type
  TfmMain = class(TForm)
    pnButtons: TPanel;
    btRestart: TButton;
    gbFormat: TGroupBox;
    lePrefix: TLabeledEdit;
    leSuffix: TLabeledEdit;
    leSeparator: TLabeledEdit;
    btRefresh: TButton;
    ParamDefiner: TParamDefiner;
    ListView: TListView;
    StatusBar: TStatusBar;
    procedure btRefreshClick(Sender: TObject);
    procedure btRestartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lePrefixChange(Sender: TObject);
    procedure leSeparatorChange(Sender: TObject);
    procedure leSuffixChange(Sender: TObject);
  private
    { Private-Deklarationen }
    Params: array [100..999] of String;
    procedure CreateReferences;
    procedure CreateItems;
    procedure GenerateParamFiles;
    function GenerateParamsString: String;
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.CreateReferences;
var
  Index: Integer;
  IdentList: TStrings;
begin
  IdentList := TStringList.Create;
  try
    IdentList.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Idents.txt');
    for Index := 0 to 899 do
    begin
      with (ParamDefiner.References.Add as TParamReference) do
      begin
        DefaultValue := '(not defined)';
        Identifier := IdentList.Strings[Index];
        Connector := @Params[Index + 100];
        Format.Prefix := lePrefix.Text;
        Format.Separator := leSeparator.Text;
        Format.Suffix := leSuffix.Text;
      end;
    end;
  finally
    IdentList.Free;
  end;
end;

procedure TfmMain.CreateItems;
var
  Index: Integer;
begin
  for Index := 0 to 899 do
  begin
    with ListView.Items.Add do
    begin
      Caption := (ParamDefiner.References.Items[Index] as TParamReference).Identifier;
      SubItems.Add(Params[Index + 100]);
    end;
  end;
end;

procedure TfmMain.GenerateParamFiles;
var
  ItemList: TStrings;
  Index: Integer;
begin
  if FileExists(ExtractFilePath(Application.ExeName) + 'Idents.txt') = False then
  begin
    ItemList := TStringList.Create;
    try
      for Index := 100 to 999 do
      begin
        ItemList.Add(IntToStr(Index));
      end;
      ItemList.SaveToFile(ExtractFilePath(Application.ExeName) + 'Idents.txt');
    finally
      ItemList.Free;
    end;
  end;
  if FileExists(ExtractFilePath(Application.ExeName) + 'Values.txt') = False then
  begin
    ItemList := TStringList.Create;
    try
      for Index := 100 to 999 do
      begin
        ItemList.Add(IntToStr(100000 + Random(899999)));
      end;
      ItemList.SaveToFile(ExtractFilePath(Application.ExeName) + 'Values.txt');
    finally
      ItemList.Free;
    end;
  end;
end;

function TfmMain.GenerateParamsString: String;
{ Diese Funktion dient nur der Veranschaulichung und sollte an eigenen Projekten
  nicht verwendet werden. Stattdessen sollten Funktionen wie "StringFromParams"
  aus der "uFrmCtrls"-Unit verwendet werden. }
var
  IdentList: TStrings;
  ValueList: TStrings;
  Index: Integer;
  TempStr: String;
begin
  Result := '';
  TempStr := '';
  IdentList := TStringList.Create;
  ValueList := TStringList.Create;
  try
    IdentList.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Idents.txt');
    ValueList.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Values.txt');
    for Index := 0 to IdentList.Count - 1 do
    begin
      if Index > 0 then
      begin
        TempStr := TempStr + ' ';
      end;
      TempStr := TempStr  + lePrefix.Text
                          + IdentList.Strings[Index]
                          + leSeparator.Text
                          + ValueList.Strings[Index]
                          + leSuffix.Text;
    end;
  finally
    IdentList.Free;
    ValueList.Free;
  end;
  Result := TempStr;
end;

procedure TfmMain.lePrefixChange(Sender: TObject);
var
  Index: Integer;
  TempStr: TCaption;
  OldIndex: Integer;
begin
  TempStr := '';
  for Index := 1 to Length(lePrefix.Text) do
  begin
    if not (lePrefix.Text[Index] in [#34,#39]) then
    begin
      TempStr := TempStr + lePrefix.Text[Index];
    end;
  end;
  if TempStr <> lePrefix.Text then
  begin
    OldIndex := lePrefix.SelStart;
    lePrefix.Text := TempStr;
    lePrefix.SelStart := OldIndex;
    Beep;
  end;
  for Index := 0 to ParamDefiner.References.Count - 1 do
  begin
    (ParamDefiner.References.Items[Index] as TParamReference).Format.Prefix := lePrefix.Text;
  end;
end;

procedure TfmMain.leSeparatorChange(Sender: TObject);
var
  Index: Integer;
  TempStr: TCaption;
  OldIndex: Integer;
begin
  TempStr := '';
  for Index := 1 to Length(leSeparator.Text) do
  begin
    if not (leSeparator.Text[Index] in [#34,#39]) then
    begin
      TempStr := TempStr + leSeparator.Text[Index];
    end;
  end;
  if TempStr <> leSeparator.Text then
  begin
    OldIndex := leSeparator.SelStart;
    leSeparator.Text := TempStr;
    leSeparator.SelStart := OldIndex;
    Beep;
  end;
  for Index := 0 to ParamDefiner.References.Count - 1 do
  begin
    (ParamDefiner.References.Items[Index] as TParamReference).Format.Separator := leSeparator.Text;
  end;
end;

procedure TfmMain.leSuffixChange(Sender: TObject);
var
  Index: Integer;
  TempStr: TCaption;
  OldIndex: Integer;
begin
  TempStr := '';
  for Index := 1 to Length(leSuffix.Text) do
  begin
    if not (leSuffix.Text[Index] in [#34,#39]) then
    begin
      TempStr := TempStr + leSuffix.Text[Index];
    end;
  end;
  if TempStr <> leSuffix.Text then
  begin
    OldIndex := leSuffix.SelStart;
    leSuffix.Text := TempStr;
    leSuffix.SelStart := OldIndex;
    Beep;
  end;
  for Index := 0 to ParamDefiner.References.Count - 1 do
  begin
    (ParamDefiner.References.Items[Index] as TParamReference).Format.Suffix := leSuffix.Text;
  end;
end;

procedure TfmMain.btRefreshClick(Sender: TObject);
var
  StartCount: Cardinal;
  FinishCount: Cardinal;
begin
  try
    StatusBar.SimpleText := 'Loading params...';
    ParamDefiner.References.Clear;
    ListView.Items.Clear;
    CreateReferences;
    StartCount := GetTickCount;
    ParamDefiner.Update;
    FinishCount := GetTickCount;
    StatusBar.SimpleText := 'Update time: ' + IntToStr(FinishCount - StartCount) + 'ms with ' + IntToStr(ParamCount) + ' params (' + IntToStr((FinishCount - StartCount) div ParamCount) + 'ms/param)';
    CreateItems;
  except
    StatusBar.SimpleText := 'No params loaded';
  end;
end;

procedure TfmMain.btRestartClick(Sender: TObject);
begin
  ShellExecute(Application.Handle,'open',PChar(Application.ExeName),PChar(GenerateParamsString),PChar(ExtractFileDir(Application.ExeName)),SW_NORMAL);
  Close;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  ListView.Column[1].Width := ListView.ClientWidth - ListView.Column[0].Width - GetSystemMetrics(SM_CXVSCROLL);
  GenerateParamFiles;
  lePrefix.Text := '[';
  leSeparator.Text := ':';
  leSuffix.Text := ']';
  if ParamCount >= 900 then
  begin
    btRefresh.Click;
  end else
  begin
    StatusBar.SimpleText := 'No params found';
    ListView.Enabled := False;
    btRefresh.Enabled := False;
  end;
end;

end.
