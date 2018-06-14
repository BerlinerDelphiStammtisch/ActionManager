unit uMain;

//////////////////////////////////////
///  Lina ListBox Manager Example  ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, uBase,
  uSysTools, uFrmCtrls;

type
  TfmMain = class(TForm)
    pnButtons: TPanel;
    btAdd: TButton;
    lbItems: TListBox;
    ListBoxManager: TListBoxManager;
    laFilter: TLabel;
    edFilter: TEdit;
    cbCase: TCheckBox;
    procedure cbCaseClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure edFilterChange(Sender: TObject);
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
  NewItem: String;
  FilterTemp: String;
begin
  if lbItems.ItemIndex > -1 then
  begin
    NewItem := lbItems.Items.Strings[lbItems.ItemIndex];
  end else
  begin
    NewItem := '';
  end;
  if InputQuery('Add item','Item name',NewItem) = True then
  begin
    NewItem := Trim(NewItem);
    if Length(NewItem) > 0 then
    begin
      FilterTemp := edFilter.Text;
      edFilter.Text := '';
      listboxmanager.Update;
      lbItems.Items.Add(NewItem);
      ListBoxManager.LoadList;
      edFilter.Text := FilterTemp;
    end;
  end;
end;

procedure TfmMain.cbCaseClick(Sender: TObject);
begin
  if cbCase.Checked = True then
  begin
    ListBoxManager.FilterOptions := [sfoCaseSensitive,sfoForceTrim];
  end else
  begin
    ListBoxManager.FilterOptions := [sfoForceTrim,sfoDefaultVisible,sfoDefaultVisible];
  end;
  ListBoxManager.Update;
end;

procedure TfmMain.edFilterChange(Sender: TObject);
begin
  ListBoxManager.Update;
end;

end.
