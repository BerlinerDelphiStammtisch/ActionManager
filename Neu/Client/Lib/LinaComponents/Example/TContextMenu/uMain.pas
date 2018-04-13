unit uMain;

//////////////////////////////////////
///  Lina Context Menu Example     ///
///  ****************************  ///
///  (c) 2014 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$IF CompilerVersion <> 26.0}
  {$MESSAGE ERROR 'This example was written to compile under Delphi XE5'}
{$ENDIF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.StdCtrls,
  uBase, uFileCtrls, Vcl.Menus;

type
  TfmMain = class(TForm)
    pnButtons: TPanel;
    btClose: TButton;
    btApply: TButton;
    gbMenuItems: TGroupBox;
    lwMenuItems: TListView;
    ContextMenu: TContextMenu;
    btRefresh: TButton;
    pmMenuItems: TPopupMenu;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miSeparator: TMenuItem;
    miEditCaption: TMenuItem;
    miEditCommand: TMenuItem;
    miEditIcon: TMenuItem;
    odIcon: TOpenDialog;
    procedure btCloseClick(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
    procedure btApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pmMenuItemsPopup(Sender: TObject);
    procedure miAddClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miEditCaptionClick(Sender: TObject);
    procedure miEditIconClick(Sender: TObject);
    procedure miEditCommandClick(Sender: TObject);
    procedure lwMenuItemsChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btApplyClick(Sender: TObject);
var
  Index: Integer;
begin
  ContextMenu.Items.Clear;
  for Index := 0 to lwMenuItems.Items.Count - 1 do
  begin
    with (ContextMenu.Items.Add as TContextMenuItem) do
    begin
      Name := lwMenuItems.Items.Item[Index].Caption;
      Caption := lwMenuItems.Items.Item[Index].SubItems.Strings[0];
      Command := lwMenuItems.Items.Item[Index].SubItems.Strings[1];
      Icon := lwMenuItems.Items.Item[Index].SubItems.Strings[2];
    end;
  end;
  ContextMenu.SaveToRegistry;
  btApply.Enabled := False;
end;

procedure TfmMain.btCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.btRefreshClick(Sender: TObject);
var
  Index: Integer;
begin
  ContextMenu.LoadFromRegistry;
  lwMenuItems.Items.Clear;
  for Index := 0 to ContextMenu.Items.Count - 1 do
  begin
    with lwMenuItems.Items.Add do
    begin
      Caption := (ContextMenu.Items.Items[Index] as TContextMenuItem).Name;
      SubItems.Add((ContextMenu.Items.Items[Index] as TContextMenuItem).Caption);
      SubItems.Add((ContextMenu.Items.Items[Index] as TContextMenuItem).Command);
      SubItems.Add((ContextMenu.Items.Items[Index] as TContextMenuItem).Icon);
    end;
  end;
  btApply.Enabled := False;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  Index: 0..3;
begin
  for Index := 0 to 3 do
  begin
    lwMenuItems.Column[Index].Width := (lwMenuItems.ClientWidth - GetSystemMetrics(SM_CXVSCROLL)) div 4;
    lwMenuItems.Column[Index].MinWidth := lwMenuItems.Column[Index].Width;
    lwMenuItems.Column[Index].MaxWidth := lwMenuItems.Column[Index].Width;
  end;
  btRefresh.Click;
end;

procedure TfmMain.lwMenuItemsChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  btApply.Enabled := True;
end;

procedure TfmMain.miAddClick(Sender: TObject);
begin
  with lwMenuItems.Items.Add do
  begin
    Caption := 'Item' + IntToStr(lwMenuItems.Items.Count);
    SubItems.Add('');
    SubItems.Add('');
    SubItems.Add('');
  end;
end;

procedure TfmMain.miDeleteClick(Sender: TObject);
begin
  lwMenuItems.Items.Delete(lwMenuItems.ItemIndex);
end;

procedure TfmMain.miEditCaptionClick(Sender: TObject);
var
  NewCaption: String;
begin
  NewCaption := lwMenuItems.Items.Item[lwMenuItems.ItemIndex].SubItems.Strings[0];
  if InputQuery('Edit item caption','Caption:',NewCaption) then
  begin
    if Length(NewCaption) > 0 then
    begin
      lwMenuItems.Items.Item[lwMenuItems.ItemIndex].SubItems.Strings[0] := NewCaption;
    end;
  end;
end;

procedure TfmMain.miEditCommandClick(Sender: TObject);
var
  NewCommand: String;
begin
  NewCommand := lwMenuItems.Items.Item[lwMenuItems.ItemIndex].SubItems.Strings[1];
  if InputQuery('Edit item command','Command:',NewCommand) then
  begin
    if Length(NewCommand) > 0 then
    begin
      lwMenuItems.Items.Item[lwMenuItems.ItemIndex].SubItems.Strings[1] := NewCommand;
    end;
  end;
end;

procedure TfmMain.miEditIconClick(Sender: TObject);
begin
  odIcon.FileName := lwMenuItems.Items.Item[lwMenuItems.ItemIndex].SubItems.Strings[2];
  if odIcon.Execute = True then
  begin
    lwMenuItems.Items.Item[lwMenuItems.ItemIndex].SubItems.Strings[2] := odIcon.FileName;
  end;
end;

procedure TfmMain.pmMenuItemsPopup(Sender: TObject);
begin
  miDelete.Enabled := (lwMenuItems.ItemIndex > -1);
  miEditCaption.Enabled := (lwMenuItems.ItemIndex > -1);
  miEditCommand.Enabled := (lwMenuItems.ItemIndex > -1);
  miEditIcon.Enabled := (lwMenuItems.ItemIndex > -1);
end;

end.
