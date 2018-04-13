unit uMain;

//////////////////////////////////////
///  Lina Process Manager Example  ///
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
  Vcl.ComCtrls, ShellAPI, uBase, uFrmCtrls, Vcl.Menus, uSysCtrls, uFileTools;

type
  TfmMain = class(TForm)
    pnButtons: TPanel;
    btTerminate: TButton;
    btRefresh: TButton;
    ListView: TListView;
    StatusBar: TStatusBar;
    MainMenu: TMainMenu;
    miProcess: TMenuItem;
    miInterval: TMenuItem;
    miProcessLaunch: TMenuItem;
    miProcessTerminate: TMenuItem;
    miIntervalNever: TMenuItem;
    miInterval1000: TMenuItem;
    miInterval500: TMenuItem;
    miInterval200: TMenuItem;
    miInterval50: TMenuItem;
    ProcessManager: TProcessManager;
    procedure btRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miIntervalNeverClick(Sender: TObject);
    procedure miInterval1000Click(Sender: TObject);
    procedure miInterval500Click(Sender: TObject);
    procedure miInterval200Click(Sender: TObject);
    procedure miInterval50Click(Sender: TObject);
    procedure ProcessManagerUpdate(Sender: TObject; const Modified: Boolean);
    procedure miProcessLaunchClick(Sender: TObject);
    procedure miProcessTerminateClick(Sender: TObject);
    procedure btTerminateClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btRefreshClick(Sender: TObject);
begin
  ProcessManager.Update;
end;

procedure TfmMain.btTerminateClick(Sender: TObject);
begin
  if MessageDlg('Terminate process "' + ListView.Selected.Caption + '"?',mtConfirmation,mbYesNo,0) = mrYes then
  begin
    ProcessManager.Kill(ListView.Selected.Caption);
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  btRefresh.Click;
  miInterval1000.Click;
end;

procedure TfmMain.miInterval1000Click(Sender: TObject);
begin
  ProcessManager.RefreshMode := prTime;
  ProcessManager.Interval := 1000;
end;

procedure TfmMain.miInterval200Click(Sender: TObject);
begin
  ProcessManager.RefreshMode := prTime;
  ProcessManager.Interval := 200;
end;

procedure TfmMain.miInterval500Click(Sender: TObject);
begin
  ProcessManager.RefreshMode := prTime;
  ProcessManager.Interval := 500;
end;

procedure TfmMain.miInterval50Click(Sender: TObject);
begin
  ProcessManager.RefreshMode := prTime;
  ProcessManager.Interval := 50;
end;

procedure TfmMain.miIntervalNeverClick(Sender: TObject);
begin
  ProcessManager.RefreshMode := prNone;
end;

procedure TfmMain.miProcessLaunchClick(Sender: TObject);
var
  FileName: String;
begin
  if InputQuery('Launch','File name:',FileName) and (Length(FileName) <> 0) then
  begin
    ProcessManager.Start(FileName);
  end;
end;

procedure TfmMain.miProcessTerminateClick(Sender: TObject);
begin
  btTerminate.Click;
end;

procedure TfmMain.ProcessManagerUpdate(Sender: TObject;
  const Modified: Boolean);
var
  Index: Integer;
begin
  while ListView.Items.Count < ProcessManager.Names.Count do
  begin
    ListView.Items.Add;
  end;
  while ListView.Items.Count > ProcessManager.Names.Count do
  begin
    ListView.Items.Delete(ListView.Items.Count - 1);
  end;
  for Index := 0 to ProcessManager.Names.Count - 1 do
  begin
    ListView.Items.Item[Index].Caption := ProcessManager.Names.Strings[Index];
    while ListView.Items.Item[Index].SubItems.Count <> 4 do
    begin
      ListView.Items.Item[Index].SubItems.Add('');
    end;
    ListView.Items.Item[Index].SubItems.Strings[0] := IntToStr(ProcessManager.GetID(ProcessManager.Names.Strings[Index]));
    ListView.Items.Item[Index].SubItems.Strings[1] := IntToStr(ProcessManager.GetPriority(ProcessManager.Names.Strings[Index]));
    try
      ListView.Items.Item[Index].SubItems.Strings[2] := IntToStr(ConvertFileSize(ProcessManager.GetMemory(ProcessManager.Names.Strings[Index]), B_TO_KB)) + ' K';
    except
      ListView.Items.Item[Index].SubItems.Strings[2] := '?';
    end;
    try
      ListView.Items.Item[Index].SubItems.Strings[3] := ProcessManager.GetUser(ProcessManager.Names.Strings[Index]);
    except
      ListView.Items.Item[Index].SubItems.Strings[3] := '?';
    end;
  end;
  StatusBar.SimpleText := IntToStr(ListView.Items.Count) + ' processes listed';
end;

end.
