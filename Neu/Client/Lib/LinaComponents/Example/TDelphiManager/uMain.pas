unit uMain;

//////////////////////////////////////
///  Lina Delphi Manager Example   ///
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
  uSysCtrls, Vcl.ComCtrls;

type
  TfmMain = class(TForm)
    rgVersions: TRadioGroup;
    pnButtons: TPanel;
    btLaunch: TButton;
    btRefresh: TButton;
    DelphiManager: TDelphiManager;
    btInfo: TButton;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure btRefreshClick(Sender: TObject);
    procedure btLaunchClick(Sender: TObject);
    procedure btInfoClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btInfoClick(Sender: TObject);
begin
  MessageDlg(DelphiVersionToFullName(NameToDelphiVersion(rgVersions.Items.Strings[rgVersions.ItemIndex])) +
           sLineBreak + 'Code name: ' + DelphiVersionToCodeName(NameToDelphiVersion(rgVersions.Items.Strings[rgVersions.ItemIndex])) +
           sLineBreak + 'Product version: ' + IntToStr(DelphiVersionToProductVersion(NameToDelphiVersion(rgVersions.Items.Strings[rgVersions.ItemIndex]))) +
           sLineBreak + 'Package version: ' + IntToStr(DelphiVersionToPackageVersion(NameToDelphiVersion(rgVersions.Items.Strings[rgVersions.ItemIndex]))),
  mtInformation,[mbOK],0);
end;

procedure TfmMain.btLaunchClick(Sender: TObject);
begin
  DelphiManager.Versions[NameToDelphiVersion(rgVersions.Items.Strings[rgVersions.ItemIndex])].Launch;
end;

procedure TfmMain.btRefreshClick(Sender: TObject);
var
  Installed: TDelphiVersions;
  Index: TDelphiVersion;
begin
  rgVersions.Items.Clear;
  Installed := DelphiManager.InstalledVersions;
  for Index := Low(TDelphiVersion) to High(TDelphiVersion) do
  begin
    if Index in Installed then
    begin
      rgVersions.Items.Add(DelphiVersionToName(Index));
    end;
  end;
  if rgVersions.Items.Count <> 0 then
  begin
    rgVersions.ItemIndex := 0;
    btInfo.Enabled := True;
    btLaunch.Enabled := True;
  end else
  begin
    rgVersions.ItemIndex := -1;
    btInfo.Enabled := False;
    btLaunch.Enabled := False;
  end;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  StatusBar.SimpleText := 'Built with ' + DelphiManager.Info.VersionFullName + ' "' + DelphiManager.Info.VersionCodeName + '" and Lina Components v' + FloatToStr(DelphiManager.Info.LinaVersion) + ' for ' + DelphiManager.Info.PlatformFullName;
  btRefresh.Click;
end;

end.
