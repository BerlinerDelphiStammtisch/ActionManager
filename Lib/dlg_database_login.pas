unit DLG_database_Login;

interface

uses {Delphi} SysUtils, Windows, Messages, Classes, Vcl.Graphics, Vcl.Controls,
              Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,
     {GeODin} G3MSG;
     {HilfeNot}

type
  TGDOLoginDialog = class(TForm)
    Panel: TPanel;
    Bevel: TBevel;
    DatabaseName: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Password: TEdit;
    UserName: TEdit;
    procedure FormShow(Sender: TObject);
  end;

function GDOLoginDialog(const ADatabaseName: string; var AUserName, APassword: string): Boolean;

implementation

{$R *.DFM}

function GDOLoginDialog(const ADatabaseName:string; var AUserName,APassword:string): Boolean;
begin
  with TGDOLoginDialog.Create(Application) do
  try
    Label3.Caption:=LoadStr(G0014)+':';
    Label1.Caption:=LoadStr(G3001)+':';
    Label2.Caption:=LoadStr(G0193)+':';
    OKButton.Caption:=LoadStr(ALLF_OK);
    CancelButton.Caption:=LoadStr(ALLF_Abbrechen);
    DatabaseName.Caption := ADatabaseName;
    UserName.Text := AUserName;
    Result := False;
    if AUserName = '' then ActiveControl := UserName;
    if ShowModal = mrOk then
    begin
      AUserName := trimRight(UserName.Text);
      APassword := trimRight(Password.Text);
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TGDOLoginDialog.FormShow(Sender: TObject);
begin
  if (DatabaseName.Width + DatabaseName.Left) >= Panel.ClientWidth then
    DatabaseName.Width := (Panel.ClientWidth - DatabaseName.Left) - 5;
end;

end.
