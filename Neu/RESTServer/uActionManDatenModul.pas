unit uActionManDatenModul;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Comp.UI,
  uActionManActionObj;

type
  TActionManDataModule = class(TDataModule)
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDQuery1: TFDQuery;
    qryAction: TFDQuery;
    updAction: TFDUpdateSQL;
    FDTransaction1: TFDTransaction;
    procedure FDConnection1AfterConnect(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    function FindAMAction(Search: string): TDataSet;
    function GetAMActionById(id: Integer): TDataSet;
    procedure DeleteAMAction(id: Integer);
    procedure AddAMAction(AAction: TAMAction);
    procedure UpdateAMAction(AAction: TAMAction);
  end;

var
  ActionManDataModule: TActionManDataModule;

implementation
uses
  MVCFramework.FireDAC.Utils;

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TActionManDataModule.DataModuleCreate(Sender: TObject);
begin
  FDConnection1.Params.Database := '..\..\Database\ActionManager.db';
end;

procedure TActionManDataModule.FDConnection1AfterConnect(Sender: TObject);
var
  lSql: TStringList;
begin
  lSql := TStringList.Create;
  try
    lSql.Add('CREATE TABLE IF NOT EXISTS actionmanager_Option (');
    lSql.Add('   ident           INTEGER PRIMARY KEY  AUTOINCREMENT,');
    lSql.Add('   action_name     TEXT,'); // NOT NULL,');
    lSql.Add('   action_category TEXT'); //        INT,');
    lSql.Add(');');
    FDConnection1.ExecSQL(lSql.Text);

    lSql.Clear;
    lSql.Add('CREATE TABLE IF NOT EXISTS actionmanager_Action (');
    lSql.Add('   ident           INTEGER PRIMARY KEY  AUTOINCREMENT,');
    lSql.Add('   action_name     TEXT,'); // NOT NULL,');
    lSql.Add('   action_call     TEXT,'); //
    lSql.Add('   action_type     TEXT,'); //
    lSql.Add('   action_category TEXT,'); //
    lSql.Add('   action_created  DATETIME,'); //
    lSql.Add('   action_lastcall DATETIME,'); //
    lSql.Add('   action_callcnt  INTEGER'); //
    lSql.Add(');');
    FDConnection1.ExecSQL(lSql.Text);
  finally
    lSql.Free;
  end;
end;

function TActionManDataModule.FindAMAction(Search: string): TDataSet;
begin
  if Search.IsEmpty then
    qryAction.Open('select * from actionmanager_Action')
  else
    qryAction.Open('SELECT * FROM actionmanager_Action where action_name like ?', [ Search.QuotedString('%')]);
  Result := qryAction;
end;

function TActionManDataModule.GetAMActionById(id: Integer): TDataSet;
begin
  qryAction.Open('SELECT * FROM actionmanager_Action where ident = ?', [id]);
  Result := qryAction;
end;

procedure TActionManDataModule.UpdateAMAction(AAction: TAMAction);
begin
  TFireDACUtils.ObjectToParameters(updAction.Commands[arUpdate].Params, AAction, 'NEW_');
  updAction.Commands[arUpdate].Params.ParamByName('OLD_IDENT').AsInteger := AAction.Ident;
  updAction.Commands[arUpdate].Execute;
end;

procedure TActionManDataModule.AddAMAction(AAction: TAMAction);
begin
  TFireDACUtils.ObjectToParameters(updAction.Commands[arInsert].Params, AAction, 'NEW_');
  updAction.Commands[arInsert].Execute;
end;

procedure TActionManDataModule.DeleteAMAction(id: Integer);
begin
  updAction.Commands[arDelete].ParamByName('OLD_IDENT').AsInteger := id;
  updAction.Commands[arDelete].Execute;
end;

{
aus ModifySQL von Update entfernt
SELECT IDENT, ACTION_NAME, ACTION_CALL, ACTION_TYPE, ACTION_CATEGORY,
  ACTION_CREATED, ACTION_LASTCALL, ACTION_CALLCNT
FROM ACTIONMANAGER_ACTION
WHERE IDENT = :NEW_IDENT

}
end.
