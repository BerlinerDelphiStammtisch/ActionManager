unit GeneralDB_FDC;

interface

uses
  Windows, Messages, SysUtils, Classes, Vcl.ComCtrls,
  Data.DB,
  {}
  FireDAC.DApt,FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client,
  {}
  String_Extended, StringList_Extended,
  Obj_TableStructure, Coll_Obj,
  Obj_BasicObj, Obj_Property,
  Collection_BasicObjects;

type  TGeneralDB_DBProgressEvent = procedure(Sender: TObject; DBProgress: integer; var GlobalCancel: boolean) of object;
      TGeneralDB_DBProgressEvent2Messages = procedure(Sender: TObject; DBProgress: integer; Message1,Message2: string; var GlobalCancel: boolean) of object;

      TGeneralDB = class(TObject)
      private
        FConnection: TFDConnection;
        FQuery: TFDQuery;
        FConnectionTabListAvailable : boolean;          //Verbindung kann automat. Tabellenliste und Felder liefern
        FTableList: TStringList;                        //Liste vorhandener bzw. gefundener Tabellen
        FTableMisses : TStringList;                     //Liste nicht gefundener Tabellen
        FDetails: TFDQuery;
        FInternQuery: TFDQuery;
        FOnDBProgress : TGeneralDB_DBProgressEvent;
        FOnDBProgress2Messages : TGeneralDB_DBProgressEvent2Messages;
        function GetConnected: boolean;
        function GetConnection: string;
        procedure SetConnection(AConnection: string);
        function GetPhysicalFieldType(AFieldType: Char;  AFieldLength,AFieldDec: integer):String;
        function GetIndexName(AFldNr:integer; ATabName:string):string;
        procedure int_QUERY_SetBasicObj2Query(AQuery: TFDQuery; ABasicObj: TKUO_BasicObj; AForUpdate: boolean);
      public
        constructor Create;
        destructor Destroy; override;
        procedure DBOpen(AConnection: string);
        procedure DBClose;
        {}
        function IsOracle: boolean;
        function IsPostgreSQL: boolean;
        function IsMSAccess: boolean;
        function IsMSSQLServer: boolean;
        function IsMySQL: boolean;
        {}
        procedure TABLE_GetList(AList: TStrings);
        function  TABLE_Exist(ATabName: string): boolean;
        procedure TABLE_GetFields(ATabName: string; AList: TStrings);
        procedure TABLE_DeleteFrom(ATabName: string;const AWhere: string='');
        function  TABLE_GetLastIdent(ATabName,AIdentField: string): integer;
        procedure TABLE_InsertCSV(ATabName: string; AList: TStrings);
        function  TABLE_FieldExists(ATabName: string; AFieldName: String): Boolean;
        procedure TABLE_CreateTable(ATabName: string; ATableStructure: TDBO_TableStructure);
        procedure TABLE_DropTable(ATabName: string);
        procedure TABLE_AlterTable(ATabName: string; ATableStructure: TDBO_TableStructure);
        procedure TABLE_DropFields(ATabName:string; AFieldList: TStrings);
        procedure TABLE_PrepareTable(ATabName: string; ATableStructure: TDBO_TableStructure);
        {=== Operationen mit dem Query-Objekt =================================}
        function QUERY_ExecuteSQL(ASQLCommand: string): integer;
        {--- Allgemein                                                         }
        function  QUERY_Open(ATabName,ASelect,AWhere,AOrder: string): boolean; overload;
        function  QUERY_Open(AStatement: string): boolean; overload;
        {}
        procedure QUERY_Fetch(ABasicObj: TKUO_BasicObj);
        procedure QUERY_LoadCollection(ABasicObj: TKUO_BasicObj; ABasicObjectCollection: TColl{ection_BasicObjects});
        procedure QUERY_Close;
        {}
        procedure QUERY_LoadCollectionFromTab(ATabName,ASelect,AWhere,AOrder: string; ABasicObj: TKUO_BasicObj; ABasicObjectCollection: TColl{ection_BasicObjects});
        procedure QUERY_ExecSQL(ASQL: string);
        function  QUERY_CountContent(ATabName,AWhereField,AWhereValue: string): integer;
        procedure QUERY_SaveCollection(ATabName: string; ABasicObjectCollection: TColl{ection_BasicObjects});
        {--- Operationen mit ValueList                                         }
        procedure QUERY_InsertValueList(ATabName: string; AValueList: TStrings);
        procedure QUERY_UpdateValueList(ATabName: string; AValueList: TStringList; AWhere: string); overload;
        procedure QUERY_UpdateValueList(ATabName: string; AValues, AWhere: string); overload;
        procedure QUERY_UpdateField(ATabName,AUpdateField,AUpdateValue,AWhereField,AWhereValue: string);
        {--- Operationen mit ListView                                          }
        procedure QUERY_GetListview(AListView: TListView; ASummaryIndex: integer; var ASummary: integer);
        {--- Operationen mit BasicObject                                       }
        procedure QUERY_SaveObject(ATabName: string; ABasicObj: TKUO_BasicObj);
        procedure QUERY_InsertObject(ATabName: string; ABasicObj: TKUO_BasicObj; const AWithNewIdent:boolean=true);
        procedure QUERY_DeleteObject(ATabName: string; ABasicObj: TKUO_BasicObj);
        procedure QUERY_InsertObjectCollection(ATabName: string; ABasicObjectCollection: TCollection_BasicObjects);
        {=== Operationen mit dem Field-Objekt =================================}
        procedure FIELD_GetLast(ATabName,AIdentField,AFieldName: string; AList: TStrings);
        function  FIELD_GetValue(ATabName,AWhere,AFieldName: string): string;
        {der Inhalt eines Feldes wird in AList zurückgeliefert}
        procedure FIELD_GetList(AStatement: string; AList: TStrings); overload;
        procedure FIELD_GetList(AStatement,ASeperator: string; AList: TStrings); overload;
        {der Inhalt der Felder eines Datensatzes wird in AList feldweise zurückgeliefert}
        procedure FIELD_GetValues(AStatement: string; AList: TStrings; const AField: string=''; const ASearch: string='');
        procedure FIELD_Insert(ATabName,AFieldName,AIdentField: string; AList: TStrings);
        procedure FIELD_Update(ATabName,AFieldName,AWhere,AValue: string);

        property Connection: string read GetConnection write SetConnection;
        property Connected: boolean read GetConnected;
        property Query: TFDQuery read FQuery;
        property Details: TFDQuery read FDetails;

        property OnDBProgress : TGeneralDB_DBProgressEvent read FOnDBProgress write FOnDBProgress;
        property OnDBProgress2Messages : TGeneralDB_DBProgressEvent2Messages read FOnDBProgress2Messages write FOnDBProgress2Messages;
      end;

const C_SQL_Select     = 'SELECT ';
      C_SQL_SelAll     = 'SELECT * ';
      C_SQL_SelDist    = 'SELECT DISTINCT ';
      C_SQL_From       = ' FROM ';
      C_SQL_Where      = ' WHERE ';
      C_SQL_Order      = ' ORDER BY ';
      C_SQL_DESC       = ' DESC';
      C_SQL_Insert     = 'INSERT INTO ';
      C_SQL_Values     = 'VALUES ';
      C_SQL_Delete     = 'DELETE FROM ';
      C_SQL_All        = ' * ';
      C_SQL_And        = ' AND ';
      C_SQL_Or         = ' OR ';
      C_SQL_Like       = ' LIKE ';
      C_SQL_DropTab    = 'DROP TABLE ';
      C_SQL_IsNotNULL  = ' IS NOT NULL';
      C_SQL_Update     = 'UPDATE ';
      C_SQL_Set        = ' SET ';
      C_SQL_Equal      = '=';
      C_SQLU_UnEqual   = '<>';
      C_SQL_CreateTable = 'CREATE TABLE ';
      C_SQL_AlterTable  = 'ALTER TABLE ';

      C_SQL_StatSelect = 'SELECT %s FROM %s';
      C_SQL_StatSelectOrder = 'SELECT %s FROM %s ORDER BY %s';
      C_SQL_EmptyTab = C_SQL_StatSelect + ' WHERE 1=2';

      CConnectMDB = 'DriverID=MSAcc;Database=%s';

      {DDL-Felder}
      C_DDL_SMALLINT     = 'SMALLINT';
      C_DDL_INTEGER      = 'INTEGER';
      C_DDL_FLOAT        = 'FLOAT';
      C_DDL_VARCHAR      = 'VARCHAR';
      C_DDL_BLOB         = 'IMAGE';
      C_DDL_DATE         = 'DATE';
      C_DDL_DATETIME     = 'DATETIME';
      C_DDL_TIMESTAMP    = 'TIMESTAMP';
      C_DDL_MEMO         = 'MEMO';
      C_DDL_BOOLEAN      = 'YESNO';
      C_DDL_BOOL         = 'BOOL';
      C_DDL_NUMBER       = 'NUMBER';
      C_DDL_INT          = 'INT';
      {Änderungs-DDL}
      C_DDL_ADDCOLUMN    = 'ADD COLUMN';
      C_DDL_ALTERCOLUMN  = 'ALTER COLUMN';
      C_DDL_DROPCOLUMN   = 'DROP COLUMN';

      {Driver}
      C_DRI_ORA = 'ORA';
      C_DRI_ACC = 'MSAcc';
      C_DRI_SQL = 'MSSQL';
      C_DRI_PGS = 'PG';
      C_DRI_MSQ = 'MySQL';

implementation

const C_PlaceHolder = ':?';
      C_Komma       = ',';

      C_TabFieldUpdate = 'UPDATE %s SET %s=:1 WHERE %s';

constructor TGeneralDB.Create;
begin
  FConnection:=TFDConnection.Create(nil);
  FQuery:=TFDQuery.Create(nil);
  FDetails:=TFDQuery.Create(nil);
  FInternQuery:=TFDQuery.Create(nil);
  FTableList:=TStringList.Create;
  FTableMisses:=TStringList.Create;
end;

destructor TGeneralDB.Destroy;
begin
  FreeAndNil(FDetails);
  FreeAndNil(FQuery);
  FreeAndNil(FConnection);
  FreeAndNil(FInternQuery);
  FreeAndNil(FTableList);
  FreeAndNil(FTableMisses);
end;

function TGeneralDB.GetConnected: boolean;
begin
  Result:=FConnection.Connected;
end;

function TGeneralDB.GetConnection: string;
begin
  Result:=FConnection.ConnectionString;
end;

procedure TGeneralDB.SetConnection(AConnection: string);
begin
  FConnection.ConnectionString:=AConnection;
end;

function TGeneralDB.GetPhysicalFieldType(AFieldType: Char;  AFieldLength,AFieldDec: integer):String;
begin
  if IsPostgreSQL then
  begin
    case AFieldType of
      'C' : Result:=C_DDL_VARCHAR+'('+IntToStr(AFieldLength)+')';
      'N' : if AFieldDec=0 then begin
             if AFieldLength<5 then Result:=C_DDL_SMALLINT
             else if AFieldLength<11 then Result:=C_DDL_INTEGER
             else Result:=C_DDL_FLOAT;
            end
            else begin
              Result:=C_DDL_FLOAT;
            end;
      'D' : Result:=C_DDL_DATE;
      'S' : Result:=C_DDL_TIMESTAMP;
      'B' : Result:=C_DDL_Blob;
      'M' : Result:=C_DDL_MEMO;
      'L' : Result:=C_DDL_BOOL;
    else
      Result:='UNKNOWN FIELDTYPE';
    end;
  end;

  if IsOracle then
  begin
    case AFieldType of
      'C' : Result:=C_DDL_VARCHAR+'('+IntToStr(AFieldLength)+')';
      'N' : if AFieldDec=0 then begin
             if AFieldLength<5 then Result:=C_DDL_SMALLINT
             else if AFieldLength<11 then Result:=C_DDL_INTEGER
             else Result:=C_DDL_FLOAT;
            end
            else begin
              Result:=C_DDL_FLOAT;
            end;
      'D' : Result:=C_DDL_DATE;
      'S' : Result:=C_DDL_TIMESTAMP;
      'B' : Result:=C_DDL_Blob;
      'M' : Result:=C_DDL_MEMO;
      'L' : Result:=C_DDL_NUMBER;
    else
      Result:='UNKNOWN FIELDTYPE';
    end;
  end;

  if IsMSAccess then
  begin
    case AFieldType of
      'C' : Result:=C_DDL_VARCHAR+'('+IntToStr(AFieldLength)+')';
      'N' : if AFieldDec=0 then begin
             if AFieldLength<5 then Result:=C_DDL_SMALLINT
             else if AFieldLength<11 then Result:=C_DDL_INTEGER
             else Result:=C_DDL_FLOAT;
            end
            else begin
              Result:=C_DDL_FLOAT;
            end;
      'D' : Result:=C_DDL_DATE;
      'S' : Result:=C_DDL_DATETIME;
      'B' : Result:=C_DDL_Blob;
      'M' : Result:=C_DDL_MEMO;
      'L' : Result:=C_DDL_BOOLEAN
    else
      Result:='UNKNOWN FIELDTYPE';
    end;
  end;

  if IsMSSQLServer then
  begin
    case AFieldType of
      'C' : Result:=C_DDL_VARCHAR+'('+IntToStr(AFieldLength)+')';
      'N' : if AFieldDec=0 then begin
             if AFieldLength<5 then Result:=C_DDL_SMALLINT
             else if AFieldLength<11 then Result:=C_DDL_INTEGER
             else Result:=C_DDL_FLOAT;
            end
            else begin
              Result:=C_DDL_FLOAT;
            end;
      'D' : Result:=C_DDL_DATE;
      'S' : Result:=C_DDL_DATETIME;
      'B' : Result:=C_DDL_Blob;
      'M' : Result:=C_DDL_MEMO;
      'L' : Result:=C_DDL_INT
    else
      Result:='UNKNOWN FIELDTYPE';
    end;
  end;

  if IsMySQL then
  begin
    case AFieldType of
      'C' : Result:=C_DDL_VARCHAR+'('+IntToStr(AFieldLength)+')';
      'N' : if AFieldDec=0 then begin
             if AFieldLength<5 then Result:=C_DDL_SMALLINT
             else if AFieldLength<11 then Result:=C_DDL_INTEGER
             else Result:=C_DDL_FLOAT;
            end
            else begin
              Result:=C_DDL_FLOAT;
            end;
      'D' : Result:=C_DDL_DATE;
      'S' : Result:=C_DDL_DATETIME;
      'B' : Result:=C_DDL_Blob;
      'M' : Result:=C_DDL_MEMO;
      'L' : Result:=C_DDL_INT;
    else
      Result:='UNKNOWN FIELDTYPE';
    end;
  end;
end;

function TGeneralDB.GetIndexName(AFldNr:integer; ATabName:string):string;
var Name : string;
{für Feld 0 -> TABNAME_I1}
begin
  if ATabName.Contains('.') then Name := ES_AfterChar(ATabName,'.')
                            else Name := ATabName;

  Result:=Name+'_I'+IntToStr(AFldNr+1);
end;

procedure TGeneralDB.int_QUERY_SetBasicObj2Query(AQuery: TFDQuery; ABasicObj: TKUO_BasicObj; AForUpdate: boolean);
var I,IdentCount : integer;
    P : TKUO_Property;
begin
  IdentCount := 0;
  for I:=0 to ABasicObj.Properties.Count-1 do
  begin
    P:=ABasicObj.Properties.At(I);

    if AForUpdate and P.IsIdentField then
    begin
      Inc(IdentCount);
      Continue;
    end;

    if P.IsString then
    begin
      if P.IsMemo then AQuery.Params[I-IdentCount].AsMemo:=P.ValueVariant
                  else AQuery.Params[I-IdentCount].AsString:=P.ValueVariant;
    end;
    if P.IsInteger then
      AQuery.Params[I-IdentCount].AsInteger:=P.ValueVariant;
    if P.IsReal then
      AQuery.Params[I-IdentCount].AsExtended:=P.ValueVariant;
    if P.IsBoolean then
      AQuery.Params[I-IdentCount].AsBoolean:=P.ValueVariant;
    if P.IsTimeStamp then
      AQuery.Params[I-IdentCount].AsDateTime:=P.ValueVariant;
    if P.IsDate then
      AQuery.Params[I-IdentCount].AsDate:=P.ValueVariant;
  end;
end;
{------------------------------------------------------------------------------}
procedure TGeneralDB.DBOpen(AConnection: string);
var I : integer;
begin
  FConnection.Connected:=false;
  FConnection.ConnectionString:=AConnection;
  FConnection.Open;
  FQuery.Connection:=FConnection;
  FDetails.Connection:=FConnection;
  FInternQuery.Connection:=FConnection;
  {FConnectionTabListAvailable trifft zu bei
    - MS Access
    - Oracle
    - PostgreSQL
    - MS SQL Server
    - MySQL
  }
  FConnectionTabListAvailable:=false;
  try
    TABLE_GetList(FTableList);
    for I:=0 to FTableList.Count-1 do FTableList[I]:=UpperCase(FTableList[I]);
    FConnectionTabListAvailable:=true;
  except end;
end;

procedure TGeneralDB.DBClose;
begin
  FConnection.Connected:=false;
  FConnection.Connected:=false;
  FTableList.Clear;
  FTableMisses.Clear;
end;

function TGeneralDB.IsOracle: boolean;
begin
  Result := FConnection.DriverName = C_DRI_ORA;
end;

function TGeneralDB.IsPostgreSQL: boolean;
begin
  Result := FConnection.DriverName = C_DRI_PGS;
end;

function TGeneralDB.IsMSAccess: boolean;
begin
  Result := FConnection.DriverName = C_DRI_ACC;
end;

function TGeneralDB.IsMSSQLServer: boolean;
begin
  Result := FConnection.DriverName = C_DRI_SQL;
end;

function TGeneralDB.IsMySQL: boolean;
begin
  Result := FConnection.DriverName = C_DRI_MSQ;
end;

procedure TGeneralDB.TABLE_GetList(AList: TStrings);
begin
  {durch die Verwendung von CurrentCatalog in GetTableName werden die Namen
    im Format <schema>.<tabelle> angegeben. Ohne diese Einstellung wird bei
    SQL Server <catalog>.<schema>.<tabelle> zurückgegeben.
  }
  AList.Clear;
  FConnection.GetTableNames(FConnection.CurrentCatalog,'','',AList);
end;

function TGeneralDB.TABLE_Exist(ATabName: string): boolean;
begin
  if FConnectionTabListAvailable then
    Result:=FTableList.IndexOf(UpperCase(ATabName))>-1
  else begin
    Result:=FTableList.IndexOf(UpperCase(ATabName))>-1;
    if (not Result) and (FTableMisses.IndexOf(UpperCase(ATabName))=-1) then
    begin
      try
        FInternQuery.SQL.Clear;
        FInternQuery.SQL.Add(Format(C_SQL_EmptyTab,[C_SQL_All,ATabName]));
        FInternQuery.Open;
        Result:=true;
        FInternQuery.Close;
      except
        FTableMisses.Add(UpperCase(ATabName));//registrieren, dass nicht vorhanden ist
      end;
    end;
  end;
end;

procedure TGeneralDB.TABLE_GetFields(ATabName: string; AList: TStrings);
begin
  AList.Clear;
  if TABLE_Exist(ATabName) then
    FConnection.GetFieldNames('','',ATabName,'',AList);
end;

procedure TGeneralDB.TABLE_DeleteFrom(ATabName: string;const AWhere: string='');
begin
  FQuery.SQL.Text:=C_SQL_Delete+ATabName;
  if AWhere<>'' then FQuery.SQL.Add(C_SQL_Where+AWhere);
  FQuery.ExecSQL;
end;

function TGeneralDB.TABLE_GetLastIdent(ATabName,AIdentField: string): integer;
begin
  Result:=0;
  FInternQuery.SQL.Clear;
  FInternQuery.SQL.Add(C_SQL_Select+AIdentField);
  FInternQuery.SQL.Add(C_SQL_From+ATabName);
  FInternQuery.SQL.Add(C_SQL_Order+AIdentField+C_SQL_DESC);
  FInternQuery.Open;
  if FInternQuery.RecordCount=0 then Exit;
  Result:=FInternQuery.FindField(AIdentField).AsInteger;
  FInternQuery.Close;
end;

procedure TGeneralDB.TABLE_InsertCSV(ATabName: string; AList: TStrings);
var Fields : string;
    List   : TStringList;
    I,N : integer;
begin
  List:=TStringList.Create;
  try
    {die erste Zeile enthält die Feldnamen}
    ES_GetListFromString(AList[0],';',List);
    if List.Count=0 then Exit;

    {Insertbefehl beginnen}
    FQuery.SQL.Clear;
    FQuery.SQL.Add(C_SQL_Insert+ATabName);
    {aus der ersten Zeile die Liste der Datenbankfelder aufbauen, die zu bedienen sind}
    for I:=0 to List.Count-1 do if I=0 then Fields:=List[I]
                                       else Fields:=Fields+C_Komma+List[I];
    FQuery.SQL.Add('('+Fields+')');

    for I:=0 to List.Count-1 do if I=0 then Fields:=':P'+IntToStr(I)
                                       else Fields:=Fields+C_Komma+':P'+IntToStr(I);
    FQuery.SQL.Add('VALUES ('+Fields+')');

    for N:=1 to AList.Count-1 do
    begin
      ES_GetListFromString(AList[N],';',List);
      for I:=0 to List.Count-1 do
      begin
        FQuery.Params[I].Value:=List[I];
      end;
      FQuery.ExecSQL;
    end;
  finally
    FreeAndNil(List);
  end;
end;

function TGeneralDB.TABLE_FieldExists(ATabName: string; AFieldName: String): Boolean;
var SL: TStringList;
    i: Integer;
begin
  Result:= false;
  SL:= TStringList.Create;
  try
    if TABLE_Exist(ATabName) then
      FConnection.GetFieldNames('','',ATabName,'',SL);
    if SL.Count> 0 then
      for i := 0 to SL.Count - 1 do
        if UpperCase(SL[i]) = UpperCase(AFieldName) then
        begin
          Result:= true;
          Exit;
        end;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TGeneralDB.TABLE_CreateTable(ATabName: string; ATableStructure: TDBO_TableStructure);
var DDLCommand : string;
    TabFieldType : string;
    TabField : TDBO_TableStructureField;
    I        : integer;
    VRows    : OleVariant;
    IdxName  : string;
begin
  DDLCommand:=C_SQL_CreateTable+ATabName+' (';
  for I := 0 to ATableStructure.Structure.Count - 1 do
  begin
    TabField:=ATableStructure.Structure.At(I);
    DDLCommand:=DDLCommand+TabField.FldName+' ';
    TabFieldType:=GetPhysicalFieldType(TabField.FldType,TabField.FldUnits1,TabField.FldUnits2);
    DDLCommand:=DDLCommand+TabFieldType;
    if TabField.HasNotNull then
      DDLCommand:=DDLCommand+' NOT NULL';
    if I<ATableStructure.Structure.Count-1 then DDLCommand:=DDLCommand+', ' else DDLCommand:=DDLCommand+')';
  end;
  {Kosch:217 - Parameter sollten definiert werden = schneller}
  try
    FConnection.ExecSQL(DDLCommand,VRows);
  except
    //on E: Exception do GDOADORaiseError(E);
    on E: Exception do raise;
  end;

  {Indizes}
  for I:=0 to ATableStructure.Structure.Count-1 do
  begin
    TabField:=ATableStructure.Structure.At(I);
    with TabField do
    begin
      if HasIndex or HasUniqueIndex then
      begin
        IdxName:=GetIndexName(I,ATabName);
        DDLCommand:='CREATE ';
        if HasUniqueIndex then DDLCommand:=DDLCommand+'UNIQUE ';
        DDLCommand:=DDLCommand+'INDEX '+IdxName+' ON '+ATabName;
        DDLCommand:=DDLCommand+' ('+FldName+')';
        {anlegen..}
        try
          FConnection.ExecSQL(DDLCommand,VRows);
        except
          //on E: Exception do GDOADORaiseError(E);
          on E: Exception do raise;
        end;
      end;
    end;
  end;
  {Tabellenliste aktualisieren}
  TABLE_GetList(FTableList);
end;

procedure TGeneralDB.TABLE_DropTable(ATabName: string);
var DDLCommand : string;
    VRows    : OleVariant;
begin
  if TABLE_Exist(ATabName) then
  begin
    DDLCommand:=C_SQL_DropTab+ATabName;
    {löschen..}
    try
      FConnection.ExecSQL(DDLCommand,VRows);
    except
      //on E: Exception do GDOADORaiseError(E);
      on E: Exception do raise;
    end;
  end;
  {Tabellenliste aktualisieren}
  TABLE_GetList(FTableList);
end;

procedure TGeneralDB.TABLE_AlterTable(ATabName: string; ATableStructure: TDBO_TableStructure);
var DDLCommand : string;
    TabFieldType : string;
    TabField : TDBO_TableStructureField;
    Fields    : TStringList;
    AddColumn : string;
    I         : integer;
begin
  AddColumn:=EmptyStr;
  Fields:=TStringList.Create;
  try
    TABLE_GetFields(ATabName,Fields);
    for I := 0 to ATableStructure.Structure.Count - 1 do
    begin
      TabField:=ATableStructure.Structure.At(I);
      if Fields.IndexOf(TabField.FldName)=-1 then
      begin
        TabFieldType:=GetPhysicalFieldType(TabField.FldType,TabField.FldUnits1,TabField.FldUnits2);
        if AddColumn=EmptyStr then AddColumn:=Format(' %s %s',[TabField.FldName,TabFieldType])
                              else AddColumn:=AddColumn+','+Format(' %s %s',[TabField.FldName,TabFieldType]);
      end;
    end;
    if AddColumn<>EmptyStr then
    begin
      DDLCommand:=C_SQL_AlterTable+ATabName+' '+C_DDL_ADDCOLUMN+AddColumn;
      FConnection.ExecSQL(DDLCommand);
    end;
  finally
    Fields.Free;
  end;

end;

procedure TGeneralDB.TABLE_DropFields(ATabName:string; AFieldList: TStrings);
var VRows      : OleVariant;
    DDLCommand : string;
    I          : integer;
begin
  try
    DDLCommand:=C_SQL_AlterTable+ATabName+' '+C_DDL_DROPCOLUMN;
    for I:=0 to AFieldList.Count-1 do
      if I=0 then DDLCommand:=DDLCommand+' '+AFieldList[I]
             else DDLCommand:=DDLCommand+','+AFieldList[I];
    FConnection.ExecSQL(DDLCommand,VRows);
  except
    //on E: Exception do GDOADORaiseError(E);
    on E: Exception do raise;
  end;
end;

procedure TGeneralDB.TABLE_PrepareTable(ATabName: string; ATableStructure: TDBO_TableStructure);
begin
  if not TABLE_Exist(ATabName) then TABLE_CreateTable(ATabName,ATableStructure)
                               else TABLE_AlterTable(ATabName,ATableStructure);
end;

function TGeneralDB.QUERY_ExecuteSQL(ASQLCommand: string): integer;
begin
  FQuery.SQL.Clear;
  FQuery.SQL.Add(ASQLCommand);
  Result:=FQuery.ExecSQL(false);
end;

function TGeneralDB.QUERY_Open(ATabName,ASelect,AWhere,AOrder: string): boolean;
begin
  FQuery.SQL.Clear;
  FQuery.SQL.Add(C_SQL_Select+ASelect);
  FQuery.SQL.Add(C_SQL_From+ATabName);
  if AWhere<>EmptyStr then FQuery.SQL.Add(C_SQL_Where+AWhere);
  if AOrder<>EmptyStr then FQuery.SQL.Add(C_SQL_Order+AOrder);
  FQuery.Open;
  Result:=FQuery.RecordCount>0;
end;

function TGeneralDB.QUERY_Open(AStatement: string): boolean;
begin
  FQuery.SQL.Clear;
  FQuery.SQL.Add(AStatement);
  FQuery.Open;
  Result:=FQuery.RecordCount>0;
end;

procedure TGeneralDB.QUERY_Fetch(ABasicObj: TKUO_BasicObj);
var Field: TField;
    P : TKUO_Property;
    I : integer;
begin
  for I:=0 to ABasicObj.Properties.Count-1 do
  begin
    P:=ABasicObj.Properties.At(I);
    {FieldByName liefert Exception, wenn das Feld nicht existiert
    Field:=FQuery.FieldByName(P.Desc);
    }
    Field:=FQuery.FindField(P.Desc);
    if Field<>nil then
    begin
      ABasicObj.Properties.SetProperty(P.Desc,Field.AsVariant);
      if P.IsNameField then
        ABasicObj.Name:=Field.AsString;
    end;
  end;
end;

procedure TGeneralDB.QUERY_LoadCollection(ABasicObj: TKUO_BasicObj; ABasicObjectCollection: TColl{ection_BasicObjects});
var B : TKUO_BasicObj;
    I,R : integer;
    Cancel : boolean;
begin
  R:=FQuery.RecordCount;
  {RecordCount enthält nur die Anzahl der initial geladenen Datensätze. Die
  Anzahl entspricht RecordSetSize.
  Alternativ kann die Query so eingestellt werden, dass sie alle Datensätze
  auf einmal liest. }
  //for I:=0 to R-1 do
  while not FQuery.Eof do
  begin
    B:=TKUO_BasicObj.CreateAssign(ABasicObj);
    QUERY_Fetch(B);
    ABasicObjectCollection.Insert(B);
    FQuery.Next;
    if Assigned(OnDBProgress) then OnDBProgress(Self,round(I/R*100),Cancel);
  end;
end;

procedure TGeneralDB.QUERY_Close;
begin
  FQuery.SQL.Clear;
  FQuery.Close;
end;

procedure TGeneralDB.QUERY_LoadCollectionFromTab(ATabName,ASelect,AWhere,AOrder: string; ABasicObj: TKUO_BasicObj; ABasicObjectCollection: TColl{ection_BasicObjects});
begin
  if QUERY_Open(ATabName,ASelect,AWhere,AOrder) then
    QUERY_LoadCollection(ABasicObj,ABasicObjectCollection);
  QUERY_Close;
end;

procedure TGeneralDB.QUERY_ExecSQL(ASQL: string);
begin
  FQuery.SQL.Clear;
  FQuery.SQL.Add(ASQL);
  FQuery.ExecSQL;
end;

function TGeneralDB.QUERY_CountContent(ATabName,AWhereField,AWhereValue: string): integer;
begin
  FQuery.SQL.Clear;
  FQuery.SQL.Add(C_SQL_SelAll);
  FQuery.SQL.Add(C_SQL_From+ATabName);
  FQuery.SQL.Add(C_SQL_Where+AWhereField+C_SQL_Equal+C_PlaceHolder);
  try
    FQuery.Params[0].Value:=AWhereValue;
    FQuery.Open;
    Result:=FQuery.RecordCount;
  finally
    FQuery.Close;
  end;
end;

procedure TGeneralDB.QUERY_SaveCollection(ATabName: string; ABasicObjectCollection: TColl{ection_BasicObjects});
var BasicObj : TKUO_BasicObj;
    I : integer;
begin
  for I := 0 to ABasicObjectCollection.Count -1 do
  begin
    BasicObj := ABasicObjectCollection.At(I);
    QUERY_SaveObject(ATabName, BasicObj);
  end;
end;

{fügt nur Werte aus der Liste ein, kein Feldbezug,
   will heißen, die Felder müssen in der richtigen Reihenfolge von außen kommen
}
procedure TGeneralDB.QUERY_InsertValueList(ATabName: string; AValueList: TStrings);
var I : integer;
    S : string;
begin
  S:=EmptyStr;
  FInternQuery.SQL.Clear;
  FInternQuery.SQL.Add(C_SQL_Insert+ATabName);
  for I:=0 to AValueList.Count-1 do if S=EmptyStr then S:=C_PlaceHolder
                                                  else S:=S+C_Komma+C_PlaceHolder;
  FInternQuery.SQL.Add(C_SQL_Values+ES_InKlammern(S));

  FInternQuery.Prepared:=true;
  for I:=0 to AValueList.Count-1 do
    FInternQuery.Params[I].Value:=AValueList[I];
  FInternQuery.ExecSQL;
end;

procedure TGeneralDB.QUERY_UpdateValueList(ATabName: string; AValueList: TStringList; AWhere: string);
var I : integer;
    S : string;
begin
  S:=EmptyStr;
  FInternQuery.SQL.Clear;
  FInternQuery.SQL.Add(C_SQL_Update+ATabName+C_SQL_Set);
  for I:=0 to AValueList.Count-1 do
  begin
    if S=EmptyStr then S:=AValueList.Names[I]+C_SQL_Equal+C_PlaceHolder
                  else S:=S+C_Komma+AValueList.Names[I]+C_SQL_Equal+C_PlaceHolder;
  end;
  FInternQuery.SQL.Add(S);
  FInternQuery.SQL.Add(C_SQL_Where+AWhere);
  FInternQuery.Prepared:=true;
  for I:=0 to AValueList.Count-1 do
    FInternQuery.Params[I].Value:=AValueList.ValueFromIndex[I];
  FInternQuery.ExecSQL;
end;

procedure TGeneralDB.QUERY_UpdateValueList(ATabName: string; AValues, AWhere: string);
var ValueList : TStringList;
begin
  ValueList:=TStringList.Create;
  try
    ValueList.Text:=AValues;
    QUERY_UpdateValueList(ATabName,ValueList,AWhere);
  finally
    ValueList.Free;
  end;
end;

procedure TGeneralDB.QUERY_UpdateField(ATabName,AUpdateField,AUpdateValue,AWhereField,AWhereValue: string);
begin
  FInternQuery.SQL.Clear;
  FInternQuery.SQL.Add(C_SQL_Update+ATabName+C_SQL_Set);
  FInternQuery.SQL.Add(AUpdateField+C_SQL_Equal+C_PlaceHolder);
  FInternQuery.SQL.Add(C_SQL_Where+AWhereField+C_SQL_Equal+C_PlaceHolder);
  FInternQuery.Params[0].Value:=AUpdateValue;
  FInternQuery.Params[1].Value:=AWhereValue;
  FInternQuery.ExecSQL;
end;

procedure TGeneralDB.QUERY_GetListview(AListView: TListView; ASummaryIndex: integer; var ASummary: integer);
var LI : TListItem;
    I,N : integer;
    F : TField;
    Cancel : boolean;
begin
  ASummary:=0; Cancel:=false;
  AListView.Items.BeginUpdate;
  AListView.Items.Clear;
  for I:=0 to Query.RecordCount-1 do
  begin
    LI:=AListView.Items.Add;
    for N:=0 to AListView.Columns.Count-1 do
    begin
      try
        if AListView.Columns[N].Caption <> '' then
        begin
          F:=Query.FieldByName(AListView.Columns[N].Caption);
          if F<>nil then
            if N=0 then LI.Caption:=F.AsString
                   else LI.SubItems.Add(F.AsString);
            if (N=ASummaryIndex) and (F.DataType in [ftSmallint,ftInteger]) then
              ASummary:=ASummary+F.AsInteger;
        end;
      except end;
    end;
    {Anzeigeelemente aktualisieren}
    if Assigned(OnDBProgress) then OnDBProgress(Self,round(I/Query.RecordCount*100),Cancel);
    {nächster Datensatz}
    Query.Next;
  end;
  AListView.Items.EndUpdate;
end;

procedure TGeneralDB.QUERY_SaveObject(ATabName: string; ABasicObj: TKUO_BasicObj);
var Where : string;
    I:integer;
begin
  Where:=ABasicObj.SQLWhere;
  if Where=EmptyStr then
  begin
    //todo Fehlermeldung
    Exit;
  end;
  FInternQuery.SQL.Clear;
  FInternQuery.SQL.Add(C_SQL_Update+ATabName+C_SQL_Set);
  FInternQuery.SQL.Add(ABasicObj.GetSQLSet(true));
  FInternQuery.SQL.Add(C_SQL_Where+Where);

  {Die ADO-Methode funktioniert hier nicht mehr. Prepare schlägt fehl.
   Prepare steigt gleich aus, da es die Datentypen der Params nicht kennt.
   Die Zuweisung als ValueVariant scheitern bei Memo, Param ist standardmäßig
   vom Typ ftString und kann nur 512 Zeichen (kommt Meldung).
   Deshalb legen wir den DataType von Param mittels Zuweiseung .As... fest.

   FInternQuery.Prepare;
   for I:=0 to ABasicObj.Properties.Count-1 do
   begin
     FInternQuery.Params[I].Value:=TKUO_Property(ABasicObj.Properties.At(I)).ValueVariant;
   end;
   {}
  int_QUERY_SetBasicObj2Query(FInternQuery,ABasicObj,true);

  FInternQuery.ExecSQL;
end;

procedure TGeneralDB.QUERY_InsertObject(ATabName: string; ABasicObj: TKUO_BasicObj; const AWithNewIdent:boolean=true);
var I : integer;
    IdentField : string;
    Ident : integer;
begin
  IdentField:=ABasicObj.IdentField;
  if IdentField=EmptyStr then
  begin
    //todo Fehlermeldung
    Exit;
  end;

  if AWithNewIdent then
  begin
    Ident:=TABLE_GetLastIdent(ATabName,IdentField)+1;
    ABasicObj.SetPropertyByName(IdentField,IntToStr(Ident));
  end;

  FInternQuery.SQL.Clear;
  FInternQuery.SQL.Add(C_SQL_Insert+ATabName);
  FInternQuery.SQL.Add(ES_InKlammern(ABasicObj.SQLFields));
  FInternQuery.SQL.Add(C_SQL_Values);
  FInternQuery.SQL.Add(ES_InKlammern(ABasicObj.SQLPlaceHolders));
  {}
  {
  FInternQuery.Prepared:=true;
  for I:=0 to ABasicObj.Properties.Count-1 do
    FInternQuery.Params[I].Value:=ABasicObj.Properties.Values[I];
  }
  int_QUERY_SetBasicObj2Query(FInternQuery,ABasicObj,false);

  FInternQuery.ExecSQL;
end;

procedure TGeneralDB.QUERY_DeleteObject(ATabName: string; ABasicObj: TKUO_BasicObj);
var Where : string;
begin
  Where:=ABasicObj.SQLWhere;
  if Where=EmptyStr then
  begin
    //todo Fehlermeldung
    Exit;
  end;
  FInternQuery.SQL.Clear;
  FInternQuery.SQL.Add(C_SQL_Delete+ATabName);
  FInternQuery.SQL.Add(C_SQL_Where+Where);
  {}
  FInternQuery.ExecSQL;
end;

procedure TGeneralDB.QUERY_InsertObjectCollection(ATabName: string; ABasicObjectCollection: TCollection_BasicObjects);
var BasicObj : TKUO_BasicObj;
    I : integer;
begin
  for I := 0 to ABasicObjectCollection.Count-1 do
  begin
    BasicObj:=ABasicObjectCollection.At(I);
    QUERY_InsertObject(ATabName,BasicObj);
  end;
end;

procedure TGeneralDB.FIELD_GetLast(ATabName,AIdentField,AFieldName: string; AList: TStrings);
var Ident : integer;
begin
  AList.Clear;
  Ident:=TABLE_GetLastIdent(ATabName,AIdentField);
  FQuery.SQL.Clear;
  FQuery.SQL.Add(C_SQL_Select+AFieldName);
  FQuery.SQL.Add(C_SQL_From+ATabName);
  FQuery.SQL.Add(C_SQL_Where+AIdentField+'='+IntToStr(Ident));
  FQuery.Open;
  if FQuery.RecordCount=0 then Exit;
  AList.Text:=FQuery.FindField(AFieldName).AsString;
end;

function TGeneralDB.FIELD_GetValue(ATabName,AWhere,AFieldName: string): string;
begin
  Result := EmptyStr;
  FQuery.SQL.Clear;
  FQuery.SQL.Add(C_SQL_Select + AFieldName);
  FQuery.SQL.Add(C_SQL_From + ATabName);
  FQuery.SQL.Add(C_SQL_Where + AWhere);
  FQuery.Open;
  if (FQuery.RecordCount>0) then
    Result := FQuery.FieldByName(AFieldName).AsString;
  FQuery.Close;
end;

procedure TGeneralDB.FIELD_GetList(AStatement: string; AList: TStrings);
begin
  AList.Clear;
  FQuery.SQL.Text:=AStatement;
  FQuery.Open;
  while not FQuery.Eof do
  begin
    AList.Add(FQuery.Fields[0].AsString);
    FQuery.Next;
  end;
  FQuery.Close;
end;

procedure TGeneralDB.FIELD_GetList(AStatement,ASeperator: string; AList: TStrings);
var Row : string;
		I : integer;
begin
  AList.Clear;
  FQuery.SQL.Text:=AStatement;
  FQuery.Open;
  while not FQuery.Eof do
  begin
    Row:=EmptyStr;
    for I := 0 to FQuery.Fields.Count-1 do
    begin
      if I=0 then Row:=FQuery.Fields[I].AsString
             else Row:=Row+ASeperator+FQuery.Fields[I].AsString;
    end;
    if Row.EndsWith(ASeperator) then
      Row:=Row.Remove(Row.Length-1);
    AList.Add(Row);
    FQuery.Next;
  end;
  FQuery.Close;
end;

procedure TGeneralDB.FIELD_GetValues(AStatement: string; AList: TStrings; const AField: string=''; const ASearch: string='');
var I : integer;
    S : string;
begin
  AList.Clear;
  FQuery.SQL.Text:=AStatement;
  FQuery.Open;
  if (FQuery.RecordCount>1) and (AField<>'') then
  begin
    S:=FQuery.FieldByName(AField).AsString;
    while (S<>ASearch) and not FQuery.Eof do
    begin
      FQuery.Next;
      S:=FQuery.FieldByName(AField).AsString;
    end;
  end;
  for I:=0 to FQuery.Fields.Count - 1 do
    AList.Add(FQuery.Fields[I].AsString);
  FQuery.Close;
end;

procedure TGeneralDB.FIELD_Insert(ATabName,AFieldName,AIdentField: string; AList: TStrings);
var Ident : integer;
begin
  Ident:=TABLE_GetLastIdent(ATabName,AIdentField)+1;
  FQuery.SQL.Clear;
  FQuery.SQL.Add(C_SQL_Insert+ATabName);
  {
  FQuery.SQL.Add('('+C_FldIdent+C_Komma+C_FldListDrive+C_Komma+C_FldListData+C_Komma+C_FldListDate+')');
  FQuery.SQL.Add('VALUES ('+IntToStr(Ident)+C_Komma+ES_StrInHochkomma(ADrive)+C_Komma+ES_StrInHochkomma(AList.Text)+C_Komma+':P01Date'+')');
  FQuery.Parameters[0].Value:=Now;
  FQuery.ExecSQL;
  }
end;

procedure TGeneralDB.FIELD_Update(ATabName,AFieldName,AWhere,AValue: string);
begin
  FQuery.SQL.Text:=Format(C_TabFieldUpdate,[ATabName,AFieldName,AWhere]);
  FQuery.Params[0].Value:=AValue;
  FQuery.ExecSQL;
end;

end.
