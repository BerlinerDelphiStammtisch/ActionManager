unit Obj_ActionManager;

interface

uses System.SysUtils, Classes, Winapi.Windows, DB,
     DBStrukturen, Collection_BasicObjects,
     GeneralDB_FDC, Obj_Action,
     MVCFramework.RESTClient,
     FireDAC.Comp.Client;

type
    TActionManager = class(TObject)
      private
        FDMemTable1: TFDMemTable;
        FRESTClient: TRESTClient;
        FLoading: Boolean;
        FModusLocalDB : boolean;
        FModusREST : boolean;
        FHasOptRecord : boolean;
        FOptRecordIdent : integer;
        procedure FDMemTable1BeforePost(DataSet: TDataSet);
        procedure FDMemTable1BeforeDelete(DataSet: TDataSet);
        function int_MemTableFindAction(AAction: TAM_Action): boolean;
      public
        ActionManagerDB : TGeneralDB;
        Strukturen : TDB_Strukturen;
        Actions : TCollection_BasicObjects;
        ActOptions : TCollection_BasicObjects;
        ActCategories : TStringList;

        constructor Create;
        destructor Destroy; override;
        procedure CheckStruktur;
        procedure ReadActions(ASortField: string);
        procedure ReadActionsREST(AFDMemTable: TFDMemTable);
        procedure ReadActOptions(ASortField: string);
        function GetLastCategory: string;
        function GetLastName: string;
        procedure SaveOptions(ACategory, AName: string);
        function  Action_CreateNew(AName,ACategory,ACall,AType : string): TAM_Action;
        procedure Action_InsertNew(AAction: TAM_Action);
        procedure Action_CallExecuted(AAction: TAM_Action);
        procedure Action_SaveAction(AAction: TAM_Action);
        procedure Action_DeleteAction(AAction: TAM_Action);

        function CheckFileExists(AFileName: string): boolean;
        procedure StartExecuteFile(AFileName: string; AParameter:string='');
        procedure StartExplorer(APath: string);


        property ModusLocalDB: boolean read FModusLocalDB write FModusLocalDB;
        property ModusREST : boolean read FModusREST write FModusREST;
        property ActionTable : TFDMemTable read FDMemTable1;
        property OptRecordIdent : integer read FOptRecordIdent;
        property RESTClient: TRESTClient read FRESTClient;
    end;

const C_OptRecIndex = -1;

implementation

uses Execute_Extended, Obj_Property, Obj_ActionOption, MVCFramework.DataSet.Utils;

constructor TActionManager.Create;
begin
  {Daten haben einen Eintrag für Optionen}
  FHasOptRecord := false;
  {Objekte erstellen}
  ActionManagerDB:=TGeneralDB.Create;
  Strukturen:=TDB_Strukturen.Create;
  Actions:=TCollection_BasicObjects.Create(CAM_Actions);
  ActOptions:=TCollection_BasicObjects.Create(CAM_ActOptions);
  Register_Action(Strukturen);
  Register_ActOption(Strukturen);
  ActCategories:=TStringList.Create;
  ActCategories.Sorted:=true;

  FDMemTable1 := TFDMemTable.Create(nil);
  FDMemTable1.BeforePost := FDMemTable1BeforePost;
  FDMemTable1.BeforeDelete := FDMemTable1BeforeDelete;
  with FDMemTable1.FieldDefs do
  begin
    with AddFieldDef do
    begin
      Name := 'ident';
      DataType := ftInteger;
    end;
    with AddFieldDef do
    begin
      Name := 'action_name';
      DataType := ftString;
      Size := 254;
    end;
    with AddFieldDef do
    begin
      Name := 'action_call';
      DataType := ftString;
      Size := 254;
    end;
    with AddFieldDef do
    begin
      Name := 'action_type';
      DataType := ftString;
      Size := 50;
    end;
    with AddFieldDef do
    begin
      Name := 'action_category';
      DataType := ftString;
      Size := 254;
    end;
    with AddFieldDef do
    begin
      Name := 'action_created';
      DataType := ftDateTime;
    end;
    with AddFieldDef do
    begin
      Name := 'action_lastcall';
      DataType := ftDateTime;
    end;
    with AddFieldDef do
    begin
      Name := 'action_callcnt';
      DataType := ftInteger;
    end;
  end;
  FRESTClient := TRESTClient.Create('localhost', 54711);
end;

destructor TActionManager.Destroy;
begin
  FreeAndNil(ActionManagerDB);
  FreeAndNil(Strukturen);
  FreeAndNil(Actions);
  FreeAndNil(ActOptions);
  FreeAndNil(ActCategories);

  FreeAndNil(FDMemTable1);
  FreeAndNil(FRESTClient);
end;

procedure TActionManager.FDMemTable1BeforePost(DataSet: TDataSet);
var
  Resp: IRESTResponse;
begin
  if FLoading then
    Exit;

  case FDMemTable1.State of
    dsEdit:
      Resp := RESTClient.DataSetUpdate('/actions', FDMemTable1, FDMemTable1.Fields[0].AsString);
    dsInsert:
      Resp := RESTClient.DataSetInsert('/actions', FDMemTable1);
  end;
  if not Resp.ResponseCode in [200, 201] then
    raise Exception.Create(Resp.ResponseText);
end;

procedure TActionManager.FDMemTable1BeforeDelete(DataSet: TDataSet);
var
  Resp: IRESTResponse;
begin
  Resp := RESTClient.DataSetDelete('/actions', FDMemTable1.Fields[0].AsString);
  if not Resp.ResponseCode in [200] then
    raise Exception.Create(Resp.ResponseText);
end;

function TActionManager.int_MemTableFindAction(AAction: TAM_Action): boolean;
begin
  FDMemTable1.First;
  while not FDMemTable1.Eof do
  begin
    if FDMemTable1.Fields[0].AsInteger = AAction.Ident then
      Exit(true);
    FDMemTable1.Next
  end;
  Result := false;
end;

procedure TActionManager.CheckStruktur;
begin
  ActionManagerDB.TABLE_PrepareTable(CAM_Actions,Strukturen.GetTabStr(CAM_Actions));
  ActionManagerDB.TABLE_PrepareTable(CAM_ActOptions,Strukturen.GetTabStr(CAM_ActOptions));
end;

procedure TActionManager.ReadActOptions(ASortField: string);
var ActOptionObj : TAM_ActOption;
begin
  ActOptionObj:=TAM_ActOption.Create(CAM_Actions,Strukturen.GetTabStr(CAM_ActOptions).Structure);
  try
    ActionManagerDB.QUERY_LoadCollectionFromTab(CAM_ActOptions,'*','',ASortField,ActOptionObj,ActOptions);
  finally
    FreeAndNil(ActOptionObj);
  end;
  if ActOptions.Count=0 then
  begin
    ActOptionObj:=TAM_ActOption.Create(CAM_ActOptions,Strukturen.GetTabStr(CAM_ActOptions).Structure);
    ActOptions.Insert(ActOptionObj);
    ActionManagerDB.QUERY_InsertObject(CAM_ActOptions,ActOptionObj);
  end;

end;

procedure TActionManager.ReadActions(ASortField: string);
var ActionObj : TAM_Action;
    Response: IRESTResponse;
    I : integer;
    ActOptionObj : TAM_ActOption;
begin

  if ModusLocalDB then
  begin
    ActionManagerDB.TABLE_PrepareTable(CAM_Actions,Strukturen.GetTabStr(CAM_Actions));

    ActionObj:=TAM_Action.Create(CAM_Actions,Strukturen.GetTabStr(CAM_Actions).Structure);
    try
      ActionManagerDB.QUERY_LoadCollectionFromTab(CAM_Actions,'*','',ASortField,ActionObj,Actions);
    finally
      FreeAndNil(ActionObj);
    end;

    {Alle geladenen Objekte durchlaufen
      - Liste der verwendeten Kategorien erstellen
      - Check, ob es einen 0-Datensatz gibt
    }
    for I:=0 to Actions.Count-1 do
    begin
      ActionObj:=Actions.At(I);
      {Datensatz mit letzten Sucheinstellungen?, Ident lässt sich nicht fixieren,
       deshalb Erkennung über einen symbolischen Eintrag in ActionCallCnt}
      if  (ActionObj.ActionCallCnt = C_OptRecIndex) then
      begin
        if not FHasOptRecord then
          FHasOptRecord := true;
        ActOptionObj:=TAM_ActOption.Create(CAM_Actions,Strukturen.GetTabStr(CAM_ActOptions).Structure);
        ActOptions.Insert(ActOptionObj);
        ActOptionObj.Ident := ActionObj.Ident;
        ActOptionObj.ActionName := ActionObj.ActionName;
        ActOptionObj.ActionCategory := ActionObj.ActionCategory;
        {Ident merken -> nicht anzeigen}
        FOptRecordIdent := ActionObj.Ident;
      end;
      {Kategorien aufsammeln}
      if ActCategories.IndexOf(ActionObj.ActionCategory)=-1 then
        ActCategories.Add(ActionObj.ActionCategory);
    end;
  end;

  if ModusREST then
  begin
    FLoading := true;

    {Die Memtable ist schon strukturell designed und hat die Tabellenstruktur wie
     die Datenbank. Das gelieferte JSON geht da perfekt rein.
     Noch besser wäre die Objekte gleich aus dem JSON zu erstellen.}
    Response := FRESTClient.doGET('/actions', []);
    FDMemTable1.Close;
    FDMemTable1.Open;
    FDMemTable1.AppendFromJSONArrayString(Response.BodyAsString);
    ReadActionsREST(FDMemTable1);

    {Alle geladenen Objekte durchlaufen
      - Liste der verwendeten Kategorien erstellen
      - Check, ob es einen 0-Datensatz gibt
    }
    for I:=0 to Actions.Count-1 do
    begin
      ActionObj:=Actions.At(I);
      {Datensatz mit letzten Sucheinstellungen?}
      if  (ActionObj.ActionCallCnt = C_OptRecIndex) then
      begin
        if not FHasOptRecord then
          FHasOptRecord := true;
        ActOptionObj:=TAM_ActOption.Create(CAM_Actions,Strukturen.GetTabStr(CAM_ActOptions).Structure);
        ActOptions.Insert(ActOptionObj);
        ActOptionObj.Ident := ActionObj.Ident;
        ActOptionObj.ActionName := ActionObj.ActionName;
        ActOptionObj.ActionCategory := ActionObj.ActionCategory;
        {Ident merken -> nicht anzeigen}
        FOptRecordIdent := ActionObj.Ident;
      end;
      {Kategorien}
      if ActCategories.IndexOf(ActionObj.ActionCategory)=-1 then
        ActCategories.Add(ActionObj.ActionCategory);
    end;

    FLoading := false;
  end;
end;

procedure TActionManager.ReadActionsREST(AFDMemTable: TFDMemTable);
var ActionObj : TAM_Action;
    Prop : TKUO_Property;
    I : integer;
begin
  Actions.FreeAll;
  AFDMemTable.First;
  while not AFDMemTable.Eof do
  begin
    ActionObj:=TAM_Action.Create(CAM_Actions,Strukturen.GetTabStr(CAM_Actions).Structure);
    for I := 0 to ActionObj.Properties.Count-1 do
    begin
      Prop := ActionObj.Properties.At(I);
      if Prop.IsInteger then
        ActionObj.SetValue(Prop.Desc,AFDMemTable.FindField(Prop.Desc).AsInteger);
      if Prop.IsString then
        ActionObj.SetValue(Prop.Desc,AFDMemTable.FindField(Prop.Desc).AsString);
      if Prop.IsTimeStamp then
        ActionObj.SetValue(Prop.Desc,AFDMemTable.FindField(Prop.Desc).AsDateTime);
    end;
    Actions.Insert(ActionObj);
    AFDMemTable.Next;
  end;
end;

function TActionManager.GetLastCategory: string;
var ActOptionObj : TAM_ActOption;
begin
  Result:=EmptyStr;
  if ActOptions.Count>0 then
  begin
    ActOptionObj:=ActOptions.At(0);
    Result:=ActOptionObj.ActionCategory;
  end;
end;

function TActionManager.GetLastName: string;
var ActOptionObj : TAM_ActOption;
begin
  Result:=EmptyStr;
  if ActOptions.Count>0 then
  begin
    ActOptionObj:=ActOptions.At(0);
    Result:=ActOptionObj.ActionName;
  end;
end;

procedure TActionManager.SaveOptions(ACategory, AName: string);
var ActOptionObj : TAM_ActOption;
    ActionObj : TAM_Action;
begin
  //if ActOptions.Count>0 then
  begin
    {
    ActOptionObj:=ActOptions.At(0);
    ActOptionObj.ActionCategory:=ACategory;
    ActOptionObj.ActionName:=AName;
    ActionManagerDB.QUERY_SaveObject(CAM_ActOptions,ActOptionObj);
    }
    if ModusLocalDB then
    begin
      if FHasOptRecord then
      begin
        ActionObj := Actions.FindByPropertyValue(CAM_Fldident,'0');
        if ActionObj <> nil  then
        begin
          ActionObj.ActionName := AName;
          ActionObj.ActionCategory := ACategory;
          ActionManagerDB.QUERY_SaveObject(CAM_Actions,ActionObj);
        end;
      end else begin
        ActionObj := TAM_Action.Create(CAM_Actions,Strukturen.GetTabStr(CAM_Actions).Structure);
        ActionObj.Ident := 0;
        ActionObj.ActionName := AName;
        ActionObj.ActionCategory := ACategory;
        ActionObj.ActionCallCnt := C_OptRecIndex;
        Actions.Insert(ActionObj);
        ActionManagerDB.QUERY_InsertObject(CAM_Actions,ActionObj);
      end;
    end;

    if ModusREST then
    begin
      if FHasOptRecord then
      begin
        ActionObj := Actions.FindByPropertyValue(CAM_Fldident,FOptRecordIdent.ToString);
        int_MemTableFindAction(ActionObj);

        FDMemtable1.Edit;
        FDMemtable1.Fields[1].AsString := AName;
        FDMemtable1.Fields[4].AsString := ACategory;
        FDMemtable1.Post;
      end else begin
        ActionObj := TAM_Action.Create(CAM_Actions,Strukturen.GetTabStr(CAM_Actions).Structure);
        ActionObj.Ident := C_OptRecIndex;
        ActionObj.ActionName := AName;
        ActionObj.ActionCategory := ACategory;
        ActionObj.ActionCallCnt := C_OptRecIndex;
        Actions.Insert(ActionObj);

        FDMemTable1.Insert;
        FDMemtable1.Fields[0].AsInteger := ActionObj.Ident;
        FDMemtable1.Fields[1].AsString := ActionObj.ActionName;
        FDMemtable1.Fields[4].AsString := ActionObj.ActionCategory;
        FDMemtable1.Post;

      end;
    end;
  end;
end;

function TActionManager.Action_CreateNew(AName,ACategory,ACall,AType : string): TAM_Action;
begin
  Result := TAM_Action.Create(CAM_Actions,Strukturen.GetTabStr(CAM_Actions).Structure);
  Result.ActionName := AName;
  Result.ActionCall := ACall;
  Result.ActionCategory := ACategory;
  Result.ActionType := AType;
  Result.ActionCreated := Now;
  Result.ActionCallCnt := 0;
end;

procedure TActionManager.Action_InsertNew(AAction: TAM_Action);
begin
  if ModusLocalDB then
  begin
    ActionManagerDB.QUERY_InsertObject(CAM_Actions,AAction);
  end;

  if ModusREST then
  begin
    AAction.Ident := Actions.GetFirstFreeIdent(CAM_Fldident);
    FDMemTable1.Insert;
    FDMemtable1.Fields[1].AsString := AAction.ActionName;
    FDMemtable1.Fields[2].AsString := AAction.ActionCall;
    FDMemtable1.Fields[3].AsString := AAction.ActionType;
    FDMemtable1.Fields[4].AsString := AAction.ActionCategory;
    FDMemtable1.Fields[5].AsDateTime := AAction.ActionCreated;
    FDMemtable1.Fields[6].AsDateTime := AAction.ActionLastCall;
    FDMemtable1.Fields[7].AsInteger := AAction.ActionCallCnt;

    FDMemtable1.Post;
  end;

  Actions.Insert(AAction);
end;

procedure TActionManager.Action_CallExecuted(AAction: TAM_Action);
begin
  AAction.ActionLastCall := Now;
  AAction.ActionCallCnt := AAction.ActionCallCnt + 1;
  Action_SaveAction(AAction);
end;

procedure TActionManager.Action_SaveAction(AAction: TAM_Action);
begin
  if ModusLocalDB then
  begin
    ActionManagerDB.QUERY_SaveObject(CAM_Actions,AAction);
  end;

  if ModusREST then
  begin
    if not int_MemTableFindAction(AAction) then
      Exit;

    FDMemtable1.Edit;

    FDMemtable1.Fields[1].AsString := AAction.ActionName;
    FDMemtable1.Fields[2].AsString := AAction.ActionCall;
    FDMemtable1.Fields[3].AsString := AAction.ActionType;
    FDMemtable1.Fields[4].AsString := AAction.ActionCategory;
    FDMemtable1.Fields[5].AsDateTime := AAction.ActionCreated;
    FDMemtable1.Fields[6].AsDateTime := AAction.ActionLastCall;
    FDMemtable1.Fields[7].AsInteger := AAction.ActionCallCnt;

    FDMemtable1.Post;
  end;
end;

procedure TActionManager.Action_DeleteAction(AAction: TAM_Action);
begin
  if ModusLocalDB then
  begin
    ActionManagerDB.QUERY_DeleteObject(CAM_Actions,AAction);
  end;

  if ModusREST  then
  begin
    if not int_MemTableFindAction(AAction) then
      Exit;

    FDMemtable1.Delete;
  end;

  Actions.FreeI(AAction);

end;

function TActionManager.CheckFileExists(AFileName: string): boolean;
begin
  Result:=FileExists(AFileName);
end;

procedure TActionManager.StartExecuteFile(AFileName: string; AParameter:string='');
begin
  ExecuteFile(AFileName,AParameter,false);
end;

procedure TActionManager.StartExplorer(APath: string);
var
  Command: array [0 .. 255] of Char;
begin
  StrPLCopy(Command,'explorer.exe "'+APath+'"',255);
  if DirectoryExists(APath) then
  begin
    EXTW_ExecTask(Command,SW_NORMAL{ SHOW },false);
  end;
end;

end.
