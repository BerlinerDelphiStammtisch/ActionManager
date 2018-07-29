unit Obj_ActionManager;

interface

uses System.SysUtils, Classes, Winapi.Windows,
     DBStrukturen, Collection_BasicObjects,
     GeneralDB_FDC, Obj_Action,
     FireDAC.Comp.Client;

type
    TActionManager = class(TObject)
      private
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

        function CheckFileExists(AFileName: string): boolean;
        procedure StartExecuteFile(AFileName: string; AParameter:string='');
        procedure StartExplorer(APath: string);
    end;

implementation

uses Execute_Extended, Obj_Property, Obj_ActionOption;

constructor TActionManager.Create;
begin
  ActionManagerDB:=TGeneralDB.Create;
  Strukturen:=TDB_Strukturen.Create;
  Actions:=TCollection_BasicObjects.Create(CAM_Actions);
  ActOptions:=TCollection_BasicObjects.Create(CAM_ActOptions);
  Register_Action(Strukturen);
  Register_ActOption(Strukturen);
  ActCategories:=TStringList.Create;
  ActCategories.Sorted:=true;
end;

destructor TActionManager.Destroy;
begin
  FreeAndNil(ActionManagerDB);
  FreeAndNil(Strukturen);
  FreeAndNil(Actions);
  FreeAndNil(ActOptions);
  FreeAndNil(ActCategories);
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
    I : integer;
begin

  ActionManagerDB.TABLE_PrepareTable(CAM_Actions,Strukturen.GetTabStr(CAM_Actions));

  ActionObj:=TAM_Action.Create(CAM_Actions,Strukturen.GetTabStr(CAM_Actions).Structure);
  try
    ActionManagerDB.QUERY_LoadCollectionFromTab(CAM_Actions,'*','',ASortField,ActionObj,Actions);
  finally
    FreeAndNil(ActionObj);
  end;

  for I:=0 to Actions.Count-1 do
  begin
    ActionObj:=Actions.At(I);
    if ActCategories.IndexOf(ActionObj.ActionCategory)=-1 then
      ActCategories.Add(ActionObj.ActionCategory);
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
begin
  if ActOptions.Count>0 then
  begin
    ActOptionObj:=ActOptions.At(0);
    ActOptionObj.ActionCategory:=ACategory;
    ActOptionObj.ActionName:=AName;
    ActionManagerDB.QUERY_SaveObject(CAM_ActOptions,ActOptionObj);
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
  ActionManagerDB.QUERY_InsertObject(CAM_Actions,AAction);
  Actions.Insert(AAction);
end;

procedure TActionManager.Action_CallExecuted(AAction: TAM_Action);
begin
  AAction.ActionLastCall := Now;
  AAction.ActionCallCnt := AAction.ActionCallCnt + 1;
  ActionManagerDB.QUERY_SaveObject(CAM_Actions,AAction);
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
