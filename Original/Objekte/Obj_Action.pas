unit Obj_Action;

interface

uses Obj_BasicObj, DBStrukturen;

type
     TAM_Action = class(TKUO_BasicObj)
       private
         function GetIdent: integer;
         procedure SetIdent(AValue: integer);
         function GetActionName: string;
         procedure SetActionName(AValue: string);
         function GetActionCall: string;
         procedure SetActionCall(AValue: string);
         function GetActionType: string;
         procedure SetActionType(AValue: string);
         function GetActionCategory: string;
         procedure SetActionCategory(AValue: string);
         function GetActionCreated: TDateTime;
         procedure SetActionCreated(AValue: TDateTime);
         function GetActionLastCall: TDateTime;
         procedure SetActionLastCall(AValue: TDateTime);
         function GetActionCallCnt: integer;
         procedure SetActionCallCnt(AValue: integer);
       public
         property Ident: integer read GetIdent write SetIdent;
         property ActionName: string read GetActionName write SetActionName;
         property ActionCall: string read GetActionCall write SetActionCall;
         property ActionType: string read GetActionType write SetActionType;
         property ActionCategory: string read GetActionCategory write SetActionCategory;
         property ActionCreated: TDateTime read GetActionCreated write SetActionCreated;
         property ActionLastCall: TDateTime read GetActionLastCall write SetActionLastCall;
         property ActionCallCnt: integer read GetActionCallCnt write SetActionCallCnt;
     end;

const CAM_Actions = 'actionmanager_Action';

procedure Register_Action(AStrukturen: TDB_Strukturen);
procedure UnRegister_Action(AStrukturen: TDB_Strukturen);

const CAM_Fldident       = 'ident';
      CAM_FldActName     = 'action_name';
      CAM_FldActCall     = 'action_call';
      CAM_FldActType     = 'action_type';
      CAM_FldActCategory = 'action_category';
      CAM_FldActCreated  = 'action_created';
      CAM_FldActLastCall = 'action_lastcall';
      CAM_FldActCallCnt  = 'action_callcnt';

implementation

uses String_Extended, Obj_TableStructure;

{$Region '------------------------------------ Register ----------------------------------'}
procedure Register_Action(AStrukturen: TDB_Strukturen);
var TabStrs: TDBO_TableStructure;
begin
  TabStrs:=TDBO_TableStructure.Create(CAM_Actions,CAM_Actions,0,1,0,'',TDBO_TableStructureEntries.Create);
  with TabStrs.Structure do
  begin
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldIdent,'N',10,0,1));
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldActName,'C',254,0,0));
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldActCall,'C',254,0,0));
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldActType,'C',50,0,0));
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldActCategory,'C',100,0,0));
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldActCreated,'S',10,0,0));
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldActLastCall,'S',10,0,0));
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldActCallCnt,'N',10,0,0));
  end;
  AStrukturen.Insert(TabStrs);
end;

procedure UnRegister_Action(AStrukturen: TDB_Strukturen);
begin
  AStrukturen.FreeI(AStrukturen.GetTabStr(CAM_Actions));
end;
{$EndRegion}

{$Region '--------------------------------- Set/Get-Routinen -----------------------------'}
function TAM_Action.GetIdent: integer;
begin
  Result:=FProperties.GetProperty(CAM_Fldident);
end;

procedure TAM_Action.SetIdent(AValue: integer);
begin
  FProperties.SetProperty(CAM_Fldident,AValue);
end;

function TAM_Action.GetActionName: string;
begin
  Result:=FProperties.GetProperty(CAM_FldActName);
end;

procedure TAM_Action.SetActionName(AValue: string);
begin
  FProperties.SetProperty(CAM_FldActName,AValue);
end;

function TAM_Action.GetActionCall: string;
begin
  Result:=FProperties.GetProperty(CAM_FldActCall);
end;

procedure TAM_Action.SetActionCall(AValue: string);
begin
  FProperties.SetProperty(CAM_FldActCall,AValue);
end;

function TAM_Action.GetActionType: string;
begin
  Result:=FProperties.GetProperty(CAM_FldActType);
end;

procedure TAM_Action.SetActionType(AValue: string);
begin
  FProperties.SetProperty(CAM_FldActType,AValue);
end;

function TAM_Action.GetActionCategory: string;
begin
  Result:=FProperties.GetProperty(CAM_FldActCategory);
end;

procedure TAM_Action.SetActionCategory(AValue: string);
begin
  FProperties.SetProperty(CAM_FldActCategory,AValue);
end;

function TAM_Action.GetActionCreated: TDateTime;
begin
  Result:=FProperties.GetProperty(CAM_FldActCreated);
end;

procedure TAM_Action.SetActionCreated(AValue: TDateTime);
begin
  FProperties.SetProperty(CAM_FldActCreated,AValue);
end;

function TAM_Action.GetActionLastCall: TDateTime;
begin
  Result:=FProperties.GetProperty(CAM_FldActLastcall);
end;

procedure TAM_Action.SetActionLastCall(AValue: TDateTime);
begin
  FProperties.SetProperty(CAM_FldActLastcall,AValue);
end;

function TAM_Action.GetActionCallCnt: integer;
begin
  Result:=FProperties.GetProperty(CAM_FldActCallCnt);
end;

procedure TAM_Action.SetActionCallCnt(AValue: integer);
begin
  FProperties.SetProperty(CAM_FldActCallCnt,AValue);
end;
{$EndRegion}

end.

