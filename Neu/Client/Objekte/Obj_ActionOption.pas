unit Obj_ActionOption;

interface

uses Obj_BasicObj, DBStrukturen;

type
     TAM_ActOption = class(TKUO_BasicObj)
       private
         function GetIdent: integer;
         procedure SetIdent(AValue: integer);
         function GetActionName: string;
         procedure SetActionName(AValue: string);
         function GetActionCategory: string;
         procedure SetActionCategory(AValue: string);
       public
         property Ident: integer read GetIdent write SetIdent;
         property ActionName: string read GetActionName write SetActionName;
         property ActionCategory: string read GetActionCategory write SetActionCategory;
     end;

const CAM_ActOptions = 'actionmanager_Option';

procedure Register_ActOption(AStrukturen: TDB_Strukturen);
procedure UnRegister_ActOption(AStrukturen: TDB_Strukturen);

const CAM_Fldident       = 'ident';
      CAM_FldActName     = 'action_name';
      CAM_FldActCategory = 'action_category';

implementation

uses String_Extended, Obj_TableStructure;

{$Region '------------------------------------ Register ----------------------------------'}
procedure Register_ActOption(AStrukturen: TDB_Strukturen);
var TabStrs: TDBO_TableStructure;
begin
  TabStrs:=TDBO_TableStructure.Create(CAM_ActOptions,CAM_ActOptions,0,1,0,'',TDBO_TableStructureEntries.Create);
  with TabStrs.Structure do
  begin
    Insert(TDBO_TableStructureField.CreateSimple(CAM_Fldident,'N',10,0,1));
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldActName,'C',254,0,0));
    Insert(TDBO_TableStructureField.CreateSimple(CAM_FldActCategory,'C',100,0,0));
  end;
  AStrukturen.Insert(TabStrs);
end;

procedure UnRegister_ActOption(AStrukturen: TDB_Strukturen);
begin
  AStrukturen.FreeI(AStrukturen.GetTabStr(CAM_ActOptions));
end;
{$EndRegion}

{$Region '--------------------------------- Set/Get-Routinen -----------------------------'}
function TAM_ActOption.GetIdent: integer;
begin
  Result:=FProperties.GetProperty(CAM_Fldident);
end;

procedure TAM_ActOption.SetIdent(AValue: integer);
begin
  FProperties.SetProperty(CAM_Fldident,AValue);
end;

function TAM_ActOption.GetActionName: string;
begin
  Result:=FProperties.GetProperty(CAM_FldActName);
end;

procedure TAM_ActOption.SetActionName(AValue: string);
begin
  FProperties.SetProperty(CAM_FldActName,AValue);
end;

function TAM_ActOption.GetActionCategory: string;
begin
  Result:=FProperties.GetProperty(CAM_FldActCategory);
end;

procedure TAM_ActOption.SetActionCategory(AValue: string);
begin
  FProperties.SetProperty(CAM_FldActCategory,AValue);
end;
{$EndRegion}

end.

