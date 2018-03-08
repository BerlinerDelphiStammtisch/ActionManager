unit DBStrukturen;

interface

uses
  Coll_Obj, Obj_TableStructure;

type

    TDB_Strukturen  = class(TColl)
    public
      procedure Init; virtual;
      function GetTabStr(ATabName: string): TDBO_TableStructure;
    end;

implementation

procedure TDB_Strukturen.Init;
begin

end;

function TDB_Strukturen.GetTabStr(ATabName: string): TDBO_TableStructure;
var L : TDBO_TableStructure;
    I : integer;
begin
  Result:=nil;
  for I:=0 to Count-1 do
  begin
    L:=At(I);
    if L.DisplayName=ATabName then
    begin
      Result:=L;
      Break;
    end;
  end;
end;

end.

