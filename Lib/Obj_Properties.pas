unit Obj_Properties;

interface

uses SysUtils, Classes,
     Coll_Obj, Obj_Property,
     Obj_TableDataRecord,
     Obj_TableStructure,
     Obj_TableRecord,
     String_Extended;
type
     TKUO_Properties = class(TColl)
       private
         function int_FindProperty(ADesc: string): TKUO_Property; overload;
         function int_FindProperty(AOption: integer): TKUO_Property; overload;
         function GetValueList: string;
         function GetValueVariant(Index: integer): variant;
         function GetValueIsNULL(Index: integer): boolean;
       public
         procedure Clear;
         procedure Reduce2NotEmpty;
         procedure Reduce2Field(AFieldName: string);
         procedure Assign(AProperties: TKUO_Properties);
         function GetProperty(ADesc: string): variant;
         function SetProperty(ADesc: string; AValue: variant): boolean;
         function HasChanges(AProperties: TKUO_Properties): boolean;
         procedure SetObj2Liste(AObjStrDes: TDBO_TableStructureEntries; ADS: TGDO_DF_DataRecord);
         procedure SetListe2Obj(AObjStrDes: TDBO_TableStructureEntries; ADS: TDBO_TableDataRecord);
         function PropertyHasValue(ADesc: string): boolean;
         function FindProperty(AOption: integer): TKUO_Property; overload;
         function FindProperty(ADesc: string): TKUO_Property; overload;
         function IsEmpty: boolean;

         property ValueList: string read GetValueList;
         property Values[Index: integer]: variant read GetValueVariant;
         property ValueIsNull[Index: integer]: boolean read GetValueIsNULL;
     end;

implementation

uses Math_Extended;

function TKUO_Properties.int_FindProperty(ADesc: string): TKUO_Property;
var I : integer;
    P : TKUO_Property;
begin
  Result:=nil;
  for I:=0 to Count-1 do
  begin
    P:=At(I);
    if P.Desc=ADesc then
    begin
      Result:=P;
      Exit;
    end;
  end;
end;

function TKUO_Properties.int_FindProperty(AOption: integer): TKUO_Property;
var I : integer;
    P : TKUO_Property;
begin
  Result:=nil;
  for I:=0 to Count-1 do
  begin
    P:=At(I);
    if BitSetInLongint(P.Opt,AOption) then
    begin
      Result:=P;
      Exit;
    end;
  end;
end;

procedure TKUO_Properties.Clear;
var P : TKUO_Property;
    I : integer;
begin
  for I:=0 to Count-1 do
  begin
    P:=At(I);
    if (P.Typ='C') or (P.Typ='M') then P.Value:=EmptyStr;
    if (P.Typ='N') or (P.Typ='D') or (P.Typ='S') then P.Value:='';
  end;
end;

procedure TKUO_Properties.Reduce2NotEmpty;
var P : TKUO_Property;
    I : integer;
begin
  I := 0;
  while I<=Count-1 do
  begin
    P:=At(I);
    if P.IsNULL{P.Value=EmptyStr} then
    begin
      AtFree(I);
      Dec(I)
    end;
    Inc(I);
  end;
end;

procedure TKUO_Properties.Reduce2Field(AFieldName: string);
var P : TKUO_Property;
    I : integer;
begin
  I := 0;
  while I<=Count-1 do
  begin
    P:=At(I);
    if (not P.IsIdentField) and (P.Desc<>AFieldName) then
    begin
      AtFree(I);
      Dec(I)
    end;
    Inc(I);
  end;
end;

procedure TKUO_Properties.Assign(AProperties: TKUO_Properties);
var I : integer;
    N,P : TKUO_Property;
begin
  FreeAll;
  for I:=0 to AProperties.Count-1 do
  begin
    P:=AProperties.At(I);
    N:=TKUO_Property.Create;
    N.Assign(P);
    Insert(N);
  end;
end;

function TKUO_Properties.GetProperty(ADesc: string): variant;
var P : TKUO_Property;
begin
  Result:='';
  P:=int_FindProperty(ADesc);
  if P<>nil then
    Result:=P.ValueVariant;
end;

function TKUO_Properties.PropertyHasValue(ADesc: string): boolean;
var P : TKUO_Property;
begin
  Result:=false;
  P:=int_FindProperty(ADesc);
  if P<>nil then Result:=P.HasValue;
end;

function TKUO_Properties.FindProperty(AOption: integer): TKUO_Property;
begin
  Result:=int_FindProperty(AOption);
end;

function TKUO_Properties.FindProperty(ADesc: string): TKUO_Property;
begin
  Result:=int_FindProperty(ADesc);
end;

function TKUO_Properties.IsEmpty: boolean;
var I : integer;
    P,C : TKUO_Property;
begin
  Result:=true;
  for I:=0 to Count-1 do
  begin
    P:=At(I);
    if not P.IsNULL then
    begin
      Result:=false;
      Exit;
    end;
  end;
end;

function TKUO_Properties.GetValueList: string;
var P : TKUO_Property;
    I : integer;
begin
  Result:=EmptyStr;
  for I:=0 to Count-1 do
  begin
    P:=At(I);
    if I=0 then Result:=P.Desc+'='+P.Value
           else Result:=Result+#13#10+P.Desc+'='+P.Value;
  end;
end;

function TKUO_Properties.GetValueVariant(Index: integer): variant;
var P : TKUO_Property;
begin
  Result:=varUnknown;
  P:=At(Index);
  if P<>nil then
    Result:=P.ValueVariant;
end;

function TKUO_Properties.GetValueIsNULL(Index: integer): boolean;
var P : TKUO_Property;
begin
  Result:=true;
  P:=At(Index);
  if P<>nil then
    Result:=P.IsNULL;
end;

function TKUO_Properties.SetProperty(ADesc: string; AValue: variant): boolean;
var P : TKUO_Property;
    V : string;
begin

  P:=int_FindProperty(ADesc);
  if P=nil then Exit(false);

  V:=P.Value;

  if (P.Typ='M') or (P.Typ='C') then if TVarData(AValue).VType<>varNull then P.Value:=AValue;

  if (P.Typ='N') and (P.Dec=0) then if TVarData(AValue).VType<>varNull then P.Value:=IntToStr(AValue);
  if (P.Typ='N') and (P.Dec>0) then if TVarData(AValue).VType<>varNull then P.Value:=ESO_RealToIStr(AValue,P.Dec);
  if (P.Typ='D') then
  begin
    if AValue>0 then P.Value:=ES_GetIntDateStringFromDate(AValue)
                else P.Value:=EmptyStr;
  end;
  if (P.Typ='S') then
  begin
    if AValue>0 then P.Value:=ESO_GetGeODinDateTimeStringFromDate(AValue)
                else P.Value:=EmptyStr;
  end;
  if (P.Typ='L') then if AValue then P.Value:='true' else P.Value:='false';
  {wurde was geändert?}
  Result:=V<>P.Value;
end;

function TKUO_Properties.HasChanges(AProperties: TKUO_Properties): boolean;
var I : integer;
    P,C : TKUO_Property;
    Different : boolean;
begin
  Result:=false;
  for I:=0 to Count-1 do
  begin
    P:=At(I);
    C:=AProperties.int_FindProperty(P.Desc);
    {Zahlen können in verschiedenen Formaten in den Objekten vorliegen. Beim Lesen
     aus der Datenbank werden durch Fields[i].AsString Nachkommstellen entfernt.
     Beim Belegen der Property, z.B. Lesen aus Formular, wird mit ESO_RealToIStr formatiert.
     Zahlen liegen also in unterschiedlichem Format vor !12' und '12.00' haben aber den
     gleichen Wer.}
    {if P.IsNumeric then Different := ES_StrIToReal(P.Value) <> ES_StrIToReal(C.Value)
                   else }Different := P.Value<>C.Value;

    if Different then
    begin
      Result:=true;
      P.Value:=p.Value;
      C.Value:=c.Value;
      Exit;
    end;
  end;
end;

procedure TKUO_Properties.SetObj2Liste(AObjStrDes: TDBO_TableStructureEntries; ADS: TGDO_DF_DataRecord);
var I,F : integer;
    P : TKUO_Property;
begin
  ADS.Data.FreeAll;
  for I:=0 to Count-1 do
  begin
    P:=At(I);
    F:=AObjStrDes.HasField(P.Desc);
    if (P.Value<>EmptyStr) and (F>-1) then ADS.SetStringValue(F,P.Value);
  end;
end;

procedure TKUO_Properties.SetListe2Obj(AObjStrDes: TDBO_TableStructureEntries; ADS: TDBO_TableDataRecord);
var I : integer;
    P : TKUO_Property;
    T : TDBO_TableStructureField;
begin
  for I:=0 to AObjStrDes.Count-1 do
  begin
    T:=AObjStrDes.At(I);
    P:=int_FindProperty(T.FldName);

    if P<>nil then
    begin
      P.Value:=ADS.GetStringValue(AObjStrDes.HasField(T.FldName));
      if P.IsNumeric then SetProperty(P.Desc,ES_StrIToReal(P.Value));
    end;
  end;
end;

end.
