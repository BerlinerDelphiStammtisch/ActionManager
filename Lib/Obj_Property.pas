unit Obj_Property;

interface

const COptIdentField = 0;
      COptNameField  = 1;

type
     TKUO_Property = class(TObject)
       private
         function GetIsNULL: boolean;
         function GetHasValue: boolean;
         function IsNumTyp: boolean;
         function IsIntegerTyp: boolean;
         function IsRealTyp: boolean;
         function IsDateTyp: boolean;
         function IsStringTyp: boolean;
         function IsMemoTyp: boolean;
         function IsBooleanTyp: boolean;
         function IsTimeStampTyp: boolean;
         function HasOption(ABitPos: integer): boolean;
       public
         Desc,
         Name,
         Typ,
         Value : string;
         Dec : integer;
         Opt : int32;
         procedure Assign(AProperty: TKUO_Property);
         function ValueVariant: variant;

         property IsNULL : boolean read GetIsNULL;
         property HasValue : boolean read GetHasValue;
         property IsNumeric : boolean read IsNumTyp;
         property IsInteger : boolean read IsIntegerTyp;
         property IsReal : boolean read IsRealTyp;
         property IsDate: boolean read IsDateTyp;
         property IsString: boolean read IsStringTyp;
         property IsMemo: boolean read IsMemoTyp;
         property IsBoolean: boolean read IsBooleanTyp;
         property IsTimeStamp: boolean read IsTimeStampTyp;
         property IsIdentField : boolean index COptIdentField read HasOption;
         property IsNameField : boolean index COptNameField read HasOption;
       end;

implementation

uses SysUtils, Math_Extended, String_Extended;

function TKUO_Property.GetIsNULL: boolean;
begin
  Result:=Length(Value)=0;
end;

function TKUO_Property.GetHasValue: boolean;
begin
  if Typ='N' then Result:=(Length(Value)>0) //and (Value<>'0')
             else Result:=Length(Value)>0;
end;

function TKUO_Property.IsNumTyp: boolean;
begin
  Result:=Typ='N';
end;

function TKUO_Property.IsIntegerTyp: boolean;
begin
  Result:=(Typ='N') and (Dec=0);
end;

function TKUO_Property.IsRealTyp: boolean;
begin
  Result:=(Typ='N') and (Dec>0);
end;

function TKUO_Property.IsDateTyp: boolean;
begin
  Result:=(Typ='D');
end;

function TKUO_Property.IsStringTyp: boolean;
begin
  Result:=(Typ='C') or (Typ='M');
end;

function TKUO_Property.IsMemoTyp: boolean;
begin
  Result:=(Typ='M');
end;

function TKUO_Property.IsBooleanTyp: boolean;
begin
  Result:=(Typ='L');
end;

function TKUO_Property.IsTimeStampTyp: boolean;
begin
  Result:=(Typ='S');
end;

function TKUO_Property.HasOption(ABitPos: integer): boolean;
begin
  Result:=BitSetInLongInt(Opt,ABitPos);
end;

procedure TKUO_Property.Assign(AProperty: TKUO_Property);
begin
  Desc:=AProperty.Desc;
  Name:=AProperty.Name;
  Typ:=AProperty.Typ;
  Value:=AProperty.Value;
  Dec:=AProperty.Dec;
  Opt:=AProperty.Opt;
end;

function TKUO_Property.ValueVariant: variant;
begin
  if (Typ='C') or (Typ='M') then Result:=Value;
  if (Typ='N') then
  begin
    if (HasValue) then
    begin
      if (Typ='N') and (Dec=0) then Result:=StrToInt(Value);
      if (Typ='N') and (Dec>0) then Result:=ESO_StrToReal(Value);
    end else Result:=0;
  end;
  if (Typ='D') then Result:=ES_GetDateFromIntDateString(Value);
  if (Typ='S') then Result:=ESO_GetDateTimeFromGeODinString(Value);

  if (Typ='L') then Result:=Value='true';
end;

end.
