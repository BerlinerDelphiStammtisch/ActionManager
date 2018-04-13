unit Obj_BasicObj;

interface

uses SysUtils, Classes,
     Coll_Obj, Obj_TableDataRecord, //Iv_Tc,
     Collection_BasicObjects,
     Obj_Property,
     Obj_TableStructure,
     Obj_TableRecord,
     Obj_Properties;

type
     TKUO_BasicObj = class(TObject)
       protected
         FStructure : TDBO_TableStructureEntries; //Pointer auf Struktur des Objekts
         FProperties : TKUO_Properties;
         FTabDesc    : string;
         FCollections : TColl;
         procedure int_InitStructure(ATableStructureEntries: TDBO_TableStructureEntries);
         function GetValueList: string;
         function GetIdentField: string;
         function GetSQLWhere: string;
         function GetSQLFields: string;
         function GetSQLPlaceHolders: string;
         procedure InitRuntime; virtual;
         procedure FreeRuntime; virtual;
       public
         Changed : boolean;
         Name    : string;
         constructor Create(ATabDesc: string; ATabStrDescription: TDBO_TableStructureEntries);
         {Objekt als Kopie erstellen}
         constructor CreateAssign(ABasicObj: TKUO_BasicObj); virtual;
         destructor Destroy; override;
         procedure Initialize; virtual;
         {alle Eigenschaften freigeben}
         procedure Empty;
         {Ist das Objekt belegt?}
         function IsEmpty: boolean;
         {alle Eigenschaften leeren}
         procedure Clear;
         {nicht belegte Eigenschaften löschen}
         procedure ClearEmpties;
         {auf die Properties reduzieren, die verändert wurden}
         procedure ClearForUpdate(AFieldName: string);
         {Initialisierung der Properties mit Struktur}
         procedure InitStructure(ATableStructureEntries: TDBO_TableStructureEntries);

         procedure SetObj2Liste(ATabStrDescription: TDBO_TableStructureEntries; ADS: TGDO_DF_DataRecord);
         procedure SetListe2Obj(ATabStrDescription: TDBO_TableStructureEntries; ADS: TDBO_TableDataRecord);

         function HasProperty(AName: string): boolean;
         function GetPropertyByName(AName: string): string; overload;
         function GetValue(AProperty: string): variant;
         procedure SetValue(AProperty: string; AVar: variant);
         function GetPropertyByName(AName: string; var ATyp: string): string; overload;
         procedure SetPropertyByName(AName,AValue: string);
         procedure SetPropertyByIndex(AIndex: integer; AValue: string);
         function HasChanges(AnObject: TKUO_BasicObj): boolean;
         procedure Assign(AnObject: TKUO_BasicObj);
         function PropertyHasValue(ADesc: string): boolean;
         function Matches(AnObject: TKUO_BasicObj): boolean;
         function SameTypIdent(AnObject: TKUO_BasicObj): boolean;
         function SameTypAndProperty(AnObject: TKUO_BasicObj; APropertyName: string): boolean;
         function SatisfiesConditions(AConditions: TStringList): boolean;
         function GetFormatstring(AFormatString: string; APropertyList: TStrings): string;
         function GetFormatVariablesString(AFormatString: string): string;
         {GetSQLSet liefert Set an Feldnamen und Placeholdern für ein Update-Statement}
         function GetSQLSet(AForUpdate: boolean): string;
         function GetParentWhere: string;
         function HasIdentField: boolean;
         function IsIdentFieldEmpty: boolean;
         function IsTyp(ATypName: string): boolean;
         function HasPropertyNumeric(AName: string): boolean;

         property ObjType: string read FTabDesc;
         property Properties: TKUO_Properties read FProperties;
         function GetCollection(AName: string): TCollection_BasicObjects;
         property ValueList: string read GetValueList;
         property IdentField: string read GetIdentField;
         property SQLWhere: string read GetSQLWhere;
         property SQLFields: string read GetSQLFields;
         property SQLPlaceHolders: string read GetSQLPlaceHolders;
     end;

const C_YearOfDate = '%DATE_YEAR';

implementation

uses System.DateUtils, String_Extended, Math_Extended;

const C_WildCard_Multi  = '*';
      C_WildCard_Single = '_';
      C_Komma           = ',';
      C_EQ              = '=';
      C_SQLAnd          = ' AND ';

      CKU_FldParent     = 'PARENT';

constructor TKUO_BasicObj.Create(ATabDesc: string; ATabStrDescription: TDBO_TableStructureEntries);
begin
  FProperties:=TKUO_Properties.Create;
  FTabDesc:=ATabDesc;
  int_InitStructure(ATabStrDescription);
  Changed:=false;
  InitRuntime;
end;

constructor TKUO_BasicObj.CreateAssign(ABasicObj: TKUO_BasicObj);
var I : integer;
begin
  FProperties:=TKUO_Properties.Create;
  FProperties.Assign(ABasicObj.FProperties);
  FTabDesc:=ABasicObj.FTabDesc;
  Changed:=false;
  if ABasicObj.FCollections<>nil then
  begin
    FCollections:=TColl.Create;
    for I := 0 to ABasicObj.FCollections.Count-1 do
      FCollections.Insert(TCollection_BasicObjects.CreateAssign(ABasicObj.FCollections.At(I)));
  end;
end;

destructor TKUO_BasicObj.Destroy;
begin
  FreeAndNil(FProperties);
  FreeRuntime;
  FStructure:=nil;
  inherited;
end;

procedure TKUO_BasicObj.InitRuntime;
begin
  //abstract;
end;

procedure TKUO_BasicObj.FreeRuntime;
begin
  //abstract;
end;

procedure TKUO_BasicObj.Initialize;
begin
  //abstract;
end;

function TKUO_BasicObj.GetValueList: string;
begin
  Result:=FProperties.ValueList;
end;

function TKUO_BasicObj.GetIdentField: string;
var P : TKUO_Property;
begin
  Result:=EmptyStr;
  P:=Properties.FindProperty(COptIdentField);
  if P<>nil then
    Result:=P.Desc;
end;

function TKUO_BasicObj.GetSQLWhere: string;
var P : TKUO_Property;
    I : integer;
begin
  Result:=EmptyStr;
  if Properties.FindProperty(COptIdentField)=nil then Exit;

  {Durchlauf und mit AND zusammenfügen, für mehrer Felder, die nur zusammen eindeutig sind}
  for I:=0 to Properties.Count-1 do
  begin
    P:=Properties.At(I);
    if P.IsIdentField then
    begin
      if Result<>EmptyStr then Result:=Result+C_SQLAnd;
      Result:=Result+P.Desc+C_EQ;
      if P.IsNumeric then Result:=Result+P.Value else
        if P.IsString then Result:=Result+ES_InHochkomma(P.Value);
    end;
  end;
end;

function TKUO_BasicObj.GetSQLSet(AForUpdate: boolean): string;
var I : integer;
    P : TKUO_Property;
begin
  Result:=EmptyStr;
  for I:=0 to Properties.Count-1 do
  begin
    P:=Properties.At(I);

    {bei AUsführung für ein Update werden Ident-Felder übergangen}
    if AForUpdate and P.IsIdentField then Continue;

    if Result=EmptyStr then Result:=P.Desc+C_EQ+':'+P.Desc
                       else Result:=Result+C_Komma+P.Desc+C_EQ+':'+P.Desc;
  end;
end;

function TKUO_BasicObj.GetParentWhere: string;
begin
  Result:=CKU_FldParent+C_EQ+ES_inHochkomma(GetPropertyByName(IdentField));
end;

function TKUO_BasicObj.HasIdentField: boolean;
begin
  Result:=not IdentField.IsEmpty;
end;

function TKUO_BasicObj.IsIdentFieldEmpty: boolean;
begin
  Result:=GetPropertyByName(IdentField).IsEmpty;
end;

function TKUO_BasicObj.IsTyp(ATypName: string): boolean;
begin
  Result:=SameText(FTabDesc,ATypName);
end;

function TKUO_BasicObj.HasPropertyNumeric(AName: string): boolean;
var I : integer;
    P : TKUO_Property;
begin
  Result:=false;
  for I:=0 to FProperties.Count-1 do
  begin
    P:=FProperties.At(I);
    if P.Desc=AName then
    begin
      Exit(P.IsNumeric);
    end;
  end;
end;

function TKUO_BasicObj.GetSQLFields: string;
var I : integer;
    P : TKUO_Property;
begin
  Result:=EmptyStr;
  for I:=0 to Properties.Count-1 do
  begin
    P:=Properties.At(I);
    if Result=EmptyStr then Result:=P.Desc
                       else Result:=Result+C_Komma+P.Desc;
  end;
end;

function TKUO_BasicObj.GetSQLPlaceHolders: string;
var I : integer;
    P : TKUO_Property;
begin
  Result:=EmptyStr;
  for I:=0 to Properties.Count-1 do
  begin
    P:=Properties.At(I);

    //if P.IsIdentField then Continue;

    if Result=EmptyStr then Result:=':'+P.Desc
                       else Result:=Result+C_Komma+':'+P.Desc;
  end;
end;

procedure TKUO_BasicObj.int_InitStructure(ATableStructureEntries: TDBO_TableStructureEntries);
var I : integer;
    T : TDBO_TableStructureField;
    P : TKUO_Property;
begin
  FProperties.FreeAll;

  if ATableStructureEntries = nil then
    Exit;

  for I:=0 to ATableStructureEntries.Count-1 do
  begin
    T:=ATableStructureEntries.At(I);
    P:=TKUO_Property.Create;
    P.Desc:=T.FldName;
    P.Name:=T.FldLong;
    P.Typ:=T.FldType;
    P.Len:=T.FldUnits1;
    P.Dec:=T.FldUnits2;
    {Option komplett übernehmen, keine Auswertung}
    P.Opt:=T.FldOpt;

    if T.FldDefault<>EmptyStr then P.Value:=T.FldDefault
                              else begin
                                //if T.FldType='N' then P.Value:='0';
                              end;;
    FProperties.Insert(P);
  end;
  FStructure:=ATableStructureEntries;
end;

procedure TKUO_BasicObj.Empty;
begin
  FProperties.FreeAll;
end;

procedure TKUO_BasicObj.Clear;
begin
  FProperties.Clear;
end;

procedure TKUO_BasicObj.ClearEmpties;
begin
  FProperties.Reduce2NotEmpty;
end;

function TKUO_BasicObj.IsEmpty: boolean;
begin
  Result:=FProperties.IsEmpty;
end;

procedure TKUO_BasicObj.ClearForUpdate(AFieldName: string);
begin
  FProperties.Reduce2Field(AFieldName);
end;

procedure TKUO_BasicObj.InitStructure(ATableStructureEntries: TDBO_TableStructureEntries);
begin
  int_InitStructure(ATableStructureEntries);
end;

procedure TKUO_BasicObj.SetObj2Liste(ATabStrDescription: TDBO_TableStructureEntries; ADS: TGDO_DF_DataRecord);
begin
  FProperties.SetObj2Liste(ATabStrDescription,ADS);
end;

procedure TKUO_BasicObj.SetListe2Obj(ATabStrDescription: TDBO_TableStructureEntries; ADS: TDBO_TableDataRecord);
begin
  FProperties.SetListe2Obj(ATabStrDescription,ADS);
end;

function TKUO_BasicObj.HasProperty(AName: string): boolean;
var I : integer;
    P : TKUO_Property;
begin
  Result:=false;
  for I:=0 to FProperties.Count-1 do
  begin
    P:=FProperties.At(I);
    if P.Desc=AName then
    begin
      Result:=true;
      Exit;
    end;
  end;
end;

function TKUO_BasicObj.GetPropertyByName(AName: string): string;
var I : integer;
    P : TKUO_Property;
begin
  Result:=EmptyStr;
  for I:=0 to FProperties.Count-1 do
  begin
    P:=FProperties.At(I);
    if P.Desc=AName then
    begin
      Result:=P.Value;
      Exit;
    end;
  end;
end;

function TKUO_BasicObj.GetValue(AProperty: string): variant;
var I : integer;
    P : TKUO_Property;
begin
  Result:=EmptyStr;
  for I:=0 to FProperties.Count-1 do
  begin
    P:=FProperties.At(I);
    if P.Desc=AProperty then
    begin
      Result:=P.ValueVariant;
      Exit;
    end;
  end;
end;

procedure TKUO_BasicObj.SetValue(AProperty: string; AVar: variant);
var I : integer;
    P : TKUO_Property;
begin
  for I:=0 to FProperties.Count-1 do
  begin
    P:=FProperties.At(I);
    if P.Desc=AProperty then
    begin
      P.ValueVariant(AVar);
      Exit;
    end;
  end;
end;

function TKUO_BasicObj.GetPropertyByName(AName: string; var ATyp: string): string;
var I : integer;
    P : TKUO_Property;
begin
  Result:=EmptyStr;
  for I:=0 to FProperties.Count-1 do
  begin
    P:=FProperties.At(I);
    if P.Desc=AName then
    begin
      Result:=P.Value;
      ATyp:=p.Typ;
      Exit;
    end;
  end;
end;

procedure TKUO_BasicObj.SetPropertyByName(AName,AValue: string);
var I : integer;
    P : TKUO_Property;
begin
  for I:=0 to FProperties.Count-1 do
  begin
    P:=FProperties.At(I);
    if P.Desc=AName then
    begin
      if P.Value<>AValue then Changed:=true;
      P.Value:=AValue;
      Exit;
    end;
  end;
end;

procedure TKUO_BasicObj.SetPropertyByIndex(AIndex: integer; AValue: string);
var P : TKUO_Property;
begin
  P:=FProperties.At(AIndex);
  if P.Value<>AValue then Changed:=true;
  P.Value:=AValue;
end;

function TKUO_BasicObj.HasChanges(AnObject: TKUO_BasicObj): boolean;
var I : integer;
    BasicObjColl : TCollection_BasicObjects;
begin

  {1. Properties vergleichen}
  Result:=FProperties.HasChanges(AnObject.FProperties);
  if Result then Exit;

  {2. Existenz der Collectionen vergleichen}
  if (FCollections=nil) and (AnObject.FCollections=nil) then Exit;
  if ((FCollections<>nil) and (AnObject.FCollections=nil)) or
     ((FCollections=nil) and (AnObject.FCollections<>nil))
  then begin
    Exit(true);
  end;

  {3. Anzahl der Colectionen vergleichen}
  if FCollections.Count<>AnObject.FCollections.Count then
  begin
    Exit(true);
  end;

  {4. Inhalte der Collectionen vergleichen}
  for I := 0 to FCollections.Count-1 do
  begin
    BasicObjColl:=FCollections.At(I);
    if BasicObjColl.HasChanges(AnObject.FCollections.At(I)) then
    begin
      Exit(true);
    end;
  end;
end;

procedure TKUO_BasicObj.Assign(AnObject: TKUO_BasicObj);
var MyP,OtherP : TKUO_Property;
    I : integer;
begin
  if AnObject=nil then Exit;
  for I:=0 to FProperties.Count-1 do
  begin
    MyP:=FProperties.At(I);
    OtherP:=AnObject.FProperties.At(I);
    MyP.Value:=OtherP.Value;
  end;
end;

function TKUO_BasicObj.PropertyHasValue(ADesc: string): boolean;
begin
  Result:=FProperties.PropertyHasValue(ADesc);
end;

function TKUO_BasicObj.Matches(AnObject: TKUO_BasicObj): boolean;
var MyP,OtherP : TKUO_Property;
    I : integer;
begin
  Result:=false;
  for I:=0 to AnObject.FProperties.Count-1 do
  begin
    OtherP:=AnObject.FProperties.At(I);
    if OtherP.HasValue then
    begin
      MyP:=FProperties.At(I);
      if MyP.IsIdentField then Continue;
      if (MyP.Typ='C') or (MyP.Typ='M') then
      begin
        if not(ES_MatchWildCard(MyP.Value,OtherP.Value,C_WildCard_Multi,C_WildCard_Single,false)) then Exit;
      end;
      if (MyP.Typ='N') then
      begin
        if MyP.Value<>OtherP.Value then Exit;
      end;
    end;
  end;
  Result:=true;
end;

{Test auf Objekt-Gleicheit an Hand des Typs und des Ident}
function TKUO_BasicObj.SameTypIdent(AnObject: TKUO_BasicObj): boolean;
begin
  Result:=(FTabDesc=AnObject.FTabDesc) and (GetPropertyByName('IDENT')=AnObject.GetPropertyByName('IDENT'));
end;

{Test auf Objekt-Gleicheit an Hand des Typs und des Ident}
function TKUO_BasicObj.SameTypAndProperty(AnObject: TKUO_BasicObj; APropertyName: string): boolean;
begin
  Result:=(FTabDesc=AnObject.FTabDesc) and (GetPropertyByName(APropertyName)=AnObject.GetPropertyByName(APropertyName));
end;

type TCondCompare = (ccUnknown,ccEqual,ccUnequal,ccGreater,ccGreaterEqual,ccLess,ccLessEqual);

function TKUO_BasicObj.SatisfiesConditions(AConditions: TStringList): boolean;
var I : integer;
    P,V : string;
    No  : boolean;
    Prop : TKUO_Property;
    Compare : TCondCompare;

  function CheckDateForYear(AConditionField,AConditionValue: string): boolean;
  var Date : TDateTime;
      Feld : string;
  begin
    Feld:=ES_ExtractKlammer(AConditionField);
    Date:=Properties.GetProperty(Feld);
    Result:=IntToStr(YearOf(Date))=AConditionValue;
  end;


begin
  Result:=false;
  {Alle Bedingungen durchlaufen und prüfen. Sobald eine false liefert Funktion verlassen.}
  for I:=0 to AConditions.Count-1 do
  begin
    P:=AConditions.Names[I];
    V:=AConditions.ValueFromIndex[I];
    Compare:=ccUnknown;

    {Ermitteln, ob es sich auf einen Text auf Ungleichheit handelt}
    if V.StartsWith('<>') then begin Compare:=ccUnequal; V:=V.Substring(2,V.Length-2) end
                          else Compare:=ccEqual;
    if P.EndsWith('>') then begin Compare:=ccGreaterEqual; P:=P.Substring(0,P.Length-1) end;
    if P.EndsWith('<') then begin Compare:=ccLessEqual; P:=P.Substring(0,P.Length-1) end;

    {1. Sonderfall: Jahr in Datum}
    if P.StartsWith(C_YearOfDate) then
    begin
      Result:=CheckDateForYear(P,V);
    end else begin
      Prop:=Properties.FindProperty(P);
      if Prop.IsReal then
      begin
        if Compare=ccUnequal then Result:=ES_StrIToReal(GetPropertyByName(P))<>ES_StrIToReal(V);
        if Compare=ccEqual   then Result:=ES_StrIToReal(GetPropertyByName(P))=ES_StrIToReal(V);
        if Compare=ccGreaterEqual then Result:=ES_StrIToReal(GetPropertyByName(P))>=ES_StrIToReal(V);
        if Compare=ccLessEqual then Result:=ES_StrIToReal(GetPropertyByName(P))<=ES_StrIToReal(V);
      end else if Prop.IsInteger then
        begin
          if Compare=ccUnequal then Result:=ESO_StrToInteger(GetPropertyByName(P))<>ESO_StrToInteger(V);
          if Compare=ccEqual   then Result:=ESO_StrToInteger(GetPropertyByName(P))=ESO_StrToInteger(V);
        end else begin
      if Prop.IsDate then
      begin
        if Compare=ccUnequal then Result:=StrToDate(GetPropertyByName(P))<>StrToDate(V);
        if Compare=ccEqual   then Result:=StrToDate(GetPropertyByName(P))=StrToDate(V);
        if Compare=ccGreaterEqual then Result:=StrToDate(GetPropertyByName(P))>=StrToDate(V);
        if Compare=ccLessEqual then Result:=StrToDate(GetPropertyByName(P))<=StrToDate(V);
      end else begin
          {allgemeiner Fall: Zeichenkettenvergleich auf gleich oder ungleich}
          if Compare=ccUnequal then
          begin
            if V.Contains(C_WildCard_Multi) or V.Contains(C_WildCard_Single)
            then Result:=not(ES_MatchWildCard(GetPropertyByName(P),V,C_WildCard_Multi,C_WildCard_Single,false))
            else Result:=GetPropertyByName(P)<>V;
          end;
          if Compare=ccEqual then
          begin
            if V.Contains(C_WildCard_Multi) or V.Contains(C_WildCard_Single)
            then Result:=ES_MatchWildCard(GetPropertyByName(P),V,C_WildCard_Multi,C_WildCard_Single,false)
            else Result:=GetPropertyByName(P)=V;
          end;
        end;
      end;
    end;

    if not Result then Exit;
  end;
  Result:=true;
end;

function TKUO_BasicObj.GetFormatstring(AFormatString: string; APropertyList: TStrings): string;
var I : integer;
    P : TKUO_Property;
    S : string;
    R : real;
    Z : integer;
    D : TDateTime;
begin
  Result:=AFormatString;
  for I := 0 to APropertyList.Count-1 do
  begin
    P:=Properties.FindProperty(APropertyList[I]);
    if P.IsString then S:=P.ValueVariant
      else if P.IsReal then begin R:=P.ValueVariant; S:=FloatToStrF(R,ffNumber,6,2) end
        else if P.IsDate then begin D:=P.ValueVariant; S:=DateToStr(D) end
          else if P.IsInteger then begin Z:=P.ValueVariant; S:=IntToStr(Z) end;

    Result:=StringReplace(Result,'%s',S,[]);
  end;
end;

function TKUO_BasicObj.GetFormatVariablesString(AFormatString: string): string;
var I : integer;
    P : TKUO_Property;
    S : string;
    R : real;
    Z : integer;
    D : TDateTime;
begin
  Result:=AFormatString;
  for I := 0 to Properties.Count - 1 do
  begin
    P:=Properties.At(I);
    if Result.Contains(ES_InDollar(P.Desc)) then
    begin
      if P.IsString then S:=P.ValueVariant
        else if P.IsReal then begin
                                R:=P.ValueVariant;
                                S:=FloatToStrF(R,ffNumber,P.Len,P.Dec);
                               end
          else if P.IsDate then begin
                                  D:=P.ValueVariant;
                                  S:=DateToStr(D);
                                end
            else if P.IsInteger then begin
                                       Z:=P.ValueVariant;
                                       S:=IntToStr(Z);
                                     end;

      Result:=StringReplace(Result,ES_InDollar(P.Desc),S,[]);
    end;
  end;
end;
function TKUO_BasicObj.GetCollection(AName: string): TCollection_BasicObjects;
var I : integer;
    BasicObjColl: TCollection_BasicObjects;
begin
  Result:=nil;
  if FCollections=nil then Exit;
  for I := 0 to FCollections.Count-1 do
  begin
    BasicObjColl:=FCollections.At(I);
    if BasicObjColl.Name=AName then
    begin
      Result:=BasicObjColl;
      Exit;
    end;
  end;
end;

end.
