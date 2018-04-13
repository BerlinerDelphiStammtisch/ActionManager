unit Collection_BasicObjects;

{$O+,F+,X+,I-,S-}

interface

{ Grundsatz:
    1: Es werden keine Programmspezifischen Units eingebunden. Der Charakter der
       Unit bleibt absolut abstrakt.
    2: Es handelt sich um eine Ableitung der TColl-Basisklasse. Die Verwendung
       ist ausschließlich auf Objekte als Ableitungen von TKUO_BasicObj als
       Mitglieder ausgelegt.
    3: Objekte werden nur als Pointer zurückgegeben und an der aufrufenden Stelle
       typisiert.
  Todo:
    1. Funtionen zur Auswertung von Datumsfunktionen integrieren, z.B. YearOf
    2. Formeln zur Berechnung von Werten integrieren, z.B. Summe von Produkt
    3. FormelTree für Berechnungen und Bedingungen integrieren
    4. Plattformspezifische Elemente wie Listview in Handler ausgliedern
}
uses  {---Delphi-RTL---}  SysUtils, Windows, Classes, Math,
      {---Delphi-VCL---}  Vcl.ComCtrls {für GetListView},
      {---Libs---}        Coll_Obj, String_Extended{, Iv_TcSQL, Iv_Tc};

type

  TCollection_BasicObjects = class(TColl)
     public
       Name : string;
       TabDesc : string;
       SortField : string;
       SortDown  : boolean;
       constructor Create(AName: string);
       constructor CreateAssign(ACollection_BasicObjects: TCollection_BasicObjects);
       {-- Sortierungen -------------------------------------------------------}
       procedure SortOnProperty(AProperty: string; Down: boolean = false);
       procedure SortOnPropertyAuto(AProperty: string; ANumeric: boolean=false);
       procedure SortByPropertyNumeric(AProperty: string; Down: boolean = false);
       {-- Prüfungen ----------------------------------------------------------}
       function HasChanges(ABasicObjColl: TCollection_BasicObjects): boolean;
       function CheckTextInProperty(AText,AProperty: String): Boolean;

       {-- Element finden -----------------------------------------------------}
       function FindByPropertyValue(AProperty, AValue: string): Pointer;
			 function PropertyValueByIndex(AProperty: string; AIndex: integer): string;
       function IndexByPropertyValue(AProperty, AValue: string): integer;
       function HasItemWithProperty(AProperty, AValue: string): boolean;
       function FindIndexBySearchObj(ABasicObj: Pointer): integer;
       {-- Inhalt abfragen ----------------------------------------------------}
       function GetPropertyContentQuery(AProperty: string): string;
       function GetPropertyTextByPropertyValue(ASearchProperty,ASearchValue,ATextProperty: string): string;
       {-- Listen -------------------------------------------------------------}
       function  GetListStringOnProperty(AProperty: string): string;
       procedure GetListOnProperty(AProperty: string; AList: TStrings); overload;
       procedure GetListOnProperty(AProperty,AMatch: string; AList: TStrings); overload;
       procedure GetUniqueListOnProperty(AProperty: string; AList: TStrings);
       procedure GetListOnPropertyOfSubObject(ASubCollection,AProperty: string; AList: TStrings);
       procedure ChangeListOnProperty(APropertyFind,APropertySet: string; AList: TStrings);
       function  GetFormatstringList(AFormatstring: string; APropertyList: TStringList; AList: TStrings;
                                     ASumFormatString,ASumProperty: string; const AAdditionalProperty:string=''): double;
       procedure GetListFormatString(AFormatstring,AField1: string; AList: TStringList); overload;
       procedure GetListFormatString(AFormatstring,AField1,AField2: string; AList: TStrings); overload;
       procedure GetListFormatString(AFormatstring,AField1,AField2,AField3: string; AList: TStringList); overload;

       {-- Ident generieren ---------------------------------------------------}
       function GetFirstFreeIdent(AProperty,AMatchStr: string): integer; overload;
       function GetFirstFreeIdent(AProperty: string): integer; overload;
       function GetNextIDPerDate(ADateProperty,AIdentProperty: string; ADate: TDateTime): integer;

       {-- Übernahme der Daten einer anderen Kollektion -----------------------}
       procedure SetObjectCollection(ADstCollection,ADstProperty,ASrcProperty: string;
                                     ASrcCollection: TCollection_BasicObjects);
       {-- Arbeiten mit Bedingungen -------------------------------------------}
       procedure GetObjectCollectionForCondition(ACondition: string; ACollection: TCollection_BasicObjects); overload;
       procedure GetObjectCollectionForCondition(ACondition: TStringList; ACollection: TCollection_BasicObjects); overload;
       function CountForCondition(ACondition: string): integer; overload;
       function CountForCondition(ACondition: TStringList): integer; overload;
       function GetLastObjectForCondition(ACondition: TStringList): Pointer;
       function GetObjectForCondition(ACondition: TStringList): Pointer; overload;
       function GetObjectForCondition(ACondition: string): Pointer;  overload;
       function HasObjectForCondition(ACondition: TStringList): boolean; overload;
       function HasObjectForCondition(ACondition: string): boolean; overload;
       procedure DeleteForCondition(ACondition: TStringList); overload;
       procedure DeleteForCondition(ACondition: string); overload;
       function GetValueForCondition(ACondition: TStringList; AProperty: string): variant; overload;
       function GetValueForCondition(ACondition: string; AProperty: string): variant;  overload;
       procedure ChangeValueForCondition(ACondition: string; AProperty,AValue: string);

       {-- Ausgabe von Berichten ----------------------------------------------}
       procedure GetReportList2(AReportList: TStringList; ASummaryField,ATypField: string;
                                var JS,JN,JU,JW,JL,JO: real);
       procedure GetAsCSV(AListe: TStrings; const ASeparator: char=';'; const ATextSeparator: boolean=false);

       {-- Bedienung von Anzeigeelementen -------------------------------------}
       procedure GetListview(AListView: TListView; const ASearchObj: Pointer = nil);
       procedure GetItems(AItems: TStrings; AProperty: string; ADupIgnore: boolean=false; ASort: boolean=false);

       {-- Statistik ----------------------------------------------------------}
       function  CountCondition(const ACondition: string = ''): integer;
       function  Summary(AField: string; const ACondition: string = ''): integer;
       function  SummaryForCondition(ACondition,ASubCondition: TStringList; AValueCollection,AAmountField,AValueField: string): Double; overload;
       function  SummaryForCondition(ACondition: string; AValueCollection,AAmountField,AValueField: string): Double; overload;
       function  SummaryForCondition(ACondition: TStringList; AValueField: string): Double; overload;
       procedure SummaryForCategory(ACategoryField, ASummaryField: string; ACategoryCollection: TCollection_BasicObjects);
       procedure FirstLastDate(ADateField: string; var AFirstDate, ALastDate: TDateTime);
     end;

{BasicObj    - Hauptelement in dieser Kollektion }
{ACondition  - Bedingung, die das Hauptelement erfüllen muss}

{AValueCollection - Name der zu verwendenden Kollektion am Hauptobjekt}
{SubBasicObj      - Unterobjekt, das in einer Kollektion (AValueCollection) am Hauptobjekt hängt}
{ASubCondition 		- Bedingung, die das Unterobjekt erfüllen muss}
{AAmountField			- Feld in SubBasicObj, das eine Mengenangabe enthält}
{AValueField      - Feld in SubBasicObj, das eine Wertangabe enthält}


implementation

uses Obj_BasicObj, Obj_Property;

const //CReferenz_ident = 'IDENT';
      CReferenz_ident = 'CODE';

      C_EQ    = '=';
      C_SQLOr = ' OR ';

      C_ULine = '_';
      C_Vert  = '|';

constructor TCollection_BasicObjects.Create(AName: string);
begin
  inherited Create;
  Name:=AName;
end;

constructor TCollection_BasicObjects.CreateAssign(ACollection_BasicObjects: TCollection_BasicObjects);
var BasicObj : TKUO_BasicObj;
    I : integer;
begin
  inherited Create;
  Name:=ACollection_BasicObjects.Name;
  for I:=0 to ACollection_BasicObjects.Count-1 do
  begin
    BasicObj:=ACollection_BasicObjects.At(I);
    Insert(TKUO_BasicObj.CreateAssign(BasicObj));
  end;
end;

{$Region '----------------------------- Sortierungen---------------------------------}
procedure TCollection_BasicObjects.SortOnProperty(AProperty: string; Down: boolean = false);
var L : TStringList;
    I : integer;
    S : string;
    A : TKUO_BasicObj;
begin
  {sortierte Liste Kreieren}
  L:=TStringList.Create;
  L.Sorted:=true;
  L.Duplicates:=dupAccept;
  {in den Eigenschaften die Sortierung merken}
  SortField:=AProperty;
  SortDown:=Down;
  {jetzt sortieren - in sortierte Liste stellen}
  try
    for I:=Count-1 downto 0 do
    begin
      A:=At(I);
      if A is TKUO_BasicObj then S:=A.GetPropertyByName(AProperty)
                            else S:='ZZZ'+IntToStr(I);
      L.AddObject(S,A);
    end;
    DeleteAll;
    {aus sortierter Liste auslesen, vorwärts oder rückwärts}
    if Down then for I:=L.Count-1 downto 0 do Insert(L.Objects[I])
            else for I:=0 to L.Count-1 do Insert(L.Objects[I]);
    {fertig}
  finally
    FreeAndNil(L);
  end;
end;

procedure TCollection_BasicObjects.SortOnPropertyAuto(AProperty: string; ANumeric: boolean=false);
begin
  if ANumeric then
    if SortField=AProperty then SortByPropertyNumeric(AProperty,not SortDown)
                           else SortByPropertyNumeric(AProperty)
  else
    if SortField=AProperty then SortOnProperty(AProperty,not SortDown)
                           else SortOnProperty(AProperty);
end;
{Sortieren nach numerischen Inhalten. Die Zahlen werden direkt verglichen. Der Algorithmus
 durchläuft die Kollektion und vergleicht immer zwei benachbarte Element.
 Mit Strings auch probiert -> ist sehr langsam.
}
procedure TCollection_BasicObjects.SortByPropertyNumeric(AProperty: string; Down: boolean = false);
var I,J,maxCount : Integer;
    Num1,Num2 : real;
    Obj1,Obj2 : TKUO_BasicObj;
    Change : boolean;
begin
  maxCount:= Count-1;
  {in den Eigenschaften die Sortierung merken}
  SortField:=AProperty;
  SortDown:=Down;
  {jetzt sortieren}
	for I:= 1 to maxCount do
  begin
    for J:= 0 to maxCount-I do
    begin
      {
      Obj1:= TKUO_BasicObj.CreateAssign(At(J));
      Obj2:= TKUO_BasicObj.CreateAssign(At(J+1));
      }
			Obj1:=At(J);
      Obj2:=At(J+1);

      Num1:=Obj1.Properties.GetProperty(AProperty);
      Num2:=Obj2.Properties.GetProperty(AProperty);
      if Down then Change:=(Num1 <= Num2)
              else Change:=(Num1 >= Num2);

      {bei diesem Verfahren ist nicht sichergestellt, dass der Rückgabewert von GetProperty wirklich numerisch
       ist. Die Sortierung ist dann nicht vollständig
      if Down then Change:=(Obj1.Properties.GetProperty(AProperty) <= Obj2.Properties.GetProperty(AProperty))
              else Change:=(Obj1.Properties.GetProperty(AProperty) >= Obj2.Properties.GetProperty(AProperty));
               }
      if Change then
        Fitems.Move(J+1,J);
      (*
			if Change then
      begin
        AtPut(J,Obj2);          {AtPut tauscht die Objekte aus}
				AtPut(J+1,Obj1);
			end else begin
        FreeAndNil(Obj1);       {passiert das nicht, müssen die Duplikate wieder gelöscht werden}
        FreeAndNil(Obj2);
      end;
      *)
    end;
  end;
end;

{$EndRegion}

{$Region '-------------------------------- Prüfungen --------------------------------------}
function TCollection_BasicObjects.HasChanges(ABasicObjColl: TCollection_BasicObjects): boolean;
var I : integer;
    BasicObj : TKUO_BasicObj;
begin
  Result:=false;
  if ABasicObjColl=nil then Exit;
  if Count<>ABasicObjColl.Count then
  begin
    Result:=true;
    Exit;
  end;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    if BasicObj.HasChanges(ABasicObjColl.At(I)) then
    begin
      Result:=true;
      Exit;
    end;
  end;
end;

function TCollection_BasicObjects.CheckTextInProperty(AText,AProperty: String): Boolean;
var I : integer;
    S : string;
    BasicObj : TKUO_BasicObj;
begin
  Result:= false;
  for I:=0 to Count- 1 do
  begin
    BasicObj:= At(I);
    S:=BasicObj.GetPropertyByName(AProperty);
    if Pos(S,AText)>0 then
    begin
      Result:= true;
      Exit;
    end;
  end;
end;
{$EndRegion}

{$Region '-------------------------------- Element finden --------------------------------'}
function TCollection_BasicObjects.FindByPropertyValue(AProperty, AValue: string): Pointer;
var I : integer;
    A : TKUO_BasicObj;
begin
  Result:=nil;
  for I:=0 to Count-1 do
  begin
    A:=At(I);
    if SameText(A.GetPropertyByName(AProperty),AValue) then
    begin
      Result:=A;
      Exit;
    end;
  end;
end;

function TCollection_BasicObjects.PropertyValueByIndex(AProperty: string; AIndex: integer): string;
var BasicObj : TKUO_BasicObj;
begin
  Result:=EmptyStr;
  BasicObj:=At(AIndex);
  if BasicObj<>nil then Result:=BasicObj.GetPropertyByName(AProperty);
end;

function TCollection_BasicObjects.IndexByPropertyValue(AProperty, AValue: string): integer;
var I : integer;
    A : TKUO_BasicObj;
begin
  Result:=-1;
  for I:=0 to Count-1 do
  begin
    A:=At(I);
    if SameText(A.GetPropertyByName(AProperty),AValue) then
    begin
      Result:=I;
      Exit;
    end;
  end;
end;

function TCollection_BasicObjects.HasItemWithProperty(AProperty, AValue: string): boolean;
begin
  Result:=IndexByPropertyValue(AProperty,AValue)>-1;
end;

function TCollection_BasicObjects.FindIndexBySearchObj(ABasicObj: Pointer): integer;
var I : integer;
    A : TKUO_BasicObj;
    Compare : TKUO_BasicObj;
begin
  Result:=-1;
  Compare:=ABasicObj;
  for I:=0 to Count-1 do
  begin
    A:=At(I);
    if A.Matches(Compare) then
      Exit(I)
  end;
end;
{$EndRegion}

{$Region '-------------------------- Inhalte abfragen ------------------------------------'}
function TCollection_BasicObjects.GetPropertyContentQuery(AProperty: string): string;
var I : integer;
    A : TKUO_BasicObj;
    S : string;
    T : string;
begin
  Result:=EmptyStr;
  for I:=Count-1 downto 0 do
  begin
    A:=At(I);
    if A is TKUO_BasicObj then
    begin
      S:=A.GetPropertyByName(AProperty,T);
      if S.IsEmpty then Continue;
      
      if T='C' then S:=ES_InHochkomma(S);
      if Result=EmptyStr then Result:=CReferenz_ident+C_EQ+S
                         else Result:=Result+C_SQLOr+CReferenz_ident+C_EQ+S;
    end;
  end;
end;

function TCollection_BasicObjects.GetPropertyTextByPropertyValue(ASearchProperty,ASearchValue,ATextProperty: string): string;
var BasicObj : TKUO_BasicObj;
begin
  Result:=EmptyStr;
  BasicObj:=FindByPropertyValue(ASearchProperty,ASearchValue);
  if BasicObj<>nil then
    Result:=BasicObj.GetPropertyByName(ATextProperty);
end;
{$EndRegion}

{$Region '---------------------------------------- Listen --------------------------------'}
function TCollection_BasicObjects.GetListStringOnProperty(AProperty: string): string;
var I : integer;
    A : TKUO_BasicObj;
begin
  Result:=EmptyStr;
  for I:=0 to Count-1 do
  begin
    A:=At(I);
    Result:=Result+A.GetPropertyByName(AProperty)+#13+#10;
  end;
end;

procedure TCollection_BasicObjects.GetListOnProperty(AProperty: string; AList: TStrings);
var I : integer;
    BasicObj : TKUO_BasicObj;
begin
  AList.Clear;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    AList.AddObject(BasicObj.GetPropertyByName(AProperty),BasicObj);
  end;
end;

procedure TCollection_BasicObjects.GetListOnProperty(AProperty,AMatch: string; AList: TStrings);
var I : integer;
    PropString : string;
    BasicObj : TKUO_BasicObj;
begin
  AList.Clear;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    PropString:=BasicObj.GetPropertyByName(AProperty);
    if AnsiLowerCase(PropString).Contains(AnsiLowerCase(AMatch)) then
      AList.AddObject(PropString,BasicObj);
  end;
end;

procedure TCollection_BasicObjects.GetUniqueListOnProperty(AProperty: string; AList: TStrings);
var I : integer;
    Value : string;
    BasicObj : TKUO_BasicObj;
begin
  AList.Clear;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    Value:=BasicObj.GetPropertyByName(AProperty);
    if AList.IndexOf(Value)=-1 then
      AList.Add(Value);
  end;
end;
{
Die Objekte der Kollektion werden durchlaufen. Von der benannten SubKollektion des
jeweiligen Objektes wird die Liste eines Elements abgeholt.
}
procedure TCollection_BasicObjects.GetListOnPropertyOfSubObject(ASubCollection,AProperty: string; AList: TStrings);
var SubList : TStringList;
    SubCollection : TCollection_BasicObjects;
    BasicObj : TKUO_BasicObj;
    I : integer;
begin
  SubList := TStringList.Create;
  try
    for I:=0 to Count-1 do
    begin
      BasicObj:=At(I);
      SubCollection:=BasicObj.GetCollection(ASubCollection);
      if SubCollection<>nil then
      begin
        SubCollection.GetListOnProperty(AProperty,SubList);
        AList.AddStrings(SubList);
      end;
    end;
  finally
    SubList.Free;
  end;
end;
{
 Die Einträge in einer Liste, die aus einer Property erstellt wurden, werden wiedergefunden
 und durch eine andere Property ersetzt
}
procedure TCollection_BasicObjects.ChangeListOnProperty(APropertyFind,APropertySet: string; AList: TStrings);
var BasicObj : TKUO_BasicObj;
    I,N : integer;
    PropertyValue : string;
begin
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    PropertyValue:=BasicObj.GetPropertyByName(APropertyFind);
    N:=AList.IndexOf(PropertyValue);
    if N>-1 then AList[N]:=BasicObj.GetPropertyByName(APropertySet);
  end;
end;


procedure TCollection_BasicObjects.GetListFormatString(AFormatstring,AField1: string; AList: TStringList);
var I : integer;
    BasicObj : TKUO_BasicObj;
    F1: string;
begin
  AList.Clear;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    F1:=BasicObj.GetPropertyByName(AField1);
    AList.AddObject(Format(AFormatString,[F1]),BasicObj);
  end;
end;

function TCollection_BasicObjects.GetFormatstringList(AFormatstring: string; APropertyList: TStringList; AList: TStrings;
                                                       ASumFormatString,ASumProperty: string; const AAdditionalProperty:string=''): double;
var I : integer;
    BasicObj : TKUO_BasicObj;
    Summe : real;
begin
  AList.Clear;
  Summe:=0;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    AList.Add(BasicObj.GetFormatstring(AFormatstring,APropertyList));
    Summe:=Summe+BasicObj.Properties.GetProperty(ASumProperty);
  end;
  {Summenzeile anfügen}
  AList.Add(EmptyStr);
  AList.Add(StringReplace(ASumFormatString,'%s',FloatToStrF(Summe,ffNumber,6,2),[]));
  {Ergänzende Angabe für Summenzeile, z.B. Währung. Es wird ein       }
  {zweites %s mit dem Inhalt von Property des letzten Objektes ersetzt}
  if AAdditionalProperty<>EmptyStr then
    AList[AList.Count-1]:=StringReplace(AList[AList.Count-1],'%s',BasicObj.GetPropertyByName(AAdditionalProperty),[]);
  {Gesamtsumme als Ergebnis zurückgeben}
  Result:=Summe;
end;

procedure TCollection_BasicObjects.GetListFormatString(AFormatstring,AField1,AField2: string; AList: TStrings);
var I : integer;
    BasicObj : TKUO_BasicObj;
    F1,F2: string;
begin
  AList.Clear;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    F1:=BasicObj.GetPropertyByName(AField1);
    F2:=BasicObj.GetPropertyByName(AField2);
    AList.AddObject(Format(AFormatString,[F1,F2]),BasicObj);
  end;
end;

procedure TCollection_BasicObjects.GetListFormatString(AFormatstring,AField1,AField2,AField3: string; AList: TStringList);
var I : integer;
    BasicObj : TKUO_BasicObj;
    F1,F2,F3: string;
begin
  AList.Clear;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    F1:=BasicObj.GetPropertyByName(AField1);
    F2:=BasicObj.GetPropertyByName(AField2);
    F3:=BasicObj.GetPropertyByName(AField3);
    AList.AddObject(Format(AFormatString,[F1,F2,F3]),BasicObj);
  end;
end;
{$EndRegion}

{$Region '----------------------------------- Ident generieren ---------------------------'}
function TCollection_BasicObjects.GetFirstFreeIdent(AProperty,AMatchStr: string): integer;
var MatchStr : string;
    Ident : integer;
    I,N   : integer;
    BasicObj : TKUO_BasicObj;
begin
  Ident:=0;

  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    MatchStr:=BasicObj.GetPropertyByName(AProperty);
    if ES_MatchWildCard(MatchStr,AMatchStr,'*','?',false) then
    begin
      N:=StrToInt(Copy(ES_GetWordByIndex(MatchStr,C_ULine,3),1,2));
      if N>Ident then Ident:=N;
    end;
  end;
  Result:=Ident+1;
end;

function TCollection_BasicObjects.GetFirstFreeIdent(AProperty: string): integer;
var Ident : integer;
    I,N   : integer;
    BasicObj : TKUO_BasicObj;
begin
  Ident:=0;

  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    N:=StrToInt(BasicObj.GetPropertyByName(AProperty));
    if N>Ident then Ident:=N;
  end;
  Result:=Ident+1;
end;

function TCollection_BasicObjects.GetNextIDPerDate(ADateProperty,AIdentProperty: string; ADate: TDateTime): integer;
var BasicObj : TKUO_BasicObj;
    Datum    : TDateTime;
    I,ID,Ident : integer;
begin
  ID:=0;
  for I:=0 to Pred(Count) do
  begin
    BasicObj:=At(I);
    Datum:=BasicObj.Properties.GetProperty(ADateProperty);
    if Datum=ADate then
    begin
      Ident:=BasicObj.Properties.GetProperty(AIdentProperty);
      if Ident>ID then
        ID:=Ident;
    end;
  end;
  Result:=Succ(ID);
end;
{$EndRegion}

{$Region '---------------------- Arbeiten mit mehreren Kollektionen ----------------------'}
procedure TCollection_BasicObjects.SetObjectCollection(ADstCollection,ADstProperty,ASrcProperty: string;
                                                       ASrcCollection: TCollection_BasicObjects);
var Own,Ext : TKUO_BasicObj;
    DstCollection : TCollection_BasicObjects;
    I,N    : integer;
begin
  for I:=0 to Count-1 do
  begin
    Own:=At(I);
    DstCollection:=Own.GetCollection(ADstCollection);
    DstCollection.DeleteAll;
    for N:=0 to ASrcCollection.Count-1 do
    begin
      Ext:=ASrcCollection.At(N);
      if Own.GetPropertyByName(ADstProperty)=Ext.GetPropertyByName(ASrcProperty) then
        DstCollection.Insert(Ext);
    end;
  end;
end;
{$EndRegion}

{$Region '------------------------------- Arbeiten mit Bedingungen -----------------------'}
procedure TCollection_BasicObjects.GetObjectCollectionForCondition(ACondition: string; ACollection: TCollection_BasicObjects);
var I : integer;
    BasicObj : TKUO_BasicObj;
    Condition : TStringList;
begin
  Condition:=TStringList.Create;
  try
    Condition.Text:=ACondition;
    for I:=0 to Count-1 do
    begin
      BasicObj:=At(I);
      if BasicObj.SatisfiesConditions(Condition) then ACollection.Insert(BasicObj);
    end;
  finally
    Condition.Free;
  end;
end;

procedure TCollection_BasicObjects.GetObjectCollectionForCondition(ACondition: TStringList; ACollection: TCollection_BasicObjects);
var I : integer;
    BasicObj : TKUO_BasicObj;
begin
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    if BasicObj.SatisfiesConditions(ACondition) then ACollection.Insert(BasicObj);
  end;
end;

function TCollection_BasicObjects.CountForCondition(ACondition: string): integer;
var ConditionList: TStringList;
begin
  ConditionList:=TStringList.Create;
  try
    ConditionList.Text:=ACondition;
    Result:=CountForCondition(ConditionList);
  finally
    FreeAndNil(ConditionList);
  end;
end;

function TCollection_BasicObjects.CountForCondition(ACondition: TStringList): integer;
var I : integer;
    BasicObj : TKUO_BasicObj;
begin
  Result:=0;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    if BasicObj.SatisfiesConditions(ACondition) then Inc(Result);
  end;
end;

function TCollection_BasicObjects.GetLastObjectForCondition(ACondition: TStringList): Pointer;
var I : integer;
    BasicObj : TKUO_BasicObj;
begin
  Result:=nil;
  for I:=Count-1 downto 0 do
  begin
    BasicObj:=At(I);
    if BasicObj.SatisfiesConditions(ACondition) then
    begin
      Result:=BasicObj;
      Exit;
    end;
  end;
end;

function TCollection_BasicObjects.GetObjectForCondition(ACondition: TStringList): Pointer;
var I : integer;
    BasicObj : TKUO_BasicObj;
begin
  Result:=nil;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    if BasicObj.SatisfiesConditions(ACondition) then
    begin
      Result:=BasicObj;
      Exit;
    end;
  end;
end;

function TCollection_BasicObjects.GetObjectForCondition(ACondition: string): Pointer;
var Condition : TStringList;
begin
  Condition:=TStringList.Create;
  try
    Condition.Text:=ACondition;
    Result:=GetObjectForCondition(Condition);
  finally
    Condition.Free;
  end;
end;

function TCollection_BasicObjects.HasObjectForCondition(ACondition: TStringList): boolean;
begin
  Result:=GetObjectForCondition(ACondition)<>nil;
end;

function TCollection_BasicObjects.HasObjectForCondition(ACondition: string): boolean;
var CL : TStringList;
begin
  CL:=TStringList.Create;
  try
    CL.Text:=ACondition;
    Result:=HasObjectForCondition(CL);
  finally
    CL.Free;
  end;
end;

procedure TCollection_BasicObjects.DeleteForCondition(ACondition: TStringList);
var I,C : integer;
    BasicObj : TKUO_BasicObj;
begin
  C:=Count; I:=0;
  while I < C do
  begin
    BasicObj:=At(I);
    if BasicObj.SatisfiesConditions(ACondition) then
    begin
      AtFree(I);
      Dec(C); Dec(I);
    end;
    Inc(I);
  end;
end;

procedure TCollection_BasicObjects.DeleteForCondition(ACondition: string);
var CL : TStringList;
begin
  CL:=TStringList.Create;
  try
    CL.Add(ACondition);
    DeleteForCondition(CL);
  finally
    CL.Free;
  end;
end;

function TCollection_BasicObjects.GetValueForCondition(ACondition: TStringList; AProperty: string): variant;
var BasicObj : TKUO_BasicObj;
begin
  BasicObj:=GetObjectForCondition(ACondition);
  if BasicObj<>nil then
    Result:=BasicObj.Properties.GetProperty(AProperty);
end;

function TCollection_BasicObjects.GetValueForCondition(ACondition: string; AProperty: string): variant;
var CL : TStringList;
begin
  CL:=TStringList.Create;
  try
    CL.Add(ACondition);
    Result:=GetValueForCondition(CL,AProperty);
  finally
    CL.Free;
  end;
end;

procedure TCollection_BasicObjects.ChangeValueForCondition(ACondition: string; AProperty,AValue: string);
var BasicObj : TKUO_BasicObj;
begin
  BasicObj:=GetObjectForCondition(ACondition);
  if BasicObj<>nil then BasicObj.SetPropertyByName(AProperty,AValue);
end;
{$EndRegion}

{$Region '-------------------------------- spezielle Berichte ----------------------------'}
procedure TCollection_BasicObjects.GetReportList2(AReportList: TStringList; ASummaryField,ATypField: string;
                                                  var JS,JN,JU,JW,JL,JO: real);
var R : TKUO_BasicObj;
    I : integer;
    S,
    MS,MN,MU,MW,ML,MO : real;
    SPN,SPU,SPW,SPL,SPO: string;
begin
 MS:=0;MN:=0;MU:=0;MW:=0;ML:=0;MO:=0;
 for I:=0 to Count-1 do
 begin
  R:=At(I);
  S:=R.Properties.GetProperty(ASummaryField);
  if R.GetPropertyByName(ATypField)='Neuerwerb' then begin MN:=MN+S; JN:=JN+S end else
   if R.GetPropertyByName(ATypField)='Update'    then begin MU:=MU+S; JU:=JU+S end else
    if R.GetPropertyByName(ATypField)='Wartung'   then begin MW:=MW+S; JW:=JW+S end else
     if R.GetPropertyByName(ATypField)='Leistung'  then begin ML:=ML+S; JL:=JL+S end else
      begin MO:=MO+S; JO:=JO+S end;
  JS:=JS+S;
  MS:=MS+S;
 end;
  if MS>0 then
  begin
   SPN:=FloatToStrF(MN*100/MS,ffFixed,6,2)+' %';
   SPU:=FloatToStrF(MU*100/MS,ffFixed,6,2)+' %';
   SPW:=FloatToStrF(MW*100/MS,ffFixed,6,2)+' %';
   SPL:=FloatToStrF(ML*100/MS,ffFixed,6,2)+' %';
   SPO:=FloatToStrF(MO*100/MS,ffFixed,6,2)+' %';
  end;
  AReportList.Add(FloatToStrF(MN,ffFixed,6,2)
   +C_Vert+FloatToStrF(MU,ffFixed,6,2)
   +C_Vert+FloatToStrF(MW,ffFixed,6,2)
   +C_Vert+FloatToStrF(ML,ffFixed,6,2)
   +C_Vert+FloatToStrF(MO,ffFixed,6,2)
   +C_Vert+FloatToStrF(MS,ffFixed,6,2)
   +C_Vert+SPN+C_Vert+SPU+C_Vert+SPW+C_Vert+SPL+C_Vert+SPO);
end;

procedure TCollection_BasicObjects.GetAsCSV(AListe: TStrings; const ASeparator: char=';'; const ATextSeparator: boolean=false);
var I,P : Integer;
    Zeile : string;
    BasicObj : TKUO_BasicObj;
    BasicPrp : TKUO_Property;
    Value : string;
begin
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    if I=0 then
    begin
      for P:=0 to BasicObj.Properties.Count-1 do
      begin
        BasicPrp:=BasicObj.Properties.At(P);
        if P=0 then Zeile:=BasicPrp.Desc
               else Zeile:=Zeile+ASeparator+BasicPrp.Desc;
      end;
      AListe.Add(Zeile);
    end;

    for P:=0 to BasicObj.Properties.Count-1 do
    begin
      BasicPrp:=BasicObj.Properties.At(P);
      Value:=BasicPrp.Value;
      if BasicPrp.IsNumeric then
        Value:=ES_ReplaceChar(Value,'.',',');
      if BasicPrp.IsString and ATextSeparator then Value:='|'+Value+'|';

      if P=0 then Zeile:=Value
             else Zeile:=Zeile+ASeparator+Value;
    end;
    Aliste.Add(Zeile);
  end;
end;
{$EndRegion}

{$Region '--------------------------------- ListView bedienen ----------------------------'}
procedure TCollection_BasicObjects.GetListview(AListView: TListView; const ASearchObj: Pointer = nil);
var LI : TListItem;
    I,N : integer;
    F : TKUO_Property;
    A : TKUO_BasicObj;
    SearchObj : TKUO_BasicObj;
    Cancel : boolean;
begin
  Cancel:=false;

  if ASearchObj <> nil then SearchObj := ASearchObj
                       else SearchObj := nil;

  AListView.Items.BeginUpdate;
  AListView.Items.Clear;
  for I:=0 to Count-1 do
  begin
    A:=At(I);

    if (ASearchObj <> nil) and (not A.Matches(ASearchObj)) then Continue;

    LI:=AListView.Items.Add;
    for N:=0 to AListView.Columns.Count-1 do
    begin
      try
        if AListView.Columns[N].Caption <> '' then
        begin
          F:=A.Properties.FindProperty(AListView.Columns[N].Caption);
          if F<>nil then
            if N=0 then LI.Caption:=F.Value
                   else LI.SubItems.Add(F.Value);
        end;
      except end;
    end;
  end;
  AListView.Items.EndUpdate;
end;
{$EndRegion}

{$Region '---------------------------- Items bedienen ------------------------------------'}
procedure TCollection_BasicObjects.GetItems(AItems: TStrings; AProperty: string; ADupIgnore: boolean=false; ASort: boolean=false);
var F : TKUO_Property;
    A : TKUO_BasicObj;
    I : integer;
    SL : TStringList;
begin
  AItems.Clear;
  AItems.BeginUpdate;
  for I:=0 to Count-1 do
  begin
    A:=At(I);
    F:=A.Properties.FindProperty(AProperty);
    if F<>nil then
    begin
      if (not ADupIgnore) or (ADupIgnore and (AItems.IndexOf(F.Value)=-1)) then
        AItems.AddObject(F.Value,A);
    end;
  end;
  if ASort then
  begin
    SL:=TStringList.Create;
    try
      SL.Sorted:=true;
    	SL.Text:=AItems.Text;
    	AItems.Text:=SL.Text;
    finally
      SL.Free;
    end;
  end;
  AItems.EndUpdate;
end;
{$EndRegion}

{$Region '------------------------------------- Statistik ---------------------------------}
function TCollection_BasicObjects.CountCondition(const ACondition: string = ''): integer;
var I : integer;
    BasicObj : TKUO_BasicObj;
    Filter : TStringList;
begin
  Result := 0;
  if Count = 0 then Exit;
  if ACondition = EmptyStr then Exit(Count);

  Filter := TStringList.Create;
  Filter.Text := ACondition;
  try
    for I := 1 to Count - 1 do
    begin
      BasicObj := At(I);
      if (ACondition <> EmptyStr) and (not BasicObj.SatisfiesConditions(Filter)) then Continue;

      Result := Result + 1;
    end;
  finally
    Filter.Free;
  end;

end;

function TCollection_BasicObjects.Summary(AField: string; const ACondition: string = ''): integer;
var I : integer;
    BasicObj : TKUO_BasicObj;
    Filter : TStringList;
begin
  Result := 0;
  if Count = 0 then Exit;

  BasicObj := At(0);
  if not BasicObj.HasPropertyNumeric(AField) then Exit;

  Filter := TStringList.Create;
  try
    Filter.Text := ACondition;
    for I := 0 to Count - 1 do
    begin
      BasicObj := At(I);
      if (ACondition <> EmptyStr) and (not BasicObj.SatisfiesConditions(Filter)) then Continue;

      Result := Result + StrToInt(BasicObj.GetPropertyByName(AField));
    end;
  finally
    Filter.Free;
  end;

end;

function TCollection_BasicObjects.SummaryForCondition(ACondition, ASubCondition: TStringList; AValueCollection,AAmountField,AValueField: string): Double;
var I,J : integer;
    BasicObj : TKUO_BasicObj;
    SubBasicObj : TKUO_BasicObj;
    ValueCollection : TCollection_BasicObjects;
begin
  Result:= 0;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    if BasicObj.SatisfiesConditions(ACondition) then
    begin
      ValueCollection:=BasicObj.GetCollection(AValueCollection);
      if ValueCollection<>nil then
      for J:=0 to ValueCollection.Count-1 do
      begin
        SubBasicObj:=ValueCollection.At(J);
        if (ASubCondition=nil) or ((ASubCondition<>nil) and SubBasicObj.SatisfiesConditions(ASubCondition)) then
          Result:=Result+SubBasicObj.Properties.GetProperty(AAmountField)*SubBasicObj.Properties.GetProperty(AValueField);
      end;
    end;
  end;
end;

function  TCollection_BasicObjects.SummaryForCondition(ACondition: string; AValueCollection,AAmountField,AValueField: string): Double;
var ConditionList: TStringList;
begin
  ConditionList:=TStringList.Create;
  try
    ConditionList.Text:=ACondition;
    Result:=SummaryForCondition(ConditionList,nil,AValueCollection,AAmountField,AValueField);
  finally
    FreeAndNil(ConditionList);
  end;
end;

function TCollection_BasicObjects.SummaryForCondition(ACondition: TStringList; AValueField: string): Double;
var I : integer;
    BasicObj : TKUO_BasicObj;
begin
  Result:=0;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    if (ACondition=nil) or BasicObj.SatisfiesConditions(ACondition) then
    begin
      Result:=Result+BasicObj.Properties.GetProperty(AValueField);
    end;
  end;
end;

procedure TCollection_BasicObjects.SummaryForCategory(ACategoryField, ASummaryField: string; ACategoryCollection: TCollection_BasicObjects);
var I : integer;
    BasicObj, CategoryObj : TKUO_BasicObj;
    Value : double;
begin
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    CategoryObj:=ACategoryCollection.FindByPropertyValue(ACategoryField,BasicObj.GetPropertyByName(ACategoryField));
    if CategoryObj<>nil then
    begin
      Value:=CategoryObj.Properties.GetProperty(ASummaryField)+BasicObj.Properties.GetProperty(ASummaryField);
      CategoryObj.Properties.SetProperty(ASummaryField,Value);
    end;
  end;
end;

procedure TCollection_BasicObjects.FirstLastDate(ADateField: string; var AFirstDate, ALastDate: TDateTime);
var I : integer;
    BasicObj : TKUO_BasicObj;
    Datum : TDateTime;
begin
  AFirstDate:=0;
  ALastDate:=0;
  for I:=0 to Count-1 do
  begin
    BasicObj:=At(I);
    if I=0 then
    begin
      AFirstDate:=BasicObj.Properties.GetProperty(ADateField);
      ALastDate:=AFirstDate;
    end else begin
      Datum:=BasicObj.Properties.GetProperty(ADateField);
      if Datum<AFirstDate then
        AFirstDate:=BasicObj.Properties.GetProperty(ADateField);
      if Datum>ALastDate then
        ALastDate:=BasicObj.Properties.GetProperty(ADateField);
    end;
  end;
end;
{$EndRegion}

end.

