unit StringList_Extended;

{============================================================================}
{================ Funktionen zum Bearbeiten von Stringlisten ================}
{============================================================================}

interface

uses SysUtils, Classes, String_Extended;

{StringListen-Operationen }
procedure ES_SplitStringLogical(InStr: string; AList: TStrings);
procedure ES_GetListFromString(const InStr,Linker: string; AList: TStrings);
function  ES_SetListToString(AList: TStrings; const Linker: string; WithLeer: boolean): string;
function  ES_DoubleInList(AList: TStrings): string;
function  ES_CountListDiff(AList: TStrings): integer;
procedure ES_ExcludeListFromList(AFullList, AExList: TStrings);
procedure ES_StringToListByLength(AList: TStringList; AString: string; ALength: integer);
procedure ES_StringToListByLengthOnSpace(AList: TStringList; AString: string; ALength: integer; AWithClear: boolean);
procedure ES_StringToListByLeadingString(const InStr,Linker,Finisher: string; AList: TStrings);
function  ES_ListLongestEntry(TheList: TStringList): integer;
procedure ES_CommaSeparatedToStringList(AList: TStrings; const Value:string; const Separator: Char=',');
function  ES_CheckListsEqual(AList1,AList2: TStrings; var CompResult: integer): boolean;
procedure ES_InsertList(AList,AInsertList: TStrings; AInsertPoint: string);
procedure ES_ReformatListByLength(AList: TStrings; ALength: integer);
function  ES_GetListPosRepeat(AList: TStringList; ASearch: string; ARepeat: integer): integer;
procedure ESL_DeleteMultis(AList: TStrings);

{Connection-String}
function ES_ConnectionGetDatasourceFileName(AConStr: string): string;
function ES_ConnectionGetMDBName(AConStr: string): string;

implementation

const
      CES_Leer1        = ' ';
      CES_Semikolon    = ';';
      CES_Punkt        = '.';
      CES_ExtMdb       = 'mdb';

{$Region '------------------------- StringListen -------------------------------------'}
procedure ES_SplitStringLogical(InStr: string; AList: TStrings);
var ExpList : TStringList;
    Work,Upper : string;
    I,P,Index : integer;
const CAnd = ' AND ';
      COr  = ' OR ';
begin
  Work:=InStr;
  Upper:=UpperCase(InStr);
  ExpList:=TStringList.Create;
  try
    ExpList.Add(CAnd);
    ExpList.Add(COr);
    P:=ES_GetFirstPosition(Upper,ExpList,Index);
    {keine logischen Ausdrücke}
    if P=0 then
    begin
      AList.Add(Work);
      Exit;
    end;
    {es gibt logische Ausdrücke}
    while P>0 do
    begin
      AList.Add(Trim(Copy(Work,1,P-1)));
      AList.Add(ExpList[Index]);
      I:=P+Length(ExpList[Index]);
      Work:=Trim(Copy(Work,I,Length(Work)-I+1));
      Upper:=Trim(Copy(Upper,I,Length(Upper)-I+1));
      P:=ES_GetFirstPosition(Upper,ExpList,Index);
    end;
    AList.Add(Work);
  finally
    FreeAndNil(ExpList);
  end;
end;

procedure ES_GetListFromString(const InStr,Linker: string; AList: TStrings);
var Work : string;
begin
  if AList<>nil then
  begin
    AList.Clear;
    Work:=InStr;
    while pos(Linker,Work)>0 do
    begin
      AList.Add(Trim(ES_BeforeSubString(Work,Linker,false)));
      Work:=ES_AfterSubString(Work,Linker,false);
    end;
    if Work<>EmptyStr then
      AList.Add(Trim(Work));
  end;
end;

function ES_SetListToString(AList: TStrings; const Linker: string; WithLeer: boolean): string;
var I : integer;
begin
  Result:=Trim(AList[0]);
  for I:=1 to Pred(AList.Count) do
  begin
    if WithLeer then Result:=Result+CES_Leer1+Linker+CES_Leer1+AList[I]
                else Result:=Result+Linker+AList[I];
  end;
end;

function ES_DoubleInList(AList: TStrings): string;
var I : integer;
    S : string;
    L : TStringList;
begin
 Result:=EmptyStr;
 S:=EmptyStr;
 L:=TStringList.Create;
 try
   L.Text:=AList.Text;
   L.Sort;
   for I:=0 to Pred(L.Count) do
   begin
     if S=L[I] then
     begin
       Result:=S;
       Break;
     end;
     S:=L[I];
   end;
 finally
   FreeAndNil(L);
 end;
end;

function  ES_CountListDiff(AList: TStrings): integer;
var TmpList : TStringList;
    I       : integer;
begin
  TmpList:=TStringList.Create;
  TmpList.Sorted:=true;
  TmpList.Duplicates:=dupIgnore;
  try
    for I:=0 to AList.Count-1 do TmpList.Add(AList[I]);
    Result:=TmpList.Count;
  finally
    FreeAndNil(TmpList);
  end;
end;

procedure ES_ExcludeListFromList(AFullList,AExList: TStrings);
var I : integer;
begin
  I:=0;
  while I<AFullList.Count do
    if AExList.IndexOf(AFullList[I])>-1 then AFullList.Delete(I) else inc(I);
end;

procedure ES_StringToListByLength(AList: TStringList; AString: string; ALength: integer);
var Work : string;
begin
 AList.Clear;
 if Length(AString)<=ALength then AList.Add(AString) else
 begin
  Work:=AString;
  while Length(Work)>ALength do
  begin
   AList.Add(Copy(Work,1,ALength));
   Work:=Copy(Work,ALength+1,Length(Work)-ALength);
  end;
  if Work<>EmptyStr then AList.Add(Work);
 end;
end;

procedure ES_StringToListByLengthOnSpace(AList: TStringList; AString: string; ALength: integer; AWithClear: boolean);
var Work : string;
begin
  if AWithClear then AList.Clear;
  if Length(AString)<=ALength then AList.Add(AString) else
  begin
    Work:=AString;
    while Length(Work)>ALength do
    begin
      while Copy(Work,Length(Work),1)<>CES_Leer1 do Work:=Copy(Work,1,Length(Work)-1);
      AList.Add(Copy(Work,1,ALength));
      Work:=Copy(Work,ALength+1,Length(Work)-ALength);
    end;
    if Work<>EmptyStr then AList.Add(Work);
  end;
end;

procedure ES_StringToListByLeadingString(const InStr,Linker,Finisher: string; AList: TStrings);
var Work,Insert: string;
    Start : integer;
begin
  AList.Clear;
  Work:=InStr;
  while Pos(Linker,Work)>0 do
  begin
    if Pos(Finisher,Work)>Pos(Linker,Work) then
    begin
      Insert:=Copy(Work,Pos(Linker,Work),Pos(Finisher,Work)+Length(Finisher)-Pos(Linker,Work));
      AList.Add(Insert);
      Start:=Pos(Insert,Work)+Length(Insert);
      Work:=Copy(Work,Start,Length(Work)-Start+1);
    end else begin
    Start:=Pos(Linker,Work)+Length(Linker);
    Work:=Copy(Work,Start,Length(Work)-Start);
    end;
  end;
end;

function ES_ListLongestEntry(TheList: TStringList): integer;
var I : integer;
    N : string;
begin
 Result:=0;
 for I:=0 to TheList.Count-1 do
 begin
  N:=TrimRight(TheList[I]);
  if Length(N)>Result then Result:=Length(N);
 end;
end;

procedure ES_CommaSeparatedToStringList(AList: TStrings; const Value:string; const Separator: Char=',');
var
  iStart,
  iEnd,
  iQuote,
  iPos,
  iLength : integer ;
  sTemp : string ;
begin
  iQuote := 0;
  iPos := 1 ;
  iLength := Length(Value) ;
  AList.Clear ;
  while (iPos <= iLength) do
  begin
    iStart := iPos ;
    iEnd := iStart ;
    while ( iPos <= iLength ) do
    begin
      if Value[iPos] = '"' then  {do not localize}
      begin
        inc(iQuote);
      end;
      if Value[iPos] = Separator then  {do not localize}
      begin
        if iQuote <> 1 then
        begin
          break;
        end;
      end;
      inc(iEnd);
      inc(iPos);
    end ;
    sTemp := Trim(Copy(Value, iStart, iEnd - iStart));
    {was soll das? wir wollen alle Einträge haben
    if Length(sTemp) > 0 then
    begin
    }
      AList.Add(sTemp);
    {
    end;
    }
    iPos := iEnd + 1 ;
    iQuote := 0 ;
  end ;
end;

function  ES_CheckListsEqual(AList1,AList2: TStrings; var CompResult: integer): boolean;
var I : integer;
begin
  CompResult:=0;
  {1. List1 enthält mehr Einträge als List2?}
  for I:=0 to Pred(AList1.Count) do
  begin
    if AList2.IndexOf(AList1[I])=-1 then
    begin
      CompResult:=1;
      Break;
    end;
  end;
  {2. List2 enthält mehr Einträge als List1?}
  for I:=0 to Pred(AList2.Count) do
  begin
    if AList1.IndexOf(AList2[I])=-1 then
    begin
      CompResult:=CompResult+2;
      Break;
    end;
  end;
  Result:=CompResult=0;
end;

procedure ES_InsertList(AList,AInsertList: TStrings; AInsertPoint: string);
var P : integer;
    I : integer;
begin
  P:=AList.IndexOf(AInsertPoint);
  if P=0 then Exit;
  if AInsertList.Count=0 then Exit;
  AList[P]:=AInsertList[0];
  for I:=Pred(AInsertList.Count) downto 1 do
  begin
    AList.Insert(Succ(P),AInsertList[I]);
  end;
end;

{Stringliste wird auf maximale Länge umformatiert}
procedure ES_ReformatListByLength(AList: TStrings; ALength: integer);
var I : integer;
    S,B,A : string;
    TmpList : TStrings;
begin
  TmpList:=TStringList.Create;
  try
    for I := 0 to Pred(AList.Count) do
    begin
      S:=AList[I];
      if Length(S)>ALength then
      begin
        A:=S;
        while Length(A)>ALength do
        begin
          ES_SplitAtLastChar(S,CES_Leer1,B,A,ALength);
          TmpList.Add(B);
          S:=A;
        end;
        if Length(A)>0 then
          if I=Pred(AList.Count) then TmpList.Add(A)
                                 else if AList[I+1]=EmptyStr then TmpList.Add(A)
                                                             else AList[I+1]:=A+CES_Leer1+AList[I+1];
      end else TmpList.Add(S);
    end;
    {Zuweisen des neu formatierten Textes}
    AList.Text:=TmpList.Text;
  finally
    FreeAndNil(TmpList);
  end;
end;

function  ES_GetListPosRepeat(AList: TStringList; ASearch: string; ARepeat: integer): integer;
var I,C : integer;
begin
  C:=0;
  Result:=-1;
  for I:=0 to AList.Count - 1 do
  begin
    if AList[I]=ASearch then
    begin
      inc(C);
      if C=ARepeat then
      begin
        Result:=I;
        Exit;
      end;
    end;
  end;
end;

{mehrfache Einträge aus der Liste löschen. Sie muss schon sortiert sein!!}
procedure ESL_DeleteMultis(AList: TStrings);
var List: TStringList;
begin
  List:=TStringList.Create;
  List.Sorted:=true; List.Duplicates:=dupIgnore;
  try
    List.AddStrings(AList);
    AList.Clear;
    AList.AddStrings(List);
  finally
    List.Free;
  end;
end;
{$EndRegion}

function ES_ConnectionGetDatasourceFileName(AConStr: string): string;
var L : TStringList;
begin
  Result:=EmptyStr;
  L:=TStringList.Create;
  try
    ES_GetListFromString(AConStr,CES_Semikolon,L);
    Result:=L.Values['Data Source'];
  finally
    FreeAndNil(L);
  end;
end;

function ES_ConnectionGetMDBName(AConStr: string): string;
var S : string;
begin
  Result:='';
  S:=ES_ConnectionGetDatasourceFileName(AConStr);
  if UpperCase(ExtractFileExt(S))=UpperCase(CES_Punkt+CES_ExtMdb) then Result:=S;
end;

end.


