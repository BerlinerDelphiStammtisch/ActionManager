unit COLL_OBJ;

{$O+,F+,X+,I-,S-}

interface

uses {DELPHI}  SysUtils, WinTypes, WinProcs, Classes, Math;

type TDCEnumFunc = function(Item:pointer):boolean of object;
     TDCEnumProc = procedure(Item:pointer) of object;

type
     /// <summary>
     ///  Standard-Kollektion
     /// </summary>
     TColl = class(TObject)
     public
       FItems : TList;
       constructor Create;
       destructor Destroy; override;
       procedure PrepareLoad;
       function At(Index:integer):pointer;
       procedure AtDelete(Index:integer);
       procedure AtFree(Index:integer);
       procedure AtInsert(Index:integer; Item:pointer);
       procedure AtPut(Index:integer; Item:pointer);
       procedure Delete(Item:pointer);
       procedure DeleteAll;
       procedure FreeItem(Item:pointer); virtual;
       {FreeI entspricht dem altem Free}
       procedure FreeI(Item:pointer);
       procedure FreeAll;
       function IndexOf(Item:pointer):integer; virtual;
       /// <summary>
       ///   Zeiger eines Items am Ende anfügen
       /// </summary>
       function Insert(Item:pointer):boolean; virtual;
       procedure Exchange(Index1,Index2:integer);
       function ExchangeList(DestIndex:integer; List:TColl; var MovedIdx:integer):boolean;
       procedure Pack;
       function Count:integer;
       procedure Truncate(NewCount:integer);
       function FirstThat(CallRoutine:TDCEnumFunc):pointer;
       function LastThat(CallRoutine:TDCEnumFunc):pointer;
       procedure ForEach(CallRoutine:TDCEnumProc);
       /// <summary>
       ///   Collection in Instanz übertragen
       /// </summary>
       procedure AssignColl(C: TColl);
     end;

     /// <summary>
     ///  Pointer-Arithmetik für Standard-Kollektion
     /// </summary>
     TObjPointerColl = class(TColl)
     public
       procedure FreeItem(Item:pointer); override;
     end;

{$REGION '------------------------------- Sortierte Kollektion -----------------------------'}
type
     /// <summary>
     ///  nach Schlüssel-Pointer sortierte Kollektion
     /// </summary>
     TSortedColl = class(TColl)
     private
       FDuplicates : boolean;
     public
       constructor Create(AllowDups:boolean);
       function Search(Key: pointer; var Index:integer): boolean;
       function Insert(Item:pointer):boolean; override;
       function IndexOf(Item:pointer):integer; override;
       function FirstIndexOf(Item:pointer): integer;
       function Compare(Key1, Key2: pointer): integer; virtual;
       function KeyOf(Item: pointer): pointer; virtual;
       function AllowDups:boolean;
     end;

     /// <summary>
     ///  nach Schlüssel-String sortierte Kollektion
     /// </summary>
     TSortedByStringColl = class(TColl)
     private
       FDuplicates : boolean;
     public
       constructor Create(AllowDups:boolean);
       function Insert(Item:pointer):boolean; override;
       function IndexOf(Item:pointer):integer; override;
       function Search(const Key:string; var Index:integer): boolean;
       {Compare und KeyOf müssen überschrieben werden}
       function Compare(const Key1,Key2:string): integer; virtual;
       function KeyOf(Item:pointer):string; virtual;
       function AllowDups:boolean;
     end;

     /// <summary>
     ///  16-Bit-Integer-Object
     /// </summary>
     TInt16Obj = class(TObject)
     public
       Wert : INT16;
       constructor Create(AWert:INT16);
     end;

     /// <summary>
     ///  Sortierte Int16-Kollektion
     /// </summary>
     TSortedInt16Coll = class(TSortedColl)
       function KeyOf(Item: pointer): pointer; override;
       function Compare(Key1, Key2: Pointer): Integer; override;
       {holt eine erste freie Nummer, auch aus Zwischenräumen}
       function GetFirstFreeInt:Int16;
       function GetNextFreeInt:Int16;
       function HasValue(Value:Int16):integer;
       function GetAsString(Del:char):string;
     end;

type INT16       = smallint;
     INTB16      = smallint;
     INT32       = integer;
     WORD16      = word;
     WORD32      = longword;

     /// <summary>
     ///  vorzeichenhaftes 32-Bit-Integer-Object
     /// </summary>
     TInt32Obj = class(TObject)
       Wert : INT32;
       constructor Create(AWert:INT32);
     end;

     /// <summary>
     ///  vorzeichenloses 32-Bit-Integer-Object
     /// </summary>
type TWORD32Object = class(TObject)
     public
       ID : WORD32;
       constructor Create(AID:WORD32);
     end;

     /// <summary>
     ///  Sortierte UInt32-Kollektion
     /// </summary>
     TSortedWORD32Coll = class(TSortedColl)
       function KeyOf(Item: pointer): pointer; override;
       function Compare(Key1, Key2: Pointer): Integer; override;
       function GetFirstFreeWORD32:WORD32;
     end;

     /// <summary>
     ///  32-Bit Fließkommazahl-Objekt
     /// </summary>
     TRealObj = class(TObject)
       Wert : real;
       constructor Create(AWert : real);
     end;

     /// <summary>
     ///  Sortierte Real-Kollektion
     /// </summary>
     TSortedRealColl = class(TSortedColl)
       function KeyOf(Item: pointer): pointer; override;
       function Compare(Key1, Key2: Pointer): Integer; override;
       {}
       function Median:real;
       function Percentile(Percent: double): double;
     end;

     /// <summary>
     ///  Real-Record
     /// </summary>
     TRealRecord = class(TObject)
       Value      : real;
       DataRecord : pointer;
       constructor Create(AValue:real; ADataRecord:pointer);
     end;

     /// <summary>
     ///  Sortierte Real-Record-Kollektion
     /// </summary>
     TSortedRealRecords = class(TSortedColl)
       function KeyOf(Item: pointer): pointer; override;
       function Compare(Key1, Key2: Pointer): Integer; override;
     end;
{$ENDREGION}

{$REGION '------------------------ Sortierte ID,Index-Kollektion --------------------------'}
type TIDIndexObj = class(TObject)
       ID,Index : integer;
       constructor Create(AID,AIndex:integer);
     end;

type TSortedIDIndexColl = class(TSortedColl)
       function KeyOf(Item: pointer): pointer; override;
       function Compare(Key1, Key2: Pointer): Integer; override;
     end;
{$ENDREGION}

implementation

{$REGION '-------------------------------- Standard-Kollektion ---------------------------'}
constructor TColl.Create;
begin
  inherited Create;
  FItems:=TList.Create;
end;

destructor TColl.Destroy;
begin
  if FItems<>nil then begin
    FreeAll;
    FItems.Free;
  end;
  inherited Destroy;
end;

procedure TColl.PrepareLoad;
begin
  FItems:=TList.Create;
end;

function TColl.At(Index:integer):pointer;
begin
  Result:=FItems[Index];
end;

procedure TColl.AtDelete(Index:integer);
begin
  FItems.Delete(Index);
end;

procedure TColl.AtFree(Index:integer);
var Item:pointer;
begin
  Item:=At(Index);
  AtDelete(Index);
  FreeItem(Item);
end;

procedure TColl.AtInsert(Index:integer; Item:pointer);
begin
  FItems.Expand.Insert(Index,Item);
end;

procedure TColl.AtPut(Index:integer; Item:pointer);
var OldItem:pointer;
begin
  OldItem:=FItems[Index];
  FreeItem(OldItem);
  FItems[Index]:=Item;
end;

procedure TColl.Delete(Item:pointer);
begin
  FItems.Remove(Item);
end;

procedure TColl.DeleteAll;
begin
  FItems.Clear;
end;

procedure TColl.FreeItem(Item:pointer);
begin
  if Item<>nil then TObject(Item).Free;
end;

procedure TColl.FreeI(Item:pointer);
var Index:integer;
begin
  Index:=FItems.IndexOf(Item);
  if (Index <> -1) then AtFree(Index);
end;

procedure TColl.FreeAll;
var I:integer;
begin
  for I:=0 to FItems.Count-1 do FreeItem(FItems[I]);
  FItems.Clear;
end;

function TColl.IndexOf(Item:pointer):integer;
begin
  IndexOf:=FItems.IndexOf(Item);
end;

function TColl.Insert(Item:pointer):boolean;
begin
  AtInsert(FItems.Count,Item); Result:=true;
end;

procedure TColl.Exchange(Index1,Index2:integer);
begin
  FItems.Exchange(Index1,Index2);
end;

function TColl.ExchangeList(DestIndex:integer; List:TColl; var MovedIdx:integer):boolean;
var AListEntry : TInt32Obj;
    NewList    : TColl;
    I          : integer;
    NewItems   : TList;
    MoveAftLast: boolean;
  function IsInList(SList:TColl; Idx:integer):boolean;
  var I          : integer;
      AListEntry : TInt32Obj;
  begin
    Result:=false;
    for I:=0 to SList.Count-1 do begin
      AListEntry:=SList.At(I);
      if AListEntry.Wert=Idx then begin
        Result:=true;
        exit;
      end;
    end;
  end;
begin
  Result:=false;
  if (List.Count=0) or (FItems.Count=0) then exit;
  if (DestIndex<0) then exit;
  {Wenn Idx größer als Liste, wird nach dem letztem Eintrag verschoben}
  if DestIndex>FItems.Count-1 then begin
    DestIndex:=FItems.Count-1;
    MoveAftLast:=true;
  end else MoveAftLast:=false;
  {wenn DestIndex in MoveList, dann nichts ausführen}
  if IsInList(List,DestIndex) then exit;
  {Neue Liste aufbauen}
  NewList:=TColl.Create;
  try
    {erstmal alles, was vor Dest kommt und nicht in der MoveList ist}
    for I:=0 to DestIndex-1 do
      if not IsInList(List,I) then NewList.Insert(TInt32Obj.Create(I));
    {evtl. letzten Eintrag aufnehmen}
    if MoveAftLast and (DestIndex=FItems.Count-1) then
      NewList.Insert(TInt32Obj.Create(DestIndex));
    {nun die aus der MoveList ranhängen}
    MovedIdx:=NewList.Count;
    for I:=0 to List.Count-1 do begin
      AListEntry:=List.At(I);
      NewList.Insert(TInt32Obj.Create(AListEntry.Wert));
    end;
    {und alle übrigen, die noch nicht übernommen wurden wiederum daran hängen}
    for I:=0 to FItems.Count-1 do
      if not IsInList(NewList,I) then NewList.Insert(TInt32Obj.Create(I));
    {Neue Reihenfolge ist nun fertig - übertragen der pointer}
    NewItems:=TList.Create;
    for I:=0 to NewList.Count-1 do begin
      AListEntry:=NewList.At(I);
      NewItems.Add(FItems[AListEntry.Wert]);
    end;
    {Alte FList freigeben ohne Freigabe der Pointer}
    FItems.Clear;
    FItems.Free;
    {und neu zuweisen}
    FItems:=NewItems;
  finally
    NewList.Free;
  end;
  Result:=true;
end;

procedure TColl.Pack;
begin
  FItems.Pack;
end;

function TColl.Count:integer;
begin
  Result:=FItems.Count;
end;

procedure TColl.Truncate(NewCount:integer);
begin
  while FItems.Count>NewCount do
    AtFree(FItems.Count-1);
end;

function TColl.FirstThat(CallRoutine:TDCEnumFunc):pointer;
var I : integer;
begin
  FirstThat:=nil;
  if FItems.Count=0 then exit;
  for I:=0 to FItems.Count-1 do
    if CallRoutine(FItems[I]) then begin
      FirstThat:=FItems[I];
      exit;
    end;
end;

function TColl.LastThat(CallRoutine:TDCEnumFunc):pointer;
var I:integer;
begin
  LastThat:=nil;
  if FItems.Count=0 then exit;
  I:=FItems.Count-1;
  while I>=0 do begin
    if CallRoutine(FItems[I]) then begin
      LastThat:=FItems[I];
      exit;
    end;
    dec(I);
  end;
end;

procedure TColl.ForEach(CallRoutine:TDCEnumProc);
var I:integer;
begin
  for I:=0 to FItems.Count-1 do CallRoutine(FItems[I]);
end;

procedure TColl.AssignColl(C: TColl);
var I : integer;
begin
 DeleteAll;
 for I:=0 to C.Count-1 do Insert(C.At(I));
end;

procedure TObjPointerColl.FreeItem(Item:pointer);
begin
  {Keine Freigabe des Objektes}
end;

{$ENDREGION}

{$REGION '---------------------------- Sortierte Kollektion -----------------------------'}
constructor TSortedColl.Create(AllowDups:boolean);
begin
  inherited Create;
  FDuplicates:=AllowDups;
end;

function TSortedColl.Search(Key:pointer; var Index:integer): boolean;
var L, H, I, C: integer;
begin
  Search:=false;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(KeyOf(FItems[I]), Key);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        if not FDuplicates then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TSortedColl.Compare(Key1, Key2: pointer): integer;
begin
  Compare:=0;
end;

function TSortedColl.KeyOf(Item: pointer): pointer;
begin
  KeyOf:=Item;
end;

function TSortedColl.AllowDups:boolean;
begin
  Result:=FDuplicates;
end;

function TSortedColl.Insert(Item:pointer):boolean;
var I:integer;
begin
  {FDuplicates darf nicht nach vorn, da Search die Position ermittelt}
  if not Search(KeyOf(Item), I) or FDuplicates then begin
    AtInsert(I, Item);
    Result:=true;
  end
  else begin
    Result:=false;
    TObject(Item).Free;
  end;
end;

function TSortedColl.IndexOf(Item:pointer): integer;
var I:integer;
begin
  IndexOf:=-1;
  if Search(KeyOf(Item), I) then
  begin
    if FDuplicates then
      while (I<Count) and (Item <> FItems[I]) do Inc(I);
    if I<Count then IndexOf:=I;
  end;
end;

function TSortedColl.FirstIndexOf(Item:pointer): integer;
var I:integer;
begin
  Result:=-1;
  if Search(KeyOf(Item), I) then Result:=I;
end;

constructor TSortedByStringColl.Create(AllowDups:boolean);
begin
  inherited Create;
  FDuplicates:=AllowDups;
end;

function TSortedByStringColl.Search(const Key:string; var Index:integer): boolean;
var L, H, I, C: integer;
begin
  Search:=false;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := Compare(KeyOf(FItems[I]), Key);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Search := True;
        if not FDuplicates then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TSortedByStringColl.Compare(const Key1,Key2:string):integer;
begin
  Compare:=0;
end;

function TSortedByStringColl.Insert(Item:pointer):boolean;
var I:integer;
begin
  {FDuplicates darf nicht nach vorn, da Search die Position ermittelt}
  if not Search(KeyOf(Item), I) or FDuplicates then begin
    AtInsert(I, Item);
    Result:=true;
  end
  else begin
    Result:=false;
    TObject(Item).Free;
  end;
end;

function TSortedByStringColl.IndexOf(Item:pointer):integer;
var I:integer;
begin
  IndexOf:=-1;
  if Search(KeyOf(Item), I) then
  begin
    if FDuplicates then
      while (I<Count) and (Item <> FItems[I]) do Inc(I);
    if I<Count then IndexOf:=I;
  end;
end;

function TSortedByStringColl.KeyOf(Item:pointer):string;
begin
  Result:='';
end;

function TSortedByStringColl.AllowDups:boolean;
begin
  Result:=FDuplicates;
end;

{$ENDREGION}

{$REGION '------------------------ Sortierte Integer-Kollektion -----------------------'}
constructor TInt16Obj.Create(AWert:INT16);
begin
  inherited Create;
  Wert:=AWert;
end;

function TSortedInt16Coll.KeyOf(Item: pointer):pointer;
begin
  Result:=@TInt16Obj(Item).Wert;
end;

function TSortedInt16Coll.Compare(Key1, Key2: Pointer): Integer;
var A,B:INT16;
begin
  move(Key1^,A,sizeOf(A));
  move(Key2^,B,sizeOf(B));
  if A<B then Result:=-1
    else if A>B then Result:=1
      else Result:=0;
end;

function TSortedInt16Coll.GetFirstFreeInt:Int16;
var Founded,LastUsed,I : integer;
    AnRecId            : TInt16Obj;
begin
  Founded:=0; LastUsed:=0;
  for I:=0 to Count-1 do begin
    AnRecID:=At(I);
    if AnRecID.Wert>LastUsed+1 then begin
      Founded:=LastUsed+1;
      break;
    end;
    LastUsed:=AnRecID.Wert;
  end;
  if Founded=0 then Founded:=LastUsed+1;
  Result:=Founded;
end;

function TSortedInt16Coll.GetNextFreeInt:Int16;
var AnRecId : TInt16Obj;
begin
  if Count=0 then Result:=1
  else begin
    AnRecID:=At(Count-1);
    Result:=AnRecID.Wert+1;
  end;
end;

function TSortedInt16Coll.HasValue(Value:Int16):integer;
{könnte man auch über Search eleganter lösen}
var AnRecId : TInt16Obj;
    I       : integer;
begin
  Result:=-1;
  for I:=0 to Count-1 do begin
    AnRecID:=At(I);
    if AnRecID.Wert=Value then begin
      Result:=I;
      exit;
    end;
  end;
end;

function TSortedInt16Coll.GetAsString(Del:char):string;
var AnRecId : TInt16Obj;
    I       : integer;
begin
  Result:='';
  for I:=0 to Count-1 do begin
    AnRecID:=At(I);
    if I>0 then Result:=Result+Del;
    Result:=Result+IntToStr(AnRecID.Wert);
  end;
end;

constructor TInt32Obj.Create(AWert:Int32);
begin
  inherited Create;
  Wert:=AWert;
end;

constructor TWORD32Object.Create(AID:WORD32);
begin
  inherited Create;
  ID:=AID;
end;

function TSortedWORD32Coll.KeyOf(Item: pointer): pointer;
begin
  Result:=@TWORD32Object(Item).ID;
end;

function TSortedWORD32Coll.Compare(Key1, Key2: Pointer): Integer;
var A,B:WORD32;
begin
  move(Key1^,A,sizeOf(A));
  move(Key2^,B,sizeOf(B));
  if A<B then Result:=-1
    else if A>B then Result:=1
      else Result:=0;
end;

function TSortedWORD32Coll.GetFirstFreeWORD32:WORD32;
var Founded,LastUsed : WORD32;
    I                : integer;
    AWORD32          : TWORD32Object;
begin
  Founded:=0; LastUsed:=0;
  for I:=0 to Count-1 do begin
    AWORD32:=At(I);
    if AWORD32.ID>LastUsed+1 then begin
      Founded:=LastUsed+1;
      break;
    end;
    LastUsed:=AWORD32.ID;
  end;
  if Founded=0 then Founded:=LastUsed+1;
  Result:=Founded;
end;

{$ENDREGION}

{$REGION '------------------------ Sortierte Real-Kollektion -----------------------'}
constructor TRealObj.Create(AWert:real);
begin
  inherited Create;
  Wert:=AWert;
end;

function TSortedRealColl.KeyOf(Item: pointer):pointer;
begin
  Result:=@TRealObj(Item).Wert;
end;

function TSortedRealColl.Compare(Key1, Key2: Pointer): Integer;
var A,B:real;
begin
  move(Key1^,A,sizeOf(A));
  move(Key2^,B,sizeOf(B));
  if A<B then Result:=-1
    else if A>B then Result:=1
      else Result:=0;
end;

function TSortedRealColl.Median:real;
var P1,P2 : integer;
begin
  if Count=0 then begin
    Result:=0;
    exit;
  end
  else if Count=1 then begin
    Result:=TRealObj(At(0)).Wert;
    exit;
  end
  else begin
    { Mittleren Wert der Reihe finden...}
    if odd(Count) then begin
      {bei ungerader Anzahl --> Median(1,2,3,4,5) = 3 }
      {3 div 2 -> 1   Index: 1
       5 div 2 -> 2   Index: 2
       7 div 2 -> 3   Index: (0,1,2) 3 (4,5,6) }
      P1:=(Count div 2);
      result:=TRealObj(At(P1)).Wert;
    end
    else begin
      {bei gerader Anzahl --> Median(1,2,3,4,5,6) = (3+4)/2 = 3,5}
      {2 div 2 -> 1   Index: 0 - 1
       4 div 2 -> 2   Index: 1 - 2
       6 div 2 -> 3   Index: (0,1) 2 - 3 (4,5) }
      P1:=(Count div 2)-1;
      P2:=P1+1;
      result:=(TRealObj(At(P1)).Wert+TRealObj(At(P2)).Wert) / 2;
    end;
  end;
end;

function TSortedRealColl.Percentile(Percent: double): double;
var x: double;
    i, N: LongInt;
begin
  Result:=0;
  if (Percent < 0.0) or (Percent > 100.0) then Exit;
  N:=Count;
  Percent:=Percent / 100.0;
  x := Int(N * Percent);
  if x = (N  * Percent) then begin
    i := Pred(Trunc(x));
    if (i>=0) and (i<Count-1) then
      Result:=(TRealObj(At(i)).Wert +TRealObj(At(i+1)).Wert) /2;
  end else begin
    i := Pred(Round(N * Percent));
    if (i>=0) and (i<Count) then
      Result := TRealObj(At(i)).Wert;
  end;
end;

{$ENDREGION}

{$REGION '---------------------- Sortierte Real-Record-Kollektion ---------------------'}
constructor TRealRecord.Create(AValue:real; ADataRecord:pointer);
begin
  Value:=AValue;
  DataRecord:=ADataRecord;
end;

function TSortedRealRecords.KeyOf(Item: pointer):pointer;
begin
  Result:=@TRealRecord(Item).Value;
end;

function TSortedRealRecords.Compare(Key1, Key2: Pointer):Integer;
var A,B:real;
begin
  move(Key1^,A,sizeOf(A));
  move(Key2^,B,sizeOf(B));
  Result:=CompareValue(A,B);
end;

{$ENDREGION}

{$REGION '------------------------ Sortierte ID, Index-Kollektion -----------------------'}
constructor TIDIndexObj.Create(AID,AIndex:integer);
begin
  ID:=AID;
  Index:=AIndex;
end;

function TSortedIDIndexColl.KeyOf(Item: pointer): pointer;
begin
  Result:=@TIDIndexObj(Item).ID;
end;

function TSortedIDIndexColl.Compare(Key1, Key2: Pointer): Integer;
var A,B:integer;
begin
  move(Key1^,A,sizeOf(A));
  move(Key2^,B,sizeOf(B));
  if A<B then Result:=-1
    else if A>B then Result:=1
      else Result:=0;
end;

{$ENDREGION}
end.
