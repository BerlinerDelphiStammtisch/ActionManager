unit Obj_TableStructure;

interface

uses {DELPHI} SysUtils, Windows, Classes, Types, Data.DB,
              COLL_OBJ, Obj_TableRecord;

const       CGDO_ftUnknown          = ftUnknown;

type
     /// <summary>
     ///   Komponente: Struktureintrag für ein Datenfeld
     ///  Beschreibung: Name,Typ,Länge
     /// </summary>
     TDBO_TableStructureField = class(TObject)
       public
       FldName   : string;
       FldType   : char;
       FldUnits1 : byte;
       FldUnits2 : byte;
       FldCnt    : byte;
       FldOpt    : longint;
       FldLong   : string;
       FldDict   : string;
       FldGroup  : string;
       FldUnit   : string;
       FldFormat : string;
       FldDefault: string;
       FldBDETyp : TFieldType;
       constructor Create(const AFldName:string; AFldType:char;
                          AFldUnits1, AFldUnits2, AFldCnt:integer;
                          AFldOpt:longint;
                          const AFldLong,AFldDict,AFldGroup,AFldUnit,
                          AFldFormat,AFldDefault:string;
                          AFldBDETyp:TFieldType);
       constructor CreateSimple(const AFldName:string; AFldType:char; AFldUnits1,AFldUnits2:integer; AFldOpt:integer);
       constructor CreateSimpleLong(const AFldName,AFldLong:string; AFldType:char; AFldUnits1,AFldUnits2:integer; AFldOpt:integer);
       constructor CreateSimpleLongD(const AFldName,AFldLong,AFldDict:string; AFldType:char;
                          AFldUnits1,AFldUnits2:integer; AFldOpt:integer);
       procedure Assign(ASource: TDBO_TableStructureField);
       function HasIndex: boolean;
       function HasUniqueIndex: boolean;
       function HasNotNull: boolean;
     end;

     /// <summary>
     ///   Sammlung von Struktureinträgen
     /// </summary>
     TDBO_TableStructureEntries = class(TColl) {of TDBO_TableStructureField}
     public
       constructor CreateAssign(Source:TDBO_TableStructureEntries);
       function FindField(const FieldName:string; var FldIndex:integer):TDBO_TableStructureField; overload;
       function FindField(const FieldName:string):TDBO_TableStructureField; overload;
       function FindLong(const FieldLong:string; var FldIndex:integer):TDBO_TableStructureField;
       function HasField(const FieldName:string):integer;
       function HasFieldLong(const FieldLong:string):integer;
       procedure Assign(ASource: TDBO_TableStructureEntries);
       {}
       procedure AssignToDFTabDef(Dest: TGDO_DF_TabDef);
       procedure AppendFieldsFromDFTabDef(DFTabDef: TGDO_DF_TabDef);
       procedure GetBDETypesFromDFTabDef(DFTabDef: TGDO_DF_TabDef; const AdjustCFields:boolean=false);
       function  GetIdentField(var AnIdentField: string; AOption: longint): boolean;
       {}
     end;

     /// <summary>
     ///   Komponente: Struktur einer Tabelle
     /// </summary>
     /// <remarks>
     ///   enthält Metadaten zur Tabellenstruktur,
     ///
     /// </remarks>
     TDBO_TableStructure = class(TObject)
     public
       Descriptor,
       DisplayName : string;
       StrOptions  : longint;
       StrTyp      : longint;     {TAB_TYPE}
       StrTrc      : byte;        {TAB_TRC}
       Structure   : TDBO_TableStructureEntries;
       StrCheckOK  : boolean;        {minimale Strukturanforderungen OK}
       constructor Create(
         const ADescriptor,ADisplayName:string; AOptions:longint;
         AType:longint; ATRC:byte; const AInv:string;
         AStrDes:TDBO_TableStructureEntries);
       constructor CreateEmpty;
       destructor Destroy; override;
       procedure Assign(ASource: TDBO_TableStructure);
       function GetSelect: string;
     end;

implementation

uses Math_Extended;

{$REGION '--------------------------- Komponente StrukturFelder ---------------------------------'}

constructor TDBO_TableStructureField.Create(const AFldName:string; AFldType:char;
                                    AFldUnits1, AFldUnits2, AFldCnt:integer;
                                    AFldOpt:longint;
                                    const AFldLong,AFldDict,AFldGroup,AFldUnit,
                                    AFldFormat,AFldDefault:string;
                                    AFldBDETyp:TFieldType);
begin
  inherited Create;
  FldName:=AFldName;
  FldType:=AFldType;
  FldUnits1:=AFldUnits1;
  FldUnits2:=AFldUnits2;
  FldCnt:=AFldCnt;
  FldOpt:=AFldOpt;
  FldLong:=AFldLong;
  FldDict:=AFldDict;
  FldGroup:=AFldGroup;
  FldUnit:=AFldUnit;
  FldFormat:=AFldFormat;
  FldDefault:=AFldDefault;
  {physischer BDE-Typ}
  FldBDETyp:=AFldBDETyp;
end;

constructor TDBO_TableStructureField.CreateSimpleLongD(const AFldName,AFldLong,AFldDict:string; AFldType:char;
  AFldUnits1,AFldUnits2:integer; AFldOpt:integer);
begin
  Create(AFldName,AFldType,AFldUnits1,AFldUnits2,
         0{AFldCnt},AFldOpt,AFldLong,AFldDict,
         ''{AFldGroup},''{AFldUnit},''{AFldFormat},''{AFldDefault},
         CGDO_ftunknown{AFldBDETyp});
end;

constructor TDBO_TableStructureField.CreateSimpleLong(const AFldName,AFldLong:string; AFldType:char;
  AFldUnits1,AFldUnits2:integer; AFldOpt:integer);
begin
  Create(AFldName,AFldType,AFldUnits1,AFldUnits2,
         0{AFldCnt},AFldOpt,AFldLong,''{AFldDict},
         ''{AFldGroup},''{AFldUnit},''{AFldFormat},''{AFldDefault},
         CGDO_ftunknown{AFldBDETyp});
end;

constructor TDBO_TableStructureField.CreateSimple(const AFldName:string; AFldType:char;
  AFldUnits1,AFldUnits2:integer; AFldOpt:integer);
begin
  CreateSimpleLong(AFldName,AFldName,AFldType,AFldUnits1,AFldUnits2,AFldOpt);
end;

procedure TDBO_TableStructureField.Assign(ASource: TDBO_TableStructureField);
begin
  FldName:=ASource.FldName;
  FldType:=ASource.FldType;
  FldUnits1:=ASource.FldUnits1;
  FldUnits2:=ASource.FldUnits2;
  FldCnt:=ASource.FldCnt;
  FldOpt:=ASource.FldOpt;
  FldLong:=ASource.FldLong;
  FldDict:=ASource.FldDict;
  FldGroup:=ASource.FldGroup;
  FldUnit:=ASource.FldUnit;
  FldFormat:=ASource.FldFormat;
  FldDefault:=ASource.FldDefault;
  FldBDETyp:=ASource.FldBDETyp;
end;

function TDBO_TableStructureField.HasIndex: boolean;
begin
  Result:=BitSetInLongInt(FldOpt,0);
end;

function TDBO_TableStructureField.HasUniqueIndex: boolean;
begin
  Result:=BitSetInLongInt(FldOpt,1);
end;

function TDBO_TableStructureField.HasNotNull: boolean;
begin
  Result:=BitSetInLongInt(FldOpt,2);
end;
{$ENDREGION}

{$REGION '------------------- Komponente Gruppe von Struktureinträgen ---------------------------'}
constructor TDBO_TableStructureEntries.CreateAssign(Source:TDBO_TableStructureEntries);
begin
  inherited Create;
  Assign(Source);
end;

function TDBO_TableStructureEntries.FindField(const FieldName:string; var FldIndex:integer):TDBO_TableStructureField;
var I     : integer;
    Entry : TDBO_TableStructureField;
begin
  Result:=nil; FldIndex:=-1;
  {bei externen Tabellen (xls,csv) kann ein Spaltennamen leer sein,
   dann darf er aber nicht verwendet werden, Leerer Feldname heißt letztlich ungültig}
  if FieldName=EmptyStr then Exit;
  for I:=0 to Count-1 do
  begin
    Entry:=At(I);
    if SameText(Entry.FldName,FieldName) then begin
      Result:=Entry;
      FldIndex:=I;
      exit;
    end;
  end;
end;

function TDBO_TableStructureEntries.FindField(const FieldName:string):TDBO_TableStructureField;
var I     : integer;
    Entry : TDBO_TableStructureField;
begin
  Result:=nil;
  {bei externen Tabellen (xls,csv) kann ein Spaltennamen leer sein,
   dann darf er aber nicht verwendet werden, Leerer Feldname heißt letztlich ungültig}
  if FieldName=EmptyStr then Exit;
  for I:=0 to Count-1 do begin
    Entry:=At(I);
    if SameText(Entry.FldName,FieldName) then begin
      Result:=Entry;
      exit;
    end;
  end;
end;

function TDBO_TableStructureEntries.FindLong(const FieldLong:string; var FldIndex:integer):TDBO_TableStructureField;
var I     : integer;
    Entry : TDBO_TableStructureField;
begin
  Result:=nil; FldIndex:=-1;
  {bei externen Tabellen (xls,csv) kann ein Spaltennamen leer sein,
   dann darf er aber nicht verwendet werden, Leerer Feldname heißt letztlich ungültig}
  if FieldLong=EmptyStr then Exit;
  for I:=0 to Count-1 do begin
    Entry:=At(I);
    if SameText(Entry.FldLong,FieldLong) then begin
      Result:=Entry;
      FldIndex:=I;
      exit;
    end;
  end;
end;

function TDBO_TableStructureEntries.HasField(const FieldName:string):integer;
begin
  FindField(FieldName,Result);
end;

function TDBO_TableStructureEntries.HasFieldLong(const FieldLong:string):integer;
begin
  FindLong(FieldLong,Result);
end;

procedure TDBO_TableStructureEntries.Assign(ASource: TDBO_TableStructureEntries);
var I      : integer;
    AnStrE : TDBO_TableStructureField;
begin
  FreeAll;
  for I:=0 to ASource.Count-1 do begin
    AnStrE:=ASource.At(I);
    with AnStrE do
      Insert(TDBO_TableStructureField.Create(
        FldName, FldType, FldUnits1, FldUnits2, FldCnt,
        FldOpt, FldLong , FldDict, FldGroup, FldUnit, FldFormat,
        FldDefault, FldBDETyp));
  end;
end;

(**)
procedure TDBO_TableStructureEntries.AssignToDFTabDef(Dest:TGDO_DF_TabDef);
var I      : integer;
    AnStrE : TDBO_TableStructureField;
begin
  Dest.FreeAll;
  for I:=0 to Count-1 do begin
    AnStrE:=At(I);
    with AnStrE do
      Dest.Insert(TGDO_DF_FldEntry.Create(FldName, FldType, FldUnits1,
        FldUnits2, FldOpt, FldBDETyp));
  end;
end;

procedure TDBO_TableStructureEntries.AppendFieldsFromDFTabDef(DFTabDef:TGDO_DF_TabDef);
var I,Ab   : integer;
    AnDFE  : TGDO_DF_FldEntry;
    LName  : string;
begin
  if DFTabDef.Count>Count then begin
    Ab:=Count;
    for I:=Ab to DFTabDef.Count-1 do begin
      AnDFE:=DFTabDef.At(I);
      with AnDFE do begin
        LName:='EF_'+IntToStr(I-Ab+1);
        Insert(TDBO_TableStructureField.Create(FldName,FldType,FldUnits1,FldUnits2,
          I+1{Cnt},FldOpt,LName,'','','','','',FldBDETyp));
      end;
    end;
  end;
end;

procedure TDBO_TableStructureEntries.GetBDETypesFromDFTabDef(DFTabDef:TGDO_DF_TabDef; const AdjustCFields:boolean=false);
var I      : integer;
    AnStrE : TDBO_TableStructureField;
    AnDFE  : TGDO_DF_FldEntry;
begin
  for I:=0 to Count-1 do begin
    AnStrE:=At(I);
    AnDFE:=DFTabDef.At(I);
    AnStrE.FldBDETyp:=AnDFE.FldBDETyp;
    {Bei Zeichenkettenfeldern wird die von der Datenbank gelieferte Länge
     auch übertragen, dadurch kommt kein SQL-Fehler, wenn z.B. in einer
     konkreten Datenbank das Feld kürzer ist als von GeODin vorgesehen,
     dies sollte aber nur bei unkritischen Tabellen (z.B. PRJDEF - Name
     der Abfrage) gemacht werden, da man sonst einen solchen Fehler nicht
     mitbekommt}
    if AdjustCFields then
      if AnStrE.FldType='C' then AnStrE.FldUnits1:=AnDFE.FldUnits1;
  end;
end;
(**)
function TDBO_TableStructureEntries.GetIdentField(var AnIdentField: string; AOption: longint): boolean;
var I : integer;
    F : TDBO_TableStructureField;
begin
  Result:=false;
  for I:=0 to Count-1 do
  begin
    F:=At(I);
    if BitSetInLongint(F.FldOpt,AOption) then
    begin
      AnIdentField:=F.FldName;
      Result:=true;
      Exit;
    end;
  end;
end;
{$ENDREGION}

{$REGION '------------------------- Komponente TabellenStruktur ---------------------------------'}

constructor TDBO_TableStructure.Create(const ADescriptor,ADisplayName:string; AOptions:longint;
  AType:longint; ATRC:byte;const AInv: string;AStrDes:TDBO_TableStructureEntries);
begin
  Descriptor:=ADescriptor;
  DisplayName:=ADisplayName;
  StrOptions:=AOptions;
  StrTyp:=AType;
  StrTrc:=ATRC;
  Structure:=AStrDes;
  StrCheckOK:=false;
end;

constructor TDBO_TableStructure.CreateEmpty;
begin
  Descriptor:='';
  DisplayName:='';
  StrOptions:=0;
  StrTyp:=0;
  StrTrc:=0;
  Structure:=TDBO_TableStructureEntries.Create;
  StrCheckOK:=false;
end;

destructor TDBO_TableStructure.Destroy;
begin
  Structure.Free;
  inherited Destroy;
end;

procedure TDBO_TableStructure.Assign(ASource: TDBO_TableStructure);
begin
  Structure.Assign(ASource.Structure);
  StrCheckOK:=ASource.StrCheckOK;
end;

function TDBO_TableStructure.GetSelect: string;
var TabField : TDBO_TableStructureField;
    I : integer;
begin
  Result:='';
  for I:=0 to Structure.Count-1 do
  begin
    TabField := Structure.At(I);
    if I=0 then Result:=TabField.FldName
           else Result:=Result+','+TabField.FldName;
  end;
end;
{$ENDREGION}

end.

