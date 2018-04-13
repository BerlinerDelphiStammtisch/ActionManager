unit Obj_TableRecord;

interface

uses Data.DB, Coll_Obj;

type

     TGDO_DF_ColEntry = class(TObject)
      public
        Idx :IntB16;
        HDat:string;
        constructor Create(AnIdx:IntB16; const AnHDat:string);
      end;

     TGDO_DF_DataRecord = class(TObject)
     public
       RecID : IntB16;
       Data  : TColl;  {of TGDO_DF_ColEntry}
       constructor Create(ARecID:IntB16);
       destructor Destroy; override;
       procedure Assign(ASource: TGDO_DF_DataRecord);
       function GetIdxOfIdx(AIdx:integer):integer;
       procedure SetValue(FieldNr:integer; const NewValue:string);
       procedure SetIntValue(FieldNr:integer; IntValue:integer);
       function GetIntValue(FieldNr:integer):integer;
       procedure SetStringValue(FieldNr:integer; const AString:string);
       function GetStringValue(FieldNr:integer):string;
     end;

     /// <summary>
     ///  Parameter zur Felddefinition
     /// </summary>
     TGDO_DF_FldEntry = class(TObject)
     public
       FldName   : string;
       FldType   : char;
       FldUnits1 : byte;
       FldUnits2 : byte;
       FldOpt    : longint;
       FldBDETyp : TFieldType;
       constructor Create(const AFldName:string; AFldType:char;
                          AFldUnits1, AFldUnits2,
                          AFldOpt:longint; AFldBDETyp:TFieldType);
     end;

     TGDO_DF_TabDef = class(TColl) {of TGDO_DF_FldEntry}
     public
       procedure Assign(ASource: TGDO_DF_TabDef);
     end;

implementation

uses System.SysUtils, String_Extended;

{------------------------------------------------------------------------------}

constructor TGDO_DF_ColEntry.Create(AnIdx:IntB16; const AnHDat:string);
begin
  inherited Create;
  Idx:=AnIdx;
  HDat:=AnHDat;
end;

{------------------------------------------------------------------------------}

constructor TGDO_DF_DataRecord.Create(ARecID:IntB16);
begin
  inherited Create;
  RecID:=ARecID;
  Data:=TColl.Create;
end;

destructor TGDO_DF_DataRecord.Destroy;
begin
  Data.Free;
  inherited Destroy;
end;

procedure TGDO_DF_DataRecord.Assign(ASource: TGDO_DF_DataRecord);
var I         : integer;
    SEnt,DEnt : TGDO_DF_ColEntry;
begin
  RecID:=ASource.RecID;
  Data.FreeAll;
  for I:=0 to ASource.Data.Count-1 do begin
    SEnt:=ASource.Data.At(I);
    DEnt:=TGDO_DF_ColEntry.Create(SEnt.Idx, SEnt.HDat);
    Data.Insert(DEnt);
  end;
end;

function TGDO_DF_DataRecord.GetIdxOfIdx(AIdx:integer):integer;
var ColEntry : TGDO_DF_ColEntry;
    I        : integer;
begin
  GetIdxOfIdx:=-1;
  for I:=0 to Data.Count-1 do begin
    ColEntry:=Data.At(I);
    if ColEntry.Idx=AIdx then begin
      GetIdxOfIdx:=I;
      exit;
    end;
  end;
end;

procedure TGDO_DF_DataRecord.SetValue(FieldNr:integer; const NewValue:string);
var AnIdx,L:integer;
begin
  L:=length(NewValue);
  AnIdx:=GetIdxOfIdx(FieldNr);
  if AnIdx=-1 then begin {neu rein}
    if L>0
      then Data.Insert(TGDO_DF_ColEntry.Create(FieldNr,NewValue));
  end
  else begin {altes austauschen oder löschen}
    if L=0 then Data.AtFree(AnIdx)
           else Data.AtPut(AnIdx,TGDO_DF_ColEntry.Create(FieldNr,NewValue));
  end;
end;

procedure TGDO_DF_DataRecord.SetIntValue(FieldNr:integer; IntValue:integer);
begin
  SetValue(FieldNr, IntToStr(IntValue));
end;

function TGDO_DF_DataRecord.GetIntValue(FieldNr:integer):integer;
begin
  Result:=ESO_StrToInteger(GetStringValue(FieldNr));
end;

procedure TGDO_DF_DataRecord.SetStringValue(FieldNr:integer; const AString:string);
begin
  SetValue(FieldNr, AString);
end;

function TGDO_DF_DataRecord.GetStringValue(FieldNr:integer):string;
var AnIdx    : integer;
    ColEntry : TGDO_DF_ColEntry;
begin
  AnIdx:=GetIdxOfIdx(FieldNr);
  if AnIdx<>-1 then begin
    ColEntry:=Data.At(AnIdx);
    Result:=ColEntry.HDat;
  end else Result:='';
end;

constructor TGDO_DF_FldEntry.Create(const AFldName:string; AFldType:char;
 AFldUnits1, AFldUnits2, AFldOpt:longint; AFldBDETyp:TFieldType);
begin
  inherited Create;
  FldName:=AFldName;
  FldType:=AFldType;
  FldUnits1:=AFldUnits1;
  FldUnits2:=AFldUnits2;
  FldOpt:=AFldOpt;
  FldBDETyp:=AFldBDETyp;
end;

procedure TGDO_DF_TabDef.Assign(ASource: TGDO_DF_TabDef);
var I      : integer;
    AnFld  : TGDO_DF_FldEntry;
begin
  FreeAll;
  for I:=0 to ASource.Count-1 do begin
    AnFld:=ASource.At(I);
    with AnFld do
      Insert(TGDO_DF_FldEntry.Create(
        FldName, FldType, FldUnits1, FldUnits2, FldOpt, FldBDETyp));
  end;
end;

end.
