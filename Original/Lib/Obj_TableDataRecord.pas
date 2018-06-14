unit Obj_TableDataRecord;

interface

uses {DELPHI} SysUtils, Windows, Classes, Types,
              COLL_OBJ,
              Obj_TableRecord,
              Obj_TableDataField,
              String_Extended, Obj_TableStructure;

type
      /// <summary>
      /// Objekt für Datensätze aus Datenbanktabelle
      /// </summary>
      TDBO_TableDataRecord = class(TObject)
        public
        Data        : TColl;     {of TDBO_TableDataField}
        constructor Create;
        destructor Destroy; override;
        procedure Assign(ASource: TDBO_TableDataRecord);
        procedure AssignToDFRecord(Dest:TGDO_DF_DataRecord);
        function GetIdxOfIdx(AIdx:integer):integer;
        procedure SetStringValue(FieldNr:integer; const S:String);
        function GetStringValue(FieldNr:integer):string;
        function GetIntValue(FieldNr:integer):integer;
        function GetDate(DateFieldNr:integer):TDateTime;
      end;

implementation

constructor TDBO_TableDataRecord.Create;
begin
  inherited Create;
  Data:=TColl.Create;
end;

destructor TDBO_TableDataRecord.Destroy;
begin
  Data.Free;
  inherited Destroy;
end;

procedure TDBO_TableDataRecord.Assign(ASource: TDBO_TableDataRecord);
var I     : integer;
    OL,NL : TDBO_TableDataField;
begin
  Data.Free; Data:=TColl.Create;
  for I:=0 to ASource.Data.Count -1 do begin
    OL:=ASource.Data.At(I);
    NL:=TDBO_TableDataField.Create(OL.Idx,OL.HDat);
    Data.Insert(NL);
  end;
end;

procedure TDBO_TableDataRecord.AssignToDFRecord(Dest:TGDO_DF_DataRecord);
var I      : integer;
    AEntry : TDBO_TableDataField;
begin
  Dest.Data.FreeAll;
  for I:=0 to Data.Count-1 do begin
    AEntry:=Data.At(I);
    Dest.Data.Insert(TGDO_DF_ColEntry.Create(AEntry.Idx,AEntry.HDat));
  end;
end;

function TDBO_TableDataRecord.GetIdxOfIdx(AIdx:integer):integer;
var OneListEntry : TDBO_TableDataField;
    I            : integer;
begin
  GetIdxOfIdx:=-1;
  for I:=0 to Data.Count-1 do begin
    OneListEntry:=Data.At(I);
    if OneListEntry.Idx=AIdx then begin
      GetIdxOfIdx:=I;
      exit;
    end;
  end;
end;

function TDBO_TableDataRecord.GetStringValue(FieldNr:integer):string;
var AnIdx        : integer;
    OneListEntry : TDBO_TableDataField;
begin
  AnIdx:=GetIdxOfIdx(FieldNr);
  if AnIdx<>-1 then begin
    OneListEntry:=Data.At(AnIdx);
    Result:=OneListEntry.HDat;
  end else Result:='';
end;

procedure TDBO_TableDataRecord.SetStringValue(FieldNr:integer; const S:String);
var AnIdx,L : integer;
    LE      : TDBO_TableDataField;
begin
  L:=length(S);
  AnIdx:=GetIdxOfIdx(FieldNr);
  if AnIdx=-1 then begin {neu rein}
    if L>0
      then Data.Insert(TDBO_TableDataField.Create(FieldNr,S));
  end
  else begin {altes austauschen oder löschen}
    if L=0 then Data.AtFree(AnIdx)
    else begin
      LE:=Data.At(AnIdx);
      LE.HDat:=S;
    end;
  end;
end;

function TDBO_TableDataRecord.GetIntValue(FieldNr:integer):integer;
begin
  Result:=ESO_StrToInteger(GetStringValue(FieldNr));
end;

function TDBO_TableDataRecord.GetDate(DateFieldNr:integer):TDateTime;
begin
  Result:=ES_GetDateFromIntDateString(GetStringValue(DateFieldNr));
end;

end.
