unit Obj_TableDataField;

interface

uses {DELPHI} SysUtils, Windows, Classes, Types,
              COLL_OBJ,
              String_Extended, Obj_TableStructure;

type
      {Althergebrachtes}
      TDBO_TableDataField = class(TObject)
      public
        Idx :IntB16;
        HDat:string;
        constructor Create(AnIdx:IntB16; AnHDat:string);
        destructor Destroy; override;
        procedure Assign(ASource: TDBO_TableDataField);
      end;

implementation

constructor TDBO_TableDataField.Create(AnIdx:IntB16; AnHDat:string);
begin
  inherited Create;
  Idx:=AnIdx;
  HDat:=AnHDat;
end;

destructor TDBO_TableDataField.Destroy;
begin
  inherited Destroy;
end;

procedure TDBO_TableDataField.Assign(ASource: TDBO_TableDataField);
begin
  Idx:=ASource.Idx;
  HDat:=ASource.HDat;
end;

end.
