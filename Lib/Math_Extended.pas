unit Math_Extended;  {Mathe}

interface

uses {Delphi} WinTypes, WinProcs, Math, SysUtils, Types,
     {Utils}  G3_TC;


function BitSet(a,n:Byte):boolean;
function BitSetInLongInt(a:longint;n:Byte):boolean;
procedure SetBitInLongInt(var a:longint;n:longint);
procedure KillBitInLongInt(var a:longint;n:longint);

function GetMinInt(A,B:integer):integer;

implementation


function BitSet(a,n:Byte):boolean;
{ermittelt ob Bit n in Byte a gesetzt ist  Bit 0-7}
begin
   BitSet:=1=(a and (1 shl n)) shr n;
end;

function BitSetInLongInt(a:longint;n:Byte):boolean;
{Bits 0-31}
begin
  {0..7}
  if n<8 then BitSetInLongInt:=BitSet(lobyte(loword(a)),n)
  else if n<16 then BitSetInLongInt:=BitSet(hibyte(loword(a)),n-8)
  else if n<24 then BitSetInLongInt:=BitSet(lobyte(hiword(a)),n-16)
  else BitSetInLongInt:=BitSet(lobyte(hiword(a)),n-24)
end;

procedure SetBitInLongInt(var a:longint;n:longint);
begin
  if not BitSetInLongInt(a,n) then a:=a+(1 shl n);
end;

function ChangeBitInLongInt(OldOpt,NewOpt:longint; n:byte):longint;
begin
  Result:=OldOpt;
  if BitSetInLongInt(NewOpt,n) then SetBitInLongInt(Result,n)
                               else KillBitInLongInt(Result,n);
end;

procedure KillBitInLongInt(var a:longint;n:longint);
begin
  if BitSetInLongInt(a,n) then a:=a-(1 shl n);
end;
{------------------------------------------------------------------------------}
function GetMinInt(A,B:integer):integer;
begin
  if A<B then GetMinInt:=A else GetMinInt:=B;
end;


end.
