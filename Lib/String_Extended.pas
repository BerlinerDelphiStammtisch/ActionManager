unit String_Extended;

{============================================================================}
{================= Stringbehandlungs-Funktionen =====================}
{============================================================================}

interface

uses SysUtils, Vcl.StdCtrls, WinProcs, Vcl.Graphics, Classes, DateUtils, Math, RegularExpressions;

type
  TBoolStrLang = (bslGer, bslEng);

function ES_CheckStrInt(const AString: string): boolean;
function ES_CheckEStrFloat(const AString: string): boolean;
function ES_CheckIStrFloat(const AString: string): boolean;
function ES_CutZero(const InStr: string): string;
function ES_CutLeadingZero(const InStr: string): string;
function ES_RightPos(const AString: string; AChar: char): integer;
function ES_CountCharInStr(const InStr: string; AChar: char): integer;
function ES_ReplaceChar(const InStr: string; OChar,NChar: char): string;
function ES_StrFill(Fill:Char;Len:integer):string;
function ES_StrFillUp(const InStr:String;Fill:char;Len:integer;Left:boolean): string;
function ES_StrCountLeadingNumber(const InStr: string): integer;
function ES_StrFillLeftZero(const InStr: string; Count: integer): string;
//function ES_SetEToFloat(InStr: string): string;
function ES_DeleteChar(const InStr: string;AChar: char): string;
function ES_DeleteSubStr(const InStr,DelStr: string): string;
function ES_CutKlammer(const InStr: string): string;
function ES_BeforeChar(const InStr: string; AChar: char): string;
function ES_AfterChar(const InStr: string; AChar: char; const AWithTrim:boolean=true): string;
function ES_BeforeSubstring(const InStr,Substr: string; CaseSensitive: boolean): string;
function ES_AfterSubstring(const InStr,Substr: string; CaseSensitive: boolean): string;
function ES_ChangeSubstr(const MainString,OString,NString: string): string;
function ES_ChangeSubstrAll(const MainString,OString,NString: string): string;
function ES_ChangeNonStandard(const InStr: string): string;

function ES_CountByChar(const InStr: string; AChar: Char): word;
function ES_GetWordByIndex(const InStr: string; AChar: Char; Index: word): string;
function ES_SetWordByIndex(const InStr,Change: string; AChar: Char; Index: word): string;
function ES_GetMeanByWordIndex(const InStr: string; AChar: Char): real;

{Zeichenketten-Analyse}
procedure ES_AnalyseChars(const InStr: string; var ALength,Cnt_Komma,Cnt_Minus,
                                       Cnt_Semikolon,Cnt_Hochkomma,Cnt_DPunkt,
                                       Cnt_KlammerA,Cnt_KlammerZ: integer);
function ES_CheckIsNumber(const InStr: string): boolean;

function ES_GetColorByName(const ColorString: string): TColor;
function ES_CountByToChar(const InStr: string; StartChar,EndChar: char): word;
function ES_GetWordByToChar(const InStr: string; StartChar,EndChar: char; Index: word): string;
function ES_CountByDblChar(const InStr: string; AChar: char): word;
function ES_GetWordByDblChar(const InStr: string; AChar: char; Index: word): string;
function ES_ChangeSeparator(const InStr: string; DecSep,ThsSep: char): string;
function ES_InKlammern(const InStr: string): string;
function ES_InEckKlammern(const InStr: string): string;
function ES_StrInHochKomma(const S:String):String;
function ES_InDHochkomma(const S:String):String;
function ES_InDollar(const InStr: string): string;
function ES_IsFormula(const InStr: string): boolean;
function ES_HasDots(const InStr: string): boolean;
function ES_StrShift(const InStr: string; AOffset: integer): string;

{Zeichenketten-Bearbeitung}
function ES_IncludeTrailingDP(const InStr: string): string;
function ES_IncludeTrailingChar(const InStr: string; AChar: char): string;
function ES_IncludeLeadingString(const InStr: string; AIntro: string): string;
function ES_CutTrailingChar(const InStr: string; AChar: char): string;
function ES_CutLeadingChar(const InStr: string; AChar: char): string;
{-}
function ES_InHochKommaWide(const S:Widestring): Widestring;
function ES_SetHochkomma2Csv(const InStr: string): string;
{-}
function ES_Url2Text(InStr: string): string;

{Extrakte}
function ES_ExtractKlammer(const InStr: string): string;
function ES_ExtractEckKlammer(const InStr: string; const WithTrim : boolean = true): string;
function ES_ExtractHochKomma(const InStr: string): string;
function ES_ExtractByPattern(const InStr: string; const StartPattern : String; const EndPattern : String): string;
function ES_ExtractLong(const TheInp:string): string;
function ES_Extract2Char(const TheInp,Char1,Char2: string): string;
procedure ES_SplitAtLastChar(const AInp: string; AChar: char; var Before,After: string; const AStartPos : integer = 0);

{Positionen}
function ES_GetFirstPosition(AString: string; AList: TStrings; var AListIndex: integer): integer;

{Farbe durch Ausdruck ermitteln}
{Check}
function ES_CheckFieldName(const TheName: string; var TheChar: char; var FirstNumber: boolean): boolean;

{Dateigröße}
function ES_GetSizeByteStr(AFileSize: int64): string;
function ES_GetSizeStr(AFileSize: int64): string;

{--- Password -----------------------------------------------------------------}
function ES_ValidPasswort2(const InpStr:String):boolean;
function ES_DecodeText2(const AText:String):String;
function ES_EncodeText2(const AText:String):String;
function ES_GetPasswordRandomly: string;

function ESO_CreateGUIDString(AWithBrackets:boolean=false):string;

{Maßeinheiten}

{Grad}
function ES_DegreeFromTxt(InStr: string): extended;

{Kopie von ES_WildCardMatching}
function ES_MatchWildCard(SearchString, Mask : string;
  fWildString,fWildChar : char; fMatchCase:boolean) : boolean;

function ESO_UpperPos(const SubStr,Str:string):integer;
function ESO_ReplCharInStr(const S:string; AFindChar,AReplChar:char):string;
function ESO_StringGetValue(const AString:string):string;
procedure ESO_ReduceValues(List:TStrings);
function ESO_StringsParamFalse(Strings:TStrings; const ParamName:string; Default:boolean):boolean;
function ESO_DeleteEndChars(const AString:string; Chars:TSysCharSet):string;
function ESO_AfterChar(const AString:string; AChar:char; const ReturnIfNotFoundChar:boolean=false):string;
procedure ESO_CutList(const S:string; Delimiter:char; List:TStrings;
  const RemoveEmptyEntries:boolean=true; const TrimEntries:boolean=true);
function ESO_RealToIStr(Value:real; Digits:integer):string;
function ESO_ExpStrToFixed(const S:string):string;
function ESO_StrToReal(const S:string):real; overload;
function ESO_StrToInteger(const S:string):integer;
function ESO_GetParamCutList(const AString,AParam:string; Delimiter:char):string;
function ES_StrIToReal(const S:String):real;
function ES_ConstStr(C:Char;N:integer):String;
function ES_InHochKomma(const S:String):String;
function ES_AppendLeadingZero(I,L:integer):string;
function ESO_AppendLeadingZeroString(const S:string; L:integer):string;
function ES_ReplCharInStr(const S:string; AFindChar,AReplChar:char):string;
function ES_WrapGeODinMsg(const Msg:string; MaxCol:integer):string;
function ES_GetETDateStringFromDate(ADate:TDateTime):string;
function ES_GetIntDateStringFromDate(ADate:TDateTime):string;
function ES_GetDateFromETDateString(const S:string):TDateTime;
function ES_GetDateFromIntDateString(const S:string):TDateTime;
function ESO_ReformatIntDateString(const IntDateString,UDateFormat:string):string;
function ESO_GetGeODinDateTimeStringFromDate(ADate:TDateTime):string;
function ESO_GetDateTimeFromGeODinString(const S:string):TDateTime;
function ESO_CreateCaseSensitiveStringList:TStringList;
function ES_GetDateTimeLog(ADate:TDateTime):string;
function ES_GetProcTimeLog(ADate:TDateTime):string;
function ES_GetETDateStringFromIntDateString(const S:string):string;
function ES_GetIntDateStringFromETDateString(const S:string):string;
function ES_GetXMLDate(ADate: TDateTime): String;

function UNICODE_WideStringToString(const ws: WideString; codePage: Word): AnsiString;
function UNICODE_StringToWideString(const s: AnsiString; codePage: Word): WideString;

function ES_BoolStr(ABoolean: Boolean; ABoolStrLang: TBoolStrLang = bslGer): String;

function ES_WrapMsg(const Msg:string; MaxCol:integer):string;
function ES_GetDateTimeFileName(ADate:TDateTime):string;
function ES_RPos(const Hs:string; C:char):integer;
function ES_FracMax(Hs:string; MaxL:integer):string;
function ESO_GetDescInd(List:TStrings; const ADesc:string):integer;
{Extrahiert aus einem String 'Name [Descriptor]' den Descriptor}
function ESO_GetDescriptor(const TheInp:string; const BeginDelimiter:char='[';
  const EndDelimiter:char=']'):string;
procedure ES_GetLenDec(const Hs:String; var Vor,Nach:byte);
function ESO_RealToIExponentStrV(R:real):string;
function ESO_RealToIExponentStr(R:real):string;
function ESO_LastChar(const S:string):char;
function ESO_RealToIStrV(Value:real; Digits:integer):string;
function ES_RealToIStrFV(Value:real; Format:TFloatFormat; Precision, Digits:integer):string;
function ES_RealToIStrF(Value:real; Format:TFloatFormat; Precision, Digits:integer):string;

{---- Telefonnummern ----}
function ES_ClearTelNr(ATel: String): String;

{Stringprüfungen}
function ES_ValidEmail(const sValue: String): Boolean;
function ES_ValidTelFaxNr(const sValue: String): Boolean;

{String Operationen}
//Exztrahiert einen Teil aus einem String der Durch delimiter definiert wird
function ES_GetPartOfStr(const AString: String; APart: Integer; Delimiter: Char): String;

const EST_CRLF         = chr(13)+chr(10);
      EST_CRLF2 = chr(13)+chr(10)+chr(13)+chr(10);

implementation

const CES_Gleich       = '=';
      CES_Komma        = ',';
      CES_Dollar       = '$';
      CES_Kl_Auf       = '(';
      CES_Kl_Zu        = ')';
      CES_Leer1        = ' ';
      CES_Semikolon    = ';';
      CES_Punkt        = '.';
      CES_Eck_Auf      = '[';
      CES_Eck_Zu       = ']';
      CES_Tab          = chr(9);
      CES_ExtMdb       = 'mdb';
      CES_Zero         = '0';

type  CET_DateP = (Day,Month,Year);
const CET_DatePos : array[1..3] of CET_DateP = (Day,Month,Year);
      CET_ETDateSeparator = '/';
      CET_IntDateSeparator = '.';

const IntDecSep = '.';

{$IFNDEF VER200}
{In Delphi 2009 soll diese Funktion benutzt werden, nur dort ist diese auch implementiert}
function CharInSet(C:Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

{Verwendung: Pj_Obj}
function ES_CutZero(const InStr: string): string;
begin
  Result:=InStr; if InStr=EmptyStr then Exit; if Pos(FormatSettings.DecimalSeparator,InStr)=0 then Exit;
  {Nullen hinten abtrennen}
  while Result[Length(Result)]='0' do Result:=copy(Result,1,Length(Result)-1);
  {hinten stehen gebliebenen DecimalSeparator abtrennen}
  if Result[Length(Result)]=FormatSettings.DecimalSeparator then Result:=copy(Result,1,Length(Result)-1);
end;

{Verwendung: CVO_MesImport }
function ES_CutLeadingZero(const InStr: string): string;
begin
  Result:=InStr; if InStr=EmptyStr then Exit;
  while (Result[1]='0') and (Length(Result)>1) do Result:=copy(Result,2,Length(Result)-1);
end;

{Verwendung: Dlg_ArcViewSurfer_Filter
             Dlg_StrukturEditor_Properties
             Shaper_F}
function ES_CheckStrInt(const AString: string): boolean;
begin
  Result:=false;
  try
    StrToInt(AString);
    Result:=true;
  except
  end;
end;

function ES_CheckEStrFloat(const AString: string): boolean;
begin
  Result:=false;
  try
    StrToFloat(AString);
    Result:=true;
  except
  end;
end;

function ES_CheckIStrFloat(const AString: string): boolean;
begin
  Result:=ES_CheckEStrFloat(ES_ChangeSubstr(AString,CES_Punkt,FormatSettings.DecimalSeparator));
end;

{Verwendung: keine}
function ES_RightPos(const AString: string; AChar: char): integer;
var I : integer;
begin
 Result:=0;
 for I:=Length(AString) downto 1 do
 begin
  if AString[I]=AChar then
  begin
   Result:=I;
   Break;
  end;
 end;
end;

function ES_CountCharInStr(const InStr: string; AChar: char): integer;
var I : integer;
begin
  Result:=0; if Length(InStr)=0 then Exit;
  for I:=1 to Length(InStr) do
    if InStr[I]=AChar then inc(Result);
end;

function ES_ReplaceChar(const InStr: string; OChar,NChar: char): string;
var I : integer;
begin
 Result:=InStr;
 for I:=1 to Length(Result) do
  if Result[I]=OChar then Result[I]:=NChar;
end;

function ES_StrFill(Fill:Char;Len:integer):string;
begin
 Result:='';
 while Length(Result)<Len do Result:=Result+Fill;
end;

function ES_StrFillUp(const InStr:String;Fill:char;Len:integer;Left:boolean): string;
var NeuStr  : string;
begin
  Result:='';
  NeuStr:=InStr;
  while length(NeuStr)<Len do
    if Left then
      NeuStr:=Fill+NeuStr
    else
      NeuStr:=NeuStr+Fill;
  Result:=NeuStr;
end;

function ES_StrCountLeadingNumber(const InStr: string): integer;
begin
  Result:=0;
  if Length(InStr)=0 then Exit;
  while CharInSet(InStr[Result+1],['0'..'9']) do inc(Result);
end;

function ES_StrFillLeftZero(const InStr: string; Count: integer): string;
var I : integer;
begin
  I:=ES_StrCountLeadingNumber(InStr);
  Result:=ES_StrFillUp(InStr,CES_Zero,Length(InStr)+Count-I,true);
end;

function ES_DeleteChar(const InStr: string;AChar: char): string;
begin
 Result:=InStr;
  while pos(AChar,Result)>0 do
    Result:=copy(Result,1,pos(AChar,Result)-1)+copy(Result,pos(AChar,Result)+1,length(Result)-pos(AChar,Result));
end;

function ES_DeleteSubStr(const InStr,DelStr: string): string;
begin
 Result:=InStr;
 if not Result.Contains(DelStr) then Exit;

 while Result.IndexOf(DelStr)>-1 do
   Result:=Result.Remove(Result.IndexOf(DelStr),DelStr.Length);
 {
  while pos(DelStr,Result)>0 do
    Result:=copy(Result,1,pos(DelStr,Result)-1)
            +copy(Result,pos(DelStr,Result)
            +Length(DelStr),length(Result)-pos(DelStr,Result));
            }
end;


function ES_CutKlammer(const InStr: string): string;
begin
  Result:=InStr;
  if (Pos(CES_Kl_Auf,Result)=1) and (Pos(CES_Kl_Zu,Result)=Length(Result))
    and (ES_CountCharInStr(Result,CES_Kl_Auf)=1)
      and (ES_CountCharInStr(Result,CES_Kl_Zu)=1) then Result:=Copy(Result,2,Length(Result)-2);
end;

function ES_ExtractKlammer(const InStr: string): string;
begin
  Result:=InStr;
  if pos(CES_Kl_Auf,Result)>0 then
    Result:=TrimRight(copy(Result,pos(CES_Kl_Auf,Result)+1,length(Result)-pos(CES_Kl_Auf,Result)));
    Result:=TrimRight(copy(Result,1,pos(CES_Kl_Zu,Result)-1));
end;

function ES_ExtractEckKlammer(const InStr: string; const WithTrim: boolean = true): string;
begin
  Result:=InStr;
  if pos(CES_Eck_Auf,Result)>0 then
  begin
    Result:=copy(Result,pos(CES_Eck_Auf,Result)+1,length(Result)-pos(CES_Eck_Auf,Result));
    Result:=Copy(Result,1,pos(CES_Eck_Zu,Result)-1);
    if WithTrim then Result:=TrimRight(Result);
  end else Result:=EmptyStr;
end;

function ES_ExtractHochkomma(const InStr: string): string;
begin
  Result:=InStr; if InStr=EmptyStr then Exit;
  if (Result[1]='"') or (Result[1]='''') then Result:=copy(Result,2,Length(Result)-1);
  if (Result[Length(Result)]='"') or (Result[Length(Result)]='''') then Result:=copy(Result,1,Length(Result)-1);
end;

function ES_ExtractByPattern(const InStr: string; const StartPattern : String; const EndPattern : String): string;
begin
  Result:=InStr;
  if pos(StartPattern,Result)>0 then
  begin
    Result:=TrimRight(copy(Result,pos(StartPattern,Result)+length(Startpattern),length(Result)-pos(StartPattern,Result)));
    Result:=TrimRight(copy(Result,1,pos(EndPattern,Result)-1));
  end else Result:=EmptyStr;
end;

function ES_BeforeChar(const InStr: string; AChar: char): string;
begin
  Result:=EmptyStr;
  if pos(AChar,InStr)>0 then
    Result:=TrimRight(copy(InStr,1,pos(AChar,InStr)-1));
end;

function ES_AfterChar(const InStr: string; AChar: char; const AWithTrim:boolean=true): string;
begin
  Result:=EmptyStr;
  if pos(AChar,InStr)>0 then
    if AWithTrim then Result:=TrimRight(copy(InStr,pos(AChar,InStr)+1,length(InStr)-pos(AChar,InStr)))
                 else Result:=copy(InStr,pos(AChar,InStr)+1,length(InStr)-pos(AChar,InStr));
end;

function ES_BeforeSubstring(const InStr, Substr: string; CaseSensitive: boolean): string;
var P : integer;
begin
 Result:=EmptyStr;
 if CaseSensitive then P:=pos(Substr,InStr) else P:=pos(UpperCase(SubStr),UpperCase(InStr));
 if P>0 then Result:=TrimRight(Copy(InStr,1,P-1));
end;

function ES_AfterSubstring(const InStr,Substr: string; CaseSensitive: boolean): string;
var P : integer;
begin
 if CaseSensitive then P:=pos(Substr,InStr) else P:=pos(UpperCase(SubStr),UpperCase(InStr));
 if P>0 then
 begin
  P:=P+Length(Substr);
  Result:=Copy(InStr,P,Length(InStr)-P+1);
 end else Result:=InStr;
end;

function ES_ChangeSubstr(const MainString,OString,NString: string): string;
begin
 if pos(OString,MainString)>0 then
 begin
  Result:=copy(MainString,1,pos(OString,MainString)-1);
  Result:=Result+NString;
  Result:=Result+copy(MainString,pos(OString,MainString)+length(OString),
                     length(MainString)-pos(OString,MainString)+length(OString));
 end else Result:=MainString;
end;


{eine while pos(MainString)>0 - Wiederholung ist kreuzgefährlich, z.B wenn A durch AB
 ersetzt wird, es gibt also unvollständige Ersetungen wie ;;->; wird zu ;;
 eine vollständige Ersetzung muss von Aussen erzwungen werden, sonst kann
 Endlosschleife die Folge sein
 }
function ES_ChangeSubstrAll(const MainString,OString,NString: string): string;
begin
 Result:=StringReplace(MainString,OString,NString,[rfReplaceAll]);
end;

function ES_ChangeNonStandard(const InStr: string): string;
var I : integer;
begin
  Result:=InStr;
  for I:=1 to length(Result) do
    if not CharInSet(Result[I],['0'..'9','A'..'Z','a'..'z','ä','ö','ü','Ä','Ö','Ü','ß']) then Result[I]:='_';
end;

function ES_CountByChar(const InStr: string; AChar: Char): word;
var I : integer;
begin
  Result:=0;
  for I:=1 to length(InStr) do if InStr[I]=AChar then inc(Result);
end;

function ES_GetWordByIndex(const InStr: string; AChar: Char; Index: word): string;
var I,TheIndex : integer;
begin
  Result:=EmptyStr; if InStr=EmptyStr then Exit;
  TheIndex:=0;
  for I:=1 to length(InStr) do
  begin
    if (TheIndex=Index) and (InStr[I]<>AChar) then Result:=Result+InStr[I];
    if InStr[I]=AChar then inc(TheIndex);
  end;
end;

function ES_SetWordByIndex(const InStr,Change: string; AChar: Char; Index: word): string;
var S,I,TheIndex : integer;
    R,W : string;
begin
  Result:=EmptyStr;
  W:=EmptyStr;
  TheIndex:=0; S:=-1;
  for I:=1 to length(InStr) do
  begin
    if (TheIndex=Index) then
    begin
      if S=-1 then S:=I;               //Start
      if InStr[I]<>AChar then W:=W+InStr[I];
    end;
    if InStr[I]=AChar then inc(TheIndex);
  end;
  if TheIndex<Index then
  begin
    while TheIndex<Index do
    begin
      W:=InStr;
      while TheIndex<Index do begin W:=W+AChar; inc(TheIndex); end;
      Result:=W+Change;
    end;
  end else begin
    R:=Copy(InStr,S+Length(W)+1,Length(InStr)-S-Length(W));
    Result:=Copy(InStr,1,S-1)+Change;
    if R<>EmptyStr then Result:=Result+AChar+R;
  end;
end;

function ES_GetMeanByWordIndex(const InStr: string; AChar: Char): real;
var I,C : integer;
    Value : string;
    Summe : real;
begin
  Summe:=0; Result:=0;
  {1. kein Wert}
  if InStr=EmptyStr then Exit;
  C:=ES_CountByChar(InStr,AChar);
  {2. ein Wert}
  if C=0 then
  begin
    Result:=StrToFloat(ES_ChangeSubstr(InStr,CES_Punkt,FormatSettings.DecimalSeparator));
    Exit;
  end;
  {3. mehrere Werte}
  for I:=0 to C do
  begin
    Value:=ES_ChangeSubstr(ES_GetWordByIndex(InStr,AChar,I),CES_Punkt,FormatSettings.DecimalSeparator);
    Summe:=Summe+StrToFloat(Value);
  end;
  Result:=Summe/Succ(C);
end;

procedure ES_AnalyseChars(const InStr: string; var ALength,Cnt_Komma,Cnt_Minus,
                                       Cnt_Semikolon,Cnt_Hochkomma,Cnt_DPunkt,
                                       Cnt_KlammerA,Cnt_KlammerZ: integer);
var I : integer;
begin
  ALength:=Length(InStr);
  Cnt_Komma:=0; Cnt_Minus:=0; Cnt_Semikolon:=0;
  Cnt_Hochkomma:=0; Cnt_DPunkt:=0; Cnt_KlammerA:=0; Cnt_KlammerZ:=0;
  for I:=1 to Length(InStr) do
  begin
    if InStr[I]='''' then Inc(Cnt_Hochkomma);
    {ungerade Anzahl von Hochkomma heißt, wir sind in einem Klartext,
     da wird nicht ausgewertet}
    if not Odd(Cnt_Hochkomma) then
    begin
      if InStr[I]='-' then Inc(Cnt_Minus);
      if InStr[I]=',' then Inc(Cnt_Komma);
      if InStr[I]=';' then Inc(Cnt_Semikolon);
      if InStr[I]=':' then Inc(Cnt_DPunkt);
      if InStr[I]='(' then Inc(Cnt_KlammerA);
      if InStr[I]=')' then Inc(Cnt_KlammerZ);
    end;
  end;
end;

function ES_CheckIsNumber(const InStr: string): boolean;
var I : integer;
begin
  Result:=false;
  for I:=1 to Length(InStr) do
  begin
    if not CharInSet(InStr[I],['0'..'9','.']) then Exit;
  end;
  Result:=true;
end;

function ES_CountByToChar(const InStr: string; StartChar,EndChar: char): word;
var I,N : integer;
begin
  Result:=0;N:=0;
  for I:=1 to length(InStr) do
  begin
    if InStr[I]=StartChar then inc(N);
    if InStr[I]=EndChar   then inc(N);
    if N=2 then
    begin
      inc(Result);
      N:=0;
    end;
  end;
end;

function ES_GetWordByToChar(const InStr: string; StartChar,EndChar: char; Index: word): string;
var I,TheIndex : integer;
    Read       : boolean;
    StartI     : integer;
begin
  Result:=EmptyStr;
  TheIndex:=0; StartI:=0;
  Read:=false;
  for I:=1 to length(InStr) do
  begin
    if (TheIndex=Index) and (InStr[I]=StartChar) then begin Read:=true; StartI:=I; end;
    if Read then Result:=Result+InStr[I];
    if (InStr[I]=EndChar) and (I>StartI) and (StartI>0) then
    begin
      if (TheIndex=Index) then Read:=false;
      inc(TheIndex);
    end;
  end;
end;

function ES_CountByDblChar(const InStr: string; AChar: char): word;
var I,N   : integer;
begin
  N:=0;
  for I:=1 to length(InStr) do if (InStr[I]=AChar) then inc(N);
  if N mod 2 >0 then Result:=trunc((N-1)/2) else Result:=trunc(N/2);
end;

function ES_GetWordByDblChar(const InStr: string; AChar: char; Index: word): string;
var I,TheIndex : integer;
    First,Read : boolean;
    StartPos   : integer;
begin
  Result:=EmptyStr;
  TheIndex:=0; StartPos:=1;
  First:=false;
  Read:=false;
  for I:=1 to length(InStr) do
  begin
    if (InStr[I]=AChar) and not First then
    begin
      First:=true;
      StartPos:=I;
    end;
    if (TheIndex=Index) and First then Read:=true;
    if Read then Result:=Result+InStr[I];
    if (InStr[I]=AChar) and First and (I>StartPos) then
    begin
      if Read then break;
      First:=false;
      inc(TheIndex);
    end;
  end;
end;

function ES_GetColorByName(const ColorString: string): TColor;
begin
 Result:=clBlack;
 if UpperCase(ColorString)='RED' then Result:=clRed;
 if UpperCase(ColorString)='GREEN' then Result:=clGreen;
 if UpperCase(ColorString)='BLUE' then Result:=clBlue;
 if UpperCase(ColorString)='YELLOW' then Result:=clYellow;
end;

function ES_ChangeSeparator(const InStr: string; DecSep,ThsSep: char): string;
var I : integer;
begin
 Result:=EmptyStr;
 for I:=1 to length(InStr) do
 begin
  if (InStr[I]='.') or (InStr[I]=',') then
  begin
   if InStr[I]='.' then Result:=Result+DecSep;
   if InStr[I]=',' then Result:=Result+ThsSep;
  end else Result:=Result+InStr[I];
 end;
end;

function ES_InKlammern(const InStr: string): string;
begin
 Result:=CES_Kl_Auf+InStr+CES_Kl_zu;
end;

function ES_InEckKlammern(const InStr: string): string;
begin
 Result:=CES_Eck_Auf+InStr+CES_Eck_Zu;
end;

function ES_StrInHochKomma(const S:String):String;
begin
  Result:=''''+S+'''';
end;

function ES_InDHochkomma(const S:String):String;
begin
  Result:='"'+S+'"';
end;

function ES_InDollar(const InStr: string): string;
begin
 Result:=CES_Dollar+InStr+CES_Dollar;
end;

function ES_IsFormula(const InStr: string): boolean;
var N : integer;
begin
 N:=ES_CountCharInStr(InStr,CES_Dollar);
 if (N>0) and not Odd(N) then Result:=true else Result:=false;
end;

{$Region '--------------------------------- Zeichenketten-Bearbeitung --------------------'}
function ES_IncludeTrailingDP(const InStr: string): string;
begin
  if InStr=EmptyStr then Result:=':'
                    else if InStr[Length(InStr)]<>':' then Result:=InStr+':'
                                                      else Result:=InStr;
end;

function ES_IncludeTrailingChar(const InStr: string; AChar: char): string;
begin
  if InStr=EmptyStr then Result:=AChar
                    else if InStr[Length(InStr)]<>AChar then Result:=InStr+AChar
                                                        else Result:=InStr;
end;

function ES_IncludeLeadingString(const InStr: string; AIntro: string): string;
begin
  if InStr=EmptyStr then Result:=AIntro
  									else if Pos(AIntro,InStr)<>1 then Result:=AIntro+InStr
                    														 else Result:=InStr;
end;

function ES_CutTrailingChar(const InStr: string; AChar: char): string;
begin
  Result:=InStr;
  if InStr=EmptyStr then Exit;
  if InStr=AChar then Result:=EmptyStr
                 else while Result[Length(Result)]=AChar do Delete(Result,Length(Result),1);
end;

function ES_CutLeadingChar(const InStr: string; AChar: char): string;
begin
  Result:=InStr;
  if InStr=EmptyStr then Exit;
  if InStr=AChar then Result:=EmptyStr else
    while Result[1]=AChar do Delete(Result,1,1);
end;

function ES_InHochKommaWide(const S:Widestring): Widestring;
begin
  Result:=''''+S+'''';
end;

function ES_SetHochkomma2Csv(const InStr: string): string;
var I : integer;
begin
  Result:=EmptyStr;
  Result:='''';
  for I:=1 to Length(InStr) do
    if InStr[I]=CES_Komma then Result:=Result+''''+InStr[I] else Result:=Result+InStr[I];
  Result:=Result+'''';
end;

function ES_Url2Text(InStr: string): string;
begin
  Result:=InStr;
  Result:=ES_ChangeSubstrAll(Result,'%E2%80%93','-');
  Result:=ES_ChangeSubstrAll(Result,'%28','(');
  Result:=ES_ChangeSubstrAll(Result,'%29',')');
  Result:=ES_ChangeSubstrAll(Result,'%C3%BC','ü');
  Result:=ES_ChangeSubstrAll(Result,'%C3%B6','ö');
  Result:=ES_ChangeSubstrAll(Result,'%C3%84','Ä');
  Result:=ES_ChangeSubstrAll(Result,'%C3%A4','ä');
  Result:=ES_ChangeSubstrAll(Result,'%26','&');
  Result:=ES_ChangeSubstrAll(Result,'%27','''');
  Result:=ES_ChangeSubstrAll(Result,'%E2%80%99','''');
  Result:=ES_ChangeSubstrAll(Result,'%C3%9F','ß');
  Result:=ES_ChangeSubstrAll(Result,'https','http');
end;
{$EndRegion}

{Verwendung: Meth_Sep3_Dictionary}
function ES_HasDots(const InStr: string): boolean;
begin
  Result:=Pos(CES_Punkt,InStr)>0;
end;

function ES_StrShift(const InStr: string; AOffset: integer): string;
var I : integer;
begin
  Result:=InStr;
  for I:=1 to Length(Result) do Result[I]:=chr(ord(Result[I])+AOffset);
end;

{Extrakte}
function ES_ExtractLong(const TheInp:string): string;
var P : integer;
begin
  Result:=TheInp;
  if length(TheInp)>0 then begin
    P:=pos(CES_Tab,TheInp);
    if P>0 then begin
      Result:=copy(TheInp,1,P-1);
    end;
  end;
end;

function ES_Extract2Char(const TheInp,Char1,Char2: string): string;
begin
 Result:=copy(TheInp,pos(Char1,TheInp)+1,(pos(Char2,TheInp)-1)-(pos(char1,TheInp)));
end;

procedure ES_SplitAtLastChar(const AInp: string; AChar: char; var Before,After: string; const AStartPos : integer = 0);
var P,I,S : integer;
begin
  P:=0;
  {wenn eine Startposition übergeben wurde, wird ab der rückwärts gesucht,
   sonst vom Ende des Strings
  }
  if AStartPos=0 then S:=Length(AInp)
                 else S:=AStartPos;
  {Suche beginnen}
  for I:=S downto 1 do
  begin
    if AInp[I]=AChar then
    begin
      P:=I;
      Break;
    end;
  end;
  {es wurde ein Vorkommen gefunden?}
  if P>0 then
  begin
    Before:=Copy(AInp,1,P-1);
    After:=Copy(AInp,P+1,length(AInp)-P);
  end else begin
    Before:=AInp;
    After:=EmptyStr;
  end;
end;

function ES_GetFirstPosition(AString: string; AList: TStrings; var AListIndex: integer): integer;
var I,V,P,Index : integer;
begin
  Result:=0; Index:=-1; P:=999;
  for I:=0 to Pred(AList.Count) do
  begin
    V:=Pos(AList[I],AString);
    if (V>0) and (V<P) then
    begin
      Index:=I;
      P:=V;
    end;
  end;
  if P<999 then
  begin
    Result:=P;
    AListIndex:=Index;
  end;
end;

function ES_CheckFieldName(const TheName: string; var TheChar: char; var FirstNumber: boolean): boolean;
var I : integer;
begin
  Result:=true;
  FirstNumber:=false;
  if length(TheName)=0 then
  begin
    Result:=false;
    exit;
  end;
  if CharInSet(TheName[1],['0'..'9']) then
  begin
    FirstNumber:=true;
    Result:=false;
    Exit;
  end;
  for I:=1 to Length(TheName) do
  begin
    Result:=CharInSet(TheName[I],['A'..'Z']);
    if not Result then Result:=CharInSet(TheName[I],['0'..'9']);
    if not Result then Result:=CharInSet(TheName[I],['_']);
    if not Result then
    begin
      TheChar:=TheName[I];
      Result:=false;
      Exit;
    end;
  end;
end;

function ES_GetSizeByteStr(AFileSize: int64): string;
var S : int64;
    C : string;
begin
  S:=Abs(AFileSize);
  if AFileSize<0 then C:='-' else C:=EmptyStr;
  Result:=C+IntToStr(S);
end;

function ES_GetSizeStr(AFileSize: int64): string;
var S : int64;
    C : string;
begin
  S:=Abs(AFileSize);
  if AFileSize<0 then C:='-' else C:=EmptyStr;

  if S>999999999999 then
  begin
    Result:=IntToStr(round(round(S/1024)/1024/1024));
    Result:=C+Copy(Result,1,length(Result)-3)+'.'+Copy(Result,length(Result)-2,3)+' TByte';
    Exit;
  end;
  if S>999999999 then
  begin
    Result:=IntToStr(round(round(S/1024)/1024));
    Result:=C+Copy(Result,1,length(Result)-3)+'.'+Copy(Result,length(Result)-2,3)+' GByte';
    Exit;
  end;
  if S>999999 then
  begin
    Result:=IntToStr(round(S/1024));
    Result:=C+Copy(Result,1,length(Result)-3)+'.'+Copy(Result,length(Result)-2,3)+' MByte';
    Exit;
  end;
  if (S>999) and (S<1000000) then
  begin
    Result:=IntToStr(S);
    Result:=C+Copy(Result,1,length(Result)-3)+'.'+Copy(Result,length(Result)-2,3)+' kByte';
    Exit;
  end;
  if S<1000 then
    Result:=C+IntToStr(S)+' Byte';
end;

const pass21 = 'zyxwvutsrqponmlkjihgfedcbaöäüZYXWVUTSRQPONMLKJIHGFEDCBA9876543210ÖÄÜ?_{}[]+*#~\§$%&/ß(),.-;:!=<>|@^`´';
      pass22 = 'VGZ]ÜUHBNJIOK?MLP[TFCXDRESAWQYß§_áâãäåæçèéêàëìíûüýùúø*&%$#@!^-+:\<>,/vg1zu3hb4nj6io7kml8pt9fcxdresawq';
      pass23 = 'gEod32INO';
      pass24 = 'zyxwvutsrqponml?kjihgfedcba+$&>ZYXWVUT_SRQ#PONMLKJIHG=<FEDCBA9876543210';
      {chars ohne Sonderzeichen}
      FTPpass = 'zyxwvutsrqponm=lkjihgfedcba+ABCWVUT_SRQPOFGHNMLKJIED9876543210';

function ES_GetPasswordRandomly: string;
var I : integer;
begin
  Randomize;
  Result:=EmptyStr;
  for I:=1 to 8 do Result:=Result+FTPpass[RandomRange(1,Length(FTPpass))];
end;

function ESO_CreateGuidString(AWithBrackets:boolean=false):string;
var ID: TGUID;
begin
  if CreateGuid(ID) = S_OK then
  begin
    Result := GUIDToString(ID);
    if not AWithBrackets then
      Result := Result.Replace('{','').Replace('}','');
  end
  else raise Exception.Create('Error creating GUID');
end;

{Prüfen, ob die verwendeten Zeichen im Password zulässig sind}
function ES_ValidPasswort2(const InpStr:String):boolean;
var I:integer;
begin
  Result:=true;
  for I:=1 to Length(InpStr) do begin
    if Pos(InpStr[I],pass21)=0 then begin
      Result:=false;
      exit;
    end;
  end;
end;

function ES_SetGeODin2PW(const InpStr: string; Encode: boolean): string;
var I,P,E : integer; C,G : Char;
begin
  Result:=EmptyStr;
  for I:=1 to Length(InpStr) do
  begin
     C:=InpStr[I];
     E:=(I+Length(pass23)-1) mod Length(pass23) + 1;
     G:=pass23[E];
     if Encode then
     begin
      P:=Ord(C)+Ord(G)+E;
     end else begin
      P:=Ord(C)-Ord(G)-E;
     end;
     Result:=Result+Chr(P);
  end;
end;

function ES_Chrtran2(const InpStr,SearchStr,ResStr:string; Encode: boolean):string;
var I,P : integer; W : string;
begin
  Result:='';
  if not Encode then W:=ES_SetGeODin2PW(InpStr,Encode) else W:=InpStr;
  for I:=1 to Length(W) do begin
    P:=Pos(W[I],SearchStr);
    if P>0 then Result:=Result+ResStr[P]
           else Result:=Result+W[I];
  end;
  if Encode then Result:=ES_SetGeODin2PW(Result,Encode);
end;

function ES_DecodeText2(const AText:String):string;
begin
  Result:=ES_Chrtran2(AText, pass22, pass21,false);
end;

function ES_EncodeText2(const AText:String):string;
begin
  Result:=ES_Chrtran2(AText, pass21, pass22,true);
end;

{===============================================================================}

function ES_DegreeFromTxt(InStr: string): extended;
var G,M,S : real;
    W : string;
begin
  Result:=0;
  try
    W:=ES_BeforeChar(InStr,'°');
    G:=StrToFloat(W);
    W:=ES_AfterChar(InStr,'°');
    W:=ES_BeforeChar(W,'''');
    M:=StrToFloat(W);
    W:=ES_AfterChar(InStr,'''');
    if Pos('"',W)>0 then W:=ES_BeforeChar(W,'"') else W:=ES_BeforeChar(W,'''');
    S:=StrToFloat(W);
    Result:=G+M/60+S/3600;
  except end;
end;

function ES_MatchWildCard(SearchString, Mask : string;
  fWildString,fWildChar : char; fMatchCase:boolean) : boolean;
var
   s, m   : string;
   ss     : string;
   c      : char;
begin
   s  := SearchString;
   m  := Mask;

   if not fMatchCase
      then begin
             s := AnsiUppercase(s);
             m := AnsiUppercase(m);
   end;

   // Compare the two strings one character at a time until one or the
   // other is exhausted. For each character that matches, we truncate
   // both strings at the left.
   while (length(s) > 0) and (length(m) > 0) do begin

      // Take the first character from the mask.
      ss := Copy(m,1,1);
      c := ss[1];

         // If the next mask character is wildchar, count the two characters
         // as matching; lop them off both strings.
      if c = fWildChar then begin
                  delete(s,1,1);
                  delete(m,1,1);
                end

         // If the next character is a WildString, lop it off the
         // mask string, since it matches whatever follows.
         // Then keep calling this routine recursively
         // to see if what's left of the search string matches what
         // remains of the mask. If it doesn't, truncate the search
         // string at the left (the WildString character matches
         // those bytes, too) and repeat until either there's a match
         // or the search string is exhausted, which means the
         // WildString character has eaten the entire remaining
         // search string.
      else
         if c = fWildString then begin
                   delete(m,1,1);
                   while (not ES_MatchWildCard(s,m,fWildString,fWildChar,fMatchCase))
                               and (length(s) > 0) do
                       delete(s,1,1);
         end

         // Any other character must be an exact match. If not,
         // clear the search string to stop the loop, as a match
         // failure has been detected. This will be sealed below
         // because the search string is null but the mask string
         // is not.
         else if Copy(s,1,1) = Copy(m,1,1)
                 then begin
                        delete(s,1,1);
                        delete(m,1,1);
                 end
                 else s := '';
   end;

   // If the loop is ended, the strings have matched if they
   // are now both reduced to null or if the match string
   // is reduced to a single WildString character.
   result := ((length(s) = 0) and (length(m) = 0))
                or (m = fWildString);
end;

{}
{$IFNDEF VER200}
{In Delphi 2009 soll diese Funktion benutzt werden, nur dort ist diese auch implementiert}
{
function CharInSet(C:Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
}
{$ENDIF}

function ESO_UpperPos(const SubStr,Str:string):integer;
begin
  Result:=pos(AnsiUppercase(SubStr),AnsiUppercase(Str));
end;

function ESO_ReplCharInStr(const S:string; AFindChar,AReplChar:char):string;
var I:integer;
begin
  Result:=S;
  for I:=1 to length(Result) do if Result[I]=AFindChar then Result[I]:=AReplChar;
end;

function ESO_StringGetValue(const AString:string):string;
var P:integer;
begin
  P:=pos('=',AString);
  if P>0 then Result:=Copy(AString,P+1,length(AString))
         else Result:='';
end;

procedure ESO_ReduceValues(List:TStrings);
var I : integer;
begin
  for I:=0 to List.Count-1 do List[I]:=ESO_StringGetValue(List[I]);
end;

function ESO_StringsParamFalse(Strings:TStrings; const ParamName:string; Default:boolean):boolean;
{Der Parameter muss angegeben und auf false gesetzt sein
 Sowohl false (egal ob groß/klein) als auch 0 gelten als wahr
 Wenn Parameter nicht vorhanden wird Default verwendet}
var ParamValue : string;
begin
  ParamValue:=Strings.Values[ParamName];
  if length(ParamValue)>0 then
    Result:=not ((SameText(ParamValue,'false')) or (SameText(ParamValue,'0')))
  else
    Result:=Default;
end;

function ESO_DeleteEndChars(const AString:string; Chars:TSysCharSet):string;
begin
  Result:=AString;
  while (length(Result)>0) and CharInSet(Result[length(Result)],Chars) do
    SetLength(Result,length(Result)-1);
end;

function ESO_AfterChar(const AString:string; AChar:char; const ReturnIfNotFoundChar:boolean=false):string;
var P:integer;
begin
  P:=pos(AChar,AString);
  if P>0 then Result:=trimRight(Copy(AString,P+1,length(AString)))
  else begin
    if ReturnIfNotFoundChar then Result:=AString
                            else Result:='';
  end;
end;

procedure ESO_CutList(const S:string; Delimiter:char; List:TStrings;
  const RemoveEmptyEntries:boolean=true; const TrimEntries:boolean=true);
{Leerzeichen zwischen Delimiter und Inhalt werden immer entfernt
 : 1, 2,3  führt zu 1,2,3
 RemoveEmptyEntries entfernt leere Inhalte zwischen Delimiter
 : 1, , 3  führt dann zu 1,3   ansonsten zu 1,,3}
var TempS,
    TeilS : string;
    P     : integer;
  procedure CheckAddTeil;
  begin
    if TrimEntries then TeilS:=trim(TeilS);
    if length(TeilS)>0 then List.Add(TeilS)
    else if not RemoveEmptyEntries then List.Add(TeilS);
  end;
begin
  List.Clear;
  if length(S)=0 then exit;
  {}
  TempS:=S;
  while pos(Delimiter,TempS)>0 do begin
    P:=pos(Delimiter,TempS);
    TeilS:=Copy(TempS,1,P-1);
    CheckAddTeil;
    TempS:=Copy(TempS,P+1,length(TempS));
  end;
  TeilS:=TempS;
  CheckAddTeil;
end;

function ESO_RealToIStr(Value:real; Digits:integer):string;
var OldS:char;
begin
  OldS:=FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator:=IntDecSep;
    Result:=FloatToStrF(Value,ffFixed,15,Digits);
  finally
    FormatSettings.DecimalSeparator:=OldS;
  end;
end;

function ESO_ExpStrToFixed(const S:string):string;
{Konvertiert wissenschaftliche Schreibweise zu Dezimalschreibweise
 z.B. 1.12e-12 , 1.12e+8 usw.
 Rein kann Zahl mit Komma oder Punkt
 Raus kommt Zahl mit Punkt}
var E : real;
begin
  Result:=S;
  if pos('E',uppercase(Result))=0 then exit;
  try
    E:=StrToFloat(ES_ReplCharInStr(Result,'.',FormatSettings.DecimalSeparator));
    Result:=FloatToStrF(E, ffFixed, 18, 18);
    Result:=ES_ReplCharInStr(Result,FormatSettings.DecimalSeparator,'.');
    {Überflüssige Nullen im Dezimalbereich abschneiden}
    if pos('.',Result)>0 then begin
      Result:=ESO_DeleteEndChars(Result,['0']);
      {Falls nun Zahl mit abschließendem Punkt übrig, (z.B. bei 1.12e+8)
       dann diesen auch entfernen}
      Result:=ESO_DeleteEndChars(Result,['.']);
    end;
  except
  end;
end;

function ESO_StrToReal(const S:string):real;
var ValueString : string;
    ValC        : integer;
    R           : real;
begin
  Result:=0;
  ValueString:=trim(ESO_ReplCharInStr(S,',','.'));
  if length(ValueString)=0 then exit
  else begin
    val(ValueString,R,ValC);
    if ValC=0 then Result:=R;
  end;
end;

function ESO_StrToInteger(const S:string):integer;
var ValC:integer;
begin
  val(S,Result,ValC);
  if ValC<>0 then Result:=0;
end;

function ESO_AddIntNotNull(const S:string; AInt:integer):string;
begin
  Result:=S;
  if AInt>0 then Result:=Result+IntToStr(AInt);
end;

function ESO_GetParamCutList(const AString,AParam:string; Delimiter:char):string;
{Bearbeitet einen String: Param1=ppp; Param2=ooo; ...}
var TempList : TStringList;
begin
  Result:='';
  if length(AString)=0 then exit;
  TempList:=TStringList.Create;
  try
    ESO_CutList(AString,Delimiter,TempList);
    Result:=TempList.Values[AParam];
  finally
    TempList.Free;
  end;
end;

function ES_FracMax(Hs:string; MaxL:integer):string;
begin
  Result:=Copy(hs,1,MaxL);
end;

function ES_StrIToReal(const S:String):real;
var VC:integer;
begin
  Result:=0;
  if length(S)=0 then VC:=1
                 else val(S,Result,VC);
  //if VC<>0 then raise EConvertError.CreateFmt(SInvalidFloat,[S]);
end;

function ES_ConstStr(C:Char;N:integer):String;
var I : integer;
begin
  Result:='';
  for I:=1 to N do Result:=Result+C;
end;

function ES_InHochKomma(const S:String):String;
begin
  Result:=''''+S+'''';
end;

function ES_AppendLeadingZero(I,L:integer):string;
begin
  Result:=IntToStr(I);
  while length(Result)<L do
    Result:='0'+Result;
end;

function ESO_AppendLeadingZeroString(const S:string; L:integer):string;
begin
  Result:=ES_ConstStr('0',L-length(S))+S;
end;

function ES_ReplCharInStr(const S:string; AFindChar,AReplChar:char):string;
var I:integer;
begin
  Result:=S;
  for I:=1 to length(Result) do if Result[I]=AFindChar then Result[I]:=AReplChar;
end;

function ES_WrapGeODinMsg(const Msg:string; MaxCol:integer):string;
var AList: TStringList;
    I    : integer;
begin
  AList:=TStringList.Create;
  try
    AList.Text:=Msg;
    Result:='';
    for I:=0 to AList.Count-1 do begin
      if length(Result)>0 then Result:=Result+EST_CRLF;
      Result:=Result+WrapText(AList[I], MaxCol);
    end;
  finally
    AList.Free;
  end;
end;

function ES_GetETDateStringFromDate(ADate:TDateTime):string;
{Input : Datum
 Output: String im CET_ShortDateFormat
         wenn Datum ungültig -> leerer Datumsstring mit Separator,
         z.B. __/__/____ }
var I                 : integer;
    ADay,AMonth,AYear : word;
begin
  Result:=''; ADay:=0; AMonth:=0; AYear:=0;
  try
    DecodeDate(ADate,AYear,AMonth,ADay);
  except end;
  for I:=1 to 3 do begin
    case CET_DatePos[I] of
      Day   : if ADay=0 then Result:=Result+'  '
                        else Result:=Result+ES_AppendLeadingZero(ADay,2);
      Month : if AMonth=0 then Result:=Result+'  '
                          else Result:=Result+ES_AppendLeadingZero(AMonth,2);
      Year  : if AYear=0 then Result:=Result+'    '
                         else Result:=Result+ES_AppendLeadingZero(AYear,4);
    end;
    if I<3 then Result:=Result+CET_ETDateSeparator;
  end;
end;

function ES_GetIntDateStringFromDate(ADate:TDateTime):string;
{Input : Datum
 Output: String im Internem Datumsformat DD.MM.YYYY
         wenn Datum ungültig -> leerer Datumsstring mit Separator,
         z.B. __.__.____ }
var ADay,AMonth,AYear : word;
begin
  Result:=''; ADay:=0; AMonth:=0; AYear:=0;
  try
    DecodeDate(ADate,AYear,AMonth,ADay);
  except end;
  if ADay=0 then Result:=Result+'  '+CET_IntDateSeparator
            else Result:=Result+ES_AppendLeadingZero(ADay,2)+CET_IntDateSeparator;
  if AMonth=0 then Result:=Result+'  '+CET_IntDateSeparator
              else Result:=Result+ES_AppendLeadingZero(AMonth,2)+CET_IntDateSeparator;
  if AYear=0 then Result:=Result+'    '
             else Result:=Result+ES_AppendLeadingZero(AYear,4);
end;

function ES_GetDateFromETDateString(const S:string):TDateTime;
{Input : String im CET_ShortDateFormat
 Output: Datum
         wenn string ungültig -> Result:=0}
var I,P               : integer;
    ADay,AMonth,AYear : word;
    ACode             : integer;
begin
  Result:=0;
  if (trim(S) <> '.  .') and (trim(S) <> '') then
  begin
    ADay:=0; AMonth:=0; AYear:=0; P:=1;
    for I:=1 to 3 do
    begin
      case CET_DatePos[I] of
        Day : begin
          val(Copy(S,P,2),ADay,ACode);
          inc(P,3);
        end;
        Month : begin
          val(Copy(S,P,2),AMonth,ACode);
          inc(P,3);
        end;
        Year : begin
          val(Copy(S,P,4),AYear,ACode);
          inc(P,5);
        end;
      end;
    end;
    try
      Result:=EncodeDate(AYear,AMonth,ADay);
    except end;
  end;
end;

function ES_GetDateFromIntDateString(const S:string):TDateTime;
{Input : String im Internem Datumsformat DD.MM.YYYY
 Output: Datum
         wenn string ungültig -> Result:=0}
var ADay,AMonth,AYear : word;
    ACode             : integer;
begin
  Result:=0;
  if trim(S)<> '' then
  begin
    val(Copy(S,1,2),ADay,ACode);
    val(Copy(S,4,2),AMonth,ACode);
    val(Copy(S,7,4),AYear,ACode);
    try
      Result:=EncodeDate(AYear,AMonth,ADay);
    except end;
  end;
end;

function ESO_ReformatIntDateString(const IntDateString,UDateFormat:string):string;
{Input  : muß ein Datum der internen Form sein: 'dd.MM.yyyy'
 OutPut : exakt gemäß UDateFormat z.B. 01-JAN-1993,
 Als UDateFormat ist auch Access-Syntax #dd/mm/yyyy# oder
 Orcale-Syntax einschließlich Hochkomma ! zugelassen}
var AnDate          : TDateTime;
    OldDF,
    UsedDateFormat  : string;
    OldDSep,
    OldTSep,DateSep : Char;
    I,AStart        : integer;
begin
  Result:='';
  if (length(IntDateString)=0) or (length(UDateFormat)<2) then exit;
  {Wegen des Oracles auch das noch:}
  if UDateFormat[1]='''' then UsedDateFormat:=AnsiDequotedStr(UDateFormat,'''')
                         else UsedDateFormat:=UDateFormat;
  OldDF:=FormatSettings.ShortDateFormat;
  OldDSep:=FormatSettings.DateSeparator;
  OldTSep:=FormatSettings.TimeSeparator;
  try
    {zunächst den IntDateString in ein Datum umwandeln}
    AnDate:=ES_GetDateFromIntDateString(IntDateString);
    {welcher DateSeparator wird im UDateFormat benutzt ?}
    {Die DateToStr-Routine benutzt bei / und : jeweils den System-Date
     oder Timeseparator, deswegen ersetzen}
    DateSep:='.';
    {1. Zeichen ist evtl. Begrenzer der Zeichenkette, daher prüfen}
    if CharInSet(UsedDateFormat[1],['#']) then AStart:=2 else AStart:=1;
    for I:=AStart to length(UsedDateFormat) do begin
      if not CharInSet(upcase(UsedDateFormat[I]),['D','M','Y']) then begin
        DateSep:=UsedDateFormat[I]; break;
      end;
    end;
    FormatSettings.DateSeparator:=DateSep;
    {TimeSeparator eigentlich Quatsch}
    if pos(':',UsedDateFormat)>0 then FormatSettings.TimeSeparator:=':';
    FormatSettings.ShortDateFormat:=UsedDateFormat;
    Result:=DateToStr(AnDate);
    {Wegen des Oracles auch das noch:}
    if UDateFormat[1]='''' then Result:=AnsiQuotedStr(Result,'''');
    FormatSettings.ShortDateFormat:=OldDF;
    FormatSettings.DateSeparator:=OldDSep;
    FormatSettings.TimeSeparator:=OldTSep;
  except
    FormatSettings.ShortDateFormat:=OldDF;
    FormatSettings.DateSeparator:=OldDSep;
    FormatSettings.TimeSeparator:=OldTSep;
    Result:='';
  end;
end;

function ESO_GetGeODinDateTimeStringFromDate(ADate:TDateTime):string;
var AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime(ADate, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  Result:=ES_AppendLeadingZero(ADay,2)+CET_IntDateSeparator+
          ES_AppendLeadingZero(AMonth,2)+CET_IntDateSeparator+
          ES_AppendLeadingZero(AYear,4)+' '+
          ES_AppendLeadingZero(AHour,2)+':'+
          ES_AppendLeadingZero(AMinute,2)+':'+
          ES_AppendLeadingZero(ASecond,2)+':'+
          ES_AppendLeadingZero(AMilliSecond,3);
end;

function ESO_GetDateTimeFromGeODinString(const S:string):TDateTime;
{01.01.2004 12:13:00:000'  (Die Zeitangabe ist optional)}
var AYear, AMonth, ADay, AHour,
    AMinute, ASecond, AMilliSecond : Word;
    S1                             : string;
begin
  Result:=0;
  if S=EmptyStr then Exit;
  ADay:=ESO_StrToInteger(Copy(S,1,2));
  AMonth:=ESO_StrToInteger(Copy(S,4,2));
  AYear:=ESO_StrToInteger(Copy(S,7,4));
  AHour:=0; AMinute:=0; ASecond:=0; AMilliSecond:=0;
  if length(S)>10 then begin
    S1:=Copy(S,12,2); if length(S1)>0 then AHour:=ESO_StrToInteger(S1);
    S1:=Copy(S,15,2); if length(S1)>0 then AMinute:=ESO_StrToInteger(S1);
    S1:=Copy(S,18,2); if length(S1)>0 then ASecond:=ESO_StrToInteger(S1);
    S1:=Copy(S,21,3); if length(S1)>0 then AMilliSecond:=ESO_StrToInteger(S1);
  end;
  Result:=EncodeDateTime(AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
end;

function ESO_CreateCaseSensitiveStringList:TStringList;
begin
  Result:=TStringList.Create;
  Result.Sorted:=true;
  Result.Duplicates:=dupIgnore;
  Result.CaseSensitive:=true;
end;

function ES_GetDateTimeLog(ADate:TDateTime):string;
var AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime(ADate, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  Result:=ES_AppendLeadingZero(AYear,4)+'-'+
          ES_AppendLeadingZero(AMonth,2)+'-'+
          ES_AppendLeadingZero(ADay,2)+' '+
          ES_AppendLeadingZero(AHour,2)+':'+
          ES_AppendLeadingZero(AMinute,2)+':'+
          ES_AppendLeadingZero(ASecond,2)+':'+
          ES_AppendLeadingZero(AMilliSecond,3);
end;

function ES_GetProcTimeLog(ADate:TDateTime):string;
var AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime(ADate, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  Result:=ES_AppendLeadingZero(AHour,2)+':'+
          ES_AppendLeadingZero(AMinute,2)+':'+
          ES_AppendLeadingZero(ASecond,2)+':'+
          ES_AppendLeadingZero(AMilliSecond,3);
end;

function ES_GetETDateStringFromIntDateString(const S:string):string;
{Input  : String im Internem Datumsformat DD.MM.YYYY
 Output : String im CET_ShortDateFormat}
begin
  Result:=ES_GetETDateStringFromDate(ES_GetDateFromIntDateString(S));
end;

function ES_GetIntDateStringFromETDateString(const S:string):string;
{Input  : String im CET_ShortDateFormat
 Output : String im Internem Datumsformat DD.MM.YYYY}
begin
  Result:=ES_GetIntDateStringFromDate(ES_GetDateFromETDateString(S));
end;

function ES_GetXMLDate(ADate: TDateTime): String;
//Generiert ein Datum im Format YYYY-MM-DD (XML konform)
//ReHo, 13. Nov. 2012
const DateSep = '-';
var ADay,AMonth,AYear : word;
begin
  Result:= ''; ADay:= 0; AMonth:= 0; AYear:= 0;
  try
    DecodeDate(ADate, AYear, AMonth, ADay);
  except end;
  if AYear = 0 then Result:=Result+'    '+ DateSep
               else Result:=Result+ES_AppendLeadingZero(AYear,4)+ DateSep;
  if AMonth= 0 then Result:=Result+'  '+ DateSep
               else Result:=Result+ES_AppendLeadingZero(AMonth,2)+ DateSep;
  if ADay  = 0 then Result:=Result+'  '
               else Result:=Result+ES_AppendLeadingZero(ADay,2);

end;

function UNICODE_WideStringToString(const ws: WideString; codePage: Word): AnsiString;
var
  l: integer;
begin
  if ws = '' then
    Result := ''
  else
  begin
    l := WideCharToMultiByte(codePage,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
      @ws[1], - 1, nil, 0, nil, nil);
    SetLength(Result, l - 1);
    if l > 1 then
      WideCharToMultiByte(codePage,
        WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
        @ws[1], - 1, @Result[1], l - 1, nil, nil);
  end;
end;

function UNICODE_StringToWideString(const s: AnsiString; codePage: Word): WideString;
var
  l: integer;
begin
  if s = '' then
    Result := ''
  else
  begin
    l := MultiByteToWideChar(codePage, MB_PRECOMPOSED, PAnsiChar(s), - 1, nil, 0);
    SetLength(Result, l - 1);
    if l > 1 then
      MultiByteToWideChar(CodePage, MB_PRECOMPOSED, PAnsiChar(s),
        - 1, PWideChar(@Result[1]), l - 1);
  end;
end;

{TODO: Das ist so nicht gut. Es hebelt jedes andere Sprachenkonzept aus}
function ES_BoolStr(ABoolean: Boolean; ABoolStrLang: TBoolStrLang = bslGer): String;
const BoolStrGer: Array[-1..0] of String = ('ja', 'nein');
const BoolStrEng: Array[-1..0] of String = ('true', 'false');
begin
  case ABoolStrLang of
    bslGer: Result:= BoolStrGer[StrToInt(BoolToStr(ABoolean))];
    bslEng: Result:= BoolStrEng[StrToInt(BoolToStr(ABoolean))]
  end;
end;


function ES_WrapMsg(const Msg:string; MaxCol:integer):string;
var AList: TStringList;
    I    : integer;
begin
  AList:=TStringList.Create;
  try
    AList.Text:=Msg;
    Result:='';
    for I:=0 to AList.Count-1 do begin
      if length(Result)>0 then Result:=Result+EST_CRLF;
      Result:=Result+WrapText(AList[I], MaxCol);
    end;
  finally
    AList.Free;
  end;
end;

function ES_GetDateTimeFileName(ADate:TDateTime):string;
var AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word;
begin
  DecodeDateTime(ADate, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond);
  Result:=ES_AppendLeadingZero(AYear,4)+'_'+
          ES_AppendLeadingZero(AMonth,2)+'_'+
          ES_AppendLeadingZero(ADay,2)+'_'+
          ES_AppendLeadingZero(AHour,2)+'_'+
          ES_AppendLeadingZero(AMinute,2)+'_'+
          ES_AppendLeadingZero(ASecond,2)+'_'+
          ES_AppendLeadingZero(AMilliSecond,3);
end;

function ES_RPos(const Hs:string; C:char):integer;
{sucht nach dem letztem Vorkommen eines Zeichens}
var I:integer;
begin
  Result:=0;
  for I:=length(Hs) downto 1 do begin
    if Hs[I]=C then begin
      Result:=I; exit;
    end;
  end;
end;

function ESO_GetDescInd(List:TStrings; const ADesc:string):integer;
var I      : integer;
    AnDesc : string;
begin
  Result:=-1;
  if length(ADesc)>0 then begin
    for I:=0 to List.Count-1 do begin
      AnDesc:=ESO_GetDescriptor(List[I]);
      if CompareStr(AnDesc,ADesc)=0 then begin
        Result:=I;
        exit;
      end;
    end;
  end;
end;

function ESO_GetDescriptor(const TheInp:string; const BeginDelimiter:char='[';
  const EndDelimiter:char=']'):string;
{auch solche Konstrukte: AAA [PARAM [V]]  -> [PARAM [V]]
                         AAA [V] [PARAM]  -> [PARAM]
                         AAA [PARAM]      -> [PARAM]
                         AAA 'PARAM'      -> 'PARAM'}
var P,C,I : integer;
begin
  Result:='';
  P:=ES_RPos(TheInp, EndDelimiter);
  if P<>0 then begin
    Result:=Copy(TheInp,1,P-1);
    C:=1;
    for I:=length(Result) downto 1 do begin
      {Achtung: bei gleichem Begin und EndDelimiter ist die Reihenfolge
       der Test wichtig, zuerst BeginDelimiter und dann EndDelimiter testen}
      if Result[I]=BeginDelimiter then begin
        if C=1 then begin
          Result:=Copy(Result,I+1,length(Result));
          exit;
        end
        else dec(C);
      end;
      {bei gleichem Begin und EndDelimiter tritt das nicht ein:}
      if Result[I]=EndDelimiter then inc(C);
    end;
  end;
end;

procedure ES_GetLenDec(const Hs:String; var Vor,Nach:byte);
{Gibt Vor- und Nachkommastellen einer Stringzahl zurück}
var P : byte;
begin
  Vor:=0; Nach:=0;
  if length(Hs)=0 then exit;
  P:=pos('.',Hs);
  if P<>0 then begin {Es gibt eine Kommastelle}
    Vor:=length(Copy(Hs,1,P-1));
    Nach:=length(Copy(Hs,P+1,length(Hs)-P));
  end else Vor:=length(Hs);
end;

function ESO_RealToIExponentStrV(R:real):string;
begin
  Result:=ESO_ReplCharInStr(ESO_RealToIExponentStr(R),IntDecSep,FormatSettings.DecimalSeparator);
end;

function ESO_RealToIExponentStr(R:real):string;
var S : string;
    P : integer;
begin
  {hierbei kommt sowas raus:
   0.00801 -> 8,010000000000000E-3
   bei Angabe eine Precision von 0 -> 8,0E-3 also es wird was abgeschnitten
   deswegen das umständliche Entfernen der überfl. Nullen}
  S:=FloatToStrF(R,ffExponent,16,0);
  S:=ES_ReplCharInStr(S,FormatSettings.DecimalSeparator,IntDecSep);
  P:=pos('E',S);
  if P>0 then begin
    Result:=Copy(S,P,length(S));
    S:=Copy(S,1,P-1);  {8.010000000000000}
    S:=ESO_DeleteEndChars(S,['0']); {8.01}
    {Im Falle 8. wird wieder eine 0 angefügt, so dass nach Decimal mind. eine Null steht}
    if ESO_LastChar(S)=IntDecSep then S:=S+'0';  {8.0}
    Result:=S+Result;
  end
  else Result:=S;
end;

function ESO_LastChar(const S:string):char;
begin
  if length(S)=0 then Result:=#0
  else Result:=S[length(S)];
end;

function ESO_RealToIStrV(Value:real; Digits:integer):string;
begin
  Result:=ESO_ReplCharInStr(ESO_RealToIStr(Value,Digits),IntDecSep,FormatSettings.DecimalSeparator);
end;

function ES_RealToIStrFV(Value:real; Format:TFloatFormat; Precision, Digits:integer):string;
begin
  Result:=ES_ReplCharInStr(ES_RealToIStrF(Value,Format,Precision,Digits),IntDecSep,FormatSettings.DecimalSeparator);
end;

function ES_RealToIStrF(Value:real; Format:TFloatFormat; Precision, Digits:integer):string;
var OldS:char;
begin
  OldS:=FormatSettings.DecimalSeparator;
  try
    FormatSettings.DecimalSeparator:=IntDecSep;
    Result:=FloatToStrF(Value,Format,15{Precision},Digits);
  finally
    FormatSettings.DecimalSeparator:=OldS;
  end;
end;

function ES_ClearTelNr(ATel: String): String;
var NewteL: string;
begin
  NewTel:= StringReplace(ATel,' ','', [rfReplaceAll]);
  NewTel:= StringReplace(NewTel,'/','', [rfReplaceAll]);
  NewTel:= StringReplace(NewTel,'-','', [rfReplaceAll]);
  NewTel:= StringReplace(NewTel,'(','', [rfReplaceAll]);
  NewTel:= StringReplace(NewTel,')','', [rfReplaceAll]);
  NewTel:= StringReplace(NewTel,'+','', [rfReplaceAll]);
  Result:= NewTel;
end;

function ES_ValidEmail(const sValue: String): Boolean;
var regex: String;
begin
  regex:= '^((?>[a-zA-Z\d!#$%&''*+\-/=?^_`{|}~]+\x20*' +
          '|"((?=[\x01-\x7f])[^"\\]|\\[\x01-\x7f])*"\' +
          'x20*)*(?<angle><))?((?!\.)(?>\.?[a-zA-Z\d!' +
          '#$%&''*+\-/=?^_`{|}~]+)+|"((?=[\x01-\x7f])' +
          '[^"\\]|\\[\x01-\x7f])*")@(((?!-)[a-zA-Z\d\' +
          '-]+(?<!-)\.)+[a-zA-Z]{2,}|\[(((?(?<!\[)\.)' +
          '(25[0-5]|2[0-4]\d|[01]?\d?\d)){4}|[a-zA-Z\' +
          'd\-]*[a-zA-Z\d]:((?=[\x01-\x7f])[^\\\[\]]|' +
          '\\[\x01-\x7f])+)\])(?(angle)>)$';
  if TRegEx.IsMatch(sValue, regex) then Result:= true
                                   else Result:= false;
end;

function ES_ValidTelFaxNr(const sValue: String): Boolean;
var regex: String;
begin
  regex:= '(?<CountryNumber>\+\d*)\s{1}'
         +'(?<AreaCode>\(0\)\d*)\s{1}'
         +'(?<Number>\d*)'
         +'(?<WholeExtension>(-(?<Extension>\d+))?)';
  //ToDo: Nur dann korrekt, wenn alle Gruppen (also Matches) OK sind!
  if TRegEx.IsMatch(sValue, regex) then Result:= true
                                   else Result:= false;
end;

function ES_GetPartOfStr(const AString: String;
  APart: Integer; Delimiter: Char): String;
var i, p: Integer;
    c: Char;
begin
  Result:= ''; p:= 0;
  for i:= 1 to length(AString) do
  begin
    c:= AString[i];
    if c = Delimiter then
    begin
      inc(p);
      if p = APart then break
                   else Continue;
    end;
    if (p+ 1) = APart then Result:= Result+ c;
  end;
end;

end.

