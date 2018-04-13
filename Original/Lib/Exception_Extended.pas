unit Exception_Extended;

interface

uses SysUtils, Classes;

type TGE_AppType = (GEANo, GEARes, GEAResFmt);

type
  /// <summary>
  /// Erweiterte Fehlermeldungen
  /// </summary>
  EExtendedError = class(Exception)
     private
       FErrors  : TStringList;
       FGDError : word;
     protected
       procedure SetErrors(Value:TStringList);
       procedure AddPrevError(Prev:EExtendedError);
     public
       constructor Create(Ident:word; const Msg:string);

       constructor CreateStr(const Msg:string);
       constructor CreateE(Prev:EExtendedError);
       constructor CreateRes(Ident:word); overload;
       constructor CreateERes(Prev:EExtendedError; Ident:word);
       constructor CreateResFmt(Ident:word; const Args: array of const);overload;
       constructor CreateStrFmt(Ident:word; const Msg:string; const Args: array of const);
       constructor CreateEResFmt(Prev:EExtendedError; Ident:word;
                                 const Args: array of const);
       constructor CreateList(TheList:TStrings);
       destructor Destroy; override;
       function HasADOError(AErrNumber: string): boolean;
       property GDError:word read FGDError;
       property Errors:TStringList read FErrors write SetErrors;
     end;

const Extended_Log : TStringList = nil;

procedure Extended_StartLog;
procedure Extended_EndLog;
procedure Extended_AddLogLine(const Txt:string);
procedure Extended_AddLogException(AnE:Exception);
{}
procedure EGE_CheckException(E:Exception;
  GEAppType: TGE_AppType; Ident:word; const Args: array of const); overload;
procedure EGE_CheckException(E:Exception; Ident:word); overload;
procedure EGE_CheckException(E:Exception; const TheText:string); overload;
procedure EGE_CheckExceptionTxt(E:Exception;
  GEAppType: TGE_AppType; const TheText:string; const Args: array of const);
procedure EGE_GetExceptionStack(E:Exception; ExceptionStack:TStrings);
function EGE_GetExceptionStackAsString(E:Exception; Mirror:boolean):string;
procedure EGE_GetExceptionStackMirror(E:Exception; ExceptionStack:TStrings);

implementation

procedure Extended_StartLog;
begin
  if Extended_Log=nil then Extended_Log:=TStringList.Create
                      else Extended_Log.Clear;
end;

procedure Extended_EndLog;
begin
  if Extended_Log=nil then Exit
                      else FreeAndNil(Extended_Log);
end;

function Extended_CanLog:boolean;
begin
  Result:=(Extended_Log<>nil);
end;

procedure Extended_AddLogLine(const Txt:string);
begin
  if Extended_CanLog then Extended_Log.Add(Txt);
end;

procedure Extended_AddLogException(AnE:Exception);
var I:integer;
begin
  if Extended_CanLog then begin
    if AnE is EExtendedError then begin
      for I:=EExtendedError(AnE).Errors.Count-1 downto 0 do Extended_Log.Add(EExtendedError(AnE).Errors[I]);
    end
    else Extended_Log.Add(AnE.Message);
    Extended_Log.Add('');
  end;
end;

{------------------------------------------------------------------------------}
constructor EExtendedError.Create(Ident:word; const Msg:string);
begin
  inherited Create(Msg);
  FGDError:=Ident;
  FErrors:=TStringList.Create;
  FErrors.Add(Message);
end;

constructor EExtendedError.CreateStr(const Msg:string);
begin
  Create(0, Msg);
end;

constructor EExtendedError.CreateStrFmt(Ident:word; const Msg:string; const Args: array of const);
var ErrStr:string;
begin
  FmtStr(ErrStr,Msg,Args);
  Create(Ident,ErrStr);
end;

constructor EExtendedError.CreateRes(Ident:word);
begin
  Create(Ident,LoadStr(Ident));
end;

constructor EExtendedError.CreateResFmt(Ident:word; const Args: array of const);
begin
  Create(Ident,Format(LoadStr(Ident), Args));
end;

constructor EExtendedError.CreateList(TheList:TStrings);
begin
  CreateStr('');
  FErrors.Clear;
  FErrors.AddStrings(TheList);
end;

procedure EExtendedError.SetErrors(Value : TStringList);
begin
  FErrors.Assign(Value);
end;

procedure EExtendedError.AddPrevError(Prev:EExtendedError);
begin
  SetErrors(Prev.Errors);
end;

constructor EExtendedError.CreateE(Prev:EExtendedError);
begin
  Create(Prev.GDError,Prev.Message);
  AddPrevError(Prev);
end;

constructor EExtendedError.CreateERes(Prev:EExtendedError; Ident:word);
begin
  Create(Prev.GDError,Prev.Message);
  AddPrevError(Prev);
  FErrors.Add(LoadStr(Ident));
end;

constructor EExtendedError.CreateEResFmt(Prev:EExtendedError; Ident:word;
                                       const Args: array of const);
begin
  Create(Prev.GDError,Prev.Message);
  AddPrevError(Prev);
  FErrors.Add(Format(LoadStr(Ident), Args));
end;

destructor EExtendedError.Destroy;
begin
  FErrors.Free;
  inherited Destroy;
end;

function EExtendedError.HasADOError(AErrNumber: string): boolean;
var I,P : integer;
begin
  Result:=false;
  for I:=0 to Pred(FErrors.Count) do
  begin
    P:=Pos(':',FErrors[I]);
    if P>0 then
      Result:=Pos(AErrNumber,FErrors[I])=1;
    if Result then Exit;
  end;
end;

procedure EGE_CheckException(E:Exception;
  GEAppType: TGE_AppType; Ident:word; const Args: array of const);
var TheGError : EExtendedError;
begin
  if E is EExtendedError then begin
    TheGError:=EExtendedError.CreateE(EExtendedError(E));
  end
  else TheGError:=EExtendedError.Create(2000,Format('%s.', [E.Message]));
  case GEAppType of
    GEARes    : TheGError.Errors.Add(LoadStr(Ident));
    GEAResFmt : TheGError.Errors.Add(Format(LoadStr(Ident),Args));
  end;
  raise TheGError;
end;

procedure EGE_CheckException(E:Exception; Ident:word);
begin
  EGE_CheckException(E,GEARes,Ident,[]);
end;

procedure EGE_CheckExceptionTxt(E:Exception;
  GEAppType: TGE_AppType; const TheText:string; const Args: array of const);
var TheGError : EExtendedError;
begin
  if E is EExtendedError then begin
    TheGError:=EExtendedError.CreateE(EExtendedError(E));
  end
  else TheGError:=EExtendedError.Create(2000,Format('%s.', [E.Message]));
  case GEAppType of
    GEARes    : TheGError.Errors.Add(TheText);
    GEAResFmt : TheGError.Errors.Add(Format(TheText,Args));
  end;
  raise TheGError;
end;

procedure EGE_CheckException(E:Exception; const TheText:string);
begin
  EGE_CheckExceptionTxt(E,GEARes,TheText,[]);
end;

procedure EGE_GetExceptionStack(E:Exception; ExceptionStack:TStrings);
var I:integer;
begin
  ExceptionStack.BeginUpdate;
  try
    if E is EExtendedError then begin
      for I:=0 to EExtendedError(E).Errors.Count-1 do
        ExceptionStack.Add(EExtendedError(E).Errors[I]);
    end
    else begin
      ExceptionStack.Add(E.Message);
    end;
  finally
    ExceptionStack.EndUpdate;
  end;
end;

procedure EGE_GetExceptionStackMirror(E:Exception; ExceptionStack:TStrings);
var I:integer;
begin
  ExceptionStack.BeginUpdate;
  try
    if E is EExtendedError then begin
      for I:=EExtendedError(E).Errors.Count-1 downto 0 do
        ExceptionStack.Add(EExtendedError(E).Errors[I]);
    end
    else begin
      ExceptionStack.Add(E.Message);
    end;
  finally
    ExceptionStack.EndUpdate;
  end;
end;

function EGE_GetExceptionStackAsString(E:Exception; Mirror:boolean):string;
var ExceptionStack:TStringList;
begin
  ExceptionStack:=TStringList.Create;
  try
    if Mirror then EGE_GetExceptionStackMirror(E,ExceptionStack)
              else EGE_GetExceptionStack(E,ExceptionStack);
    Result:=ExceptionStack.Text;
  finally
    ExceptionStack.Free;
  end;
end;

end.
