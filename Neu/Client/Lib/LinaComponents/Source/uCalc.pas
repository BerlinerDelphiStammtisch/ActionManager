unit uCalc;

//////////////////////////////////////
///  Lina Calculator Unit          ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

uses
  { Standard-Units }
  SysUtils, Classes, Math,
  { Andere Package-Units }
  uBase, uSysTools;

type
  { Fehlermeldungen }
  ECalculate = class(Exception);
  EIdentifierExists = class(Exception);

  { Hilfsklassen }
  TCalcOperation = (coAdd,coSub,coMul,coDiv,coMod,coExp);
  TCalcOperations = set of TCalcOperation;
  TDecimalSeparators = set of (dsPoint,dsComma);
  TCalculatorOptions = set of (coBrackets,coOperatorsOrder);

  { Ereignisse }
  TCalculatorCalculateEvent = procedure(Sender: TObject) of object;
  TCalculatorConstantEvent = procedure(Sender: TObject; const Name: String) of object;
  TCalculatorFunctionEvent = procedure(Sender: TObject; const Name: String; var Value: Extended) of object;

  TCalculatorTerm = record
  private
    { Private-Deklarationen }
    FValue: Extended;
    FOperation: TCalcOperation;
  public
    { Public-Deklarationen }
    property Value: Extended read FValue write FValue;
    property Operation: TCalcOperation read FOperation write FOperation;
  end;

  TCalculatorTermArray = array of TCalculatorTerm;

  TCalculator = class;

  TCalculatorConstant = class(TCollectionItem)
  private
    { Private-Deklarationen }
    FName: String;
    FValue: Extended;
    { Methoden }
    procedure SetName(Value: String);
  published
    { Published-Deklarationen }
    { Eigenschaften }
    property Name: String read FName write SetName;
    property Value: Extended read FValue write FValue;
    { Methoden }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  end;

  TCalculatorConstants = class(TCollection)
  private
    { Private-Deklarationen }
    FCalculator: TCalculator;
  public
    { Public-Deklarationen }
    { Eigenschaften }
    property Calculator: TCalculator read FCalculator write FCalculator;
    { Methoden }
    constructor Create(ItemClass: TCollectionItemClass; ACalculator: TCalculator);
    destructor Destroy; override;
    function IndexOf(Name: String): Integer;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCalculator = class(TComponent)
  private
    { Ereignisse}
    FCalculateEvent: TCalculatorCalculateEvent;
    FConstantEvent: TCalculatorConstantEvent;
    FFunctionEvent: TCalculatorFunctionEvent;
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FExpression: String;
    FValue: Extended;
    FConstants: TCalculatorConstants;
    FFunctions: TStrings;
    FOperations: TCalcOperations;
    FDecimalSeparators: TDecimalSeparators;
    FOptions: TCalculatorOptions;
    FError: Integer;
    FTerms: TCalculatorTermArray;
    { Methoden }
    function GetFunctions: TStrings;
    procedure SetFunctions(Value: TStrings);
  protected
    { Protected-Deklarationen }
    function GetOperation(const Character: Char): TCalcOperation;
    function CallConstant(const Name: String): Extended;
    function CallFunction(const Name: String; Parameter: Extended): Extended;
    function CalcOperation(A,B: Extended; Operation: TCalcOperation): Extended;
    procedure RaiseError(const Text: String; Index: PChar);
  public
    { Public-Deklarationen }
    { Eigenschaften }
    property Value: Extended read FValue;
    property Error: Integer read FError;
    property Terms: TCalculatorTermArray read FTerms;
    { Methoden }
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AExpression: String); overload;
    destructor Destroy; override;
    function Compile: Boolean;
    procedure Calculate;
  published
    { Published-Deklarationen }
    { Ereignisse}
    property OnCalculate: TCalculatorCalculateEvent read FCalculateEvent write FCalculateEvent;
    property OnConstant: TCalculatorConstantEvent read FConstantEvent write FConstantEvent;
    property OnFunction: TCalculatorFunctionEvent read FFunctionEvent write FFunctionEvent;
    { Eigenschaften }
    property About: TComponentAbout read FAbout;
    property Expression: String read FExpression write FExpression;
    property Constants: TCalculatorConstants read FConstants write FConstants;
    property Functions: TStrings read GetFunctions write SetFunctions;
    property Operations: TCalcOperations read FOperations write FOperations;
    property DecimalSeparators: TDecimalSeparators read FDecimalSeparators write FDecimalSeparators;
    property Options: TCalculatorOptions read FOptions write FOptions;
  end;

  {$IFDEF ADD_COMPONENTREG}
    procedure Register;
  {$ENDIF}

const
  { Sonderzeichen für Ausdrücke }
  CalcSeperators = ['.',','];
  CalcBrackets = ['(',')'];
  CalcOperatorsPre = ['+','-'];
  CalcOperators = CalcOperatorsPre + ['*','/','%','^'];

implementation

{$IFDEF ADD_COMPONENTREG}
  procedure Register;
  begin
    RegisterComponents({$IFDEF ADD_SINGLECATEGORY}ComponentsPage{$ELSE}ComponentsPage_Misc{$ENDIF},[TCalculator]);
  end;
{$ENDIF}

{ ----------------------------------------------------------------------------
  TCalculatorConstant
  ---------------------------------------------------------------------------- }

constructor TCalculatorConstant.Create(Collection: TCollection);
begin
  inherited;
  FName := '';
  FValue := 0;
end;

destructor TCalculatorConstant.Destroy;
begin
  //...
  inherited;
end;

procedure TCalculatorConstant.SetName(Value: String);
begin
  Value := LowerCase(Value);
  if FName = Value then
  begin
    Exit;
  end;
  if (Length(Value) <> 0) and ((Collection as TCalculatorConstants).IndexOf(Value) <> -1) or ((Collection as TCalculatorConstants).Calculator.Functions.IndexOf(Value) <> -1) then
  begin
    raise EIdentifierExists.Create('Identifier redefined: "' + Value + '"');
  end;
  FName := Value;
  DisplayName := Value;
end;

{ ----------------------------------------------------------------------------
  TCalculatorConstants
  ---------------------------------------------------------------------------- }

constructor TCalculatorConstants.Create(ItemClass: TCollectionItemClass; ACalculator: TCalculator);
begin
  inherited Create(ItemClass);
  FCalculator := ACalculator;
end;

destructor TCalculatorConstants.Destroy;
begin
  //...
  inherited;
end;

function TCalculatorConstants.IndexOf(Name: String): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Count - 1 do
  begin
    if (Items[Index] as TCalculatorConstant).Name = Name then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

{ ----------------------------------------------------------------------------
  TCalculator
  ---------------------------------------------------------------------------- }

constructor TCalculator.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TCalculator);
  FExpression := '';
  FValue := 0;
  FConstants := TCalculatorConstants.Create(TCalculatorConstant,Self);
  FFunctions := TStringList.Create;
  FOperations := [coAdd,coSub,coMul,coDiv];
  FDecimalSeparators := [dsPoint];
  FOptions := [coBrackets,coOperatorsOrder];
  FError := 0;
  SetLength(FTerms,0);
  //Naturkonstante "Pi" wird automatisch hinzugefügt
  with (Constants.Add as TCalculatorConstant) do
  begin
    Name := 'pi';
    Value := Pi;
  end;
end;

constructor TCalculator.Create(AOwner: TComponent; AExpression: String);
begin
  Create(AOwner);
  FExpression := AExpression;
end;

destructor TCalculator.Destroy;
begin
  FConstants.Free;
  FFunctions.Free;
  inherited;
end;

function TCalculator.GetFunctions: TStrings;
begin
  Result := FFunctions;
end;

procedure TCalculator.SetFunctions(Value: TStrings);
var
  Index: Integer;
begin
  if Value <> nil then
  begin
    for Index := 0 to Value.Count - 1 do
    begin
      Value.Strings[Index] := Lowercase(Value.Strings[Index]);
      if (Value.IndexOf(Value.Strings[Index]) <> Index) or (Constants.IndexOf(Value.Strings[Index]) <> -1) then
      begin
        raise EIdentifierExists.Create('Identifier redefined: "' + Value.Strings[Index] + '"');
      end;
    end;
  end;
  (FFunctions as TStringList).Assign(Value);
end;

function TCalculator.GetOperation(const Character: Char): TCalcOperation;
begin
  case Character of
  '+': Result := coAdd;
  '-': Result := coSub;
  '*': Result := coMul;
  '/': Result := coDiv;
  '%': Result := coMod;
  '^': Result := coExp;
  end;
end;

function TCalculator.CallConstant(const Name: String): Extended;
begin
  if Assigned(OnCalculate) then
  begin
    OnConstant(Self,Name);
  end;
  Result := (Constants.Items[Constants.IndexOf(Name)] as TCalculatorConstant).Value;
end;

function TCalculator.CallFunction(const Name: String; Parameter: Extended): Extended;
var
  Value: Extended;
begin
  Value := Parameter;
  if Assigned(OnCalculate) then
  begin
    OnFunction(Self,Name,Value);
  end;
  Result := Value;
end;

function TCalculator.CalcOperation(A,B: Extended; Operation: TCalcOperation): Extended;
begin
  case Operation of
    coAdd: Result := A + B;
    coSub: Result := A - B;
    coMul: Result := A * B;
    coDiv: Result := A / B;
    coMod: Result := FloatMod(A,B);
    coExp: Result := Power(A,B);
  end;
end;

procedure TCalculator.RaiseError(const Text: String; Index: PChar);
begin
  FError := CharPosition(Index,Expression);
  raise ECalculate.Create('Error at position ' + IntToStr(Error) + ': ' + Text);
end;

function TCalculator.Compile: Boolean;
var
  Current: PChar;
  Block: array of Char;
  WantNumber: Boolean;
  WantOperator: Boolean;
begin
  if Assigned(OnCalculate) then
  begin
    OnCalculate(Self);
  end;
  if Length(Expression) = 0 then
  begin
    Exit;
  end;
  Result := False;
  Current := @Expression[1];
  SetLength(FTerms,1);
  WantNumber := True;
  WantOperator := True;
  while Current^ <> #0 do
  begin
    if Current^ in Spaces then
    begin
      if Length(Block) <> 0 then
      begin
        WantNumber := False;
      end;
    end else
    begin
      if Current^ in Numbers + Letters then
      begin
        if WantNumber then
        begin
          SetLength(Block,Length(Block) + 1);
          Block[Length(Block)] := Current^;
        end else
        begin
          RaiseError('Missing operator',Current);
        end;
      end else
      begin
        if Current^ in CalcOperators then
        begin
          if WantOperator then
          begin
            Terms[Length(Terms)].Operation := GetOperation(Current^);
            WantOperator := False;
          end else
          begin
            RaiseError('Multiple operators',Current);
          end;
        end;
      end;
    end;
    Inc(Current);
  end;
  if WantNumber or WantOperator then
  begin
    RaiseError('Incomplete expression',Current);
  end;
  Result := True;
end;

procedure TCalculator.Calculate;
var
  Recent,Current,Last: ^TCalculatorTerm;
begin
  FValue := 0;
  if Length(Terms) = 0 then
  begin
    Exit;
  end;
  Last := @Terms[High(Terms)];
  Inc(Last);
  //   +1    -3    *5   [^2 ]  +2
  //   +1    -3   [*25]  ^4    +2
  //  [+1 ] [-75]  *25   ^4   [+2 ]
  if coOperatorsOrder in Options then
  begin
    //Exp
    Current := @Terms[0];
    while Current <> Last do
    begin
      if Current^.Operation = coExp then
      begin
        Recent^.Value := CalcOperation(Recent^.Value,Current^.Value,coExp);
      end else
      begin
        Recent := @Current^;
      end;
      Inc(Current);
    end;
    //Mul,Div,Mod
    Current := @Terms[0];
    while Current <> Last do
    begin
      if Current^.Operation in [coMul,coDiv,coMod] then
      begin
        Recent^.Value := CalcOperation(Recent^.Value,Current^.Value,Current^.Operation);
      end else
      begin
        if Current^.Operation <> coExp then
        begin
          Recent := @Current^;
        end;
      end;
      Inc(Current);
    end;
    //Add,Sub
    Current := @Terms[0];
    while Current <> Last do
    begin
      if Current^.Operation in [coAdd,coSub] then
      begin
        FValue := CalcOperation(Value,Current^.Value,Current^.Operation);
      end;
      Inc(Current);
    end;
  end else
  begin
    Current := @Terms[0];
    while Current <> Last do
    begin
      FValue := CalcOperation(Value,Current.Value,Current.Operation);
      Inc(Current);
    end;
  end;
end;

end.
