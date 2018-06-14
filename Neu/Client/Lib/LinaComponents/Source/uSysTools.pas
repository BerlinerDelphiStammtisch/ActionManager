unit uSysTools;

//////////////////////////////////////
///  Lina System Tools Unit        ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}
  {$POINTERMATH ON}

interface

uses
  { Standard-Units }
  SysUtils, Variants, Classes, Math, Windows, Graphics, Printers, TypInfo,
  Controls, Forms;

type
  { Fehlermeldungen }
  EWinUserInformation = class(Exception);
  EStringCharAccess = class(Exception);
  EInvalidBFCommand = class(Exception);
  EIntersection = class(Exception);

  { Hilfsklassen }
  TLinePosition = (lpAnyPosition,lpBeginning); //Mutter-Hilfsklasse für sämtliche Enums
  TStringFilterMode = type TLinePosition;
  TStringFilterOptions = set of (sfoCaseSensitive,sfoForceTrim,sfoDefaultVisible);
  TCharEncoding = (ceANSI,ceASCII,ceBigEndianUnicode,ceUnicode,ceUTF7,ceUTF8);
  {$IF !Declared(TVerticalAlignment}
    TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
  {$ENDIF}
  TCheckSum = (csBitCount,csFetcher);

  TBit = 0..1;

  { Typisierte Arrays }
  TPointerArray = array of Pointer;
  TPCharArray = array of PChar;
  TCharArray = array of Char;

  TVariantArray = array of Variant;

  TStringArray = array of String;
  TShortStringArray = array of ShortString;
  {$IFDEF NO_UNICODE}
    TAnsiStringArray = TStringArray;
  {$ELSE}
    TAnsiStringArray = array of AnsiString;
    TUnicodeStringArray = TStringArray;
  {$ENDIF}

  TBitArray = array of TBit;
  TByteArray = array of Byte;
  TUInt8Array = TByteArray;
  TWordArray = array of Word;
  TUInt16Array = TWordArray;
  TCardinalArray = array of Cardinal;
  TUInt32Array = TCardinalArray;
  TUInt64Array = array of UInt64;

  TShortIntArray = array of ShortInt;
  TInt8Array = TShortIntArray;
  TSmallIntArray = array of SmallInt;
  TInt16Array = TSmallIntArray;
  TIntegerArray = array of Integer;
  TInt32Array = TIntegerArray;
  TLongIntArray = TIntegerArray;
  TInt64Array = array of Int64;

  TBooleanArray = array of Boolean;
  TByteBoolArray = array of ByteBool;
  TWordBoolArray = array of WordBool;
  TLongBoolArray = array of LongBool;

  TFloatArray = array of Extended;
  TSingleArray = array of Single;
  TDoubleArray = array of Double;
  TRealArray = TDoubleArray;
  TExtendedArray = TFloatArray;

  TPointArray = array of TPoint;

  TByte = array [0..7] of TBit;

  TReferenceData = record
    Value: Pointer;
    Reference: PPointer;
  end;
  TReferenceDataArray = array of TReferenceData;
  PReferenceData = ^TReferenceData;
  PReferenceDataArray = ^TReferenceDataArray;
  TRefDataReferenceData = record
    Value: Pointer;
    Reference: PReferenceData;
  end;
  TRefDataReferenceDataArray = array of TRefDataReferenceData;
  TRefDataArrayReferenceData = record
    Value: Pointer;
    Reference: PReferenceDataArray;
  end;
  TRefDataArrayReferenceDataArray = array of TRefDataArrayReferenceData;

  TStringReferenceData = record
    Value: String;
    Reference: PString;
  end;
  TStringReferenceDataArray = array of TStringReferenceData;
  PStringReferenceData = ^TStringReferenceData;
  PStringReferenceDataArray = ^TStringReferenceDataArray;
  TStringRefDataReferenceData = record
    Value: String;
    Reference: PStringReferenceData;
  end;
  TStringRefDataReferenceDataArray = array of TStringRefDataReferenceData;
  TStringRefDataArrayReferenceData = record
    Value: String;
    Reference: PStringReferenceDataArray;
  end;
  TStringRefDataArrayReferenceDataArray = array of TStringRefDataArrayReferenceData;

  TIntegerReferenceData = record
    Value: Integer;
    Reference: PInteger;
  end;
  TIntegerReferenceDataArray = array of TIntegerReferenceData;
  PIntegerReferenceData = ^TIntegerReferenceData;
  PIntegerReferenceDataArray = ^TIntegerReferenceDataArray;
  TIntegerRefDataReferenceData = record
    Value: Integer;
    Reference: PIntegerReferenceData;
  end;
  TIntegerRefDataReferenceDataArray = array of TIntegerRefDataReferenceData;
  TIntegerRefDataArrayReferenceData = record
    Value: Integer;
    Reference: PIntegerReferenceDataArray;
  end;
  TIntegerRefDataArrayReferenceDataArray = array of TIntegerRefDataArrayReferenceData;

  TFloatReferenceData = record
    Value: Extended;
    Reference: PExtended;
  end;
  TFloatReferenceDataArray = array of TFloatReferenceData;
  PFloatReferenceData = ^TFloatReferenceData;
  PFloatReferenceDataArray = ^TFloatReferenceDataArray;
  TFloatRefDataReferenceData = record
    Value: Extended;
    Reference: PFloatReferenceData;
  end;
  TFloatRefDataReferenceDataArray = array of TFloatRefDataReferenceData;
  TFloatRefDataArrayReferenceData = record
    Value: Extended;
    Reference: PFloatReferenceDataArray;
  end;
  TFloatRefDataArrayReferenceDataArray = array of TFloatRefDataArrayReferenceData;

  TByteSet = set of Byte;
  TCharSet = set of Char;

  TVector   = TIntegerArray;
  TVector1D = array [1..1] of Integer;
  TVector2D = array [1..2] of Integer;
  TVector3D = array [1..3] of Integer;
  TMatrix = array of TVector;
  TMatrix1D = array of TVector1D;
  TMatrix2D = array of TVector2D;
  TMatrix3D = array of TVector3D;

  TVectorF   = TDoubleArray;
  TVectorF1D = array [1..1] of Double;
  TVectorF2D = array [1..2] of Double;
  TVectorF3D = array [1..3] of Double;
  TMatrixF = array of TVectorF;
  TMatrixF1D = array of TVectorF1D;
  TMatrixF2D = array of TVectorF2D;
  TMatrixF3D = array of TVectorF3D;

  TRGBTripleArray = array [Word] of TRGBTriple;
  PRGBTripleArray = ^TRGBTripleArray;

  TLonelyPointer = array [0..0] of Pointer;
  TLonelyVariant = array [0..0] of Variant;
  TLonelyChar = array [0..0] of Char;
  TLonelyString = array [0..0] of String;
  TLonelyShortString = array [0..0] of ShortString;
  TLonelyByte = array [0..0] of Byte;
  TLonelyWord = array [0..0] of Word;
  TLonelyCardinal = array [0..0] of Cardinal;
  TLonelyUInt64 = array [0..0] of UInt64;
  TLonelyShortInt = array [0..0] of ShortInt;
  TLonelySmallInt = array [0..0] of SmallInt;
  TLonelyInteger = array [0..0] of Integer;
  TLonelyInt64 = array [0..0] of Int64;
  TLonelySingle = array [0..0] of Extended;
  TLonelyDouble = array [0..0] of Double;
  TLonelyExtended = array [0..0] of Extended;

  TPointerPair = array [0..1] of Pointer;
  TVariantPair = array [0..1] of Variant;
  TCharPair = array [0..1] of Char;
  TStringPair = array [0..1] of String;
  TShortStringPair = array [0..1] of ShortString;
  TBytePair = array [0..1] of Byte;
  TWordPair = array [0..1] of Word;
  TCardinalPair = array [0..1] of Cardinal;
  TUInt64Pair = array [0..1] of UInt64;
  TShortIntPair = array [0..1] of ShortInt;
  TSmallIntPair = array [0..1] of SmallInt;
  TIntegerPair = array [0..1] of Integer;
  TInt64Pair = array [0..1] of Int64;
  TSinglePair = array [0..1] of Extended;
  TDoublePair = array [0..1] of Double;
  TExtendedPair = array [0..1] of Extended;

  TLine = class
  private
    { Private-Deklarationen }
    FOffset: Integer;
    FSlope: Extended;
    function GetY(X: Integer): Integer;
  public
    { Public-Deklarationen }
    constructor Create(AOffset: Integer; ASlope: Extended); overload;
    constructor Create(A: TPoint; ASlope: Extended); overload;
    constructor Create(A,B: TPoint); overload;
    destructor Destroy; override;
    procedure LoadFromPoints(A,B: TPoint);
    function Contains(Point: TPoint): Boolean;
    function Intersection(Line: TLine): TPoint;
    function Intersects(Line: TLine): Boolean;
    function Equals(Line: TLine): Boolean;
    function Parallel(Line: TLine): Boolean;
    property Offset: Integer read FOffset write FOffset default 0;
    property Slope: Extended read FSlope write FSlope;
    property Y[X: Integer]: Integer read GetY; default;
  end;

  TCicle = class
  private
    { Private-Deklarationen }
    FCenter: TPoint;
    FRadius: Integer;
    { Methoden }
    function GetDiameter: Integer;
    procedure SetDiameter(Value: Integer);
    function GetCircumference: Extended;
    procedure SetCircumference(Value: Extended);
  public
    { Public-Deklarationen }
    constructor Create(ACenter: TPoint; ARadius: Integer); overload;
    constructor Create(ACenter,ABorder: TPoint); overload;
    destructor Destroy; override;
    function Contains(Point: TPoint): Boolean; overload;
    function Contains(Cicle: TCicle): Boolean; overload;
    function Intersection(Line: TLine): TPoint; overload;
    function Intersection(Cicle: TCicle): TPoint; overload;
    function Intersects(Line: TLine): Boolean; overload;
    function Intersects(Cicle: TCicle): Boolean; overload;
    function Equals(Cicle: TCicle): Boolean;
    property Center: TPoint read FCenter write FCenter;
    property Radius: Integer read FRadius write FRadius;
    property Diameter: Integer read GetDiameter write SetDiameter;
    property Circumference: Extended read GetCircumference write SetCircumference;
  end;

  TRange = class
  private
    { Private-Deklarationen }
    FOffset: Integer;
    FTarget: Integer;
    FStep: Integer;
    { Methoden }
    procedure SetOffset(Value: Integer);
    procedure SetTarget(Value: Integer);
    function GetLength: Integer;
    procedure SetLength(Value: Integer);
    function GetCount: Integer;
    function GetElements: TIntegerArray;
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
    property Offset: Integer read FOffset write SetOffset default 0;
    property Target: Integer read FTarget write SetTarget default 0;
    property Length: Integer read GetLength write SetLength default 0;
    property Count: Integer read GetCount default 0;
    property Elements: TIntegerArray read GetElements;
    property Step: Integer read FStep write FStep default 1;
  end;

  TField = class
  private
    { Private-Deklarationen }
    FPoints: array of TPoint;
    FHighest: Integer;
    FLowest: Integer;
    FLeftest: Integer;
    FRightest: Integer;
    function GetPoints(Index: Integer): TPoint;
    procedure SetPoints(Index: Integer; Value: TPoint);
    function GetPointCount: Integer;
    function GetRect: TRect;
    function GetHighest: TPoint;
    function GetLowest: TPoint;
    function GetLeftest: TPoint;
    function GetRightest: TPoint;
    function GetMinDistance: TPoint;
    function GetMaxDistance: TPoint;
    function GetAverage: TPoint;
  public
    { Public-Deklarationen }
    constructor Create; overload;
    constructor Create(APoints: array of TPoint); overload;
    destructor Destroy; override;
    function Add(Point: TPoint): Integer;
    function AddPoints(APoints: array of TPoint): Integer;
    procedure Delete(Index: Integer);
    procedure Remove(Point: TPoint);
    function IndexOf(Point: TPoint): Integer;
    function Contains(Point: TPoint): Boolean;
    property Points[Index: Integer]: TPoint read GetPoints write SetPoints;
    property PointCount: Integer read GetPointCount;
    property Rect: TRect read GetRect;
    property Highest: TPoint read GetHighest;
    property Lowest: TPoint read GetLowest;
    property Leftest: TPoint read GetLeftest;
    property Rightest: TPoint read GetRightest;
    property Average: TPoint read GetAverage;
    property MinDistance: TPoint read GetMinDistance;
    property MaxDistance: TPoint read GetMaxDistance;
  end;

  (*TIntegerList = class(TPersistent)
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;*)

  TFilteredStringList = class(TStringList)
  private
    { Private-Deklarationen }
    FFiltered: Boolean;
    FFilter: String;
    FFilteredStrings: TStrings;
    FFilterMode: TStringFilterMode;
    FFilterOptions: TStringFilterOptions;
    { Methoden }
    function GetFilteredStrings: TStrings;
  protected
    { Protected-Deklarationen }
    procedure FilterUpdate;
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
    property Filtered: Boolean read FFiltered write FFiltered default True;
    property Filter: String read FFilter write FFilter;
    property FilteredStrings: TStrings read GetFilteredStrings;
    property FilterMode: TStringFilterMode read FFilterMode write FFilterMode default lpBeginning;
    property FilterOptions: TStringFilterOptions read FFilterOptions write FFilterOptions default [sfoCaseSensitive,sfoForceTrim,sfoDefaultVisible];
  end;

  { Typenumwandelungen }
  function BoolToInt(B: Boolean): Integer; inline;
  function IntToBool(Value: Integer): Boolean; inline;
  { Typen-Äquivalenz-Prüfungen }
  function StrIsInt(const S: String): Boolean;
  function StrIsFloat(const S: String): Boolean;
  function StrIsBool(const S: String): Boolean;
  function FloatIsInt(Value: Extended): Boolean;
  { WinUser }
  function WinUserName: String;
  function WinUserDirectory: String;
  function WinUserAdmin: Boolean;
  function WinUserExists(UsrNme: String): Boolean;
  { Array-Position }
  function ArrayPos(const AValue; const AArray: array of const): Integer; overload;
  function ArrayPos(const AValue: Variant; const AArray: array of Variant): Integer; overload;
  function ArrayPos(const AValue: Pointer; const AArray: array of Pointer): Integer; overload;
  function ArrayPos(const AValue: Char; const AArray: array of Char): Integer; overload;
  function ArrayPos(const AValue: ShortString; const AArray: array of ShortString): Integer; overload;
  function ArrayPos(const AValue: String; const AArray: array of String): Integer; overload;
  function ArrayPos(const AValue: ShortInt; const AArray: array of ShortInt): Integer; overload;
  function ArrayPos(const AValue: SmallInt; const AArray: array of SmallInt): Integer; overload;
  function ArrayPos(const AValue: Integer; const AArray: array of Integer): Integer; overload;
  function ArrayPos(const AValue: Int64; const AArray: array of Int64): Integer; overload;
  function ArrayPos(const AValue: Byte; const AArray: array of Byte): Integer; overload;
  function ArrayPos(const AValue: Word; const AArray: array of Word): Integer; overload;
  function ArrayPos(const AValue: Cardinal; const AArray: array of Cardinal): Integer; overload;
  function ArrayPos(const AValue: UInt64; const AArray: array of UInt64): Integer; overload;
  function ArrayPos(const AValue: Single; const AArray: array of Single): Integer; overload;
  function ArrayPos(const AValue: Double; const AArray: array of Double): Integer; overload;
  function ArrayPos(const AValue: Real; const AArray: array of Real): Integer; overload;
  function ArrayPos(const AValue: Extended; const AArray: array of Extended): Integer; overload;
  function ArrayPosRef(const AValue: Pointer; const AArray: array of TReferenceData): Integer; overload;
  function ArrayPosRef(const AValue: String; const AArray: array of TStringReferenceData; IgnoreCase: Boolean = False): Integer; overload;
  function ArrayPosRef(const AValue: Integer; const AArray: array of TIntegerReferenceData): Integer; overload;
  function ArrayPosRef(const AValue: Extended; const AArray: array of TFloatReferenceData): Integer; overload;
  function ArrayPosRef(const AValue: Pointer; const AArray: array of TRefDataArrayReferenceData): Integer; overload;
  function ArrayPosRef(const AValue: String; const AArray: array of TStringRefDataArrayReferenceData; IgnoreCase: Boolean = False): Integer; overload;
  function ArrayPosRef(const AValue: Integer; const AArray: array of TIntegerRefDataArrayReferenceData): Integer; overload;
  function ArrayPosRef(const AValue: Extended; const AArray: array of TFloatRefDataArrayReferenceData): Integer; overload;
  function ArrayPosType(AValue: TClass; AArray: array of TClass): Integer; overload;
  function ArrayPosType(AValue: TClass; AArray: array of TObject): Integer; overload;
  { Array-Element Löschen }
  procedure ArrayDelete(var AArray: TVariantArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TCharArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TShortStringArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TStringArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TShortIntArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TSmallIntArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TIntegerArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TInt64Array; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TByteArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TWordArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TCardinalArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TUInt64Array; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TSingleArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TDoubleArray; Index: Integer; Count: Integer); overload;
  procedure ArrayDelete(var AArray: TExtendedArray; Index: Integer; Count: Integer); overload;
  { TComponent Laden/Speichern }
  procedure ComponentSaveToFile(const FileName: String; Component: TComponent);
  procedure ComponentLoadFromFile(const FileName: String; Component: TComponent);
  procedure ComponentSaveToStream(var Stream: TStream; Component: TComponent);
  procedure ComponentLoadFromStream(Stream: TStream; Component: TComponent);
  { Char-Case-Umwandelungen }
  function LowCase(Ch: Char): Char;
  function CharLowerCase(Character: Char): Char;
  function CharUpperCase(Character: Char): Char;
  { Null-/Plus-Minus-Unendlich- Annäherung }
  procedure ToZero(var X: Integer);
  procedure ToInf(var X: Integer);
  { Gradzahlen-Normalisierer }
  procedure NormalizeDeg(var X: Integer); overload;
  procedure NormalizeDeg(var X: Single); overload;
  procedure NormalizeDeg(var X: Double); overload;
  procedure NormalizeDeg(var X: Extended); overload;
  { PQ-Formel }
  function PQFormula(P,Q: Single): TSingleArray; overload;
  function PQFormula(P,Q: Double): TDoubleArray; overload;
  function PQFormula(P,Q: Extended): TExtendedArray; overload;
  { Gleitkomma-Modulo }
  function FloatMod(X,Y: Single): Single; overload;
  function FloatMod(X,Y: Double): Double; overload;
  function FloatMod(X,Y: Extended): Extended; overload;
  { Ganzzahliges Exponenzieren }
  function IntPow(Base: ShortInt; Exponent: Byte): Int64; overload;
  function IntPow(Base: SmallInt; Exponent: Byte): Int64; overload;
  function IntPow(Base: Integer; Exponent: Byte): Int64; overload;
  function IntPow(Base: Int64; Exponent: Byte): Int64; overload;
  { GGT ("GCD") / KGV ("LCM") }
  function GCD(A,B: Byte): Byte; overload;
  function GCD(A,B: Word): Word; overload;
  function GCD(A,B: Cardinal): Cardinal; overload;
  function LCM(A,B: Byte): Word; overload;
  function LCM(A,B: Word): Cardinal; overload;
  function LCM(A,B: Cardinal): Cardinal; overload;
  { Summenformel }
  function SumOf(Value: Byte; Offset: Byte = 1): Word; overload;
  function SumOf(Value: Word; Offset: Word = 1): Cardinal; overload;
  function SumOf(Value: Cardinal; Offset: Cardinal = 1): Cardinal; overload;
  { Addition numerischer Arrays }
  function ArrayAdd(A,B: array of ShortInt): TShortIntArray; overload;
  function ArrayAdd(A,B: array of SmallInt): TSmallIntArray; overload;
  function ArrayAdd(A,B: array of Integer): TIntegerArray; overload;
  function ArrayAdd(A,B: array of Int64): TInt64Array; overload;
  function ArrayAdd(A,B: array of Byte): TByteArray; overload;
  function ArrayAdd(A,B: array of Word): TWordArray; overload;
  function ArrayAdd(A,B: array of Cardinal): TCardinalArray; overload;
  function ArrayAdd(A,B: array of Single): TSingleArray; overload;
  function ArrayAdd(A,B: array of Double): TDoubleArray; overload;
  function ArrayAdd(A,B: array of Extended): TExtendedArray; overload;
  { Sortier-Algorithmen }
  procedure BubbleSort(var Elements: array of String); overload;
  procedure BubbleSort(var Elements: array of ShortInt); overload;
  procedure BubbleSort(var Elements: array of SmallInt); overload;
  procedure BubbleSort(var Elements: array of Integer); overload;
  procedure BubbleSort(var Elements: array of Int64); overload;
  procedure BubbleSort(var Elements: array of Byte); overload;
  procedure BubbleSort(var Elements: array of Word); overload;
  procedure BubbleSort(var Elements: array of Cardinal); overload;
  procedure BubbleSort(var Elements: array of Single); overload;
  procedure BubbleSort(var Elements: array of Double); overload;
  procedure BubbleSort(var Elements: array of Extended); overload;
  { RTTI-Werzeuge }
  function GetSubPropInfo(Instance: TObject; const PropName: String; AKinds: TTypeKinds = []): PPropInfo;
  function GetObjectSubProp(Instance: TObject; const PropName: String): TObject;
  procedure SetObjectSubProp(Instance: TObject; const PropName: String; Value: TObject);
  function GetVariantSubProp(Instance: TObject; const PropName: String): Variant;
  procedure SetVariantSubProp(Instance: TObject; const PropName: String; Value: Variant);
  function GetStrSubProp(Instance: TObject; const PropName: String): String;
  procedure SetStrSubProp(Instance: TObject; const PropName: String; Value: String);
  {$IFNDEF NO_UNICODE}
    function GetAnsiStrSubProp(Instance: TObject; const PropName: String): AnsiString;
    procedure SetAnsiStrSubProp(Instance: TObject; const PropName: String; Value: AnsiString);
  {$ENDIF}
  function GetInt64SubProp(Instance: TObject; const PropName: String): Int64;
  procedure SetInt64SubProp(Instance: TObject; const PropName: String; Value: Int64);
  function GetFloatSubProp(Instance: TObject; const PropName: String): Extended;
  procedure SetFloatSubProp(Instance: TObject; const PropName: String; Value: Extended);
  function GetOrdSubProp(Instance: TObject; const PropName: String): NativeInt;
  procedure SetOrdSubProp(Instance: TObject; const PropName: String; Value: NativeInt);
  function GetEnumSubProp(Instance: TObject; const PropName: String): String;
  procedure SetEnumSubProp(Instance: TObject; const PropName: String; Value: String);
  function GetSetSubProp(Instance: TObject; const PropName: String): String;
  procedure SetSetSubProp(Instance: TObject; const PropName: String; Value: String);
  function GetDynArraySubProp(Instance: TObject; const PropName: String): Pointer;
  procedure SetDynArraySubProp(Instance: TObject; const PropName: String; Value: Pointer);
  function GetInterfaceSubProp(Instance: TObject; const PropName: String): IInterface;
  procedure SetInterfaceSubProp(Instance: TObject; const PropName: String; Value: IInterface);
  function GetMethodSubProp(Instance: TObject; const PropName: String): TMethod;
  procedure SetMethodSubProp(Instance: TObject; const PropName: String; Value: TMethod);
  function SubPropIsType(Instance: TObject; const PropName: String; TypeKind: TTypeKind): Boolean;
  function SubPropType(Instance: TObject; const PropName: String): TTypeKind;
  function IsPublishedSubProp(Instance: TObject; const PropName: String): Boolean;
  { Datum/Uhrzeit }
  function SystemTime: TSystemTime;
  function Year: Word;
  function Month: Word;
  function DayOfWeek: Word;
  function Day: Word;
  function Hour: Word;
  function Minute: Word;
  function Second: Word;
  function Milliseconds: Word;
  { Set-Operationen }
  function Count(Elements: TByteSet): Byte; overload;
  function Count(Elements: TCharSet): Byte; overload;
  function SetToArray(Elements: TByteSet): TByteArray; overload;
  function SetToArray(Elements: TCharSet): TCharArray; overload;
  function ArrayToSet(Elements: array of Byte): TByteSet; overload;
  function ArrayToSet(Elements: array of Char): TCharSet; overload;
  function SetToBools(Elements: TByteSet): TBooleanArray; overload;
  function SetToBools(Elements: TCharSet): TBooleanArray; overload;
  { Primzahlen-Bestimmung }
  function Prime(X: ShortInt): Boolean; overload;
  function Prime(X: Byte): Boolean; overload;
  function Prime(X: Integer): Boolean; overload;
  function Prime(X: Cardinal): Boolean; overload;
  { Bedingungsroutinen }
  function IfThenElse(Condition: Boolean; TrueValue, FalseValue: Variant): Variant; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Variant): Variant; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Pointer; FalseValue: Pointer = nil): Pointer; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: PChar; FalseValue: PChar = nil): PChar; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Char; FalseValue: Char = #0): Char; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: String; FalseValue: String = ''): String; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: ShortString; FalseValue: ShortString = ''): ShortString; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Byte; FalseValue: Byte = 0): Byte; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Word; FalseValue: Word = 0): Word; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Cardinal; FalseValue: Cardinal = 0): Cardinal; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: UInt64; FalseValue: UInt64 = 0): UInt64; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: ShortInt; FalseValue: ShortInt = 0): ShortInt; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: SmallInt; FalseValue: SmallInt = 0): SmallInt; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Integer; FalseValue: Integer = 0): Integer; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Int64; FalseValue: Int64 = 0): Int64; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Currency; FalseValue: Currency = 0): Currency; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Single; FalseValue: Single = 0): Single; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Double; FalseValue: Double = 0): Double; overload; inline;
  function IfThenElse(Condition: Boolean; TrueValue: Extended; FalseValue: Extended = 0): Extended; overload; inline;
  { Sonstige }
  function PointAdd(A,B: TPoint): TPoint;
  function PointSub(A,B: TPoint): TPoint;
  function Diagonal(A,B: TPoint): Extended;
  function Distance(A,B: TPoint): Cardinal;
  function Congruent(A,B,Divisor: Integer): Boolean; overload;
  function Congruent(A,B,Divisor: Extended): Boolean; overload;
  procedure ExtEuclideanAlg(A,B: Integer; var D,S,T: Integer);
  function StringToRange(const S: String; var Range: TRange): Boolean;
  function ExprInStr(const S: String; Position: Integer): String;
  function Factional(X: Byte): Cardinal;
  function ExtractClassName(FullClassName: String; CaseSensitive: Boolean = False): String;
  function CountLines(S: String): Integer;
  function CountLine(S: String; Line: Integer): Integer;
  function Wrappable(S: String; Canvas: TCanvas; MaxWidth: Integer): Boolean;
  function WrappedTextHeight(S: String; Canvas: TCanvas; MaxWidth: Integer): Integer;
  function CharEncoding(EncodingClass: TEncoding): TCharEncoding;
  function EncodingClass(CharEncoding: TCharEncoding): TEncoding;
  function SecToTime(const Sec: Cardinal): TTime;
  function GetExecTime(Command: Pointer; Amount: Cardinal; Attempts: Cardinal = 1): Cardinal;
  function SystemLanguage: String;
  function ExtractUserName(const Owner: String): String;
  function ExtractUserDomain(const Owner: String): String;
  function RoundPos(Number: Single; Pos: Byte): Single;
  function FontSizeToHeight(Size: Integer; PpI: Integer): Integer;
  function FontHeightToSize(Height: Integer; PpI: Integer): Integer;
  function ComponentByTag(Owner: TComponent; const Tag: Integer): TComponent;
  function ControlIndex(Control: TControl): Integer;
  function IntToStrMinLength(Value: Integer; MinLength: SmallInt): String;
  function BmpRect(Bitmap: TBitmap): TRect;
  function GetOwnerForm(Component: TComponent): TCustomForm;
  function MultiPos(const SubStr, Str: ShortString; Offset: Integer = 1): TIntegerArray; overload;
  function MultiPos(const SubStr, Str: String; Offset: Integer = 1): TIntegerArray; overload;
  function CharLine(Current: PAnsiChar; Text: AnsiString): Integer; {$IFNDEF NO_UNICODE} overload;
    function CharLine(Current: PWideChar; Text: UnicodeString): Integer; overload;
  {$ENDIF}
  function CharPosition(Current: PAnsiChar; Text: AnsiString): Integer; {$IFNDEF NO_UNICODE} overload;
    function CharPosition(Current: PWideChar; Text: UnicodeString): Integer; overload;
  {$ENDIF}
  function ConsistsOf(const S: String; Chars: array of Char): Boolean; overload;
  function ConsistsOf(const S: String; Chars: TCharSet): Boolean; overload;
  function ArrayToString(AArray: array of const): String;
  function BitsToBytes(Bits: array of TBit): TByteArray;
  function BytesToBits(Bytes: array of Byte): TBitArray;
  function CheckSum(Bytes: array of Byte; Algorith: TCheckSum): Word;
  procedure IndentBlock(var S: String; Indents: Byte; const Indent: Char = ' ');
  procedure Exchange(var X,Y); inline;
  procedure PrintText(Strings: TStrings; Font: TFont);
  procedure BFInterpret(const S: String; var P: Pointer); overload;
  procedure BFInterpret(const S: String; var P: Pointer; var ReadBuffer, WriteBuffer: TIntegerArray); overload;
  procedure ExtractChars(var Text: String; Chars: array of Char); overload;
  procedure ExtractChars(var Text: String; Chars: TCharSet); overload;
  procedure DeleteChars(var Text: String; Chars: array of Char); overload;
  procedure DeleteChars(var Text: String; Chars: TCharSet); overload;
  procedure SetPrivilege(const Name: PChar; Value: Boolean); overload;
  procedure SetPrivilege(const Name: String; Value: Boolean); overload;

const
  Spaces = [#9,#10,#13,#32,#160];
  Numbers = ['0'..'9'];
  UpperCaseLetters = ['A'..'Z'];
  LowerCaseLetters = ['a'..'z'];
  Letters = UpperCaseLetters + LowerCaseLetters + ['_'];
  Operators = ['+','-','*','/'];
  { WinAPI-Privilegien }
  SE_ASSIGNPRIMARYTOKEN_NAME = 'SeAssignPrimaryTokenPrivilege';
  SE_AUDIT_NAME = 'SeAuditPrivilege';
  SE_BACKUP_NAME = 'SeBackupPrivilege';
  SE_CHANGE_NOTIFY_NAME = 'SeChangeNotifyPrivilege';
  SE_CREATE_GLOBAL_NAME = 'SeCreateGlobalPrivilege';
  SE_CREATE_PAGEFILE_NAME = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME = 'SeCreatePermanentPrivilege';
  SE_CREATE_SYMBOLIC_LINK_NAME = 'SeCreateSymbolicLinkPrivilege';
  SE_CREATE_TOKEN_NAME = 'SeCreateTokenPrivilege';
  SE_DEBUG_NAME = 'SeDebugPrivilege';
  SE_ENABLE_DELEGATION_NAME = 'SeEnableDelegationPrivilege';
  SE_IMPERSONATE_NAME = 'SeImpersonatePrivilege';
  SE_INC_BASE_PRIORITY_NAME = 'SeIncreaseBasePriorityPrivilege';
  SE_INCREASE_QUOTA_NAME = 'SeIncreaseQuotaPrivilege';
  SE_INC_WORKING_SET_NAME = 'SeIncreaseWorkingSetPrivilege';
  SE_LOAD_DRIVER_NAME = 'SeLoadDriverPrivilege';
  SE_LOCK_MEMORY_NAME = 'SeLockMemoryPrivilege';
  SE_MACHINE_ACCOUNT_NAME = 'SeMachineAccountPrivilege';
  SE_MANAGE_VOLUME_NAME = 'SeManageVolumePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_RELABEL_NAME = 'SeRelabelPrivilege';
  SE_REMOTE_SHUTDOWN_NAME = 'SeRemoteShutdownPrivilege';
  SE_RESTORE_NAME = 'SeRestorePrivilege';
  SE_SECURITY_NAME = 'SeSecurityPrivilege';
  SE_SHUTDOWN_NAME = 'SeShutdownPrivilege';
  SE_SYNC_AGENT_NAME = 'SeSyncAgentPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME = 'SeSystemEnvironmentPrivilege';
  SE_SYSTEM_PROFILE_NAME = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME = 'SeSystemtimePrivilege';
  SE_TAKE_OWNERSHIP_NAME = 'SeTakeOwnershipPrivilege';
  SE_TCB_NAME = 'SeTcbPrivilege';
  SE_TIME_ZONE_NAME = 'SeTimeZonePrivilege';
  SE_TRUSTED_CREDMAN_ACCESS_NAME = 'SeTrustedCredManAccessPrivilege';
  SE_UNDOCK_NAME = 'SeUndockPrivilege';
  SE_UNSOLICITED_INPUT_NAME = 'SeUnsolicitedInputPrivilege';

implementation

uses
  uFileTools;

function BoolToInt(B: Boolean): Integer; inline;
begin
  Result := -Integer(B);
end;

function IntToBool(Value: Integer): Boolean; inline;
begin
  Result := (Value <> 0);
end;

function StrIsInt(const S: String): Boolean;
var
  TryMethodBuffer: Integer;
begin
  Result := TryStrToInt(S,TryMethodBuffer);
end;

function StrIsFloat(const S: String): Boolean;
var
  TryMethodBuffer: Extended;
begin
  Result := TryStrToFloat(S,TryMethodBuffer);
end;

function StrIsBool(const S: String): Boolean;
var
  TryMethodBuffer: Boolean;
begin
  Result := TryStrToBool(S,TryMethodBuffer);
end;

function FloatIsInt(Value: Extended): Boolean;
begin
  Result := (Frac(Value) = 0);
end;

procedure ComponentSaveToFile(const FileName: String; Component: TComponent);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName,fmCreate);
  try
    FS.WriteComponentRes(Component.Name,Component);
  finally
    FS.Free;
  end;
end;

procedure ComponentLoadFromFile(const FileName: String; Component: TComponent);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  try
    FS.ReadComponentRes(Component);
  finally
    FS.Free;
  end;
end;

procedure ComponentSaveToStream(var Stream: TStream; Component: TComponent);
begin
  Stream.WriteComponentRes(Component.Name,Component);
end;

procedure ComponentLoadFromStream(Stream: TStream; Component: TComponent);
begin
  Stream.ReadComponentRes(Component);
end;

function LowCase(Ch: Char): Char;
begin
  Result := CharLowerCase(Ch);
end;

function CharLowerCase(Character: Char): Char;
{ Basierend auf der Technik von SysUtils.LowerCase, nur simpler/schneller }
begin
  if Character in ['A'..'Z'] then
  begin
    Result := Char(Word(Character) or $0020);
  end else
  begin
    Result := Character;
  end;
end;

function CharUpperCase(Character: Char): Char;
{ Basierend auf der Technik von SysUtils.UpperCase, nur simpler/schneller }
begin
  if Character in ['a'..'z'] then
  begin
    Result := Char(Word(Character) xor $0020);
  end else
  begin
    Result := Character;
  end;
end;

procedure ToZero(var X: LongInt);
{$IFDEF PUREPASCAL}
begin
  if X < 0 then
  begin
    Inc(X);
  end;
  if X > 0 then
  begin
    Dec(X);
  end;
end;
{$ELSE}
asm
    CMP [EAX],0
    JE @Zer
    JL @Neg
    SUB [EAX],1
    RET
  @Neg:
    ADD [EAX],1
  @Zer:
end;
{$ENDIF}

procedure ToInf(var X: LongInt);
{$IFDEF PUREPASCAL}
begin
  if X < 0 then
  begin
    Dec(X);
  end;
  if X > 0 then
  begin
    Inc(X);
  end;
end;
{$ELSE}
asm
    CMP [EAX],0
    JE @Zer
    JL @Neg
    ADD [EAX],1
    RET
  @Neg:
    SUB [EAX],1
  @Zer:
end;
{$ENDIF}

procedure NormalizeDeg(var X: Integer);
{$IFDEF PUREPASCAL}
begin
  while X >= 360 do
  begin
    X := X - 360;
  end;
  while X < 0 do
  begin
    X := X + 360;
  end;
end;
{$ELSE}
asm
  @Cmp_Low:
    CMP [EAX],0
    JL @Low
  @Cmp_High:
    CMP [EAX],360
    JGE @High
    RET
  @Low:
    ADD [EAX],360
    JMP @Cmp_Low
  @High:
    SUB [EAX],360
    JMP @Cmp_High
end;
{$ENDIF}

procedure NormalizeDeg(var X: Single); overload;
begin
  while X >= 360 do
  begin
    X := X - 360;
  end;
  while X < 0 do
  begin
    X := X + 360;
  end;
end;

procedure NormalizeDeg(var X: Double); overload;
begin
  while X >= 360 do
  begin
    X := X - 360;
  end;
  while X < 0 do
  begin
    X := X + 360;
  end;
end;

procedure NormalizeDeg(var X: Extended); overload;
begin
  while X >= 360 do
  begin
    X := X - 360;
  end;
  while X < 0 do
  begin
    X := X + 360;
  end;
end;

function PQFormula(P,Q: Single): TSingleArray; overload;
var
  Root: Single;
begin
  Root := Sqr(P / 2) - Q;
  if Root < 0 then
  begin
    SetLength(Result,0);
  end else
  begin
    if Root = 0 then
    begin
      SetLength(Result,1);
    end else
    begin
      SetLength(Result,2);
      Result[1] := -(P / 2) - Sqrt(Root);
    end;
    Result[0] := -(P / 2) + Sqrt(Root);
  end;
end;

function PQFormula(P,Q: Double): TDoubleArray; overload;
var
  Root: Double;
begin
  Root := Sqr(P / 2) - Q;
  if Root < 0 then
  begin
    SetLength(Result,0);
  end else
  begin
    if Root = 0 then
    begin
      SetLength(Result,1);
    end else
    begin
      SetLength(Result,2);
      Result[1] := -(P / 2) - Sqrt(Root);
    end;
    Result[0] := -(P / 2) + Sqrt(Root);
  end;
end;

function PQFormula(P,Q: Extended): TExtendedArray; overload;
var
  Root: Extended;
begin
  Root := Sqr(P / 2) - Q;
  if Root < 0 then
  begin
    SetLength(Result,0);
  end else
  begin
    if Root = 0 then
    begin
      SetLength(Result,1);
    end else
    begin
      SetLength(Result,2);
      Result[1] := -(P / 2) - Sqrt(Root);
    end;
    Result[0] := -(P / 2) + Sqrt(Root);
  end;
end;

function FloatMod(X,Y: Single): Single; overload;
begin
  Result := X - Y * Trunc(X / Y);
end;

function FloatMod(X,Y: Double): Double; overload;
begin
  Result := X - Y * Trunc(X / Y);
end;

function FloatMod(X,Y: Extended): Extended; overload;
begin
  Result := X - Y * Trunc(X / Y);
end;

function IntPow(Base: ShortInt; Exponent: Byte): Int64; overload;
begin
  Result := 1;
  while Exponent > 0 do
  begin
    Result := Result * Base;
    Dec(Exponent);
  end;
end;

function IntPow(Base: SmallInt; Exponent: Byte): Int64; overload;
begin
  Result := 1;
  while Exponent > 0 do
  begin
    Result := Result * Base;
    Dec(Exponent);
  end;
end;

function IntPow(Base: Integer; Exponent: Byte): Int64; overload;
begin
  Result := 1;
  while Exponent > 0 do
  begin
    Result := Result * Base;
    Dec(Exponent);
  end;
end;

function IntPow(Base: Int64; Exponent: Byte): Int64; overload;
begin
  Result := 1;
  while Exponent > 0 do
  begin
    Result := Result * Base;
    Dec(Exponent);
  end;
end;

function GCD(A,B: Byte): Byte; overload;
begin
  if B <> 0 then
  begin
    if A > B then
    begin
      Result := B;
      B := A;
      A := Result;
    end else
    begin
      if A <> 0 then
      begin
        Result := A;
      end else
      begin
        Result := 0;
        Exit;
      end;
    end;
    while (A mod Result <> 0) or (B mod Result <> 0) do
    begin
      Dec(Result);
    end;
  end else
  begin
    Result := 0;
  end;
end;

function GCD(A,B: Word): Word; overload;
begin
  if B <> 0 then
  begin
    if A > B then
    begin
      Result := B;
      B := A;
      A := Result;
    end else
    begin
      if A <> 0 then
      begin
        Result := A;
      end else
      begin
        Result := 0;
        Exit;
      end;
    end;
    while (A mod Result <> 0) or (B mod Result <> 0) do
    begin
      Dec(Result);
    end;
  end else
  begin
    Result := 0;
  end;
end;

function GCD(A,B: Cardinal): Cardinal; overload;
begin
  if B <> 0 then
  begin
    if A > B then
    begin
      Result := B;
      B := A;
      A := Result;
    end else
    begin
      if A <> 0 then
      begin
        Result := A;
      end else
      begin
        Result := 0;
        Exit;
      end;
    end;
    while (A mod Result <> 0) or (B mod Result <> 0) do
    begin
      Dec(Result);
    end;
  end else
  begin
    Result := 0;
  end;
end;

function LCM(A,B: Byte): Word; overload;
begin
  Result := GCD(A,B);
  if Result <> 0 then
  begin
    Result := A * B div Result;
  end;
end;

function LCM(A,B: Word): Cardinal; overload;
begin
  Result := GCD(A,B);
  if Result <> 0 then
  begin
    Result := A * B div Result;
  end;
end;

function LCM(A,B: Cardinal): Cardinal; overload;
begin
  Result := GCD(A,B);
  if Result <> 0 then
  begin
    Result := A * B div Result;
  end;
end;

function SumOf(Value: Byte; Offset: Byte = 1): Word;
begin
  if Offset < Value then
  begin
    Result := (Sqr(Value) + Value) div 2 - SumOf(Offset - 1);
  end else
  begin
    Result := 0;
  end;
end;

function SumOf(Value: Word; Offset: Word = 1): Cardinal;
begin
  if Offset < Value then
  begin
    Result := (Sqr(Value) + Value) div 2 - SumOf(Offset - 1);
  end else
  begin
    Result := 0;
  end;
end;

function SumOf(Value: Cardinal; Offset: Cardinal = 1): Cardinal;
begin
  if Offset < Value then
  begin
    Result := (Sqr(Value) + Value) div 2 - SumOf(Offset - 1);
  end else
  begin
    Result := 0;
  end;
end;

function ArrayAdd(A,B: array of ShortInt): TShortIntArray;
begin

end;

function ArrayAdd(A,B: array of SmallInt): TSmallIntArray;
begin

end;

function ArrayAdd(A,B: array of Integer): TIntegerArray;
begin

end;

function ArrayAdd(A,B: array of Int64): TInt64Array;
begin

end;

function ArrayAdd(A,B: array of Byte): TByteArray;
begin

end;

function ArrayAdd(A,B: array of Word): TWordArray;
begin

end;

function ArrayAdd(A,B: array of Cardinal): TCardinalArray;
begin

end;

function ArrayAdd(A,B: array of Single): TSingleArray;
begin

end;

function ArrayAdd(A,B: array of Double): TDoubleArray;
begin

end;

function ArrayAdd(A,B: array of Extended): TExtendedArray;
begin

end;

procedure BubbleSort(var Elements: array of String); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: String;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if AnsiCompareText(Elements[Index],Elements[Index + 1]) > 0 then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of ShortInt); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: ShortInt;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of SmallInt); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: SmallInt;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of Integer); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: Integer;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of Int64); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: Int64;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of Byte); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: Byte;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of Word); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: Word;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of Cardinal); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: Cardinal;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of Single); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: Single;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of Double); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: Double;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

procedure BubbleSort(var Elements: array of Extended); overload;
var
  Changed: Boolean;
  Index: Integer;
  Buffer: Extended;
begin
  Changed := True;
  while Changed do
  begin
    Changed := False;
    for Index := Low(Elements) to High(Elements) - 1 do
    begin
      if Elements[Index] > Elements[Index + 1] then
      begin
        Buffer := Elements[Index];
        Elements[Index] := Elements[Index + 1];
        Elements[Index + 1] := Buffer;
        if not Changed then
        begin
          Changed := True;
        end;
      end;
    end;
  end;
end;

function GetSubPropInfo(Instance: TObject; const PropName: String; AKinds: TTypeKinds = []): PPropInfo;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetPropInfo(Instance,Trim(PropName),AKinds);
  end else
  begin
    Result := GetSubPropInfo(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),AKinds);
  end;
end;

function GetObjectSubProp(Instance: TObject; const PropName: String): TObject;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetObjectProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetObjectSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetObjectSubProp(Instance: TObject; const PropName: String; Value: TObject);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetObjectProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetObjectSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function GetVariantSubProp(Instance: TObject; const PropName: String): Variant;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetVariantProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetVariantSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetVariantSubProp(Instance: TObject; const PropName: String; Value: Variant);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetVariantProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetVariantSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function GetStrSubProp(Instance: TObject; const PropName: String): String;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetStrProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetStrSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetStrSubProp(Instance: TObject; const PropName: String; Value: String);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetStrProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetStrSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

{$IFNDEF NO_UNICODE}
function GetAnsiStrSubProp(Instance: TObject; const PropName: String): AnsiString;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetAnsiStrProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetAnsiStrSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetAnsiStrSubProp(Instance: TObject; const PropName: String; Value: AnsiString);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetAnsiStrProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetAnsiStrSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;
{$ENDIF}

function GetInt64SubProp(Instance: TObject; const PropName: String): Int64;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetInt64Prop(Instance,Trim(PropName));
  end else
  begin
    Result := GetInt64SubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetInt64SubProp(Instance: TObject; const PropName: String; Value: Int64);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetInt64Prop(Instance,Trim(PropName),Value);
  end else
  begin
    SetInt64SubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function GetFloatSubProp(Instance: TObject; const PropName: String): Extended;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetFloatProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetFloatSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetFloatSubProp(Instance: TObject; const PropName: String; Value: Extended);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetFloatProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetFloatSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function GetOrdSubProp(Instance: TObject; const PropName: String): NativeInt;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetOrdProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetOrdSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetOrdSubProp(Instance: TObject; const PropName: String; Value: NativeInt);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetOrdProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetOrdSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function GetEnumSubProp(Instance: TObject; const PropName: String): String;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetEnumProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetEnumSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetEnumSubProp(Instance: TObject; const PropName: String; Value: String);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetEnumProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetEnumSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function GetSetSubProp(Instance: TObject; const PropName: String): String;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetSetProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetSetSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetSetSubProp(Instance: TObject; const PropName: String; Value: String);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetSetProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetSetSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function GetDynArraySubProp(Instance: TObject; const PropName: String): Pointer;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetDynArrayProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetDynArraySubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetDynArraySubProp(Instance: TObject; const PropName: String; Value: Pointer);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetDynArrayProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetDynArraySubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function GetInterfaceSubProp(Instance: TObject; const PropName: String): IInterface;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetInterfaceProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetInterfaceSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetInterfaceSubProp(Instance: TObject; const PropName: String; Value: IInterface);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetInterfaceProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetInterfaceSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function GetMethodSubProp(Instance: TObject; const PropName: String): TMethod;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := GetMethodProp(Instance,Trim(PropName));
  end else
  begin
    Result := GetMethodSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

procedure SetMethodSubProp(Instance: TObject; const PropName: String; Value: TMethod);
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    SetMethodProp(Instance,Trim(PropName),Value);
  end else
  begin
    SetMethodSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),Value);
  end;
end;

function SubPropIsType(Instance: TObject; const PropName: String; TypeKind: TTypeKind): Boolean;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := PropIsType(Instance,Trim(PropName),TypeKind);
  end else
  begin
    Result := SubPropIsType(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2),TypeKind);
  end;
end;

function SubPropType(Instance: TObject; const PropName: String): TTypeKind;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := PropType(Instance,Trim(PropName));
  end else
  begin
    Result := SubPropType(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

function IsPublishedSubProp(Instance: TObject; const PropName: String): Boolean;
var
  DotPos: Integer;
begin
  DotPos := Pos(DotSep,PropName);
  if DotPos = 0 then
  begin
    Result := IsPublishedProp(Instance,Trim(PropName));
  end else
  begin
    Result := IsPublishedSubProp(GetObjectProp(Instance,Trim(Copy(PropName,1,DotPos - 1))),Copy(PropName,DotPos + 1,Length(PropName) - 2));
  end;
end;

function SystemTime: TSystemTime;
begin
  GetLocalTime(Result);
end;

function Year: Word;
begin
  Result := SystemTime.wYear;
end;

function Month: Word;
begin
  Result := SystemTime.wMonth;
end;

function DayOfWeek: Word;
begin
  Result := SystemTime.wDayOfWeek;
end;

function Day: Word;
begin
  Result := SystemTime.wDay;
end;

function Hour: Word;
begin
  Result := SystemTime.wHour;
end;

function Minute: Word;
begin
  Result := SystemTime.wMinute;
end;

function Second: Word;
begin
  Result := SystemTime.wSecond;
end;

function Milliseconds: Word;
begin
  Result := SystemTime.wMilliseconds;
end;

function Count(Elements: TByteSet): Byte;
var
  Current: Byte;
begin
  Result := 0;
  for Current in Elements do
  begin
    Inc(Result);
  end;
end;

function Count(Elements: TCharSet): Byte;
var
  Current: Char;
begin
  Result := 0;
  for Current in Elements do
  begin
    Inc(Result);
  end;
end;

function SetToArray(Elements: TByteSet): TByteArray;
var
  Current: Byte;
  Index: Integer;
begin
  SetLength(Result,Count(Elements));
  Index := 0;
  for Current in Elements do
  begin
    Result[Index] := Current;
    Inc(Index);
  end;
end;

function SetToArray(Elements: TCharSet): TCharArray;
var
  Current: Char;
  Index: Integer;
begin
  SetLength(Result,Count(Elements));
  Index := 0;
  for Current in Elements do
  begin
    Result[Index] := Current;
    Inc(Index);
  end;
end;

function ArrayToSet(Elements: array of Byte): TByteSet;
var
  Index: Integer;
begin
  Result := [];
  for Index := 0 to Length(Elements) - 1 do
  begin
    Result := Result + [Elements[Index]];
  end;
end;

function ArrayToSet(Elements: array of Char): TCharSet;
var
  Index: Integer;
begin
  Result := [];
  for Index := 0 to Length(Elements) - 1 do
  begin
    Result := Result + [Elements[Index]];
  end;
end;

function SetToBools(Elements: TByteSet): TBooleanArray;
var
  Index: Integer;
begin
  SetLength(Result,SizeOf(TByteSet) * 8);
  for Index := Low(Result) to High(Result) do
  begin
    Result[Index] := Index in Elements;
  end;
end;

function SetToBools(Elements: TCharSet): TBooleanArray;
var
  Index: Integer;
begin
  SetLength(Result,SizeOf(TCharSet) * 8);
  for Index := Low(Result) to High(Result) do
  begin
    Result[Index] := Chr(Index) in Elements;
  end;
end;

function Prime(X: ShortInt): Boolean;
begin
  Result := Prime(Byte(Abs(X)));
end;

function Prime(X: Byte): Boolean;
const
  PrimeNumbers = [  2,  3,  5,  7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
                   53, 59, 61, 67, 71, 73, 79, 83, 89, 97,101,103,107,109,113,
                  127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,
                  199,211,223,227,229,233,239,241,251                         ];
begin
  Result := X in PrimeNumbers;
end;

function Prime(X: Integer): Boolean;
begin
  Result := Prime(Cardinal(Abs(X)));
end;

function Prime(X: Cardinal): Boolean;
var
  Index: Integer;
begin
  Result := Prime(Byte(X));
  if (not Result) and (X > MAXBYTE) and Odd(X) then
  begin
    Index := 3;
    while Index <= X div Index do
    begin
      if X mod Index = 0 then
      begin
        Exit;
      end;
      Inc(Index,2);
    end;
    Result := True;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue, FalseValue: Variant): Variant;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Variant): Variant;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := Null;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Pointer; FalseValue: Pointer = nil): Pointer;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: PChar; FalseValue: PChar = nil): PChar;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Char; FalseValue: Char = #0): Char;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: String; FalseValue: String = ''): String;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: ShortString; FalseValue: ShortString = ''): ShortString;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Byte; FalseValue: Byte = 0): Byte;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Word; FalseValue: Word = 0): Word;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Cardinal; FalseValue: Cardinal = 0): Cardinal;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: UInt64; FalseValue: UInt64 = 0): UInt64;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: ShortInt; FalseValue: ShortInt = 0): ShortInt;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: SmallInt; FalseValue: SmallInt = 0): SmallInt;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Integer; FalseValue: Integer = 0): Integer;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Int64; FalseValue: Int64 = 0): Int64;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Currency; FalseValue: Currency = 0): Currency;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Single; FalseValue: Single = 0): Single;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Double; FalseValue: Double = 0): Double;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function IfThenElse(Condition: Boolean; TrueValue: Extended; FalseValue: Extended = 0): Extended;
begin
  if Condition then
  begin
    Result := TrueValue;
  end else
  begin
    Result := FalseValue;
  end;
end;

function PointAdd(A,B: TPoint): TPoint;
begin
  Result := Point(A.X + B.X,A.Y + B.Y);
end;

function PointSub(A,B: TPoint): TPoint;
begin
  Result := Point(A.X - B.X,A.Y - B.Y);
end;

function Diagonal(A,B: TPoint): Extended;
begin
  Result := Sqrt(Sqr(B.X - A.X) + Sqr(B.Y - A.Y));
end;

function Distance(A,B: TPoint): Cardinal;
begin
  Result := Round(Hypot(B.X - A.X,B.Y - A.Y));
end;

function Congruent(A,B,Divisor: Integer): Boolean;
begin
  Result := (A mod Divisor) = (B mod Divisor);
end;

function Congruent(A,B,Divisor: Extended): Boolean;
begin
  Result := FloatMod(A,Divisor) = FloatMod(B,Divisor);
end;

procedure ExtEuclideanAlg(A,B: Integer; var D,S,T: Integer);
{ Erweiterter Euklidischer Algorithmus }
begin
  if B = 0 then
  begin
    D := A;
    S := 1;
    T := 0;
  end else
  begin
    ExtEuclideanAlg(B,A mod B,D,T,S);
    T := T - A div B * T;
  end;
end;

function StringToRange(const S: String; var Range: TRange): Boolean;
var
  Current: PChar;
  Block: String;
  Temp: TRange;
  Position: (posOffset,posDots,posTarget,posFinish);
begin
  {if Range = nil then
  begin
    Range := TRange.Create;
  end;
  Result := False;
  if Length(S) = 0 then
  begin
    Exit;
  end;
  Current := PChar(S);
  Position := posOffset;
  Temp := TRange.Create;
  try
    while Current^ <> #0 do
    begin
      if (Position^ = posFinish) and (not (Current^ in Spaces)) then
      begin
        Result := False;
        Break;
      end;
      case Current^ of
      Spaces: if Position in [posOffset,posTarget] then
              begin
                Inc(Position);
              end;
      '0'..'9': Block := Block + Current^;
      '+','-': if Length(Block) = 0 then
               begin
                 Block := Block + Current^;
               end else
               begin
                 Result := False;
                 Break;
               end;
      '.': if (Current + 1)^ = '.' then
           begin

             Block := '';
             Inc(Current);
           end else
           begin
             Result := False;
             Break;
           end;
      end;
      Inc(Current);
    end;
  finally
    Temp.Free;
  end;
  if Result then
  begin
    Range.Offset := Temp.Offset;
    Range.Target := Temp.Target;
    Range.Step := Temp.Step;
  end;        }
end;

function ExprInStr(const S: String; Position: Integer): String;
var
  Current: PChar;
begin
  if (Position < 1) or (Position > Length(S)) then
  begin
    raise EStringCharAccess.Create('Access to String char at position ' + IntToStr(Position) + ' not possible');
  end;
  Current := @S[Position];
  Result := '';
  while (Current^ in Letters) and (Current >= @S[1]) do
  begin
    Result := Current^ + Result;
    Dec(Current);
  end;
  Current := @S[Position + 1];
  while Current^ in Letters do
  begin
    Result := Result + Current^;
    Inc(Current);
  end;
end;

function Factional(X: Byte): Cardinal;
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
  Result := 1;
  for Index := 2 to X do
  begin
    Result := Result * Index;
  end;
end;
{$ELSE}
asm
    MOV ECX,EAX
    MOV EAX,1
    JMP @Check
  @Multiply:
    IMUL EAX,ECX
    DEC ECX
  @Check:
    CMP ECX,1
    JNE @Multiply
end;
{$ENDIF}

function ExtractClassName(FullClassName: String; CaseSensitive: Boolean = False): String;
begin
  if (Length(FullClassName) <> 0) and ((FullClassName[1] = 'T') or ((not CaseSensitive) and (FullClassName[1] = 't'))) and ((FullClassName[2] in UppercaseLetters) or (not CaseSensitive)) then
  begin
    Result := Copy(FullClassName,2,Length(FullClassName) - 1);
  end else
  begin
    Result := FullClassName;
  end;
end;

function CountLines(S: String): Integer;
var
  Current: PChar;
begin
  if Length(S) = 0 then
  begin
    Result := 0;
    Exit;
  end;
  Result := 1;
  Current := PChar(S);
  while Current^ <> #0 do
  begin
    if Current^ = #13 then
    begin
      if (Current + 1)^ = #10 then
      begin
        Inc(Result);
        Inc(Current);
      end;
    end;
    Inc(Current);
  end;
end;

function CountLine(S: String; Line: Integer): Integer;
var
  Current: PChar;
begin
  if Line < 0 then
  begin
    Result := -1;
    Exit;
  end;
  Result := 0;
  Current := PChar(S);
  while Current^ <> #0 do
  begin
    if Current^ = #13 then
    begin
      if (Current + 1)^ = #10 then
      begin
        if Line = 0 then
        begin
          Exit;
        end;
        Dec(Line);
        Inc(Current);
        Continue;
      end;
    end;
    if Line = 0 then
    begin
      Inc(Result);
    end;
    Inc(Current);
  end;
  if Line <> 0 then
  begin
    Result := -1;
  end;
end;

function Wrappable(S: String; Canvas: TCanvas; MaxWidth: Integer): Boolean; overload;
begin
  Result := (Canvas.TextWidth(S) <= MaxWidth);
end;

function WrappedTextHeight(S: String; Canvas: TCanvas; MaxWidth: Integer): Integer;
var
  Current: PChar;
  Line: String;
begin
  Result := 0;
  if (Length(S) = 0) or (MaxWidth = 0) then
  begin
    Exit;
  end;
  Line := '';
  Current := PChar(S);
  while Current^ <> #0 do
  begin
    if ((Current^ = #13) and ((Current + 1)^ = #10)) or (Canvas.TextWidth(Line + Current^) > MaxWidth) then
    begin
      Inc(Result,Canvas.TextHeight(Line));
      Line := Current^;
      if (Current + 1)^ = #10 then
      begin
        Inc(Current);
      end;
    end else
    begin
      Line := Line + Current^;
    end;
    Inc(Current);
  end;
  Inc(Result,Canvas.TextHeight(Line));
end;

function CharEncoding(EncodingClass: TEncoding): TCharEncoding;
begin
  if EncodingClass = TEncoding.ANSI then
  begin
    Result := ceANSI;
  end else
  begin
    if EncodingClass = TEncoding.ASCII then
    begin
      Result := ceASCII;
    end else
    begin
      if EncodingClass = TEncoding.BigEndianUnicode then
      begin
        Result := ceBigEndianUnicode;
      end else
      begin
        if EncodingClass = TEncoding.Unicode then
        begin
          Result := ceUnicode;
        end else
        begin
          if EncodingClass = TEncoding.UTF7 then
          begin
            Result := ceUTF7;
          end else
          begin
            if EncodingClass = TEncoding.UTF8 then
            begin
              Result := ceUTF8;
            end else
            begin
              raise EConvertError.Create('Invalid encoding type');
            end;
          end;
        end;
      end;
    end;
  end;
end;

function EncodingClass(CharEncoding: TCharEncoding): TEncoding;
begin
  case CharEncoding of
    ceANSI: Result := TEncoding.ANSI;
    ceASCII: Result := TEncoding.ASCII;
    ceBigEndianUnicode: Result := TEncoding.BigEndianUnicode;
    ceUnicode: Result := TEncoding.Unicode;
    ceUTF7: Result := TEncoding.UTF7;
    ceUTF8: Result := TEncoding.UTF8;
  end;
end;

function SecToTime(const Sec: Cardinal): TTime;
var
   Hrs, Mins: Word;
begin
  Hrs := Sec div 3600;
  Mins := Sec div 60 - Hrs * 60;
  Result := StrToTime(IntToStr(Hrs) + ':' + IntToStr(Mins) + ':' + IntToStr(Sec - (Hrs * 3600 + Mins * 60)));
end;

function GetExecTime(Command: Pointer; Amount: Cardinal; Attempts: Cardinal = 1): Cardinal;
var
  Index_Amount: Cardinal;
  Index_Attempts: Cardinal;
  Tick_Start: Cardinal;
  Tick_Finish: Cardinal;
begin
  Result := 0;
  if (not Assigned(Command)) or (Amount = 0) or (Attempts = 0) then
  begin
    Exit;
  end;
  try
    for Index_Attempts := 1 to Attempts do
    begin
      for Index_Amount := 1 to Amount do
      begin
        Tick_Start := GetTickCount;
        TProcedure(Command);
        Tick_Finish := GetTickCount;
        Inc(Result,Tick_Finish - Tick_Start);
      end;
    end;
  finally
    Result := Result div Index_Attempts;
  end;
end;

function SystemLanguage: String;
begin
  Result := Languages.NameFromLocaleID[Languages.UserDefaultLocale];
end;

function ArrayPos(const AValue; const AArray: array of const): Integer; overload;
{ Da diese Methode anonyme Parameter übergeben bekommt (bzw. die Parameter
  anonym übergeben werden), entsteht aufgrund der Typenüberprüfung für jedes
  Element von AArray eine erhebliche Performanzeinbuße.
  Diese Methode sollte deswegen nur in Ausnamefällen verwendet werden.
  Sofern möglich, sollte alternativ auch die Funktion ArrayPos(Variant, array of
  Vairant) verwendet werden. }
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    case AArray[Index].VType of
      vtInteger: if AArray[Index].VInteger = Integer(AValue) then
                 begin
                   Result := Index;
                   Exit;
                 end;
      vtBoolean: if AArray[Index].VBoolean = Boolean(AValue) then
                 begin
                   Result := Index;
                   Exit;
                 end;
      vtChar:  if AArray[Index].VChar = AnsiChar(AValue) then
               begin
                 Result := Index;
                 Exit;
               end;
      vtExtended: if AArray[Index].VExtended = PExtended(AValue) then
                  begin
                    Result := Index;
                    Exit;
                  end;
      vtString: if AArray[Index].VString = PShortString(AValue) then
                begin
                  Result := Index;
                  Exit;
                end;
      vtPointer: if AArray[Index].VPointer = Pointer(AValue) then
                 begin
                   Result := Index;
                   Exit;
                 end;
      vtPChar: if AArray[Index].VPChar = PAnsiChar(AValue) then
               begin
                 Result := Index;
                 Exit;
               end;
      vtObject: if AArray[Index].VObject = TObject(AValue) then
                begin
                  Result := Index;
                  Exit;
                end;
      vtClass: if AArray[Index].VClass = TClass(AValue) then
               begin
                 Result := Index;
                 Exit;
               end;
      {$IFNDEF NO_UNICODE}
        vtWideChar: if AArray[Index].VWideChar = Char(AValue) then
                    begin
                      Result := Index;
                      Exit;
                    end;
        vtPWideChar: if AArray[Index].VWideChar = PChar(AValue) then
                     begin
                       Result := Index;
                       Exit;
                     end;
        vtUnicodeString: if AArray[Index].VUnicodeString = Pointer(AValue) then
                         begin
                           Result := Index;
                           Exit;
                         end;
      {$ENDIF}
      vtAnsiString: if AArray[Index].VAnsiString = Pointer(AValue) then
                    begin
                      Result := Index;
                      Exit;
                    end;
      vtCurrency: if AArray[Index].VCurrency = PCurrency(AValue) then
                  begin
                    Result := Index;
                    Exit;
                  end;
      vtVariant: if AArray[Index].VVariant = PVariant(AValue) then
                 begin
                   Result := Index;
                   Exit;
                 end;
      vtInterface: if AArray[Index].VInterface = Pointer(AValue) then
                   begin
                     Result := Index;
                     Exit;
                   end;

      vtWideString: if AArray[Index].VWideString = Pointer(AValue) then
                    begin
                      Result := Index;
                      Exit;
                    end;
      vtInt64: if AArray[Index].VInt64 = PInt64(AValue) then
               begin
                 Result := Index;
                 Exit;
               end;
    end;
  end;
end;

function ArrayPos(const AValue: Variant; const AArray: array of Variant): Integer; overload;
{ Da diese Funktion als Parametertyp Variants verwendet, wird von der Verwendung
  bei komplexeren Abläufen aufgrund der geringen Performance varianter Typen
  ausdrücklich abgeraten! }
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Pointer; const AArray: array of Pointer): Integer;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Char; const AArray: array of Char): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: ShortString; const AArray: array of ShortString): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: String; const AArray: array of String): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: ShortInt; const AArray: array of ShortInt): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: SmallInt; const AArray: array of SmallInt): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: LongInt; const AArray: array of LongInt): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Int64; const AArray: array of Int64): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Byte; const AArray: array of Byte): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Word; const AArray: array of Word): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Cardinal; const AArray: array of Cardinal): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: UInt64; const AArray: array of UInt64): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Single; const AArray: array of Single): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Double; const AArray: array of Double): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Real; const AArray: array of Real): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPos(const AValue: Extended; const AArray: array of Extended): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosRef(const AValue: Pointer; const AArray: array of TReferenceData): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index].Value = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosRef(const AValue: String; const AArray: array of TStringReferenceData; IgnoreCase: Boolean = False): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if (AArray[Index].Value = AValue) or (IgnoreCase and (LowerCase(AArray[Index].Value) = LowerCase(AValue))) then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosRef(const AValue: Integer; const AArray: array of TIntegerReferenceData): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index].Value = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosRef(const AValue: Extended; const AArray: array of TFloatReferenceData): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index].Value = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosRef(const AValue: Pointer; const AArray: array of TRefDataArrayReferenceData): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index].Value = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosRef(const AValue: String; const AArray: array of TStringRefDataArrayReferenceData; IgnoreCase: Boolean = False): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if (AArray[Index].Value = AValue) or (IgnoreCase and (LowerCase(AArray[Index].Value) = LowerCase(AValue))) then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosRef(const AValue: Integer; const AArray: array of TIntegerRefDataArrayReferenceData): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index].Value = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosRef(const AValue: Extended; const AArray: array of TFloatRefDataArrayReferenceData): Integer; overload;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index].Value = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosType(AValue: TClass; AArray: array of TClass): Integer;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] = AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

function ArrayPosType(AValue: TClass; AArray: array of TObject): Integer;
var
  Index: Integer;
begin
  Result := Low(AArray) - 1;
  for Index := Low(AArray) to High(AArray) do
  begin
    if AArray[Index] is AValue then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

procedure ArrayDelete(var AArray: TVariantArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TCharArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TShortStringArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TStringArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TShortIntArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TSmallIntArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TIntegerArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TInt64Array; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TByteArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TWordArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TCardinalArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TUInt64Array; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TSingleArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TDoubleArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

procedure ArrayDelete(var AArray: TExtendedArray; Index: Integer; Count: Integer);
begin
  if Count <> 0 then
  begin
    Move(AArray[Index + Count],AArray[Index],SizeOf(AArray[0]) * (Length(AArray) - Index - Count));
    SetLength(AArray,Length(AArray) - Count);
  end;
end;

function ExtractUserName(const Owner: String): String;
var
  Index: Integer;
begin
  for Index := 1 to Length(Owner) do
  begin
    if Owner[Index] = '@' then
    begin
      Break;
    end else
    begin
      Result := Result + Owner[Index];
    end;
  end;
end;

function ExtractUserDomain(const Owner: String): String;
var
  Index: Integer;
begin
  for Index := Length(Owner) downto 0 do
  begin
    if Owner[Index] = '@' then
    begin
      Break;
    end else
    begin
      Result := Result + Owner[Index];
    end;
  end;
end;

function RoundPos(Number: Single; Pos: Byte): Single;
var
  Factor: Single;
begin
  Factor := IntPower(10,Pos);
  Result := Round(Number * Factor) / Factor;
end;

function FontSizeToHeight(Size: Integer; PpI: Integer): Integer;
begin
  Result := - Size * PpI div 72;
end;

function FontHeightToSize(Height: Integer; PpI: Integer): Integer;
begin
  Result := - Height * 72 div PpI;
end;

function ComponentByTag(Owner: TComponent; const Tag: Integer): TComponent;
var
  Index: Integer;
begin
  for Index := 0 to Owner.ComponentCount - 1 do
  begin
    if Owner.Components[Index].Tag = Tag then
    begin
      Result := Owner.Components[Index];
      Exit;
    end;
  end;
  Result := nil;
end;

function ControlIndex(Control: TControl): Integer;
var
  Index: Integer;
begin
  for Index := 0 to Control.Parent.ControlCount - 1 do
  begin
    if Control.Parent.Controls[Index] = Control then
    begin
      Result := Index;
      Exit;
    end;
  end;
  Result := -1;
end;

function IntToStrMinLength(Value: Integer; MinLength: SmallInt): String;
begin
  Result := IntToStr(Value);
  while Length(Result) < MinLength do
  begin
    Result := '0' + Result;
  end;
end;

function BmpRect(Bitmap: TBitmap): TRect;
begin
  Result := Rect(0,0,Bitmap.Width,Bitmap.Height);
end;

function GetOwnerForm(Component: TComponent): TCustomForm;
begin
  if not (Assigned(Component) and Assigned(Component.Owner)) then
  begin
    Result := nil;
  end else
  begin
    if Component is TCustomForm then
    begin
      Result := Component as TCustomForm;
    end else
    begin
      Result := GetOwnerForm(Component.Owner);
    end;
  end;
end;

function MultiPos(const SubStr, Str: ShortString; Offset: Integer = 1): TIntegerArray;
begin

end;

function MultiPos(const SubStr, Str: String; Offset: Integer = 1): TIntegerArray;
var
  Temp: PChar;
  Position: Integer;
  Further: TIntegerArray;
begin
  SetLength(Result,0);
  if (Offset < 1) or (Offset - 1 > (Length(Str) - Length(SubStr))) then
  begin
    Exit;
  end;
  Temp := @Str[OffSet];
  Position := Pos(SubStr,String(Temp));
  if Position <> 0 then
  begin
    SetLength(Result,1);
    Result[0] := Position;
    Further := MultiPos(SubStr,Str,Offset + Position + Length(SubStr) - 1);
    if Length(Further) <> 0 then
    begin
      SetLength(Result,1 + Length(Further));
      Move(Further[0],Result[1],SizeOf(Further) * Length(Further));
    end;
  end;
end;

function CharLine(Current: PAnsiChar; Text: AnsiString): Integer;
var
  Index: PAnsiChar;
  InLineBreak: PAnsiChar;
  Position: Integer;
begin
  Position := Pos(AnsiString(Current),Text);
  if Position = 0 then
  begin
    if Current^ = #0 then
    begin
      Result := Length(Text) + 1;
    end else
    begin
      Result := 0;
    end;
    Exit;
  end;
  Result := 0;
  SetLength(Text,Position - 1);
  InLineBreak := @sLineBreak[1];
  Index := PAnsiChar(Text);
  while Index^ <> #0 do
  begin
    if Index^ = InLineBreak^ then
    begin
      Inc(InLineBreak);
      if InLineBreak^ = #0 then
      begin
        Inc(Result);
        InLineBreak := @sLineBreak[1];
      end;
    end else
    begin
      InLineBreak := @sLineBreak[1];
    end;
    Inc(Index);
  end;
end;

{$IFNDEF NO_UNICODE}
function CharLine(Current: PWideChar; Text: UnicodeString): Integer;
var
  Index: PWideChar;
  InLineBreak: PWideChar;
  Position: Integer;
  UnicodeLineBreak: UnicodeString;
begin
  Position := Pos(UnicodeString(Current),Text);
  if Position = 0 then
  begin
    if Current^ = #0 then
    begin
      Result := Length(Text) + 1;
    end else
    begin
      Result := 0;
    end;
    Exit;
  end;
  UnicodeLineBreak := UnicodeString(sLineBreak);
  Result := 0;
  SetLength(Text,Position - 1);
  InLineBreak := @UnicodeLineBreak[1];
  Index := PChar(Text);
  while Index^ <> #0 do
  begin
    if Index^ = InLineBreak^ then
    begin
      Inc(InLineBreak);
      if InLineBreak^ = #0 then
      begin
        Inc(Result);
        InLineBreak := @UnicodeLineBreak[1];
      end;
    end else
    begin
      InLineBreak := @UnicodeLineBreak[1];
    end;
    Inc(Index);
  end;
end;
{$ENDIF}

function CharPosition(Current: PAnsiChar; Text: AnsiString): Integer;
var
  Index: PAnsiChar;
  InLineBreak: PAnsiChar;
  Position: Integer;
  Line: Integer;
begin
  Position := Pos(AnsiString(Current),Text);
  if Position = 0 then
  begin
    if Current^ = #0 then
    begin
      Result := Length(Text) + 1;
    end else
    begin
      Result := 0;
    end;
    Exit;
  end;
  Result := 1;
  SetLength(Text,Position - 1);
  InLineBreak := @sLineBreak[1];
  Index := PAnsiChar(Text);
  while Index^ <> #0 do
  begin
    if Index^ = InLineBreak^ then
    begin
      Inc(InLineBreak);
      if InLineBreak^ = #0 then
      begin
        Result := 1;
        Inc(Line);
        InLineBreak := @sLineBreak[1];
      end;
    end else
    begin
      InLineBreak := @sLineBreak[1];
      Inc(Result);
    end;
    Inc(Index);
  end;
end;

{$IFNDEF NO_UNICODE}
function CharPosition(Current: PWideChar; Text: UnicodeString): Integer;
var
  Index: PWideChar;
  InLineBreak: PWideChar;
  Position: Integer;
  UnicodeLineBreak: UnicodeString;
  Line: Integer;
begin
  Position := Pos(UnicodeString(Current),Text);
  if Position = 0 then
  begin
    if Current^ = #0 then
    begin
      Result := Length(Text) + 1;
    end else
    begin
      Result := 0;
    end;
    Exit;
  end;
  UnicodeLineBreak := UnicodeString(sLineBreak);
  Result := 1;
  SetLength(Text,Position - 1);
  InLineBreak := @UnicodeLineBreak[1];
  Index := PChar(Text);
  while Index^ <> #0 do
  begin
    if Index^ = InLineBreak^ then
    begin
      Inc(InLineBreak);
      if InLineBreak^ = #0 then
      begin
        Result := 1;
        Inc(Line);
        InLineBreak := @UnicodeLineBreak[1];
      end;
    end else
    begin
      InLineBreak := @UnicodeLineBreak[1];
      Inc(Result);
    end;
    Inc(Index);
  end;
end;
{$ENDIF}

function ConsistsOf(const S: String; Chars: array of Char): Boolean; overload;
var
  Current: PChar;
begin
  Current := PChar(S);
  while Current^ <> #0 do
  begin
    if ArrayPos(Current^,Chars) = -1 then
    begin
      Result := False;
      Exit;
    end;
    Inc(Current);
  end;
  Result := True;
end;

function ConsistsOf(const S: String; Chars: TCharSet): Boolean; overload;
var
  Current: PChar;
begin
  Current := PChar(S);
  while Current^ <> #0 do
  begin
    if not (Current^ in Chars) then
    begin
      Result := False;
      Exit;
    end;
    Inc(Current);
  end;
  Result := True;
end;

function ArrayToString(AArray: array of const): String;
var
  OldDecSep: Char;
  Index: Integer;
begin
  {$IF Declared(FormatSettings)}
    OldDecSep := FormatSettings.DecimalSeparator;
    FormatSettings.DecimalSeparator := '.';
  {$ELSE}
    OldDecSep := DecimalSeparator;
    DecimalSeparator := '.';
  {$ENDIF}
  try
    for Index := Low(AArray) to High(AArray) do
    begin
      case AArray[Index].VType of
        vtInteger: Result := Result + IntToStr(AArray[Index].VInteger);
        vtBoolean: Result := Result + BoolToStr(AArray[Index].VBoolean, True);
        vtChar: Result := Result + QuotedStr(AArray[Index].VChar);
        vtExtended: Result := Result + FloatToStr(AArray[Index].VExtended^);
        vtString: Result := Result + QuotedStr(AArray[Index].VString^);
        vtPointer: Result := Result + IntToStr(Integer(AArray[Index].VPointer));
        vtPChar: Result := Result + QuotedStr(String(AArray[Index].VPChar));
        vtObject: Result := Result + AArray[Index].VObject.ClassName + '(' + IntToStr(Integer(AArray[Index].VObject)) + ')';
        vtClass: Result := Result + AArray[Index].VClass.ClassName;
        {$IFNDEF NO_UNICODE}
          vtWideChar: Result := Result + QuotedStr(AArray[Index].VWideChar);
          vtPWideChar: Result := Result + QuotedStr(WideString(AArray[Index].VPWideChar));
          vtUnicodeString: Result := Result + QuotedStr(String(AArray[Index].VUnicodeString));
        {$ENDIF}
        vtAnsiString: Result := Result + QuotedStr(AnsiString(AArray[Index].VAnsiString));
        vtCurrency: Result := Result + CurrToStr(AArray[Index].VCurrency^);
        vtVariant: begin
                     if VarIsStr(AArray[Index].VVariant^) then
                     begin
                       Result := Result + 'Variant(' + QuotedStr(VarToStr(AArray[Index].VVariant^)) + ')';
                     end else
                     begin
                       Result := Result + 'Variant(' + VarToStr(AArray[Index].VVariant^) + ')';
                     end;
                   end;
        vtInterface: Result := Result + 'Interface(' + IntToStr(Integer(AArray[Index].VInterface)) + ')';
        vtWideString: Result := Result + QuotedStr(WideString(AArray[Index].VWideString));
        vtInt64: Result := Result + IntToStr(AArray[Index].VInteger);
      end;
      if Index <> High(AArray) then
      begin
        Result := Result + ',';
      end;
    end;
    Result := '[' + Result + ']';
  finally
    {$IF Declared(FormatSettings)}
      FormatSettings.DecimalSeparator := OldDecSep;
    {$ELSE}
      DecimalSeparator := OldDecSep;
    {$ENDIF}
  end;
end;

function BitsToBytes(Bits: array of TBit): TByteArray;
var
  Index: Integer;
begin
  SetLength(Result, Ceil(Length(Bits) / 8));
  for Index := Low(Bits) to High(Bits) do
  begin
    Result[Index div 8] := Result[Index div 8] or (Bits[Index] shl (7 - Index mod 8));
  end;
end;

function BytesToBits(Bytes: array of Byte): TBitArray;
var
  Index: Integer;
begin
  SetLength(Result, Length(Bytes) * 8);
  for Index := Low(Result) to High(Result) do
  begin
    Result[Index] := Bytes[Index div 8] and (Index shl (7 - Index mod 8));
  end;
end;

function CheckSum(Bytes: array of Byte; Algorith: TCheckSum): Word;
var
  Index: Integer;
begin
  Result := 0;
  case Algorith of
    csBitCount: begin
                  for Index := Low(Bytes) to High(Bytes) do
                  begin
                    //...
                  end;
                end;
    csFetcher: begin
                 for Index := Low(Bytes) to High(Bytes) do
                 begin
                   if TBytePair(Result)[0] + Bytes[Index] > MAXBYTE then
                   begin
                     //0-Bytewert ausgleichen, schneller als "mod MAXBYTE"
                     Inc(TBytePair(Result)[0]);
                   end;
                   Inc(TBytePair(Result)[0], Bytes[Index]);
                   if TBytePair(Result)[0] + TBytePair(Result)[1] > MAXBYTE then
                   begin
                     //0-Bytewert ausgleichen, schneller als "mod MAXBYTE"
                     Inc(TBytePair(Result)[1]);
                   end;
                   Inc(TBytePair(Result)[1], TBytePair(Result)[0]);
                 end;
               end;
  end;
end;

procedure IndentBlock(var S: String; Indents: Byte; const Indent: Char = ' ');
var
  Index: Integer;
  Sequence: String;
begin
  Sequence := StringOfChar(Indent, Indents);
  Index := 1;
  repeat
    Insert(Sequence, S, Index);
    Inc(Index, Indents);
    Index := Pos(sLineBreak, S, Index);
  until Index = 0;
end;

procedure Exchange(var X,Y); inline;
var
  Buffer: Pointer;
begin
  Buffer := Pointer(X);
  Pointer(X) := Pointer(Y);
  Pointer(Y) := Buffer;
end;

procedure PrintText(Strings: TStrings; Font: TFont);
var
  Index: Integer;
  PrintText: TextFile;
begin
  AssignPrn(PrintText);
  Rewrite(PrintText);
  try
    if Font = nil then
    begin
      Printer.Canvas.Font := Font;
    end;
    for Index := 0 to Strings.Count - 1 do
    begin
      Writeln(PrintText, Strings.Strings[Index]);
    end;
  finally
    CloseFile(PrintText);
  end;
end;

procedure BFInterpret(const S: String; var P: Pointer);
{ Liest einen BF-Quelltext "S" ein und verändert den Zeiger "P" entsprechend.
  Ungültige Zeichen lösen eine Fehlermeldung aus. Eingaben werden aus der
  aktuellen Datei oder der Konsole eingelesen und Ausgaben dort ausgegeben. }
var
  Current: PChar;
begin
  if P <> nil then
  begin
    Current := PChar(S);
    while Current^ <> #0 do
    begin
      case Current^ of
        '>': Inc(Integer(P));
        '<': Dec(Integer(P));
        '+': Inc(Integer(P^));
        '-': Dec(Integer(P^));
        '.': Write(Integer(P^));
        ',': Read(Integer(P^));
        '[': if Integer(P^) = 0 then
             begin
               repeat
                 Inc(Current);
               until Current^ = ']';
             end;
        ']': if Integer(P^) <> 0 then
             begin
               repeat
                 Dec(Current);
               until Pred(Current^) = '[';
             end;
        else raise EInvalidBFCommand.Create('Invalid character in BF source code: "' + Current^ + '"');
      end;
      Inc(Current);
    end;
  end;
end;

procedure BFInterpret(const S: String; var P: Pointer; var ReadBuffer, WriteBuffer: TIntegerArray);
{ Siehe Methode "BFInterpret(String,Pointer)".
  Eingaben werden aus "ReadBuffer" eingelesen und Ausgaben in "WriteBuffer"
  ausgegeben. }
var
  Current: PChar;
  Reader: PInteger;
begin
  if P <> nil then
  begin
    Current := PChar(S);
    Reader := PInteger(ReadBuffer);
    while Current^ <> #0 do
    begin
      case Current^ of
        '>': Inc(Integer(P));
        '<': Dec(Integer(P));
        '+': Inc(Integer(P^));
        '-': Dec(Integer(P^));
        '.': begin
               SetLength(WriteBuffer,Length(WriteBuffer) + 1);
               WriteBuffer[Length(WriteBuffer) - 1] := Integer(P^);
             end;
        ',': begin
               Integer(P^) := Reader^;
               Inc(Reader);
             end;
        '[': if Integer(P^) = 0 then
             begin
               repeat
                 Inc(Current);
               until Current^ = ']';
             end;
        ']': if Integer(P^) <> 0 then
             begin
               repeat
                 Dec(Current);
               until Pred(Current^) = '[';
             end;
        else raise EInvalidBFCommand.Create('Invalid character in BF source code: "' + Current^ + '"');
      end;
      Inc(Current);
    end;
  end;
end;

procedure ExtractChars(var Text: String; Chars: array of Char); overload;
var
  Current: PChar;
  OutPut: String;
begin
  Current := PChar(Text);
  SetLength(OutPut,0);
  while Current^ <> #0 do
  begin
    if ArrayPos(Current^,Chars) <> -1 then
    begin
      OutPut := OutPut + Current^;
    end;
    Inc(Current);
  end;
  Text := OutPut;
end;

procedure ExtractChars(var Text: String; Chars: TCharSet); overload;
var
  Current: PChar;
  OutPut: String;
begin
  Current := PChar(Text);
  SetLength(OutPut,0);
  while Current^ <> #0 do
  begin
    if Current^ in Chars then
    begin
      OutPut := OutPut + Current^;
    end;
    Inc(Current);
  end;
  Text := OutPut;
end;

procedure DeleteChars(var Text: String; Chars: array of Char); overload;
var
  Current: PChar;
  OutPut: String;
begin
  Current := PChar(Text);
  SetLength(OutPut,0);
  while Current^ <> #0 do
  begin
    if ArrayPos(Current^,Chars) = -1 then
    begin
      OutPut := OutPut + Current^;
    end;
    Inc(Current);
  end;
  Text := OutPut;
end;

procedure DeleteChars(var Text: String; Chars: TCharSet); overload;
var
  Current: PChar;
  OutPut: String;
begin
  Current := PChar(Text);
  SetLength(OutPut,0);
  while Current^ <> #0 do
  begin
    if not (Current^ in Chars) then
    begin
      OutPut := OutPut + Current^;
    end;
    Inc(Current);
  end;
  Text := OutPut;
end;

procedure SetPrivilege(const Name: PChar; Value: Boolean); overload;
var
  Token: THandle;
  Privileges: TTokenPrivileges;
begin
  if OpenProcessToken(GetCurrentProcess(),TOKEN_ADJUST_PRIVILEGES,Token) then
  begin
    Privileges.PrivilegeCount := 1;
    LookupPrivilegeValue(nil,Name,Privileges.Privileges[0].Luid);
    if Value then
    begin
      Privileges.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
    end else
    begin
      Privileges.Privileges[0].Attributes := 0;
    end;
    AdjustTokenPrivileges(Token,False,Privileges,SizeOf(Privileges),nil,DWord(nil^));
    CloseHandle(Token);
  end;
end;

procedure SetPrivilege(const Name: String; Value: Boolean); overload;
begin
  SetPrivilege(PChar(Name),Value);
end;

function WinUserName: String;
var
  Buffer: array [0..255] of Char;
  Size: DWord;
begin
  Size := SizeOf(Buffer);
  if not GetUserName(Buffer, Size) then
  begin
    raise EWinUserInformation.Create('Could not collect information on user name');
  end;
  SetString(Result,Buffer,Size - 1);
end;

function WinUserDirectory: String;
begin
  Result := GetEnvironmentVariable('USERPROFILE');
end;

function WinUserAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0,0,0,0,0,5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  X: Integer;
  bSuccess: BOOL;
begin
  Result := False;
  bSuccess := False;
  ptgGroups := nil;
  psidAdministrators := nil;
  try
    bSuccess := OpenThreadToken(GetCurrentThread,TOKEN_QUERY,True,hAccessToken);
    if not bSuccess then
    begin
      if GetLastError = ERROR_NO_TOKEN then
      begin
        bSuccess := OpenProcessToken(GetCurrentProcess,TOKEN_QUERY,hAccessToken);
      end;
    end;
    if bSuccess then
    begin
      GetMem(ptgGroups,1024);
      bSuccess := GetTokenInformation(hAccessToken,TokenGroups,ptgGroups,1024,dwInfoBufferSize);
      if bSuccess then
      begin
        AllocateAndInitializeSid(SECURITY_NT_AUTHORITY,2,SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_ADMINS,0,0,0,0,0,0,psidAdministrators);
        {$R-}
        for X := 0 to ptgGroups.GroupCount - 1 do
        begin
          if EqualSid(psidAdministrators,ptgGroups.Groups[X].Sid) then
          begin
            Result := True;
            Break;
          end;
        end;
        {$R+}
      end;
    end;
  finally
    if bSuccess then
    begin
      CloseHandle(hAccessToken);
    end;
    if Assigned(ptgGroups) then
    begin
      FreeMem(ptgGroups);
    end;
    if Assigned(psidAdministrators) then
    begin
      FreeSid(psidAdministrators);
    end;
  end;
end;

function WinUserExists(UsrNme: String): Boolean;
begin
  Result := False;
  //...   MUSS NOCH GESCHRIEBEN WERDEN!!!
end;

{ ----------------------------------------------------------------------------
  TLine
  ---------------------------------------------------------------------------- }

constructor TLine.Create(AOffset: Integer; ASlope: Extended);
begin
  inherited Create;
  FOffset := AOffset;
  FSlope := ASlope;
end;

constructor TLine.Create(A: TPoint; ASlope: Extended);
begin
  Create(Round(A.Y - A.X * ASlope),ASlope);
end;

constructor TLine.Create(A,B: TPoint);
begin
  inherited Create;
  LoadFromPoints(A,B);
end;

destructor TLine.Destroy;
begin
  //...
  inherited;
end;

procedure TLine.LoadFromPoints(A,B: TPoint);
begin
  Slope := (A.Y - B.Y) / (A.X - B.X);
  Offset := Round(A.Y - Slope * A.X);
end;

function TLine.GetY(X: Integer): Integer;
begin
  Result := Round(Slope * X + Offset);
end;

function TLine.Contains(Point: TPoint): Boolean;
begin
  Result := Y[Point.X] = Point.Y;
end;

function TLine.Intersection(Line: TLine): TPoint;
begin
  if Intersects(Line) then
  begin
    Result.X := Round((Line.Offset - Offset) / (Slope - Line.Slope));
    Result.Y := Y[Result.X];
  end else
  begin
    if Equals(Line) then
    begin
      raise EIntersection.Create('The two lines are identical');
    end else
    begin
      raise EIntersection.Create('The two lines are parallel');
    end;
  end;
end;

function TLine.Intersects(Line: TLine): Boolean;
begin
  Result := not Parallel(Line);
end;

function TLine.Equals(Line: TLine): Boolean;
begin
  Result := (Line.Slope = Slope) and (Line.Offset = Offset);
end;

function TLine.Parallel(Line: TLine): Boolean;
begin
  Result := Line.Slope = Slope;
end;

{ ----------------------------------------------------------------------------
  TCycle
  ---------------------------------------------------------------------------- }

constructor TCicle.Create(ACenter: TPoint; ARadius: Integer);
begin
  inherited Create;
  FCenter := ACenter;
  FRadius := ARadius;
end;

constructor TCicle.Create(ACenter,ABorder: TPoint);
begin
  Create(ACenter,Distance(ACenter,ABorder));
end;

destructor TCicle.Destroy;
begin
  //...
  inherited;
end;

function TCicle.GetDiameter: Integer;
begin
  Result := Radius * 2;
end;

procedure TCicle.SetDiameter(Value: Integer);
begin
  Radius := Round(Value / 2);
end;

function TCicle.GetCircumference: Extended;
begin
  Result := 2 * Pi * Radius;
end;

procedure TCicle.SetCircumference(Value: Extended);
begin
  FRadius := Round(Value / 2 / Pi);
end;

function TCicle.Contains(Point: TPoint): Boolean;
begin
  Result := Distance(Center,Point) <= Radius;
end;

function TCicle.Contains(Cicle: TCicle): Boolean;
begin
  Result := Radius >= Distance(Center,Cicle.Center) + Cicle.Radius;
end;

function TCicle.Intersection(Line: TLine): TPoint;
begin

end;

function TCicle.Intersection(Cicle: TCicle): TPoint;
begin

end;

function TCicle.Intersects(Line: TLine): Boolean;
begin

end;

function TCicle.Intersects(Cicle: TCicle): Boolean;
begin
  Result := Distance(Center,Cicle.Center) <= Radius + Cicle.Radius;
end;

function TCicle.Equals(Cicle: TCicle): Boolean;
begin
  Result := (Cicle.Center = Center) and (Cicle.Radius = Radius);
end;

{ ----------------------------------------------------------------------------
  TRange
  ---------------------------------------------------------------------------- }

constructor TRange.Create;
begin
  inherited;
  FOffset := 0;
  FTarget := 0;
  FStep := 1;
end;

destructor TRange.Destroy;
begin
  //...
  inherited;
end;

procedure TRange.SetOffset(Value: Integer);
begin
  if Value <= Target then
  begin
    FOffset := Value;
  end;
end;

procedure TRange.SetTarget(Value: Integer);
begin
  if Value >= Offset then
  begin
    FTarget := Value;
  end;
end;

function TRange.GetLength: Integer;
begin
  Result := Target - Offset;
end;

procedure TRange.SetLength(Value: Integer);
begin
  if Value >= 0 then
  begin
    Length := Value;
  end;
end;

function TRange.GetCount: Integer;
begin
  Result := Length div Step;
end;

function TRange.GetElements: TIntegerArray;
var
  Index: Integer;
begin
  System.SetLength(Result,Count);
  for Index := 0 to System.Length(Result) - 1 do
  begin
    Result[Index] := Offset + Step * Index;
  end;
end;

{ ----------------------------------------------------------------------------
  TField
  ---------------------------------------------------------------------------- }

constructor TField.Create;
begin
  Inherited;
  FHighest := -1;
  FLowest := -1;
  FLeftest := -1;
  FRightest := -1;
end;

constructor TField.Create(APoints: array of TPoint);
var
  Index: Integer;
begin
  Create;
  AddPoints(APoints);
end;

destructor TField.Destroy;
begin
  //...
  inherited;
end;

function TField.Add(Point: TPoint): Integer;
begin
  Result := PointCount;
  SetLength(FPoints,Succ(Result));
  Points[Result] := Point;
end;

function TField.AddPoints(APoints: array of TPoint): Integer;
var
  Current: TPoint;
begin
  for Current in APoints do
  begin
    Add(Current);
  end;
end;

procedure TField.Delete(Index: Integer);
var
  CurrentIndex: Integer;
begin
  Move(FPoints[Succ(Index)],FPoints[Index],SizeOf(TPoint) * (Length(FPoints) - Succ(Index)));
  SetLength(FPoints,Pred(Length(FPoints)));
  if Index = FHighest then
  begin
    if PointCount = 0 then
    begin
      FHighest := -1;
    end else
    begin
      FHighest := 0;
      for CurrentIndex := 1 to PointCount - 1 do
      begin
        if Points[CurrentIndex].Y > Highest.Y then
        begin
          FHighest := CurrentIndex;
        end;
      end;
    end;
  end else
  begin
    if Index < FHighest then
    begin
      Dec(FHighest);
    end;
  end;
  if Index = FLowest then
  begin
    if PointCount = 0 then
    begin
      FLowest := -1;
    end else
    begin
      FLowest := 0;
      for CurrentIndex := 1 to PointCount - 1 do
      begin
        if Points[CurrentIndex].Y < Lowest.Y then
        begin
          FLowest := CurrentIndex;
        end;
      end;
    end;
  end else
  begin
    if Index < FLowest then
    begin
      Dec(FLowest);
    end;
  end;
  if Index = FLeftest then
  begin
    if PointCount = 0 then
    begin
      FLeftest := -1;
    end else
    begin
      FLeftest := 0;
      for CurrentIndex := 1 to PointCount - 1 do
      begin
        if Points[CurrentIndex].X < Leftest.X then
        begin
          FLeftest := CurrentIndex;
        end;
      end;
    end;
  end else
  begin
    if Index < FLeftest then
    begin
      Dec(FLeftest);
    end;
  end;
  if Index = FRightest then
  begin
    if PointCount = 0 then
    begin
      FRightest := -1;
    end else
    begin
      FRightest := 0;
      for CurrentIndex := 1 to PointCount - 1 do
      begin
        if Points[CurrentIndex].X > Rightest.X then
        begin
          FRightest := CurrentIndex;
        end;
      end;
    end;
  end else
  begin
    if Index < FRightest then
    begin
      Dec(FRightest);
    end;
  end;
end;

procedure TField.Remove(Point: TPoint);
begin
  Delete(IndexOf(Point));
end;

function TField.IndexOf(Point: TPoint): Integer;
begin
  for Result := 0 to PointCount - 1 do
  begin
    if Points[Result] = Point then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TField.Contains(Point: TPoint): Boolean;
begin
  Result := IndexOf(Point) <> -1;
end;

function TField.GetPoints(Index: Integer): TPoint;
begin
  Result := FPoints[Index];
end;

procedure TField.SetPoints(Index: Integer; Value: TPoint);
begin
  FPoints[Index] := Value;
  if (Value.X < Leftest.X) or (FLeftest = -1) then
  begin
    FLeftest := Index;
  end;
  if (Value.X > Rightest.X) or (FRightest = -1) then
  begin
    FRightest := Index;
  end;
  if (Value.Y < Lowest.Y) or (FLowest = -1) then
  begin
    FLowest := Index;
  end;
  if (Value.Y > Highest.Y) or (FHighest = -1) then
  begin
    FHighest := Index;
  end;
end;

function TField.GetPointCount: Integer;
begin
  Result := Length(FPoints);
end;

function TField.GetRect: TRect;
begin
  Result := Classes.Rect(Leftest.X,Highest.Y,Rightest.X,Lowest.Y);
end;

function TField.GetHighest: TPoint;
begin
  if FHighest = -1 then
  begin
    Result := Point(0,0);
  end else
  begin
    Result := Points[FHighest];
  end;
end;

function TField.GetLowest: TPoint;
begin
  if FLowest = -1 then
  begin
    Result := Point(0,0);
  end else
  begin
    Result := Points[FLowest];
  end;
end;

function TField.GetLeftest: TPoint;
begin
  if FLeftest = -1 then
  begin
    Result := Point(0,0);
  end else
  begin
    Result := Points[FLeftest];
  end;
end;

function TField.GetRightest: TPoint;
begin
  if FRightest = -1 then
  begin
    Result := Point(0,0);
  end else
  begin
    Result := Points[FRightest];
  end;
end;

function TField.GetMinDistance: TPoint;
begin

end;

function TField.GetMaxDistance: TPoint;
begin

end;

function TField.GetAverage: TPoint;
var
  Index: Integer;
begin
  if PointCount = 0 then
  begin
    for Index := 0 to PointCount - 1 do
    begin
      Inc(Result.X,Points[Index].X);
      Inc(Result.Y,Points[Index].Y);
    end;
    Result.X := Result.X div PointCount;
    Result.Y := Result.Y div PointCount;
  end else
  begin
    Result := Point(0,0);
  end;
end;

{ ----------------------------------------------------------------------------
  TFilteredStringList
  ---------------------------------------------------------------------------- }

constructor TFilteredStringList.Create;
begin
  inherited;
  FFilteredStrings := TStringList.Create;
  FFiltered := True;
  FFilterMode := lpBeginning;
  FFilterOptions := [sfoCaseSensitive,sfoForceTrim,sfoDefaultVisible];
end;

destructor TFilteredStringList.Destroy;
begin
  FFilteredStrings.Free;
  inherited;
end;

function TFilteredStringList.GetFilteredStrings: TStrings;
begin
  if Filtered then
  begin
    FilterUpdate;
    Result := FFilteredStrings;
  end else
  begin
    Result := Self;
  end;
end;

procedure TFilteredStringList.FilterUpdate;
var
  Index: Integer;
  StringPos: Integer;
  FilterValue: String;
  StringValue: String;
begin
  FFilteredStrings.Clear;
  for Index := 0 to Count - 1 do
  begin
    FilterValue := Filter;
    StringValue := Strings[Index];
    if not (sfoCaseSensitive in FilterOptions) then
    begin
      FilterValue := LowerCase(FilterValue);
      StringValue := LowerCase(StringValue);
    end;
    if (sfoForceTrim in FilterOptions) then
    begin
      FilterValue := Trim(FilterValue);
      StringValue := Trim(StringValue);
    end;
    StringPos := Pos(FilterValue,StringValue);
    if (((FilterMode = lpBeginning) and (StringPos = 1)) or
       ((FilterMode = lpAnyPosition) and (StringPos > 0))) or
       ((Length(FilterValue) = 0) and (sfoDefaultVisible in FilterOptions)) then
    begin
      FFilteredStrings.Add(Strings[Index]);
    end;
  end;
end;

end.
