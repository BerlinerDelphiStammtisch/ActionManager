unit uLocalMgr;

//////////////////////////////////////
///  Lina Localize Manager Unit    ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

uses
  { Standard-Units }
  SysUtils, Classes, Controls, Forms, TypInfo, IniFiles,
  { Andere Package-Units }
  uBase, uSysTools, uFileTools;

type
  { Fehlermeldungen }
  EInvalidFormat = class(Exception);
  ELanguageTagExists = class(Exception);
  ELocalizationParse = class(Exception);

  { Hilfsklassen }
  TLanguageTag = String[3];
  TCommentAllow = type TLinePosition;
  TLocalizationApplyMode = (laCustom,laAll,laMainForm,laOwnerForm,laNone);

  { Ereignisse }
  TLocalizationManagerChangeQueryEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer; var CanChange: Boolean) of object;
  TLocalizationManagerChangeEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer) of object;
  TLocalizationManagerChangeSuccessEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer) of object;
  TLocalizationManagerChangeFailEvent = procedure(Sender: TObject; OldIndex, NewIndex: Integer) of object;

  { Hauptklassen }
  TLocalizationFormat = class(TPersistent)
  private
    { Private-Deklarationen }
    FAllowComment: TCommentAllow;
    FComment: String;
    FSeparator: String;
    FHeader: String;
    FIndent: String;
    FAddress: String;
    FCharacter: String;
    FSpecial: String;
    { Methoden }
    procedure SetComment(Value: String);
    procedure SetSeparator(Value: String);
    procedure SetHeader(Value: String);
    procedure SetIndent(Value: String);
    procedure SetAddress(Value: String);
    procedure SetCharacter(Value: String);
    procedure SetSpecial(Value: String);
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property AllowComment: TCommentAllow read FAllowComment write FAllowComment default lpAnyPosition;
    property Comment: String read FComment write SetComment;         //nicht leer
    property Separator: String read FSeparator write SetSeparator;   //nicht leer
    property Header: String read FHeader write SetHeader;
    property Indent: String read FIndent write SetIndent;
    property Address: String read FAddress write SetAddress;
    property Character: String read FCharacter write SetCharacter;
    property Special: String read FSpecial write SetSpecial;
  end;

  TLocalizationData = class;

  TLocalization = class;

  TLocalizationImporter = class
  private
    { Private-Deklarationen }
    FLocalization: TLocalization;
    FClearBeforeImport: Boolean;
    { Methoden }
    procedure PrepareImport;
    { Eigenschaften }
    property Localization: TLocalization read FLocalization;
  public
    { Public-Deklarationen }
    constructor Create(ALocalization: TLocalization);
    destructor Destroy; override;
    procedure ImportFromData(Data: TLocalizationData);
    procedure ImportFromINI(INI: TIniFile);
    { Eigenschaften }
    property ClearBeforeImport: Boolean read FClearBeforeImport write FClearBeforeImport default True;
  end;

  TLocalization = class(TCollectionItem)
  private
    { Private-Deklarationen }
    FName: ShortString;
    FLines: TStrings;
    FTag: TLanguageTag;
    FFormat: TLocalizationFormat;
    FEncoding: TCharEncoding;
    FConverter: TLocalizationImporter;
    { Methoden }
    function GetDisplayName: String; override;
    function GetLines: TStrings;
    procedure SetLines(Value: TStrings);
    procedure SetName(Value: ShortString);
    procedure SetTag(Value: TLanguageTag);
    function GetFormat: TLocalizationFormat;
    procedure SetFormat(Value: TLocalizationFormat);
    procedure SetEncoding(Value: TCharEncoding);
  protected
    { Protected-Deklarationen }
    procedure RaiseParseError(Text: String; LineIndex: Integer;
      CharIndex: PChar; ExprLength: Integer = 0);
    procedure RaiseParseErrorUnexpected(Expected: String; Found: String; LineIndex: Integer;
      CharIndex: PChar; ExprLength: Integer = 0);
    procedure RaiseParseErrorUndeclared(Identifier: String; LineIndex: Integer;
      CharIndex: PChar; ExprLength: Integer = 0);
  public
    { Public-Deklarationen }
    constructor Create(Collection: TCollection); overload; override;
    constructor Create(Collection: TCollection; const AFileName: TFileName); overload;
    destructor Destroy; override;
    function Apply: Boolean;
  published
    { Published-Deklarationen }
    { Eigenschaften }
    property Name: ShortString read FName write SetName;
    property Tag: TLanguageTag read FTag write SetTag;
    property Lines: TStrings read GetLines write SetLines;
    property Format: TLocalizationFormat read GetFormat write SetFormat;
    property Encoding: TCharEncoding read FEncoding write SetEncoding default {$IFDEF NO_UNICODE} ceUTF8 {$ELSE} ceUnicode {$ENDIF};
    property Converter: TLocalizationImporter read FConverter write FConverter;
  end;

  TLocalizationManager = class;

  TLocalizations = class(TCollection)
  private
    { Private-Deklarationen }
    FManager: TLocalizationManager;
  public
    { Public-Deklarationen }
    constructor Create(AManager: TLocalizationManager);
    destructor Destroy; override;
    function IndexOfTag(const Tag: TLanguageTag): Integer;
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    procedure LoadFromDirectory(const Dir: String; FileExts: array of String; RecDepth: TRecursionDepth);
  end;

  TLocalizationExporter = class
  private
    { Private-Deklarationen }
    FData: TLocalizationData;
    FClearBeforeExport: Boolean;
    { Eigenschaften }
    property Data: TLocalizationData read FData;
  public
    { Public-Deklarationen }
    constructor Create(AData: TLocalizationData);
    destructor Destroy; override;
    procedure ExportToINI(var INI: TIniFile);
  published
    { Published-Deklarationen }
    { Eigenschaften }
    property ClearBeforeExport: Boolean read FClearBeforeExport write FClearBeforeExport default True;
  end;

  TLocalizationData = class
  private
    { Private-Deklarationen }
    Sections: TStringRefDataArrayReferenceDataArray;
    Indents: array of TStringReferenceDataArray;
    Values: TStringArray;
    FManager: TLocalizationManager;
    FExporter: TLocalizationExporter;
  protected
    { Protected-Deklarationen }
    procedure Clear;
    procedure AddSection(Section: String);
    procedure AddIndent(const Section: String; Indent: String);
  public
    { Public-Deklarationen }
    constructor Create(AManager: TLocalizationManager);
    destructor Destroy; override;
    function IndexOfSection(const Section: String): Integer;
    function IndexOfIndent(const Section,Indent: String): Integer;
    function SectionExists(const Section: String): Boolean;
    function IndentExists(const Section,Indent: String): Boolean;
    procedure ReadSections(var ASections: TStrings);
    procedure ReadIndents(const Section: String; var AIndents: TStrings);
    procedure ReadValues(const Section: String; var AValues: TStrings);
    function ReadString(const Section,Indent: String; Default: String): String;
    function ReadInteger(const Section,Indent: String; Default: Integer): Integer;
    function ReadFloat(const Section,Indent: String; Default: Extended): Extended;
    procedure WriteString(const Section,Indent: String; Value: String);
    procedure WriteInteger(const Section,Indent: String; Value: Integer);
    procedure WriteFloat(const Section,Indent: String; Value: Extended);
    procedure Address(const Section,Indent,Target: String);
    property Exporter: TLocalizationExporter read FExporter write FExporter;
  end;

  TLocalizationReferences = class(TCollection)
  private
    { Private-Deklarationen }
    FManager: TLocalizationManager;
  public
    { Public-Deklarationen }
    constructor Create(AManager: TLocalizationManager);
    destructor Destroy; override;
    procedure Apply;
  end;

  TLocalizationReference = class(TCollectionItem)
  private
    { Private-Deklarationen }
    FComponent: TComponent;
    FSection: String;
    FIndent: String;
    FReference: PString;
    FField: String;
    { Methoden }
    procedure SetIndent(Value: String);
    procedure Apply;
  protected
    { Protected-Deklarationen }
    function GetDisplayName: String; override;
  public
    { Public-Deklarationen }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Reference: PString read FReference write FReference;
  published
    { Published-Deklarationen }
    property Component: TComponent read FComponent write FComponent;
    property Section: String read FSection write FSection;
    property Indent: String read FIndent write SetIndent;
    property Field: String read FField write FField;
  end;

  TLocalizationApplier = class
  private
    { Private-Deklarationen }
    FManager: TLocalizationManager;
    FApplyMode: TLocalizationApplyMode;
    { Methoden }
    procedure SetApplyMode(Value: TLocalizationApplyMode);
  public
    { Public-Deklarationen }
    constructor Create(AManager: TLocalizationManager);
    destructor Destroy; override;
    { Methoden }
    procedure Apply;
  protected
    { Protected-Deklarationen }
    procedure ApplyToComponent(Component: TComponent; Field,Section,Indent: String);
    procedure ApplyToForm(Form: TCustomForm);
    procedure ApplyToFormEx(Form: TCustomForm);
    procedure ApplyToAll;
  published
    { Published-Deklarationen }
    property ApplyMode: TLocalizationApplyMode read FApplyMode write SetApplyMode default laCustom;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TLocalizationManager = class(TComponent)
  private
    { Ereignisse}
    FChangeQueryEvent: TLocalizationManagerChangeQueryEvent;
    FChangeEvent: TLocalizationManagerChangeEvent;
    FChangeSuccessEvent: TLocalizationManagerChangeSuccessEvent;
    FChangeFailEvent: TLocalizationManagerChangeFailEvent;
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FLocalizations: TLocalizations;
    FData: TLocalizationData;
    FReferences: TLocalizationReferences;
    FCurrent: Integer;
    FIgnoreCase: Boolean;
    FApplier: TLocalizationApplier;
    { Methoden }
    procedure SetCurrent(Value: Integer);
    procedure SetIgnoreCase(Value: Boolean);
  public
    { Public-Deklarationen }
    constructor Create(AOwnder: TComponent); override;
    destructor Destroy; override;
    { Eigenschaften }
    property Data: TLocalizationData read FData write FData;
  published
    { Published-Deklarationen }
    { Ereignisse}
    property OnChangeQuery: TLocalizationManagerChangeQueryEvent read FChangeQueryEvent write FChangeQueryEvent;
    property OnChange: TLocalizationManagerChangeEvent read FChangeEvent write FChangeEvent;
    property OnChangeSuccess: TLocalizationManagerChangeSuccessEvent read FChangeSuccessEvent write FChangeSuccessEvent;
    property OnChangeFail: TLocalizationManagerChangeFailEvent read FChangeFailEvent write FChangeFailEvent;
    { Eigenschaften }
    property About: TComponentAbout read FAbout;
    property Localizations: TLocalizations read FLocalizations write FLocalizations;
    property References: TLocalizationReferences read FReferences write FReferences;
    property Current: Integer read FCurrent write SetCurrent default -1;
    property IgnoreCase: Boolean read FIgnoreCase write SetIgnoreCase default True;
    property Applier: TLocalizationApplier read FApplier write FApplier;
  end;

  {$IFDEF ADD_COMPONENTREG}
    procedure Register;
  {$ENDIF}

implementation

{$IFDEF ADD_COMPONENTREG}
  procedure Register;
  begin
    RegisterComponents({$IFDEF ADD_SINGLECATEGORY}ComponentsPage{$ELSE}ComponentsPage_Misc{$ENDIF},[TLocalizationManager]);
  end;
{$ENDIF}

{ ----------------------------------------------------------------------------
  TLocalizationFormat
  ---------------------------------------------------------------------------- }

constructor TLocalizationFormat.Create;
begin
  inherited;
  FAllowComment := lpAnyPosition;
  FComment := ';';
  FSeparator := '=';
  FHeader := '*';
  FAddress := '@';
  FCharacter := '$#';
  FSpecial := '!';
end;

destructor TLocalizationFormat.Destroy;
begin
  //...
  inherited;
end;

procedure TLocalizationFormat.SetComment(Value: String);
var
  Index: Integer;
begin
  if (ArrayPos(Value,[FSeparator,FHeader,FIndent,FAddress,FCharacter,FSpecial]) <> -1) or (Length(Value) = 0) then
  begin
    raise EInvalidFormat.Create('Invalid localization format for property: "Comment"');
  end;
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in Spaces then
    begin
      raise EInvalidFormat.Create('Invalid localization format for property: "Comment"');
    end;
  end;
  FComment := Value;
end;

procedure TLocalizationFormat.SetSeparator(Value: String);
var
  Index: Integer;
begin
  if (ArrayPos(Value,[FComment,FHeader,FIndent,FAddress,FCharacter,FSpecial]) <> -1) or (Length(Value) = 0) then
  begin
    raise EInvalidFormat.Create('Invalid localization format for property: "Separator"');
  end;
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in Spaces then
    begin
      raise EInvalidFormat.Create('Invalid localization format for property: "Separator"');
    end;
  end;
  FSeparator := Value;
end;

procedure TLocalizationFormat.SetHeader(Value: String);
var
  Index: Integer;
begin
  if ArrayPos(Value,[FComment,FSeparator,FIndent,FAddress,FCharacter,FSpecial]) <> -1 then
  begin
    raise EInvalidFormat.Create('Invalid localization format for property: "Header"');
  end;
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in Spaces then
    begin
      raise EInvalidFormat.Create('Invalid localization format for property: "Header"');
    end;
  end;
  FHeader := Value;
end;

procedure TLocalizationFormat.SetIndent(Value: String);
var
  Index: Integer;
begin
  if ArrayPos(Value,[FComment,FSeparator,FHeader,FAddress,FCharacter,FSpecial]) <> -1 then
  begin
    raise EInvalidFormat.Create('Invalid localization format for property: "Indent"');
  end;
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in Spaces then
    begin
      raise EInvalidFormat.Create('Invalid localization format for property: "Indent"');
    end;
  end;
  FIndent := Value;
end;

procedure TLocalizationFormat.SetAddress(Value: String);
var
  Index: Integer;
begin
  if (ArrayPos(Value,[FComment,FSeparator,FHeader,FIndent,FCharacter,FSpecial]) <> -1) or (Length(Value) = 0) then
  begin
    raise EInvalidFormat.Create('Invalid localization format for property: "Address"');
  end;
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in Spaces then
    begin
      raise EInvalidFormat.Create('Invalid localization format for property: "Address"');
    end;
  end;
  FAddress := Value;
end;

procedure TLocalizationFormat.SetCharacter(Value: String);
var
  Index: Integer;
begin
  if (ArrayPos(Value,[FComment,FSeparator,FHeader,FIndent,FAddress,FSpecial]) <> -1) or (Length(Value) = 0) then
  begin
    raise EInvalidFormat.Create('Invalid localization format for property: "Character"');
  end;
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in Spaces then
    begin
      raise EInvalidFormat.Create('Invalid localization format for property: "Character"');
    end;
  end;
  FCharacter := Value;
end;

procedure TLocalizationFormat.SetSpecial(Value: String);
var
  Index: Integer;
begin
  if ArrayPos(Value,[FComment,FSeparator,FHeader,FIndent,FAddress,FCharacter]) <> -1 then
  begin
    raise EInvalidFormat.Create('Invalid localization format for property: "Special"');
  end;
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in Spaces then
    begin
      raise EInvalidFormat.Create('Invalid localization format for property: "Special"');
    end;
  end;
  FSpecial := Value;
end;

{ ----------------------------------------------------------------------------
  TLocalizationImporter
  ---------------------------------------------------------------------------- }

constructor TLocalizationImporter.Create(ALocalization: TLocalization);
begin
  inherited Create;
  FLocalization := ALocalization;
  FClearBeforeImport := True;
end;

destructor TLocalizationImporter.Destroy;
begin
  //...
  inherited;
end;

procedure TLocalizationImporter.PrepareImport;
begin
  if ClearBeforeImport then
  begin
    Localization.Lines.Clear;
  end;
  Localization.Lines.Add(Localization.Format.Comment + ' ' + '+++ AUTO-GENERATED LOCALIZATION CONTENT +++');
end;

procedure TLocalizationImporter.ImportFromData(Data: TLocalizationData);
var
  Section: Integer;
  Indent: Integer;
begin
  if Data = nil then
  begin
    Exit;
  end;
  PrepareImport;
  for Section := 0 to Length(Data.Sections) - 1 do
  begin
    if Data.Sections[Section].Value <> '' then
    begin
      Localization.Lines.Add(Localization.Format.Header + ' ' + Data.Sections[Section].Value);
    end;
    for Indent := 0 to Length(Data.Sections[Section].Reference^) - 1 do
    begin
      Localization.Lines.Add(Localization.Format.Indent + ' ' + Data.Sections[Section].Reference^[Indent].Value + ' ' + Localization.Format.Separator + ' ' + Data.Sections[Section].Reference^[Indent].Reference^);
    end;
  end;
end;

procedure TLocalizationImporter.ImportFromINI(INI: TIniFile);
var
  Sections: TStrings;
  Section: Integer;
  Indents: TStrings;
  Indent: Integer;
begin
  if INI = nil then
  begin
    Exit;
  end;
  PrepareImport;
  Sections := TStringList.Create;
  try
    INI.ReadSections(Sections);
    for Section := 0 to Sections.Count - 1 do
    begin
      if Sections.Strings[Section] <> '' then
      begin
        Localization.Lines.Add(Localization.Format.Header + ' ' + Sections[Section]);
      end;
      Indents := TStringList.Create;
      try
        for Indent := 0 to Indents.Count - 1 do
        begin
          Localization.Lines.Add(Localization.Format.Indent + ' ' + Indents.Strings[Indent] + ' ' + Localization.Format.Separator + ' ' + INI.ReadString(Sections.Strings[Section],Indents.Strings[Indent],''));
        end;
      finally
        Indents.Free;
      end;
    end;
  finally
    Sections.Free;
  end;
end;

{ ----------------------------------------------------------------------------
  TLocalization
  ---------------------------------------------------------------------------- }

constructor TLocalization.Create(Collection: TCollection);
begin
  inherited;
  FLines := TStringList.Create;
  FFormat := TLocalizationFormat.Create;
  FTag := 'L' + IntToStrMinLength(ID,2);
  Encoding := {$IFDEF NO_UNICODE} ceUTF8 {$ELSE} ceUnicode {$ENDIF};
  FConverter := TLocalizationImporter.Create(Self);
end;

constructor TLocalization.Create(Collection: TCollection; const AFileName: TFileName);
begin
  Create(Collection);
  Lines.LoadFromFile(AFileName);
  FConverter.Free;
end;

destructor TLocalization.Destroy;
begin
  FLines.Free;
  FFormat.Free;
  inherited;
end;

function TLocalization.GetDisplayName: String;
begin
  inherited;
  Result := FTag;
end;

function TLocalization.GetLines: TStrings;
begin
  Result := FLines;
end;

procedure TLocalization.SetLines(Value: TStrings);
begin
  (FLines as TStringList).Assign(Value);
  if FLines <> nil then
  begin
    FLines.DefaultEncoding := EncodingClass(Encoding);
  end;
end;

procedure TLocalization.SetTag(Value: TLanguageTag);
begin
  Value := Trim(Value);
  if ((Collection as TLocalizations).IndexOfTag(Value) > -1) and
     ((Collection as TLocalizations).IndexOfTag(Value) <> Index)then
  begin
    raise ELanguageTagExists.Create('Language tag already exists');
  end else
  begin
    FTag := UpperCase(Value);
  end;
end;

procedure TLocalization.SetName(Value: ShortString);
begin
  Value := Trim(Value);
  if Value[1] in ['a'..'z'] then
  begin
    Value[1] := UpCase(Value[1]);
  end;
  FName := Value;
end;

function TLocalization.GetFormat: TLocalizationFormat;
begin
  Result := FFormat;
end;

procedure TLocalization.SetFormat(Value: TLocalizationFormat);
begin
  FFormat.Assign(FFormat);
end;

procedure TLocalization.SetEncoding(Value: TCharEncoding);
begin
  Lines.DefaultEncoding := EncodingClass(Value);
  FEncoding := Value;
end;

procedure TLocalization.RaiseParseError(Text: String; LineIndex: Integer;
  CharIndex: PChar; ExprLength: Integer = 0);
begin
  raise ELocalizationParse.Create('[' + IntToStr(LineIndex + 1) + '.' + IntToStr(CharPosition(CharIndex,Lines.Strings[LineIndex]) - ExprLength) + ']' + ' ' + Text);
end;

procedure TLocalization.RaiseParseErrorUnexpected(Expected: String; Found: String; LineIndex: Integer;
  CharIndex: PChar; ExprLength: Integer = 0);
begin
  RaiseParseError(Expected +  ' expected, but ' + Found + ' found',LineIndex,CharIndex,ExprLength);
end;

procedure TLocalization.RaiseParseErrorUndeclared(Identifier: String; LineIndex: Integer;
  CharIndex: PChar; ExprLength: Integer = 0);
begin
  RaiseParseError('Undeclared indent: ' + '"' + Identifier + '"',LineIndex,CharIndex,ExprLength);
end;

function TLocalization.Apply: Boolean;
{ Compiliert den Inhalt eines TStrings-Objects zeilenweise zu einem Konstrukt
  aus Record-Arrays und Records mit je einem String-Wert und einem Pointer zur
  Referenzierung (siehe hierzu: uSysTools.TStringReferenceData etc.).
  Hierrauf wird im Anschluss durch Methoden der Klasse TLocalizationData
  zugegriffen.
  Die Datenstruktur entspricht dem folgenden Schema:

     [SECTIONS]     [INDENTS]      [VALUES]
  |- 0 ------------ 0 ------------ 0
  |  \           |  \
  |   Titel      |   Titel
  |              |
  |              |- 1 ------------ 1
  |                 \
  |                  Titel
  |
  |- 1 ------------ 2 ------------ 2
     \              \
      Titel          Titel

  Die Syntax entspricht den in der TLocalizationFormat-Klasse festgelegten
  Bausteinen und Einstellungen, die zur Laufzeit für jede Sprache Individuell
  festgelegt werden kann.

  Spezielle Direktiven:
  - "LOC <Name>": Namen der aktuellen Sprache ändern
  - "TAG <Tag>": Sprachkürzel der aktuellen Sprache ändern
  - "INC <Sprache>": Andere Sprache einbinden
  - "END": Kompilierung beenden
  - "APP <Anwendung>": Fortfahren, falls der Titel der Anwendung übereinstimmt
  - "NOT <Anwendung>": Beenden, falls der Titel der Anwendung übereinstimmt }
const
  Specials: array [0..5] of String = ('loc','tag','inc','end','app','not');
var
  Position: (posPrefix,posIndent,posSeparator,posValue);
  Line: Integer;
  Complete: Boolean;
  Finished: Boolean;
  Header: Boolean;
  Special: Boolean;
  Address: Boolean;
  Character: Boolean;
  Current: PChar;
  InComment: PChar;
  InCharacter: PChar;
  Block: String;
  Section: String;
  Indent: String;
  Ordinal: String;
label
  EoL;
begin
  (Collection as TLocalizations).FManager.Data.Clear;
  Result := False;
  Section := '';
  Indent := '';
  Ordinal := '';
  for Line := 0 to Lines.Count - 1 do
  begin
    //Leere Zeile
    if Length(Lines.Strings[Line]) = 0 then
    begin
      Continue;
    end;
    //Zeilenanfang
    Position := posPrefix;
    Header := False;
    Special := False;
    Address := False;
    Complete := False;
    Finished := False;
    Character := False;
    Current := @Lines.Strings[Line][1];
    InComment := @Format.Comment[1];
    InCharacter := @Format.Character[1];
    //Parsen
    while not Complete do
    begin
      if ((not Address) or (Current^ <> #0)) and (not (Current^ in Spaces)) or ((((not Address) and (Position = posValue)) or Header) and (Length(Block) <> 0)) then
      begin
        //Zeichen zu Block hinzufügen
        if Current^ <> #0 then
        begin
          if not Character then
          begin
            Block := Block + Current^;
          end else
          begin
            Ordinal := Ordinal + Current^;
          end;
        end;
        if (Current^ = #0{@Lines.Strings[Line][Length(Lines.Strings[Line])]}) or (Character and (Current^ in Spaces)) then
        begin
          goto EoL;
        end else
        begin
          //Kommentare in Value (bei Indents) bzw. Indent (bei Headern)
          if Format.AllowComment = lpAnyPosition then
          begin
            if InComment^ = Current^ then
            begin
              Inc(InComment);
            end else
            begin
              if (InComment^ = #0) and (Current^ in Spaces) then
              begin
                SetLength(Block,Length(Block) - Length(Format.Comment) - 1);
                goto EoL;
              end else
              begin
                InComment := @Format.Comment[1];
              end;
            end;
          end;
          //Character-Unterstützung in Value
          if Position = posValue then
          begin
            if InCharacter^ = Current^ then
            begin
              Inc(InCharacter);
            end else
            begin
              if (InCharacter^ = #0) and (Current^ in Spaces) then
              begin
                Character := True;
                SetLength(Block,Length(Block) - Length(Format.Character) - 1);
              end else
              begin
                InCharacter := @Format.Character[1];
              end;
            end;
          end;
        end;
      end else
      begin
        //Alternativ auch bei Zeilenende
        EoL:
          if Length(Ordinal) <> 0 then
          begin
            if Length(Block) <> 0 then
            begin
              SetLength(Block,Length(Block) - 1);
              Insert(Chr(StrToInt(Trim(Ordinal))),Block,Length(Block) + 1);
            end else
            begin
              Block := Chr(StrToInt(Trim(Ordinal)));
            end;
            Ordinal := '';
            Character := False;
            if Current^ <> #0 then
            begin
              Inc(Current);
              Continue;
            end;
          end;
          if (Length(Block) <> 0) or ((Position = posValue) and (not Address)) or ((Position = posIndent) and Header) then
          begin
            if //Kommentaranfang
               ((Block = Format.Comment) and ((Format.AllowComment = lpAnyPosition) or ((Format.AllowComment = lpBeginning) and (Current = @Lines.Strings[Line][1]))) or (Current^ = #0{@Lines.Strings[Line][Length(Lines.Strings[Line])]}) or (InComment^ = #0)) or
               //Zeilenende
               ((Current^ = #0{@Lines.Strings[Line][Length(Lines.Strings[Line])]}) and (not (Current^ in Spaces))) or
               //Adressenende
               Address then
            begin
              //Block fertig
              if ((Position = posSeparator) and (Block = Format.Separator)) or ((Block = Format.Comment) and ((Position = posPrefix) or Finished)) or (Header and (Position = posIndent) and (Length(Block) <> 0)) or (((Position = PosValue) or ((Current^ = #0) and Special)) and ((not Address) or ((Length(Block) <> 0) or Special))) then
              begin
                if Position = posSeparator then
                begin
                  Block := '';
                end;
                if Header then
                begin
                  //Ende von Header
                  Section := TrimRight(Block);
                end else
                begin
                  //Ende von Value
                  if (not Finished) and (Block <> Format.Comment) then
                  begin
                    //Ende von Special
                    if Special then
                    begin
                      if Length(Indent) = 0 then
                      begin
                        Indent := LowerCase(Block);
                        Block := '';
                      end;
                      if Indent = Specials[0] then
                      begin
                        //LOC abc
                        Name := Block;
                      end else
                      begin
                        if Indent = Specials[1] then
                        begin
                          //TAG abc
                          Tag := Block;
                        end else
                        begin
                          if Indent = Specials[2] then
                          begin
                            //INC abc
                            ((Collection as TLocalizations).Items[(Collection as TLocalizations).IndexOfTag(Trim(Block))] as TLocalization).Apply;
                          end else
                          begin
                            if Indent = Specials[3] then
                            begin
                              //END
                              if Length(Trim(Block)) <> 0 then
                              begin
                                RaiseParseErrorUnexpected('End of line','expression',Line,Current,Length(Block));
                              end;
                              Exit;
                            end else
                            begin
                              if Indent = Specials[4] then
                              begin
                                //APP abc
                                Block := Trim(Block);
                                if (Length(Block) <> 0) and (Position = posValue) then
                                begin
                                  if Application.Title <> Block then
                                  begin
                                    Exit;
                                  end;
                                end else
                                begin
                                  RaiseParseErrorUnexpected('Argument','end of line',Line,Current);
                                  Exit;
                                end;
                              end else
                              begin
                                if Indent = Specials[5] then
                                begin
                                  //NOT abc
                                  if (Length(Block) <> 0) and (Position = posValue) then
                                  begin
                                    if Application.Title = Block then
                                    begin
                                      Exit;
                                    end;
                                  end else
                                  begin
                                    RaiseParseErrorUnexpected('Argument','end of line',Line,Current);
                                    Exit;
                                  end;
                                end else
                                begin
                                  RaiseParseErrorUnexpected('End of line','expression',Line,Current);
                                  Exit;
                                end;
                              end;
                            end;
                          end;
                        end;
                      end;
                    end else
                    begin
                      //Ende von Indent
                      if not Address then
                      begin
                        //Wert
                        (Collection as TLocalizations).FManager.Data.WriteString(Section,Indent,TrimRight(Block));
                      end else
                      begin
                        //Adressierung
                        Complete := (Current^ = #0{@Lines.Strings[Line][Length(Lines.Strings[Line])]});
                        if (Collection as TLocalizations).FManager.Data.IndentExists(Section,Block) then
                        begin
                            (Collection as TLocalizations).FManager.Data.Address(Section,Indent,Block);
                            Finished := True;
                            Block := '';
                            Inc(Current);
                            Continue;
                        end else
                        begin
                          RaiseParseErrorUndeclared(Block,Line,Current,Length(Block));
                          Exit;
                        end;
                      end;
                    end;
                  end;
                end;
                Complete := True;
              end else
              begin
                RaiseParseErrorUnexpected('Argument','end of line',Line,Current);
                Exit;
              end;
            end else
            begin
              case Position of
                posPrefix: begin
                             if (Block <> Format.Header) and (Block <> Format.Indent) and (Block <> Format.Special) then
                             begin
                               if (Length(Format.Header) = 0) or (Length(Format.Indent) = 0) or (Length(Format.Special) = 0) then
                               begin
                                 Header := (Length(Format.Header) = 0);
                                 Special := (Length(Format.Special) = 0);
                                 Position := posIndent;
                                 Continue;
                               end else
                               begin
                                 RaiseParseErrorUnexpected('Prefix','"' + Block + '"',Line,Current,Length(Block));
                                 Exit;
                               end;
                             end;
                             Header := (Block = Format.Header);
                             Special := (Block = Format.Special);
                             Inc(Position);
                           end;
                posIndent: begin
                             Indent := LowerCase(TrimRight(Block));
                             if not Header then
                             begin
                               Inc(Position);
                               if Special then
                               begin
                                 if ArrayPos(Indent,Specials) <> -1 then
                                 begin
                                   Inc(Position);
                                 end else
                                 begin
                                   RaiseParseErrorUnexpected('Command','"' + Block + '"',Line,Current,Length(Block));
                                   Exit;
                                 end;
                                { if Indent = Specials[3] then
                                 begin
                                   Finished := True;
                                 end;   }
                               end;
                             end;
                           end;
                posSeparator: begin
                                Address := (Block = Format.Address);
                                if (Block <> Format.Separator) and (not Address) then
                                begin
                                  RaiseParseErrorUnexpected('Separator or address mark','"' + Block + '"',Line,Current,Length(Block));
                                  Exit;
                                end;
                                Inc(Position);
                              end;
                posValue: begin
                            if Finished and ((InComment^ <> #0) or (not (Current^ in Spaces))) then
                            begin
                              RaiseParseErrorUnexpected('End of line','"' + Block + '"',Line,Current,Length(Block));
                              Exit;
                            end;
                          end;
              end;
            end;
            Block := '';
          end;
      end;
      Inc(Current);
    end;
    Indent := '';
  end;
  Result := True;
end;

{ ----------------------------------------------------------------------------
  TLocalizations
  ---------------------------------------------------------------------------- }

constructor TLocalizations.Create(AManager: TLocalizationManager);
begin
  inherited Create(TLocalization);
  FManager := AManager;
end;

destructor TLocalizations.Destroy;
begin
  FManager := nil;
  inherited;
end;

function TLocalizations.IndexOfTag(const Tag: TLanguageTag): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := 0 to Count - 1 do
  begin
    if (Items[Index] as TLocalization).Tag = UpperCase(Tag) then
    begin
      Result := Index;
      Exit;
    end;
  end;
end;

procedure TLocalizations.LoadFromFile(const FileName: String);
var
  Languages: TStrings;
  Index: Integer;
begin
  Languages := TStringList.Create;
  try
    Languages.LoadFromFile(FileName);
    for Index := 0 to Languages.Count - 1 do
    begin
      (Add as TLocalization).Name := Languages.Strings[Index];
    end;
  finally
    Languages.Free;
  end;
end;

procedure TLocalizations.SaveToFile(const FileName: String);
var
  Languages: TStrings;
  Index: Integer;
begin
  Languages := TStringList.Create;
  try
    for Index := 0 to Count - 1 do
    begin
      Languages.Add((Items[Index] as TLocalization).Name);
    end;
    Languages.SaveToFile(FileName);
  finally
    Languages.Free;
  end;
end;

procedure TLocalizations.LoadFromDirectory(const Dir: String; FileExts: array of String; RecDepth: TRecursionDepth);
var
  Languages: TStrings;
  Language_Index: Integer;
begin
  Languages := TStringList.Create;
  try
    ListFiles(Dir,Languages,FileExts,[fnDirectory,fnExtension],RecDepth);
    for Language_Index := 0 to Languages.Count - 1 do
    begin
      with (Add as TLocalization) do
      begin
        Name := ExtractFileName(ChangeFileExt(Languages.Strings[Language_Index],''));
        Lines.LoadFromFile(Languages.Strings[Language_Index]);
      end;
    end;
  finally
    Languages.Free;
  end;
end;

{ ----------------------------------------------------------------------------
  TLocalizationExporter
  ---------------------------------------------------------------------------- }

constructor TLocalizationExporter.Create(AData: TLocalizationData);
begin
  inherited Create;
  FData := AData;
  FClearBeforeExport := True;
end;

destructor TLocalizationExporter.Destroy;
begin
  //...
  inherited;
end;

procedure TLocalizationExporter.ExportToINI(var INI: TIniFile);
var
  Section: Integer;
  Sections: TStrings;
  Indent: Integer;
begin
  if INI = nil then
  begin
    Exit;
  end;
  if ClearBeforeExport then
  begin
    Sections := TStringList.Create;
    INI.ReadSections(Sections);
    for Section := 0 to Sections.Count - 1 do
    begin
      INI.EraseSection(Sections.Strings[Section]);
    end;
  end;

    {
  Localization.Lines.Add(Localization.Format.Comment + ' ' + '+++ AUTO-GENERATED LOCALIZATION CONTENT +++');

  Sections := TStringList.Create;
  try
    INI.ReadSections(Sections);
    for Section := 0 to Sections.Count - 1 do
    begin
      if Sections.Strings[Section] <> '' then
      begin
        Localization.Lines.Add(Localization.Format.Header + ' ' + Sections[Section]);
      end;
      Indents := TStringList.Create;
      try
        for Indent := 0 to Indents.Count - 1 do
        begin
          Localization.Lines.Add(Localization.Format.Indent + ' ' + Indents.Strings[Indent] + ' ' + Localization.Format.Separator + ' ' + INI.ReadString(Sections.Strings[Section],Indents.Strings[Indent],''));
        end;
      finally
        Indents.Free;
      end;
    end;
  finally
    Sections.Free;
  end;   }
end;

{ ----------------------------------------------------------------------------
  TLocalizationData
  ---------------------------------------------------------------------------- }

constructor TLocalizationData.Create(AManager: TLocalizationManager);
begin
  inherited Create;
  FManager := AManager;
  FExporter := TLocalizationExporter.Create(Self);
end;

destructor TLocalizationData.Destroy;
begin
  FManager := nil;
  FExporter.Free;
  inherited;
end;

procedure TLocalizationData.Clear;
begin
  SetLength(Sections,0);
  SetLength(Indents,0);
  SetLength(Values,0);
end;

procedure TLocalizationData.AddSection(Section: String);
begin
  SetLength(Sections,Length(Sections) + 1);
  SetLength(Indents,Length(Indents) + 1);
  Sections[High(Sections)].Reference := @Indents[High(Indents)];
  Sections[High(Sections)].Value := Section;
end;

procedure TLocalizationData.AddIndent(const Section: String; Indent: String);
{ Diese Methode soll NICHT direkt aufgerufen werden!
  Stattdessen die Methode WriteString() bzw. Address() verwenden, welche unter
  anderem auf diese Methode zurückgreift. Ansonsten kann es zu
  Zugriffsverletzungen oder nicht addressierten Werten kommen. }
begin
  SetLength(Sections[IndexOfSection(Section)].Reference^,Length(Sections[IndexOfSection(Section)].Reference^) + 1);
  SetLength(Values,Length(Values) + 1);
  Sections[IndexOfSection(Section)].Reference^[High(Sections[IndexOfSection(Section)].Reference^)].Reference := @Values[High(Values)];
  Sections[IndexOfSection(Section)].Reference^[High(Sections[IndexOfSection(Section)].Reference^)].Value := Indent;
end;

function TLocalizationData.IndexOfSection(const Section: String): Integer;
begin
  Result := ArrayPosRef(Section,Sections,FManager.IgnoreCase);
end;

function TLocalizationData.IndexOfIndent(const Section,Indent: String): Integer;
var
  SectionIndex: Integer;
begin
  SectionIndex := IndexOfSection(Section);
  if SectionIndex <> -1 then
  begin
    Result := ArrayPosRef(Indent,Sections[SectionIndex].Reference^,FManager.IgnoreCase);
  end else
  begin
    Result := SectionIndex;
  end;
end;

function TLocalizationData.SectionExists(const Section: String): Boolean;
begin
  Result := (IndexOfSection(Section) >= 0);
end;

function TLocalizationData.IndentExists(const Section,Indent: String): Boolean;
begin
  Result := (IndexOfIndent(Section,Indent) >= 0);
end;

procedure TLocalizationData.ReadSections(var ASections: TStrings);
var
  Index: Integer;
begin
  ASections.Clear;
  for Index := Low(Sections) to High(Sections) do
  begin
    ASections.Add(Sections[Index].Value);
  end;
end;

procedure TLocalizationData.ReadIndents(const Section: String; var AIndents: TStrings);
var
  SectionIndex: Integer;
  Index: Integer;
begin
  AIndents.Clear;
  SectionIndex := IndexOfSection(Section);
  if SectionIndex <> -1 then
  begin
    for Index := Low(Sections[SectionIndex].Reference^) to High(Sections[SectionIndex].Reference^) do
    begin
      AIndents.Add(Sections[IndexOfSection(Section)].Reference^[Index].Value);
    end;
  end;
end;

procedure TLocalizationData.ReadValues(const Section: String; var AValues: TStrings);
var
  Index: Integer;
begin
  AValues.Clear;
  for Index := Low(Sections[IndexOfSection(Section)].Reference^) to High(Sections[IndexOfSection(Section)].Reference^) do
  begin
    AValues.Add(Sections[IndexOfSection(Section)].Reference^[Index].Reference^);
  end;
end;

function TLocalizationData.ReadString(const Section,Indent: String; Default: String): String;
begin
  if IndentExists(Section,Indent) then
  begin
    Result := Sections[IndexOfSection(Section)].Reference^[IndexOfIndent(Section,Indent)].Reference^;
  end else
  begin
    Result := Default;
  end;
end;

function TLocalizationData.ReadInteger(const Section,Indent: String; Default: Integer): Integer;
begin
  Result := StrToInt(ReadString(Section,Indent,IntToStr(Default)));
end;

function TLocalizationData.ReadFloat(const Section,Indent: String; Default: Extended): Extended;
begin
  Result := StrToFloat(ReadString(Section,Indent,FloatToStr(Default)));
end;

procedure TLocalizationData.WriteString(const Section,Indent: String; Value: String);
begin
  if not SectionExists(Section) then
  begin
    AddSection(Section);
  end;
  if not IndentExists(Section,Indent) then
  begin
    AddIndent(Section,Indent);
  end;
  if ReadString(Section,Indent,'') <> Value then
  begin
    Sections[IndexOfSection(Section)].Reference^[IndexOfIndent(Section,Indent)].Reference^ := Value;
  end;
end;

procedure TLocalizationData.WriteInteger(const Section,Indent: String; Value: Integer);
begin
  WriteString(Section,Indent,IntToStr(Value));
end;

procedure TLocalizationData.WriteFloat(const Section,Indent: String; Value: Extended);
begin
  WriteString(Section,Indent,FloatToStr(Value));
end;

procedure TLocalizationData.Address(const Section,Indent,Target: String);
begin
  if not SectionExists(Section) then
  begin
    AddSection(Section);
  end;
  if not IndentExists(Section,Indent) then
  begin
    AddIndent(Section,Indent);
  end;
  Sections[IndexOfSection(Section)].Reference^[IndexOfIndent(Section,Indent)].Reference := Sections[IndexOfSection(Section)].Reference^[IndexOfIndent(Section,Target)].Reference;
end;

{ ----------------------------------------------------------------------------
  TLocalizationReferences
  ---------------------------------------------------------------------------- }

constructor TLocalizationReferences.Create(AManager: TLocalizationManager);
begin
  inherited Create(TLocalizationReference);
  FManager := AManager;
end;

destructor TLocalizationReferences.Destroy;
begin
  FManager := nil;
  inherited;
end;

procedure TLocalizationReferences.Apply;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    (Items[Index] as TLocalizationReference).Apply;
  end;
end;

{ ----------------------------------------------------------------------------
  TLocalizationReference
  ---------------------------------------------------------------------------- }

constructor TLocalizationReference.Create(Collection: TCollection);
begin
  inherited;
  Section := '';
  Indent := 'Reference' + IntToStr(ID);
  Field := '';
end;

destructor TLocalizationReference.Destroy;
begin
  Component := nil;
  Reference := nil;
  inherited;
end;

function TLocalizationReference.GetDisplayName: String;
begin
  inherited;
  Result := Indent;
  if Length(Section) <> 0 then
  begin
    Result := Section + '.' + Result;
  end;
  if Component <> nil then
  begin
    Result := Result + ' (' + Component.Name + '.' + Field + ')';
  end;
end;

procedure TLocalizationReference.SetIndent(Value: String);
var
  Index: Integer;
begin
  if Length(Value) = 0 then
  begin
    raise EInvalidFormat.Create('Invalid reference format for property: "Indent"');
  end;
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in Spaces then
    begin
      raise EInvalidFormat.Create('Invalid reference format for property: "Indent"');
    end;
  end;
  FIndent := Value;
end;

procedure TLocalizationReference.Apply;
begin
  if Reference <> nil then
  begin
    Reference^ := (Collection as TLocalizationReferences).FManager.Data.ReadString(Section,Indent,Reference^);
  end;
  (Collection as TLocalizationReferences).FManager.Applier.ApplyToComponent(Component,Field,Section,Indent);
end;

{ ----------------------------------------------------------------------------
  TLocalizationApplier
  ---------------------------------------------------------------------------- }

constructor TLocalizationApplier.Create(AManager: TLocalizationManager);
begin
  inherited Create;
  FManager := AManager;
  FApplyMode := laCustom;
end;

destructor TLocalizationApplier.Destroy;
begin
  FManager := nil;
  inherited;
end;

procedure TLocalizationApplier.SetApplyMode(Value: TLocalizationApplyMode);
begin
  FApplyMode := Value;
  Apply;
end;

procedure TLocalizationApplier.Apply;
begin
  case FApplyMode of
    laCustom: FManager.References.Apply;
    laAll: ApplyToAll;
    laMainForm: ApplyToFormEx(Application.MainForm);
    laOwnerForm: ApplyToFormEx(GetOwnerForm(FManager));
  end;
end;

procedure TLocalizationApplier.ApplyToComponent(Component: TComponent; Field,Section,Indent: String);
begin
  if Assigned(Component) and IsPublishedSubProp(Component,Field) then
  begin
    SetStrSubProp(Component,Field,FManager.Data.ReadString(Section,Indent,GetStrSubProp(Component,Field)));
  end;
end;

procedure TLocalizationApplier.ApplyToForm(Form: TCustomForm);
{ Empfehlenswert, falls mehrere Formulare lokalisiert werden sollen. Falls ein
  einziges Formular automatisch lokalisiert werden soll, sollte die Methode
  "ApplyToFormEx(TCustomForm)" verwendet werden.
  Definiert die Eigenschaften eines Formulars und deren Komponenten über den
  Inhalt der TLocalization.Lines-Eigenschaft.
  Es wird erwartet, dass die Definitionen so vorliegen, dass jedes Formular
  einen eigenen Abschnitt besitzt und jedes zu definierende Feld ein Eintrag
  ist. Für Eigenschaften von Komponenten müssen diese dem Namen des Eintrags
  vorweg-gestellt sein. }
var
  Index: Integer;
  Indents: TStrings;
begin
  if Assigned(Form) then
  begin
    Indents := TStringList.Create;
    try
      FManager.Data.ReadIndents(Form.Name,Indents);
      for Index := 0 to Indents.Count - 1 do
      begin
        ApplyToComponent(Form,Indents.Strings[Index],Form.Name,Indents.Strings[Index]);
      end;
    finally
      Indents.Free;
    end;
  end;
end;

procedure TLocalizationApplier.ApplyToFormEx(Form: TCustomForm);
{ Empfehlenswert, falls ein einziges Formular automatisch lokalisiert werden
  soll. Falls mehrere Formulare lokalisiert werden sollen, sollte die Methode
  "ApplyToForm(TCustomForm)" verwendet werden.
  Definiert die Eigenschaften eines Formulars und deren Komponenten EXKLUSIV (!)
  über den Inhalt der TLocalization.Lines-Eigenschaft.
  Es wird erwartet, dass die Definitionen so vorliegen, dass jede Komponente
  einen eigenen Abschnitt besitzt und jedes zu definierende Feld ein Eintrag
  ist. Für das Formular selber ist der namenlose (Kopf-)Abschnitt vorgesehen. }
var
  Index_Section: Integer;
  Index_Indent: Integer;
  Indents: TStrings;
begin
  if Assigned(Form) then
  begin
    Indents := TStringList.Create;
    try
      //Formular
      FManager.Data.ReadIndents('',Indents);
      for Index_Indent := 0 to Indents.Count - 1 do
      begin
        ApplyToComponent(Form,Indents.Strings[Index_Indent],'',Indents.Strings[Index_Indent]);
      end;
      //Komponenten
      for Index_Section := 0 to Form.ComponentCount - 1 do
      begin
        FManager.Data.ReadIndents(Form.Components[Index_Section].Name,Indents);
        for Index_Indent := 0 to Indents.Count - 1 do
        begin
          ApplyToComponent(Form.Components[Index_Section],Indents.Strings[Index_Indent],Form.Components[Index_Section].Name,Indents.Strings[Index_Indent]);
        end;
      end;
    finally
      Indents.Free;
    end;
  end;
end;

procedure TLocalizationApplier.ApplyToAll;
var
  Index: Integer;
begin
  for Index := 0 to Screen.CustomFormCount - 1 do
  begin
    ApplyToForm(Screen.Forms[Index]);
  end;
end;

{ ----------------------------------------------------------------------------
  TLocalizationManager
  ---------------------------------------------------------------------------- }

constructor TLocalizationManager.Create(AOwnder: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TLocalizationManager);
  FLocalizations := TLocalizations.Create(Self);
  FData := TLocalizationData.Create(Self);
  FReferences := TLocalizationReferences.Create(Self);
  FApplier := TLocalizationApplier.Create(Self);
  FCurrent := -1;
  FIgnoreCase := True;
end;

destructor TLocalizationManager.Destroy;
begin
  FAbout.Free;
  FLocalizations.Free;
  FData.Free;
  FReferences.Free;
  FApplier.Free;
  inherited;
end;

procedure TLocalizationManager.SetCurrent(Value: Integer);
var
  AllowChange: Boolean;
begin
  if Assigned(OnChangeQuery) then
  begin
    AllowChange := True;
    OnChangeQuery(Self,FCurrent,Value,AllowChange);
    if not AllowChange then
    begin
      Exit;
    end;
  end;
  if Value > -1 then
  begin
    if (Localizations.Items[Value] as TLocalization).Apply then
    begin
      if Assigned(OnChangeSuccess) then
      begin
        OnChangeSuccess(Self,FCurrent,Value);
      end;
    end else
    begin
      if Assigned(OnChangeFail) then
      begin
        OnChangeFail(Self,FCurrent,Value);
      end;
      Exit;
    end;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self,FCurrent,Value);
  end;
  FCurrent := Value;
  FApplier.Apply;
end;

procedure TLocalizationManager.SetIgnoreCase(Value: Boolean);
begin
  Current := Current;
  FIgnoreCase := Value;
end;

end.
