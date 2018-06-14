unit uWebCtrls;

//////////////////////////////////////
///  Lina Web Controls Unit        ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

uses
  { Standard-Units }
  SysUtils, Classes, UITypes,
  { Indy-Units }
  {$IFDEF ADD_DEPENDENCIES}
    idHTTP, idSSLOpenSSL, idComponent,
  {$ENDIF}
  { Andere Package-Units }
  uBase, uSysTools;

type
  { Fehlermeldungen }
  {$IFDEF ADD_DEPENDENCIES}
    EInvalidWebAddress = class(Exception);
  {$ENDIF}
  EInvalidTagChar = class(Exception);
  EHtmlParse = class(Exception);
  ECssParse = class(Exception);

  { Hilfsklassen }
  TCssDocumentRuleKind = (cssNone,cssTag,cssClass,cssID);

  { Ereignisse }
  {$IFDEF ADD_DEPENDENCIES}
    TDownloadWorkEvent = procedure(Sender: TObject; AWorkMode: TWorkMode) of object;
    TDownloadWorkBeginEvent = procedure(Sender: TObject; AWorkMode: TWorkMode) of object;
    TDownloadWorkEndEvent = procedure(Sender: TObject; AWorkMode: TWorkMode) of object;
  {$ENDIF}
  THtmlDocumentTag = class;
  THtmlDocumentTagAddEvent = procedure(Sender: TObject; Tag: THtmlDocumentTag) of object;
  THtmlDocumentParseEvent = procedure(Sender: TObject) of object;
  THtmlDocumentParseSuccessEvent = procedure(Sender: TObject) of object;
  THtmlDocumentParseFailEvent = procedure(Sender: TObject) of object;

  { Hauptklassen }
  {$IFDEF ADD_DEPENDENCIES}
    {$IFNDEF NO_MULTIPLATFORM}
      [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
    {$ENDIF}
    TDownload = class(TComponent)
    private
      { Private-Deklarationen }
      idHTTPObject: TidHTTP;
      SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
      FAbout: TComponentAbout;
      FAddress: String;
      FProgress: Int64;
      FProgressMax: Int64;
      FSSL: Boolean;
      { Ereignisse }
      FWorkEvent: TDownloadWorkEvent;
      FWorkBeginEvent: TDownloadWorkBeginEvent;
      FWorkEndEvent: TDownloadWorkEndEvent;
      { Methoden }
      procedure SetAddress(Value: String);
      procedure idHTTPObjectWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
      procedure idHTTPObjectWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
      procedure idHTTPObjectWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    protected
      { Protected-Deklarationen }
      procedure Prepare;
    public
      { Public-Deklarationen }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      { Fortschritt }
      function GetProgress: Int64;
      function GetProgressMax: Int64;
      function GetProgressPercent: Byte;
      { Herunterladen }
      function GetText: String;
      procedure SaveToFile(const FileName: TFileName);
      procedure SaveToStream(var Stream: TStream);
    published
      { Published-Deklarationen }
      { Ereignisse }
      property OnWork: TDownloadWorkEvent read FWorkEvent write FWorkEvent;
      property OnWorkBegin: TDownloadWorkBeginEvent read FWorkBeginEvent write FWorkBeginEvent;
      property OnWorkEnd: TDownloadWorkEndEvent read FWorkEndEvent write FWorkEndEvent;
      { Eigenschaften }
      property About: TComponentAbout read FAbout;
      property Address: String read FAddress write SetAddress;
      property SSL: Boolean read FSSL write FSSL default False;
    end;
  {$ENDIF}

  THtmlDocumentTagParam = record
    Name: String;
    Value: String;
  end;

  THtmlDocument = class;

  THtmlDocumentTag = class
  private
    { Private-Deklarationen }
    FName: String;
    FIndex: Integer;
    FId: Integer;
    FDocument: THtmlDocument;
    FParent: THtmlDocumentTag;
    FLines: TStrings;
    FParams: array of THtmlDocumentTagParam;
    FTags: array of THtmlDocumentTag;
    FText: String;
    { Methoden }
    function GetParams(Index: Integer): THtmlDocumentTagParam;
    function GetParamCount: Integer;
    function GetTags(Index: Integer): THtmlDocumentTag;
    function GetTagCount: Integer;
    procedure LinesChange(Sender: TObject);
  public
    { Public-Deklarationen }
    constructor Create(const AName: String; ADocument: THtmlDocument; AParent: THtmlDocumentTag); overload;
    constructor Create(const AName: String; ADocument: THtmlDocument; AParent: THtmlDocumentTag; ALines: TStrings); overload;
    destructor Destroy; override;
    function FindTag(const Id: Integer): THtmlDocumentTag;
    function ParamValue(const Name: String): String;
    function IsParent(Tag: THtmlDocumentTag): Boolean;
    function GetPath(const Separator: Char = '.'): String;
    { Eigenschaften }
    property Name: String read FName;
    property Index: Integer read FIndex;
    property Id: Integer read FId;
    property Document: THtmlDocument read FDocument;
    property Parent: THtmlDocumentTag read FParent;
    property Lines: TStrings read FLines;
    property Params[Index: Integer]: THtmlDocumentTagParam read GetParams;
    property ParamCount: Integer read GetParamCount;
    property Tags[Index: Integer]: THtmlDocumentTag read GetTags;
    property TagCount: Integer read GetTagCount;
    property Text: String read FText;
  end;

  THtmlDocument = class(TPersistent)
  private
    { Private-Deklarationen }
    FLines: TStrings;
    FTags: array of THtmlDocumentTag;
    FText: String;
    FNextId: Integer;
    { Ereignisse }
    FTagAddEvent: THtmlDocumentTagAddEvent;
    FParseEvent: THtmlDocumentParseEvent;
    FParseSuccessEvent: THtmlDocumentParseSuccessEvent;
    FParseFailEvent: THtmlDocumentParseFailEvent;
    { Methoden }
    procedure SetLines(Value: TStrings);
    function GetTags(Index: Integer): THtmlDocumentTag;
    function GetTagCount: Integer;
    function GetNextId: Integer;
    procedure LinesChange(Sender: TObject);
  protected
    { Protected-Deklarationen }
    property NextId: Integer read GetNextId;
  public
    { Public-Deklarationen }
    constructor Create; overload;
    constructor Create(ALines: TStrings); overload;
    destructor Destroy; override;
    function FindTag(const Id: Integer): THtmlDocumentTag;
    { Ereignisse }
    property OnTagAdd: THtmlDocumentTagAddEvent read FTagAddEvent write FTagAddEvent;
    property OnParse: THtmlDocumentParseEvent read FParseEvent write FParseEvent;
    property OnParseSuccess: THtmlDocumentParseSuccessEvent read FParseSuccessEvent write FParseSuccessEvent;
    property OnParseFail: THtmlDocumentParseFailEvent read FParseFailEvent write FParseFailEvent;
    { Eigenschaften }
    property Lines: TStrings read FLines write SetLines;
    property Tags[Index: Integer]: THtmlDocumentTag read GetTags;
    property TagCount: Integer read GetTagCount;
    property Text: String read FText;
  end;

  TCssDocumentRuleProperty = record
    Name: String;
    Value: String;
  end;

  TCssDocument = class;

  TCssDocumentRule = class
  private
    { Private-Deklarationen }
    FName: String;
    FIndex: Integer;
    FDocument: TCssDocument;
    FLines: TStrings;
    FProperties: array of TCssDocumentRuleProperty;
    FKind: TCssDocumentRuleKind;
    { Methoden }
    function GetProperties(Index: Integer): TCssDocumentRuleProperty;
    function GetPropertyCount: Integer;
    procedure LinesChange(Sender: TObject);
  public
    { Public-Deklarationen }
    constructor Create(const AName: String; ADocument: TCssDocument); overload;
    constructor Create(const AName: String; ADocument: TCssDocument; ALines: TStrings); overload;
    destructor Destroy; override;
    function PropertyValue(const Name: String): String;
    { Eigenschaften }
    property Name: String read FName;
    property Index: Integer read FIndex;
    property Document: TCssDocument read FDocument;
    property Lines: TStrings read FLines;
    property Properties[Index: Integer]: TCssDocumentRuleProperty read GetProperties;
    property PropertyCount: Integer read GetPropertyCount;
    property Kind: TCssDocumentRuleKind read FKind default cssNone;
  end;

  TCssDocument = class(TPersistent)
  private
    { Private-Deklarationen }
    FLines: TStrings;
    FRules: array of TCssDocumentRule;
    { Methoden }
    procedure SetLines(Value: TStrings);
    function GetRules(Index: Integer): TCssDocumentRule;
    function GetRuleCount: Integer;
    procedure LinesChange(Sender: TObject);
  public
    { Public-Deklarationen }
    constructor Create; overload;
    constructor Create(ALines: TStrings); overload;
    destructor Destroy; override;
    function FindRule(const Name: String): TCssDocumentRule;
    { Eigenschaften }
    property Lines: TStrings read FLines write SetLines;
    property Rules[Index: Integer]: TCssDocumentRule read GetRules;
    property RuleCount: Integer read GetRuleCount;
  end;

  function ValidProtocol(const Protocol: String; const Protocols: array of String): Boolean;
  function StrIsURL(const S: String): Boolean;
  function GetTagParamValue(const S,Tag,Param: String): String;
  function PxToPercent(const Px, Width: Integer): Integer;
  function PercentToPx(const Percent, Width: Integer): Integer;
  function ColorToHtml(const Color: TColor; ColorNames: Boolean): String;
  function HtmlToColor(const Html: String): TColor;
  function HtmlColorName(const Value: String): String;
  function HtmlColorValue(const Name: String): String;

  {$IFDEF ADD_COMPONENTREG}
    procedure Register;
  {$ENDIF}

const
  { Web-Protokolle }
  WP_HTTP = 'http://';
  WP_HTTPS = 'https://';
  WP_FTP = 'ftp://';
  WP_CALL = 'callto:';
  WP_MAIL = 'mailto:';
  WebProtocols: array [0..4] of String = (WP_HTTP,WP_HTTPS,WP_FTP,WP_CALL,WP_MAIL);
  WebProtocolsSimple: array [0..1] of String = (WP_MAIL,WP_CALL);
  WebProtocolsSpecial: array [0..2] of String = (WP_HTTP,WP_HTTPS,WP_FTP);
  HtmlColorNames: array [0..139] of TStringPair = (
    ('#F0F8FF','aliceblue'           ),('#FAEBD7','antiquewhite'   ),('#00FFFF','aqua'           ),
    ('#7FFFD4','aquamarine'          ),('#F0FFFF','azure'          ),('#F5F5DC','beige'          ),
    ('#FFE4C4','bisque'              ),('#000000','black'          ),('#FFFFCD','blanchedalmond' ),
    ('#0000FF','blue'                ),('#8A2BE2','blueviolet'     ),('#A52A2A','brown'          ),
    ('#DEB887','burlywood'           ),('#5F9EA0','cadetblue'      ),('#7FFF00','chartreuse'     ),
    ('#D2691E','chocolate'           ),('#FF7F50','coral'          ),('#6495ED','cornflowerblue' ),
    ('#FFF8DC','cornsilk'            ),('#DC143C','crimson'        ),('#00FFFF','cyan'           ),
    ('#00008B','darkblue'            ),('#008B8B','darkcyan'       ),('#B8860B','darkgoldenrod'  ),
    ('#A9A9A9','darkgray'            ),('#006400','darkgreen'      ),('#BDB76B','darkkhaki'      ),
    ('#8B008B','darkmagenta'         ),('#556B2F','darkolivegreen' ),('#FF8C00','darkorange'     ),
    ('#9932CC','darkorchid'          ),('#8B0000','darkred'        ),('#E9967A','darksalmon'     ),
    ('#8FBC8F','darkseagreen'        ),('#483D8B','darkslateblue'  ),('#2F4F4F','darkslategray'  ),
    ('#00CED1','darkturquoise'       ),('#9400D3','darkviolet'     ),('#FF1493','deeppink'       ),
    ('#00BFFF','deepskyblue'         ),('#696969','dimgray'        ),('#1E90FF','dodgerblue'     ),
    ('#B22222','firebrick'           ),('#FFFAF0','floralwhite'    ),('#228B22','forestgreen'    ),
    ('#FF00FF','fuchsia'             ),('#DCDCDC','gainsboro'      ),('#F8F8FF','ghostwhite'     ),
    ('#FFD700','gold'                ),('#DAA520','goldenrod'      ),('#808080','gray'           ),
    ('#008000','green'               ),('#ADFF2F','greenyellow'    ),('#F0FFF0','honeydew'       ),
    ('#FF69B4','hotpink'             ),('#CD5C5C','indianred'      ),('#4B0082','indigo'         ),
    ('#FFF0F0','ivory'               ),('#F0E68C','khaki'          ),('#E6E6FA','lavender'       ),
    ('#FFF0F5','lavenderblush'       ),('#7CFC00','lawngreen'      ),('#FFFACD','lemonchiffon'   ),
    ('#ADD8E6','lightblue'           ),('#F08080','lightcoral'     ),('#E0FFFF','lightcyan'      ),
    ('#FAFAD2','lightgoldenrodyellow'),('#90EE90','lightgreen'     ),('#D3D3D3','lightgrey'      ),
    ('#FFB6C1','lightpink'           ),('#FFA07A','lightsalmon'    ),('#20B2AA','lightseagreen'  ),
    ('#87CEFA','lightskyblue'        ),('#778899','lightslategray' ),('#B0C4DE','lightsteelblue' ),
    ('#FFFFE0','lightyellow'         ),('#00FF00','lime'           ),('#32CD32','limegreen'      ),
    ('#FAF0E6','linen'               ),('#FF00FF','magenta'        ),('#800000','maroon'         ),
    ('#66CDAA','mediumaquamarine'    ),('#0000CD','mediumblue'     ),('#BA55D3','mediumorchid'   ),
    ('#9370DB','mediumpurple'        ),('#3CB371','mediumseagreen' ),('#7B68EE','mediumslateblue'),
    ('#00FA9A','mediumspringgreen'   ),('#48D1CC','mediumturquoise'),('#C71585','mediumvioletred'),
    ('#191970','midnightblue'        ),('#F5FFFA','mintcream'      ),('#FFE4E1','mistyrose'      ),
    ('#FFE4B5','moccasin'            ),('#FFDEAD','navajowhite'    ),('#000080','navy'           ),
    ('#FDF5E6','oldlace'             ),('#808000','olive'          ),('#6B8E23','olivedrab'      ),
    ('#FFA500','orange'              ),('#FF4500','orangered'      ),('#DA70D6','orchid'         ),
    ('#EEE8AA','palegoldenrod'       ),('#98FB98','palegreen'      ),('#AFEEEE','paleturquoise'  ),
    ('#DB7093','palevioletred'       ),('#FFEFD5','papayawhip'     ),('#FFDBBD','peachpuff'      ),
    ('#CD853F','peru'                ),('#FFC0CB','pink'           ),('#DDA0DD','plum'           ),
    ('#B0E0E6','powderblue'          ),('#800080','purple'         ),('#FF0000','red'            ),
    ('#BC8F8F','rosybrown'           ),('#4169E1','royalblue'      ),('#8B4513','saddlebrown'    ),
    ('#FA8072','salmon'              ),('#F4A460','sandybrown'     ),('#2E8B57','seagreen'       ),
    ('#FFF5EE','seashell'            ),('#A0522D','sienna'         ),('#C0C0C0','silver'         ),
    ('#87CEEB','skyblue'             ),('#6A5ACD','slateblue'      ),('#708090','slategray'      ),
    ('#FFFAFA','snow'                ),('#00FF7F','springgreen'    ),('#4682B4','steelblue'      ),
    ('#D2B48C','tan'                 ),('#008080','teal'           ),('#D8BFD8','thistle'        ),
    ('#FD6347','tomato'              ),('#40E0D0','turquoise'      ),('#EE82EE','violet'         ),
    ('#F5DEB3','wheat'               ),('#FFFFFF','white'          ),('#F5F5F5','whitesmoke'     ),
    ('#FFFF00','yellow'              ),('#9ACD32','yellowgreen'    )
  );

implementation

{$IFDEF ADD_COMPONENTREG}
  procedure Register;
  begin
    RegisterComponents({$IFDEF ADD_SINGLECATEGORY}ComponentsPage{$ELSE}ComponentsPage_Web{$ENDIF},[{$IFDEF ADD_DEPENDENCIES}TDownload{$ENDIF}]);
  end;
{$ENDIF}

function ValidProtocol(const Protocol: String;
  const Protocols: array of String): Boolean;
var
  Index: 1..5;
begin
  Result := (ArrayPos(Protocol,Protocols) <> -1);
end;

function StrIsURL(const S: String): Boolean;
{ Ein einfacher (iterativer) Parser, der überprüfen soll, ob ein String die
  Anforderungen an eine URL-Internetadresse erfüllt.
  Hier sei gesagt, dass dies KEINE vollständige Prüfung ist, sondern nur die
  wichtigsten Kriterien beinhaltet.
  Außerdem wird auch nicht überprüft, ob eine Adresse verfügbar ist. }
const
  //Diese Zeichen sind zwar erlaubt, dürfen jedoch nicht doppelt hintereinander vorkommen
  InvalidDoubleChars = [':','.'];
var
  Index: Integer;
  DomainLength: Integer;
  Protocol: String;
  ProtocolValid: Boolean;
  DoubleSlashRequired: Boolean;
begin
  Result := True;
  ProtocolValid := False;
  DoubleSlashRequired := False;
  DomainLength := 0;
  Protocol := '';
  for Index := 1 to Length(S) do
  begin
    if (S[Index] in InvalidDoubleChars) and
       (S[Index] = S[Index - 1]) then
    begin
      Result := False;
      Exit;
    end;
    if (S[Index] in Spaces) or
       ((S[Index] = ':') and ProtocolValid) then
    begin
      Result := False;
      Exit;
    end;
    if not ProtocolValid then
    begin
      Protocol := Protocol + S[Index];
      if S[Index] = ':' then
      begin
        ProtocolValid := True;
        if not ValidProtocol(Protocol,WebProtocolsSimple) then
        begin
          DoubleSlashRequired := True;
          Continue;
        end;
      end;
      if S[Index] = '/' then
      begin
        Result := False;
        Exit;
      end;
    end else
    begin
      if S[Index] = '/' then
      begin
        if DoubleSlashRequired then
        begin
          Protocol := Protocol + S[Index];
          DoubleSlashRequired := False;
        end else
        begin
          if S[Index - 1] = '/' then
          begin
            if not ValidProtocol(Protocol + S[Index],WebProtocolsSpecial) then
            begin
              Result := False;
              Exit;
            end else
            begin
              Protocol := Protocol + S[Index];
            end;
          end;
        end;
      end else
      begin
        if (DoubleSlashRequired) or ((S[Index - 1] = '/') and (S[Index - 2] <> '/') and (DomainLength = 0)) then
        begin
          Result := False;
          Exit;
        end else
        begin
          DomainLength := DomainLength + 1;
        end;
      end;
    end;
  end;
  Result := (ProtocolValid and (DomainLength > 3));
end;

function GetTagParamValue(const S,Tag,Param: String): String;
{ Ziemlich schneller XML/HTML-Parser, der nach einem Tag und einem dort
  enthaltenem Parameter sucht. Der Wert dieses Parameters wird dann
  als Ergebnis der Funktion zurückgegeben. }
var
  Finished: Boolean;
  Current: PChar;
  InTag,InValue: Boolean;
  Ignore: Boolean;
  Equal: Boolean;
  Block: String;
begin
  Result := '';
  if (Length(S) < 4) or (Length(Tag) = 0) or (Length(Param) = 0) then
  begin
    Exit;
  end;
  Finished := False;
  InTag := False;
  InValue := False;
  Ignore := True;
  Equal := False;
  Block := '';
  Current := PChar(S);
  while True do
  begin
    if InTag then
    begin
      if InValue then
      begin
        if Current^ = '"' then
        begin
          InValue := False;
          if not Ignore then
          begin
            Result := Block;
            Exit;
          end;
          Block := '';
          Equal := False;
        end else
        begin
          Block := Block + Current^;
        end;
        //-->
        Inc(Current);
        Continue;
      end;
      if Equal then
      begin
        if Current^ in Spaces then
        begin
          //-->
          Inc(Current);
          Continue;
        end;
        if Current^ = '"' then
        begin
          InValue := True;
          Ignore := not (Ignore and (Block = Param));
          Block := '';
          //-->
          Inc(Current);
          Continue;
        end;
      end else
      begin
        if Current^ in Letters + Numbers then
        begin
          Block := Block + Current^;
          //-->
          Inc(Current);
          Continue;
        end;
        if Current^ = '>' then
        begin
          InTag := False;
          Block := '';
          //-->
          Inc(Current);
          Continue;
        end;
        if Current^ in Spaces then
        begin
          if Block = Tag then
          begin
            Ignore := False;
          end;
          Block := '';
          //-->
          Inc(Current);
          Continue;
        end;
        if Current^ = '=' then
        begin
          Equal := True;
          //-->
          Inc(Current);
          Continue;
        end;
      end;
    end else
    begin
      if Current^ = '<' then
      begin
        InTag := True;
      end;
      //-->
      Inc(Current);
      Continue;
    end;
    raise EInvalidTagChar.Create('Invalid character: "' + Current^ + '"');
    Exit;
  end;
end;

function PxToPercent(const Px, Width: Integer): Integer;
begin
  Result := 100 * Px div Width; //Identisch mit 100 / (Width / Px), aber schneller
end;

function PercentToPx(const Percent, Width: Integer): Integer;
begin
  Result := Percent div 100 * Width;
end;

function ColorToHtml(const Color: TColor; ColorNames: Boolean): String;
begin
  //...
  if ColorNames then
  begin
    Result := HtmlColorName(Result);
  end;
end;

function HtmlToColor(const Html: String): TColor;
var
  ColorValue: String;
begin
  ColorValue := HtmlColorValue(Html);

end;

function HtmlColorName(const Value: String): String;
var
  Index: Integer;
begin
  for Index := Low(HtmlColorNames) to High(HtmlColorNames) do
  begin
    //...
  end;
  Result := Value;
end;

function HtmlColorValue(const Name: String): String;
var
  Index: Integer;
begin
  for Index := Low(HtmlColorNames) to High(HtmlColorNames) do
  begin
    //...
  end;
  Result := Name;
end;

{ ----------------------------------------------------------------------------
  TDownload
  ---------------------------------------------------------------------------- }

{$IFDEF ADD_DEPENDENCIES}
  constructor TDownload.Create(AOwner: TComponent);
  begin
    inherited;
    FAbout := TComponentAbout.Create(TDownload);
    idHTTPObject := TidHTTP.Create(Self);
    idHTTPObject.HandleRedirects := True;
    idHTTPObject.OnWork := idHTTPObjectWork;
    idHTTPObject.OnWorkBegin := idHTTPObjectWorkBegin;
    idHTTPObject.OnWorkEnd := idHTTPObjectWorkEnd;
    SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Self);
    SSL := False;
  end;

  destructor TDownload.Destroy;
  begin
    FAbout.Free;
    inherited;
  end;

  procedure TDownload.SetAddress(Value: String);
  begin
    if StrIsURL(Value) or (Length(Value) = 0) then
    begin
      FAddress := Value;
    end else
    begin
      raise EInvalidWebAddress.Create('"' + Value + '" is not a valid URL address');
    end;
  end;

  procedure TDownload.idHTTPObjectWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  begin
    FProgress := AWorkCount;
    if Assigned(OnWork) then
    begin
      OnWork(Self,AWorkMode);
    end;
  end;

  procedure TDownload.idHTTPObjectWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
  begin
    FProgressMax := AWorkCountMax;
    if Assigned(OnWorkBegin) then
    begin
      OnWorkBegin(Self,AWorkMode);
    end;
  end;

  procedure TDownload.idHTTPObjectWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
  begin
    if Assigned(OnWorkEnd) then
    begin
      OnWorkEnd(Self,AWorkMode);
    end;
  end;

  function TDownload.GetProgress: Int64;
  begin
    Result := FProgress;
  end;

  function TDownload.GetProgressMax: Int64;
  begin
    Result := FProgressMax;
  end;

  function TDownload.GetProgressPercent: Byte;
  begin
    if FProgressMax > 0 then
    begin
      Result := (FProgress div FProgressMax) * 100;
    end else
    begin
      { Bei aktivierter SSL-Verschlüsselung wird keine insgesamte Dateigröße
        übermittelt. Deshalb wird hier eine 0 angegeben, um das Ergebnis nicht
        undefiniert zu lassen. }
      Result := 0;
    end;
  end;

  function TDownload.GetText: String;
  begin
    Prepare;
    Result := idHTTPObject.Get(Address);
  end;

  procedure TDownload.SaveToFile(const FileName: TFileName);
  var
    FS: TFileStream;
  begin
    Prepare;
    FS := TFileStream.Create(FileName,fmCreate or fmShareDenyWrite);
    try
      idHTTPObject.Get(Address,FS);
    finally
      FS.Free;
    end;
  end;

  procedure TDownload.SaveToStream(var Stream: TStream);
  begin
    Prepare;
    idHTTPObject.Get(Address,Stream);
  end;

  procedure TDownload.Prepare;
  begin
    if SSL then
    begin
      idHTTPObject.IOHandler := SSLHandler;
    end else
    begin
      idHTTPObject := nil;
    end;
  end;
{$ENDIF}

{ ----------------------------------------------------------------------------
  THtmlDocumentTag
  ---------------------------------------------------------------------------- }

constructor THtmlDocumentTag.Create(const AName: String; ADocument: THtmlDocument; AParent: THtmlDocumentTag);
begin
  inherited Create;
  FName := AName;
  if AParent = nil then
  begin
    FIndex := ADocument.TagCount - 1;
  end else
  begin
    FIndex := AParent.TagCount - 1;
  end;
  FId := ADocument.NextId;
  FDocument := ADocument;
  FParent := AParent;
  FLines := TStringList.Create;
  (FLines as TStringList).OnChange := LinesChange;
end;

constructor THtmlDocumentTag.Create(const AName: String; ADocument: THtmlDocument; AParent: THtmlDocumentTag; ALines: TStrings);
begin
  Create(AName,ADocument,AParent);
  FLines.Assign(ALines);
end;

destructor THtmlDocumentTag.Destroy;
var
  Index: Integer;
begin
  for Index := 0 to TagCount - 1 do
  begin
    Tags[Index].Free;
  end;
  FLines.Free;
  inherited;
end;

function THtmlDocumentTag.FindTag(const Id: Integer): THtmlDocumentTag;
var
  Index: Integer;
begin
  for Index := 0 to TagCount - 1 do
  begin
    if Tags[Index].Id = Id then
    begin
      Result := Tags[Index];
      Exit;
    end else
    begin
      Result := Tags[Index].FindTag(Id);
      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function THtmlDocumentTag.ParamValue(const Name: String): String;
var
  Index: Integer;
begin
  for Index := 0 to ParamCount - 1 do
  begin
    if Params[Index].Name = Name then
    begin
      //Result := Params[Index];
      Exit;
    end;
  end;
  Result := '';
end;

function THtmlDocumentTag.IsParent(Tag: THtmlDocumentTag): Boolean;
var
  Current: THtmlDocumentTag;
begin
  Current := Parent;
  while Current <> nil do
  begin
    if Current = Tag then
    begin
      Result := True;
      Exit;
    end;
    Current := Current.Parent;
  end;
  Result := False;
end;

function THtmlDocumentTag.GetPath(const Separator: Char = '.'): String;
var
  Current: THtmlDocumentTag;
begin
  Result := Name;
  Current := Parent;
  while Current <> nil do
  begin
    Result := Current.Name + Separator + Result;
    Current := Current.Parent;
  end;
end;

function THtmlDocumentTag.GetParams(Index: Integer): THtmlDocumentTagParam;
begin
  Result := FParams[Index];
end;

function THtmlDocumentTag.GetParamCount: Integer;
begin
  Result := Length(FParams);
end;

function THtmlDocumentTag.GetTags(Index: Integer): THtmlDocumentTag;
begin
  Result := FTags[Index];
end;

function THtmlDocumentTag.GetTagCount: Integer;
begin
  Result := Length(FTags);
end;

procedure THtmlDocumentTag.LinesChange(Sender: TObject);
var
  Index: Integer;
  Current: PChar;
  CurrentTag: String;
  TagName: String;
  TagText: String;
  TagLines: TStrings;
  InTag, InTagArea: Boolean;
  TagKind: (tagOpen,tagClose,tagSingle,tagComment);
begin
  for Index := 0 to TagCount - 1 do
  begin
    Tags[Index].Free;
  end;
  SetLength(FTags,0);
  FText := '';
  TagName := '';
  TagText := '';
  TagLines := TStringList.Create;
  InTag := False;
  InTagArea := False;
  Current := PChar(Lines.Text);
  try
    try
      while Current^ <> #0 do
      begin
        if (Current^ = '<') and ((Current + 1)^ in Letters + ['!','/']) then
        begin
          InTag := True;
          InTagArea := True;
          if (Current + 1)^ in Letters then
          begin
            TagKind := tagOpen;
            if Length(TagName) <> 0 then
            begin
              TagText := TagText + Current^;
            end;
            //-->
            Inc(Current);
            Continue;
          end;
          if ((Current + 1)^ = '!') and ((Current + 2)^ = '-') and ((Current + 3)^ = '-') then
          begin
            TagKind := tagComment;
            if Length(TagName) <> 0 then
            begin
              TagText := TagText + Current^ + (Current + 1)^ + (Current + 2)^ + (Current + 3)^;
            end;
            //-->
            Inc(Current,4);
            Continue;
          end;
          if ((Current + 1)^ = '/') and ((Current + 2)^ in Letters) then
          begin
            TagKind := tagClose;
            //-->
            Inc(Current,2);
            Continue;
          end;
        end;
        if InTagArea then
        begin
          if InTag then
          begin
            case TagKind of
              tagSingle,
              tagOpen: if (Current^ = '>') or ((Current^ = '/') and (Length(Trim(CurrentTag)) <> 0)) then
                       begin
                         if Length(CurrentTag) <> 0 then
                         begin
                           InTag := False;
                           if Length(TagName) = 0 then
                           begin
                             TagName := CurrentTag;
                           end else
                           begin
                             TagText := TagText + Current^;
                           end;
                           CurrentTag := '';
                           //Parse spaces and parameters .... (to be added)
                           if Current^ = '/' then
                           begin
                             TagKind := tagSingle;
                             if Length(TagText) <> 0 then
                             begin
                               TagText := TagText + Current^;
                             end;
                           end;
                           //-->
                           Inc(Current);
                           Continue;
                         end;
                       end else
                       begin
                         CurrentTag := CurrentTag + Current^;
                         if Length(TagName) <> 0 then
                         begin
                           TagText := TagText + Current^;
                         end;
                         //-->
                         Inc(Current);
                         Continue;
                       end;
              tagClose: if Current^ = '>' then
                        begin
                          if (Length(CurrentTag) <> 0) and (Length(TagName) <> 0) then
                          begin
                            InTag := False;
                            if TrimRight(CurrentTag) = TagName then
                            begin
                              InTagArea := False;
                              SetLength(FTags,Length(FTags) + 1);
                              TagLines.Text := TagText;
                              FTags[TagCount - 1] := THtmlDocumentTag.Create(TagName,Document,Self,TagLines);
                              if Assigned(Document.OnTagAdd) then
                              begin
                                Document.OnTagAdd(Self,FTags[TagCount - 1]);
                              end;
                              TagLines.Clear;
                              TagName := '';
                              TagText := '';
                            end else
                            begin
                              TagText := TagText + '</' + CurrentTag + '>';
                            end;
                            CurrentTag := '';
                            //-->
                            Inc(Current);
                            Continue;
                          end;
                        end else
                        begin
                          CurrentTag := CurrentTag + Current^;
                          //-->
                          Inc(Current);
                          Continue;
                        end;
              tagComment: if (Current^ = '-') and ((Current + 1)^ = '-') and ((Current + 2)^ = '>') then
                          begin
                            InTag := False;
                            TagText := TagText + Current^ + (Current + 1)^ + (Current + 2)^;
                            //-->
                            Inc(Current,3);
                            Continue;
                          end else
                          begin
                            TagText := TagText + Current^;
                            //-->
                            Inc(Current);
                            Continue;
                          end;
            end;
          end else
          begin
            TagText := TagText + Current^;
            //-->
            Inc(Current);
            Continue;
          end;
        end else
        begin
          FText := FText + Current^;
          //-->
          Inc(Current);
          Continue;
        end;
        raise EHtmlParse.Create('HTML parse error on line ' + IntToStr(CharLine(Current,Lines.Text)) + ' at position ' + IntToStr(CharPosition(Current,Lines.Text)));
      end;
      if InTagArea then
      begin
        SetLength(FTags,Length(FTags) + 1);
        TagLines.Text := TagText;
        FTags[TagCount - 1] := THtmlDocumentTag.Create(TagName,Document,Self,TagLines);
        if Assigned(Document.OnTagAdd) then
        begin
          Document.OnTagAdd(Self,FTags[TagCount - 1]);
        end;
      end;
    finally
      TagLines.Free;
    end;
  except
    raise;
  end;
end;

{ ----------------------------------------------------------------------------
  THtmlDocument
  ---------------------------------------------------------------------------- }

constructor THtmlDocument.Create;
begin
  inherited;
  FNextId := 0;
  FLines := TStringList.Create;
  (FLines as TStringList).OnChange := LinesChange;
end;

constructor THtmlDocument.Create(ALines: TStrings);
begin
  Create;
  Lines := ALines;
end;

destructor THtmlDocument.Destroy;
begin
  FLines.Free;
  inherited;
end;

function THtmlDocument.FindTag(const Id: Integer): THtmlDocumentTag;
var
  Index: Integer;
begin
  for Index := 0 to TagCount - 1 do
  begin
    if Tags[Index].Id = Id then
    begin
      Result := Tags[Index];
      Exit;
    end else
    begin
      Result := Tags[Index].FindTag(Id);
      if Result <> nil then
      begin
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

procedure THtmlDocument.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
end;

function THtmlDocument.GetTags(Index: Integer): THtmlDocumentTag;
begin
  Result := FTags[Index];
end;

function THtmlDocument.GetTagCount: Integer;
begin
  Result := Length(FTags);
end;

function THtmlDocument.GetNextId: Integer;
begin
  Result := FNextId;
  Inc(FNextId);
end;

procedure THtmlDocument.LinesChange(Sender: TObject);
var
  Index: Integer;
  Current: PChar;
  CurrentTag: String;
  TagName: String;
  TagText: String;
  TagLines: TStrings;
  InTag, InTagArea: Boolean;
  TagKind: (tagOpen,tagClose,tagSingle,tagComment,tagSpecial);
begin
  if Assigned(OnParse) then
  begin
    OnParse(Self);
  end;
  for Index := 0 to TagCount - 1 do
  begin
    Tags[Index].Free;
  end;
  SetLength(FTags,0);
  FText := '';
  TagName := '';
  TagText := '';
  TagLines := TStringList.Create;
  InTag := False;
  InTagArea := False;
  Current := PChar(Lines.Text);
  try
    try
      while Current^ <> #0 do
      begin
        if (Current^ = '<') and ((Current + 1)^ in Letters + ['!','/']) then
        begin
          InTag := True;
          InTagArea := True;
          if (Current + 1)^ in Letters then
          begin
            TagKind := tagOpen;
            if Length(TagName) <> 0 then
            begin
              TagText := TagText + Current^;
            end;
            //-->
            Inc(Current);
            Continue;
          end;
          if ((Current + 1)^ = '!') and ((Current + 2)^ in Letters + ['-']) then
          begin
            if ((Current + 2)^ = '-') and ((Current + 3)^ = '-') then
            begin
              TagKind := tagComment;
              if Length(TagName) <> 0 then
              begin
                TagText := TagText + Current^ + (Current + 1)^;
              end;
              Inc(Current,2);
            end else
            begin
              TagKind := tagSpecial;
            end;
            if Length(TagName) <> 0 then
            begin
              TagText := TagText + Current^ + (Current + 1)^;
            end;
            //-->
            Inc(Current,2);
            Continue;
          end;
          if ((Current + 1)^ = '/') and ((Current + 2)^ in Letters) then
          begin
            TagKind := tagClose;
            //-->
            Inc(Current,2);
            Continue;
          end;
        end;
        if InTagArea then
        begin
          if InTag then
          begin
            case TagKind of
              tagSingle,
              tagOpen: if (Current^ = '>') or ((Current^ = '/') and (Length(Trim(CurrentTag)) <> 0)) then
                       begin
                         if Length(CurrentTag) <> 0 then
                         begin
                           InTag := False;
                           if Length(TagName) = 0 then
                           begin
                             TagName := CurrentTag;
                           end else
                           begin
                             TagText := TagText + Current^;
                           end;
                           CurrentTag := '';
                           //Parse spaces and parameters .... (to be added)
                           if Current^ = '/' then
                           begin
                             TagKind := tagSingle;
                             if Length(TagText) <> 0 then
                             begin
                               TagText := TagText + Current^;
                             end;
                           end;
                           //-->
                           Inc(Current);
                           Continue;
                         end;
                       end else
                       begin
                         CurrentTag := CurrentTag + Current^;
                         if Length(TagName) <> 0 then
                         begin
                           TagText := TagText + Current^;
                         end;
                         //-->
                         Inc(Current);
                         Continue;
                       end;
              tagClose: if Current^ = '>' then
                        begin
                          if (Length(CurrentTag) <> 0) and (Length(TagName) <> 0) then
                          begin
                            InTag := False;
                            if TrimRight(CurrentTag) = TagName then
                            begin
                              InTagArea := False;
                              SetLength(FTags,Length(FTags) + 1);
                              TagLines.Text := TagText;
                              FTags[TagCount - 1] := THtmlDocumentTag.Create(TagName,Self,nil,TagLines);
                              if Assigned(OnTagAdd) then
                              begin
                                OnTagAdd(Self,FTags[TagCount - 1]);
                              end;
                              TagLines.Clear;
                              TagName := '';
                              TagText := '';
                            end else
                            begin
                              TagText := TagText + '</' + CurrentTag + '>';
                            end;
                            CurrentTag := '';
                            //-->
                            Inc(Current);
                            Continue;
                          end;
                        end else
                        begin
                          CurrentTag := CurrentTag + Current^;
                          //-->
                          Inc(Current);
                          Continue;
                        end;
              tagComment: if (Current^ = '-') and ((Current + 1)^ = '-') and ((Current + 2)^ = '>') then
                          begin
                            InTag := False;
                            TagText := TagText + Current^ + (Current + 1)^ + (Current + 2)^;
                            //-->
                            Inc(Current,3);
                            Continue;
                          end else
                          begin
                            TagText := TagText + Current^;
                            //-->
                            Inc(Current);
                            Continue;
                          end;
              tagSpecial: if (Length(Text) = 0) and (TagCount = 0) then
                          begin
                            if Current^ = '>' then
                            begin
                              if Length(CurrentTag) <> 0 then
                              begin
                                TagText := TagText + Current^;
                              end;
                            end else
                            begin
                              CurrentTag := CurrentTag + Current^;
                              TagText := TagText + Current^;
                              //-->
                              Inc(Current);
                              Continue;
                            end;
                          end;
            end;
          end else
          begin
            TagText := TagText + Current^;
            //-->
            Inc(Current);
            Continue;
          end;
        end else
        begin
          FText := FText + Current^;
          //-->
          Inc(Current);
          Continue;
        end;
        raise EHtmlParse.Create('HTML parse error on line ' + IntToStr(CharLine(Current,Lines.Text) + 1) + ' at position ' + IntToStr(CharPosition(Current,Lines.Text) + 1));
      end;
      if InTagArea then
      begin
        SetLength(FTags,Length(FTags) + 1);
        TagLines.Text := TagText;
        FTags[TagCount - 1] := THtmlDocumentTag.Create(TagName,Self,nil,TagLines);
        if Assigned(OnTagAdd) then
        begin
          OnTagAdd(Self,FTags[TagCount - 1]);
        end;
      end;
      if Assigned(OnParseSuccess) then
      begin
        OnParseSuccess(Self);
      end;
    finally
      TagLines.Free;
    end;
  except
    if Assigned(OnParseFail) then
    begin
      OnParseFail(Self);
    end else
    begin
      raise;
    end;
  end;
end;

{ ----------------------------------------------------------------------------
  TCssDocumentRule
  ---------------------------------------------------------------------------- }

constructor TCssDocumentRule.Create(const AName: String; ADocument: TCssDocument);
begin
  inherited Create;
  FName := AName;
  FIndex := ADocument.RuleCount - 1;
  FDocument := ADocument;
  FLines := TStringList.Create;
  (FLines as TStringList).OnChange := LinesChange;
  FKind := cssNone;
end;

constructor TCssDocumentRule.Create(const AName: String; ADocument: TCssDocument; ALines: TStrings);
begin
  Create(AName,ADocument);
  FLines.Assign(ALines);
end;

destructor TCssDocumentRule.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TCssDocumentRule.PropertyValue(const Name: String): String;
var
  Index: Integer;
begin
  for Index := 0 to PropertyCount - 1 do
  begin
    if Properties[Index].Name = Name then
    begin
      //Result := Properties[Index];
      Exit;
    end;
  end;
  Result := '';
end;

function TCssDocumentRule.GetProperties(Index: Integer): TCssDocumentRuleProperty;
begin
  Result := FProperties[Index];
end;

function TCssDocumentRule.GetPropertyCount: Integer;
begin
  Result := Length(FProperties);
end;

procedure TCssDocumentRule.LinesChange(Sender: TObject);
begin

end;

{ ----------------------------------------------------------------------------
  TCssDocument
  ---------------------------------------------------------------------------- }

constructor TCssDocument.Create;
begin
  inherited;
  FLines := TStringList.Create;
  (FLines as TStringList).OnChange := LinesChange;
end;

constructor TCssDocument.Create(ALines: TStrings);
begin
  Create;
  Lines := ALines;
end;

destructor TCssDocument.Destroy;
begin
  FLines.Free;
  inherited;
end;

function TCssDocument.FindRule(const Name: String): TCssDocumentRule;
var
  Index: Integer;
begin
  for Index := 0 to RuleCount - 1 do
  begin
    if Rules[Index].Name = Name then
    begin
      Result := Rules[Index];
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TCssDocument.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
end;

function TCssDocument.GetRules(Index: Integer): TCssDocumentRule;
begin
  Result := FRules[Index];
end;

function TCssDocument.GetRuleCount: Integer;
begin
  Result := Length(FRules);
end;

procedure TCssDocument.LinesChange(Sender: TObject);
var
  Current: PChar;
begin
  Current := PChar(Lines.Text);
end;

end.
