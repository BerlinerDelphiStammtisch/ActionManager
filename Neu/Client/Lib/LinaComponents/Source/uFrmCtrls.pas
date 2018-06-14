unit uFrmCtrls;

//////////////////////////////////////
///  Lina Form Controls Unit       ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

uses
  { Standard-Units }
  SysUtils, Classes, Controls, Windows, Forms, StdCtrls, ComCtrls, ExtCtrls,
  Graphics, Dialogs, Messages,
  { Andere Package-Units }
  uBase, uSysTools, uSysCtrls, uWebCtrls;

type
  { Fehlermeldungen }
  EInvalidParamChar = class(Exception);
  EInvalidParamIdentifier = class(Exception);
  EInvalidParamFormat = class(Exception);
  EInvalidDiagramValueCollection = class(Exception);
  EInvalidDiagramGap = class(Exception);

  { Hilfsklassen }
  TSplashScreenMode = (ssmDefault,ssmModal);
  TSplashScreenAnimation = (ssaNone,ssaShallow);
  TProgressBarManagerMode = (pmmNone,pmmBattery{$IFDEF ADD_DEPENDENCIES},pmmDownload{$ENDIF});
  TListBoxManagerMode = (lmmNone,lmmEdit,lmmComboBox);
  TDiagramLayout = (dloColumns,dloPoints,dloStraightLines,dloCurvedLines,dloCustom);
  TDiagramGridLines = (dglHorizontal,dglVertical,dglBoth);
  TDiagramTrendLineMethod = (dtlFirstLast,dtlAvgAll,dtlAvgLeftRight);

  { Ereignisse }
  TSplashScreenCreateEvent = procedure(Sender: TObject) of object;
  TSplashScreenDestroyEvent = procedure(Sender: TObject) of object;
  TSplashScreenShowEvent = procedure(Sender: TObject) of object;
  TSplashScreenHideEvent = procedure(Sender: TObject) of object;
  TSplashScreenChangeEvent = procedure(Sender: TObject) of object;
  TSplashScreenTimerEvent = procedure(Sender: TObject) of object;
  TComponentManagerUpdateEvent = procedure(Sender: TObject) of object;
  TParamDefinerUpdateEvent = procedure(Sender: TObject) of object;
  TDiagramDrawValueEvent = procedure(Sender: TObject; Index: Integer) of object;
  TDiagramCustomDrawValueEvent = procedure(Sender: TObject; Index: Integer; Rect: TRect) of object;

  { Hauptklassen }

  { TSplash... }
  TSplashObject = class(TPersistent)
  private
    { Private-Deklarationen }
    SplashScreenVisible: Boolean;
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
  end;

  TSplashForm = class(TSplashObject)
  private
    { Private-Deklarationen }
    FormObject: TForm;
    { Methoden }
    function GetBorderStyle: TFormBorderStyle;
    procedure SetBorderStyle(Value: TFormBorderStyle);
    function GetBorderIcons: TBorderIcons;
    procedure SetBorderIcons(Value: TBorderIcons);
    function GetLeft: Integer;
    procedure SetLeft(Value: Integer);
    function GetTop: Integer;
    procedure SetTop(Value: Integer);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
    function GetHeight: Integer;
    procedure SetHeight(Value: Integer);
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign);
    function GetAlphaBlend: Boolean;
    procedure SetAlphaBlend(Value: Boolean);
    function GetAlphaBlendValue: Byte;
    procedure SetAlphaBlendValue(Value: Byte);
    function GetCaption: TCaption;
    procedure SetCaption(Value: TCaption);
    function GetColor: TColor;
    procedure SetColor(Value: TColor);
    function GetEnabled: Boolean;
    procedure SetEnabled(Value: Boolean);
    function GetWindowState: TWindowState;
    procedure SetWindowState(Value: TWindowState);
    function GetPosition: TPosition;
    procedure SetPosition(Value: TPosition);
    function GetCursor: TCursor;
    procedure SetCursor(Value: TCursor);
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property BorderStyle: TFormBorderStyle read GetBorderStyle write SetBorderStyle default bsNone;
    property BorderIcons: TBorderIcons read GetBorderIcons write SetBorderIcons default [];
    property Left: Integer read GetLeft write SetLeft default 0;
    property Top: Integer read GetTop write SetTop default 0;
    property Width: Integer read GetWidth write SetWidth default 600;
    property Height: Integer read GetHeight write SetHeight default 400;
    property Align: TAlign read GetAlign write SetAlign default alNone;
    property AlphaBlend: Boolean read GetAlphaBlend write SetAlphaBlend default False;
    property AlphaBlendValue: Byte read GetAlphaBlendValue write SetAlphaBlendValue default 255;
    property Caption: TCaption read GetCaption write SetCaption;
    property Color: TColor read GetColor write SetColor default clBtnFace;
    property Enabled: Boolean read GetEnabled write SetEnabled default True;
    property WindowState: TWindowState read GetWindowState write SetWindowState default wsNormal;
    property Position: TPosition read GetPosition write SetPosition default poScreenCenter;
    property Cursor: TCursor read GetCursor write SetCursor default crHourGlass;
  end;

  TSplashProgressBar = class(TSplashObject)
  private
    { Private-Deklarationen }
    ProgressBarObject: TProgressBar;
    { Methoden }
    function GetLeft: Integer;
    procedure SetLeft(Value: Integer);
    function GetTop: Integer;
    procedure SetTop(Value: Integer);
    function GetWidth: Integer;
    procedure SetWidth(Value: Integer);
    function GetHeight: Integer;
    procedure SetHeight(Value: Integer);
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    function GetPosition: Integer;
    procedure SetPosition(Value: Integer);
    function GetMax: Integer;
    procedure SetMax(Value: Integer);
    function GetMin: Integer;
    procedure SetMin(Value: Integer);
    function GetAlign: TAlign;
    procedure SetAlign(Value: TAlign);
    function GetBackgroundColor: TColor;
    procedure SetBackgroundColor(Value: TColor);
    function GetBarColor: TColor;
    procedure SetBarColor(Value: TColor);
    function GetState: TProgressBarState;
    procedure SetState(Value: TProgressBarState);
    function GetStyle: TProgressBarStyle;
    procedure SetStyle(Value: TProgressBarStyle);
    function GetSmooth: Boolean;
    procedure SetSmooth(Value: Boolean);
    function GetSmoothReverse: Boolean;
    procedure SetSmoothReverse(Value: Boolean);
    function GetMarqueeInterval: Integer;
    procedure SetMarqueeInterval(Value: Integer);
    function GetStep: Integer;
    procedure SetStep(Value: Integer);
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Left: Integer read GetLeft write SetLeft default 250;
    property Top: Integer read GetTop write SetTop default 250;
    property Width: Integer read GetWidth write SetWidth default 100;
    property Height: Integer read GetHeight write SetHeight default 50;
    property Visible: Boolean read GetVisible write SetVisible default True;
    property Position: Integer read GetPosition write SetPosition default 0;
    property Max: Integer read GetMax write SetMax default 100;
    property Min: Integer read GetMin write SetMin default 0;
    property Align: TAlign read GetAlign write SetAlign default alNone;
    property BackgroundColor: TColor read GetBackgroundColor write SetBackgroundColor default clDefault;
    property BarColor: TColor read GetBarColor write SetBarColor default clDefault;
    property State: TProgressBarState read GetState write SetState default pbsNormal;
    property Style: TProgressBarStyle read GetStyle write SetStyle default pbstNormal;
    property Smooth: Boolean read GetSmooth write SetSmooth default False;
    property SmoothReverse: Boolean read GetSmoothReverse write SetSmoothReverse default False;
    property MarqueeInterval: Integer read GetMarqueeInterval write SetMarqueeInterval default 10;
    property Step: Integer read GetStep write SetStep default 10;
  end;

  TSplashImage = class(TSplashObject)
  private
    { Private-Deklarationen }
    ImageObject: TImage;
    { Methoden }
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
    function GetTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);
    function GetPicture: TPicture;
    procedure SetPicture(Value: TPicture);
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Visible: Boolean read GetVisible write SetVisible default True;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property Picture: TPicture read GetPicture write SetPicture;
  end;

  TSplashTimer = class(TSplashObject)
  private
    { Private-Deklarationen }
    TimerObject: TTimer;
    FEnabled: Boolean;
    { Methoden }
    procedure SetEnabled(Value: Boolean);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
  public
    { Public-Deklarationen }
    constructor Create;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read GetInterval write SetInterval default 1000;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TSplashScreen = class(TComponent)
  private
    { Private-Deklarationen }
    AnimationTimerShow: TTimer;
    AnimationTimerHide: TTimer;
    FAbout: TComponentAbout;
    FSplashForm: TSplashForm;
    FSplashProgressBar: TSplashProgressBar;
    FSplashImage: TSplashImage;
    FSplashTimer: TSplashTimer;
    FDisplayTime: Cardinal;
    FAutoShow: Boolean;
    FMode: TSplashScreenMode;
    FAnimation: TSplashScreenAnimation;
    FAnimationSpeed: Byte;
    FVisible: Boolean;
    { Ereignisse }
    FCreateEvent: TSplashScreenCreateEvent;
    FDestroyEvent: TSplashScreenDestroyEvent;
    FShowEvent: TSplashScreenShowEvent;
    FHideEvent: TSplashScreenHideEvent;
    FChangeEvent: TSplashScreenChangeEvent;
    FTimerEvent: TSplashScreenTimerEvent;
    { Methoden }
    procedure SetVisible(Value: Boolean);
    procedure SetSplashForm(Value: TSplashForm);
    procedure SetSplashProgressBar(Value: TSplashProgressBar);
    procedure SetSplashImage(Value: TSplashImage);
    procedure SetSplashTimer(Value: TSplashTimer);
    procedure AnimationTimerShowTimer(Sender: TObject);
    procedure AnimationTimerHideTimer(Sender: TObject);
    procedure FormObjectShow(Sender: TObject);
    procedure FormObjectHide(Sender: TObject);
    procedure TimerObjectTimer(Sender: TObject);
  protected
    { Protected-Deklarationen }
    procedure Refresh;                 //Vorberaitung (benutzerdefinierte Werte)
    procedure Reset;                   //Vorbereitung (standardmäßige Werte)
  public
    { Public-Deklarationen }
    property Visible: Boolean read FVisible write SetVisible;
    constructor Create(AOwnder: TComponent); override;
    destructor Destroy; override;
    procedure ApplyChanges;            //Änderungen übernehmen
    procedure Show;                    //Anzeigen
    procedure Hide;                    //Verstecken (schließen)
  published
    { Published-Deklarationen }
    { Ereignisse }
    property OnCreate: TSplashScreenCreateEvent read FCreateEvent write FCreateEvent;
    property OnDestroy: TSplashScreenDestroyEvent read FDestroyEvent write FDestroyEvent;
    property OnShow: TSplashScreenShowEvent read FShowEvent write FShowEvent;
    property OnHide: TSplashScreenHideEvent read FHideEvent write FHideEvent;
    property OnChange: TSplashScreenChangeEvent read FChangeEvent write FChangeEvent;
    property OnTimer: TSplashScreenTimerEvent read FTimerEvent write FTimerEvent;
    { Eigenschaften }
    property About: TComponentAbout read FAbout;
    property SplashForm: TSplashForm read FSplashForm write SetSplashForm;
    property SplashProgressBar: TSplashProgressBar read FSplashProgressBar write SetSplashProgressBar;
    property SplashImage: TSplashImage read FSplashImage write SetSplashImage;
    property SplashTimer: TSplashTimer read FSplashTimer write SetSplashTimer;
    property DisplayTime: Cardinal read FDisplayTime write FDisplayTime default 2000;
    property AutoShow: Boolean read FAutoShow write FAutoShow default False;
    property Mode: TSplashScreenMode read FMode write FMode default ssmDefault;
    property Animation: TSplashScreenAnimation read FAnimation write FAnimation default ssaNone;
    property AnimationSpeed: Byte read FAnimationSpeed write FAnimationSpeed default 10;
  end;

  { T...Manager }
  TComponentManager = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    { Ereignisse }
    FUpdateEvent: TComponentManagerUpdateEvent;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; virtual;
  published
    { Published-Deklarationen }
    { Ereignisse }
    property OnUpdate: TComponentManagerUpdateEvent read FUpdateEvent write FUpdateEvent;
    { Eigenschaften }
    property About: TComponentAbout read FAbout;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TProgressBarManager = class(TComponentManager)
  private
    { Private-Deklarationen }
    FTarget: TProgressBar;
    FSourceBattery: TBattery;
    {$IFDEF ADD_DEPENDENCIES}
      FSourceDownload: TDownload;
    {$ENDIF}
    FMode: TProgressBarManagerMode;
    { Methoden }
    procedure SetTarget(Value: TProgressBar);
    procedure SetSourceBattery(Value: TBattery);
    {$IFDEF ADD_DEPENDENCIES}
      procedure SetSourceDownload(Value: TDownload);
    {$ENDIF}
    procedure SetMode(Value: TProgressBarManagerMode);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; override;
  published
    { Published-Deklarationen }
    property Target: TProgressBar read FTarget write SetTarget;
    property SourceBattery: TBattery read FSourceBattery write SetSourceBattery;
    {$IFDEF ADD_DEPENDENCIES}
      property SourceDownload: TDownload read FSourceDownload write SetSourceDownload;
    {$ENDIF}
    property Mode: TProgressBarManagerMode read FMode write SetMode default pmmNone;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TListBoxManager = class(TComponentManager)
  private
    { Private-Deklarationen }
    FilteredList: TFilteredStringList;
    FTarget: TListBox;
    FSourceEdit: TEdit;
    FSourceComboBox: TComboBox;
    FMode: TListBoxManagerMode;
    FFilterOptions: TStringFilterOptions;
    { Methoden }
    procedure SetTarget(Value: TListBox);
    procedure SetSourceEdit(Value: TEdit);
    procedure SetSourceComboBox(Value: TComboBox);
    procedure SetMode(Value: TListBoxManagerMode);
    procedure SetFilterOptions(Value: TStringFilterOptions);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update; override;
    procedure LoadList;
  published
    { Published-Deklarationen }
    property Target: TListBox read FTarget write SetTarget;
    property SourceEdit: TEdit read FSourceEdit write SetSourceEdit;
    property SourceComboBox: TComboBox read FSourceComboBox write SetSourceComboBox;
    property Mode: TListBoxManagerMode read FMode write SetMode default lmmNone;
    property FilterOptions: TStringFilterOptions read FFilterOptions write SetFilterOptions default [sfoCaseSensitive,sfoForceTrim,sfoDefaultVisible];
  end;

  { TParam... }
  TParamFormat = class(TPersistent)
  private
    { Private-Deklarationen }
    FPrefix: String;
    FSuffix: String;
    FSeparator: String;
    { Methoden }
    procedure SetPrefix(Value: String);
    procedure SetSuffix(Value: String);
    procedure SetSeparator(Value: String);
  public
    { Public-Deklarationen }
    constructor Create; overload;
    constructor Create(APrefix,ASeparator,ASuffix: String); overload;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Prefix: String read FPrefix write SetPrefix;
    property Suffix: String read FSuffix write SetSuffix;
    property Separator: String read FSeparator write SetSeparator;
  end;

  TParamReference = class(TCollectionItem)
  private
    { Private-Deklarationen }
    FDefaultValue: String;
    FIdentifier: String;
    FConnector: PString;
    FFormat: TParamFormat;
    { Methoden }
    procedure SetIdentifier(Value: String);
    function GetFormat: TParamFormat;
    procedure SetFormat(Value: TParamFormat);
  public
    { Public-Deklarationen }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Update;
    function AsText(UseDefVal: Boolean = False): String;
    { Eigenschaften }
    property Connector: PString read FConnector write FConnector;
  published
    { Published-Deklarationen }
    property DefaultValue: String read FDefaultValue write FDefaultValue;
    property Identifier: String read FIdentifier write SetIdentifier;
    property Format: TParamFormat read GetFormat write SetFormat;
  end;

  TParamReferences = class(TCollection);

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TParamDefiner = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FReferences: TParamReferences;
    { Ereignisse }
    FUpdateEvent: TParamDefinerUpdateEvent;
    { Methoden }
    procedure SetReferences(Value: TParamReferences);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update;
  published
    { Published-Deklarationen }
    { Ereignisse }
    property OnUpdate: TParamDefinerUpdateEvent read FUpdateEvent write FUpdateEvent;
    { Eigenschaften }
    property About: TComponentAbout read FAbout;
    property References: TParamReferences read FReferences write SetReferences;
  end;

  TDiagram = class;

  TDiagramPadding = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FTop: Integer;
    FBottom: Integer;
    FLeft: Integer;
    FRight: Integer;
    FAxis: Integer;
    { Methoden }
    procedure SetTop(Value: Integer);
    procedure SetBottom(Value: Integer);
    procedure SetLeft(Value: Integer);
    procedure SetRight(Value: Integer);
    procedure SetAxis(Value: Integer);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property Top: Integer read FTop write SetTop default 0;
    property Bottom: Integer read FBottom write SetBottom default 0;
    property Left: Integer read FLeft write SetLeft default 5;
    property Right: Integer read FRight write SetRight default 5;
    property Axis: Integer read FAxis write SetAxis default 0;
  end;

  TDiagramValueData = packed record
    Name: ShortString;
    Color: TColor;
    Value: Integer;
    Visible: Boolean;
    BorderStyle: TBorderStyle;
    BorderWidth: Integer;
    BorderColor: TColor;
  end;

  TDiagramValue = class(TCollectionItem)
  private
    { Private-Deklarationen }
    FName: String;
    FColor: TColor;
    FValue: Integer;
    FVisible: Boolean;
    FBorderStyle: TBorderStyle;
    FBorderWidth: Integer;
    FBorderColor: TColor;
    { Methoden }
    function GetData: TDiagramValueData;
    procedure SetData(Value: TDiagramValueData);
    procedure SetName(Value: String);
    procedure SetColor(Value: TColor);
    procedure SetValue(Value: Integer);
    procedure SetVisible(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetBorderWidth(Value: Integer);
    procedure SetBorderColor(Value: TColor);
    function GetDisplayName: String; override;
  public
    { Public-Deklarationen }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Data: TDiagramValueData read GetData write SetData;
  published
    { Published-Deklarationen }
    property Name: String read FName write SetName;
    property Color: TColor read FColor write SetColor default clNone;
    property Value: Integer read FValue write SetValue default 0;
    property Visible: Boolean read FVIsible write SetVisible default True;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
  end;

  TDiagramValues = class(TCollection)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    function GetMinValue: Integer;
    function GetMaxValue: Integer;
    function GetMidValue: Integer;
    function GetAvgValue: Integer;
    function GetFirst: Integer;
    function GetLast: Integer;
    function GetVisibleCount: Integer;
  protected
    { Protected-Deklarationen }
    procedure Update(Item: TCollectionItem); override;
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    function AddData(Data: TDiagramValueData): TDiagramValue;
    function FindItem(const Name: String): TDiagramValue;
    procedure ImportFromFile(const FileName: String);
    procedure ExportToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    procedure SaveToFile(const FileName: String);
    property MinValue: Integer read GetMinValue;
    property MaxValue: Integer read GetMaxValue;
    property MidValue: Integer read GetMidValue;
    property AvgValue: Integer read GetAvgValue;
    property First: Integer read GetFirst;
    property Last: Integer read GetLast;
    property VisibleCount: Integer read GetVisibleCount;
  end;

  TDiagramScaleValueArtLines = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FVisible: Boolean;
    FColor: TColor;
    FWidth: Integer;
    FDotted: Boolean;
    { Methoden }
    procedure SetVisible(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetWidth(Value: Integer);
    procedure SetDotted(Value: Boolean);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property Visible: Boolean read FVisible write SetVisible default False;
    property Color: TColor read FColor write SetColor default clDkGray;
    property Width: Integer read FWidth write SetWidth default 1;
    property Dotted: Boolean read FDotted write SetDotted default True;
  end;

  TDiagramScaleCursorArtLines = class(TDiagramScaleValueArtLines)
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Dotted: Boolean read FDotted write SetDotted default False;
  end;

  TDiagramScaleArtLines = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FValues: TDiagramScaleValueArtLines;
    FCursor: TDiagramScaleCursorArtLines;
    procedure SetValues(Value: TDiagramScaleValueArtLines);
    procedure SetCursor(Value: TDiagramScaleCursorArtLines);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property Values: TDiagramScaleValueArtLines read FValues write SetValues;
    property Cursor: TDiagramScaleCursorArtLines read FCursor write SetCursor;
  end;

  TDiagramScaleBar = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FVisible: Boolean;
    FColor: TColor;
    FWidth: Integer;
    FRulerWidth: Word;
    FRulerGap: Word;
    FRulerNumbers: Boolean;
    { Methoden }
    procedure SetVisible(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetWidth(Value: Integer);
    procedure SetRulerWidth(Value: Word);
    procedure SetRulerGap(Value: Word);
    procedure SetRulerNumbers(Value: Boolean);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property Visible: Boolean read FVisible write SetVisible default True;
    property Color: TColor read FColor write SetColor default clBlack;
    property Width: Integer read FWidth write SetWidth default 2;
    property RulerWidth: Word read FRulerWidth write SetRulerWidth default 0;
    property RulerGap: Word read FRulerGap write SetRulerGap default 10;
    property RulerNumbers: Boolean read FRulerNumbers write SetRulerNumbers default False;
  end;

  TDiagramScaleGrid = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FVisible: Boolean;
    FDotted: Boolean;
    FLines: TDiagramGridLines;
    FColor: TColor;
    FWidth: Integer;
    FGap: Word;
    { Methoden }
    procedure SetVisible(Value: Boolean);
    procedure SetDotted(Value: Boolean);
    procedure SetLines(Value: TDiagramGridLines);
    procedure SetColor(Value: TColor);
    procedure SetWidth(Value: Integer);
    procedure SetGap(Value: Word);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property Visible: Boolean read FVisible write SetVisible default False;
    property Dotted: Boolean read FDotted write SetDotted default False;
    property Lines: TDiagramGridLines read FLines write SetLines default dglBoth;
    property Color: TColor read FColor write SetColor default clSilver;
    property Width: Integer read FWidth write SetWidth default 1;
    property Gap: Word read FGap write SetGap default 10;
  end;

  TDiagramScaleValues = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FVisible: Boolean;
    FFont: TFont;
    FAutoColor: Boolean;
    { Methoden }
    procedure SetVisible(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetAutoColor(Value: Boolean);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property Visible: Boolean read FVisible write SetVisible default False;
    property Font: TFont read FFont write SetFont;
    property AutoColor: Boolean read FAutoColor write SetAutoColor default False;
  end;

  TDiagramScale = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FArtLines: TDiagramScaleArtLines;
    FBar: TDiagramScaleBar;
    FGrid: TDiagramScaleGrid;
    FValues: TDiagramScaleValues;
    procedure SetArtLines(Value: TDiagramScaleArtlines);
    procedure SetBar(Value: TDiagramScaleBar);
    procedure SetGrid(Value: TDiagramScaleGrid);
    procedure SetValues(Value: TDiagramScaleValues);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property ArtLines: TDiagramScaleArtlines read FArtLines write SetArtLines;
    property Bar: TDiagramScaleBar read FBar write SetBar;
    property Grid: TDiagramScaleGrid read FGrid write SetGrid;
    property Values: TDiagramScaleValues read FValues write SetValues;
  end;

  TDiagramCaption = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FText: TCaption;
    FFont: TFont;
    FAlignment: TAlignment;
    FVerticalAlignment: TVerticalAlignment;
    { Methoden }
    procedure SetText(Value: TCaption);
    procedure SetFont(Value: TFont);
    procedure SetAlignment(Value: TAlignment);
    procedure SetVerticalAlignment(Value: TVerticalAlignment);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property Text: TCaption read FText write SetText;
    property Font: TFont read FFont write SetFont;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default taAlignTop;
  end;

  TDiagramAutoColors = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FEnabled: Boolean;
    FPositive: TColor;
    FZero: TColor;
    FNegative: TColor;
    { Methoden }
    procedure SetEnabled(Value: Boolean);
    procedure SetPositive(Value: TColor);
    procedure SetZero(Value: TColor);
    procedure SetNegative(Value: TColor);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Positive: TColor read FPositive write SetPositive default clBlue;
    property Zero: TColor read FZero write SetZero default clWhite;
    property Negative: TColor read FNegative write SetNegative default clRed;
  end;

  TDiagramTrendLine = class(TPersistent)
  private
    { Private-Deklarationen }
    FDiagram: TDiagram;
    FVisible: Boolean;
    FColor: TColor;
    FDotted: Boolean;
    FWidth: Integer;
    FMethod: TDiagramTrendLineMethod;
    { Methoden }
    procedure SetVisible(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetDotted(Value: Boolean);
    procedure SetWidth(Value: Integer);
    procedure SetMethod(Value: TDiagramTrendLineMethod);
  public
    { Public-Deklarationen }
    constructor Create(ADiagram: TDiagram);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published-Deklarationen }
    property Visible: Boolean read FVisible write SetVisible default False;
    property Color: TColor read FColor write SetColor default clNone;
    property Dotted: Boolean read FDotted write SetDotted default False;
    property Width: Integer read FWidth write SetWidth default 1;
    property Method: TDiagramTrendLineMethod read FMethod write SetMethod default dtlAvgLeftRight;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDiagram = class(TGraphicControl)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FCaption: TDiagramCaption;
    FLayout: TDiagramLayout;
    FValues: TDiagramValues;
    FPadding: TDiagramPadding;
    FScale: TDiagramScale;
    FAutoColors: TDiagramAutoColors;
    FTrendLine: TDiagramTrendLine;
    { Ereignisse }
    FDrawValueEvent: TDiagramDrawValueEvent;
    FCustomDrawValueEvent: TDiagramCustomDrawValueEvent;
    { Methoden }
    procedure SetCaption(Value: TDiagramCaption);
    procedure SetLayout(Value: TDiagramLayout);
    procedure SetValues(Value: TDiagramValues);
    procedure SetPadding(Value: TDiagramPadding);
    procedure SetScale(Value: TDiagramScale);
    procedure SetAutoColors(Value: TDiagramAutoColors);
    procedure SetTrendLine(Value: TDiagramTrendLine);
    function GetValueRect(Index: Integer): TRect; overload;
    function GetZeroWidth: Integer;
    function GetZeroHeight: Integer;
    function GetValueHeight(Value: Integer): Integer;
    function GetHeightValue(Height: Integer): Integer;
    function GetValueLeft(Index: Integer): Integer;
    function GetValueWidth: Integer;
    function GetValueSpace: Integer;
    function GetIndexAt(Left: Integer): Integer;
  protected
    { Protected-Deklarationen }
    procedure Paint; override;
    procedure DrawBackground; virtual;
    procedure DrawCaption; virtual;
    procedure DrawBar; virtual;
    procedure DrawGrid; virtual;
    procedure DrawValueArtLines; virtual;
    procedure DrawCursorArtLines; virtual;
    procedure DrawTrendLine; virtual;
    procedure DrawValue(Index: Integer); virtual;
    procedure DrawColumn(Index: Integer); virtual;
    procedure DrawPoint(Index: Integer); virtual;
    procedure DrawStraightLine(Index: Integer); virtual;
    procedure DrawCurvedLine(Index: Integer); virtual;
    procedure PropertyChange(Sender: TObject);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    function GetValueRect(Value: TDiagramValue): TRect; overload;
  public
    { Public-Deklarationen }
    property ValueRect[Index: Integer]: TRect read GetValueRect;
    property ZeroWidth: Integer read GetZeroWidth;
    property ZeroHeight: Integer read GetZeroHeight;
    property ValueHeight[Value: Integer]: Integer read GetValueHeight;
    property HeightValue[Height: Integer]: Integer read GetHeightValue;
    property ValueLeft[Index: Integer]: Integer read GetValueLeft;
    property ValueWidth: Integer read GetValueWidth;
    property ValueSpace: Integer read GetValueSpace;
    property IndexAt[Left: Integer]: Integer read GetIndexAt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    { Ereignisse }
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnDrawValue: TDiagramDrawValueEvent read FDrawValueEvent write FDrawValueEvent;
    property OnCustomDrawValue: TDiagramCustomDrawValueEvent read FCustomDrawValueEvent write FCustomDrawValueEvent;
    { Eigenschaften }
    property Align;
    property Width default 200;
    property Height default 50;
    property Color;
    property Caption: TDiagramCaption read FCaption write SetCaption;
    property About: TComponentAbout read FAbout;
    property Layout: TDiagramLayout read FLayout write SetLayout default dloColumns;
    property Values: TDiagramValues read FValues write SetValues;
    property Padding: TDiagramPadding read FPadding write SetPadding;
    property Scale: TDiagramScale read FScale write SetScale;
    property AutoColors: TDiagramAutoColors read FAutoColors write SetAutoColors;
    property TrendLine: TDiagramTrendLine read FTrendLine write SetTrendLine;
  end;

  { ShowMessage-Varianten }
  procedure ShowMessageVal(const Msg: Integer); overload;
  procedure ShowMessageVal(const Msg: Extended); overload;
  { String-Aus-Parameter-Methoden }
  function StringFromParam(Ident,Value: String; Format: TParamFormat): String;
  function StringFromParamRef(ParamRef: TParamReference; UseDefVal: Boolean = True): String;
  function StringFromParams(Idents,Values: TStrings; Format: TParamFormat): String;
  { Sonstige }
  procedure SetTaskBarVisibe(const Value: Boolean);
  procedure ListParams(out List: TStrings);

  {$IFDEF ADD_COMPONENTREG}
    procedure Register;
  {$ENDIF}

implementation

{$IFDEF ADD_COMPONENTREG}
  procedure Register;
  begin
    RegisterComponents({$IFDEF ADD_SINGLECATEGORY}ComponentsPage{$ELSE}ComponentsPage_Form{$ENDIF},[TSplashScreen,TProgressBarManager,TListBoxManager,TParamDefiner,TDiagram]);
  end;
{$ENDIF}

procedure ShowMessageVal(const Msg: Integer);
begin
  ShowMessage(IntToStr(Msg));
end;

procedure ShowMessageVal(const Msg: Extended);
begin
  ShowMessage(FloatToStr(Msg));
end;

function StringFromParam(Ident,Value: String; Format: TParamFormat): String;
var
  TmpStr: String;
  Index: Integer;
  RequireQuotes: Boolean;
begin
  Result := '';
  TmpStr := Format.Prefix + Ident + Format.Separator + Value + Format.Suffix;
  RequireQuotes := False;
  for Index := 1 to Length(TmpStr) do
  begin
    if TmpStr[Index] in Spaces then
    begin
      RequireQuotes := True;
    end;
    if TmpStr[Index] in ['''','"'] then
    begin
      raise EInvalidParamChar.Create('Invalid character at position: ' + IntToStr(Index));
    end;
  end;
  if RequireQuotes then
  begin
    Result := '"' + TmpStr + '"';
  end else
  begin
    Result := TmpStr;
  end;
end;

function StringFromParamRef(ParamRef: TParamReference; UseDefVal: Boolean = True): String;
var
  Value: String;
begin
  if UseDefVal then
  begin
    Value := ParamRef.DefaultValue;
  end else
  begin
    Value := ParamRef.Connector^;
  end;
  Result := StringFromParam(ParamRef.Identifier,Value,ParamRef.Format);
end;

function StringFromParams(Idents,Values: TStrings; Format: TParamFormat): String;
var
  TmpStr: String;
  Index: Integer;
begin
  Result := '';
  for Index := 0 to Idents.Count do
  begin
    if Index > 0 then
    begin
      TmpStr := TmpStr + ' ';
    end;
    TmpStr := TmpStr + StringFromParam(Idents.Strings[Index],Values[Index],Format);
  end;
  Result := TmpStr;
end;

procedure SetTaskBarVisibe(const Value: Boolean);
begin
  if Value then
  begin
    ShowWindow(Application.Handle,SW_HIDE);
    SetWindowLong(Application.Handle,GWL_EXSTYLE,GetWindowLong(Application.Handle,GWL_EXSTYLE) and not WS_EX_TOOLWINDOW or WS_EX_APPWINDOW);
    ShowWindow(Application.Handle,SW_SHOW);
  end else
  begin
    ShowWindow(Application.Handle,SW_SHOW);
    SetWindowLong(Application.Handle,GWL_EXSTYLE,GetWindowLong(Application.Handle,GWL_EXSTYLE) and not WS_EX_TOOLWINDOW or WS_EX_APPWINDOW);
    ShowWindow(Application.Handle,SW_HIDE);
  end;
end;

procedure ListParams(out List: TStrings);
var
  Index: Integer;
begin
  if Assigned(List) then
  begin
    List.Clear;
  end else
  begin
    List := TStringList.Create;
  end;
  for Index := 1 to ParamCount do
  begin
    List.Add(ParamStr(Index));
  end;
end;

{ ----------------------------------------------------------------------------
  TSplashObject
  ---------------------------------------------------------------------------- }

constructor TSplashObject.Create;
begin
  inherited;
  SplashScreenVisible := False;
end;

destructor TSplashObject.Destroy;
begin
  //...
  inherited;
end;

{ ----------------------------------------------------------------------------
  TSplashForm
  ---------------------------------------------------------------------------- }

constructor TSplashForm.Create;
begin
  FormObject := TForm.Create(nil);
  FormObject.Visible := False;
end;

destructor TSplashForm.Destroy;
begin
  FormObject.Close;
  FormObject.Free;
  inherited;
end;

function TSplashForm.GetBorderStyle: TFormBorderStyle;
begin
  Result := FormObject.BorderStyle;
end;

procedure TSplashForm.SetBorderStyle(Value: TFormBorderStyle);
begin
  FormObject.BorderStyle := Value;
end;

function TSplashForm.GetBorderIcons: TBorderIcons;
begin
  Result := FormObject.BorderIcons;
end;

procedure TSplashForm.SetBorderIcons(Value: TBorderIcons);
begin
  FormObject.BorderIcons := Value;
end;

function TSplashForm.GetLeft: Integer;
begin
  Result := FormObject.Left;
end;

procedure TSplashForm.SetLeft(Value: Integer);
begin
  FormObject.Left := Value;
end;

function TSplashForm.GetTop: Integer;
begin
  Result := FormObject.Top;
end;

procedure TSplashForm.SetTop(Value: Integer);
begin
  FormObject.Top := Value;
end;

function TSplashForm.GetWidth: Integer;
begin
  Result := FormObject.Width;
end;

procedure TSplashForm.SetWidth(Value: Integer);
begin
  FormObject.Width := Value;
end;

function TSplashForm.GetHeight: Integer;
begin
  Result := FormObject.Height;
end;

procedure TSplashForm.SetHeight(Value: Integer);
begin
  FormObject.Height := Value;
end;

function TSplashForm.GetAlign: TAlign;
begin
  Result := FormObject.Align;
end;

procedure TSplashForm.SetAlign(Value: TAlign);
begin
  FormObject.Align := Value;
end;

function TSplashForm.GetAlphaBlend: Boolean;
begin
  Result := FormObject.AlphaBlend;
end;

procedure TSplashForm.SetAlphaBlend(Value: Boolean);
begin
  FormObject.AlphaBlend := Value;
end;

function TSplashForm.GetAlphaBlendValue: Byte;
begin
  Result := FormObject.AlphaBlendValue;
end;

procedure TSplashForm.SetAlphaBlendValue(Value: Byte);
begin
  FormObject.AlphaBlendValue := Value;
end;

function TSplashForm.GetCaption: TCaption;
begin
  Result := FormObject.Caption;
end;

procedure TSplashForm.SetCaption(Value: TCaption);
begin
  FormObject.Caption := Value;
end;

function TSplashForm.GetColor: TColor;
begin
  Result := FormObject.Color;
end;

procedure TSplashForm.SetColor(Value: TColor);
begin
  FormObject.Color := Value;
end;

function TSplashForm.GetEnabled: Boolean;
begin
  Result := FormObject.Enabled;
end;

procedure TSplashForm.SetEnabled(Value: Boolean);
begin
  FormObject.Enabled := Value;
end;

function TSplashForm.GetWindowState: TWindowState;
begin
  Result := FormObject.WindowState;
end;

procedure TSplashForm.SetWindowState(Value: TWindowState);
begin
  FormObject.WindowState := Value;
end;

function TSplashForm.GetPosition: TPosition;
begin
  Result := FormObject.Position;
end;

procedure TSplashForm.SetPosition(Value: TPosition);
begin
  FormObject.Position := Value;
end;

function TSplashForm.GetCursor: TCursor;
begin
  Result := FormObject.Cursor;
end;

procedure TSplashForm.SetCursor(Value: TCursor);
begin
  FormObject.Cursor := Value;
end;

{ ----------------------------------------------------------------------------
  TSplashProgressBar
  ---------------------------------------------------------------------------- }

constructor TSplashProgressBar.Create;
begin
  ProgressBarObject := TProgressBar.Create(nil);
end;

destructor TSplashProgressBar.Destroy;
begin
  ProgressBarObject.Free;
  inherited;
end;

function TSplashProgressBar.GetLeft: Integer;
begin
  Result := ProgressBarObject.Left;
end;

procedure TSplashProgressBar.SetLeft(Value: Integer);
begin
  ProgressBarObject.Left := Value;
end;

function TSplashProgressBar.GetTop: Integer;
begin
  Result := ProgressBarObject.Top;
end;

procedure TSplashProgressBar.SetTop(Value: Integer);
begin
  ProgressBarObject.Top := Value;
end;

function TSplashProgressBar.GetWidth: Integer;
begin
  Result := ProgressBarObject.Width;
end;

procedure TSplashProgressBar.SetWidth(Value: Integer);
begin
  ProgressBarObject.Width := Value;
end;

function TSplashProgressBar.GetHeight: Integer;
begin
  Result := ProgressBarObject.Height;
end;

procedure TSplashProgressBar.SetHeight(Value: Integer);
begin
  ProgressBarObject.Height := Value;
end;

function TSplashProgressBar.GetVisible: Boolean;
begin
  Result := ProgressBarObject.Visible;
end;

procedure TSplashProgressBar.SetVisible(Value: Boolean);
begin
  ProgressBarObject.Visible := Value;
end;

function TSplashProgressBar.GetPosition: Integer;
begin
  Result := ProgressBarObject.Position;
end;

procedure TSplashProgressBar.SetPosition(Value: Integer);
begin
  ProgressBarObject.Position := Value;
end;

function TSplashProgressBar.GetMax: Integer;
begin
  Result := ProgressBarObject.Max;
end;

procedure TSplashProgressBar.SetMax(Value: Integer);
begin
  ProgressBarObject.Max := Value;
end;

function TSplashProgressBar.GetMin: Integer;
begin
  Result := ProgressBarObject.Min;
end;

procedure TSplashProgressBar.SetMin(Value: Integer);
begin
  ProgressBarObject.Min := Value;
end;

function TSplashProgressBar.GetAlign: TAlign;
begin
  Result := ProgressBarObject.Align;
end;

procedure TSplashProgressBar.SetAlign(Value: TAlign);
begin
  ProgressBarObject.Align := Value;
end;

function TSplashProgressBar.GetBackgroundColor: TColor;
begin
  Result := ProgressBarObject.BackgroundColor;
end;

procedure TSplashProgressBar.SetBackgroundColor(Value: TColor);
begin
  ProgressBarObject.BackgroundColor := Value;
end;

function TSplashProgressBar.GetBarColor: TColor;
begin
  Result := ProgressBarObject.BarColor;
end;

procedure TSplashProgressBar.SetBarColor(Value: TColor);
begin
  ProgressBarObject.BarColor := Value;
end;

function TSplashProgressBar.GetState: TProgressBarState;
begin
  Result := ProgressBarObject.State;
end;

procedure TSplashProgressBar.SetState(Value: TProgressBarState);
begin
  ProgressBarObject.State := Value;
end;

function TSplashProgressBar.GetStyle: TProgressBarStyle;
begin
  Result := ProgressBarObject.Style;
end;

procedure TSplashProgressBar.SetStyle(Value: TProgressBarStyle);
begin
  ProgressBarObject.Style := Value;
end;

function TSplashProgressBar.GetSmooth: Boolean;
begin
  Result := ProgressBarObject.Smooth;
end;

procedure TSplashProgressBar.SetSmooth(Value: Boolean);
begin
  ProgressBarObject.Smooth := Value;
end;

function TSplashProgressBar.GetSmoothReverse: Boolean;
begin
  Result := ProgressBarObject.SmoothReverse;
end;

procedure TSplashProgressBar.SetSmoothReverse(Value: Boolean);
begin
  ProgressBarObject.SmoothReverse := Value;
end;

function TSplashProgressBar.GetMarqueeInterval: Integer;
begin
  Result := ProgressBarObject.MarqueeInterval;
end;

procedure TSplashProgressBar.SetMarqueeInterval(Value: Integer);
begin
  ProgressBarObject.MarqueeInterval := Value;
end;

function TSplashProgressBar.GetStep: Integer;
begin
  Result := ProgressBarObject.Step;
end;

procedure TSplashProgressBar.SetStep(Value: Integer);
begin
  ProgressBarObject.Step := Value;
end;


{ ----------------------------------------------------------------------------
  TSplashImage
  ---------------------------------------------------------------------------- }

constructor TSplashImage.Create;
begin
  ImageObject := TImage.Create(nil);
  Picture := TPicture.Create;
  ImageObject.Align := alClient;
  ImageObject.Stretch := True;
end;

destructor TSplashImage.Destroy;
begin
  //Picture.Free;  Keine Ahrnung, wieso das hier automatisch wieder freigegeben wird (??)
  ImageObject.Free;
  inherited;
end;

function TSplashImage.GetVisible: Boolean;
begin
  Result := ImageObject.Visible;
end;

procedure TSplashImage.SetVisible(Value: Boolean);
begin
  ImageObject.Visible := Value;
end;

function TSplashImage.GetTransparent: Boolean;
begin
  Result := ImageObject.Transparent;
end;

procedure TSplashImage.SetTransparent(Value: Boolean);
begin
  ImageObject.Transparent := Value;
end;

function TSplashImage.GetPicture: TPicture;
begin
  Result := ImageObject.Picture;
end;

procedure TSplashImage.SetPicture(Value: TPicture);
begin
  ImageObject.Picture.Assign(Value);
end;

{ ----------------------------------------------------------------------------
  TSplashTimer
  ---------------------------------------------------------------------------- }

constructor TSplashTimer.Create;
begin
  TimerObject := TTimer.Create(nil);
  TimerObject.Enabled := False;
end;

destructor TSplashTimer.Destroy;
begin
  TimerObject.Enabled := False;
  TimerObject.Free;
  inherited;
end;

procedure TSplashTimer.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  if SplashScreenVisible then
  begin
    TimerObject.Enabled := Value;
  end;
end;

function TSplashTimer.GetInterval: Cardinal;
begin
  Result := TimerObject.Interval;
end;

procedure TSplashTimer.SetInterval(Value: Cardinal);
begin
  TimerObject.Interval := Value;
end;

{ ----------------------------------------------------------------------------
  TSplashScreen
  ---------------------------------------------------------------------------- }

constructor TSplashScreen.Create(AOwnder: TComponent);
begin
  inherited;
  FDisplayTime := 2000;
  FAutoShow := False;
  FMode := ssmDefault;
  FAnimation := ssaNone;
  FAnimationSpeed := 10;
  FVisible := False;

  FAbout := TComponentAbout.Create(TSplashScreen);
  FSplashForm := TSplashForm.Create;
  FSplashForm.FormObject.OnShow := FormObjectShow;
  FSplashForm.FormObject.OnHide := FormObjectHide;
  FSplashTimer := TSplashTimer.Create;
  FSplashTimer.TimerObject.OnTimer := TimerObjectTimer;
  FSplashProgressBar := TSplashProgressBar.Create;
  FSplashImage := TSplashImage.Create;
  FSplashTimer := TSplashTimer.Create;
  { Parents richtig setzen }
  SplashProgressBar.ProgressBarObject.Parent := SplashForm.FormObject;
  SplashImage.ImageObject.Parent := SplashForm.FormObject;
  { AnimationTimer... }
  AnimationTimerShow := TTimer.Create(Self);
  AnimationTimerShow.OnTimer := AnimationTimerShowTimer;
  AnimationTimerShow.Enabled := False;
  AnimationTimerShow.Interval := 50;
  AnimationTimerHide := TTimer.Create(Self);
  AnimationTimerHide.OnTimer := AnimationTimerHideTimer;
  AnimationTimerHide.Enabled := False;
  AnimationTimerHide.Interval := 50;
  Reset;
  if Assigned(OnCreate) then
  begin
    OnCreate(Self);
  end;
  { Erst NACHDEM fertig erstellt wurde und OnCreate-Event ausgeführt wurde... }
  if AutoShow then
  begin
    Show;
  end;
end;

destructor TSplashScreen.Destroy;
begin
  if Assigned(OnDestroy) then
  begin
    OnDestroy(Self);
  end;
  FAbout.Free;
  FSplashProgressBar.Free;
  FSplashImage.Free;
  FSplashTimer.Free;
  FSplashForm.Free;
  //ProgressBarObject.Free;      _
  //ImageObject.Free;             |
  //TimerObject.Free;             |____\  Automatisch
  //AnimationTimerShow.Free;      |    /  freigegeben
  //AnimationTimerHide.Free;     _|
  inherited;
end;

procedure TSplashScreen.Show;
begin
  SplashTimer.TimerObject.Enabled := SplashTimer.FEnabled;
  if Mode = ssmModal then
  begin
    SplashForm.FormObject.ShowModal;
  end else
  begin
    SplashForm.FormObject.Show;
  end;
  if Animation = ssaShallow then
  begin
    SplashForm.FormObject.AlphaBlend := True;
    SplashForm.FormObject.AlphaBlendValue := 0;
    AnimationTimerHide.Enabled := False;
    AnimationTimerShow.Enabled := True;
  end;
  FVisible := True;
  SplashForm.FormObject.BringToFront;
  SplashForm.SplashScreenVisible := True;
  SplashProgressBar.SplashScreenVisible := True;
  SplashImage.SplashScreenVisible := True;
  SplashTimer.SplashScreenVisible := True;
  if Assigned(OnShow) then
  begin
    OnShow(Self);
  end;
end;

procedure TSplashScreen.Hide;
begin
  SplashTimer.TimerObject.Enabled := False;
  if Animation = ssaNone then
  begin
    SplashForm.FormObject.Hide;
  end else
  begin
    if Animation = ssaShallow then
    begin
      SplashForm.FormObject.AlphaBlend := True;
      AnimationTimerShow.Enabled := False;
      AnimationTimerHide.Enabled := True;
    end;
  end;
  FVisible := False;
  SplashForm.SplashScreenVisible := False;
  SplashProgressBar.SplashScreenVisible := False;
  SplashImage.SplashScreenVisible := False;
  SplashTimer.SplashScreenVisible := False;
  if Assigned(OnHide) then
  begin
    OnHide(Self);
  end;
end;

procedure TSplashScreen.FormObjectShow(Sender: TObject);
begin
  Visible := True;
end;

procedure TSplashScreen.FormObjectHide(Sender: TObject);
begin
  Visible := False;
end;

procedure TSplashScreen.TimerObjectTimer(Sender: TObject);
begin
  if Assigned(OnTimer) then
  begin
    OnTimer(Self);
  end;
end;

procedure TSplashScreen.ApplyChanges;
begin
  { "Refresh;" ist nicht public, deswegen hier eine Referenz.
    Falls beim übernehmen der Änderungen noch sonstige Dinge getan werden
    müssen, können diese hier definiert werden. }
  Refresh;
end;

procedure TSplashScreen.Refresh;
begin
  { TSplashForm -> TForm }
  SplashForm.FormObject.BorderStyle := SplashForm.BorderStyle;
  SplashForm.FormObject.BorderIcons := SplashForm.BorderIcons;
  SplashForm.FormObject.Left := SplashForm.Left;
  SplashForm.FormObject.Top := SplashForm.Top;
  SplashForm.FormObject.Width := SplashForm.Width;
  SplashForm.FormObject.Height := SplashForm.Height;
  SplashForm.FormObject.Align := SplashForm.Align;
  SplashForm.FormObject.AlphaBlend := SplashForm.AlphaBlend;
  SplashForm.FormObject.AlphaBlendValue := SplashForm.AlphaBlendValue;
  SplashForm.FormObject.Caption := SplashForm.Caption;
  SplashForm.FormObject.Color := SplashForm.Color;
  SplashForm.FormObject.Enabled := SplashForm.Enabled;
  SplashForm.FormObject.WindowState := SplashForm.WindowState;
  SplashForm.FormObject.Position := SplashForm.Position;
  SplashForm.FormObject.Cursor := SplashForm.Cursor;
  { TSplashProgressBar -> TProgressBar }
  SplashProgressBar.ProgressBarObject.Left := SplashProgressBar.Left;
  SplashProgressBar.ProgressBarObject.Top := SplashProgressBar.Top;
  SplashProgressBar.ProgressBarObject.Width := SplashProgressBar.Width;
  SplashProgressBar.ProgressBarObject.Height := SplashProgressBar.Height;
  SplashProgressBar.ProgressBarObject.Visible := SplashProgressBar.Visible;
  SplashProgressBar.ProgressBarObject.Position := SplashProgressBar.Position;
  SplashProgressBar.ProgressBarObject.Max := SplashProgressBar.Max;
  SplashProgressBar.ProgressBarObject.Min := SplashProgressBar.Min;
  SplashProgressBar.ProgressBarObject.Cursor := SplashForm.FormObject.Cursor;
  SplashProgressBar.ProgressBarObject.Align := SplashProgressBar.Align;
  SplashProgressBar.ProgressBarObject.BackgroundColor := SplashProgressBar.BackgroundColor;
  SplashProgressBar.ProgressBarObject.BarColor := SplashProgressBar.BarColor;
  SplashProgressBar.ProgressBarObject.State := SplashProgressBar.State;
  SplashProgressBar.ProgressBarObject.Style := SplashProgressBar.Style;
  SplashProgressBar.ProgressBarObject.Smooth := SplashProgressBar.Smooth;
  SplashProgressBar.ProgressBarObject.SmoothReverse := SplashProgressBar.SmoothReverse;
  SplashProgressBar.ProgressBarObject.MarqueeInterval := SplashProgressBar.MarqueeInterval;
  SplashProgressBar.ProgressBarObject.Step := SplashProgressBar.Step;
  { TSplashImage -> TImage }
  SplashImage.ImageObject.Visible := SplashImage.Visible;
  SplashImage.ImageObject.Transparent := SplashImage.Transparent;
  SplashImage.ImageObject.Picture := SplashImage.Picture;
  { TSplashTimer -> TTimer }
  SplashTimer.TimerObject.Enabled := SplashTimer.Enabled;
  SplashTimer.TimerObject.Interval := SplashTimer.Interval;
end;

procedure TSplashScreen.Reset;
begin
  { TSplashForm }
  SplashForm.BorderStyle := bsNone;
  SplashForm.BorderIcons := [];
  SplashForm.Left := 0;
  SplashForm.Top := 0;
  SplashForm.Width := 600;
  SplashForm.Height := 400;
  SplashForm.Align := alNone;
  SplashForm.AlphaBlend := False;
  SplashForm.AlphaBlendValue := 255;
  SplashForm.Color := clBtnFace;
  SplashForm.Enabled := True;
  SplashForm.WindowState := wsNormal;
  SplashForm.Position := poScreenCenter;
  SplashForm.Cursor := crHourGlass;
  { TSplashProgressBar }
  SplashProgressBar.Left := 250;
  SplashProgressBar.Top := 250;
  SplashProgressBar.Width := 100;
  SplashProgressBar.Height := 50;
  SplashProgressBar.Visible := True;
  SplashProgressBar.Position := 0;
  SplashProgressBar.Max := 100;
  SplashProgressBar.Min := 0;
  SplashProgressBar.Align := alNone;
  SplashProgressBar.BackgroundColor := clDefault;
  SplashProgressBar.BarColor := clDefault;
  SplashProgressBar.State := pbsNormal;
  SplashProgressBar.Style := pbstNormal;
  SplashProgressBar.Smooth := False;
  SplashProgressBar.SmoothReverse := False;
  SplashProgressBar.MarqueeInterval := 10;
  SplashProgressBar.Step := 10;
  { TSplashImage }
  SplashImage.Visible := True;
  SplashImage.Transparent := False;
  { TSplashTimer }
  SplashTimer.Enabled := False;
  SplashTimer.Interval := 1000;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TSplashScreen.SetVisible(Value: Boolean);
begin
  if Value then
  begin
    if not SplashForm.FormObject.Visible then
    begin
      Show;
    end;
  end else
  begin
    if SplashForm.FormObject.Visible then
    begin
      Hide;
    end;
  end;
end;

procedure TSplashScreen.SetSplashForm(Value: TSplashForm);
begin
  FSplashForm.Assign(Value);
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TSplashScreen.SetSplashProgressBar(Value: TSplashProgressBar);
begin
  FSplashProgressBar.Assign(Value);
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TSplashScreen.SetSplashImage(Value: TSplashImage);
begin
  FSplashImage.Assign(Value);
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TSplashScreen.SetSplashTimer(Value: TSplashTimer);
begin
  FSplashTimer.Assign(Value);
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TSplashScreen.AnimationTimerShowTimer(Sender: TObject);
begin
  if Animation = ssaShallow then
  begin
    if SplashForm.FormObject.AlphaBlendValue < SplashForm.AlphaBlendValue then
    begin
      if SplashForm.AlphaBlendValue - SplashForm.FormObject.AlphaBlendValue < AnimationSpeed then
      begin
        SplashForm.FormObject.AlphaBlendValue := SplashForm.FormObject.AlphaBlendValue + (SplashForm.AlphaBlendValue - SplashForm.FormObject.AlphaBlendValue);
      end else
      begin
        SplashForm.FormObject.AlphaBlendValue := SplashForm.FormObject.AlphaBlendValue + AnimationSpeed;
      end;
    end else
    begin
      AnimationTimerShow.Enabled := False;
      SplashForm.FormObject.AlphaBlend := SplashForm.AlphaBlend;
    end;
  end;
end;

procedure TSplashScreen.AnimationTimerHideTimer(Sender: TObject);
begin
  if Animation = ssaShallow then
  begin
    if SplashForm.FormObject.AlphaBlendValue > 0 then
    begin
      if SplashForm.FormObject.AlphaBlendValue < AnimationSpeed then
      begin
        SplashForm.FormObject.AlphaBlendValue := SplashForm.FormObject.AlphaBlendValue - SplashForm.FormObject.AlphaBlendValue;
      end else
      begin
        SplashForm.FormObject.AlphaBlendValue := SplashForm.FormObject.AlphaBlendValue - AnimationSpeed;
      end;
    end else
    begin
      AnimationTimerHide.Enabled := False;
      SplashForm.FormObject.AlphaBlend := SplashForm.AlphaBlend;
      SplashForm.FormObject.Hide;
    end;
  end;
end;

{ ----------------------------------------------------------------------------
  TComponentManager
  ---------------------------------------------------------------------------- }

constructor TComponentManager.Create(AOwner: TComponent);
begin
  inherited;
  //...
end;

destructor TComponentManager.Destroy;
begin
  //...
  inherited;
end;

procedure TComponentManager.Update;
begin
  if Assigned(OnUpdate) then
  begin
    OnUpdate(Self);
  end;
end;

{ ----------------------------------------------------------------------------
  TProgressBarManager
  ---------------------------------------------------------------------------- }

constructor TProgressBarManager.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TProgressBarManager);
  FMode := pmmNone;

  Update;
end;

destructor TProgressBarManager.Destroy;
begin
  FAbout.Free;
  inherited;
end;

procedure TProgressBarManager.Update;
begin
  if not Assigned(Target) then
  begin
    Exit;
  end;
  if Mode <> pmmNone then
  begin
    if (Mode = pmmBattery) and (not Assigned(SourceBattery))
    {$IFDEF ADD_DEPENDENCIES}or (Mode = pmmDownload) and (not Assigned(SourceDownload)){$ENDIF} then
    begin
      Exit;
    end;
  end;
  if Mode = pmmBattery then
  begin
    Target.Position := (Target.Max div 100) * SourceBattery.GetBatteryPercent;
  end;
  {$IFDEF ADD_DEPENDENCIES}
    if Mode = pmmDownload then
    begin
      Target.Position := (Target.Max div 100) * SourceDownload.GetProgressPercent;
    end;
  {$ENDIF}
  inherited;
end;

procedure TProgressBarManager.SetTarget(Value: TProgressBar);
begin
  FTarget := Value;
  Update;
end;

procedure TProgressBarManager.SetSourceBattery(Value: TBattery);
begin
  FSourceBattery := Value;
  if Mode = pmmBattery then
  begin
    Update;
  end;
end;

{$IFDEF ADD_DEPENDENCIES}
  procedure TProgressBarManager.SetSourceDownload(Value: TDownload);
  begin
    FSourceDownload := Value;
    if Mode = pmmDownload then
    begin
      Update;
    end;
  end;
{$ENDIF}

procedure TProgressBarManager.SetMode(Value: TProgressBarManagerMode);
begin
  FMode := Value;
  Update;
end;

{ ----------------------------------------------------------------------------
  TListBoxManager
  ---------------------------------------------------------------------------- }

constructor TListBoxManager.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TListBoxManager);
  FMode := lmmNone;
  FFilterOptions := [sfoCaseSensitive,sfoForceTrim,sfoDefaultVisible];
  FilteredList := TFilteredStringList.Create;
  Update;
end;

destructor TListBoxManager.Destroy;
begin
  FAbout.Free;
  FilteredList.Free;
  inherited;
end;

procedure TListBoxManager.Update;
begin
  if not Assigned(Target) then
  begin
    Exit;
  end;
  if Mode <> lmmNone then
  begin
    if (Mode = lmmEdit) and (not Assigned(SourceEdit))
    or (Mode = lmmComboBox) and (not Assigned(SourceComboBox)) then
    begin
      Exit;
    end;
  end;
  if Mode = lmmEdit then
  begin
    FilteredList.Filter := SourceEdit.Text;
  end;
  if Mode = lmmComboBox then
  begin
    FilteredList.Filter := SourceComboBox.Text;
  end;
  Target.Items.Assign(FilteredList.FilteredStrings);
  inherited;
end;

procedure TListBoxManager.LoadList;
begin
  if Assigned(Target) then
  begin
    FilteredList.Assign(Target.Items);
  end;
end;

procedure TListBoxManager.SetTarget(Value: TListBox);
begin
  FTarget := Value;
  LoadList;
  Update;
end;

procedure TListBoxManager.SetSourceEdit(Value: TEdit);
begin
  FSourceEdit := Value;
  if Mode = lmmEdit then
  begin
    Update;
  end;
end;

procedure TListBoxManager.SetSourceComboBox(Value: TComboBox);
begin
  FSourceComboBox := Value;
  if Mode = lmmComboBox then
  begin
    Update;
  end;
end;

procedure TListBoxManager.SetMode(Value: TListBoxManagerMode);
begin
  FMode := Value;
  Update;
end;

procedure TListBoxManager.SetFilterOptions(Value: TStringFilterOptions);
begin
  FilteredList.FilterOptions := Value;
  FFilterOptions := Value;
  Update;
end;

{ ----------------------------------------------------------------------------
  TParamFormat
  ---------------------------------------------------------------------------- }

constructor TParamFormat.Create;
begin
  inherited;
  //...
end;

constructor TParamFormat.Create(APrefix,ASeparator,ASuffix: String);
begin
  Create;
  Prefix := APrefix;
  Separator := ASeparator;
  Suffix := ASuffix;
end;

destructor TParamFormat.Destroy;
begin
  //...
  inherited;
end;

procedure TParamFormat.SetPrefix(Value: String);
var
  Index: Integer;
begin
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in [#34,#39] then
    begin
      raise EInvalidParamFormat.Create('Invalid param format for property: "Prefix"');
    end;
  end;
  FPrefix := Value;
end;

procedure TParamFormat.SetSuffix(Value: String);
var
  Index: Integer;
begin
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in [#34,#39] then
    begin
      raise EInvalidParamFormat.Create('Invalid param format for property: "Suffix"');
    end;
  end;
  FSuffix := Value;
end;

procedure TParamFormat.SetSeparator(Value: String);
var
  Index: Integer;
begin
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] in [#34,#39] then
    begin
      raise EInvalidParamFormat.Create('Invalid param format for property: "Seperator"');
    end;
  end;
  FSeparator := Value;
end;

{ ----------------------------------------------------------------------------
  TParamReference
  ---------------------------------------------------------------------------- }

constructor TParamReference.Create(Collection: TCollection);
begin
  inherited;
  FFormat := TParamFormat.Create;
  FIdentifier := ClassName + IntToStr(ID);
end;

destructor TParamReference.Destroy;
begin
  FConnector := nil;
  FFormat.Free;
  inherited;
end;

function TParamReference.GetFormat: TParamFormat;
begin
  Result := FFormat;
end;

procedure TParamReference.SetFormat(Value: TParamFormat);
begin
  FFormat.Assign(Value);
end;

procedure TParamReference.SetIdentifier(Value: String);
var
  Index: Integer;
begin
  if Length(Value) > 0 then
  begin
    for Index := 1 to Length(Value) do
    begin
      if (Value[Index] in Spaces) or (Value[Index] in [#34,#39]) then
      begin
        raise EInvalidParamIdentifier.Create('"' + Value + '" is not a valid identifier value');
      end;
    end;
  end else
  begin
    raise EInvalidParamIdentifier.Create('"' + Value + '" is not a valid identifier value');
  end;
  FIdentifier := Value;
end;

procedure TParamReference.Update;
var
  Value: String;
  ParamIndex: Integer;
  ParamItem: String;
  CharIndex: Integer;
  InPrefix: Boolean;
  InSep: Boolean;
  InDef: Boolean;
  InSuffix: Boolean;
begin
  Connector^ := DefaultValue;
  Value := '';
  for ParamIndex := 1 to ParamCount do
  begin
    InPrefix := True;
    InSuffix := False;
    InSep := False;
    InDef := False;
    ParamItem := ParamStr(ParamIndex);
    CharIndex := 1;
    while CharIndex <= Length(ParamItem) do
    begin
      //[X----]
      if InPrefix then
      begin
        if Length(Format.Prefix) = 0 then
        begin
          InPrefix := False;
          Continue;
        end;
        if ParamItem[CharIndex] = Format.Prefix[CharIndex] then
        begin
          if CharIndex = Length(Format.Prefix) then
          begin
            InPrefix := False;
          end;
        end else
        begin
          Break;
        end;
      end else
      begin
        //[----X]
        if InSuffix then
        begin
          if (ParamItem[CharIndex] = Format.Suffix[CharIndex - Length(Format.Prefix + Identifier + Format.Separator + Value)]) then
          begin
            if CharIndex = Length(ParamItem) then
            begin
              //--> EXIT <--
              Connector^ := Value;
              Exit;
            end;
          end else
          begin
            Break;
          end;
        end else
        //Identifier [ InIdent := not (InPrefix or InSuffix) ]
        begin
          //[--X--]
          if InSep then
          begin
            if Length(Format.Separator) = 0 then
            begin
              InSep := False;
              InDef := True;
              Continue;
            end;
            if ParamItem[CharIndex] = Format.Separator[CharIndex - Length(Format.Prefix + Identifier)] then
            begin
              if CharIndex = Length(Format.Prefix + Identifier + Format.Separator) then
              begin
                InSep := False;
                InDef := True;
              end;
            end else
            begin
              Break;
            end;
          end else
          begin
            //[---X-]
            if InDef then
            begin
              Value := Value + ParamItem[CharIndex];
              if CharIndex = Length(ParamItem) - Length(Format.Suffix) then
              begin
                InDef := False;
                InSuffix := True;
                if Length(Format.Suffix) = 0 then
                begin
                  //--> EXIT <--
                  Connector^ := Value;
                  Exit;
                end;
              end;
            end else
            begin
              //[-X---]
              if ParamItem[CharIndex] = Identifier[CharIndex - Length(Format.Prefix)] then
              begin
                if CharIndex = Length(Identifier + Format.Prefix) then
                begin
                  InSep := True;
                end;
              end else
              begin
                Break;
              end;
            end;
          end;
        end;
      end;
      Inc(CharIndex);
    //While (CharIndex)
    end;
  //For (ParamIndex)
  end;
end;

function TParamReference.AsText(UseDefVal: Boolean = False): String;
begin
  Result := StringFromParamRef(Self,UseDefVal);
end;

{ ----------------------------------------------------------------------------
  TParamDefiner
  ---------------------------------------------------------------------------- }

constructor TParamDefiner.Create;
begin
  inherited;
  FAbout := TComponentAbout.Create(TParamDefiner);
  FReferences := TParamReferences.Create(TParamReference);
end;

destructor TParamDefiner.Destroy;
begin
  FAbout.Free;
  FReferences.Free;
  inherited;
end;

procedure TParamDefiner.SetReferences(Value: TParamReferences);
begin
  FReferences := Value;
end;

procedure TParamDefiner.Update;
var
  Index: Integer;
begin
  if References.Count > 0 then
  begin
    try
      for Index := 0 to References.Count - 1 do
      begin
        (References.Items[Index] as TParamReference).Update;
      end;
    finally
      if Assigned(OnUpdate) then
      begin
        OnUpdate(Self);
      end;
    end;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramPadding
  ---------------------------------------------------------------------------- }

constructor TDiagramPadding.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FLeft := 5;
  FRight := 5;
  FTop := 0;
  FBottom := 0;
  FAxis := 0;
end;

destructor TDiagramPadding.Destroy;
begin
  //...
  inherited;
end;

procedure TDiagramPadding.SetTop(Value: Integer);
begin
  FTop := Value;
  FDiagram.Repaint;
end;

procedure TDiagramPadding.SetBottom(Value: Integer);
begin
  FBottom := Value;
  FDiagram.Repaint;
end;

procedure TDiagramPadding.SetLeft(Value: Integer);
begin
  FLeft := Value;
  FDiagram.Repaint;
end;

procedure TDiagramPadding.SetRight(Value: Integer);
begin
  FRight := Value;
  FDiagram.Repaint;
end;

procedure TDiagramPadding.SetAxis(Value: Integer);
begin
  FAxis := Value;
  FDiagram.Repaint;
end;

procedure TDiagramPadding.Assign(Source: TPersistent);
begin
  if Source is TDiagramPadding then
  begin
    Top := (Source as TDiagramPadding).Top;
    Bottom := (Source as TDiagramPadding).Bottom;
    Left := (Source as TDiagramPadding).Left;
    Right := (Source as TDiagramPadding).Right;
    Axis := (Source as TDiagramPadding).Axis;
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramValue
  ---------------------------------------------------------------------------- }

constructor TDiagramValue.Create(Collection: TCollection);
begin
  if not (Collection is TDiagramValues) then
  begin
    raise EInvalidDiagramValueCollection.Create('Collection must be a TDiagramValues object');
  end;
  inherited;
  FName := 'Value' + IntToStr(ID);
  FColor := clNone;
  FValue := 0;
  FVisible := True;
  FBorderStyle := bsSingle;
  FBorderWidth := 1;
  FBorderColor := clNone;
end;

destructor TDiagramValue.Destroy;
begin
  //...
  inherited;
end;

function TDiagramValue.GetData: TDiagramValueData;
begin
  Result.Name := Name;
  Result.Color := Color;
  Result.Value := Value;
  Result.Visible := Visible;
  Result.BorderStyle := BorderStyle;
  Result.BorderWidth := BorderWidth;
  Result.BorderColor := BorderColor;
end;

procedure TDiagramValue.SetData(Value: TDiagramValueData);
begin
  Name := Value.Name;
  Color := Value.Color;
  Self.Value := Value.Value;
  Visible := Value.Visible;
  BorderStyle := Value.BorderStyle;
  BorderWidth := Value.BorderWidth;
  BorderColor := Value.BorderColor;
end;

procedure TDiagramValue.SetName(Value: String);
begin
  FName := Value;
  (Collection as TDiagramValues).FDiagram.Repaint;
end;

procedure TDiagramValue.SetColor(Value: TColor);
begin
  FColor := Value;
  (Collection as TDiagramValues).FDiagram.Repaint;
end;

procedure TDiagramValue.SetValue(Value: Integer);
begin
  FValue := Value;
  (Collection as TDiagramValues).FDiagram.Repaint;
end;

procedure TDiagramValue.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  (Collection as TDiagramValues).FDiagram.Repaint;
end;

procedure TDiagramValue.SetBorderStyle(Value: TBorderStyle);
begin
  FBorderStyle := Value;
  (Collection as TDiagramValues).FDiagram.Repaint;
end;

procedure TDiagramValue.SetBorderWidth(Value: Integer);
begin
  FBorderWidth := Value;
  (Collection as TDiagramValues).FDiagram.Repaint;
end;

procedure TDiagramValue.SetBorderColor(Value: TColor);
begin
  FBorderColor := Value;
  (Collection as TDiagramValues).FDiagram.Repaint;
end;

function TDiagramValue.GetDisplayName: String;
begin
  inherited;
  Result:= FName + ' (' + IntToStr(FValue) + ')';
end;

procedure TDiagramValue.Assign(Source: TPersistent);
begin
  if Source is TDiagramValue then
  begin
    Name := (Source as TDiagramValue).Name;
    Color := (Source as TDiagramValue).Color;
    Value := (Source as TDiagramValue).Value;
    Visible := (Source as TDiagramValue).Visible;
    BorderStyle := (Source as TDiagramValue).BorderStyle;
    BorderWidth := (Source as TDiagramValue).BorderWidth;
    BorderColor := (Source as TDiagramValue).BorderColor;
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramValues
  ---------------------------------------------------------------------------- }

constructor TDiagramValues.Create(ADiagram: TDiagram);
begin
  inherited Create(TDiagramValue);
  FDiagram := ADiagram;
end;

destructor TDiagramValues.Destroy;
begin
  //...
  inherited;
end;

function TDiagramValues.GetMinValue: Integer;
var
  Index: Integer;
begin
  if VisibleCount = 0 then
  begin
    Result := 0;
  end else
  begin
    Result := (Items[First] as TDiagramValue).Value;
    for Index := 1 to Count - 1 do
    begin
      if (Items[Index] as TDiagramValue).Visible and ((Items[Index] as TDiagramValue).Value < Result) then
      begin
        Result := (Items[Index] as TDiagramValue).Value;
      end;
    end;
  end;
end;

function TDiagramValues.GetMaxValue: Integer;
var
  Index: Integer;
begin
  if VisibleCount = 0 then
  begin
    Result := 0;
  end else
  begin
    Result := (Items[First] as TDiagramValue).Value;
    for Index := 1 to Count - 1 do
    begin
      if (Items[Index] as TDiagramValue).Visible and ((Items[Index] as TDiagramValue).Value > Result) then
      begin
        Result := (Items[Index] as TDiagramValue).Value;
      end;
    end;
  end;
end;

function TDiagramValues.GetMidValue: Integer;
begin
  Result := MinValue + (MaxValue - MinValue) div 2;
end;

function TDiagramValues.GetAvgValue: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to Count - 1 do
  begin
    if (Items[Index] as TDiagramValue).Visible then
    begin
      Result := Result + (Items[Index] as TDiagramValue).Value;
    end;
  end;
  Result := Result div VisibleCount;
end;

function TDiagramValues.GetFirst: Integer;
begin
  for Result := 0 to Count - 1 do
  begin
    if (Items[Result] as TDiagramValue).Visible then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

function TDiagramValues.GetLast: Integer;
begin
  for Result := Count - 1 downto 0 do
  begin
    if (Items[Result] as TDiagramValue).Visible then
    begin
      Exit;
    end;
  end;
end;

function TDiagramValues.GetVisibleCount: Integer;
var
  Index: Integer;
begin
  Result := 0;
  for Index := 0 to Count - 1 do
  begin
    if (Items[Index] as TDiagramValue).Visible then
    begin
      Inc(Result);
    end;
  end;
end;

procedure TDiagramValues.Update(Item: TCollectionItem);
begin
  inherited;
  FDiagram.Repaint;
end;

function TDiagramValues.AddData(Data: TDiagramValueData): TDiagramValue;
begin
  Result := (Add as TDiagramValue);
  Result.Data := Data;
end;

function TDiagramValues.FindItem(const Name: String): TDiagramValue;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    if (Items[Index] as TDiagramValue).Name = Name then
    begin
      Result := Items[Index] as TDiagramValue;
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TDiagramValues.ImportFromFile(const FileName: String);
var
  DataFile: file of TDiagramValueData;
  Data: TDiagramValueData;
begin
  AssignFile(DataFile,FileName);
  Reset(DataFile);
  while not Eof(DataFile) do
  begin
    Read(DataFile,Data);
    AddData(Data);
  end;
  CloseFile(DataFile);
end;

procedure TDiagramValues.ExportToFile(const FileName: String);
var
  DataFile: file of TDiagramValueData;
  Index: Integer;
  Data: TDiagramValueData;
begin
  AssignFile(DataFile,FileName);
  if FileExists(FileName) then
  begin
    Reset(DataFile);
  end else
  begin
    Rewrite(DataFile);
  end;
  for Index := 0 to Count - 1 do
  begin
    Data := (Items[Index] as TDiagramValue).Data;
    Write(DataFile,Data);
  end;
  CloseFile(DataFile);
end;

procedure TDiagramValues.LoadFromFile(const FileName: String);
begin
  Clear;
  ImportFromFile(FileName);
end;

procedure TDiagramValues.SaveToFile(const FileName: String);
var
  DataFile: file of TDiagramValueData;
  Index: Integer;
  Data: TDiagramValueData;
begin
  AssignFile(DataFile,FileName);
  Rewrite(DataFile);
  for Index := 0 to Count - 1 do
  begin
    Data := (Items[Index] as TDiagramValue).Data;
    Write(DataFile,Data);
  end;
  CloseFile(DataFile);
end;

{ ----------------------------------------------------------------------------
  TDiagramScaleValueArtLines
  ---------------------------------------------------------------------------- }

constructor TDiagramScaleValueArtLines.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FVisible := False;
  FColor := clDkGray;
  FWidth := 1;
  FDotted := True;
end;

destructor TDiagramScaleValueArtLines.Destroy;
begin
  //...
  inherited;
end;

procedure TDiagramScaleValueArtLines.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleValueArtLines.SetColor(Value: TColor);
begin
  FColor := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleValueArtLines.SetWidth(Value: Integer);
begin
  FWidth := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleValueArtLines.SetDotted(Value: Boolean);
begin
  FDotted := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleValueArtLines.Assign(Source: TPersistent);
begin
  if Source is TDiagramScaleValueArtLines then
  begin
    Visible := (Source as TDiagramScaleValueArtLines).Visible;
    Color := (Source as TDiagramScaleValueArtLines).Color;
    Width := (Source as TDiagramScaleValueArtLines).Width;
    Dotted := (Source as TDiagramScaleValueArtLines).Dotted;
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramScaleCursorArtLines
  ---------------------------------------------------------------------------- }

constructor TDiagramScaleCursorArtLines.Create(ADiagram: TDiagram);
begin
  inherited;
  FDotted := False;
end;

destructor TDiagramScaleCursorArtLines.Destroy;
begin
  //...
  inherited;
end;

{ ----------------------------------------------------------------------------
  TDiagramScaleArtLines
  ---------------------------------------------------------------------------- }

constructor TDiagramScaleArtLines.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FValues := TDiagramScaleValueArtLines.Create(ADiagram);
  FCursor := TDiagramScaleCursorArtLines.Create(ADiagram);
end;

destructor TDiagramScaleArtLines.Destroy;
begin
  FValues.Free;
  FCursor.Free;
  inherited;
end;

procedure TDiagramScaleArtLines.SetValues(Value: TDiagramScaleValueArtLines);
begin
  FValues.Assign(Value);
  FDiagram.Repaint;
end;

procedure TDiagramScaleArtLines.SetCursor(Value: TDiagramScaleCursorArtLines);
begin
  FCursor.Assign(Value);
  FDiagram.Repaint;
end;

procedure TDiagramScaleArtLines.Assign(Source: TPersistent);
begin
  if Source is TDiagramScaleArtLines then
  begin
    Values.Assign((Source as TDiagramScaleArtLines).Values);
    Cursor.Assign((Source as TDiagramScaleArtLines).Cursor);
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramScaleBar
  ---------------------------------------------------------------------------- }

constructor TDiagramScaleBar.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FVisible := True;
  FColor := clBlack;
  FWidth := 2;
  FRulerWidth := 0;
  FRulerGap := 10;
  FRulerNumbers := False;
end;

destructor TDiagramScaleBar.Destroy;
begin
  //...
  inherited;
end;

procedure TDiagramScaleBar.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleBar.SetColor(Value: TColor);
begin
  FColor := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleBar.SetWidth(Value: Integer);
begin
  FWidth := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleBar.SetRulerWidth(Value: Word);
begin
  FRulerWidth := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleBar.SetRulerGap(Value: Word);
begin
  if Value = 0 then
  begin
    raise EInvalidDiagramGap.Create('Invalid diagram ruler gap value for property "Bar"');
  end;
  FRulerGap := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleBar.SetRulerNumbers(Value: Boolean);
begin
  FRulerNumbers := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleBar.Assign(Source: TPersistent);
begin
  if Source is TDiagramScaleBar then
  begin
    Visible := (Source as TDiagramScaleBar).Visible;
    Color := (Source as TDiagramScaleBar).Color;
    Width := (Source as TDiagramScaleBar).Width;
    RulerWidth := (Source as TDiagramScaleBar).RulerWidth;
    RulerGap := (Source as TDiagramScaleBar).RulerGap;
    RulerNumbers := (Source as TDiagramScaleBar).RulerNumbers;
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramScaleGrid
  ---------------------------------------------------------------------------- }

constructor TDiagramScaleGrid.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FVisible := False;
  FDotted := False;
  FLines := dglBoth;
  FColor := clSilver;
  FWidth := 1;
  FGap := 10;
end;

destructor TDiagramScaleGrid.Destroy;
begin
  //...
  inherited;
end;

procedure TDiagramScaleGrid.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleGrid.SetDotted(Value: Boolean);
begin
  FDotted := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleGrid.SetLines(Value: TDiagramGridLines);
begin
  FLines := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleGrid.SetColor(Value: TColor);
begin
  FColor := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleGrid.SetWidth(Value: Integer);
begin
  FWidth := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleGrid.SetGap(Value: Word);
begin
  if Value = 0 then
  begin
    raise EInvalidDiagramGap.Create('Invalid diagram gap value for property "Grid"');
  end;
  FGap := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleGrid.Assign(Source: TPersistent);
begin
  if Source is TDiagramScaleGrid then
  begin
    Visible := (Source as TDiagramScaleGrid).Visible;
    Dotted := (Source as TDiagramScaleGrid).Dotted;
    Lines := (Source as TDiagramScaleGrid).Lines;
    Color := (Source as TDiagramScaleGrid).Color;
    Width := (Source as TDiagramScaleGrid).Width;
    Gap := (Source as TDiagramScaleGrid).Gap;
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramScaleValues
  ---------------------------------------------------------------------------- }

constructor TDiagramScaleValues.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FVisible := False;
  FFont := TFont.Create;
  FFont.Color := clNone;
  FAutoColor := False;
end;

destructor TDiagramScaleValues.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TDiagramScaleValues.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleValues.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  FDiagram.Repaint;
end;

procedure TDiagramScaleValues.SetAutoColor(Value: Boolean);
begin
  FAutoColor := Value;
  FDiagram.Repaint;
end;

procedure TDiagramScaleValues.Assign(Source: TPersistent);
begin
  if Source is TDiagramScaleValues then
  begin
    Visible := (Source as TDiagramScaleValues).Visible;
    Font.Assign((Source as TDiagramScaleValues).Font);
    AutoColor := (Source as TDiagramScaleValues).AutoColor;
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramScale
  ---------------------------------------------------------------------------- }

constructor TDiagramScale.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FArtLines := TDiagramScaleArtLines.Create(ADiagram);
  FBar := TDiagramScaleBar.Create(ADiagram);
  FGrid := TDiagramScaleGrid.Create(ADiagram);
  FValues := TDiagramScaleValues.Create(ADiagram);
end;

destructor TDiagramScale.Destroy;
begin
  FArtLines.Free;
  FBar.Free;
  FGrid.Free;
  FValues.Free;
  inherited;
end;

procedure TDiagramScale.SetArtLines(Value: TDiagramScaleArtlines);
begin
  FArtLines.Assign(Value);
  FDiagram.Repaint;
end;

procedure TDiagramScale.SetBar(Value: TDiagramScaleBar);
begin
  FBar.Assign(Value);
  FDiagram.Repaint;
end;

procedure TDiagramScale.SetGrid(Value: TDiagramScaleGrid);
begin
  FGrid.Assign(Value);
  FDiagram.Repaint;
end;

procedure TDiagramScale.SetValues(Value: TDiagramScaleValues);
begin
  FValues.Assign(Value);
  FDiagram.Repaint;
end;

procedure TDiagramScale.Assign(Source: TPersistent);
begin
  if Source is TDiagramScale then
  begin
    ArtLines.Assign((Source as TDiagramScale).ArtLines);
    Bar.Assign((Source as TDiagramScale).Bar);
    Grid.Assign((Source as TDiagramScale).Grid);
    Values.Assign((Source as TDiagramScale).Values);
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramCaption
  ---------------------------------------------------------------------------- }

constructor TDiagramCaption.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FFont := TFont.Create;
  FFont.Size := 12;
  FFont.Color := clSilver;
  FAlignment := taCenter;
  FVerticalAlignment := taAlignTop;
end;

destructor TDiagramCaption.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TDiagramCaption.SetText(Value: TCaption);
begin
  FText := Value;
  FDiagram.Repaint;
end;

procedure TDiagramCaption.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  FDiagram.Repaint;
end;

procedure TDiagramCaption.SetAlignment(Value: TAlignment);
begin
  FAlignment := Value;
  FDiagram.Repaint;
end;

procedure TDiagramCaption.SetVerticalAlignment(Value: TVerticalAlignment);
begin
  FVerticalAlignment := Value;
  FDiagram.Repaint;
end;

procedure TDiagramCaption.Assign(Source: TPersistent);
begin
  if Source is TDiagramCaption then
  begin
    Text := (Source as TDiagramCaption).Text;
    Font.Assign((Source as TDiagramCaption).Font);
    Alignment := (Source as TDiagramCaption).Alignment;
    VerticalAlignment := (Source as TDiagramCaption).VerticalAlignment;
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramAutoColors
  ---------------------------------------------------------------------------- }

constructor TDiagramAutoColors.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FEnabled := False;
  FPositive := clBlue;
  FZero := clWhite;
  FNegative := clRed;
end;

destructor TDiagramAutoColors.Destroy;
begin
  //...
  inherited;
end;

procedure TDiagramAutoColors.SetEnabled(Value: Boolean);
begin
  FEnabled := Value;
  FDiagram.Repaint;
end;

procedure TDiagramAutoColors.SetPositive(Value: TColor);
begin
  FPositive := Value;
  FDiagram.Repaint;
end;

procedure TDiagramAutoColors.SetZero(Value: TColor);
begin
  FZero := Value;
  FDiagram.Repaint;
end;

procedure TDiagramAutoColors.SetNegative(Value: TColor);
begin
  FNegative := Value;
  FDiagram.Repaint;
end;

procedure TDiagramAutoColors.Assign(Source: TPersistent);
begin
  if Source is TDiagramAutoColors then
  begin
    Enabled := (Source as TDiagramAutoColors).Enabled;
    Positive := (Source as TDiagramAutoColors).Positive;
    Zero := (Source as TDiagramAutoColors).Zero;
    Negative := (Source as TDiagramAutoColors).Negative;
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagramTrendLine
  ---------------------------------------------------------------------------- }

constructor TDiagramTrendLine.Create(ADiagram: TDiagram);
begin
  inherited Create;
  FDiagram := ADiagram;
  FVisible := False;
  FColor := clNone;
  FDotted := False;
  FMethod := dtlAvgLeftRight;
end;

destructor TDiagramTrendLine.Destroy;
begin
  //...
  inherited;
end;

procedure TDiagramTrendLine.SetVisible(Value: Boolean);
begin
  FVisible := Value;
  FDiagram.Repaint;
end;

procedure TDiagramTrendLine.SetColor(Value: TColor);
begin
  FColor := Value;
  FDiagram.Repaint;
end;

procedure TDiagramTrendLine.SetDotted(Value: Boolean);
begin
  FDotted := Value;
  FDiagram.Repaint;
end;

procedure TDiagramTrendLine.SetWidth(Value: Integer);
begin
  FWidth := Value;
  FDiagram.Repaint;
end;

procedure TDiagramTrendLine.SetMethod(Value: TDiagramTrendLineMethod);
begin
  FMethod := Value;
  FDiagram.Repaint;
end;

procedure TDiagramTrendLine.Assign(Source: TPersistent);
begin
  if Source is TDiagramTrendLine then
  begin
    Visible := (Source as TDiagramTrendLine).Visible;
    Color := (Source as TDiagramTrendLine).Color;
    Dotted := (Source as TDiagramTrendLine).Dotted;
    Width := (Source as TDiagramTrendLine).Width;
    Method := (Source as TDiagramTrendLine).Method;
  end else
  begin
    inherited;
  end;
end;

{ ----------------------------------------------------------------------------
  TDiagram
  ---------------------------------------------------------------------------- }

constructor TDiagram.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TDiagram);
  FCaption := TDiagramCaption.Create(Self);
  FLayout := dloColumns;
  FValues := TDiagramValues.Create(Self);
  Width := 200;
  Height := 50;
  FPadding := TDiagramPadding.Create(Self);
  FScale := TDiagramScale.Create(Self);
  FAutoColors := TDiagramAutoColors.Create(Self);
  FTrendLine := TDiagramTrendLine.Create(Self);
  //OnChange-Ereignisse
  FCaption.Font.OnChange := PropertyChange;
  FScale.Values.Font.OnChange := PropertyChange;
end;

destructor TDiagram.Destroy;
begin
  FAbout.Free;
  FCaption.Free;
  FValues.Free;
  FPadding.Free;
  FScale.Free;
  FAutoColors.Free;
  FTrendLine.Free;
  inherited;
end;

procedure TDiagram.SetCaption(Value: TDiagramCaption);
begin
  FCaption.Assign(Value);
  Repaint;
end;

procedure TDiagram.SetLayout(Value: TDiagramLayout);
begin
  FLayout := Value;
  Repaint;
end;

procedure TDiagram.SetValues(Value: TDiagramValues);
begin
  FValues.Assign(Value);
  Repaint;
end;

procedure TDiagram.SetPadding(Value: TDiagramPadding);
begin
  FPadding.Assign(Value);
  Repaint;
end;

procedure TDiagram.SetScale(Value: TDiagramScale);
begin
  FScale := Value;
  Repaint;
end;

procedure TDiagram.SetAutoColors(Value: TDiagramAutoColors);
begin
  FAutoColors.Assign(Value);
  Repaint;
end;

procedure TDiagram.SetTrendLine(Value: TDiagramTrendLine);
begin
  FTrendLine.Assign(Value);
  Repaint;
end;

procedure TDiagram.PropertyChange(Sender: TObject);
begin
  Repaint;
end;

procedure TDiagram.Paint;
var
  Index: Integer;
begin
  inherited;
  DrawBackground;
  if Scale.FGrid.Visible then
  begin
    DrawGrid;
  end;
  if Scale.Bar.Visible then
  begin
    DrawBar;
  end;
  if Scale.ArtLines.Values.Visible then
  begin
    DrawValueArtLines;
  end;
  case Layout of
    dloColumns: for Index := 0 to Values.Count - 1 do
                begin
                  if (Values.Items[Index] as TDiagramValue).Visible then
                  begin
                    DrawColumn(Index);
                    if Assigned(OnDrawValue) then
                    begin
                      OnDrawValue(Self,Index);
                    end;
                  end;
                end;
    dloPoints: for Index := 0 to Values.Count - 1 do
               begin
                 if (Values.Items[Index] as TDiagramValue).Visible then
                 begin
                   DrawPoint(Index);
                   if Assigned(OnDrawValue) then
                   begin
                     OnDrawValue(Self,Index);
                   end;
                 end;
               end;
    dloStraightLines: for Index := 0 to Values.Count - 1 do
                      begin
                        if (Values.Items[Index] as TDiagramValue).Visible then
                        begin
                          DrawStraightLine(Index);
                          if Assigned(OnDrawValue) then
                          begin
                            OnDrawValue(Self,Index);
                          end;
                        end;
                      end;
    dloCurvedLines: for Index := 0 to Values.Count - 1 do
                    begin
                      if (Values.Items[Index] as TDiagramValue).Visible then
                      begin
                        DrawCurvedLine(Index);
                        if Assigned(OnDrawValue) then
                        begin
                          OnDrawValue(Self,Index);
                        end;
                      end;
                    end;
    dloCustom: for Index := 0 to Values.Count - 1 do
               begin
                 if (Values.Items[Index] as TDiagramValue).Visible then
                 begin
                   if Assigned(OnCustomDrawValue) then
                   begin
                     OnCustomDrawValue(Self,Index,ValueRect[Index]);
                   end;
                   if Assigned(OnDrawValue) then
                   begin
                     OnDrawValue(Self,Index);
                   end;
                 end;
               end;
  end;
  if TrendLine.Visible and (FValues.VisibleCount > 1) then
  begin
    DrawTrendLine;
  end;
  DrawCaption;
  if Scale.ArtLines.Cursor.Visible then
  begin
    DrawCursorArtLines;
  end;
  if Scale.Values.Visible then
  begin
    for Index := 0 to Values.Count - 1 do
    begin
      DrawValue(Index);
    end;
  end;
end;

procedure TDiagram.DrawBackground;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(Rect(0,0,Width,Height))
  end;
end;

procedure TDiagram.DrawCaption;
var
  Top: Integer;
begin
  if Length(Caption.Text) <> 0 then
  begin
    with Canvas do
    begin
      Font.Assign(Caption.Font);
      Brush.Style := bsClear;
      case Caption.VerticalAlignment of
        taAlignTop: Top := 0;
        taAlignBottom: Top := Height - TextHeight(Caption.Text);
        taVerticalCenter: Top := (Height - TextHeight(Caption.Text)) div 2;
      end;
      case Caption.Alignment of
        taLeftJustify: TextOut(0,Top,Caption.Text);
        taCenter: TextOut((Width - TextWidth(Caption.Text)) div 2,Top,Caption.Text);
        taRightJustify: TextOut(Width - TextWidth(Caption.Text),Top,Caption.Text);
      end;
    end;
  end;
end;

procedure TDiagram.DrawBar;
var
  Index: Integer;
begin
  with Canvas do
  begin
    Pen.Color := Scale.Bar.Color;
    Pen.Width := Scale.Bar.Width;
    MoveTo(ZeroWidth,0);
    LineTo(ZeroWidth,Height);
    MoveTo(ZeroWidth,ZeroHeight);
    LineTo(Width,ZeroHeight);
    Index := ZeroHeight - Scale.Bar.RulerGap;
    while Index >= 0 do
    begin
      MoveTo(ZeroWidth - Scale.Bar.RulerWidth - Scale.Bar.Width div 2 - 1,Index);
      LineTo(ZeroWidth,Index);
      Dec(Index,Scale.Bar.RulerGap);
    end;
    Index := ZeroHeight + Scale.Bar.RulerGap;
    while Index < Height do
    begin
      MoveTo(ZeroWidth - Scale.Bar.RulerWidth - Scale.Bar.Width div 2 - 1,Index);
      LineTo(ZeroWidth,Index);
      Inc(Index,Scale.Bar.RulerGap);
    end;
    if Scale.Bar.RulerWidth <> 0 then
    begin
      Brush.Color := Color;
      FillRect(Rect(0,0,ZeroWidth - Scale.Bar.RulerWidth - Scale.Bar.Width div 2,Height));
    end;
    MoveTo(-1,ZeroHeight);
    LineTo(Width,ZeroHeight);
    if Scale.Bar.RulerNumbers then
    begin
      Font.Assign(TFont.Create);
      Font.Color := Scale.Bar.Color;
      Font.Height := Scale.Bar.RulerGap;
      Index := ZeroHeight + Scale.Bar.RulerGap;
      while Index < Height do
      begin
        TextOut(ZeroWidth - Scale.Bar.RulerWidth - TextWidth(IntToStr(HeightValue[Index])),Index - Font.Height div 2,IntToStr(HeightValue[Index]));
        Inc(Index,Scale.Bar.RulerGap);
      end;
      Index := ZeroHeight - Scale.Bar.RulerGap;
      while Index >= 0 do
      begin
        TextOut(ZeroWidth - Scale.Bar.RulerWidth - TextWidth(IntToStr(HeightValue[Index])),Index - Font.Height div 2,IntToStr(HeightValue[Index]));
        Dec(Index,Scale.Bar.RulerGap);
      end;
    end;
  end;
end;

procedure TDiagram.DrawGrid;
var
  Index: Integer;
begin
  with Canvas do
  begin
    Pen.Color := Scale.Grid.Color;
    Pen.Width := Scale.Grid.Width;
    if Scale.Grid.Dotted then
    begin
      Pen.Style := psDot;
    end else
    begin
      Pen.Style := psSolid;
    end;
    if Scale.Grid.Lines in [dglBoth,dglHorizontal] then
    begin
      if Scale.Bar.Visible then
      begin
        Index := ZeroHeight - Scale.Grid.Gap;
      end else
      begin
        Index := ZeroHeight;
      end;
      while Index >= 0 do
      begin
        MoveTo(ZeroWidth,Index);
        LineTo(Width,Index);
        Dec(Index,Scale.Grid.Gap);
      end;
      if Scale.Bar.Visible then
      begin
        Index := ZeroHeight + Scale.Grid.Gap;
      end else
      begin
        Index := ZeroHeight;
      end;
      while Index < Height do
      begin
        MoveTo(ZeroWidth,Index);
        LineTo(Width,Index);
        Inc(Index,Scale.Grid.Gap);
      end;
    end;
    if Scale.Grid.Lines in [dglBoth,dglVertical] then
    begin
      if Scale.Bar.Visible then
      begin
        Index := ZeroWidth + Scale.Grid.Gap;
      end else
      begin
        Index := ZeroWidth;
      end;
      while Index < Width do
      begin
        MoveTo(Index,0);
        LineTo(Index,Height);
        Inc(Index,Scale.Grid.Gap);
      end;
    end;
  end;
end;

procedure TDiagram.DrawValueArtLines;
var
  Index: Integer;
begin
  with Canvas do
  begin
    if Scale.ArtLines.Values.Dotted then
    begin
      Canvas.Pen.Style := psDot;
    end else
    begin
      Canvas.Pen.Style := psSolid;
    end;
    Canvas.Pen.Color := Scale.ArtLines.Values.Color;
    Canvas.Pen.Width := Scale.ArtLines.Values.Width;
    for Index := 0 to Values.Count - 1 do
    begin
      if (Values.Items[Index] as TDiagramValue).Value <> 0 then
      begin
        if (Values.Items[Index] as TDiagramValue).Value > 0 then
        begin
          MoveTo(ZeroWidth,ValueHeight[(Values.Items[Index] as TDiagramValue).Value]);
          LineTo(ValueLeft[Index] + ValueWidth div 2,ValueHeight[(Values.Items[Index] as TDiagramValue).Value]);
        end else
        begin
          MoveTo(ZeroWidth,ValueHeight[(Values.Items[Index] as TDiagramValue).Value] - 1);
          LineTo(ValueLeft[Index] + ValueWidth div 2,ValueHeight[(Values.Items[Index] as TDiagramValue).Value] - 1);
        end;
        LineTo(ValueLeft[Index] + ValueWidth div 2,ZeroHeight);
      end;
    end;
  end;
end;

procedure TDiagram.DrawCursorArtLines;
var
  CursorPos: TPoint;
begin
  if not (csDesigning in ComponentState) then
  begin
    with Canvas do
    begin
      if Scale.ArtLines.Cursor.Dotted then
      begin
        Canvas.Pen.Style := psDot;
      end else
      begin
        Canvas.Pen.Style := psSolid;
      end;
      Canvas.Pen.Color := Scale.ArtLines.Cursor.Color;
      Canvas.Pen.Width := Scale.ArtLines.Cursor.Width;
      GetCursorPos(CursorPos);
      CursorPos := ScreenToClient(CursorPos);
      if (CursorPos.Y <> ZeroHeight) and (CursorPos.X > ZeroWidth) and (CursorPos.X < Width) and (CursorPos.Y >= 0) and (CursorPos.Y < Height) then
      begin
        MoveTo(ZeroWidth,CursorPos.Y);
        LineTo(CursorPos.X,CursorPos.Y);
        LineTo(CursorPos.X,ZeroHeight);
      end;
    end;
  end;
end;

procedure TDiagram.DrawTrendLine;
var
  Line: TLine;
  Index: Integer;
  VisibleIndex: Integer;
  LeftY: Integer;
  RightY: Integer;
begin
  with Canvas do
  begin
    if TrendLine.Dotted then
    begin
      Canvas.Pen.Style := psDot;
    end else
    begin
      Canvas.Pen.Style := psSolid;
    end;
    Canvas.Pen.Color := TrendLine.Color;
    Canvas.Pen.Width := TrendLine.Width;
    Line := TLine.Create;
    case TrendLine.Method of
      dtlFirstLast: begin
                      Line.LoadFromPoints(Point(ValueLeft[Values.First],             ValueHeight[(Values.Items[Values.First] as TDiagramValue).Value]) ,
                                          Point(ValueLeft[Values.Last ] + ValueWidth,ValueHeight[(Values.Items[Values.Last ] as TDiagramValue).Value]));
                    end;
      dtlAvgAll: begin
                   Line.Offset := ValueHeight[Values.AvgValue];
                   Line.Slope := 0;
                 end;
      dtlAvgLeftRight: begin
                         LeftY := 0;
                         RightY := 0;
                         Index := 0;
                         VisibleIndex := 0;
                         while (Index < Values.Count) and (VisibleIndex < (Values.VisibleCount + 1) div 2) do
                         begin
                           if (Values.Items[Index] as TDiagramValue).Visible then
                           begin
                             Inc(LeftY,(Values.Items[Index] as TDiagramValue).Value);
                             Inc(VisibleIndex);
                           end;
                           Inc(Index);
                         end;
                         LeftY := LeftY div ((Values.VisibleCount + 1) div 2);
                         if Odd(Values.VisibleCount) then
                         begin
                           Dec(VisibleIndex);
                           Dec(Index);
                         end;
                         while (Index < Values.Count) and (VisibleIndex < Values.VisibleCount) do
                         begin
                           if (Values.Items[Index] as TDiagramValue).Visible then
                           begin
                             Inc(RightY,(Values.Items[Index] as TDiagramValue).Value);
                           end;
                           Inc(Index);
                         end;
                         RightY := RightY div ((Values.VisibleCount + 1) div 2);
                         Line.LoadFromPoints(Point(        (Width - ZeroWidth) div 4,ValueHeight[LeftY])  ,
                                             Point(Width - (Width - ZeroWidth) div 4,ValueHeight[RightY]));
                       end;
    end;
    MoveTo(ZeroWidth,Line.Offset);
    LineTo(Width,Line.Y[Width]);
  end;
end;

procedure TDiagram.DrawValue(Index: Integer);
begin
  with Canvas do
  begin

  end;
end;

procedure TDiagram.DrawColumn(Index: Integer);
begin
  with Canvas do
  begin
    Pen.Width := (Values.Items[Index] as TDiagramValue).BorderWidth;
    Pen.Color := (Values.Items[Index] as TDiagramValue).BorderColor;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    if AutoColors.Enabled then
    begin
      if (Values.Items[Index] as TDiagramValue).Value > 0 then
      begin
        Brush.Color := AutoColors.Positive;
      end else
      begin
        Brush.Color := AutoColors.Negative;
      end;
    end else
    begin
      Brush.Color := (Values.Items[Index] as TDiagramValue).Color;
    end;
    Rectangle(ValueLeft[Index],ZeroHeight,ValueLeft[Index] + ValueWidth,ValueHeight[(Values.Items[Index] as TDiagramValue).Value]);
  end;
end;

procedure TDiagram.DrawPoint(Index: Integer);
begin
  with Canvas do
  begin
    Pen.Width := (Values.Items[Index] as TDiagramValue).BorderWidth;
    Pen.Color := (Values.Items[Index] as TDiagramValue).BorderColor;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    if AutoColors.Enabled then
    begin
      if (Values.Items[Index] as TDiagramValue).Value = 0 then
      begin
        Brush.Color := AutoColors.Zero;
      end else
      begin
        if (Values.Items[Index] as TDiagramValue).Value > 0 then
        begin
          Brush.Color := AutoColors.Positive;
        end else
        begin
          Brush.Color := AutoColors.Negative;
        end;
      end;
    end else
    begin
      Brush.Color := (Values.Items[Index] as TDiagramValue).Color;
    end;
    Ellipse(ValueLeft[Index],ValueHeight[(Values.Items[Index] as TDiagramValue).Value] - ValueWidth div 2,ValueLeft[Index] + ValueWidth,ValueHeight[(Values.Items[Index] as TDiagramValue).Value] + ValueWidth div 2);
  end;
end;

procedure TDiagram.DrawStraightLine(Index: Integer);
begin
  with Canvas do
  begin
    Pen.Width := (Values.Items[Index] as TDiagramValue).BorderWidth;
    Pen.Color := (Values.Items[Index] as TDiagramValue).Color;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    if Index = 0 then
    begin
      MoveTo(ZeroWidth + Padding.Left,ValueHeight[(Values.Items[Index] as TDiagramValue).Value]);
    end else
    begin
      //MoveTo(
    end;
  end;
end;

procedure TDiagram.DrawCurvedLine(Index: Integer);
begin
  with Canvas do
  begin
    Pen.Width := (Values.Items[Index] as TDiagramValue).BorderWidth;
    Pen.Color := (Values.Items[Index] as TDiagramValue).Color;
    Pen.Style := psSolid;
    Brush.Style := bsSolid;
    if Index = 0 then
    begin
      MoveTo(ZeroWidth + Padding.Left,ValueHeight[(Values.Items[Index] as TDiagramValue).Value]);
    end else
    begin
      //MoveTo(
    end;
  end;
end;

function TDiagram.GetZeroWidth: Integer;
var
  Index: Integer;
  MaxWidth: Integer;
begin
  if Scale.Bar.Visible then
  begin
    if Scale.Bar.RulerNumbers then
    begin
      with Canvas do
      begin
        Font.Assign(TFont.Create);
        Font.Color := Scale.Bar.Color;
        Font.Height := Scale.Bar.RulerGap;
      end;
      MaxWidth := Canvas.TextWidth('1');
      for Index := Values.MinValue to Values.MaxValue do
      begin
        if Canvas.TextWidth(IntToStr(Index)) > MaxWidth then
        begin
          MaxWidth := Canvas.TextWidth(IntToStr(Index));
        end;
      end;
      Result := MaxWidth + Scale.Bar.RulerWidth + Scale.Bar.Width div 2;
    end else
    begin
      Result := Scale.Bar.RulerWidth + Scale.Bar.Width div 2;
    end;
  end else
  begin
    Result := 0;
  end;
end;

function TDiagram.GetZeroHeight: Integer;
begin
  if Values.MinValue >= 0 then
  begin
    Result := Height - Scale.Bar.Width div 2 - Padding.Bottom;
  end else
  begin
    if Values.MaxValue <= 0 then
    begin
      Result := Scale.Bar.Width div 2 + Padding.Top;
    end else
    begin
      Result := Padding.Top - Padding.Bottom + Round((Height - Padding.Top + Padding.Bottom) / (Values.MaxValue - Values.MinValue) * Values.MaxValue);
    end;
  end;
end;

function TDiagram.GetValueHeight(Value: Integer): Integer;
begin
  if Value = 0 then
  begin
    Result := ZeroHeight;
  end else
  begin
    if Value > 0 then
    begin
      Result := ZeroHeight - Round(Value / Values.MaxValue * (ZeroHeight - Padding.Top));
    end else
    begin
      Result := ZeroHeight + Round(Value / Values.MinValue * (Height - ZeroHeight - Padding.Bottom));
    end;
  end;
end;

function TDiagram.GetHeightValue(Height: Integer): Integer;
begin
  if Height = ZeroHeight then
  begin
    Result := 0;
  end else
  begin
    if Height < ZeroHeight then
    begin
      Result := Trunc((ZeroHeight - Height) * (Values.MaxValue / (ZeroHeight - Padding.Top)));
    end else
    begin
      Result := Trunc((Height - ZeroHeight) * (Values.MinValue / (Self.Height - Padding.Bottom - ZeroHeight)));
    end;
  end;
end;

function TDiagram.GetValueLeft(Index: Integer): Integer;
var
  ValueIndex: Integer;
begin
  Result := ZeroWidth + Padding.Left;
  for ValueIndex := 0 to Index - 1 do
  begin
    if (Values.Items[ValueIndex] as TDiagramValue).Visible then
    begin
      Inc(Result,ValueSpace);
    end;
  end;
end;

function TDiagram.GetValueWidth: Integer;
begin
  Result := ValueSpace - Padding.Left - Padding.Right;
end;

function TDiagram.GetValueSpace: Integer;
begin
  if Values.Count = 0 then
  begin
    Result := 0;
  end else
  begin
    Result := (Width - ZeroWidth) div Values.VisibleCount;
  end;
end;

function TDiagram.GetIndexAt(Left: Integer): Integer;
begin
  Dec(Left, ZeroWidth);
  while Left >= 0 do
  begin

  end;

end;

procedure TDiagram.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Scale.FArtLines.Cursor.Visible then
  begin
    Repaint;
  end;
end;

procedure TDiagram.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Scale.FArtLines.Cursor.Visible then
  begin
    Repaint;
  end;
end;

procedure TDiagram.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Scale.FArtLines.Cursor.Visible then
  begin
    Repaint;
  end;
end;

function TDiagram.GetValueRect(Index: Integer): TRect;
begin
  Result := GetValueRect(Values.Items[Index] as TDiagramValue);
end;

function TDiagram.GetValueRect(Value: TDiagramValue): TRect;
begin
  if Value.Visible then
  begin
    Result := Rect(ValueLeft[Value.Index],Padding.Top,ValueLeft[Value.Index] + ValueWidth,Height - Padding.Bottom);
  end else
  begin
    Result := Rect(ValueLeft[Value.Index],Padding.Top,ValueLeft[Value.Index],Height - Padding.Bottom);
  end;
end;

end.
