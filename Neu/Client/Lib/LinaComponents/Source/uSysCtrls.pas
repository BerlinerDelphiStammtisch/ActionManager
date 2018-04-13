unit uSysCtrls;

//////////////////////////////////////
///  Lina System Controls Unit     ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

uses
  { Standard-Units }
  SysUtils, Classes, Windows, ExtCtrls, Registry, TlHelp32, PsAPI,
  { Andere Package-Units }
  uBase, uSysTools, uFileTools;

type
  { Fehlermeldungen }
  EBatteryFlag = class(Exception);
  EDelphiVersion = class(Exception);
  EInvalidKey = class(Exception);

  { Hilfsklassen }
  TBatteryFlag = (bfHealthy,bfLow,bfCritical,bfCharge,bfHealthyAccu,bfNone,bfUnknown);
  TBatteryStatus = (bsInternal,bsExternal);
  TProcessRefreshMode = (prNone,prAccess,prTime);
  TDelphiVersion = (dv7,dv2005,dv2006,dv2007,dv2009,dv2010,dvXE,dvXE2,dvXE3,dvXE4,dvXE5,dvXE6,dvXE7,dvXE8,dv10,dv101,dv102);
  TDelphiVersions = set of TDelphiVersion;
  TDelphiCompilerOptions = set of (dcmDebug,dcmConsole,dcmTypeInfo,dcmUnicode,dcmExtSyntax,dcmAssembler,dcmCondExpr);
  TDelphiTargetPlatform = (dpWin32,dpWin64,dpOSX32,dpAndroid32,dpIOSDev,dpIOSSim,dpIOSDev32,dpIOSDev64);
  TDelphiTargetPlatforms = set of TDelphiTargetPlatform;
  TDelphiLocalPlatform = dpWin32..dpWin64; //Unterstützte Zielplattformen von TDelphiManager
  TDelphiEdition = (dePersonal,deProfessional,deEnterprise,deUltimate,deArchitect);
  TCryptMode = (cmCustom,cmXor,cmCaesar,cmVigenere);

  { Ereignisse }
  TProcessManagerUpdateEvent = procedure(Sender: TObject; const Modified: Boolean) of object;
  TStringContainerChangeEvent = procedure(Sender: TObject) of object;
  TStringContainerChangingEvent = procedure(Sender: TObject) of object;
  TCryptWorkEvent = procedure(Sender: TObject) of object;
  TCryptKeyChangeEvent = procedure(Sender: TObject) of object;
  TCryptKeyChangeQueryEvent = procedure(Sender: TObject; const NewKey: String; var CanChange: Boolean) of object;

  { Hauptklassen }
  PTOKEN_USER = ^TOKEN_USER;
  _TOKEN_USER = record
    User: TSidAndAttributes;
  end;
  TOKEN_USER = _TOKEN_USER;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TBattery = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
  protected
    { Protected-Deklarationen }
    function BatteryFlag(Flag: Integer): TBatteryFlag;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetPowerStatus: TBatteryStatus;
    function GetBatteryTime: TDateTime;
    function GetBatteryFullTime: TDateTime;
    function GetBatteryFlag: TBatteryFlag;
    function GetBatteryFlagReport: String;
    function GetBatteryPercent: Byte;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCursorFix = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TProcessManager = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    TimerObject: TTimer;
    FNames: TStrings;
    FTimeOut: DWORD;
    FRefreshMode: TProcessRefreshMode;
    { Ereignisse }
    FUpdateEvent: TProcessManagerUpdateEvent;
    { Methoden }
    function GetNames: TStrings;
    procedure SetRefreshMode(Value: TProcessRefreshMode);
    function GetInterval: Cardinal;
    procedure SetInterval(Value: Cardinal);
    procedure TimerObjectTimer(Sender: TObject);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update;
    procedure Kill(ProcID: DWORD); overload;
    procedure Kill(ProcName: String); overload;
    procedure Start(FileName: String);
    function GetID(ProcName: String): DWORD;
    function GetName(ProcID: DWORD): String;
    function GetPath(ProcID: DWORD): String; overload;
    function GetPath(ProcName: String): String; overload;
    function GetThreads(ProcName: String): DWORD;
    function GetParentID(ProcName: String): DWORD;
    function GetPriority(ProcName: String): Integer;
    function GetMemory(ProcID: DWORD): DWORD; overload;
    function GetMemory(ProcName: String): DWORD; overload;
    function GetUser(ProcID: DWORD): String; overload;
    function GetUser(ProcName: String): String; overload;
    function GetDomain(ProcID: DWORD): String; overload;
    function GetDomain(ProcName: String): String; overload;
    property Names: TStrings read GetNames;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
    property TimeOut: DWORD read FTimeOut write FTimeOut default 0;
    property RefreshMode: TProcessRefreshMode read FRefreshMode write SetRefreshMode default prNone;
    property Interval: Cardinal read GetInterval write SetInterval default 1000;
    { Ereignisse }
    property OnUpdate: TProcessManagerUpdateEvent read FUpdateEvent write FUpdateEvent;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TStringContainer = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FItems: TStrings;
    { Ereignisse }
    FChangeEvent: TStringContainerChangeEvent;
    FChangingEvent: TStringContainerChangingEvent;
    { Methoden }
    function GetItems: TStrings;
    procedure SetItems(Value: TStrings);
    procedure ItemsChange(Sender: TObject);
    procedure ItemsChanging(Sender: TObject);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
    property Items: TStrings read GetItems write SetItems;
    { Ereignisse }
    property OnChange: TStringContainerChangeEvent read FChangeEvent write FChangeEvent;
    property OnChanging: TStringContainerChangingEvent read FChangingEvent write FChangingEvent;
  end;

  TDelphiManager = class;

  TDelphiManagerInfo = class
  private
    { Private-Deklarationen }
    FDelphiManager: TDelphiManager;
    { Methoden }
    function GetVersion: TDelphiVersion;
    function GetVersionName: String;
    function GetVersionFullName: String;
    function GetVersionCodeName: String;
    function GetCompilerVersion: Extended;
    function GetRuntimeVersion: Extended;
    function GetLinaVersion: Extended;
    function GetProductVersion: Integer;
    function GetPackageVersion: Integer;
    function GetCompilerOptions: TDelphiCompilerOptions;
    function GetPlatform: TDelphiLocalPlatform;
    function GetPlatformName: String;
    function GetPlatformFullName: String;
  public
    { Public-Deklarationen }
    constructor Create(ADelphiManager: TDelphiManager);
    destructor Destroy; override;
    { Eigenschaften }
    property Version: TDelphiVersion read GetVersion;
    property VersionName: String read GetVersionName;
    property VersionFullName: String read GetVersionFullName;
    property VersionCodeName: String read GetVersionCodeName;
    property CompilerVersion: Extended read GetCompilerVersion;
    property RuntimeVersion: Extended read GetRuntimeVersion;
    property LinaVersion: Extended read GetLinaVersion;
    property ProductVersion: Integer read GetProductVersion;
    property PackageVersion: Integer read GetPackageVersion;
    property CompilerOptions: TDelphiCompilerOptions read GetCompilerOptions;
    property Platform: TDelphiLocalPlatform read GetPlatform;
    property PlatformName: String read GetPlatformName;
    property PlatformFullName: String read GetPlatformFullName;
  end;

  TDelphiManagerVersionPath = class(TCollectionItem)
  private
    { Private-Deklarationen }
    FName: String;
    FPath: String;
  protected
    { Protected-Deklarationen }
    function GetDisplayName: String; override;
  public
    { Public-Deklarationen }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Name: String read FName write FName;
    property Path: String read FName write FName;
  end;

  TDelphiManagerVersionPaths = TCollection;

  TDelphiManagerVersionPackage = class(TCollectionItem)
  private
    { Private-Deklarationen }
    FFileName: String;
    FDescription: String;
  protected
    { Protected-Deklarationen }
    function GetDisplayName: String; override;
  public
    { Public-Deklarationen }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property FileName: String read FFileName write FFileName;
    property Description: String read FDescription write FDescription;
  end;

  TDelphiManagerVersionPackages = TCollection;

  TDelphiManagerVersion = class
  private
    { Private-Deklarationen }
    FVersion: TDelphiVersion;
    FPackages: TDelphiManagerVersionPackages;
    FIdePackages: TDelphiManagerVersionPackages;
    { Methoden }
    function GetInstalled: Boolean;
    function GetEdition: TDelphiEdition;
    function GetRootDir: String;
    function GetExeName: String;
    function GetLanguage: String;
    procedure SetLanguage(Value: String);
    function GetTargetPlatforms: TDelphiTargetPlatforms;
    function GetBrowsingPaths(TargetPlatform: TDelphiTargetPlatform): TStrings;
    procedure SetBrowsingPaths(TargetPlatform: TDelphiTargetPlatform; Value: TStrings);
    function GetSearchPaths(TargetPlatform: TDelphiTargetPlatform): TStrings;
    procedure SetSearchPaths(TargetPlatform: TDelphiTargetPlatform; Value: TStrings);
    function GetEnvironmentVariables: TStrings;
    function GetEnvironmentVariable(Variable: String): String;
  protected
    { Protected-Deklarationen }
    function RegistryPath: String;
    function TargetPlatformKey(TargetPlatform: TDelphiTargetPlatform): String;
  public
    { Public-Deklarationen }
    constructor Create(AVersion: TDelphiVersion);
    destructor Destroy; override;
    property Version: TDelphiVersion read FVersion;
    property Installed: Boolean read GetInstalled;
    property Edition: TDelphiEdition read GetEdition;
    property RootDir: String read GetRootDir;
    property ExeName: String read GetExeName;
    property Language: String read GetLanguage;
    property TargetPlatforms: TDelphiTargetPlatforms read GetTargetPlatforms;
    property BrowsingPaths[TargetPlatform: TDelphiTargetPlatform]: TStrings read GetBrowsingPaths write SetBrowsingPaths;
    property SearchPaths[TargetPlatform: TDelphiTargetPlatform]: TStrings read GetSearchPaths write SetBrowsingPaths;
    property Packages: TDelphiManagerVersionPackages read FPackages;
    property IdePackages: TDelphiManagerVersionPackages read FIdePackages;
    property EnvironmentVariables: TStrings read GetEnvironmentVariables;
    property EnvironmentVariable[Variable: String]: String read GetEnvironmentVariable;
    procedure Launch;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDelphiManager = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FInfo: TDelphiManagerInfo;
    function GetInstalledVersions: TDelphiVersions;
    function GetVersions(Version: TDelphiVersion): TDelphiManagerVersion;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Info: TDelphiManagerInfo read FInfo;
    property InstalledVersions: TDelphiVersions read GetInstalledVersions;
    property Versions[Version: TDelphiVersion]: TDelphiManagerVersion read GetVersions;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCryptManager = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FMode: TCryptMode;
    FKey: String;
    FDecrypted: TStrings;
    FEncrypted: TStrings;
    FAutoCrypt: Boolean;
    FWorkEvent: TCryptWorkEvent;
    FKeyChangeEvent: TCryptKeyChangeEvent;
    FKeyChangeQueryEvent: TCryptKeyChangeQueryEvent;
    { Methoden }
    procedure SetMode(Value: TCryptMode);
    procedure SetKey(Value: String);
    function GetDecrypted: TStrings;
    procedure SetDecrypted(Value: TStrings);
    function GetEncrypted: TStrings;
    procedure SetEncrypted(Value: TStrings);
  protected
    { Protected-Deklarationen }
    procedure DecryptedChange(Sender: TObject);
    procedure EncryptedChange(Sender: TObject);
    procedure XorEncrypt;
    procedure XorDecrypt;
    procedure CaesarEncrypt;
    procedure CaesarDecrypt;
    procedure VigenereEncrypt;
    procedure VigenereDecrypt;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Decrypt;
    procedure Encrypt;
    function ValidKey(const S: String): Boolean;
    function GenerateKey: String;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
    property Mode: TCryptMode read FMode write SetMode default cmCustom;
    property Key: String read FKey write SetKey;
    property Decrypted: TStrings read GetDecrypted write SetDecrypted;
    property Encrypted: TStrings read GetEncrypted write SetEncrypted;
    property AutoCrypt: Boolean read FAutoCrypt write FAutoCrypt default True;
    { Ereignisse }
    property OnWork: TCryptWorkEvent read FWorkEvent write FWorkEvent;
    property OnKeyChange: TCryptKeyChangeEvent read FKeyChangeEvent write FKeyChangeEvent;
    property OnKeyChangeQuery: TCryptKeyChangeQueryEvent read FKeyChangeQueryEvent write FKeyChangeQueryEvent;
  end;

  {$IFDEF ADD_COMPONENTREG}
    procedure Register;
  {$ENDIF}

  function NameToDelphiVersion(const Name: String): TDelphiVersion;
  function FullNameToDelphiVersion(const FullName: String): TDelphiVersion;
  function CodeNameToDelphiVersion(const CodeName: String): TDelphiVersion;
  function DelphiVersionToName(const DelphiVersion: TDelphiVersion): String;
  function DelphiVersionToFullName(const DelphiVersion: TDelphiVersion): String;
  function DelphiVersionToCodeName(const DelphiVersion: TDelphiVersion): String;
  function DelphiVersionToProductVersion(const DelphiVersion: TDelphiVersion): Integer;
  function DelphiVersionToPackageVersion(const DelphiVersion: TDelphiVersion): Integer;
  function ProductVersionToDelphiVersion(const ProductVersion: Integer): TDelphiVersion;
  function PackageVersionToDelphiVersion(const PackageVersion: Integer): TDelphiVersion;
  function ProductVersionToPackageVersion(const ProductVersion: Integer): Integer;
  function PackageVersionToProductVersion(const PackageVersion: Integer): Integer;
  function TargetPlatformToName(const TargetPlatform: TDelphiTargetPlatform): String;
  function TargetPlatformToFullName(const TargetPlatform: TDelphiTargetPlatform): String;
  function DelphiVersionReleaseDate(const DelphiVersion: TDelphiVersion): TDate;
  function ValidKey(Mode: TCryptMode; const Key: String): Boolean;
  function GenerateKey(Mode: TCryptMode): String;

const
  { PowerStatus-Meldungen }
  PS_HEALTHY = 'Healthy';
  PS_LOW = 'Low';
  PS_CRITICAL = 'Critical';
  PS_CHARGE = 'Charge';
  PS_HEALTHYACCU = 'HealthyAccu';
  PS_NONE = 'None';
  PS_UNKNOWN = 'Unknown';

implementation

{$IFDEF ADD_COMPONENTREG}
  procedure Register;
  begin
    RegisterComponents({$IFDEF ADD_SINGLECATEGORY}ComponentsPage{$ELSE}ComponentsPage_System{$ENDIF},[TBattery,TCursorFix,TProcessManager,TStringContainer,TDelphiManager,TCryptManager]);
  end;
{$ENDIF}

function NameToDelphiVersion(const Name: String): TDelphiVersion;
const
  VersionNames: array [TDelphiVersion] of String = (
                                                    '7','2005','2006','2007','2009','2010',
                                                    'XE','XE2','XE3','XE4','XE5','XE6','XE7','XE8',
                                                    '10','10.1','10.2'
  );
begin
  Result := TDelphiVersion(ArrayPos(Name,VersionNames));
end;

function FullNameToDelphiVersion(const FullName: String): TDelphiVersion;
const
  VersionFullNames: array [TDelphiVersion] of String = (
                                                        'Delphi 7','Delphi 2005','Delphi 2006','Delphi 2007','Delphi 2009','Delphi 2010',
                                                        'Delphi XE','Delphi XE2','Delphi XE3','Delphi XE4','Delphi XE5','Delphi XE6','Delphi XE7','Delphi XE8',
                                                        'Delphi 10','Delphi 10.1','Delphi 10.2'
  );
begin
  Result := TDelphiVersion(ArrayPos(FullName,VersionFullNames));
end;

function CodeNameToDelphiVersion(const CodeName: String): TDelphiVersion;
const
  VersionCodeNames: array [TDelphiVersion] of String = (
                                                        'Aurora','DiamondBack','DeXter','Spacely','Tiburón','Weaver',
                                                        'Fulcrum','Pulsar','Waterdragon','Quintessence','Zephyr','Proteus','Carpathia','Elbrus',
                                                        'Seattle','Berlin','Tokyo'
  );
begin
  Result := TDelphiVersion(ArrayPos(CodeName,VersionCodeNames));
end;

function DelphiVersionToName(const DelphiVersion: TDelphiVersion): String;
const
  VersionNames: array [TDelphiVersion] of String = (
                                                    '7','2005','2006','2007','2009','2010',
                                                    'XE','XE2','XE3','XE4','XE5','XE6','XE7','XE8',
                                                    '10','10.1','10.2'
  );
begin
  Result := VersionNames[DelphiVersion];
end;

function DelphiVersionToFullName(const DelphiVersion: TDelphiVersion): String;
const
  VersionFullNames: array [TDelphiVersion] of String = (
                                                        'Delphi 7','Delphi 2005','Delphi 2006','Delphi 2007','Delphi 2009','Delphi 2010',
                                                        'Delphi XE','Delphi XE2','Delphi XE3','Delphi XE4','Delphi XE5','Delphi XE6','Delphi XE7','Delphi XE8',
                                                        'Delphi 10','Delphi 10.1','Delphi 10.2'
  );
begin
  Result := VersionFullNames[DelphiVersion];
end;

function DelphiVersionToCodeName(const DelphiVersion: TDelphiVersion): String;
const
  VersionCodeNames: array [TDelphiVersion] of String = (
                                                        'Aurora','DiamondBack','DeXter','Spacely','Tiburón','Weaver',
                                                        'Fulcrum','Pulsar','Waterdragon','Quintessence','Zephyr','Proteus','Carpathia','Elbrus',
                                                        'Seattle','Berlin','Tokyo'
  );
begin
  Result := VersionCodeNames[DelphiVersion];
end;

function DelphiVersionToProductVersion(const DelphiVersion: TDelphiVersion): Integer;
begin
  if DelphiVersion = dv7 then
  begin
    Result := 7;
  end else
  begin
    Result := Integer(DelphiVersion) + 9;
  end;
end;

function DelphiVersionToPackageVersion(const DelphiVersion: TDelphiVersion): Integer;
begin
  Result := DelphiVersionToProductVersion(DelphiVersion) * 10;
end;

function ProductVersionToDelphiVersion(const ProductVersion: Integer): TDelphiVersion;
begin
  Result := TDelphiVersion(ProductVersion - 7);
end;

function PackageVersionToDelphiVersion(const PackageVersion: Integer): TDelphiVersion;
begin
  Result := TDelphiVersion(PackageVersion - 7 div 10)
end;

function ProductVersionToPackageVersion(const ProductVersion: Integer): Integer;
begin
  Result := ProductVersion * 10;
end;

function PackageVersionToProductVersion(const PackageVersion: Integer): Integer;
begin
  Result := PackageVersion div 10;
end;

function TargetPlatformToName(const TargetPlatform: TDelphiTargetPlatform): String;
const
  TargetPlatformNames: array [TDelphiTargetPlatform] of String = (
                                                                  'Win32','Win64',
                                                                  'OSX32',
                                                                  'Android32',
                                                                  'IOSDev','IOSSim','IOSDev32','IOSDev64'
  );
begin
  Result := TargetPlatformNames[TargetPlatform];
end;

function TargetPlatformToFullName(const TargetPlatform: TDelphiTargetPlatform): String;
const
  TargetPlatformNames: array [TDelphiTargetPlatform] of String = (
                                                                  'Windows 32-Bit','Windows 64-Bit',
                                                                  'Mac OS/X 32-Bit',
                                                                  'Android 32-Bit',
                                                                  'iOS','IOS Simulator','IOS 32-Bit','IOS 64-Bit'
  );
begin
  Result := TargetPlatformNames[TargetPlatform];
end;

function DelphiVersionReleaseDate(const DelphiVersion: TDelphiVersion): TDate;
const
  ReleaseDates: array [TDelphiVersion] of TDate = (37477, //09.08.2002 (Delphi 7)
                                                   38272, //12.10.2004 (Delphi 2005)
                                                   38679, //23.11.2005 (Delphi 2006)
                                                   39157, //16.03.2007 (Delphi 2007)
                                                   39685, //25.08.2008 (Delphi 2009)
                                                   40040, //15.08.2009 (Delphi 2010)
                                                   40420, //30.08.2010 (Delphi XE)
                                                   40788, //02.09.2011 (Delphi XE2)
                                                   41155, //03.09.2012 (Delphi XE3)
                                                   41386, //22.04.2013 (Delphi XE4)
                                                   41528, //11.09.2013 (Delphi XE5)
                                                   41744, //15.04.2014 (Delphi XE6)
                                                   41884, //02.09.2014 (Delphi XE7)
                                                   42101, //07.04.2015 (Delphi XE8)
                                                   42247, //31.08.2015 (Delphi 10)
                                                   42480, //20.04.2016 (Delphi 10.1)
                                                   42816  //22.03.2017 (Delphi 10.2)
  );
begin
  Result := ReleaseDates[DelphiVersion];
end;

function ValidKey(Mode: TCryptMode; const Key: String): Boolean;
begin
  if Length(Key) <> 0 then
  begin
    case Mode of
      cmXor: Result := (StrIsInt(Key) and (StrToInt(Key) in [0..31]));
      cmCaesar: Result := ((Length(Key) = 1) and (Key[1] in Letters));
      cmVigenere: Result := ConsistsOf(Key,Letters);
      cmCustom: Result := True;
    end;
  end;
end;

function GenerateKey(Mode: TCryptMode): String;
var
  Current: PChar;
begin
  Randomize;
  case Mode of
    cmXor: Result := IntToStr(Random(32));
    cmCaesar: Result := Chr(Random(Ord('Z') - Ord('A')) + Ord('A'));
    cmVigenere: begin
                  SetLength(Result,1 + Random(15));
                  Current := PChar(Result);
                  while Current^ <> #0 do
                  begin
                    Current^ := Chr(Random(Ord('Z') - Ord('A')) + Ord('A'));
                    Inc(Current);
                  end;
                end;
    cmCustom: Result := '';
  end;
end;

{ ----------------------------------------------------------------------------
  TBattery
  ---------------------------------------------------------------------------- }

function TBattery.BatteryFlag(Flag: Integer): TBatteryFlag;
begin
  case Flag of
    1: Result := bfHealthy;
    2: Result := bfLow;
    4: Result := bfCritical;
    8: Result := bfCharge;
    9: Result := bfHealthyAccu;
    128: Result := bfNone;
    255: Result := bfUnknown;
  else
    begin
      raise EBatteryFlag.Create('Unable to obtain battery flag information');
    end;
  end;
end;

constructor TBattery.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TBattery);
end;

destructor TBattery.Destroy;
begin
  FAbout.Free;
  inherited;
end;

function TBattery.GetPowerStatus: TBatteryStatus;
var
  SysPowerStatus: TSystemPowerStatus;
begin
  GetSystemPowerStatus(SysPowerStatus);
  if Boolean(SysPowerStatus.ACLineStatus) then
  begin
    Result := bsExternal;
  end else
  begin
    Result := bsInternal;
  end;
end;

function TBattery.GetBatteryTime: TDateTime;
var
  SysPowerStatus: TSystemPowerStatus;
begin
  GetSystemPowerStatus(SysPowerStatus);
  Result := SecToTime(SysPowerStatus.BatteryLifeTime);
end;

function TBattery.GetBatteryFlag: TBatteryFlag;
var
  SysPowerStatus: TSystemPowerStatus;
begin
  GetSystemPowerStatus(SysPowerStatus);
  Result := BatteryFlag(SysPowerStatus.BatteryFlag);
end;

function TBattery.GetBatteryFlagReport: String;
begin
  case GetBatteryFlag of
    bfHealthy: Result := PS_HEALTHY;
    bfLow: Result := PS_LOW;
    bfCritical: Result := PS_CRITICAL;
    bfCharge: Result := PS_CHARGE;
    bfHealthyAccu: Result := PS_HEALTHYACCU;
    bfNone: Result := PS_NONE;
    bfUnknown: Result := PS_UNKNOWN;
  end;
end;

function TBattery.GetBatteryPercent: Byte;
var
  SysPowerStatus: TSystemPowerStatus;
begin
  GetSystemPowerStatus(SysPowerStatus);
  Result := SysPowerStatus.BatteryLifePercent;
end;

function TBattery.GetBatteryFullTime: TDateTime;
var
  SysPowerStatus: TSystemPowerStatus;
begin
  GetSystemPowerStatus(SysPowerStatus);
  Result := SecToTime(SysPowerStatus.BatteryFullLifeTime);
end;

{ ----------------------------------------------------------------------------
  TCursorFix
  ---------------------------------------------------------------------------- }

constructor TCursorFix.Create(AOwner: TComponent);
{$IFDEF NO_CURSOR}
  var
    CursorHandle: THandle;
{$ENDIF}
begin
  inherited;
  FAbout := TComponentAbout.Create(TCursorFix);
  {$IFDEF NO_CURSOR}
    CursorHandle := Screen.Cursors[crHandPoint];
    Screen.Cursors[crHandPoint] := LoadCursor(0,IDC_HAND);
    DestroyCursor(CursorHandle);
  {$ELSE}
    {$MESSAGE WARN 'TCursorFix component has been created but not initialized due to the current IDE version'}
    OutputDebugString('TCursorFix component has been created but not initialized due to the current IDE version');
  {$ENDIF}
end;


destructor TCursorFix.Destroy;
{$IFDEF NO_CURSOR}
  var
    CursorHandle: THandle;
{$ENDIF}
begin
  FAbout.Free;
  {$IFDEF NO_CURSOR}
    CursorHandle := Screen.Cursors[crHandPoint];
    Screen.Cursors[crHandPoint] := LoadCursor(0,IDC_HANDPT);
    DestroyCursor(CursorHandle);
  {$ENDIF}
  inherited;
end;

{ ----------------------------------------------------------------------------
  TProcessManager
  ---------------------------------------------------------------------------- }

constructor TProcessManager.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TProcessManager);
  FNames := TStringList.Create;
  TimerObject := TTimer.Create(Self);
  FTimeOut := 0;
  FRefreshMode := prNone;
  TimerObject.OnTimer := TimerObjectTimer;
end;

destructor TProcessManager.Destroy;
begin
  FAbout.Free;
  FNames.Free;
  inherited;
end;

function TProcessManager.GetNames: TStrings;
begin
  if FRefreshMode = prAccess then
  begin
    Update;
  end;
  Result := FNames;
end;

procedure TProcessManager.SetRefreshMode(Value: TProcessRefreshMode);
begin
  FRefreshMode := Value;
  TimerObject.Enabled := (Value = prTime);
end;

procedure TProcessManager.Update;
var
  OldNames: String;
  Snapshot: THandle;
  ProcEntry: TProcessEntry32;
begin
  OldNames := FNames.Text;
  FNames.Clear;
  Snapshot := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
  try
    ProcEntry.dwSize := SizeOf(ProcEntry);
    if Process32First(Snapshot,ProcEntry) then
    begin
      repeat
        FNames.Add(ProcEntry.szExeFile);
      until not Process32Next(Snapshot,ProcEntry)
    end else
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(Snapshot);
  end;
  if Assigned(OnUpdate) then
  begin
    OnUpdate(Self,OldNames = FNames.Text);
  end;
end;

function TProcessManager.GetInterval: Cardinal;
begin
  Result := TimerObject.Interval;
end;

procedure TProcessManager.SetInterval(Value: Cardinal);
begin
  TimerObject.Interval := Value;
end;

procedure TProcessManager.TimerObjectTimer(Sender: TObject);
begin
  Update;
end;

procedure TProcessManager.Kill(ProcID: DWORD);
var
  CurrentProc: THandle;
  Error: DWORD;
begin
  CurrentProc := OpenProcess(SYNCHRONIZE or PROCESS_TERMINATE,False,ProcID);
  try
    if CurrentProc > 0 then
    begin
      Error := Integer(TerminateProcess(CurrentProc,1));
      if Error <> 0 then
      begin
        Error := WaitForSingleObject(CurrentProc,FTimeOut);
        if Error = WAIT_FAILED then
        begin
          RaiseLastOSError;
        end;
      end else
      begin
        RaiseLastOSError;
      end;
    end else
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(CurrentProc);
  end;
end;

procedure TProcessManager.Kill(ProcName: String);
begin
  Kill(GetID(ProcName));
end;

procedure TProcessManager.Start(FileName: String);
begin
  ExecuteFile(FileName);
end;

function TProcessManager.GetID(ProcName: String): DWORD;
var
  Snapshot: THandle;
  ProcEntry: TProcessEntry32;
begin
  Result := 0;
  Snapshot := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
  try
    ProcEntry.dwSize := SizeOf(ProcEntry);
    if Process32First(Snapshot,ProcEntry) then
    begin
      repeat
        if Pos(AnsiLowerCase(ProcEntry.szExeFile),AnsiLowerCase(ExtractFilename(ProcName))) > 0 then
        begin
          Result := ProcEntry.th32ProcessID;
          Break;
        end;
      until not Process32Next(Snapshot,ProcEntry)
    end else
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(Snapshot);
  end;
end;

function TProcessManager.GetName(ProcID: DWORD): String;
var
  Snapshot: THandle;
  ProcEntry: TProcessEntry32;
begin
  Result := '';
  Snapshot := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
  try
    ProcEntry.dwSize := SizeOf(ProcEntry);
    if Process32First(Snapshot,ProcEntry) then
    begin
      repeat
        if ProcEntry.th32ProcessID = ProcID then
        begin
          Result := ProcEntry.szExeFile;
          Break;
        end;
      until not Process32Next(Snapshot,ProcEntry)
    end else
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(Snapshot);
  end;
end;

function TProcessManager.GetPath(ProcID: DWORD): String;
var
  Snapshot: THandle;
  ModEntry: TModuleEntry32;
begin
  Result := '';
  Snapshot := CreateToolHelp32Snapshot(TH32CS_SNAPMODULE,ProcID);
  try
    ModEntry.dwSize := SizeOf(ModEntry);
    if Module32First(Snapshot,ModEntry) then
    begin
      Result := ModEntry.szExePath;
    end else
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(Snapshot);
  end;
end;

function TProcessManager.GetPath(ProcName: String): String;
begin
  Result := GetPath(GetID(ProcName));
end;

function TProcessManager.GetThreads(ProcName: String): DWORD;
var
  Snapshot: THandle;
  ProcEntry: TProcessEntry32;
begin
  Result := 0;
  Snapshot := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
  try
    ProcEntry.dwSize := SizeOf(ProcEntry);
    if Process32First(Snapshot,ProcEntry) then
    begin
      repeat
        if Pos(AnsiLowerCase(ProcEntry.szExeFile),AnsiLowerCase(ExtractFilename(ProcName))) > 0 then
        begin
          Result := ProcEntry.cntThreads;
          Break;
        end;
      until not Process32Next(Snapshot,ProcEntry)
    end else
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(Snapshot);
  end;
end;

function TProcessManager.GetParentID(ProcName: String): DWORD;
var
  Snapshot: THandle;
  ProcEntry: TProcessEntry32;
begin
  Result := 0;
  Snapshot := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
  try
    ProcEntry.dwSize := SizeOf(ProcEntry);
    if Process32First(Snapshot,ProcEntry) then
    begin
      repeat
        if Pos(AnsiLowerCase(ProcEntry.szExeFile),AnsiLowerCase(ExtractFilename(ProcName))) > 0 then
        begin
          Result := ProcEntry.th32ParentProcessID;
          Break;
        end;
      until not Process32Next(Snapshot,ProcEntry)
    end else
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(Snapshot);
  end;
end;

function TProcessManager.GetPriority(ProcName: String): Integer;
var
  Snapshot: THandle;
  ProcEntry: TProcessEntry32;
begin
  Result := -1;
  Snapshot := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS,0);
  try
    ProcEntry.dwSize := SizeOf(ProcEntry);
    if Process32First(Snapshot,ProcEntry) then
    begin
      repeat
        if Pos(AnsiLowerCase(ProcEntry.szExeFile),AnsiLowerCase(ExtractFilename(ProcName))) > 0 then
        begin
          Result := ProcEntry.pcPriClassBase;
          Break;
        end;
      until not Process32Next(Snapshot,ProcEntry);
    end else
    begin
      RaiseLastOSError;
    end;
  finally
    CloseHandle(Snapshot);
  end;
end;

function TProcessManager.GetMemory(ProcID: DWORD): DWORD;
var
  ProcMem: TProcessMemoryCounters;
begin
  Result := 0;
  ProcMem.cb := SizeOf(ProcMem);
  if GetProcessMemoryInfo(OpenProcess(PROCESS_QUERY_INFORMATION,False,ProcID),@ProcMem,SizeOf(ProcMem)) then
  begin
    Result := ProcMem.WorkingSetSize;
  end else
  begin
    RaiseLastOSError;
  end;
end;

function TProcessManager.GetMemory(ProcName: String): DWORD;
begin
  Result := GetMemory(GetID(ProcName));
end;

function TProcessManager.GetUser(ProcID: DWORD): String;
var
  Process: THandle;
  Token: THandle;
  Buffer: DWORD;
  User: PTOKEN_USER;
  NameUse: SID_NAME_USE;
  Success: Boolean;
  Size: DWORD;
  DomainBuffer: String;
begin
  Result := '';
  Process := OpenProcess(PROCESS_QUERY_INFORMATION,False,ProcID);
  if Process <> 0 then
  begin
    if OpenProcessToken(Process,TOKEN_QUERY,Token) then
    begin
      Success := GetTokenInformation(Token,TokenUser,nil,0,Buffer);
      User := nil;
      while (not Success) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) do
      begin
        ReallocMem(User,Buffer);
        Success := GetTokenInformation(Token,TokenUser,User,Buffer,Buffer);
      end;
      CloseHandle(Token);
      if not Success then
      begin
        RaiseLastOSError;
      end;
      LookupAccountSid(nil,User.User.Sid,nil,Size,nil,Buffer,NameUse);
      if Size <> 0 then
      begin
        SetLength(Result,Size);
        SetLength(DomainBuffer,Buffer);
        LookupAccountSid(nil,User.User.Sid,PChar(Result),Size,PChar(DomainBuffer),Buffer,NameUse);
      end;
      FreeMem(User);
    end else
    begin
      RaiseLastOSError;
    end;
    CloseHandle(Process);
  end else
  begin
    RaiseLastOSError;
  end;
end;

function TProcessManager.GetUser(ProcName: String): String;
begin
  Result := GetUser(GetID(ProcName));
end;

function TProcessManager.GetDomain(ProcID: DWORD): String;
var
  Process: THandle;
  Token: THandle;
  Buffer: DWORD;
  User: PTOKEN_USER;
  NameUse: SID_NAME_USE;
  Success: Boolean;
  Size: DWORD;
  UserBuffer: String;
begin
  Result := '';
  Process := OpenProcess(PROCESS_QUERY_INFORMATION,False,ProcID);
  if Process <> 0 then
  begin
    if OpenProcessToken(Process,TOKEN_QUERY,Token) then
    begin
      Success := GetTokenInformation(Token,TokenUser,nil,0,Buffer);
      User := nil;
      while (not Success) and (GetLastError = ERROR_INSUFFICIENT_BUFFER) do
      begin
        ReallocMem(User,Buffer);
        Success := GetTokenInformation(Token,TokenUser,User,Buffer,Buffer);
      end;
      CloseHandle(Token);
      if not Success then
      begin
        RaiseLastOSError;
      end;
      LookupAccountSid(nil,User.User.Sid,nil,Buffer,nil,Size,NameUse);
      if Size <> 0 then
      begin
        SetLength(Result,Size);
        SetLength(UserBuffer,Buffer);
        LookupAccountSid(nil,User.User.Sid,PChar(UserBuffer),Buffer,PChar(Result),Size,NameUse);
      end;
      FreeMem(User);
    end else
    begin
      RaiseLastOSError;
    end;
    CloseHandle(Process);
  end else
  begin
    RaiseLastOSError;
  end;
end;

function TProcessManager.GetDomain(ProcName: String): String;
begin
  Result := GetDomain(GetID(ProcName));
end;

{ ----------------------------------------------------------------------------
  TStringContainer
  ---------------------------------------------------------------------------- }

constructor TStringContainer.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TStringContainer);
  FItems := TStringList.Create;
  (FItems as TStringList).OnChange := ItemsChange;
  (FItems as TStringList).OnChanging := ItemsChanging;
end;

destructor TStringContainer.Destroy;
begin
  FAbout.Free;
  FItems.Free;
  inherited;
end;

function TStringContainer.GetItems: TStrings;
begin
  Result := FItems;
end;

procedure TStringContainer.SetItems(Value: TStrings);
begin
  (FItems as TStringList).Assign(Value);
end;

procedure TStringContainer.ItemsChange(Sender: TObject);
begin
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TStringContainer.ItemsChanging(Sender: TObject);
begin
  if Assigned(OnChanging) then
  begin
    OnChanging(Self);
  end;
end;

{ ----------------------------------------------------------------------------
  TDelphiManagerCurrentVersion
  ---------------------------------------------------------------------------- }

constructor TDelphiManagerInfo.Create(ADelphiManager: TDelphiManager);
begin
  inherited Create;
  FDelphiManager := ADelphiManager;
end;

destructor TDelphiManagerInfo.Destroy;
begin
  //...
  inherited;
end;

function TDelphiManagerInfo.GetVersion: TDelphiVersion;
begin
  {$IFDEF VER150}
    Result := dv7;
  {$ELSE}
    {$IFDEF VER170}
      Result := dv2005;
    {$ELSE}
      {$IFDEF VER180}
        {$IFNDEF VER185}
          Result := dv2006;
        {$ELSE}
          Result := dv2007;
        {$ENDIF}
      {$ELSE}
        {$IFDEF VER200}
          Result := dv2009;
        {$ELSE}
          {$IFDEF VER210}
            Result := dv2010;
          {$ELSE}
            {$IFDEF VER220}
              Result := dvXE;
            {$ELSE}
              {$IFDEF VER230}
                Result := dvXE2;
              {$ELSE}
                {$IFDEF VER240}
                  Result := dvXE3;
                {$ELSE}
                  {$IFDEF VER250}
                    Result := dvXE4;
                  {$ELSE}
                    {$IFDEF VER260}
                      Result := dvXE5;
                    {$ELSE}
                      {$IFDEF VER270}
                        Result := dvXE6;
                      {$ELSE}
                        {$IFDEF VER280}
                          Result := dvXE7;
                        {$ELSE}
                          {$IFDEF VER290}
                            Result := dvXE8;
                          {$ELSE}
                            {$IFDEF VER300}
                              Result := dv10;
                            {$ELSE}
                              {$IFDEF VER310}
                                Result := dv101;
                              {$ELSE}
                                {$IFDEF VER320}
                                  Result := dv102;
                                {$ELSE}
                                  raise EDelphiVersion.Create('Delphi version could not be determined');
                                {$ENDIF}
                              {$ENDIF}
                            {$ENDIF}
                          {$ENDIF}
                        {$ENDIF}
                      {$ENDIF}
                    {$ENDIF}
                  {$ENDIF}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
end;

function TDelphiManagerInfo.GetVersionName: String;
begin
  Result := DelphiVersionToName(Version);
end;

function TDelphiManagerInfo.GetVersionFullName: String;
begin
  Result := DelphiVersionToFullName(Version);
end;

function TDelphiManagerInfo.GetVersionCodeName: String;
begin
  Result := DelphiVersionToCodeName(Version);
end;

function TDelphiManagerInfo.GetCompilerVersion: Extended;
begin
  Result := System.CompilerVersion;
end;

function TDelphiManagerInfo.GetRuntimeVersion: Extended;
begin
  Result := RTLVersion;
end;

function TDelphiManagerInfo.GetLinaVersion: Extended;
begin
  Result := uBase.LinaVersion;
end;

function TDelphiManagerInfo.GetProductVersion: Integer;
begin
  Result := DelphiVersionToProductVersion(Version);
end;

function TDelphiManagerInfo.GetPackageVersion: Integer;
begin
  Result := DelphiVersionToPackageVersion(Version);
end;

function TDelphiManagerInfo.GetCompilerOptions: TDelphiCompilerOptions;
begin
  Result := []
               {$IFDEF DEBUG}                  + [dcmDebug]     {$ENDIF}
               {$IFOPT M+}                     + [dcmTypeInfo]  {$ENDIF}
               {$IFDEF CONSOLE}                + [dcmConsole]   {$ENDIF}
               {$IFDEF UNICODE}                + [dcmUnicode]   {$ENDIF}
               {$IFOPT X+}                     + [dcmExtSyntax] {$ENDIF}
               {$IFDEF ASSEMBLER}              + [dcmAssembler] {$ENDIF}
               {$IFDEF CONDITIONALEXPRESSIONS} + [dcmCondExpr]  {$ENDIF}
  ;
end;

function TDelphiManagerInfo.GetPlatform: TDelphiLocalPlatform;
begin
  {$IFDEF WIN32}
    Result := dpWin32;
  {$ENDIF}
  {$IFDEF WIN64}
    Result := dpWin64;
  {$ENDIF}
end;

function TDelphiManagerInfo.GetPlatformName: String;
begin
  Result := TargetPlatformToName(Platform);
end;

function TDelphiManagerInfo.GetPlatformFullName: String;
begin
  Result := TargetPlatformToFullName(Platform);
end;

{ ----------------------------------------------------------------------------
  TDelphiManagerVersionPath
  ---------------------------------------------------------------------------- }

constructor TDelphiManagerVersionPath.Create(Collection: TCollection);
begin
  inherited;
  //...
end;

destructor TDelphiManagerVersionPath.Destroy;
begin
  //...
  inherited;
end;

function TDelphiManagerVersionPath.GetDisplayName: String;
begin
  inherited;
  Result := Name;
end;

{ ----------------------------------------------------------------------------
  TDelphiManagerVersionPackage
  ---------------------------------------------------------------------------- }

constructor TDelphiManagerVersionPackage.Create(Collection: TCollection);
begin
  inherited;
  //...
end;

destructor TDelphiManagerVersionPackage.Destroy;
begin
  //...
  inherited;
end;

function TDelphiManagerVersionPackage.GetDisplayName: String;
begin
  inherited;
  Result := ExtractFileName(FileName);
end;

{ ----------------------------------------------------------------------------
  TDelphiManagerVersion
  ---------------------------------------------------------------------------- }

constructor TDelphiManagerVersion.Create(AVersion: TDelphiVersion);
begin
  inherited Create;
  FVersion := AVersion;
  FPackages := TDelphiManagerVersionPackages.Create(TDelphiManagerVersionPackage);
  FIdePackages := TDelphiManagerVersionPackages.Create(TDelphiManagerVersionPackage);
end;

destructor TDelphiManagerVersion.Destroy;
begin
  FPackages.Free;
  FIdePackages.Free;
  inherited;
end;

function TDelphiManagerVersion.GetInstalled: Boolean;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    try
      Result := Reg.KeyExists(RegistryPath) and FileExists(ExeName);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TDelphiManagerVersion.GetEdition: TDelphiEdition;
const
  RegEntry = 'Edition';
  Editions: array [TDelphiEdition] of String = (
                                                'Personal',
                                                'Professional',
                                                'Enterprise',
                                                'Ultimate',
                                                'Architect'
  );
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath)) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKeyReadOnly(RegistryPath);
      Result := TDelphiEdition(ArrayPos(Reg.ReadString(RegEntry),Editions));
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TDelphiManagerVersion.GetRootDir: String;
const
  RegEntry = 'RootDir';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath)) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKeyReadOnly(RegistryPath);
      Result := Reg.ReadString(RegEntry);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TDelphiManagerVersion.GetExeName: String;
const
  RegEntry = 'App';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath)) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKeyReadOnly(RegistryPath);
      Result := Reg.ReadString(RegEntry)
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TDelphiManagerVersion.GetLanguage: String;
const
  RegEntry = 'InstallLanguage';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath)) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKeyReadOnly(RegistryPath);
      Result := Reg.ReadString(RegEntry);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TDelphiManagerVersion.SetLanguage(Value: String);
const
  RegEntry = 'InstallLanguage';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath)) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKey(RegistryPath,True);
      Reg.WriteString(RegEntry,Value);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TDelphiManagerVersion.GetTargetPlatforms: TDelphiTargetPlatforms;
begin
  Result := [dpWin32];
  if Version >= dvXE2 then
  begin
    Result := Result + [dpWin64,dpOSX32];
    if Version <> dvXE3 then
    begin
      Result := Result + [dpIOSSim];
      if Version < dvXE8 then
      begin
        Result := Result + [dpIOSDev];
      end else
      begin
        Result := Result + [dpIOSDev32,dpIOSDev64];
      end;
      if Version >= dvXE5 then
      begin
        Result := Result + [dpAndroid32];
      end;
    end;
  end;
end;

function TDelphiManagerVersion.GetBrowsingPaths(TargetPlatform: TDelphiTargetPlatform): TStrings;
const
  RegSubKey = 'Library';
  RegEntry = 'Browsing Path';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath + PathDelim + RegSubKey + PathDelim + TargetPlatformKey(TargetPlatform))) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKeyReadOnly(RegistryPath + PathDelim + RegSubKey + PathDelim + TargetPlatformKey(TargetPlatform));
      Result.StrictDelimiter := True;
      Result.Delimiter := ';';
      Result.DelimitedText := Reg.ReadString(RegEntry);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TDelphiManagerVersion.SetBrowsingPaths(TargetPlatform: TDelphiTargetPlatform; Value: TStrings);
const
  RegSubKey = 'Library';
  RegEntry = 'Browsing Path';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath + PathDelim + RegSubKey + PathDelim + TargetPlatformKey(TargetPlatform))) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKey(RegistryPath + PathDelim + RegSubKey + PathDelim + TargetPlatformKey(TargetPlatform),True);
      Value.StrictDelimiter := True;
      Value.Delimiter := ';';
      Reg.WriteString(RegEntry,Value.DelimitedText);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TDelphiManagerVersion.GetSearchPaths(TargetPlatform: TDelphiTargetPlatform): TStrings;
const
  RegSubKey = 'Library';
  RegEntry = 'Search Path';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath + PathDelim + RegSubKey + PathDelim + TargetPlatformKey(TargetPlatform))) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKeyReadOnly(RegistryPath + PathDelim + RegSubKey + PathDelim + TargetPlatformKey(TargetPlatform));
      Result.StrictDelimiter := True;
      Result.Delimiter := ';';
      Result.DelimitedText := Reg.ReadString(RegEntry);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TDelphiManagerVersion.SetSearchPaths(TargetPlatform: TDelphiTargetPlatform; Value: TStrings);
const
  RegSubKey = 'Library';
  RegEntry = 'Search Path';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath + PathDelim + RegSubKey + PathDelim + TargetPlatformKey(TargetPlatform))) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKey(RegistryPath + PathDelim + RegSubKey + PathDelim + TargetPlatformKey(TargetPlatform),True);
      Value.StrictDelimiter := True;
      Value.Delimiter := ';';
      Reg.WriteString(RegEntry,Value.DelimitedText);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TDelphiManagerVersion.GetEnvironmentVariables: TStrings;
const
  RegSubKey = 'Environment Variables';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath + PathDelim + RegSubKey)) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKeyReadOnly(RegistryPath + PathDelim + RegSubKey);
      Reg.GetValueNames(Result);
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TDelphiManagerVersion.GetEnvironmentVariable(Variable: String): String;
const
  RegSubKey = 'Environment Variables';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if (not Reg.KeyExists(RegistryPath + PathDelim + RegSubKey)) then
    begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;
    end;
    try
      Reg.OpenKeyReadOnly(RegistryPath + PathDelim + RegSubKey);
      if Reg.ValueExists(Variable) then
      begin
        Result := Reg.ReadString(Variable);
      end else
      begin
        Result := '';
      end;
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TDelphiManagerVersion.RegistryPath: String;
const
  RegistryPaths: array [TDelphiVersion] of String = (
                                                    'Software\Borland\Delphi\7.0',
                                                    'Software\Borland\BDS\3.0',
                                                    'Software\Borland\BDS\4.0',
                                                    'Software\Borland\BDS\5.0',
                                                    'Software\CodeGear\BDS\6.0',
                                                    'Software\CodeGear\BDS\7.0',
                                                    'Software\Embarcadero\BDS\8.0',
                                                    'Software\Embarcadero\BDS\9.0',
                                                    'Software\Embarcadero\BDS\10.0',
                                                    'Software\Embarcadero\BDS\11.0',
                                                    'Software\Embarcadero\BDS\12.0',
                                                    'Software\Embarcadero\BDS\14.0',
                                                    'Software\Embarcadero\BDS\15.0',
                                                    'Software\Embarcadero\BDS\16.0',
                                                    'Software\Embarcadero\BDS\17.0',
                                                    'Software\Embarcadero\BDS\18.0',
                                                    'Software\Embarcadero\BDS\19.0'
  );
begin
  Result := RegistryPaths[Version];
end;

function TDelphiManagerVersion.TargetPlatformKey(TargetPlatform: TDelphiTargetPlatform): String;
const
  TargetPlatforms: array [TDelphiTargetPlatform] of String = (
                                                              'Win32',
                                                              'Win64',
                                                              'OSX32',
                                                              'Android32',
                                                              'iOSDevice',
                                                              'iOSSimulator',
                                                              'iOSDevice32',
                                                              'iOSDevice64'
  );
begin
  Result := TargetPlatforms[TargetPlatform];
end;

procedure TDelphiManagerVersion.Launch;
begin
  ExecuteFile(ExeName);
end;

{ ----------------------------------------------------------------------------
  TDelphiManager
  ---------------------------------------------------------------------------- }

constructor TDelphiManager.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TDelphiManager);
  FInfo := TDelphiManagerInfo.Create(Self);
end;

destructor TDelphiManager.Destroy;
begin
  FAbout.Free;
  FInfo.Free;
  inherited;
end;

function TDelphiManager.GetInstalledVersions: TDelphiVersions;
var
  Index: TDelphiVersion;
begin
  Result := [];
  for Index := Low(TDelphiVersion) to High(TDelphiVersion) do
  begin
    if Versions[Index].Installed then
    begin
      Result := Result + [Index];
    end;
  end;
end;

function TDelphiManager.GetVersions(Version: TDelphiVersion): TDelphiManagerVersion;
begin
  Result := TDelphiManagerVersion.Create(Version);
end;

{ ----------------------------------------------------------------------------
  TCryptManager
  ---------------------------------------------------------------------------- }

constructor TCryptManager.Create;
begin
  inherited;
  FMode := cmCustom;
  FAutoCrypt := True;
  FAbout := TComponentAbout.Create(TCryptManager);
  FDecrypted := TStringList.Create;
  (FDecrypted as TStringList).OnChange := DecryptedChange;
  FEncrypted := TStringList.Create;
  (FEncrypted as TStringList).OnChange := EncryptedChange;
end;

destructor TCryptManager.Destroy;
begin
  FAbout.Free;
  FDecrypted.Free;
  FEncrypted.Free;
  inherited;
end;

procedure TCryptManager.SetMode(Value: TCryptMode);
begin
  FMode := Value;
  Key := '';
end;

procedure TCryptManager.SetKey(Value: String);
var
  CanChange: Boolean;
begin
  if Length(Value) <> 0 then
  begin
    case Mode of
      cmXor: if not (StrIsInt(Value) and (StrToInt(Value) in [0..31])) then
             begin
               raise EInvalidKey.Create('Invalid Xor crypt key value');
             end;
      cmCaesar: begin
                  Value[1] := CharUpperCase(Value[1]);
                  if not ((Length(Value) = 1) and (Value[1] in UpperCaseLetters)) then
                  begin
                    raise EInvalidKey.Create('Invalid Caesar crypt key value');
                  end;
                end;
      cmVigenere: begin
                    Value := UpperCase(Value);
                    if not ConsistsOf(Value,UpperCaseLetters) then
                      begin
                      raise EInvalidKey.Create('Invalid Vigenere crypt key value');
                    end;
                  end;
    end;
  end;
  CanChange := True;
  if Assigned(OnKeyChange) then
  begin
    OnKeyChangeQuery(Self,Value,CanChange);
  end;
  if CanChange then
  begin
    FKey := Value;
    if Assigned(OnKeyChange) then
    begin
      OnKeyChange(Self);
    end;
  end;
end;

function TCryptManager.GetDecrypted: TStrings;
begin
  Result := FDecrypted;
end;

procedure TCryptManager.SetDecrypted(Value: TStrings);
begin
  (FDecrypted as TStringList).Assign(Value);
  if AutoCrypt then
  begin
    Encrypt;
  end;
end;

function TCryptManager.GetEncrypted: TStrings;
begin
  Result := FEncrypted;
end;

procedure TCryptManager.SetEncrypted(Value: TStrings);
begin
  (FEncrypted as TStringList).Assign(Value);
  if AutoCrypt then
  begin
    Decrypt;
  end;
end;

procedure TCryptManager.DecryptedChange(Sender: TObject);
begin
  if AutoCrypt then
  begin
    Encrypt;
  end;
end;

procedure TCryptManager.EncryptedChange(Sender: TObject);
begin
  if AutoCrypt then
  begin
    Decrypt;
  end;
end;

procedure TCryptManager.XorEncrypt;
var
  Index: Integer;
  Line: String;
  Current: PChar;
  Delta: Integer;
begin
  Delta := StrToInt(Key);
  Encrypted.Clear;
  for Index := 0 to Decrypted.Count - 1 do
  begin
    Line := Decrypted.Strings[Index];
    Current := PChar(Line);
    while Current^ <> #0 do
    begin
      Current^ := Chr(Ord(Current^) xor Delta);
      Inc(Current);
    end;
    Encrypted.Add(Line);
  end;
end;

procedure TCryptManager.XorDecrypt;
var
  Index: Integer;
  Line: String;
  Current: PChar;
  Delta: Integer;
begin
  Delta := StrToInt(Key);
  Decrypted.Clear;
  for Index := 0 to Encrypted.Count - 1 do
  begin
    Line := Encrypted.Strings[Index];
    Current := PChar(Line);
    while Current^ <> #0 do
    begin
      if Current^ > #31 then
      begin
        Current^ := Chr(Ord(Current^) xor Delta);
      end;
      Inc(Current);
    end;
    Decrypted.Add(Line);
  end;
end;

procedure TCryptManager.CaesarEncrypt;
var
  Index: Integer;
  Line: String;
  Current: PChar;
  Delta: Byte;
begin
  Delta := Ord(Key[1]) - Ord('A');
  Encrypted.Clear;
  for Index := 0 to Decrypted.Count - 1 do
  begin
    Line := Decrypted.Strings[Index];
    Current := PChar(Line);
    while Current^ <> #0 do
    begin
      Current^ := Chr(Ord(Current^) + Delta);
      Inc(Current);
    end;
    Encrypted.Add(Line);
  end;
end;

procedure TCryptManager.CaesarDecrypt;
var
  Index: Integer;
  Line: String;
  Current: PChar;
  Delta: Byte;
begin
  Delta := Ord(Key[1]) - Ord('A');
  Decrypted.Clear;
  for Index := 0 to Encrypted.Count - 1 do
  begin
    Line := Encrypted.Strings[Index];
    Current := PChar(Line);
    while Current^ <> #0 do
    begin
      Current^ := Chr(Ord(Current^) - Delta);
      Inc(Current);
    end;
    Decrypted.Add(Line);
  end;
end;

procedure TCryptManager.VigenereEncrypt;
var
  Index: Integer;
  Line: String;
  Current: PChar;
  CurrentKey: PChar;
begin
  CurrentKey := PChar(Key);
  Encrypted.Clear;
  for Index := 0 to Decrypted.Count - 1 do
  begin
    Line := Decrypted.Strings[Index];
    Current := PChar(Line);
    while Current^ <> #0 do
    begin
      Current^ := Chr(Ord(Current^) + Ord(CurrentKey^) - Ord('A'));
      Inc(CurrentKey);
      if CurrentKey^ = #0 then
      begin
        CurrentKey := PChar(Key);
      end;
      Inc(Current);
    end;
    Encrypted.Add(Line);
  end;
end;

procedure TCryptManager.VigenereDecrypt;
var
  Index: Integer;
  Line: String;
  Current: PChar;
  CurrentKey: PChar;
begin
  CurrentKey := PChar(Key);
  Decrypted.Clear;
  for Index := 0 to Encrypted.Count - 1 do
  begin
    Line := Encrypted.Strings[Index];
    Current := PChar(Line);
    while Current^ <> #0 do
    begin
      Current^ := Chr(Ord(Current^) - Ord(CurrentKey^) + Ord('A'));
      Inc(CurrentKey);
      if CurrentKey^ = #0 then
      begin
        CurrentKey := PChar(Key);
      end;
      Inc(Current);
    end;
    Decrypted.Add(Line);
  end;
end;

procedure TCryptManager.Decrypt;
var
  OldAutoCrypt: Boolean;
begin
  if AutoCrypt then
  begin
    OldAutoCrypt := True;
    AutoCrypt := False;
  end else
  begin
    OldAutoCrypt := False;
  end;
  if Length(Key) <> 0 then
  begin
    case Mode of
      cmXor: XorDecrypt;
      cmCaesar: CaesarDecrypt;
      cmVigenere: VigenereDecrypt;
    end;
  end;
  if Assigned(OnWork) then
  begin
    OnWork(Self);
  end;
  if OldAutoCrypt then
  begin
    AutoCrypt := True;
  end;
end;

procedure TCryptManager.Encrypt;
var
  OldAutoCrypt: Boolean;
begin
  if AutoCrypt then
  begin
    OldAutoCrypt := True;
    AutoCrypt := False;
  end else
  begin
    OldAutoCrypt := False;
  end;
  if Length(Key) <> 0 then
  begin
    case Mode of
      cmXor: XorEncrypt;
      cmCaesar: CaesarEncrypt;
      cmVigenere: VigenereEncrypt;
    end;
  end;
  if Assigned(OnWork) then
  begin
    OnWork(Self);
  end;
  if OldAutoCrypt then
  begin
    AutoCrypt := True;
  end;
end;

function TCryptManager.ValidKey(const S: String): Boolean;
begin
  Result := uSysCtrls.ValidKey(Mode,S);
end;

function TCryptManager.GenerateKey: String;
begin
  Result := uSysCtrls.GenerateKey(Mode);
end;

end.
