unit uScriptMgr;

//////////////////////////////////////
///  Lina Script Manager Unit      ///
///  ****************************  ///
///  (c) 2017 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

  { Wegen Kompatibilitätsgründen mussten einige AnsiString-Werte eingebaut
    werden, da die PascalScript-Engine von RemObjects noch für ältere Versionen
    von Delphi ausgelegt war. Damit TScriptManager auch unter neueren Delphi-
    Versionen kompiliert werden kann, müssen die AnsiString-Werte so belassen
    werden, da es sonst zu Fehlern kommt. }

uses
  { Standard-Units }
  Classes, SysUtils, Dialogs, Forms,
  { PS Komponenten-Units }
  uPSCompiler, uPSRuntime, uPSComponent,
  { PS Syntax-Libraries }
  uPSComponent_Default, uPSComponent_COM, uPSComponent_Controls,
  uPSComponent_Forms, uPSComponent_StdCtrls, uPSComponent_DB,
  { Andere Package-Units }
  uBase, uFileTools;

type
  { Fehlermeldungen }
  EMissingReturnTarget = class(Exception);
  EMissingComponentName = class(Exception);
  EInvalidCodeLine = class(Exception);

  { Hilfsklassen }
  TScriptReturnMode = (srNone,srAll,srErrors);
  TScriptReturnStyle = (srSimple,srTime,srDateTime,srName);
  TScriptLibraries = set of (slClasses,slControls,slStdCtrls,slForms,slDateUtils,slComObj,slDB,slCustom);

  { Ereignisse }
  TScriptManagerCreateEvent = procedure(Sender: TObject) of object;
  TScriptManagerDestroyEvent = procedure(Sender: TObject) of object;
  TScriptManagerReturnEvent = procedure(Sender: TObject; const Msg: String) of object;
  TScriptManagerNeedFileEvent = function(Sender:TObject; const OriginFileName: AnsiString; var FileName, Output: AnsiString): Boolean of object;
  TScriptManagerCompileEvent = procedure(Sender: TObject) of object;
  TScriptManagerExecuteEvent = procedure(Sender: TObject) of object;
  TScriptManagerAfterExecuteEvent = procedure(Sender: TObject) of object;

  { Hauptklassen }
  TScriptManager = class;

  TScriptManagerPlugins = class(TPersistent)
  private
    { Private-Deklarationen }
    FScriptManager: TScriptManager;
    { Methoden }
    function GetScriptManager: TScriptManager;
    procedure SetScriptManager(Value: TScriptManager);
    function GetItems(Index: Integer): TPSPlugin;
    procedure SetItems(Index: Integer; Value: TPSPlugin);
    function GetCount: Integer;
  public
    { Public-Deklarationen }
    constructor Create(AScriptManager: TScriptManager);
    destructor Destroy; override;
    property ScriptManager: TScriptManager read GetScriptManager write SetScriptManager;
    property Items[Index: Integer]: TPSPlugin read GetItems write SetItems;
    property Count: Integer read GetCount;
    procedure Add(Plugin: TPSPlugin);
    procedure Delete(Plugin: TPSPlugin);
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TScriptManager = class(TComponent)
  private
    { Private-Deklarationen }
    ScriptEngine: TPSScript;
    Log: TStrings;
    CustomPlugins: TPSPlugIns;
    FAbout: TComponentAbout;
    FReturnMode: TScriptReturnMode;
    FReturnStyle: TScriptReturnStyle;
    FReturnTarget: TStrings;
    FSecureMode: Boolean;
    FVarApplication: TApplication;
    FVarSelf: TForm;
    FLibraries: TScriptLibraries;
    FPlugins: TScriptManagerPlugins;
    { Ereignisse }
    FCreateEvent: TScriptManagerCreateEvent;
    FDestroyEvent: TScriptManagerDestroyEvent;
    FReturnEvent: TScriptManagerReturnEvent;
    FNeedFileEvent: TScriptManagerNeedFileEvent;
    FCompileEvent: TScriptManagerCompileEvent;
    FExecuteEvent: TScriptManagerExecuteEvent;
    FAfterExecuteEvent: TScriptManagerAfterExecuteEvent;
    { Methoden }
    function GetCompilerOptions: TPSCompOptions;
    procedure SetCompilerOptions(Value: TPSCompOptions);
    function GetRunning: Boolean;
    procedure SetLibraries(Value: TScriptLibraries);
    function GetUsePreProcessor: Boolean;
    procedure SetUsePreProcessor(Value: Boolean);
    function GetMainFileName: AnsiString;
    procedure SetMainFileName(Value: AnsiString);
    function GetDefines: TStrings;
    procedure SetDefines(Value: TStrings);
    function GetCode: TStrings;
    procedure SetCode(Value: TStrings);
    function GetPlugins: TScriptManagerPlugins;
    procedure SetPlugins(Value: TScriptManagerPlugins);
  published
    { Published-Deklarationen }
    { Ereignisse}
    property OnCreate: TScriptManagerCreateEvent read FCreateEvent write FCreateEvent;
    property OnDestroy: TScriptManagerDestroyEvent read FDestroyEvent write FDestroyEvent;
    property OnReturn: TScriptManagerReturnEvent read FReturnEvent write FReturnEvent;
    property OnNeedFile: TScriptManagerNeedFileEvent read FNeedFileEvent write FNeedFileEvent;
    property OnCompile: TScriptManagerCompileEvent read FCompileEvent write FCompileEvent;
    property OnExecute: TScriptManagerExecuteEvent read FExecuteEvent write FExecuteEvent;
    property OnAfterExecute: TScriptManagerAfterExecuteEvent read FAfterExecuteEvent write FAfterExecuteEvent;
    { Eigenschaften }
    property About: TComponentAbout read FAbout;
    property CompilerOptions: TPSCompOptions read GetCompilerOptions write SetCompilerOptions default [];
    property ReturnMode: TScriptReturnMode read FReturnMode write FReturnMode default srNone;         //Wann soll eine Rückmeldung erfolgen?
    property ReturnStyle: TScriptReturnStyle read FReturnStyle write FReturnStyle default srSimple;   //Wie soll diese Rückmeldung aussehen?
    property SecureMode: Boolean read FSecureMode write FSecureMode default True;                     //Darf der Script auf den ScriptMgr zugreifen?
    property VarApplication: TApplication read FVarApplication write FVarApplication;                 //Variable "Application"
    property VarSelf: TForm read FVarSelf write FVarSelf;                                             //Variable "Self"
    property Libraries: TScriptLibraries read FLibraries write SetLibraries default [slClasses,slControls,slStdCtrls,slForms,slDateUtils,slComObj,slCustom];
    property UsePreProcessor: Boolean read GetUsePreProcessor write SetUsePreProcessor;               //Sind "Include"-Anweisungen erlaubt?
    property MainFileName: AnsiString read GetMainFileName write SetMainFileName;                     //Dateiname für "Include"-Anweisungen
    property Defines: TStrings read GetDefines write SetDefines;
    property Code: TStrings read GetCode write SetCode;
  protected
    { Protected-Deklarationen }
    procedure AddLog(Entry: String);
    procedure AddCustomPlugins;
    function ScriptEngineNeedFile(Sender:TObject; const OriginFileName: AnsiString; var FileName, Output: AnsiString): Boolean;    //TPSScript.OnNeedFile-Ereignis
    procedure ScriptEngineCompile(Sender: TPSScript);                                                    //TPSScript.OnCompile-Ereignis
    procedure ScriptEngineExecute(Sender: TPSScript);                                                    //TPSScript.OnExecute-Ereignis
    procedure ScriptEngineAfterExecute(Sender: TPSScript);                                               //TPSScript.OnAfterExecute-Ereignis
    procedure ScriptEngineLine(Sender: TObject);                                                         //TPSScript.OnLine-Ereignis
    { Skript-Funktionen }
    function PS_Random(LimitPlusOne : Integer): Integer;
    function PS_ExecuteFile(FileName: String; ExecMode: TFileExecuteMode): Boolean;
    function PS_InputBox(const ACaption, APrompt, ADefault: String): String;
    function PS_InputQuery(const ACaption, APrompt: String; var Value: String): Boolean;
    procedure PS_ShowMessage(const Msg: String);
    procedure PS_Randomize;
    procedure PS_Sleep(Milliseconds: Cardinal);
    procedure PS_About;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Running: Boolean read GetRunning;
    procedure Stop; virtual;
    property ReturnTarget: TStrings read FReturnTarget write FReturnTarget; //Wohin soll die Rückmeldung geschrieben werden?
    function CompileAndExecute: Boolean;
    property Plugins: TScriptManagerPlugins read GetPlugins write SetPlugins;
  end;

  procedure AddPlugin(PluginList: TPSPlugins; Plugin: TPSPlugin);
  procedure DeletePlugin(PluginList: TPSPlugins; Plugin: TPSPlugin);

  {$IFDEF ADD_COMPONENTREG}
    procedure Register;
  {$ENDIF}

const
  { Fehlermeldungen }
  Error_MissingReturnTarget = 'Missing log-return target';
  Error_MissingComponentName = 'Missing component name';

var
  { PS Import-Units }
  PS_Import_Classes: TPSImport_Classes;
  PS_Import_Controls: TPSImport_Controls;
  PS_Import_StdCtrls: TPSImport_StdCtrls;
  PS_Import_Forms: TPSImport_Forms;
  PS_Import_DateUtils: TPSImport_DateUtils;
  PS_Import_ComObj: TPSImport_ComObj;
  PS_Import_DB: TPSImport_DB;

implementation

{$IFDEF ADD_COMPONENTREG}
  procedure Register;
  begin
    RegisterComponents(ComponentsPage,[TScriptManager]);
  end;
{$ENDIF}

procedure AddPlugin(PluginList: TPSPlugins; Plugin: TPSPlugin);
begin
  TPSPluginItem(PluginList.Add).Plugin := Plugin;
end;

procedure DeletePlugin(PluginList: TPSPlugins; Plugin: TPSPlugin);
var
  Index: Integer;
  PluginFound: Boolean;
begin
  Index := 0;
  if PluginList.Count > Index then
  begin
    PluginFound := False;
    repeat
      if TPSPluginItem(PluginList.Items[Index]).Plugin = Plugin then
      begin
        PluginList.Delete(Index);
        PluginFound := True;
      end else
      begin
        Index := Index + 1;
        PluginFound := (Index >= PluginList.Count - 1);
      end;
    until PluginFound;
  end;
end;

{ ----------------------------------------------------------------------------
  TScriptManagerPlugins
  ---------------------------------------------------------------------------- }

constructor TScriptManagerPlugins.Create(AScriptManager: TScriptManager);
begin
  FScriptManager := AScriptManager;
end;

destructor TScriptManagerPlugins.Destroy;
begin
  //...
  inherited;
end;

function TScriptManagerPlugins.GetScriptManager: TScriptManager;
begin
  Result := FScriptManager;
end;

procedure TScriptManagerPlugins.SetScriptManager(Value: TScriptManager);
begin
  FScriptManager := Value;
end;

procedure TScriptManagerPlugins.Add(Plugin: TPSPlugin);
begin
  AddPlugin(FScriptManager.CustomPlugins,Plugin);
  FScriptManager.SetLibraries(FScriptManager.Libraries);
end;

procedure TScriptManagerPlugins.Delete(Plugin: TPSPlugin);
begin
  DeletePlugin(FScriptManager.CustomPlugins,Plugin);
  FScriptManager.SetLibraries(FScriptManager.Libraries);
end;

function TScriptManagerPlugins.GetItems(Index: Integer): TPSPlugin;
begin
  Result := TPSPluginItem(Items[Index]).Plugin;
end;

procedure TScriptManagerPlugins.SetItems(Index: Integer; Value: TPSPlugin);
begin
  TPSPluginItem(  Items[Index]).Plugin := Value;
end;

function TScriptManagerPlugins.GetCount: Integer;
begin
  Result := FScriptManager.Plugins.Count;
end;

{ ----------------------------------------------------------------------------
  TScriptManager
  ---------------------------------------------------------------------------- }

constructor TScriptManager.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TScriptManager);
  ReturnMode := srNone;
  ReturnStyle := srSimple;
  SecureMode := True;
  //ReturnSL := TStringList.Create;       Nicht erstellen, Property ist nur ein Pointer auf ext. TStrings
  FLibraries := [slClasses,slControls,slStdCtrls,slForms,slDateUtils,slCustom];
  if (Self.Owner is TForm) or (Owner.ClassParent = TForm) then
  begin
    VarSelf := (Self.Owner as TForm);
  end;
  Plugins := TScriptManagerPlugins.Create(Self);
  Log := TStringList.Create;
  CustomPlugins := TPSPlugins.Create(ScriptEngine);
  ScriptEngine := TPSScript.Create(Self);
  ScriptEngine.OnNeedFile := ScriptEngineNeedFile;
  ScriptEngine.OnCompile := ScriptEngineCompile;
  ScriptEngine.OnExecute := ScriptEngineExecute;
  ScriptEngine.OnAfterExecute := ScriptEngineAfterExecute;
  ScriptEngine.OnLine := ScriptEngineLine;

  { Import-Unit-Komponenten erstellen }
  PS_Import_Classes := TPSImport_Classes.Create(Self);
  PS_Import_Controls := TPSImport_Controls.Create(Self);
  PS_Import_StdCtrls := TPSImport_StdCtrls.Create(Self);
  PS_Import_Forms := TPSImport_Forms.Create(Self);
  PS_Import_DateUtils := TPSImport_DateUtils.Create(Self);
  PS_Import_ComObj := TPSImport_ComObj.Create(Self);
  PS_Import_DB := TPSImport_DB.Create(Self);
  //Libraries := [slClasses,slControls,slStdCtrls,slForms,slDateUtils,slCustom];  Durch DEFAULT festgelegt!
  if Assigned(OnCreate) then
  begin
    OnCreate(Self);
  end;
end;

destructor TScriptManager.Destroy;
begin
  if Assigned(OnDestroy) then
  begin
    OnDestroy(Self);
  end;
  VarSelf := nil;
  VarApplication := nil;
  ReturnMode := srNone;
  ReturnStyle := srSimple;
  Plugins.Free;
  //ReturnSL.Free;              Pointer auf ext. Komponente, nicht freigeben
  Log.Free;
  //ScriptEngine.Free;          Wird automatisch mit dem Owner vom TScriptManager freigegeben
  About.Free;
  {PS_Import_DB.Free;
  PS_Import_ComObj.Free;
  PS_Import_DateUtils.Free;
  PS_Import_Forms.Free;
  PS_Import_StdCtrls.Free;
  PS_Import_Controls.Free;
  PS_Import_Classes.Free;   }
  CustomPlugins.Free;
  ScriptEngine.Free;
  inherited;
end;

function TScriptManager.GetCompilerOptions: TPSCompOptions;
begin
  Result := ScriptEngine.CompilerOptions;
end;

procedure TScriptManager.SetCompilerOptions(Value: TPSCompOptions);
begin
  ScriptEngine.CompilerOptions := Value;
  ScriptEngine.Stop
end;

function TScriptManager.GetRunning: Boolean;
begin
  Result := ScriptEngine.Running;
end;

procedure TScriptManager.Stop;
begin
  ScriptEngine.Stop;
end;

procedure TScriptManager.SetLibraries(Value: TScriptLibraries);
begin
  ScriptEngine.Plugins.Clear;
  FLibraries := Value;
  { Import-Unit-Komponenten integrieren }
  if slClasses in FLibraries then
  begin
    AddPlugin(ScriptEngine.Plugins,PS_Import_Classes);
  end else
  begin
    FLibraries := FLibraries - [slControls,slStdCtrls,slForms,slDateUtils,slComObj,slDB];
  end;
  if slControls in FLibraries then
  begin
    AddPlugin(ScriptEngine.Plugins,PS_Import_Controls);
  end else
  begin
    FLibraries := FLibraries - [slStdCtrls,slForms];
  end;
  if slStdCtrls in Libraries then
  begin
    AddPlugin(ScriptEngine.Plugins,PS_Import_StdCtrls);
  end else
  begin
    FLibraries := FLibraries - [slForms];
  end;
  if slForms in FLibraries then
  begin
    AddPlugin(ScriptEngine.Plugins,PS_Import_Forms);
  end;
  if slDateUtils in FLibraries then
  begin
    AddPlugin(ScriptEngine.Plugins,PS_Import_DateUtils);
  end;
  if slComObj in FLibraries then
  begin
    AddPlugin(ScriptEngine.Plugins,PS_Import_ComObj);
  end;
  if slDB in FLibraries then
  begin
    AddPlugin(ScriptEngine.Plugins,PS_Import_DB);
  end;
  if slCustom in FLibraries then
  begin
    AddCustomPlugins;
  end;
end;

function TScriptManager.GetUsePreProcessor: Boolean;
begin
  Result := ScriptEngine.UsePreProcessor;
end;

procedure TScriptManager.SetUsePreProcessor(Value: Boolean);
begin
  ScriptEngine.UsePreProcessor := Value;
end;

function TScriptManager.GetMainFileName: AnsiString;
begin
  Result := ScriptEngine.MainFileName;
end;

procedure TScriptManager.SetMainFileName(Value: AnsiString);
begin
  ScriptEngine.MainFileName := Value;
end;

function TScriptManager.GetDefines: TStrings;
begin
  Result := ScriptEngine.Defines;
end;

procedure TScriptManager.SetDefines(Value: TStrings);
begin
  ScriptEngine.Defines := Value;
end;

function TScriptManager.GetCode: TStrings;
begin
  Result := ScriptEngine.Script;
end;

procedure TScriptManager.SetCode(Value: TStrings);
begin
  ScriptEngine.Script := Value;
end;

function TScriptManager.GetPlugins: TScriptManagerPlugins;
begin
  Result := FPlugins;
end;

procedure TScriptManager.SetPlugins(Value: TScriptManagerPlugins);
begin
  FPlugins := Value;
end;

procedure TScriptManager.AddLog(Entry: string);
const
  PrefixBegin = '[';
  PrefixEnd = ']';
begin
  Log.Add(Entry);
  if ReturnTarget = nil then
  begin
    raise EMissingReturnTarget.Create(Error_MissingReturnTarget);
  end;
  if ReturnStyle = srTime then
  begin
    Entry := PrefixBegin + TimeToStr(Time) + PrefixEnd + ' ' + Entry;
  end;
  if ReturnStyle = srDateTime then
  begin
    Entry := PrefixBegin + DateToStr(Date) + ' ' + TimeToStr(Time) + PrefixEnd + ' ' + Entry;
  end;
  if ReturnStyle = srName then
  begin
    if Length(Name) > 0 then
    begin
      Entry := PrefixBegin + Name + PrefixEnd + ' ' + Entry;
    end else
    begin
      raise EMissingComponentName.Create(Error_MissingComponentName);
    end;
  end;
  ReturnTarget.Add(Entry);
  if Assigned(OnReturn) then
  begin
    OnReturn(Self,Entry);
  end;
end;

procedure TScriptManager.AddCustomPlugins;
var
  Index: Integer;
begin
  for Index := 0 to CustomPlugins.Count - 1 do
  begin
    AddPlugin(ScriptEngine.Plugins,TPSPluginItem(CustomPlugins.Items[Index]).Plugin);
  end;
end;

function TScriptManager.CompileAndExecute: Boolean;
begin
  Result := False;
  if ReturnMode = srAll then
  begin
    AddLog('Compiling script...');
  end;
  if ScriptEngine.Compile then
  begin
    if ReturnMode = srAll then
    begin
      AddLog('Script succesfully compiled');
    end;
    if ReturnMode = srAll then
    begin
      AddLog('Executing script...');
    end;
    if not ScriptEngine.Execute then
    begin
      if (ReturnMode = srAll) or (ReturnMode = srErrors) then
      begin
        AddLog(ScriptEngine.ExecErrorToString + ' at ' + IntToStr(ScriptEngine.ExecErrorProcNo) + '.'+IntToStr(ScriptEngine.ExecErrorByteCodePosition));
      end;
    end else
    begin
      if ReturnMode = srAll then
      begin
        AddLog('Script succesfully executed');
      end;
      Result := True;
    end;
  end else
  begin
    if ReturnMode = srAll then
    begin
      AddLog('Script compilation failed');
    end;
  end;
end;

function TScriptManager.ScriptEngineNeedFile(Sender:TObject; const OriginFileName: AnsiString; var FileName, Output: AnsiString): Boolean;
begin
  Result := ScriptEngine.OnNeedFile(Sender,OriginFileName,FileName,Output);
  if Assigned(OnNeedFile) then
  begin
    OnNeedFile(Self,OriginFileName,FileName,OutPut);
  end;
end;

procedure TScriptManager.ScriptEngineCompile(Sender: TPSScript);
begin
  with (Sender as TPSScript) do
  begin
    { VARIABLEN }
    if slForms in Libraries then
    begin
      if Assigned(VarSelf) then
      begin
        AddRegisteredVariable('Self','TForm');
      end;
      if Assigned(VarApplication) then
      begin
        AddRegisteredVariable('Application','TApplication');
      end;
    end;
    { TYPEN }
    //Comp.AddTypeS('TFileExecuteMode','(feOpen,feEdit)');
    { FUNKTIONEN }
    AddMethod(Self,@TScriptManager.PS_Random,'function Random(LimitPlusOne: Integer): Integer');
    //AddMethod(Self,@TScriptManager.PS_ExecuteFile,'function ExecuteFile(FileName: String; ExecMode: TFileExecuteMode): Boolean');
    AddMethod(Self,@TScriptManager.PS_InputBox,'function InputBox(const ACaption, APrompt, ADefault: String): String');
    AddMethod(Self,@TScriptManager.PS_InputQuery,'function InputQuery(const ACaption, APrompt: String; var Value: String): Boolean');
    { PROZEDUREN }
    AddMethod(Self,@TScriptManager.PS_ShowMessage,'procedure ShowMessage (const Msg: String)');
    AddMethod(Self,@TScriptManager.PS_Randomize,'procedure Randomize');
    AddMethod(Self,@TScriptManager.PS_Sleep,'procedure Sleep(Milliseconds: Cardinal)');
    AddMethod(Self,@TScriptManager.PS_About,'procedure About');
    if not SecureMode then
    begin
      AddMethod(Self,@TScriptManager.CompileAndExecute,'function CompileAndExecute: Boolean');
    end;
  end;
  if Assigned(OnCompile) then
  begin
    OnCompile(Self);
  end;
end;

procedure TScriptManager.ScriptEngineExecute(Sender: TPSScript);
begin
  with (Sender as TPSScript) do
  begin
    if Assigned(VarSelf) then
    begin
      SetVarToInstance('Self',VarSelf);
    end;
    if Assigned(VarApplication) then
    begin
      SetVarToInstance('Application',VarApplication);
    end;
  end;
  if Assigned(OnExecute) then
  begin
    OnExecute(Self);
  end;
end;

procedure TScriptManager.ScriptEngineAfterExecute(Sender: TPSScript);
begin
  with (Sender as TPSScript) do
  begin
   // ScriptEngine.Plugins.Clear;        NICHT BENUTZEN!
  end;
  if Assigned(OnAfterExecute) then
  begin
    OnAfterExecute(Self);
  end;
end;

procedure TScriptManager.ScriptEngineLine(Sender: TObject);
begin
  with (Sender as TPSScript) do
  begin
    //...                                Nur definieren falls verwendet/benötigt!
  end;
end;

function TScriptManager.PS_Random(LimitPlusOne: Integer): Integer;
begin
  Result := Random(LimitPlusOne);
end;

function TScriptManager.PS_ExecuteFile(FileName: String; ExecMode: TFileExecuteMode): Boolean;
begin
  Result := ExecuteFile(FileName,feOpen);
end;

function TScriptManager.PS_InputBox(const ACaption, APrompt, ADefault: String): String;
begin
  Result := InputBox(ACaption,APrompt,ADefault);
end;

function TScriptManager.PS_InputQuery(const ACaption, APrompt: String; var Value: String): Boolean;
begin
  Result := InputQuery(ACaption,APrompt,Value);
end;

procedure TScriptManager.PS_ShowMessage(const Msg: string);
begin
  ShowMessage(Msg);
end;

procedure TScriptManager.PS_Randomize;
begin
  Randomize;
end;

procedure TScriptManager.PS_Sleep(Milliseconds: Cardinal);
begin
  Sleep(Milliseconds);
end;

procedure TScriptManager.PS_About;
begin
  MessageDlg(About.Name + ' Component v'
            + About.Version + sLineBreak
            + 'For: Borland Delphi' + sLineBreak
            + About.Copyright + ' by '
            + About.Author + sLineBreak
            + 'Requires and uses: RemObjects PascalScript component library',
            mtInformation,[mbOK],0);
end;

end.
