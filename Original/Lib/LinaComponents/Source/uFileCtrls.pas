unit uFileCtrls;

//////////////////////////////////////
///  Lina File Controls Unit       ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

uses
  { Standard-Units }
  SysUtils, Classes, Windows, Registry,
  { Andere Package-Units }
  uBase, uSysTools;

type
  { Fehlermeldungen }
  EInvalidItemName = class(Exception);
  EInvalidExt = class(Exception);
  EInvalidAlias = class(Exception);

  { Ereignisse }
  TContextMenuCreateItemEvent = procedure(Sender: TObject) of object;
  TContextMenuCreateEntryEvent = procedure(Sender: TObject) of object;

  { Hauptklassen }
  TContextMenuItem = class(TCollectionItem)
  private
    { Private-Deklarationen }
    FName: TComponentName;
    FCaption: ShortString;
    FCommand: AnsiString;
    FIcon: TFileName;
    { Methden }
    procedure SetName(Value: TComponentName);
    procedure SetIcon(Value: TFileName);
  public
    { Public-Deklarationen }
    constructor Create(Collextion: TCollection); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property Name: TComponentName read FName write SetName;
    property Caption: ShortString read FCaption write FCaption;
    property Command: AnsiString read FCommand write FCommand;
    property Icon: TFileName read FIcon write SetIcon;
  end;

  TContextMenuItems = class(TCollection);

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TContextMenu = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FItems: TContextMenuItems;
    FExt: ShortString;
    FAutoLoad: Boolean;
    FAlias: ShortString;
    FAliasRedirect: Boolean;
    { Ereignisse }
    FCreateItemEvent: TContextMenuCreateItemEvent;
    FCreateEntryEvent: TContextMenuCreateEntryEvent;
    { Methoden }
    function GetRegKey: String;
    procedure SetExt(Value: ShortString);
    procedure SetAutoLoad(Value: Boolean);
    procedure SetAlias(Value: ShortString);
    procedure SetAliasRedirect(Value: Boolean);
  protected
    { Protected-Deklarationen }
    procedure CreateMenuItem(const AName: TComponentName;
     const ACaption: ShortString; const ACommand: AnsiString;
     const AIcon: TFileName);
    procedure CreateRegistryEntry(var ARegistry: TRegistry;
     const AName: TComponentName; const ACaption: ShortString;
     const ACommand: AnsiString; const AIcon: TFileName);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromRegistry;
    procedure SaveToRegistry;
    property RegKey: String read GetRegKey;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
    property Items: TContextMenuItems read FItems write FItems;
    property Ext: ShortString read FExt write SetExt;
    property AutoLoad: Boolean read FAutoLoad write SetAutoLoad default False;
    property Alias: ShortString read FAlias write SetAlias;
    property AliasRedirect: Boolean read FAliasRedirect write SetAliasRedirect default True;
    { Ereignisse }
    property OnCreateItem: TContextMenuCreateItemEvent read FCreateItemEvent write FCreateItemEvent;
    property OnCreateEntry: TContextMenuCreateEntryEvent read FCreateEntryEvent write FCreateEntryEvent;
  end;

  {$IFDEF ADD_COMPONENTREG}
    procedure Register;
  {$ENDIF}

const
  { Sonstige }
  ContextRegPathShell = '\shell';
  ContextRegPathCommand = '\command';

implementation

{$IFDEF ADD_COMPONENTREG}
  procedure Register;
  begin
    RegisterComponents({$IFDEF ADD_SINGLECATEGORY}ComponentsPage{$ELSE}ComponentsPage_File{$ENDIF},[TContextMenu]);
  end;
{$ENDIF}

{ ----------------------------------------------------------------------------
  TContextMenuItem
  ---------------------------------------------------------------------------- }

constructor TContextMenuItem.Create(Collextion: TCollection);
begin
  inherited;
  FName := ClassName + IntToStr(ID);
end;

destructor TContextMenuItem.Destroy;
begin
  //...
  inherited;
end;

procedure TContextMenuItem.SetName(Value: TComponentName);
var
  Index: Integer;
begin
  if Length(Value) = 0 then
  begin
    raise EInvalidItemName.Create('Invalid context menu item name');
  end;
  for Index := 1 to Length(Value) do
  begin
    if Value[Index] = PathDelim then
    begin
      raise EInvalidItemName.Create('Invalid contect menu item name');
    end;
  end;
  FName := Value;
end;

procedure TContextMenuItem.SetIcon(Value: TFileName);
begin
  if FileExists(Value) or (Length(Value) = 0) then
  begin
    FIcon := Value;
  end else
  begin
    raise EFileNotFoundException.Create('Icon file not found: "' + Value + '"');
  end;
end;

{ ----------------------------------------------------------------------------
  TContextMenu
  ---------------------------------------------------------------------------- }

constructor TContextMenu.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TContextMenu);
  FItems := TContextMenuItems.Create(TContextMenuItem);
  FExt := '*';
  FAutoLoad := False;
  FAliasRedirect := True;
end;

destructor TContextMenu.Destroy;
begin
  FAbout.Free;
  FItems.Free;
  inherited;
end;

function TContextMenu.GetRegKey: String;
begin
  if (Length(Alias) = 0) or (not AliasRedirect) then
  begin
    if Ext = '*' then
    begin
      Result := Ext;
    end else
    begin
      Result := '.' + Ext;
    end;
  end else
  begin
    Result := Alias;
  end;
end;

procedure TContextMenu.SetExt(Value: ShortString);
var
  Index: Integer;
begin
  if Value = FExt then
  begin
    Exit;
  end;
  if Length(Value) > 0 then
  begin
    if Value <> '*' then
    begin
      for Index := 1 to Length(Value) do
      begin
        if not (Value[Index] in Numbers + Letters) then
        begin
          raise EInvalidExt.Create('Invalid file extension value');
        end;
      end;
    end;
    FExt := Value;
  end else
  begin
    FExt := '*';
  end;
  if FAutoLoad then
  begin
    LoadFromRegistry;
  end;
end;

procedure TContextMenu.SetAutoLoad(Value: Boolean);
begin
  FAutoLoad := Value;
  if Value then
  begin
    LoadFromRegistry;
  end;
end;

procedure TContextMenu.SetAlias(Value: ShortString);
var
  Index: Integer;
begin
  if Value = FAlias then
  begin
    Exit;
  end;
  if Length(Value) > 0 then
  begin
    for Index := 1 to Length(Value) do
    begin
      if not (Value[Index] in Numbers + Letters) then
      begin
        raise EInvalidAlias.Create('Invalid extension alias value');
      end;
    end;
  end;
  FAlias := Value;
  if FAutoLoad then
  begin
    LoadFromRegistry;
  end;
end;

procedure TContextMenu.SetAliasRedirect(Value: Boolean);
begin
  FAliasRedirect := Value;
  if FAutoLoad then
  begin
    LoadFromRegistry;
  end;
end;

procedure TContextMenu.CreateMenuItem(const AName: TComponentName;
 const ACaption: ShortString; const ACommand: AnsiString;
 const AIcon: TFileName);
begin
  with (Items.Add as TContextMenuItem) do
  begin
    Name := AName;
    Caption := ACaption;
    Command := ACommand;
    Icon := AIcon;
  end;
  if Assigned(OnCreateItem) then
  begin
    OnCreateItem(Self);
  end;
end;

procedure TContextMenu.CreateRegistryEntry(var ARegistry: TRegistry;
 const AName: TComponentName; const ACaption: ShortString;
 const ACommand: AnsiString; const AIcon: TFileName);
var
  Key: String;
begin
  Key := RegKey + ContextRegPathShell;
  with ARegistry do
  begin
    OpenKey(Key + PathDelim + AName,True);
    WriteString('',ACaption);
    WriteString('icon',AIcon);
    CloseKey;
    OpenKey(Key + PathDelim + AName + ContextRegPathCommand,True);
    WriteString('',ACommand);
    CloseKey;
  end;
  if Assigned(OnCreateEntry) then
  begin
    OnCreateEntry(Self);
  end;
end;

procedure TContextMenu.LoadFromRegistry;
var
  Reg: TRegistry;
  Key: String;
  SubKeys: TStrings;
  Index: Integer;
  Cptn: ShortString;
  Cmd: AnsiString;
  Icn: TFileName;
begin
  Items.Clear;
  Reg := TRegistry.Create;
  SubKeys := TStringList.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.Access := KEY_READ;
    if Reg.KeyExists(RegKey) then
    begin
      Reg.OpenKeyReadOnly(RegKey);
      if Reg.ValueExists('') then
      begin
        FAlias := Reg.ReadString('');
      end;
      Reg.CloseKey;
    end;
    Key := RegKey + ContextRegPathShell;
    if Reg.KeyExists(Key) then
    begin
      Reg.OpenKeyReadOnly(Key);
      if Reg.HasSubKeys then
      begin
        Reg.GetKeyNames(SubKeys);
        for Index := 0 to SubKeys.Count - 1 do
        begin
          if Reg.KeyExists(SubKeys.Strings[Index] + ContextRegPathCommand) then
          begin
            Reg.CloseKey;
            Reg.OpenKeyReadOnly(Key + PathDelim + SubKeys.Strings[Index]);
            if Reg.ValueExists('') then
            begin
              Cptn := Reg.ReadString('');
            end else
            begin
              Cptn := '';
            end;
            if Reg.ValueExists('icon') then
            begin
              Icn := Reg.ReadString('icon');
            end else
            begin
              Icn := '';
            end;
            Reg.CloseKey;
            Reg.OpenKeyReadOnly(Key + PathDelim + SubKeys.Strings[Index] + ContextRegPathCommand);
            if Reg.ValueExists('') then
            begin
              Cmd := Reg.ReadString('');
            end else
            begin
              Cmd := '';
            end;
            CreateMenuItem(SubKeys.Strings[Index],Cptn,Cmd,Icn);
          end;
        end;
      end;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
    SubKeys.Free;
  end;
end;

procedure TContextMenu.SaveToRegistry;
var
  Reg: TRegistry;
  Index: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Reg.Access := KEY_WRITE;
    Reg.OpenKey(RegKey,True);
    Reg.WriteString('',FAlias);
    Reg.CloseKey;
    for Index := 0 to Items.Count - 1 do
    begin
      CreateRegistryEntry(Reg,(Items.Items[Index] as TContextMenuItem).Name,
                              (Items.Items[Index] as TContextMenuItem).Caption,
                              (Items.Items[Index] as TContextMenuItem).Command,
                              (Items.Items[Index] as TContextMenuItem).Icon);
    end;
  finally
    Reg.Free;
  end;
end;

end.
