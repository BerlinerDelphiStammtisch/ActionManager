unit uAdvCtrls;

//////////////////////////////////////
///  Lina Advanced Controls Unit   ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

uses
  { Standard-Units }
  SysUtils, Classes, StdCtrls, ExtCtrls, Windows, Messages, Graphics, Controls,
  Printers, Contnrs, Forms, Math, ShLwApi,
  { Andere Package-Units }
  uBase, uSysTools, uFileTools;

type
  { Hilfsklassen }
  TMemoCaptionMode = (mcmAnyState,mcmUnfocusedOnly);
  TShortcutLabelState = (slsDefault,slsHovered,slsPressed);
  TSizePanelAllow = set of (spaResize,spaMove);
  TPanelBorderStyle = bsNone..bsSizeable;
  TPathEditPaths = set of (pepURL_History,pepURL_Recent,pepFS_Dirs,pepFS_Shell);
  TValueEditAllow = set of (veaNumbers,veaLetters,veaSpaces,veaSeparators,veaOperators,veaOther);

  { Ereignisse }
  TPaintMemoPaintEvent = procedure(Sender: TObject) of object;
  TShortcutLabelOpenTargetEvent = procedure(Sender: TObject) of object;
  TShortcutLabelOpenTargetQueryEvent = procedure(Sender: TObject; var CanOpen: Boolean) of object;

  { Hauptklassen }
  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCommandButton = class(TButton)
  protected
    procedure CreateWnd; override;
    procedure Loaded; override;
    procedure CreateParams(var Params: TCreateParams); override;
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FCommandLink: Boolean;
    FNote: WideString;
    FShield: Boolean;
    FIcon: TIcon;
    FReplaceCaption: Boolean;
    { Methoden }
    procedure SetCommandLink(Value: Boolean);
    procedure SetNote(Value: WideString);
    procedure SetShield(Value: Boolean);
    procedure SetIcon(Value: TIcon);
    procedure SetReplaceCaption(Value: Boolean);
    procedure IconChange(Sender: TObject);
    procedure UpdateShield;
    procedure UpdateIcon;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
    property CommandLink: Boolean read FCommandLink write SetCommandLink default False;
    property Note: WideString read FNote write SetNote;
    property Shield: Boolean read FShield write SetShield default False;
    property Icon: TIcon read FIcon write SetIcon;
    property ReplaceCaption: Boolean read FReplaceCaption write SetReplaceCaption default False;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TScrollListBox = class(TListBox)
  protected
    { Protected-Deklarationen }
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure WndProc(var Message: TMessage); override;
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FHorizontalScrollBar: Boolean;
    FWordWrap: Boolean;
    FItemHeights: array of Integer;
    { Methoden }
    function GetTopIndex: Integer;
    procedure SetTopIndex(Value: Integer);
    procedure SetHorizontalScrollBar(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
    function GetItemHeights(Index: Integer): Integer;
    procedure UpdateItemHeights;
    procedure UpdateHorizontalScrollBar;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScrollTo(Index: Integer);
    property ItemHeights [Index: Integer]: Integer read GetItemHeights;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
    property TopIndex: Integer read GetTopIndex write SetTopIndex;
    property HorizontalScrollBar: Boolean read FHorizontalScrollBar write SetHorizontalScrollBar default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPaintMemo = class(TMemo)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FShowCaption: Boolean;
    FCaption: TCaption;
    FCaptionFont: TFont;
    FCaptionMode: TMemoCaptionMode;
    FCaptionVisible: Boolean;
    FCaptionPosition: TPoint;
    { Ereignisse }
    FPaintEvent: TPaintMemoPaintEvent;
    { Methoden }
    procedure SetShowCaption(Value: Boolean);
    procedure SetCaptionFont(Value: TFont);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    property CaptionVisible: Boolean read FCaptionVisible default False;
  published
    { Ereignisse}
    property OnPaint: TPaintMemoPaintEvent read FPaintEvent write FPaintEvent;
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property Caption: TCaption read FCaption write FCaption;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionMode: TMemoCaptionMode read FCaptionMode write FCaptionMode default mcmUnfocusedOnly;
    property CaptionPosition: TPoint read FCaptionPosition write FCaptionPosition;
  end;

  TShortcutLabel = class;

  TShortcutLabelFont = class(TPersistent)
  private
    { Private-Deklarationen }
    FLabel: TShortcutLabel;
    FDefault: TFont;
    FHovered: TFont;
    FPressed: TFont;
    FVisited: TColor;
    { Methoden }
    procedure SetDefault(Value: TFont);
    procedure SetHovered(Value: TFont);
    procedure SetPressed(Value: TFont);
    procedure SetVisited(Value: TColor);
    procedure FontChange(Sender: TObject);
  public
    { Public-Deklarationen }
    constructor Create(ALabel: TShortcutLabel);
    destructor Destroy; override;
    procedure Update;
  published
    { Published-Deklarationen }
    property Default: TFont read FDefault write SetDefault;
    property Hovered: TFont read FHovered write SetHovered;
    property Pressed: TFont read FPressed write SetPressed;
    property Visited: TColor read FVisited write SetVisited default clPurple;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TShortcutLabel = class(TLabel)
  protected
    { Protected-Deklarationen }
    procedure RegisterToList;
    procedure UnregisterFromList;
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FAutoOpenTarget: Boolean;
    FTarget: String;
    FState: TShortcutLabelState;
    FFont: TShortcutLabelFont;
    FHighlightVisited: Boolean;
    FStoreVisited: Boolean;
    { Ereignisse }
    FOpenTargetEvent: TShortcutLabelOpenTargetEvent;
    FOpenTargetQueryEvent: TShortcutLabelOpenTargetQueryEvent;
    { Methoden }
    procedure SetFont(Value: TShortcutLabelFont);
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    property State: TShortcutLabelState read FState default slsDefault;
  published
    { Ereignisse }
    property OnOpenTarget: TShortcutLabelOpenTargetEvent read FOpenTargetEvent write FOpenTargetEvent;
    property OnOpenTargetQuery: TShortcutLabelOpenTargetQueryEvent read FOpenTargetQueryEvent write FOpenTargetQueryEvent;
    { Published-Deklarationen }
    property Cursor default crHandPoint;
    property About: TComponentAbout read FAbout;
    property AutoOpenTarget: Boolean read FAutoOpenTarget write FAutoOpenTarget default True;
    property Target: String read FTarget write FTarget;
    property Font: TShortcutLabelFont read FFont write SetFont;
    property HighlightVisited: Boolean read FHighlightVisited write FHighlightVisited default True;
    property StoreVisited: Boolean read FStoreVisited write FStoreVisited default True;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TSizePanel = class(TPanel)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FAllow: TSizePanelAllow;
    FBorderStyle: TPanelBorderStyle;
    { Methoden }
    procedure SetBorderStyle(Value: TPanelBorderStyle);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
    property Allow: TSizePanelAllow read FAllow write FAllow default [spaResize,spaMove];
    property BorderStyle: TPanelBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPathEdit = class(TEdit)
  protected
    { Protected-Deklarationen }
    procedure UpdateAutoComplete;
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FAutoSuggest: Boolean;
    FAutoAppend: Boolean;
    FPaths: TPathEditPaths;
    { Methoden }
    procedure SetPaths(Value: TPathEditPaths);
    procedure SetAutoSuggest(Value: Boolean);
    procedure SetAutoAppend(Value: Boolean);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property About: TComponentAbout read FAbout;
    property AutoSuggest: Boolean read FAutoSuggest write SetAutoSuggest default True;
    property AutoAppend: Boolean read FAutoAppend write SetAutoAppend default True;
    property Paths: TPathEditPaths read FPaths write SetPaths default [pepURL_History,pepURL_Recent,pepFS_Dirs,pepFS_Shell];
  end;

  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TValueEdit = class(TEdit)
  protected
    { Protected-Deklarationen }
    procedure Change; override;
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FMinValue: Extended;
    FMaxValue: Extended;
    FAllow: TValueEditAllow;
    FMaxSeparators: Byte;
    FSeparatorChar: Char;
    { Methoden }
    procedure SetMinValue(Value: Extended);
    procedure SetMaxValue(Value: Extended);
    procedure SetAllow(Value: TValueEditAllow);
    procedure SetMaxSeparators(Value: Byte);
    procedure SetSeparatorChar(Value: Char);
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
    property MinValue: Extended read FMinValue write SetMinValue;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property Allow: TValueEditAllow read FAllow write SetAllow default [veaNumbers,veaSeparators];
    property MaxSeparators: Byte read FMaxSeparators write SetMaxSeparators default 1;
    property SeparatorChar: Char read FSeparatorChar write SetSeparatorChar default '.';
  end;

  {$IFDEF ADD_COMPONENTREG}
    procedure Register;
  {$ENDIF}

  function BmpToIco(Bitmap: TBitmap): TIcon;
  function BmpToMask(Bitmap: TBitmap; BkColor: TColor = clNone): TBitmap;
  procedure BmpResize(var Bitmap: TBitmap; NewWidth, NewHeight: Integer);
  function TargetVisited(Target: String): Boolean;

const
  { Messages }
  BS_COMMANDLINK = $0000000E;
  BM_SETIMAGE = $00F7;
  BCM_SETNOTE = $00001609;
  BCM_FIRST = $1600;
  BCM_SETSHIELD = BCM_FIRST + $000C;

var
  VisitedTargets: TStrings;
  ShortcutLabels: TObjectList;

implementation

{$IFDEF ADD_COMPONENTREG}
  procedure Register;
  begin
    RegisterComponents({$IFDEF ADD_SINGLECATEGORY}ComponentsPage{$ELSE}ComponentsPage_Advanced{$ENDIF},[TCommandButton,TScrollListBox,TPaintMemo,TShortcutLabel,TSizePanel,TPathEdit,TValueEdit]);
  end;
{$ENDIF}

function BmpToIco(Bitmap: TBitmap): TIcon;
begin
  with TImageList.CreateSize(Bitmap.Width,Bitmap.Height) do
  begin
    try
      AddMasked(Bitmap,Bitmap.TransparentColor);
      Result := TIcon.Create;
      GetIcon(0,Result);
    finally
      Free;
    end;
  end;
end;

function BmpToMask(Bitmap: TBitmap; BkColor: TColor = clNone): TBitmap;
begin
  Result := TBitmap.Create;
  Result.Assign(Bitmap);
  Result.Canvas.Brush.Color := BkColor;
  Result.Monochrome := True;
end;

procedure BmpResize(var Bitmap: TBitmap; NewWidth, NewHeight: Integer);
var
  ResizedBitmap: TBitmap;
begin
  if (Bitmap.Width <> NewWidth) and (Bitmap.Height <> NewHeight) then
  begin
    ResizedBitmap := TBitmap.Create;
    ResizedBitmap.Assign(Bitmap);
    ResizedBitmap.SetSize(NewWidth,NewHeight);
    SetStretchBltMode(Bitmap.Canvas.Handle,HALFTONE);
    StretchBlt(ResizedBitmap.Canvas.Handle,0,0,ResizedBitmap.Width,ResizedBitmap.Height,Bitmap.Canvas.Handle,0,0,Bitmap.Width,Bitmap.Height,SRCCOPY);
    Bitmap.Assign(ResizedBitmap);
  end;
end;

function TargetVisited(Target: String): Boolean;
begin
  if VisitedTargets = nil then
  begin
    VisitedTargets := TStringList.Create;
    (VisitedTargets as TStringList).Duplicates := dupIgnore;
  end;
  Result := (VisitedTargets.IndexOf(Target) <> -1);
end;

{ ----------------------------------------------------------------------------
  TCommandButton
  ---------------------------------------------------------------------------- }

constructor TCommandButton.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TCommandButton);
  FCommandLink := False;
  FShield := False;
  FIcon := TIcon.Create;
  FIcon.OnChange := IconChange;
  FReplaceCaption := False;
  Width := 125;
end;

destructor TCommandButton.Destroy;
begin
  FAbout.Free;
  FIcon.Free;
  inherited;
end;

procedure TCommandButton.SetCommandLink(Value: Boolean);
begin
  FCommandLink := Value;
  RecreateWnd;
end;

procedure TCommandButton.SetNote(Value: WideString);
begin
  SendMessage(Handle,BCM_SETNOTE,0,LParam(Pointer(Value)));
  FNote := Value;
end;

procedure TCommandButton.SetShield(Value: Boolean);
begin
  FShield := Value;
  UpdateShield;
  UpdateIcon;
end;

procedure TCommandButton.SetIcon(Value: TIcon);
begin
  FIcon.Assign(Value);
  UpdateShield;
  UpdateIcon;
end;

procedure TCommandButton.SetReplaceCaption(Value: Boolean);
begin
  FReplaceCaption := Value;
  if Value then
  begin
    SetWindowLong(Handle,GWL_STYLE,GetWindowLong(Handle,GWL_STYLE) or BS_ICON);
  end else
  begin
    SetWindowLong(Handle,GWL_STYLE,GetWindowLong(Handle,GWL_STYLE) and (not BS_ICON));
  end;
  UpdateShield;
  UpdateIcon;
end;

procedure TCommandButton.CreateWnd;
begin
  inherited;
  if not (csLoading in ComponentState) then
  begin
    UpdateShield;
    UpdateIcon;
  end;
end;

procedure TCommandButton.Loaded;
begin
  inherited;
  UpdateShield;
  UpdateIcon;
end;

procedure TCommandButton.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if CommandLink then
  begin
    Params.Style := Params.Style or BS_COMMANDLINK;
  end;
end;

procedure TCommandButton.IconChange(Sender: TObject);
begin
  UpdateShield;
  UpdateIcon;
end;

procedure TCommandButton.UpdateShield;
begin
  if Icon.Empty and Shield then
  begin
    SendMessage(Handle,BCM_SETSHIELD,0,LParam(Shield));
  end;
end;

procedure TCommandButton.UpdateIcon;
begin
  if (not Icon.Empty) or (not Shield) then
  begin
    SendMessage(Handle,BM_SETIMAGE,1,LParam(Icon.Handle));
  end;
end;

{ ----------------------------------------------------------------------------
  TScrollListBox
  ---------------------------------------------------------------------------- }

constructor TScrollListBox.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TScrollListBox);
  FHorizontalScrollBar := False;
  FWordWrap := False;
end;

destructor TScrollListBox.Destroy;
begin
  FAbout.Free;
  inherited;
end;

function TScrollListBox.GetTopIndex: Integer;
begin
  Result := SendMessage(Handle,LB_GETTOPINDEX,0,0);
end;

procedure TScrollListBox.SetTopIndex(Value: Integer);
begin
  ScrollTo(Value);
end;

procedure TScrollListBox.SetHorizontalScrollBar(Value: Boolean);
begin
  if Value then
  begin
    UpdateHorizontalScrollBar;
  end else
  begin
    Perform(LB_SETHORIZONTALEXTENT,0,0);
  end;
  FHorizontalScrollBar := Value;
end;

procedure TScrollListBox.SetWordWrap(Value: Boolean);
begin
  if Value then
  begin
    Style := lbOwnerDrawVariable;
  end;
  FWordWrap := Value;
end;

function TScrollListBox.GetItemHeights(Index: Integer): Integer;
begin
  UpdateItemHeights;
  Result := FItemHeights[Index];
end;

procedure TScrollListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  CountIndex: Integer;
begin
  if WordWrap then
  begin
    Canvas.FillRect(Rect);
    if (Index < Count) and (Index >= TopIndex) then
    begin
      Perform(LB_SETITEMHEIGHT,Index,ItemHeights[Index]);
      Rect.Top := 0;
      for CountIndex := TopIndex to Index - 1 do
      begin
        Rect.Top := Rect.Top + FItemHeights[CountIndex];
      end;
      if UseRightToLeftAlignment then
      begin
        Rect.Right := Rect.Right - 2;
      end else
      begin
        Rect.Left := Rect.Left + 2;
      end;
      Rect.Height := ItemHeights[Index];
      Canvas.Pen.Color := clWhite;
      DrawText(Canvas.Handle,Items[Index],Length(Items[Index]),Rect,DT_WORDBREAK or DT_LEFT or DT_TOP or DT_NOPREFIX or DT_END_ELLIPSIS);
    end;
  end else
  begin
    inherited;
  end;
end;

procedure TScrollListBox.WndProc(var Message: TMessage);
begin
  inherited;
  if HorizontalScrollBar and (Message.Msg = LB_ADDSTRING) or (Message.Msg = LB_DELETESTRING) or (Message.Msg = LB_INSERTSTRING) then
  begin
    UpdateHorizontalScrollBar;
  end;
end;

procedure TScrollListBox.UpdateItemHeights;
var
  Index: Integer;
  Rect: TRect;
begin
  SetLength(FItemHeights,Items.Count);
  for Index := 0 to Length(FItemHeights) - 1 do
  begin
    Rect := ItemRect(Index);
    FItemHeights[Index] := DrawText(Canvas.Handle,Items.Strings[Index],Length(Items.Strings[Index]),Rect,DT_CALCRECT or DT_WORDBREAK or DT_LEFT or DT_TOP or DT_NOPREFIX);
  end;
end;

procedure TScrollListBox.UpdateHorizontalScrollBar;
var
  Index: Integer;
  MaxWidth: Integer;
begin
  MaxWidth := 0;
  for Index := 0 to Items.Count - 1 do
  begin
    if Canvas.TextWidth(Items[Index]) > MaxWidth then
    begin
      MaxWidth := Canvas.TextWidth(Items[Index]);
    end;
  end;
  MaxWidth := MaxWidth + GetSystemMetrics(SM_CXFRAME);
  Perform(LB_SETHORIZONTALEXTENT,MaxWidth,0);
end;

procedure TScrollListBox.ScrollTo(Index: Integer);
begin
  Perform(LB_SETTOPINDEX,Index,0);
end;

{ ----------------------------------------------------------------------------
  TPaintMemo
  ---------------------------------------------------------------------------- }

constructor TPaintMemo.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TPaintMemo);
  FShowCaption := True;
  FCaptionFont := TFont.Create;
  FCaptionFont.Assign(Font);
  FCaptionFont.Color := clGray;
  FCaptionFont.Style := [fsItalic];
  FCaptionMode := mcmUnfocusedOnly;
  FCaptionVisible := False;
  FCaptionPosition.X := 5;
  FCaptionPosition.Y := 0;
end;

destructor TPaintMemo.Destroy;
begin
  FAbout.Free;
  FCaptionFont.Free;
  inherited;
end;

procedure TPaintMemo.Clear;
{ Hier wurde ein Bug behoben, bei dem das OnChange-Ereignis beim aufrufen der
  "Clear"-Methode nicht aufgerufen wurde. }
begin
  inherited;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TPaintMemo.SetShowCaption(Value: Boolean);
begin
  FShowCaption := Value;
  Repaint;
end;

procedure TPaintMemo.SetCaptionFont(Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Repaint;
end;

procedure TPaintMemo.WMPaint(var Message: TWMPaint);
var
  CCanvas: TControlCanvas;
  CRect: TRect;
begin
  inherited;
  FCaptionVisible := False;
  if FShowCaption and (not (csDestroying in ComponentState)) then
  begin
    if ((not Focused) or (CaptionMode = mcmAnyState)) and (Length(Text) < 1) then
    begin
      CCanvas := TControlCanvas.Create;
      try
        CCanvas.Control := Self;
        CCanvas.Brush.Color := Color;
        CRect := ClientRect;
        InflateRect(CRect,3,3);
        CCanvas.FillRect(CRect);
        CCanvas.Font.Assign(FCaptionFont);
        CCanvas.TextOut(FCaptionPosition.X,FCaptionPosition.Y,FCaption);
      finally
        CCanvas.Free;
      end;
      FCaptionVisible := True;
    end;
  end;
  if Assigned(OnPaint) then
  begin
    OnPaint(Self);
  end;
end;
{ ----------------------------------------------------------------------------
  TShortcutLabelFont
  ---------------------------------------------------------------------------- }

constructor TShortcutLabelFont.Create(ALabel: TShortcutLabel);
begin
  FLabel := ALabel;
  FDefault := TFont.Create;
  FHovered := TFont.Create;
  FPressed := TFont.Create;
  Default.OnChange := FontChange;
  Default.Color := clHotlight;
  Hovered.OnChange := FontChange;
  Hovered.Color := clHighlight;
  Hovered.Style := [fsUnderline];
  Pressed.OnChange := FontChange;
  Pressed.Color := clHotlight;
  Pressed.Style := [fsUnderline];
  Visited := clPurple;
end;

destructor TShortcutLabelFont.Destroy;
begin
  FLabel := nil;
  FDefault.Free;
  FHovered.Free;
  FPressed.Free;
  inherited;
end;

procedure TShortcutLabelFont.Update;
begin
  case FLabel.State of
    slsDefault: (FLabel as TLabel).Font.Assign(Default);
    slsHovered: (FLabel as TLabel).Font.Assign(Hovered);
    slsPressed: (FLabel as TLabel).Font.Assign(Pressed);
  end;
  if FLabel.HighlightVisited and TargetVisited(FLabel.Target) then
  begin
    (FLabel as TLabel).Font.Color := Visited;
  end;
end;

procedure TShortcutLabelFont.SetDefault(Value: TFont);
begin
  FDefault.Assign(Value);
  Update;
end;

procedure TShortcutLabelFont.SetHovered(Value: TFont);
begin
  FHovered.Assign(Value);
  Update;
end;

procedure TShortcutLabelFont.SetPressed(Value: TFont);
begin
  FPressed.Assign(Value);
  Update;
end;

procedure TShortcutLabelFont.SetVisited(Value: TColor);
begin
  FVisited := Value;
  Update;
end;

procedure TShortcutLabelFont.FontChange(Sender: TObject);
begin
  Update;
end;

{ ----------------------------------------------------------------------------
  TShortcutLabel
  ---------------------------------------------------------------------------- }

constructor TShortcutLabel.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TShortcutLabel);
  FAutoOpenTarget := True;
  FState := slsDefault;
  FFont := TShortcutLabelFont.Create(Self);
  FHighlightVisited := True;
  FStoreVisited := True;
  Cursor := crHandPoint;
  RegisterToList;
end;

destructor TShortcutLabel.Destroy;
begin
  FAbout.Free;
  FFont.Free;
  UnregisterFromList;
  inherited;
end;

procedure TShortcutLabel.Click;
var
  Index: Integer;
  CanOpen: Boolean;
begin
  inherited;
  if AutoOpenTarget and (Length(Target) <> 0) then
  begin
    CanOpen := True;
    if Assigned(OnOpenTargetQuery) then
    begin
      OnOpenTargetQuery(Self,CanOpen);
    end;
    if CanOpen then
    begin
      ExecuteFile(Target);
      if StoreVisited then
      begin
        VisitedTargets.Add(Target);
        for Index := 0 to ShortcutLabels.Count - 1 do
        begin
          (ShortcutLabels.Items[Index] as TShortcutLabel).Font.Update;
        end;
      end;
      if Assigned(OnOpenTarget) then
      begin
        OnOpenTarget(Self);
      end;
    end;
  end;
end;

procedure TShortcutLabel.SetFont(Value: TShortcutLabelFont);
begin
  FFont := Value;
  FFont.Update;
end;

procedure TShortcutLabel.RegisterToList;
begin
  if ShortcutLabels = nil then
  begin
    ShortcutLabels := TObjectList.Create(False);
  end;
  ShortcutLabels.Add(Self);
end;

procedure TShortcutLabel.UnregisterFromList;
begin
  if ShortcutLabels.Count = 1 then
  begin
    FreeAndNil(ShortcutLabels);
  end;
end;

procedure TShortcutLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if State <> slsHovered then
  begin
    FState := slsHovered;
    Font.Update;
  end;
end;

procedure TShortcutLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FState := slsDefault;
  Font.Update;
end;

procedure TShortcutLabel.WMLButtonDown(var Message: TWMLButtonDown);
begin
  inherited;
  FState := slsPressed;
  Font.Update;
end;

procedure TShortcutLabel.WMLButtonUp(var Message: TWMLButtonUp);
begin
  inherited;
  FState := slsHovered;
  Font.Update;
end;

{ ----------------------------------------------------------------------------
  TSizePanel
  ---------------------------------------------------------------------------- }

constructor TSizePanel.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TSizePanel);
  FAllow := [spaResize,spaMove];
  FBorderStyle := bsNone;
end;

destructor TSizePanel.Destroy;
begin
  FAbout.Free;
  inherited;
end;

procedure TSizePanel.SetBorderStyle(Value: TPanelBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    if Value <> bsSizeable then
    begin
      (Self as TPanel).BorderStyle := Value;
    end;
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TSizePanel.WMNCHitTest(var Message: TWMNCHitTest);
var
  ScreenPos: TPoint;
  MoveArea: TRect;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    ScreenPos := ScreenToClient(Point(Message.XPos,Message.YPos));
    MoveArea := Rect(BevelWidth,BevelWidth,Width - BevelWidth,Height - BevelWidth);
    if (spaResize in Allow) and (BorderStyle <> bsSizeable) then
    begin
      if ScreenPos.X < BevelWidth then
      begin
        if ScreenPos.Y < BevelWidth then
        begin
          Message.Result := HTTOPLEFT;
        end else
        begin
          if ScreenPos.Y >= Height - BevelWidth then
          begin
            Message.Result := HTBOTTOMLEFT;
          end else
          begin
            Message.Result := HTLEFT;
          end;
        end;
      end else
      begin
        if ScreenPos.X >= Width - BevelWidth then
        begin
          if ScreenPos.Y < BevelWidth then
          begin
            Message.Result := HTTOPRIGHT;
          end else
          begin
            if ScreenPos.Y >= Height - BevelWidth then
            begin
              Message.Result := HTBOTTOMRIGHT;
            end else
            begin
              Message.Result := HTRIGHT;
            end;
          end;
        end else
        begin
          if ScreenPos.Y < BevelWidth then
          begin
            Message.Result := HTTOP;
          end else
          begin
            if ScreenPos.Y >= Height - BevelWidth then
            begin
              Message.Result := HTBOTTOM;
            end;
          end;
        end;
      end;
    end;
    if (Message.Result = HTCLIENT) and PtInRect(MoveArea,ScreenPos) and (spaMove in Allow) then
    begin
      Message.Result := HTCAPTION;
    end;
  end;
end;

procedure TSizePanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if NewStyleControls and Ctl3D and (BorderStyle = bsSizeable) then
  begin
    Params.Style := Params.Style or WS_SIZEBOX and not WS_BORDER;
    Params.ExStyle := Params.ExStyle and not WS_EX_CLIENTEDGE;
  end;
end;

{ ----------------------------------------------------------------------------
  TPathEdit
  ---------------------------------------------------------------------------- }

constructor TPathEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TPathEdit);
  FAutoSuggest := True;
  FAutoAppend := True;
  FPaths := [pepURL_History,pepURL_Recent,pepFS_Dirs,pepFS_Shell];
end;

destructor TPathEdit.Destroy;
begin
  FAbout.Free;
  inherited;
end;

procedure TPathEdit.SetPaths(Value: TPathEditPaths);
begin
  UpdateAutoComplete;
  FPaths := Value;
end;

procedure TPathEdit.SetAutoSuggest(Value: Boolean);
begin
  UpdateAutoComplete;
  FAutoSuggest := Value;
end;

procedure TPathEdit.SetAutoAppend(Value: Boolean);
begin
  UpdateAutoComplete;
  FAutoAppend := Value;
end;

procedure TPathEdit.UpdateAutoComplete;
const
  SHACF_FILESYS_DIRS = $00000020;
var
  AutoCompleteOptions: DWORD;
begin
  if AutoSuggest then
  begin
    AutoCompleteOptions := SHACF_AUTOSUGGEST_FORCE_ON;
  end else
  begin
    AutoCompleteOptions := SHACF_AUTOSUGGEST_FORCE_OFF;
  end;
  if AutoAppend then
  begin
    AutoCompleteOptions := AutoCompleteOptions or SHACF_AUTOAPPEND_FORCE_ON;
  end else
  begin
    AutoCompleteOptions := AutoCompleteOptions or SHACF_AUTOAPPEND_FORCE_OFF;
  end;
  if pepURL_History in Paths then
  begin
    AutoCompleteOptions := AutoCompleteOptions or SHACF_URLHISTORY;
  end;
  if pepURL_Recent in Paths then
  begin
    AutoCompleteOptions := AutoCompleteOptions or SHACF_URLMRU;
  end;
  if pepFS_Dirs in Paths then
  begin
    AutoCompleteOptions := AutoCompleteOptions or SHACF_FILESYS_DIRS;
  end;
  if pepFS_Shell in Paths then
  begin
    AutoCompleteOptions := AutoCompleteOptions or SHACF_FILESYSTEM;
  end;
  AutoCompleteOptions := AutoCompleteOptions or SHACF_FILESYS_ONLY;
  SHAutoComplete(Self.Handle,AutoCompleteOptions or SHACF_FILESYS_ONLY);
end;

{ ----------------------------------------------------------------------------
  TValueEdit
  ---------------------------------------------------------------------------- }

constructor TValueEdit.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TValueEdit);
  FAllow := [veaNumbers,veaSeparators];
end;

destructor TValueEdit.Destroy;
begin
  FAbout.Free;
  inherited;
end;

procedure TValueEdit.SetMinValue(Value: Extended);
begin
  if True then

end;

procedure TValueEdit.SetMaxValue(Value: Extended);
begin

end;

procedure TValueEdit.SetAllow(Value: TValueEditAllow);
begin

end;

procedure TValueEdit.SetMaxSeparators(Value: Byte);
begin

end;

procedure TValueEdit.SetSeparatorChar(Value: Char);
begin

end;

procedure TValueEdit.Change;
begin
  if False then
  begin
    inherited;
  end;
end;

initialization

finalization
  if VisitedTargets <> nil then
  begin
    VisitedTargets.Free;
  end;

end.
