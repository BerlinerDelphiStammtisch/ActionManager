unit AcionManager_Frm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.ImageList, System.Actions,
  {Visuelle Elemente}
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.ListView,
  FMX.Menus, FMX.DialogService, FMX.ActnList,
  FMX.ScrollBox, FMX.Memo,
  {FireDAC}
  FireDAC.UI.Intf, FireDAC.FMXUI.Wait, FireDAC.Comp.UI,
  FireDAC.Phys.MSAccDef, FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Phys.ODBCBase,
  FireDAC.Phys.MSAcc, FMX.ImgList,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Stan.Option,
  {ActionManager-OBjekte}
  Obj_ActionManager, Obj_Action, Obj_ActionOption,
  Collection_BasicObjects;
type
  TFrm_ActionManager = class(TForm)
    LVw_Actions: TListView;
    CmB_ActionCategory: TComboBox;
    Edt_SearchName: TEdit;
    Btn_Start: TButton;
    FDPhysMSAccessDriverLink1: TFDPhysMSAccessDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Lab_ActionCall: TLabel;
    Pop_ActionEdit: TPopupMenu;
    Btn_AddAction: TButton;
    Men_ActionDelete: TMenuItem;
    Men_ActionEdit: TMenuItem;
    StyleBook1: TStyleBook;
    Btn_Reload: TButton;
    Men_ActionDuplicate: TMenuItem;
    Men_GroupLine1: TMenuItem;
    Men_CategorySet: TMenuItem;
    Men_FilePathOpen: TMenuItem;
    Edt_ActionCategory: TEdit;
    Lab_SearchName: TLabel;
    Lab_SearchCategory: TLabel;
    Men_ActionStart: TMenuItem;
    ActionImageList: TImageList;
    RdB_All: TRadioButton;
    RdB_Folder: TRadioButton;
    RdB_Executable: TRadioButton;
    RdB_Call: TRadioButton;
    RdB_File: TRadioButton;
    Button1: TButton;
    ActionList1: TActionList;
    Act_ActionEdit: TAction;
    Act_ActionExecute: TAction;
    MenuItem1: TMenuItem;
    Act_ActionNew: TAction;
    Lab_LastCall: TLabel;
    Lab_CallCount: TLabel;
    CmB_FileFilter: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CmB_ActionCategoryChange(Sender: TObject);
    procedure Men_ActionDeleteClick(Sender: TObject);
    procedure LVw_ActionsDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure LVw_ActionsDragOver(Sender: TObject; const Data: TDragObject;
      const Point: TPointF; var Operation: TDragOperation);
    procedure Btn_ReloadClick(Sender: TObject);
    procedure Men_CategorySetClick(Sender: TObject);
    procedure LVw_ActionsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure Men_FilePathOpenClick(Sender: TObject);
    procedure Pop_ActionEditPopup(Sender: TObject);
    procedure Edt_SearchNameChangeTracking(Sender: TObject);
    procedure Men_ActionDuplicateClick(Sender: TObject);
    procedure Edt_ActionCategoryChangeTracking(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure RdB_AllChange(Sender: TObject);
    procedure RdB_FolderChange(Sender: TObject);
    procedure RdB_ExecutableChange(Sender: TObject);
    procedure RdB_CallChange(Sender: TObject);
    procedure RdB_FileChange(Sender: TObject);
    procedure Act_ActionEditExecute(Sender: TObject);
    procedure Act_ActionExecuteExecute(Sender: TObject);
    procedure Act_ActionNewExecute(Sender: TObject);
    procedure CmB_FileFilterChange(Sender: TObject);
  private
    FActionManager : TActionManager;
    procedure StartExplorer(APath: string);
    procedure SetActionsToForm(AActionCollection: TCollection_BasicObjects);
    procedure SetCategoriesToForm;
    procedure GetSearchFromForm(ASearch: TAM_Action);
    procedure GetCategoryFromForm(ASearch: TAM_Action);
    procedure int_AddAction(AName,ACategory,ACall,AType: string);
    procedure int_SetCategoryFromEdit;
    procedure int_LoadFromRest;
  protected
  end;

var
  Frm_ActionManager: TFrm_ActionManager;

implementation

{$R *.fmx}
{$R *.XLgXhdpiTb.fmx ANDROID}

uses ActionEdit;

const CConnectMDB = 'DriverID=MSAcc;Database=%s';
      CProgName   = 'Action-Manager';
      ASK_DirCreate = 'Soll das Verzeichnis %s erstellt werden?';
      ERR_DirCreate = 'Das Verzeichnis %s konnte nicht erstellt werden?';
      TelSoft = 'C:\Program Files (x86)\Alcatel_PIMphony\aocphone.exe ';

      TypExecFile  = 'ExecFile';
      TypOpenDir   = 'OpenDir';
      TypOpenURL   = 'OpenURL';
      TypCallPhone = 'CallPhone';

      Ask_DeleteAction = 'Soll die Aktion "%s" gelöscht werden?';

      C_TXT_LastCall = 'Letzer Aufruf: ';
      C_TXT_Never    = 'noch nie';
      C_TXT_Calls    = 'Aufrufe: ';

{$Region '-------------------------- Actions of Action Manager ---------------------------'}
procedure TFrm_ActionManager.Act_ActionExecuteExecute(Sender: TObject);
var ActionObj  : TAM_Action;
    ActionItem : TListViewItem;
begin
  if LVw_Actions.ItemIndex=-1 then Exit;

  ActionItem:=LVw_Actions.Items[LVw_Actions.ItemIndex];
  if ActionItem=nil then Exit;

  ActionObj:=FActionManager.Actions.At(ActionItem.Tag);
  {Programm oder Datei über verknüpfte Anwendung ausführen}
  if ActionObj.ActionType=TypExecFile then
    if FActionManager.CheckFileExists(ActionObj.ActionCall) then
      FActionManager.StartExecuteFile(ActionObj.ActionCall);
  {Explorer mit Verzeichnis starten}
  if ActionObj.ActionType=TypOpenDir then
    StartExplorer(ActionObj.ActionCall);
  {URL im Standardbrowser aufrufen}
  if ActionObj.ActionType=TypOpenURL then
    FActionManager.StartExecuteFile(ActionObj.ActionCall);
  {Telefonanruf starten}
  if ActionObj.ActionType=TypCallPhone then
    FActionManager.StartExecuteFile(TelSoft,ActionObj.ActionCall);

  FActionManager.Action_CallExecuted(ActionObj);
end;

procedure TFrm_ActionManager.Act_ActionNewExecute(Sender: TObject);
var Category: string;
begin
  {neues Action-Objekt erstellen, wenn eine Kategorie gerade aktiv ist, dann diese eintragen}
  if CmB_ActionCategory.ItemIndex>-1 then Category:=CmB_ActionCategory.Items[CmB_ActionCategory.ItemIndex]
                                     else Category:=EmptyStr;
  int_AddAction(EmptyStr,Category,EmptyStr,EmptyStr);
end;

procedure TFrm_ActionManager.Act_ActionEditExecute(Sender: TObject);
var ActionObj  : TAM_Action;
    ActionItem : TListViewItem;
begin
  if LVw_Actions.ItemIndex=-1 then Exit;

  ActionItem:=LVw_Actions.Items[LVw_Actions.ItemIndex];
  if ActionItem=nil then Exit;

  ActionObj:=FActionManager.Actions.At(ActionItem.Tag);

  if Dlg_ActionEdit(ActionObj,false) then
  begin
    FActionManager.Action_SaveAction(ActionObj);
    ActionItem.Text:=ActionObj.ActionName;
  end;
end;
{$EndRegion}

procedure TFrm_ActionManager.Btn_ReloadClick(Sender: TObject);
begin
  {-- Einschränkungen aufheben --}
  CmB_ActionCategory.ItemIndex:=-1;
  Edt_ActionCategory.Text:=EmptyStr;
  Edt_SearchName.Text:=EmptyStr;
  {-- Kollektion und Formularelemente leer machen --}
  FActionManager.Actions.FreeAll;
  FActionManager.ActCategories.Clear;
  {-- Daten holen --}
  FActionManager.ReadActions(CAM_FldActName);
  {-- Formularelemente neu füllen --}
  SetActionsToForm(FActionManager.Actions);
  SetCategoriesToForm;
end;

procedure TFrm_ActionManager.CmB_ActionCategoryChange(Sender: TObject);
begin
  SetActionsToForm(FActionManager.Actions);
end;

procedure TFrm_ActionManager.CmB_FileFilterChange(Sender: TObject);
begin
  SetActionsToForm(FActionManager.Actions);
end;

procedure TFrm_ActionManager.Edt_ActionCategoryChangeTracking(Sender: TObject);
begin
  int_SetCategoryFromEdit;
end;

procedure TFrm_ActionManager.Edt_SearchNameChangeTracking(Sender: TObject);
begin
  SetActionsToForm(FActionManager.Actions);
end;

{$Region '--------------------------- Formularmethoden -----------------------------------'}
procedure TFrm_ActionManager.FormCreate(Sender: TObject);
begin
  {Objekte erstellen}
  FActionManager := TActionManager.Create;
  FActionManager.ModusLocalDB := false;
  FActionManager.ModusREST := true;

  {Datenbank öffnen, die Datenbank wird mit dem Programmnamen im Programmverzeichnis erwartet}
  if FActionManager.ModusLocalDB then
  begin
    FActionManager.ActionManagerDB.DBOpen(Format(CConnectMDB,[ChangeFileExt(ParamStr(0),'.mdb')]));
    {Einstellungen von letzter Sitzung einlesen und setzen}
    FActionManager.ReadActOptions(CAM_FldActName);
  end;

  {Aus DB lesen, in Formularelemente bringen}
  FActionManager.ReadActions(CAM_FldActName);

  Edt_ActionCategory.Text := FActionManager.GetLastCategory;
  Edt_SearchName.Text := FActionManager.GetLastName;

  SetActionsToForm(FActionManager.Actions);
  SetCategoriesToForm;
  int_SetCategoryFromEdit;

  {Beschriftungen}
  Caption:=CProgName;
  Lab_SearchCategory.Text:='Suchkategorie (Strg+K):';
  Lab_SearchName.Text:='Suchname (Strg+F):';
  Lab_ActionCall.Text:=EmptyStr;
  Lab_LastCall.Text:=EmptyStr;
  Lab_CallCount.Text:=EmptyStr;

  {Radiobuttons}
  RdB_All.Text := '&Alles';
  RdB_Folder.Text := '&Ordner';
  RdB_Executable.Text := 'A&usführen';
  RdB_Call.Text := 'An&rufen';
  RdB_File.Text := '&Dateien';
  RdB_All.IsChecked := true;

  CmB_FileFilter.Items.Add('doc');
  CmB_FileFilter.Items.Add('exe');
  CmB_FileFilter.Items.Add('pdf');
  CmB_FileFilter.Items.Add('xls');
  CmB_FileFilter.ItemIndex := 0;

end;

procedure TFrm_ActionManager.FormDestroy(Sender: TObject);
var Category: string;
begin
  if CmB_ActionCategory.ItemIndex>-1 then Category:=CmB_ActionCategory.Items[CmB_ActionCategory.ItemIndex]
                                     else Category:=EmptyStr;
  FActionManager.SaveOptions(Category,Edt_SearchName.Text);
  if FActionManager.ModusLocalDB then
    FActionManager.ActionManagerDB.DBClose;
  FreeAndNil(FActionManager);
  {}
end;

procedure TFrm_ActionManager.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if (chr(Key)='F') and (ssCtrl in Shift) then Edt_SearchName.SetFocus;
  if (chr(Key)='K') and (ssCtrl in Shift) then Edt_ActionCategory.SetFocus;
  if (chr(Key)='T') and (ssCtrl in Shift) then int_LoadFromRest;
end;
{$EndRegion}

procedure TFrm_ActionManager.SetActionsToForm(AActionCollection: TCollection_BasicObjects);
var I : integer;
    SearchObj,
    ActionObj  : TAM_Action;
    ActionItem : TListViewItem;
begin
  SearchObj:=TAM_Action.Create(CAM_Actions,FACtionManager.Strukturen.GetTabStr(CAM_Actions).Structure);
  GetSearchFromForm(SearchObj);
  GetCategoryFromForm(SearchObj);

  LVw_Actions.BeginUpdate;
  LVw_Actions.Items.Clear;
  try
    for I:=0 to AActionCollection.Count-1 do
    begin
      ActionObj := AActionCollection.At(I);

      {Datensatz überspringen, der Optionen enthält}
      if ActionObj.Ident = FActionManager.OptRecordIdent then Continue;

      if ActionObj.Matches(SearchObj) then
      begin
        ActionItem := LVw_Actions.Items.Add;
        ActionItem.Text := ActionObj.ActionName;
        ActionItem.Detail := ActionObj.ActionCall;
        ActionItem.Tag := I;

        {Hauptfarbe je Kategorie, kann durch Dateityp s.u. überschrieben werden}
        if ActionObj.ActionType=TypExecFile then ActionItem.Objects.TextObject.TextColor:=TAlphaColorRec.Red
          else if ActionObj.ActionType=TypOpenDir then ActionItem.Objects.TextObject.TextColor :=  TAlphaColorRec.Navy
            else if ActionObj.ActionType=TypOpenURL then ActionItem.Objects.TextObject.TextColor :=  TAlphaColorRec.Fuchsia
              else if ActionObj.ActionType=TypCallPhone then ActionItem.Objects.TextObject.TextColor :=  TAlphaColorRec.Black;

        {Darstellung Image und Schriftfarbe nach Dateityp}
        {Die Zuweisung der Darstellung erfolgt nach Dateiendungen, Aktionstyp oder konkreten Programmen}
        if ActionObj.ActionCall.Contains('.doc') then
        begin
          ActionItem.Objects.TextObject.TextColor:=TAlphaColorRec.Blue;
          ActionItem.ImageIndex:=0;
        end;
        if ActionObj.ActionCall.Contains('.xls') then
        begin
          ActionItem.Objects.TextObject.TextColor:=TAlphaColorRec.Green;
          ActionItem.ImageIndex:=1;
        end;
        if ActionObj.ActionCall.Contains('.dproj') then
        begin
          ActionItem.ImageIndex:=3;
        end;
        if ActionObj.ActionType.Contains(TypCallPhone) then
        begin
          ActionItem.ImageIndex:=4;
        end;
        if ActionObj.ActionType.Contains(TypOpenURL) then
        begin
          ActionItem.ImageIndex:=5;
        end;
        {Mailto ist auch ein Urlaufruf, muss also hinter die allgmeine Form}
        if ActionObj.ActionCall.Contains('mailto') then
        begin
          ActionItem.ImageIndex:=2;
        end;
        if ActionObj.ActionType.Contains(TypOpenDir) then
        begin
          ActionItem.ImageIndex:=6;
        end;
        if ActionObj.ActionCall.Contains('.pdf') then
        begin
          ActionItem.ImageIndex:=7;
        end;
        if (ActionObj.ActionCall.Contains('.exe'))
          or (ActionObj.ActionCall.Contains('.lnk'))
          or (ActionObj.ActionCall.Contains('.bat'))
          then
        begin
          ActionItem.ImageIndex:=8;
        end;
        if ActionObj.ActionCall.Contains('.mdb') or ActionObj.ActionCall.Contains('.accdb') then
        begin
          ActionItem.Objects.TextObject.TextColor :=  TAlphaColorRec.Maroon;
          ActionItem.ImageIndex:=9;
        end;
        if ActionObj.ActionCall.Contains('.rdp') then
        begin
          ActionItem.ImageIndex:=10;
        end;
        if ActionObj.ActionCall.Contains('.ini')
          or ActionObj.ActionCall.Contains('.txt')
          or ActionObj.ActionCall.Contains('.sql')
          then
        begin
          ActionItem.ImageIndex:=11;
        end;
        if ActionObj.ActionCall.Contains('.tc') then
        begin
          ActionItem.ImageIndex:=12;
        end;
        if ActionObj.ActionCall.Contains('geodin') and ActionObj.ActionCall.Contains('.exe') then
        begin
          ActionItem.ImageIndex:=13;
        end;
        if LowerCase(ActionObj.ActionCall).Contains('geodindbanalyser') and ActionObj.ActionCall.Contains('.exe') then
        begin
          ActionItem.ImageIndex:=14;
        end;
        if ActionObj.ActionCall.Contains('.kdbx') then
        begin
          ActionItem.ImageIndex:=15;
        end;
        if ActionObj.ActionCall.Contains('.eddx') then
        begin
          ActionItem.ImageIndex:=16;
        end;
      end;

      {wenn es die Kategorie des aktuellen ActioItems noch nicht gibt, wird sie
        der Kategorienliste hinzugefügt
      }
      if FActionManager.ActCategories.IndexOf(ActionObj.ActionCategory)=-1 then
      begin
        FActionManager.ActCategories.Add(ActionObj.ActionCategory);
      end;
    end;
    {für den Fall einer neuen Kategorie neu sortieren}
    FActionManager.ActCategories.Sort;
  finally
    LVw_Actions.EndUpdate;
    FreeAndNil(SearchObj);
  end;
end;

procedure TFrm_ActionManager.SetCategoriesToForm;
var I : integer;
begin
  CmB_ActionCategory.Items.Clear;
  for I:=0 to FActionManager.ActCategories.Count-1 do
    CmB_ActionCategory.Items.Add(FActionManager.ActCategories[I]);
end;

const C_WirldCard_Multi = '*';

procedure TFrm_ActionManager.GetSearchFromForm(ASearch: TAM_Action);
begin
  if CmB_ActionCategory.ItemIndex>-1 then
    ASearch.ActionCategory:=CmB_ActionCategory.Items[CmB_ActionCategory.ItemIndex];
  if Edt_SearchName.Text<>EmptyStr then
    ASearch.ActionName:=C_WirldCard_Multi+Edt_SearchName.Text+C_WirldCard_Multi;
end;

procedure TFrm_ActionManager.GetCategoryFromForm(ASearch: TAM_Action);
begin
  if RdB_Executable.IsChecked then ASearch.ActionType := TypExecFile;
  if RdB_Folder.IsChecked then ASearch.ActionType := TypOpenDir;
  if RdB_Call.IsChecked then ASearch.ActionType := TypCallPhone;
  if RdB_File.IsChecked then
  begin
    ASearch.ActionType := TypExecFile;
    ASearch.ActionCall := C_WirldCard_Multi+CmB_FileFilter.Items[CmB_FileFilter.ItemIndex]+C_WirldCard_Multi;
  end;
end;

{$Region '--------------------------- interne Dienstleister ------------------------------'}
procedure TFrm_ActionManager.int_AddAction(AName,ACategory,ACall,AType: string);
var NewAction : TAM_Action;
begin
  NewAction := FActionManager.Action_CreateNew(AName,ACategory,ACall,AType);
  if Dlg_ActionEdit(NewAction,True) then
  begin
    FActionManager.Action_InsertNew(NewAction);
    SetActionsToForm(FActionManager.Actions);
    //Lab_DragObject.Caption:=EmptyStr;
  end else FreeAndNil(NewAction);
end;

procedure TFrm_ActionManager.int_SetCategoryFromEdit;
var I : integer;
begin
  if Edt_ActionCategory.Text.IsEmpty then
  begin
    CmB_ActionCategory.ItemIndex:=-1;
    Exit;
  end;

  for I:=0 to CmB_ActionCategory.Items.Count-1 do
  begin
    if CmB_ActionCategory.Items[I].Contains(Edt_ActionCategory.Text) then
    begin
      CmB_ActionCategory.ItemIndex:=I;
      Exit;
    end;
  end;
end;

procedure TFrm_ActionManager.int_LoadFromRest;
begin
  {REST}
  FActionManager.ModusLocalDB := false;
  FActionManager.ModusREST := true;
  FActionManager.ReadActions(EmptyStr);
  {}
  SetActionsToForm(FActionManager.Actions);
  SetCategoriesToForm;
end;
{$EndRegion}

{$Region '------------------------ Drag&Drop an Listview ---------------------------------'}
procedure TFrm_ActionManager.LVw_ActionsDragDrop(Sender: TObject; const Data: TDragObject;
  const Point: TPointF);
var Category : string;
    FileName : string;
begin
  {extrahieren von Dateiname oder letzten Unterverzeichnis als Vorbelegung Name}
  FileName := ExtractFileName(Data.Files[Low(Data.Files)]);
  FileName := FileName.Replace(ExtractFileExt(FileName),EmptyStr);

  if CmB_ActionCategory.ItemIndex>-1 then Category:=CmB_ActionCategory.Items[CmB_ActionCategory.ItemIndex]
                                     else Category:=EmptyStr;
  int_AddAction(FileName,Category,Data.Files[Low(Data.Files)],EmptyStr);

end;

procedure TFrm_ActionManager.LVw_ActionsDragOver(Sender: TObject; const Data: TDragObject;
  const Point: TPointF; var Operation: TDragOperation);
begin
  Operation:=TDragOperation.Copy;
end;
{$EndRegion}

procedure TFrm_ActionManager.LVw_ActionsItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var ActionObj: TAM_Action;
    ActionItem : TListViewItem;
begin
  if LVw_Actions.ItemIndex=-1 then Exit;
  ActionItem:=LVw_Actions.Items[LVw_Actions.ItemIndex];
  if ActionItem=nil then Exit;

  {Anzeige der ausgewählten Aktion}
  ActionObj:=FActionManager.Actions.At(ActionItem.Tag);
  Lab_ActionCall.Text:=ActionObj.ActionCall;
  if ActionObj.ActionCallCnt > 0
    then Lab_LastCall.Text := C_TXT_LastCall + DateTimeToStr(ActionObj.ActionLastCall)
    else Lab_LastCall.Text := C_TXT_LastCall + C_TXT_Never;
  Lab_CallCount.Text:= C_TXT_Calls + IntToStr(ActionObj.ActionCallCnt);
end;

procedure TFrm_ActionManager.Men_FilePathOpenClick(Sender: TObject);
var ActionObj: TAM_Action;
    ActionItem : TListViewItem;
begin
  if LVw_Actions.ItemIndex=-1 then Exit;

  ActionItem:=LVw_Actions.Items[LVw_Actions.ItemIndex];
  if ActionItem=nil then Exit;

  ActionObj:=FActionManager.Actions.At(ActionItem.Tag);

  StartExplorer(ExtractFileDir(ActionObj.ActionCall));
end;

procedure TFrm_ActionManager.Pop_ActionEditPopup(Sender: TObject);
var ActionObj: TAM_Action;
    ActionItem : TListViewItem;
begin
  if LVw_Actions.ItemIndex=-1 then Exit;

  ActionItem:=LVw_Actions.Items[LVw_Actions.ItemIndex];
  if ActionItem=nil then Exit;

  ActionObj:=FActionManager.Actions.At(ActionItem.Tag);

  Men_FilepathOpen.Enabled:=ActionObj.ActionType=TypExecFile;
end;

{$Region '--------------------------- Radiobutton Typ ------------------------------------'}
procedure TFrm_ActionManager.RdB_AllChange(Sender: TObject);
begin
  SetActionsToForm(FActionManager.Actions);
end;

procedure TFrm_ActionManager.RdB_CallChange(Sender: TObject);
begin
  SetActionsToForm(FActionManager.Actions);
end;

procedure TFrm_ActionManager.RdB_ExecutableChange(Sender: TObject);
begin
  SetActionsToForm(FActionManager.Actions);
end;

procedure TFrm_ActionManager.RdB_FileChange(Sender: TObject);
begin
  SetActionsToForm(FActionManager.Actions);
end;

procedure TFrm_ActionManager.RdB_FolderChange(Sender: TObject);
begin
  SetActionsToForm(FActionManager.Actions);
end;
{$EndRegion}

procedure TFrm_ActionManager.Men_ActionDeleteClick(Sender: TObject);
var ActionObj  : TAM_Action;
    ActionItem : TListViewItem;
    R : integer;
begin
  if LVw_Actions.ItemIndex=-1 then Exit;

  ActionItem:=LVw_Actions.Items[LVw_Actions.ItemIndex];
  if ActionItem=nil then Exit;

  ActionObj:=FActionManager.Actions.At(ActionItem.Tag);

  TDialogService.MessageDialog(Format(ASK_DeleteAction,[ActionObj.ActionName]),TMsgDlgType.mtConfirmation,
                              [TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],TMsgDlgBtn.mbNo,0,
    procedure(const AResult: TModalResult)
    begin
      case AResult of
        { Detect which button was pushed and show a different message }
        mrYES:
          R:=0;
        mrNo:
          R:=1;
      end;
    end);

  if R=0 then
  begin
    FActionManager.Action_DeleteAction(ActionObj);
    SetActionsToForm(FActionManager.Actions);
  end;
end;

procedure TFrm_ActionManager.Men_ActionDuplicateClick(Sender: TObject);
var ActionObj: TAM_Action;
    ActionItem : TListViewItem;
begin
  if LVw_Actions.ItemIndex=-1 then Exit;
  ActionItem:=LVw_Actions.Items[LVw_Actions.ItemIndex];
  if ActionItem=nil then Exit;

  ActionObj:=FActionManager.Actions.At(ActionItem.Tag);
  int_AddAction(ActionObj.ActionName,ActionObj.ActionCategory,ActionObj.ActionCall,ActionObj.ActionType);
end;

procedure TFrm_ActionManager.Men_CategorySetClick(Sender: TObject);
var ActionObj: TAM_Action;
    ActionItem : TListViewItem;
begin
  if LVw_Actions.ItemIndex=-1 then Exit;

  ActionItem:=LVw_Actions.Items[LVw_Actions.ItemIndex];
  if ActionItem=nil then Exit;

  ActionObj:=FActionManager.Actions.At(ActionItem.Tag);

  CmB_ActionCategory.ItemIndex:=CmB_ActionCategory.Items.IndexOf(ActionObj.ActionCategory);
  SetActionsToForm(FActionManager.Actions);
end;

procedure TFrm_ActionManager.StartExplorer(APath: string);
var R : integer;
begin
  if not DirectoryExists(APath) then
  begin
  TDialogService.MessageDialog(Format(ASK_DirCreate,[APath]),TMsgDlgType.mtConfirmation,
                              [TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],TMsgDlgBtn.mbNo,0,
    procedure(const AResult: TModalResult)
    begin
      case AResult of
        { Detect which button was pushed and show a different message }
        mrYES:
          R:=0;
        mrNo:
          R:=1;
      end;
    end);

    if R=0 then
      if not ForceDirectories(APath) then ShowMessage(Format(ERR_DirCreate,[APath]))
                                     else FActionManager.StartExplorer(APath);
  end else
    FActionManager.StartExplorer(APath);
end;

end.