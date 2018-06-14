unit ActionEdit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Obj_Action, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.ListBox;

type
  TFrm_ActionEdit = class(TForm)
    Lab_ActionCategory: TLabel;
    Lab_ActionName: TLabel;
    Lab_ActionCall: TLabel;
    Lab_ActionType: TLabel;
    Edt_ActionCategory: TEdit;
    Edt_ActionName: TEdit;
    Edt_ActionCall: TEdit;
    CmB_ActionType: TComboBox;
    Btn_Ok: TButton;
    Btn_Cancel: TButton;
    StyleBook1: TStyleBook;
    procedure FormCreate(Sender: TObject);
  private
    procedure int_GetDataFromForm(AAction: TAM_Action);
  public
    { Public-Deklarationen }
  end;

var
  Frm_ActionEdit: TFrm_ActionEdit;

  function Dlg_ActionEdit(AAction: TAM_Action; ANewAction: boolean): boolean;

implementation

{$R *.fmx}

const C_ActionCategory = 'Kategorie';
      C_ActionName = 'Name';
      C_ActionCall = 'Aufruf';
      C_ActionType = 'Typ';
      C_FormTitelNeu = 'ActionManager: neue Aktion';
      C_FormTitelEdt = 'ActionManager: Aktion bearbeiten';

function Dlg_ActionEdit(AAction: TAM_Action; ANewAction: boolean): boolean;
begin
  Frm_ActionEdit:=TFrm_ActionEdit.Create(Application);
  try
    with Frm_ActionEdit do
    begin
      BringToFront;  //für Drag&Drop notwendig

      if ANewAction  then Caption:=C_FormTitelNeu
                     else Caption:=C_FormTitelEdt;

      Lab_ActionCategory.Text:=C_ActionCategory;
      Lab_ActionName.Text:=C_ActionName;
      Lab_ActionCall.Text:=C_ActionCall;
      Lab_ActionType.Text:=C_ActionType;
      Edt_ActionCategory.Text:=AAction.ActionCategory;
      Edt_ActionName.Text:=AAction.ActionName;
      Edt_ActionCall.Text:=AAction.ActionCall;
      CmB_ActionType.ItemIndex:=CmB_ActionType.Items.IndexOf(AAction.ActionType);

      if ANewAction and AAction.ActionType.IsEmpty then
      begin
        if FileExists(AAction.ActionCall) then CmB_ActionType.ItemIndex:=CmB_ActionType.Items.IndexOf('ExecFile')
                                          else CmB_ActionType.ItemIndex:=CmB_ActionType.Items.IndexOf('OpenDir');

      end;
      if ShowModal=mrOk then
      begin
        int_GetDataFromForm(AAction);
        Result:=true;
      end else Result:=false;
    end;
  finally
    Frm_ActionEdit.Free;
  end;
end;

procedure TFrm_ActionEdit.FormCreate(Sender: TObject);
begin
  CmB_ActionType.Items.Add('ExecFile');
  CmB_ActionType.Items.Add('OpenDir');
  CmB_ActionType.Items.Add('OpenURL');
  CmB_ActionType.Items.Add('CallPhone');

  Btn_Ok.Text:='OK';
  Btn_Cancel.Text:='Abbrechen';
end;

procedure TFrm_ActionEdit.int_GetDataFromForm(AAction: TAM_Action);
begin
  AAction.ActionCategory:=Edt_ActionCategory.Text;
  AAction.ActionName:=Edt_ActionName.Text;
  AAction.ActionCall:=Edt_ActionCall.Text;
  AAction.ActionType:=CmB_ActionType.Items[CmB_ActionType.ItemIndex];
end;

end.
