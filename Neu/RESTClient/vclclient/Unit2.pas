unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.StdCtrls,
  MVCFramework.RESTClient,

  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait, FireDAC.Comp.UI, Vcl.ToolWin, Vcl.ComCtrls,
  System.Actions, Vcl.ActnList, Vcl.StdActns, System.ImageList, Vcl.ImgList,
  Vcl.Buttons, Data.Bind.Controls, Data.Bind.Components, Data.Bind.DBScope,
  Vcl.Bind.Navigator, Data.Bind.EngExt, Vcl.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, Vcl.Bind.Editors;

type
  TVclAMClientMainForm = class(TForm)
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Memo1: TMemo;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDMemTable1: TFDMemTable;
    FDMemTable1ident: TIntegerField;
    FDMemTable1action_name: TStringField;
    ToolBar1: TToolBar;
    Splitter1: TSplitter;
    ActionList1: TActionList;
    ImageList1: TImageList;
    actDatenLaden: TAction;
    SpeedButton1: TSpeedButton;
    BindNavigator1: TBindNavigator;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actDatenLadenExecute(Sender: TObject);
  private
    FRESTClient: TRESTClient;
    FLoading: Boolean;
    procedure DatenLaden;
  public
    { Public-Deklarationen }
  end;

var
  VclAMClientMainForm: TVclAMClientMainForm;

implementation

uses
  System.JSON,
  Converters,
//  uClientActionDM,
  MVCFramework.DataSet.Utils;

{$R *.dfm}


procedure TVclAMClientMainForm.FormCreate(Sender: TObject);
begin
  FRESTClient := TRESTClient.Create('localhost', 54711);


end;

procedure TVclAMClientMainForm.FormDestroy(Sender: TObject);
begin
  FRESTClient.free;

 // erst Commit, dann Push

end;

procedure TVclAMClientMainForm.actDatenLadenExecute(Sender: TObject);
begin
  DatenLaden;
end;

procedure TVclAMClientMainForm.DatenLaden;
var
  response: IRESTResponse;
begin
  FDMemTable1.DisableControls;
  Memo1.Lines.BeginUpdate;
  try
    response := FRESTClient.doGET('/actions', []);
    Memo1.Lines.Text := TConverters.JsonReformat(response.BodyAsString, true);
    FDMemTable1.Close;
    FDMemTable1.Open;
    FLoading := True;
    FDMemTable1.LoadFromJSONArrayString(response.BodyAsString);
    FDMemTable1.First;
    FLoading := False;
  finally
    Memo1.Lines.EndUpdate;
    FDMemTable1.EnableControls;
  end;

end;

end.
