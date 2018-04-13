program ActionManager;

uses
  System.StartUpCopy,
  FMX.Forms,
  AcionManager_Frm in 'AcionManager_Frm.pas' {Frm_ActionManager};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrm_ActionManager, Frm_ActionManager);
  Application.Run;
end.
