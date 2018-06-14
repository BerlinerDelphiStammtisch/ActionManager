program pExample;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Example';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
