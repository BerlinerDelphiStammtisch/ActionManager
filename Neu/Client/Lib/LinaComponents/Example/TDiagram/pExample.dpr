program pExample;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fmMain},
  uValue in 'uValue.pas' {fmValue};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Example';
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmValue, fmValue);
  Application.Run;
end.
