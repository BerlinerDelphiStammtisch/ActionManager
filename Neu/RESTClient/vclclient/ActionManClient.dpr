program ActionManClient;

uses
  Vcl.Forms,
  Unit2 in 'Unit2.pas' {VclAMClientMainForm},
  uClientActionDM in 'uClientActionDM.pas' {DataModule3: TDataModule},
  Vcl.Themes,
  Vcl.Styles,
  Converters in '..\Converters.pas',
  Writers in '..\Writers.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Sapphire Kamri');
  Application.CreateForm(TVclAMClientMainForm, VclAMClientMainForm);
  Application.Run;
end.
