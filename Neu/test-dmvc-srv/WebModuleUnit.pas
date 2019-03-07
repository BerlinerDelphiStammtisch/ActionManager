unit WebModuleUnit;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework, MainController;

type
  TWebModule1 = class(TWebModule)

    procedure WebModuleCreate(Sender: TObject);
  private
    { Private declarations }
     FMVCEngine: TMVCEngine;
  public
    { Public declarations }
  end;


  var
  WebModuleClass: TComponentClass = TWebModule1;
implementation

{ TWebModule1 }

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  FMVCEngine := TMVCEngine.Create(self);
  FMVCEngine.AddController(TMainController);

end;

end.
