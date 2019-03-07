unit WebModuleUnit1;

interface

uses System.SysUtils, System.Classes, Web.HTTPApp, MVCFramework, MainController;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
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

{%CLASSGROUP 'Vcl.Controls.TControl'}
Uses MVCFramework.commons;

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
var filename:string;
begin
    FileName := GetModuleName(Hinstance);

    FileName := copy(FileName, 0, lastdelimiter('\',GetModuleName(Hinstance)));
    FMVCEngine := TMVCEngine.Create(Self).addController(TMainController);
end;

end.
