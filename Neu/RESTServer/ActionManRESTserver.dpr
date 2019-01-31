program ActionManRESTserver;

 {$APPTYPE CONSOLE}

uses
  System.SysUtils,
  MVCFramework.Logger,
  MVCFramework.Commons,
  MVCFramework.REPLCommandsHandlerU,
  Web.ReqMulti,
  Web.WebReq,
  Web.WebBroker,
  IdHTTPWebBrokerBridge,
  uActionManController in '..\Source\uActionManController.pas',
  uActionManWebModul in '..\Source\uActionManWebModul.pas' {ActionManWebModule: TWebModule},
  uActionManDatenModul in '..\Source\uActionManDatenModul.pas' {ActionManDataModule: TDataModule},
  uActionManActionObj in '..\Source\uActionManActionObj.pas',
  uActionMan.ServerStart in '..\Source\uActionMan.ServerStart.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  IsMultiThread := True;
  try
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    WebRequestHandlerProc.MaxConnections := 1024;
    Writeln('Call:');
    Writeln('http://localhost:54711/');
    Writeln('');
    RunServer(54711);
    //http://localhost:54711/
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
