unit ServiceUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.SvcMgr, Vcl.Dialogs, IdHTTPWebBrokerBridge, Web.WebReq, WebModuleUnit1;

type
  TService1 = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
    FServer: TIdHTTPWebBrokerBridge;
    procedure startServer;
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  Service1: TService1;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  Service1.Controller(CtrlCode);
end;

function TService1.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TService1.ServiceStart(Sender: TService; var Started: Boolean);
begin
  startServer;
  if WebRequestHandler <> nil then
    WebRequestHandler.WebModuleClass := WebModuleClass;
end;

procedure TService1.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  if Assigned(FServer) then
    FreeAndNil(FServer);
end;

procedure TService1.startServer;
begin
   FServer := TIdHTTPWebBrokerBridge.Create(Self);

  if not FServer.Active then
  begin
    FServer.Bindings.Clear;
    FServer.DefaultPort := 8000;
    FServer.Active := True;
  end;
end;

end.
