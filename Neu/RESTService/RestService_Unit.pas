unit RestService_Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.SvcMgr;

type
  TService1 = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private-Deklarationen }
  public
    function GetServiceController: TServiceController; override;
    { Public-Deklarationen }
  end;

var
  Service1: TService1;

implementation

{$R *.dfm}

uses uActionMan.ServerStart;

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
  RunServerAsService(54711);
end;

procedure TService1.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  StopServer;
end;

end.
