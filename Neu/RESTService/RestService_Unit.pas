unit RestService_Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.SvcMgr;

type
  TServiceActMan = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private-Deklarationen }
  public
    function GetServiceController: TServiceController; override;
    { Public-Deklarationen }
  end;

var
  ServiceActMan: TServiceActMan;

implementation

{$R *.dfm}

uses uActionMan.ServerStart;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ServiceActMan.Controller(CtrlCode);
end;

function TServiceActMan.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TServiceActMan.ServiceStart(Sender: TService; var Started: Boolean);
begin
  RunServerAsService(54711);
end;

procedure TServiceActMan.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  StopServer;
end;

end.
