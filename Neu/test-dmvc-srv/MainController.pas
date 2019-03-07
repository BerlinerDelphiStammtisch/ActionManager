unit MainController;

interface

uses MVCFramework;

type
[MVCPath('/')]
TMainController = class (TMVCController)

  [MVCPath('/')]
  procedure index();
end;

implementation

{ TMainController }

procedure TMainController.index;
begin
  Render('Hello world!');
end;

end.
