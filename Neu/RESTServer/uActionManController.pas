unit uActionManController;
//
interface

uses
  MVCFramework, MVCFramework.Commons,
  uActionManDatenModul;

const
  cVersion = '1.0.0.0';

type

  [MVCPath('/')]
  TActionManController = class(TMVCController)
  private
    FDM: TActionManDataModule;

  protected
    procedure OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean); override;
    procedure OnAfterAction(Context: TWebContext; const AActionName: string); override;

  public
    [MVCPath('/')]
    [MVCHTTPMethod([httpGET])]
    procedure Index;

    [MVCPath('/reversedstrings/($Value)')]
    [MVCHTTPMethod([httpGET])]
    procedure GetReversedString(const Value: String);

  public
    [MVCPath('/actions')]
    [MVCHTTPMethod([httpGET])]
    procedure ActionList(ctx: TWebContext);

    [MVCPath('/actions/search/($value)')]
    [MVCHTTPMethod([httpGET])]
    procedure FindAction(ctx: TWebContext);

    [MVCPath('/actions/($id)')]
    [MVCHTTPMethod([httpGET, httpDELETE])]
    procedure ActionById(ctx: TWebContext);

    [MVCPath('/actions')]
    [MVCHTTPMethod([httpPOST])]
    procedure SaveAction(ctx: TWebContext);

    [MVCPath('/actions/($id)')]
    [MVCHTTPMethod([httpPUT])]
    procedure UpdateActionById(ctx: TWebContext);

  end;

implementation

uses
  System.SysUtils,
  MVCFramework.Serializer.Commons,
  MVCFramework.Logger, System.StrUtils, uActionManActionObj;

procedure TActionManController.OnBeforeAction(Context: TWebContext; const AActionName: string; var Handled: Boolean);
begin
  { Executed before each action
    if handled is true (or an exception is raised) the actual
    action will not be called }

  inherited;

  FDM := TActionManDataModule.Create(nil);
end;

procedure TActionManController.SaveAction(ctx: TWebContext);
var
  lAction: TAMAction;
begin
  lAction := ctx.Request.BodyAs<TAMAction>;
  try
    FDM.AddAMAction(lAction);
    Log.Info('Action correctly saved', clogActionManServer);
  finally
    lAction.Free;
  end;
end;

procedure TActionManController.UpdateActionById(ctx: TWebContext);
var
  lAction: TAMAction;
begin
  lAction := ctx.Request.BodyAs<TAMAction>;
  try
    FDM.UpdateAMAction(lAction);
    Log.Info('Action correctly updated', clogActionManServer);
  finally
    lAction.Free;
  end;
  Render(200, 'Action updated');
end;

procedure TActionManController.OnAfterAction(Context: TWebContext; const AActionName: string);
begin
  { Executed after each action }
  FDM.DisposeOf;
  inherited;
end;

procedure TActionManController.Index;
begin
  //use Context property to access to the HTTP request and response
  Render('Willkommen beim Delphi-Stammtisch-Berlin Action-Manager. <br/>Version '+cVersion);
end;

procedure TActionManController.GetReversedString(const Value: String);
begin
  Render(System.StrUtils.ReverseString(Value.Trim));
end;

procedure TActionManController.ActionById(ctx: TWebContext);
begin
  // different behaviour according to the request http method
  case ctx.Request.HTTPMethod of
    httpDELETE:
      begin
        FDM.DeleteAMAction(StrToInt(ctx.Request.Params['id']));
        Log.Info('Action deleted', clogActionManServer);
        Render(200, 'Action deleted');
      end;
    httpGET:
      begin
        Render(FDM.GetAMActionById(StrToInt(ctx.Request.Params['id'])), False, dstSingleRecord);
      end
  else
    raise Exception.Create('Invalid http method for action');
  end;

end;

procedure TActionManController.ActionList(ctx: TWebContext);
begin
  Render(FDM.FindAMAction(''), False);
  Log.Info('Getting Actionlist', clogActionManServer);
end;

procedure TActionManController.FindAction(ctx: TWebContext);
begin
  Render(FDM.FindAMAction(ctx.Request.Params['value']), False);
  Log.Info('Find Actions', clogActionManServer);
end;

end.
