unit uActionManActionObj;

interface

uses System.Generics.Collections, MVCFramework.Serializer.Commons;

const
  clogActionManServer = 'ACTIONMAN_SERVER';

type

  [MVCNameCase(ncLowerCase)]
  TAMAction = class
  private
    FActionCreated: TDateTime;
    FActionCategory: string;
    FActionType: string;
    FActionCall: string;
    FActionLastCall: TDateTime;
    FActionName: string;
    FActionCallCnt: integer;
    FIdent: integer;
    procedure SetActionCall(const Value: string);
    procedure SetActionCallCnt(const Value: integer);
    procedure SetActionCategory(const Value: string);
    procedure SetActionCreated(const Value: TDateTime);
    procedure SetActionLastCall(const Value: TDateTime);
    procedure SetActionName(const Value: string);
    procedure SetActionType(const Value: string);
    procedure SetIdent(const Value: integer);
  public
    property Ident: integer read FIdent write SetIdent;
    property Action_Name: string read FActionName write SetActionName;
    property Action_Call: string read FActionCall write SetActionCall;
    property Action_Type: string read FActionType write SetActionType;
    property Action_Category: string read FActionCategory write SetActionCategory;
    property Action_Created: TDateTime read FActionCreated write SetActionCreated;
    property Action_LastCall: TDateTime read FActionLastCall write SetActionLastCall;
    property Action_CallCnt: integer read FActionCallCnt write SetActionCallCnt;
  end;

  TAMActions = class(TObjectList<TAMAction>);


implementation

{ TAMAction }


{ TAMAction }

procedure TAMAction.SetActionCall(const Value: string);
begin
  FActionCall := Value;
end;

procedure TAMAction.SetActionCallCnt(const Value: integer);
begin
  FActionCallCnt := Value;
end;

procedure TAMAction.SetActionCategory(const Value: string);
begin
  FActionCategory := Value;
end;

procedure TAMAction.SetActionCreated(const Value: TDateTime);
begin
  FActionCreated := Value;
end;

procedure TAMAction.SetActionLastCall(const Value: TDateTime);
begin
  FActionLastCall := Value;
end;

procedure TAMAction.SetActionName(const Value: string);
begin
  FActionName := Value;
end;

procedure TAMAction.SetActionType(const Value: string);
begin
  FActionType := Value;
end;

procedure TAMAction.SetIdent(const Value: integer);
begin
  FIdent := Value;
end;

end.
