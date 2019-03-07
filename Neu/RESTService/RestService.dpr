program RestService;

uses
  Vcl.SvcMgr,
  RestService_Unit in 'RestService_Unit.pas' {ServiceActMan: TService},
  uActionMan.ServerStart in '..\Source\uActionMan.ServerStart.pas',
  uActionManActionObj in '..\Source\uActionManActionObj.pas',
  uActionManController in '..\Source\uActionManController.pas',
  uActionManDatenModul in '..\Source\uActionManDatenModul.pas' {ActionManDataModule: TDataModule},
  uActionManWebModul in '..\Source\uActionManWebModul.pas' {ActionManWebModule: TWebModule};

{$R *.RES}

begin
  // F�r Windows 2003 Server muss StartServiceCtrlDispatcher vor
  // CoRegisterClassObject aufgerufen werden, das indirekt von
  // Application.Initialize aufgerufen werden kann. TServiceApplication.DelayInitialize
  // erm�glicht, dass Application.Initialize von TService.Main (nach
  // StartServiceCtrlDispatcher) aufgerufen werden kann.
  //
  // Eine verz�gerte Initialisierung des Application-Objekts kann sich auf
  // Ereignisse auswirken, die dann vor der Initialisierung ausgel�st werden,
  // wie z.B. TService.OnCreate. Dies wird nur empfohlen, wenn ServiceApplication
  // ein Klassenobjekt bei OLE registriert und f�r die Verwendung mit
  // Windows 2003 Server vorgesehen ist.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TServiceActMan, ServiceActMan);
  Application.CreateForm(TActionManDataModule, ActionManDataModule);
  Application.Run;
end.
