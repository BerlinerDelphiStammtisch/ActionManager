unit uVirtObj;

//////////////////////////////////////
///  Lina Virtual Object Unit      ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

uses
  { Standard-Units }
  SysUtils, Classes,
  { Andere Package-Units }
  uBase;

type
  { Hauptklassen }

  { VirtualObject }
  TVirtualObject = class

  end;

  TVirtualObjects = array of TVirtualObject;

  { VirtualMethod }
  TVirtualMethod = class

  end;

  TVirtualMethods = array of TVirtualMethod;

  { VirtualLirary }
  TVirtualLibray = class

  end;

  TVirtualLibraries = array of TVirtualLibray;

  { VirtualClass }
  TVirtualClass = class

  end;

  TVirtualClasses = array of TVirtualClass;

  { VirtualProgram }
  {$IFNDEF NO_MULTIPLATFORM}
    [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TVirtualProgram = class(TComponent)
  private
    { Private-Deklarationen }
    FAbout: TComponentAbout;
    FObjects: TVirtualObjects;
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published-Deklarationen }
   // property Objects [Index: Integer]: TVirtualObject read FObjects write FObjects;
    { Meta-Daten }
    property About: TComponentAbout read FAbout;
  end;

  {$IFDEF ADD_COMPONENTREG}
    procedure Register;
  {$ENDIF}

implementation

{$IFDEF ADD_COMPONENTREG}
  procedure Register;
  begin
    RegisterComponents({$IFDEF ADD_SINGLECATEGORY}ComponentsPage{$ELSE}ComponentsPage_Misc{$ENDIF},[TVirtualProgram]);
  end;
{$ENDIF}

constructor TVirtualProgram.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := TComponentAbout.Create(TVirtualProgram);
end;

destructor TVirtualProgram.Destroy;
begin
  FAbout.Free;
  inherited;
end;

end.
