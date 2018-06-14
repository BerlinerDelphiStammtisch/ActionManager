unit uInit;

//////////////////////////////////////
///  Lina Initialization Unit      ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

  { Dies ist die Initialisierungs-Unit für das Lina Components Package.
    Diese Unit darf NIEMALS direkt in eine Laufzeit-Bibliothek eingebunden
    werden! }

uses
  { Standard-Units }
  ToolsAPI, Graphics;

  {$IFDEF ADD_SPLASHENTRY}
    procedure RegisterPackageOnSplash;
  {$ENDIF}
  {$IFDEF ADD_ABOUTENTRY}
    procedure RegisterPackageOnAbout;
    procedure UnregisterPackageOnAbout;
  {$ENDIF}

{$IF Defined(ADD_SPLASHENTRY) OR Defined(ADD_ABOUTENTRY)}
  const
    Package_Name = 'Lina Components';
    {$IFDEF ADD_ABOUTENTRY}
      Package_Description = 'Components and code library for Delphi' + sLineBreak + '© 2017 Dennis Göhlert a.o.';
    {$ENDIF}
    Package_License = 'Mozilla Public License (MPL) 2.0';
    Package_SKU = '(Dev-Preview)';
{$ENDIF}

{$IFDEF ADD_ABOUTENTRY}
  var
    PluginIndex: Integer = -1;
    AboutBitmap: TBitmap;
{$ENDIF}

implementation

{$IFDEF ADD_SPLASHENTRY}
  procedure RegisterPackageOnSplash;
  var
    SplashBitmap: TBitmap;
  begin
    SplashBitmap := TBitmap.Create;
    try
      SplashBitmap.LoadFromResourceName(HInstance,'LINA');
      (SplashScreenServices as IOTASplashScreenServices).AddPluginBitmap(
       Package_Name,SplashBitmap.Handle,False,Package_License,Package_SKU);
    finally
      SplashBitmap.Free;
    end;
  end;
{$ENDIF}


{$IFDEF ADD_ABOUTENTRY}
  procedure RegisterPackageOnAbout;
  begin
    AboutBitmap := TBitmap.Create;
    try
      AboutBitmap.LoadFromResourceName(HInstance,'LINA');
      PluginIndex := (BorlandIDEServices as IOTAAboutBoxServices120).AddPluginInfo(
      Package_Name,Package_Description,AboutBitmap.Handle,False,Package_License,Package_SKU);
    except
      AboutBitmap.Free;
    end;
  end;

  procedure UnregisterPackageOnAbout;
  begin
    try
      (BorlandIDEServices as IOTAAboutBoxServices).RemovePluginInfo(PluginIndex);
    finally
      AboutBitmap.Free;
    end;
  end;
{$ENDIF}

initialization
  { Package-Registrierung }
  {$IFDEF ADD_SPLASHENTRY}
    RegisterPackageOnSplash;
  {$ENDIF}
  {$IFDEF ADD_ABOUTENTRY}
    RegisterPackageOnAbout;
  {$ENDIF}


finalization
  { Package-Deregistrierung }
  {$IFDEF ADD_ABOUTENTRY}
    UnregisterPackageOnAbout;
  {$ENDIF}

end.
