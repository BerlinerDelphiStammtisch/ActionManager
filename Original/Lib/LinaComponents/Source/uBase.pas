unit uBase;

//////////////////////////////////////
///  Lina Base Unit                ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

  {$I 'Config.inc'}

interface

  { Dies ist die Basis-Unit für die Lina-Komponenten bzw. Methoden und Klassen.
    Diese Unit soll nur indirekt über Lina-Komponenten und ggf. Klassen
    eingebunden werden! }

uses
  { Standard-Units }
  SysUtils, Classes, Dialogs, uSysTools;

const
  UnitName = 'uBase';
  LinaVersion = 1.00;
  About_Title = 'About...';
  { Komponenten-Informationen }
  {$IFDEF ADD_SINGLECATEGORY}
    ComponentsPage = 'Lina';
  {$ELSE}
    ComponentsPage_System = 'Lina - System';
    ComponentsPage_Advanced = 'Lina - Advanced';
    ComponentsPage_Form = 'Lina - Form';
    ComponentsPage_File = 'Lina - File';
    ComponentsPage_Web = 'Lina - Web';
    ComponentsPage_Misc = 'Lina - Misc';
  {$ENDIF}
  ComponentsVersion = '1.0';
  ComponentsCopyright = 'Copyright © 2017';
  ComponentsAuthor = 'Dennis Göhlert a.o.';
  ComponentsHomepage = '';

type
  TComponentAbout = class
  private
    { Private-Deklarationen }
    FComponent: TComponentClass;
    FName: TComponentName;
    FVersion: ShortString;
    FAuthor: ShortString;
    FCopyright: ShortString;
    FHomepage: ShortString;
  protected
    { Protected-Deklarationen }
    property Component: TComponentClass read FComponent write FComponent;
  published
    { Published-Deklarationen }
    property Name: TComponentName read FName;
    property Version: ShortString read FVersion;
    property Copyright: ShortString read FCopyright;
    property Author: ShortString read FAuthor;
    property Homepage: ShortString read FHomepage;
  public
    { Public-Deklarationen }
    constructor Create(Component: TComponentClass; Ver:
      ShortString = ComponentsVersion; Copy: ShortString = ComponentsCopyright;
      Auth: ShortString = ComponentsAuthor;
      Home: ShortString = ComponentsHomepage);
    { Über-Dialog }
    procedure AboutDlg;
  end;

implementation

constructor TComponentAbout.Create(Component: TComponentClass;
  Ver: ShortString = ComponentsVersion; Copy: ShortString = ComponentsCopyright;
  Auth: ShortString = ComponentsAuthor; Home: ShortString = ComponentsHomepage);
begin
  FComponent := Component;
  FName := ExtractClassName(Component.ClassName);
  FVersion := Ver;
  FCopyright := Copy;
  FAuthor := Auth;
  FHomepage := Homepage;
end;

procedure TComponentAbout.AboutDlg;
begin
  {$IFDEF NO_VISTA}
    { MessageDlg, falls der Compiler KEINE Vista-Dialoge unterstützt }
    MessageDlg(
  {$ELSE}
    { TaskMessageDlg, falls der Compiler Vista-Dialoge unterstützt }
    TaskMessageDlg(
                   About_Title,
  {$ENDIF}
                 { ---------------------------------- }
                   Name + ' v'
                 + Version + sLineBreak
                 + Copyright + ' ' + Author + sLineBreak
                 + Homepage,
                 { ---------------------------------- }
                   mtInformation,
                 { ---------------------------------- }
                   [mbClose],0)
end;

end.
