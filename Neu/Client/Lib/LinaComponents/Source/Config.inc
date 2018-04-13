//////////////////////////////////////
///  Lina Configuration File       ///
///  ****************************  ///
///  (c) 2018 Dennis Göhlert a.o.  ///
//////////////////////////////////////

{$DEFINE LINA}
{ Bedingte Compilerversions-Ausdrücke }
{$DEFINE VER_UNKNOWN}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF Defined(VER320)}
    {$DEFINE VER_10_2}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER310) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_10_1_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER300) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_10_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER290) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_XE8_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER280) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_XE7_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER270) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_XE6_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER260) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_XE5_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER250) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_XE4_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER240) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_XE3_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER230) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_XE2_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER220) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_XE_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER210) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_2010_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER200) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_2009_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER185) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_2007_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER180) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_2006_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER170) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_2005_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
  {$IF Defined(VER150) OR (NOT Defined(VER_UNKNOWN))}
    {$DEFINE VER_7_UP}
    {$UNDEF VER_UNKNOWN}
  {$ENDIF}
{$ENDIF}
{ Globale Einstellungen }
{$DEFINE ADD_SPLASHENTRY}
{$DEFINE ADD_ABOUTENTRY}
{$DEFINE ADD_COMPONENTREG}
//{$DEFINE ADD_DEPENDENCIES}
{$DEFINE ADD_SINGLECATEGORY}
{$DEFINE WARN_INCOMPATIBLEPLATFORM}
{$DEFINE WARN_INCOMPATIBLECOMPILER}
{$DEFINE WARN_INCOMPATIBLEVERSION}
{ Globale Einschränkungen }
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF NOT Defined(VER_XE2_UP)}
    { Unter früheren Delphi-Versionen als XE2 stand lediglich eine Ziel-Platform
      für kompilierte Anwendungen zur Verfügung. Deshalb brauchte noch nicht auf
      Kompatibilität zu mehrfacher Platform-Unterstützung geachtet werden. }
    {$DEFINE NO_MULTIPLATFORM}
  {$IFEND}
  {$IF (NOT Defined(DCC)) AND Defined(WARN_INCOMPATIBLECOMPILER)}
    { Nur der Delphi-Compiler wird (offiziell) unterstützt. }
    {$MESSAGE ERROR 'Lina Components requires Delphi'}
  {$IFEND}
  {$IF (NOT Defined(NO_MULTIPLATFORM)) AND (NOT Defined(MSWINDOWS)) AND Defined(WARN_INCOMPATIBLECOMPILER)}
    { Nur Windows wird (offiziell) als Ziel-Platform unterstützt. }
    {$MESSAGE ERROR 'Lina Components requires Microsoft Windows'}
  {$IFEND}
  {$IF Defined(VER_UNKNOWN) AND Defined(WARN_INCOMPATIBLEVERSION)}
    { Frühere Delphi-Versionen als Delphi 7 werden nicht (offiziell)
      unterstützt. }
    {$MESSAGE ERROR 'Lina Components requires Delphi 7 or higher'}
  {$IFEND}
  {$IF NOT Defined(VER_2005_UP)}
    { Unter früheren Delphi-Versionen als 2005 gab es noch nicht die
      Möglichkeit, Records mit methoden zu versehen. Die Implementierung von
      Prozeduren und/oder Funktionen war Klassen vorenthalten. }
    {$DEFINE NO_RECORDMETHODS}
  {$IFEND}
  {$IF NOT Defined(VER_2007_UP)}
    { Unter früheren Delphi-Versionen als 2007 gab es (offiziell) noch keine
      Unterstützung für Windows-Vista-spezifische funktionen wie die TaskDialog-
      Komponente oder einige Funktionen.
      Damit dies nicht zu Problemen führt, geht der Compiler darauf mithilfe
      dieser Direktive ein. }
    {$DEFINE NO_VISTA}
  {$IFEND}
  {$IF NOT Defined(VER_2009_UP)}
    { Unter früheren Delphi-Versionen als 2009 war die Nutzung generischer
      Datentypen (Generics) nicht möglich. Da es ohne diese nahezu unmöglich
      scheint, typisierte Objekte, wie zum Beispiel typisierte TObjectList's
      zu deklarieren, fallen diese Deklarationen unter früheren Delphi-Versionen
      weg. Dies kann eventuell zu kompatibilitätsproblemen zwischen
      unterschiedlichen Nutzern dieser Komponenten führen, lässt sich aber nicht
      vermeiden. }
    {$DEFINE NO_GENERIC}
    { Unter früheren Delphi-Versionen als 2009 entsprach der String-Typenalias
      dem AnsiString-Typen. Da es gegebenfalls bei manchen externen (zB. OS-
      Spezifischen) Klassen zu Kompatibilitätsproblemen kommmen könnte, wird
      hier eine Überprüfung der Unicode-Unterstützung durchgeführt.
      Zu beachten ist jedoch, dass Delphi 2009 und höher AnsiStrings implizit
      in UnicodeStrings umwandelt und umgekehrt (sofern möglich). }
    {$DEFINE NO_UNICODE}
	{ Unter früheren Delphi-Versionen als 2009 standen Namensräume noch nicht
	  zur verfügung. Daher sind Unitnamen wie zB. "System.Win.Registry" nicht
	  verfügbar. Allerdings müssen diese trotzdem in nicht-GUI-Anwendungen
	  verwendet werden, da dort keine Unit-Aliase für VCL/FMX voreingestellt
	  sind. }
	{$DEFINE NO_NAMESPACE}
  {$IFEND}
  {$IF NOT Defined(VER_2010_UP)}
    { Unter früheren Delphi-Versionen als 2010 gab es ein Problem mit dem
      "crHandPoint" cursor, sodass dieser nicht korrekt unter Windows Vista oder
      höher dargestellt wurde, sondern stattdessen im alten Windows-Look.
      Bei älteren Delphi-Versionen als 2010 steht deswegen eine Pseudo-
      Komponente zur Behebung dieses Problems zur Verfügung. }
    {$DEFINE NO_HANDPOINT}
  {$IFEND}
{$ENDIF}
