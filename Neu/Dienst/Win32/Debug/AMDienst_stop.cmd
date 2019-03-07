@echo off
rem mit Admin-Rechten ausführen!

c:
cd C:\DQ\AT\ActionManager\Neu\Dienst\Win32\Debug\

net stop AMDienst
AMDienst /uninstall /silent
pause