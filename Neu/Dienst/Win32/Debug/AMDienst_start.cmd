@echo off
rem mit Admin-Rechten ausführen!

c:
cd C:\DQ\AT\ActionManager\Neu\Dienst\Win32\Debug\

AMDienst /install /silent
net start AMDienst
pause
