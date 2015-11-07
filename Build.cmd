@ECHO OFF
SETLOCAL

::
:: Requires Visual Studio 2015
::



::
:: Builds F# Binaries
::

CALL "%VS140COMNTOOLS%"\VsDevCmd.bat

msbuild.exe .\FsArchSims.sln /t:Build /p:Configuration=Release
IF ERRORLEVEL 1 PAUSE && EXIT /B 1

mstest.exe /testcontainer:ArchSims.Tests\bin\Release\ArchSims.Tests.dll
IF ERRORLEVEL 1 PAUSE && EXIT /B 1



::
:: Creates output folder
::

SET OUT_DIR=.\BuildOutput

MKDIR %OUT_DIR% 2> NUL
DEL /S /Q %OUT_DIR%\* > NUL

COPY .\ArchSims.CmdLine\bin\Release\*.exe %OUT_DIR% > NUL
COPY .\ArchSims.CmdLine\bin\Release\*.dll %OUT_DIR% > NUL

COPY %OUT_DIR%\ArchSims.CmdLine.exe %OUT_DIR%\Ramses.exe > NUL
COPY %OUT_DIR%\ArchSims.CmdLine.exe %OUT_DIR%\Cesar.exe > NUL
COPY .\Samples\* %OUT_DIR%\ > NUL

:: ToDo: remove the need for these .config files in runtime ?
COPY .\ArchSims.CmdLine\bin\Release\ArchSims.CmdLine.exe.config %OUT_DIR% > NUL
COPY .\ArchSims.CmdLine\bin\Release\ArchSims.CmdLine.exe.config %OUT_DIR%\Ramses.exe.config > NUL
COPY .\ArchSims.CmdLine\bin\Release\ArchSims.CmdLine.exe.config %OUT_DIR%\Cesar.exe.config > NUL

ENDLOCAL
