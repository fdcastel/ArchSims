@ECHO OFF
SETLOCAL

::
:: Requires Visual Studio 2015
::



::
:: Builds F# Binaries
::

IF NOT EXIST .\packages (
    ECHO *
    ECHO * Required packages not found. 
    ECHO *
    ECHO * Please build once in Visual Studio to download required packages.
    ECHO *
    PAUSE && EXIT /B 1
)

CALL "%VS140COMNTOOLS%"\VsDevCmd.bat

msbuild.exe .\ArchSims.sln /t:Build /p:Configuration=Release
IF ERRORLEVEL 1 PAUSE && EXIT /B 1

mstest.exe /testcontainer:.\Core\Tests\bin\Release\ArchSims.Core.Tests.dll
IF ERRORLEVEL 1 PAUSE && EXIT /B 1

mstest.exe /testcontainer:.\Assemblers\Tests\bin\Release\ArchSims.Assemblers.Tests.dll
IF ERRORLEVEL 1 PAUSE && EXIT /B 1

mstest.exe /testcontainer:.\Emulators\Tests\bin\Release\ArchSims.Emulators.Tests.dll
IF ERRORLEVEL 1 PAUSE && EXIT /B 1

::
:: Creates output folder
::

SET OUT_DIR=.\BuildOutput

MKDIR %OUT_DIR% 2> NUL
DEL /S /Q %OUT_DIR%\* > NUL

COPY .\CmdLine\bin\Release\*.exe %OUT_DIR% > NUL
COPY .\CmdLine\bin\Release\*.dll %OUT_DIR% > NUL

COPY %OUT_DIR%\ArchSims.CmdLine.exe %OUT_DIR%\Ramses.exe > NUL
COPY %OUT_DIR%\ArchSims.CmdLine.exe %OUT_DIR%\Cesar.exe > NUL
COPY .\Samples\* %OUT_DIR%\ > NUL

:: ToDo: remove the need for these .config files in runtime ?
COPY .\CmdLine\bin\Release\ArchSims.CmdLine.exe.config %OUT_DIR% > NUL
COPY .\CmdLine\bin\Release\ArchSims.CmdLine.exe.config %OUT_DIR%\Ramses.exe.config > NUL
COPY .\CmdLine\bin\Release\ArchSims.CmdLine.exe.config %OUT_DIR%\Cesar.exe.config > NUL

COPY .\Tests\RamsesEmulator.Cesar.txt %OUT_DIR%\ > NUL
%OUT_DIR%\Cesar.exe %OUT_DIR%\RamsesEmulator.Cesar.txt -Save %OUT_DIR%\RamsesEmulator.Cesar.mem > NUL

ENDLOCAL
