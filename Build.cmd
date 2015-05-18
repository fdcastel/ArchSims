@ECHO OFF

CALL "%VS140COMNTOOLS%"\VsDevCmd.bat

msbuild.exe .\FsArchSims.sln /t:Build /p:Configuration=Release
IF ERRORLEVEL 1 PAUSE && EXIT /B 1

mstest.exe /testcontainer:ArchSims.Tests\bin\Release\ArchSims.Tests.dll
IF ERRORLEVEL 1 PAUSE && EXIT /B 1

MKDIR .\BuildOutput > NUL
COPY .\ArchSims.Runners\bin\Release\*.exe .\BuildOutput
COPY .\ArchSims.Runners\bin\Release\*.dll .\BuildOutput

COPY .\BuildOutput\ArchSims.Runners.exe .\BuildOutput\Ramses.exe
COPY .\BuildOutput\ArchSims.Runners.exe .\BuildOutput\Cesar.exe
COPY .\Samples\* .\BuildOutput\

REM -- 
REM -- ToDo: remove the need for these .config files in runtime
REM -- 
COPY .\ArchSims.Runners\bin\Release\ArchSims.Runners.exe.config .\BuildOutput
COPY .\ArchSims.Runners\bin\Release\ArchSims.Runners.exe.config .\BuildOutput\Ramses.exe.config
COPY .\ArchSims.Runners\bin\Release\ArchSims.Runners.exe.config .\BuildOutput\Cesar.exe.config

