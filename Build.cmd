@ECHO OFF

CALL "%VS140COMNTOOLS%"\VsDevCmd.bat

msbuild.exe .\FsArchSims.sln /t:Build /p:Configuration=Release
IF ERRORLEVEL 1 PAUSE && EXIT /B 1

MSTest /testcontainer:ArchSims.Tests\bin\Release\ArchSims.Tests.dll
IF ERRORLEVEL 1 PAUSE && EXIT /B 1