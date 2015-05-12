@ECHO OFF

CALL "%VS140COMNTOOLS%"\VsDevCmd.bat

msbuild.exe .\FsArchSims.sln /t:Build /p:Configuration=Release
IF ERRORLEVEL 1 pause

MSTest /testcontainer:ArchSims.Tests\bin\Release\ArchSims.Tests.dll
IF ERRORLEVEL 1 pause