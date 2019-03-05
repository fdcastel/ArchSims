@ECHO OFF
PUSHD "%~dp0"

:: Publish
TITLE Building...
PUSHD ..\ArchSims.CmdLine
dotnet publish -c Release -r win10-x64
POPD
CLS

TITLE Cesar: BitShift
..\ArchSims.CmdLine\bin\Release\netcoreapp2.1\win10-x64\ArchSims.CmdLine.exe BitShift.Cesar.txt -Cpu Cesar -Output Binary -Speed 500
PAUSE
