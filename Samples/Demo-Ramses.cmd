@ECHO OFF
PUSHD "%~dp0"

:: Publish
TITLE Building...
PUSHD ..\ArchSims.CmdLine
dotnet publish -c Release -r win10-x64
POPD
CLS

TITLE Ramses: BitShift
..\ArchSims.CmdLine\bin\Release\netcoreapp2.1\win10-x64\ArchSims.CmdLine.exe BitShift.Ramses.txt -Cpu Ramses -Output Binary -Speed 500
PAUSE
