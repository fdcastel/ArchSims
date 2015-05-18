@ECHO OFF
powershell -c "Get-ChildItem -Include bin,obj,BuildOutput,TestResults -Recurse | Remove-Item -Recurse -Force | Out-Null"
IF ERRORLEVEL 1 PAUSE && EXIT /B 1