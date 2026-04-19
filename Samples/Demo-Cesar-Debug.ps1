#!/usr/bin/env pwsh
$ErrorActionPreference = 'Stop'

$source  = Join-Path $PSScriptRoot 'BitShift.Cesar.txt'
$project = Join-Path $PSScriptRoot '..' 'fs' 'ArchSims.CmdLine'

dotnet run --project $project -- $source -Cpu Cesar -Output Decimal -Speed 500 -Mode AllSteps | Out-Host -Paging
