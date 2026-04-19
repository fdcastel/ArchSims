#!/usr/bin/env pwsh
$ErrorActionPreference = 'Stop'

$source  = Join-Path $PSScriptRoot 'BitShift.Ramses.txt'
$project = Join-Path $PSScriptRoot '..' 'fs' 'ArchSims.CmdLine'

dotnet run --project $project -- $source -Cpu Ramses -Output Binary -Speed 500
