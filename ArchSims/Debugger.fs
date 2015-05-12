namespace Ufrgs.Inf.ArchSims

open System.Collections.Generic

module Debugger =

    type DebuggerStopReason =
    | None
    | Halted
    | Breakpoint
    | RunningForever

    type Debugger = {
        CpuGetProgramCounter: (unit -> int)
        CpuStep: (unit -> bool)                  // Must return 'true' when halted
        mutable InstructionCount: int
        mutable LastStop: DebuggerStopReason
        Breakpoints: HashSet<int>
    }

    let CreateDebugger cpuGetProgramCounter cpuStep = {
        CpuGetProgramCounter = cpuGetProgramCounter
        CpuStep = cpuStep
        InstructionCount = 0
        LastStop = None
        Breakpoints = new HashSet<int>()
    }

    let DebuggerReset debugger =
        debugger.InstructionCount <- 0
        debugger.LastStop <- None
        debugger.Breakpoints.Clear()

    let DebuggerStep debugger =
        debugger.LastStop <- None
        if debugger.Breakpoints.Contains (debugger.CpuGetProgramCounter()) then
            debugger.LastStop <- Breakpoint

        if debugger.CpuStep() then
            debugger.LastStop <- Halted

        debugger.InstructionCount <- debugger.InstructionCount + 1

    let DebuggerRun debugger maximumInstructions =
        debugger.LastStop <- None
        while debugger.LastStop = None do
            DebuggerStep debugger
            if debugger.InstructionCount >= maximumInstructions then
                debugger.LastStop <- RunningForever
