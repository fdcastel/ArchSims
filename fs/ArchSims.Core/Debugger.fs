namespace Ufrgs.Inf.ArchSims.Core

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
        mutable Breakpoints: Set<int>
    }

    let CreateDebugger cpuGetProgramCounter cpuStep = {
        CpuGetProgramCounter = cpuGetProgramCounter
        CpuStep = cpuStep
        InstructionCount = 0
        LastStop = None
        Breakpoints = Set.empty
    }

    let DebuggerReset debugger =
        debugger.InstructionCount <- 0
        debugger.LastStop <- None
        debugger.Breakpoints <- Set.empty

    let DebuggerStep debugger =
        debugger.LastStop <- if debugger.CpuStep() 
                             then Halted
                             else if debugger.Breakpoints.Contains (debugger.CpuGetProgramCounter()) 
                                  then Breakpoint
                                  else None
        debugger.InstructionCount <- debugger.InstructionCount + 1

    let DebuggerRun debugger maximumInstructions =
        debugger.LastStop <- None
        let limitInstructions = debugger.InstructionCount + maximumInstructions
        while debugger.LastStop = None do
            DebuggerStep debugger
            if debugger.InstructionCount >= limitInstructions then
                debugger.LastStop <- RunningForever

    let DebuggerSetBreakpoint debugger address =
        debugger.Breakpoints <- debugger.Breakpoints.Add(address)

    let DebuggerClearBreakpoint debugger address =
        debugger.Breakpoints <- debugger.Breakpoints.Remove(address)