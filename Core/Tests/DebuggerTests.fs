namespace Ufrgs.Inf.ArchSims.Core.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Debugger
open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Neander

open Ufrgs.Inf.ArchSims.Core.Tests.Utils

type DebuggerState =
    | Instructions of int
    | DebuggerLastStop of DebuggerStopReason
    | None

[<TestClass>]
type DebuggerTests() = 
    let cpu = CreateCpu()
    let debugger = CreateDebugger (fun () -> int cpu.Registers.ProgramCounter)
                                  (fun () -> Step cpu;
                                             cpu.Registers.Flags.Halted)

    let assertDebuggerState states =
        for state in states do
            match state with
            | Instructions i -> debugger.InstructionCount |>== i
            | DebuggerLastStop reason -> debugger.LastStop |>== reason
            | None -> ()

    [<TestInitialize>]
    member this.Setup() =
        DebuggerReset debugger
        
    [<TestMethod>]
    member this.``Debugger: DebuggerRun detects when running forever``() =
        DebuggerRun debugger 1000
        assertDebuggerState [DebuggerLastStop RunningForever; Instructions 1000]
        DebuggerRun debugger 500
        assertDebuggerState [DebuggerLastStop RunningForever; Instructions 1500]

    [<TestMethod>]
    member this.``Debugger: DebuggerReset reverts to clean state``() =
        cpu.Memory.Data.[0] <- byte Instruction.Hlt
        DebuggerSetBreakpoint debugger 10
        DebuggerStep debugger
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.Halted; Instructions 1]
        DebuggerReset debugger
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.None; Instructions 0]
        for i = 0 to cpu.Memory.Data.Length - 1 do
            Assert.IsFalse(debugger.Breakpoints.Contains(i))
        
    [<TestMethod>]
    member this.``Debugger: Debugger.LastStop works as expected``() =
        DebuggerStep debugger
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.None]

        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        DebuggerStep debugger
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.Halted]

        DebuggerStep debugger
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.None]

        DebuggerSetBreakpoint debugger 4
        DebuggerStep debugger
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint]

        DebuggerStep debugger
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.None]

        cpu.Memory.Data.[5] <- byte Instruction.Hlt
        DebuggerSetBreakpoint debugger 5
        DebuggerStep debugger
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.Halted]

        DebuggerStep debugger
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.None]

        cpu.Memory.Data.[123] <- byte Instruction.Hlt
        DebuggerRun debugger 1000
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.Halted; Instructions 124]
        
    [<TestMethod>]
    member this.``Debugger: Breakpoints halts execution``() =
        DebuggerSetBreakpoint debugger 12
        DebuggerSetBreakpoint debugger 50
        
        DebuggerRun debugger 1000
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint; Instructions 12]
        
        DebuggerRun debugger 1000
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint; Instructions 50]

        DebuggerRun debugger 1000
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint; Instructions (256 + 12)]

        DebuggerRun debugger 1000
        assertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint; Instructions (256 + 50)]

