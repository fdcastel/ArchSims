namespace Ufrgs.Inf.ArchSims.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Debugger
open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Neander

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

    member this.AssertDebuggerState states =
        for state in states do
            match state with
            | Instructions i -> Assert.AreEqual(i, debugger.InstructionCount)
            | DebuggerLastStop reason -> Assert.AreEqual(reason, debugger.LastStop)
            | None -> ()

    [<TestInitialize>]
    member this.Setup() =
        DebuggerReset debugger
        
    [<TestMethod>]
    member this.``Debugger: DebuggerRun detects when running forever``() =
        DebuggerRun debugger 1000
        this.AssertDebuggerState [DebuggerLastStop RunningForever; Instructions 1000]
        DebuggerRun debugger 500
        this.AssertDebuggerState [DebuggerLastStop RunningForever; Instructions 1500]

    [<TestMethod>]
    member this.``Debugger: DebuggerReset reverts to clean state``() =
        cpu.Memory.Data.[0] <- byte Instruction.Hlt
        DebuggerSetBreakpoint debugger 10
        DebuggerStep debugger
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.Halted; Instructions 1]
        DebuggerReset debugger
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.None; Instructions 0]
        for i = 0 to cpu.Memory.Data.Length - 1 do
            Assert.IsFalse(debugger.Breakpoints.Contains(i))
        
    [<TestMethod>]
    member this.``Debugger: Debugger.LastStop works as expected``() =
        DebuggerStep debugger
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.None]

        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        DebuggerStep debugger
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.Halted]

        DebuggerStep debugger
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.None]

        DebuggerSetBreakpoint debugger 3
        DebuggerStep debugger
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint]

        DebuggerStep debugger
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.None]

        cpu.Memory.Data.[5] <- byte Instruction.Hlt
        DebuggerSetBreakpoint debugger 5
        DebuggerStep debugger
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.Halted]

        DebuggerStep debugger
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.None]

        cpu.Memory.Data.[123] <- byte Instruction.Hlt
        DebuggerRun debugger 1000
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.Halted; Instructions 124]
        
    [<TestMethod>]
    member this.``Debugger: Breakpoints halts execution``() =
        DebuggerSetBreakpoint debugger 12
        DebuggerSetBreakpoint debugger 50
        
        DebuggerRun debugger 1000
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint; Instructions 13]
        
        DebuggerRun debugger 1000
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint; Instructions 51]

        DebuggerRun debugger 1000
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint; Instructions (256 + 13)]

        DebuggerRun debugger 1000
        this.AssertDebuggerState [DebuggerLastStop DebuggerStopReason.Breakpoint; Instructions (256 + 51)]

