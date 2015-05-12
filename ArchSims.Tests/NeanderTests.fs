namespace Neander.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Common
open Ufrgs.Inf.ArchSims.Neander

type NeanderState =
    | Accumulator of byte
    | ProgramCounter of byte
    | MemoryReads of int
    | MemoryWrites of int
    | FlagsNegative of bool
    | FlagsZero of bool
    | Instructions of int
    | DebuggerFlagsBreakpointHit of bool
    | DebuggerFlagsHalted of bool
    | None

[<TestClass>]
type testrun() = 
    let cpu = new Cpu()

    member this.AssertNeanderState states =
        for state in states do
            match state with
            | Accumulator ac -> Assert.AreEqual(ac, cpu.Registers.Accumulator)
            | ProgramCounter pc -> Assert.AreEqual(pc, cpu.Registers.ProgramCounter)
            | MemoryReads reads -> Assert.AreEqual(reads, cpu.Memory.ReadCount)
            | MemoryWrites writes -> Assert.AreEqual(writes, cpu.Memory.WriteCount)
            | FlagsNegative n -> Assert.AreEqual(n, cpu.Registers.Flags.Negative)
            | FlagsZero z -> Assert.AreEqual(z, cpu.Registers.Flags.Zero)
            | Instructions i -> Assert.AreEqual(i, cpu.Debugger.InstructionCount)
            | DebuggerFlagsBreakpointHit breakpointHit -> Assert.AreEqual(breakpointHit, cpu.Debugger.BreakpointHit)
            | DebuggerFlagsHalted halted -> Assert.AreEqual(halted, cpu.Debugger.Halted)
            | None -> ()

    member this.AssertCpuStateIsClean() =
        this.AssertNeanderState [ProgramCounter 0uy; Accumulator 0uy; 
            FlagsNegative false; FlagsZero true; 
            DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false; 
            MemoryReads 0; MemoryWrites 0; Instructions 0]
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OpCode)
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OperandAddress)

        for i = 0 to cpu.Memory.Data.Length - 1 do
            Assert.AreEqual(0uy, cpu.Memory.Data.[i])
            Assert.IsFalse(cpu.Debugger.Breakpoints.Contains(i))

    member this.TestJumpOperation(instruction: Instruction, jumpExpected) =
        cpu.Memory.Data.[0] <- byte instruction
        cpu.Memory.Data.[1] <- 123uy
        cpu.Step()
        let expectedPc = if jumpExpected then 123uy else 2uy
        this.AssertNeanderState [ProgramCounter expectedPc; MemoryReads 2; Instructions 1]

    [<TestInitialize>]
    member this.Setup() =
        cpu.Reset()
        
    [<TestMethod>]
    member this.``Neander: New Cpu starts in clean state``() =
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Neander: Program Counter wraps at end of memory``() =
        cpu.Registers.ProgramCounter <- byte (cpu.Memory.Data.Length - 1)
        cpu.Step()
        this.AssertNeanderState [ProgramCounter 0uy; MemoryReads 1; Instructions 1]

    [<TestMethod>]
    [<ExpectedException(typedefof<CpuRunningForeverException>)>]
    member this.``Neander: Run() throws exception when running forever``() =
        cpu.Run(1000);

    [<TestMethod>]
    member this.``Neander: Reset() reverts to clean state``() =
        cpu.Registers.Accumulator <- 1uy
        cpu.Registers.ProgramCounter <- 1uy
        ReadByte cpu.Memory 1 |> ignore
        WriteByte cpu.Memory 1 (byte Instruction.Hlt)
        cpu.Debugger.Breakpoints.Add(1) |> ignore
        cpu.Step()
        this.AssertNeanderState [Accumulator 1uy; ProgramCounter 2uy; Instructions 1]
        cpu.Reset()
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Neander: Flags Zero and Negative are set when Accumulator changes``() =
        cpu.Memory.Data.[0] <- byte Instruction.Not
        cpu.Memory.Data.[1] <- byte Instruction.Not
        for i = 0 to int System.Byte.MaxValue do
            cpu.Registers.Accumulator <- byte i
            cpu.Registers.ProgramCounter <- 0uy
            cpu.Step()
            let noti = ~~~(byte i)
            this.AssertNeanderState [FlagsNegative (noti > 127uy); FlagsZero (noti = 0uy)]
            cpu.Step()
            this.AssertNeanderState [FlagsNegative (i > 127); FlagsZero (i = 0)]

    [<TestMethod>]
    member this.``Neander: DebuggerFlags works as expected``() =
        this.AssertNeanderState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

        cpu.Step()
        this.AssertNeanderState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        cpu.Step()
        this.AssertNeanderState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted true]

        cpu.Step()
        this.AssertNeanderState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

        cpu.Debugger.Breakpoints.Add(3) |> ignore
        cpu.Step()
        this.AssertNeanderState [DebuggerFlagsBreakpointHit true; DebuggerFlagsHalted false]

        cpu.Step()
        this.AssertNeanderState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

        cpu.Memory.Data.[5] <- byte Instruction.Hlt
        cpu.Debugger.Breakpoints.Add(5) |> ignore
        cpu.Step()
        this.AssertNeanderState [DebuggerFlagsBreakpointHit true; DebuggerFlagsHalted true]

        cpu.Step()
        this.AssertNeanderState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

    [<TestMethod>]
    member this.``Neander: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        cpu.Step()
        this.AssertNeanderState [ProgramCounter 1uy; MemoryReads 1; Instructions 1]

    [<TestMethod>]
    member this.``Neander: LDA loads value from memory into Accumulator``() =
        cpu.Memory.Data.[0] <- byte Instruction.Lda
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        cpu.Step()
        this.AssertNeanderState [Accumulator 234uy; ProgramCounter 2uy; MemoryReads 3; Instructions 1]

    [<TestMethod>]
    member this.``Neander: STA stores value from Accumulator into memory``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Sta
        cpu.Memory.Data.[1] <- 123uy
        cpu.Step()
        Assert.AreEqual(234uy, cpu.Memory.Data.[123])
        this.AssertNeanderState [Accumulator 234uy; ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1; Instructions 1]

    [<TestMethod>]
    member this.``Neander: ADD works as expected``() =
        cpu.Registers.Accumulator <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Add
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 23uy
        cpu.Step()
        this.AssertNeanderState [Accumulator (12uy + 23uy); ProgramCounter 2uy; MemoryReads 3; Instructions 1]

    [<TestMethod>]
    member this.``Neander: OR works as expected``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Or
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        cpu.Step()
        this.AssertNeanderState [Accumulator (234uy ||| 12uy); ProgramCounter 2uy; MemoryReads 3; Instructions 1]

    [<TestMethod>]
    member this.``Neander: AND works as expected``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.And
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        cpu.Step()
        this.AssertNeanderState [Accumulator (234uy &&& 12uy); ProgramCounter 2uy; MemoryReads 3; Instructions 1]

    [<TestMethod>]
    member this.``Neander: NOT works as expected``() =
        cpu.Registers.Accumulator <- 85uy  (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Not
        cpu.Step()
        this.AssertNeanderState [Accumulator 170uy (* 10101010 *); ProgramCounter 1uy; MemoryReads 1; Instructions 1]

    [<TestMethod>]
    member this.``Neander: JMP changes Program Counter``() =
        this.TestJumpOperation(Instruction.Jmp, true)

    [<TestMethod>]
    member this.``Neander: JN jumps only if Negative flag is set``() =
        cpu.Registers.Flags.Negative <- false
        this.TestJumpOperation(Instruction.Jn, false)

        cpu.Reset()
        cpu.Registers.Flags.Negative <- true
        this.TestJumpOperation(Instruction.Jn, true)

    [<TestMethod>]
    member this.``Neander: JZ jumps only if Zero flag is set``() =
        cpu.Registers.Flags.Zero <- false
        this.TestJumpOperation(Instruction.Jz, false)

        cpu.Reset()
        cpu.Registers.Flags.Zero <- true
        this.TestJumpOperation(Instruction.Jz, true)

    [<TestMethod>]
    member this.``Neander: HLT halts execution``() =
        cpu.Memory.Data.[123] <- byte Instruction.Hlt
        cpu.Run()
        this.AssertNeanderState [ProgramCounter 124uy; MemoryReads 124; Instructions 124]
        
    [<TestMethod>]
    member this.``Neander: Breakpoints halts execution``() =
        cpu.Debugger.Breakpoints.Add(12) |> ignore
        cpu.Debugger.Breakpoints.Add(50) |> ignore
        
        cpu.Run()
        this.AssertNeanderState [ProgramCounter 13uy; MemoryReads 13; Instructions 13]
        
        cpu.Run()
        this.AssertNeanderState [ProgramCounter 51uy; MemoryReads 51; Instructions 51]

        cpu.Run()
        this.AssertNeanderState [ProgramCounter 13uy; MemoryReads (256 + 13); Instructions (256 + 13)]

        cpu.Run()
        this.AssertNeanderState [ProgramCounter 51uy; MemoryReads (256 + 51); Instructions (256 + 51)]

