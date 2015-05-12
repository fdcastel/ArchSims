namespace Ramses.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Common
open Ufrgs.Inf.ArchSims.Ramses

type RamsesState =
    | Ra of byte
    | Rb of byte
    | Rx of byte
    | ProgramCounter of byte
    | MemoryReads of int
    | MemoryWrites of int
    | FlagsNegative of bool
    | FlagsZero of bool
    | FlagsCarry of bool
    | Instructions of int
    | DebuggerFlagsBreakpointHit of bool
    | DebuggerFlagsHalted of bool
    | None

[<TestClass>]
type testrun() = 
    let cpu = new Cpu()

    member this.AssertRamsesState states =
        for state in states do
            match state with
            | Ra ra -> Assert.AreEqual(ra, cpu.Registers.Ra)
            | Rb rb -> Assert.AreEqual(rb, cpu.Registers.Rb)
            | Rx rx -> Assert.AreEqual(rx, cpu.Registers.Rx)
            | ProgramCounter pc -> Assert.AreEqual(pc, cpu.Registers.ProgramCounter)
            | MemoryReads reads -> Assert.AreEqual(reads, cpu.Memory.ReadCount)
            | MemoryWrites writes -> Assert.AreEqual(writes, cpu.Memory.WriteCount)
            | FlagsNegative n -> Assert.AreEqual(n, cpu.Registers.Flags.Negative)
            | FlagsZero z -> Assert.AreEqual(z, cpu.Registers.Flags.Zero)
            | FlagsCarry c -> Assert.AreEqual(c, cpu.Registers.Flags.Carry)
            | Instructions i -> Assert.AreEqual(i, cpu.Debugger.InstructionCount)
            | DebuggerFlagsBreakpointHit breakpointHit -> Assert.AreEqual(breakpointHit, cpu.Debugger.BreakpointHit)
            | DebuggerFlagsHalted halted -> Assert.AreEqual(halted, cpu.Debugger.Halted)
            | None -> ()

    member this.AssertCpuStateIsClean() =
        this.AssertRamsesState [ProgramCounter 0uy; Ra 0uy; Rb 0uy; Rx 0uy; 
            FlagsNegative false; FlagsZero true; FlagsCarry false; 
            DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false; 
            MemoryReads 0; MemoryWrites 0; Instructions 0]
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OpCode)
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OperandAddress)

        for i = 0 to cpu.Memory.Data.Length - 1 do
            Assert.AreEqual(0uy, cpu.Memory.Data.[i])
            Assert.IsFalse(cpu.Debugger.Breakpoints.Contains(i))

    member this.WriteRegister(register, value) =
        match register with
        | 0 -> cpu.Registers.Ra <- value
        | 1 -> cpu.Registers.Rb <- value
        | 2 -> cpu.Registers.Rx <- value
        | _ -> cpu.Registers.ProgramCounter <- value

    member this.TestJumpOperation(instruction: Instruction, jumpExpected) =
        cpu.Memory.Data.[0] <- byte instruction
        cpu.Memory.Data.[1] <- 123uy
        cpu.Step()
        let expectedPc = if jumpExpected then 123uy else 2uy
        this.AssertRamsesState [ProgramCounter expectedPc; MemoryReads 2; Instructions 1]

    [<TestInitialize>]
    member this.Setup() =
        cpu.Reset()
        
    [<TestMethod>]
    member this.``Ramses: New Cpu starts in clean state``() =
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Ramses: Program Counter wraps at end of memory``() =
        cpu.Registers.ProgramCounter <- byte (cpu.Memory.Data.Length - 1)
        cpu.Step()
        this.AssertRamsesState [ProgramCounter 0uy; MemoryReads 1; Instructions 1]

    [<TestMethod>]
    [<ExpectedException(typedefof<CpuRunningForeverException>)>]
    member this.``Ramses: Run() throws exception when running forever``() =
        cpu.Run(1000);

    [<TestMethod>]
    member this.``Ramses: Reset() reverts to clean state``() =
        cpu.Registers.Ra <- 1uy
        cpu.Registers.Rb <- 2uy
        cpu.Registers.Rx <- 3uy
        cpu.Registers.ProgramCounter <- 1uy
        ReadByte cpu.Memory 1 |> ignore
        WriteByte cpu.Memory 1 (byte Instruction.Hlt)
        cpu.Debugger.Breakpoints.Add(1) |> ignore
        cpu.Step()
        this.AssertRamsesState [Ra 1uy; Rb 2uy; Rx 3uy; ProgramCounter 2uy; Instructions 1]
        cpu.Reset()
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Ramses: Flags Zero and Negative are set when a register changes``() =
        for r = 0 to 2 do
            cpu.Memory.Data.[0] <- byte Instruction.Not ||| (byte r <<< 2)
            cpu.Memory.Data.[1] <- byte Instruction.Not ||| (byte r <<< 2)
            for i = 0 to int System.Byte.MaxValue do
                this.WriteRegister(r, byte i)
                cpu.Registers.ProgramCounter <- 0uy
                cpu.Step()
                let noti = ~~~(byte i)
                this.AssertRamsesState [FlagsNegative (noti > 127uy); FlagsZero (noti = 0uy)]
                cpu.Step()
                this.AssertRamsesState [FlagsNegative (i > 127); FlagsZero (i = 0)]

    [<TestMethod>]
    member this.``Ramses: DebuggerFlags works as expected``() =
        this.AssertRamsesState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

        cpu.Step()
        this.AssertRamsesState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        cpu.Step()
        this.AssertRamsesState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted true]

        cpu.Step()
        this.AssertRamsesState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

        cpu.Debugger.Breakpoints.Add(3) |> ignore
        cpu.Step()
        this.AssertRamsesState [DebuggerFlagsBreakpointHit true; DebuggerFlagsHalted false]

        cpu.Step()
        this.AssertRamsesState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

        cpu.Memory.Data.[5] <- byte Instruction.Hlt
        cpu.Debugger.Breakpoints.Add(5) |> ignore
        cpu.Step()
        this.AssertRamsesState [DebuggerFlagsBreakpointHit true; DebuggerFlagsHalted true]

        cpu.Step()
        this.AssertRamsesState [DebuggerFlagsBreakpointHit false; DebuggerFlagsHalted false]

    [<TestMethod>]
    member this.``Ramses: AddressModes works as expected``() =
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Ra ||| byte AddressMode.Direct
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        cpu.Step()
        this.AssertRamsesState [Ra 234uy; ProgramCounter 2uy; MemoryReads 3; Instructions 1]

        cpu.Reset()
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Rb ||| byte AddressMode.Indirect
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        cpu.Memory.Data.[234] <- 245uy
        cpu.Step()
        this.AssertRamsesState [Rb 245uy; ProgramCounter 2uy; MemoryReads 4; Instructions 1]

        cpu.Reset()
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 123uy
        cpu.Step()
        this.AssertRamsesState [Rx 123uy; ProgramCounter 2uy; MemoryReads 2; Instructions 1]

        cpu.Reset()
        cpu.Registers.Rx <- 23uy
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Ra ||| byte AddressMode.Indexed
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[146] <- 234uy
        cpu.Step()
        this.AssertRamsesState [Ra 234uy; Rx 23uy; ProgramCounter 2uy; MemoryReads 3; Instructions 1]

    [<TestMethod>]
    member this.``Ramses: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        cpu.Step()
        this.AssertRamsesState [ProgramCounter 1uy; MemoryReads 1; Instructions 1]

    [<TestMethod>]
    member this.``Ramses: LDR loads value from memory into any register``() =
        for r = 0 to 2 do
            cpu.Reset()
            cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| (byte r <<< 2) ||| byte AddressMode.Immediate
            cpu.Memory.Data.[1] <- 123uy
            cpu.Step()
            let rCheck rr = if rr = r then 123uy else 0uy
            this.AssertRamsesState [Ra (rCheck 0); Rb (rCheck 1); Rx (rCheck 2); ProgramCounter 2uy; MemoryReads 2; Instructions 1]

    [<TestMethod>]
    member this.``Ramses: STA stores value from any register into memory``() =
        for r = 0 to 2 do
            cpu.Reset()
            this.WriteRegister(r, 234uy)
            cpu.Memory.Data.[0] <- byte Instruction.Str ||| (byte r <<< 2) ||| byte AddressMode.Direct
            cpu.Memory.Data.[1] <- 123uy
            cpu.Step()
            Assert.AreEqual(234uy, cpu.Memory.Data.[123])
            this.AssertRamsesState [ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1; Instructions 1]

    [<TestMethod>]
    member this.``Ramses: ADD works as expected``() =
        cpu.Registers.Ra <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Add ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 23uy
        cpu.Step()
        this.AssertRamsesState [Ra (12uy + 23uy); ProgramCounter 2uy; MemoryReads 2; Instructions 1; FlagsNegative false; FlagsZero false; FlagsCarry false]

        cpu.Reset()
        cpu.Registers.Ra <- byte(256 - 12)
        cpu.Memory.Data.[0] <- byte Instruction.Add ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- byte(256 - 23)
        cpu.Step()
        this.AssertRamsesState [Ra (byte(256 - 12 - 23)); ProgramCounter 2uy; MemoryReads 2; Instructions 1; FlagsNegative true; FlagsZero false; FlagsCarry true]

    [<TestMethod>]
    member this.``Ramses: OR works as expected``() =
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Or
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        cpu.Step()
        this.AssertRamsesState [Ra (234uy ||| 12uy); ProgramCounter 2uy; MemoryReads 3; Instructions 1]

    [<TestMethod>]
    member this.``Ramses: AND works as expected``() =
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.And
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        cpu.Step()
        this.AssertRamsesState [Ra (234uy &&& 12uy); ProgramCounter 2uy; MemoryReads 3; Instructions 1]

    [<TestMethod>]
    member this.``Ramses: NOT works as expected``() =
        cpu.Registers.Ra <- 85uy  (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Not
        cpu.Step()
        this.AssertRamsesState [Ra 170uy (* 10101010 *); ProgramCounter 1uy; MemoryReads 1; Instructions 1]

    [<TestMethod>]
    member this.``Ramses: SUB works as expected``() =
        cpu.Registers.Ra <- 23uy
        cpu.Memory.Data.[0] <- byte Instruction.Sub ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 12uy
        cpu.Step()
        this.AssertRamsesState [Ra (23uy - 12uy); ProgramCounter 2uy; MemoryReads 2; Instructions 1; FlagsNegative false; FlagsZero false; FlagsCarry false]

        cpu.Reset()
        cpu.Registers.Ra <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Sub ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 23uy
        cpu.Step()
        this.AssertRamsesState [Ra (byte(256 + 12 - 23)); ProgramCounter 2uy; MemoryReads 2; Instructions 1; FlagsNegative true; FlagsZero false; FlagsCarry true]

    [<TestMethod>]
    member this.``Ramses: JMP changes Program Counter``() =
        this.TestJumpOperation(Instruction.Jmp, true)

    [<TestMethod>]
    member this.``Ramses: JN jumps only if Negative flag is set``() =
        cpu.Registers.Flags.Negative <- false
        this.TestJumpOperation(Instruction.Jn, false)

        cpu.Reset()
        cpu.Registers.Flags.Negative <- true
        this.TestJumpOperation(Instruction.Jn, true)

    [<TestMethod>]
    member this.``Ramses: JZ jumps only if Zero flag is set``() =
        cpu.Registers.Flags.Zero <- false
        this.TestJumpOperation(Instruction.Jz, false)

        cpu.Reset()
        cpu.Registers.Flags.Zero <- true
        this.TestJumpOperation(Instruction.Jz, true)

    [<TestMethod>]
    member this.``Ramses: JC jumps only if Carry flag is set``() =
        cpu.Registers.Flags.Carry <- false
        this.TestJumpOperation(Instruction.Jc, false)

        cpu.Reset()
        cpu.Registers.Flags.Carry <- true
        this.TestJumpOperation(Instruction.Jc, true)

    [<TestMethod>]
    member this.``Ramses: JSR jumps and saves Program Counter``() =
        cpu.Memory.Data.[0] <- byte Instruction.Jsr
        cpu.Memory.Data.[1] <- 123uy
        cpu.Step()
        Assert.AreEqual(2uy, cpu.Memory.Data.[123])
        this.AssertRamsesState [ProgramCounter 124uy; MemoryReads 2; MemoryWrites 1; Instructions 1]

    [<TestMethod>]
    member this.``Ramses: NEG works as expected``() =
        cpu.Registers.Ra <- 23uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        cpu.Step()
        this.AssertRamsesState [Ra (byte(256 - 23)); ProgramCounter 1uy; MemoryReads 1; Instructions 1; FlagsNegative true; FlagsZero false; FlagsCarry false]

        cpu.Reset()
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        cpu.Step()
        this.AssertRamsesState [Ra (byte(256 - 234)); ProgramCounter 1uy; MemoryReads 1; Instructions 1; FlagsNegative false; FlagsZero false; FlagsCarry false]

        cpu.Reset()
        cpu.Registers.Ra <- 128uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        cpu.Step()
        this.AssertRamsesState [Ra (byte(256 - 128)); ProgramCounter 1uy; MemoryReads 1; Instructions 1; FlagsNegative true; FlagsZero false; FlagsCarry false]

        cpu.Reset()
        cpu.Registers.Ra <- 0uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        cpu.Step()
        this.AssertRamsesState [Ra (byte(256 - 0)); ProgramCounter 1uy; MemoryReads 1; Instructions 1; FlagsNegative false; FlagsZero true; FlagsCarry true]

    [<TestMethod>]
    member this.``Ramses: SHR works as expected``() =
        cpu.Registers.Ra <- 85uy (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Shr
        cpu.Step()
        this.AssertRamsesState [Ra 42uy; ProgramCounter 1uy; MemoryReads 1; Instructions 1; FlagsNegative false; FlagsZero false; FlagsCarry true]

        cpu.Memory.Data.[1] <- byte Instruction.Shr
        cpu.Step()
        this.AssertRamsesState [Ra 21uy; ProgramCounter 2uy; MemoryReads 2; Instructions 2; FlagsNegative false; FlagsZero false; FlagsCarry false]

        cpu.Memory.Data.[2] <- byte Instruction.Shr
        cpu.Step()
        this.AssertRamsesState [Ra 10uy; ProgramCounter 3uy; MemoryReads 3; Instructions 3; FlagsNegative false; FlagsZero false; FlagsCarry true]

    [<TestMethod>]
    member this.``Ramses: HLT halts execution``() =
        cpu.Memory.Data.[123] <- byte Instruction.Hlt
        cpu.Run()
        this.AssertRamsesState [ProgramCounter 124uy; MemoryReads 124; Instructions 124]
        
    [<TestMethod>]
    member this.``Ramses: Breakpoints halts execution``() =
        cpu.Debugger.Breakpoints.Add(12) |> ignore
        cpu.Debugger.Breakpoints.Add(50) |> ignore
        
        cpu.Run()
        this.AssertRamsesState [ProgramCounter 13uy; MemoryReads 13; Instructions 13]
        
        cpu.Run()
        this.AssertRamsesState [ProgramCounter 51uy; MemoryReads 51; Instructions 51]

        cpu.Run()
        this.AssertRamsesState [ProgramCounter 13uy; MemoryReads (256 + 13); Instructions (256 + 13)]

        cpu.Run()
        this.AssertRamsesState [ProgramCounter 51uy; MemoryReads (256 + 51); Instructions (256 + 51)]
