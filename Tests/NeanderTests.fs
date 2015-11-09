namespace Ufrgs.Inf.ArchSims.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Neander

type NeanderState =
    | Accumulator of byte
    | ProgramCounter of byte
    | MemoryReads of int
    | MemoryWrites of int
    | FlagsHalted of bool
    | FlagsNegative of bool
    | FlagsZero of bool
    | None

[<TestClass>]
type NeanderTests() = 
    let mutable cpu = CreateCpu()

    member this.AssertNeanderState states =
        for state in states do
            match state with
            | Accumulator ac -> Assert.AreEqual(ac, cpu.Registers.Accumulator)
            | ProgramCounter pc -> Assert.AreEqual(pc, cpu.Registers.ProgramCounter)
            | MemoryReads reads -> Assert.AreEqual(reads, cpu.Memory.ReadCount)
            | MemoryWrites writes -> Assert.AreEqual(writes, cpu.Memory.WriteCount)
            | FlagsHalted h -> Assert.AreEqual(h, cpu.Registers.Flags.Halted)
            | FlagsNegative n -> Assert.AreEqual(n, cpu.Registers.Flags.Negative)
            | FlagsZero z -> Assert.AreEqual(z, cpu.Registers.Flags.Zero)
            | NeanderState.None -> ()

    member this.AssertCpuStateIsClean() =
        this.AssertNeanderState [ProgramCounter 0uy; Accumulator 0uy; 
            FlagsHalted false; FlagsNegative false; FlagsZero true; 
            MemoryReads 0; MemoryWrites 0]
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OpCode)
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OperandAddress)
        for i = 0 to cpu.Memory.Data.Length - 1 do
            Assert.AreEqual(0uy, cpu.Memory.Data.[i])

    member this.TestJumpOperation(instruction: Instruction, jumpExpected) =
        cpu.Memory.Data.[0] <- byte instruction
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        let expectedPc = if jumpExpected then 123uy else 2uy
        this.AssertNeanderState [ProgramCounter expectedPc; MemoryReads 2]

    [<TestInitialize>]
    member this.Setup() =
        cpu <- CreateCpu()
        
    [<TestMethod>]
    member this.``Neander: New Cpu starts in clean state``() =
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Neander: Program Counter wraps at end of memory``() =
        cpu.Registers.ProgramCounter <- byte (cpu.Memory.Data.Length - 1)
        Step cpu
        this.AssertNeanderState [ProgramCounter 0uy; MemoryReads 1]

    [<TestMethod>]
    member this.``Neander: Reset reverts to clean state``() =
        cpu.Registers.Accumulator <- 1uy
        cpu.Registers.ProgramCounter <- 1uy
        MemoryReadByte cpu.Memory 1 |> ignore
        byte Instruction.Hlt |> MemoryWriteByte cpu.Memory 1
        Step cpu
        this.AssertNeanderState [Accumulator 1uy; ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1]
        Reset cpu
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Neander: Flags Zero and Negative are set when Accumulator changes``() =
        cpu.Memory.Data.[0] <- byte Instruction.Not
        cpu.Memory.Data.[1] <- byte Instruction.Not
        for i = 0 to int System.Byte.MaxValue do
            cpu.Registers.Accumulator <- byte i
            cpu.Registers.ProgramCounter <- 0uy
            Step cpu
            let noti = ~~~(byte i)
            this.AssertNeanderState [FlagsNegative (noti > 127uy); FlagsZero (noti = 0uy)]
            Step cpu
            this.AssertNeanderState [FlagsNegative (i > 127); FlagsZero (i = 0)]

    [<TestMethod>]
    member this.``Neander: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        Step cpu
        this.AssertNeanderState [ProgramCounter 1uy; MemoryReads 1]

    [<TestMethod>]
    member this.``Neander: LDA loads value from memory into Accumulator``() =
        cpu.Memory.Data.[0] <- byte Instruction.Lda
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        Step cpu
        this.AssertNeanderState [Accumulator 234uy; ProgramCounter 2uy; MemoryReads 3]

    [<TestMethod>]
    member this.``Neander: STA stores value from Accumulator into memory``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Sta
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        Assert.AreEqual(234uy, cpu.Memory.Data.[123])
        this.AssertNeanderState [Accumulator 234uy; ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1]

    [<TestMethod>]
    member this.``Neander: ADD works as expected``() =
        cpu.Registers.Accumulator <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Add
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 23uy
        Step cpu
        this.AssertNeanderState [Accumulator (12uy + 23uy); ProgramCounter 2uy; MemoryReads 3]

    [<TestMethod>]
    member this.``Neander: OR works as expected``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Or
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        this.AssertNeanderState [Accumulator (234uy ||| 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<TestMethod>]
    member this.``Neander: AND works as expected``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.And
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        this.AssertNeanderState [Accumulator (234uy &&& 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<TestMethod>]
    member this.``Neander: NOT works as expected``() =
        cpu.Registers.Accumulator <- 85uy  (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Not
        Step cpu
        this.AssertNeanderState [Accumulator 170uy (* 10101010 *); ProgramCounter 1uy; MemoryReads 1]

    [<TestMethod>]
    member this.``Neander: JMP changes Program Counter``() =
        this.TestJumpOperation(Instruction.Jmp, true)

    [<TestMethod>]
    member this.``Neander: JN jumps only if Negative flag is set``() =
        cpu.Registers.Flags.Negative <- false
        this.TestJumpOperation(Instruction.Jn, false)

        Reset cpu
        cpu.Registers.Flags.Negative <- true
        this.TestJumpOperation(Instruction.Jn, true)

    [<TestMethod>]
    member this.``Neander: JZ jumps only if Zero flag is set``() =
        cpu.Registers.Flags.Zero <- false
        this.TestJumpOperation(Instruction.Jz, false)

        Reset cpu
        cpu.Registers.Flags.Zero <- true
        this.TestJumpOperation(Instruction.Jz, true)

    [<TestMethod>]
    member this.``Neander: HLT sets Halted flag``() =
        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        Step cpu
        this.AssertNeanderState [FlagsHalted false]
        Step cpu
        this.AssertNeanderState [FlagsHalted true]
        Step cpu
        this.AssertNeanderState [FlagsHalted false]
