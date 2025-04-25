namespace Ufrgs.Inf.ArchSims.Core.Tests.Neander

open NUnit.Framework

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Neander

open Ufrgs.Inf.ArchSims.Core.Tests.Utils

type NeanderState =
    | Accumulator of byte
    | ProgramCounter of byte
    | MemoryReads of int
    | MemoryWrites of int
    | FlagsHalted of bool
    | FlagsNegative of bool
    | FlagsZero of bool
    | MemoryAt of int * byte
    | None

[<TestFixture>]
type NeanderTests() = 
    let mutable cpu = CreateCpu()

    let assertNeanderState states =
        for state in states do
            match state with
            | Accumulator ac -> cpu.Registers.Accumulator |>== ac
            | ProgramCounter pc -> cpu.Registers.ProgramCounter |>== pc
            | MemoryReads reads -> cpu.Memory.ReadCount |>== reads
            | MemoryWrites writes -> cpu.Memory.WriteCount |>== writes
            | FlagsHalted h -> cpu.Registers.Flags.Halted |>== h
            | FlagsNegative n -> cpu.Registers.Flags.Negative |>== n
            | FlagsZero z -> cpu.Registers.Flags.Zero |>== z
            | MemoryAt (addr, v) -> cpu.Memory.Data.[addr] |>== v
            | NeanderState.None -> ()

    let assertCpuStateIsClean() =
        assertNeanderState [ProgramCounter 0uy; Accumulator 0uy; 
            FlagsHalted false; FlagsNegative false; FlagsZero true; 
            MemoryReads 0; MemoryWrites 0]
        cpu.Registers.InstructionRegister |>== { OpCode = 0uy; OperandAddress = 0uy }
        cpu.Memory.Data |> Array.iter (equals 0uy) 

    let testJumpOperation instruction jumpExpected =
        cpu.Memory.Data.[0] <- byte instruction
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        let expectedPc = if jumpExpected then 123uy else 2uy
        assertNeanderState [ProgramCounter expectedPc; MemoryReads 2]

    [<SetUp>]
    member this.Setup() =
        cpu <- CreateCpu()
        
    [<Test>]
    member this.``Neander: New Cpu starts in clean state``() =
        assertCpuStateIsClean()
        
    [<Test>]
    member this.``Neander: Program Counter wraps at end of memory``() =
        cpu.Registers.ProgramCounter <- byte (cpu.Memory.Data.Length - 1)
        Step cpu
        assertNeanderState [ProgramCounter 0uy; MemoryReads 1]

    [<Test>]
    member this.``Neander: Reset reverts to clean state``() =
        cpu.Registers.Accumulator <- 1uy
        cpu.Registers.ProgramCounter <- 1uy
        MemoryReadByte cpu.Memory 1 |> ignore
        byte Instruction.Hlt |> MemoryWriteByte cpu.Memory 1
        Step cpu
        assertNeanderState [Accumulator 1uy; ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1]
        Reset cpu
        assertCpuStateIsClean()
        
    [<Test>]
    member this.``Neander: Flags Zero and Negative are set when Accumulator changes``() =
        cpu.Memory.Data.[0] <- byte Instruction.Not
        cpu.Memory.Data.[1] <- byte Instruction.Not
        for i = 0 to int System.Byte.MaxValue do
            cpu.Registers.Accumulator <- byte i
            cpu.Registers.ProgramCounter <- 0uy
            Step cpu
            let noti = ~~~(byte i)
            assertNeanderState [FlagsNegative (noti > 127uy); FlagsZero (noti = 0uy)]
            Step cpu
            assertNeanderState [FlagsNegative (i > 127); FlagsZero (i = 0)]

    [<Test>]
    member this.``Neander: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        Step cpu
        assertNeanderState [ProgramCounter 1uy; MemoryReads 1]

    [<Test>]
    member this.``Neander: LDA loads value from memory into Accumulator``() =
        cpu.Memory.Data.[0] <- byte Instruction.Lda
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        Step cpu
        assertNeanderState [Accumulator 234uy; ProgramCounter 2uy; MemoryReads 3]

    [<Test>]
    member this.``Neander: STA stores value from Accumulator into memory``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Sta
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        assertNeanderState [Accumulator 234uy; ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1; MemoryAt (123, 234uy)]

    [<Test>]
    member this.``Neander: ADD works as expected``() =
        cpu.Registers.Accumulator <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Add
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 23uy
        Step cpu
        assertNeanderState [Accumulator (12uy + 23uy); ProgramCounter 2uy; MemoryReads 3]

    [<Test>]
    member this.``Neander: OR works as expected``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Or
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        assertNeanderState [Accumulator (234uy ||| 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<Test>]
    member this.``Neander: AND works as expected``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.And
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        assertNeanderState [Accumulator (234uy &&& 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<Test>]
    member this.``Neander: NOT works as expected``() =
        cpu.Registers.Accumulator <- 85uy  (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Not
        Step cpu
        assertNeanderState [Accumulator 170uy (* 10101010 *); ProgramCounter 1uy; MemoryReads 1]

    [<Test>]
    member this.``Neander: JMP changes Program Counter``() =
        testJumpOperation Instruction.Jmp true

    [<Test>]
    member this.``Neander: JN jumps only if Negative flag is set``() =
        cpu.Registers.Flags.Negative <- false
        testJumpOperation Instruction.Jn false

        Reset cpu
        cpu.Registers.Flags.Negative <- true
        testJumpOperation Instruction.Jn true

    [<Test>]
    member this.``Neander: JZ jumps only if Zero flag is set``() =
        cpu.Registers.Flags.Zero <- false
        testJumpOperation Instruction.Jz false

        Reset cpu
        cpu.Registers.Flags.Zero <- true
        testJumpOperation Instruction.Jz true

    [<Test>]
    member this.``Neander: HLT sets Halted flag``() =
        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        Step cpu
        assertNeanderState [FlagsHalted false]
        Step cpu
        assertNeanderState [FlagsHalted true]
        Step cpu
        assertNeanderState [FlagsHalted false]
