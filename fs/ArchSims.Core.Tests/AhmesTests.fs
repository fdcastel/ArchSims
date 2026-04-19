namespace Ufrgs.Inf.ArchSims.Core.Tests.Ahmes

open NUnit.Framework

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Ahmes

open Ufrgs.Inf.ArchSims.Core.Tests.Utils

type AhmesState =
    | Accumulator of byte
    | ProgramCounter of byte
    | MemoryReads of int
    | MemoryWrites of int
    | FlagsHalted of bool
    | FlagsNegative of bool
    | FlagsZero of bool
    | FlagsCarry of bool
    | FlagsOverflow of bool
    | FlagsBorrow of bool
    | MemoryAt of int * byte
    | None

[<TestFixture>]
type AhmesTests() =
    let mutable cpu = CreateCpu()

    let assertAhmesState states =
        for state in states do
            match state with
            | Accumulator ac -> cpu.Registers.Accumulator |>== ac
            | ProgramCounter pc -> cpu.Registers.ProgramCounter |>== pc
            | MemoryReads reads -> cpu.Memory.ReadCount |>== reads
            | MemoryWrites writes -> cpu.Memory.WriteCount |>== writes
            | FlagsHalted h -> cpu.Registers.Flags.Halted |>== h
            | FlagsNegative n -> cpu.Registers.Flags.Negative |>== n
            | FlagsZero z -> cpu.Registers.Flags.Zero |>== z
            | FlagsCarry c -> cpu.Registers.Flags.Carry |>== c
            | FlagsOverflow v -> cpu.Registers.Flags.Overflow |>== v
            | FlagsBorrow b -> cpu.Registers.Flags.Borrow |>== b
            | MemoryAt (addr, v) -> cpu.Memory.Data.[addr] |>== v
            | AhmesState.None -> ()

    let assertCpuStateIsClean() =
        assertAhmesState [ProgramCounter 0uy; Accumulator 0uy;
            FlagsHalted false; FlagsNegative false; FlagsZero true;
            FlagsCarry false; FlagsOverflow false; FlagsBorrow false;
            MemoryReads 0; MemoryWrites 0]
        cpu.Registers.InstructionRegister |>== { OpCode = 0uy; OperandAddress = 0uy }
        cpu.Memory.Data |> Array.iter (equals 0uy)

    let testJumpOperation instruction jumpExpected =
        cpu.Memory.Data.[0] <- byte instruction
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        let expectedPc = if jumpExpected then 123uy else 2uy
        assertAhmesState [ProgramCounter expectedPc; MemoryReads 2]

    [<SetUp>]
    member this.Setup() =
        cpu <- CreateCpu()

    [<Test>]
    member this.``Ahmes: New Cpu starts in clean state``() =
        assertCpuStateIsClean()

    [<Test>]
    member this.``Ahmes: Program Counter wraps at end of memory``() =
        cpu.Registers.ProgramCounter <- byte (cpu.Memory.Data.Length - 1)
        Step cpu
        assertAhmesState [ProgramCounter 0uy; MemoryReads 1]

    [<Test>]
    member this.``Ahmes: Reset reverts to clean state``() =
        cpu.Registers.Accumulator <- 1uy
        cpu.Registers.ProgramCounter <- 1uy
        cpu.Registers.Flags.Carry <- true
        cpu.Registers.Flags.Overflow <- true
        cpu.Registers.Flags.Borrow <- true
        MemoryReadByte cpu.Memory 1 |> ignore
        byte Instruction.Hlt |> MemoryWriteByte cpu.Memory 1
        Step cpu
        assertAhmesState [Accumulator 1uy; ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1]
        Reset cpu
        assertCpuStateIsClean()

    [<Test>]
    member this.``Ahmes: Flags Zero and Negative are set when Accumulator changes``() =
        cpu.Memory.Data.[0] <- byte Instruction.Not
        cpu.Memory.Data.[1] <- byte Instruction.Not
        for i = 0 to int System.Byte.MaxValue do
            cpu.Registers.Accumulator <- byte i
            cpu.Registers.ProgramCounter <- 0uy
            Step cpu
            let noti = ~~~(byte i)
            assertAhmesState [FlagsNegative (noti > 127uy); FlagsZero (noti = 0uy)]
            Step cpu
            assertAhmesState [FlagsNegative (i > 127); FlagsZero (i = 0)]

    [<Test>]
    member this.``Ahmes: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        Step cpu
        assertAhmesState [ProgramCounter 1uy; MemoryReads 1]

    [<Test>]
    member this.``Ahmes: LDA loads value from memory into Accumulator``() =
        cpu.Memory.Data.[0] <- byte Instruction.Lda
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        Step cpu
        assertAhmesState [Accumulator 234uy; ProgramCounter 2uy; MemoryReads 3;
                          FlagsNegative true; FlagsZero false]

    [<Test>]
    member this.``Ahmes: STA stores value from Accumulator into memory``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Sta
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        assertAhmesState [Accumulator 234uy; ProgramCounter 2uy; MemoryReads 2;
                          MemoryWrites 1; MemoryAt (123, 234uy)]

    [<Test>]
    member this.``Ahmes: ADD without carry``() =
        cpu.Registers.Accumulator <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Add
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 23uy
        Step cpu
        assertAhmesState [Accumulator (12uy + 23uy); ProgramCounter 2uy; MemoryReads 3;
                          FlagsNegative false; FlagsZero false;
                          FlagsCarry false; FlagsOverflow false]

    [<Test>]
    member this.``Ahmes: ADD with unsigned carry (no signed overflow)``() =
        // 0xFF + 0x02 = 0x101 -> result 0x01, Carry=1, no V
        cpu.Registers.Accumulator <- 0xFFuy
        cpu.Memory.Data.[0] <- byte Instruction.Add
        cpu.Memory.Data.[1] <- 10uy
        cpu.Memory.Data.[10] <- 0x02uy
        Step cpu
        assertAhmesState [Accumulator 0x01uy; FlagsCarry true; FlagsOverflow false;
                          FlagsNegative false; FlagsZero false]

    [<Test>]
    member this.``Ahmes: ADD with signed overflow (positive + positive -> negative)``() =
        // 0x7F + 0x01 = 0x80; V=1, C=0
        cpu.Registers.Accumulator <- 0x7Fuy
        cpu.Memory.Data.[0] <- byte Instruction.Add
        cpu.Memory.Data.[1] <- 10uy
        cpu.Memory.Data.[10] <- 0x01uy
        Step cpu
        assertAhmesState [Accumulator 0x80uy; FlagsOverflow true; FlagsCarry false;
                          FlagsNegative true; FlagsZero false]

    [<Test>]
    member this.``Ahmes: ADD with signed overflow (negative + negative -> positive)``() =
        // 0x80 + 0xFF = 0x17F; low byte 0x7F; V=1, C=1
        cpu.Registers.Accumulator <- 0x80uy
        cpu.Memory.Data.[0] <- byte Instruction.Add
        cpu.Memory.Data.[1] <- 10uy
        cpu.Memory.Data.[10] <- 0xFFuy
        Step cpu
        assertAhmesState [Accumulator 0x7Fuy; FlagsOverflow true; FlagsCarry true;
                          FlagsNegative false; FlagsZero false]

    [<Test>]
    member this.``Ahmes: ADD yielding zero sets Zero flag and Carry``() =
        // 0xFF + 0x01 = 0x100; low byte 0x00; Z=1, C=1, V=0
        cpu.Registers.Accumulator <- 0xFFuy
        cpu.Memory.Data.[0] <- byte Instruction.Add
        cpu.Memory.Data.[1] <- 10uy
        cpu.Memory.Data.[10] <- 0x01uy
        Step cpu
        assertAhmesState [Accumulator 0x00uy; FlagsZero true; FlagsCarry true;
                          FlagsOverflow false; FlagsNegative false]

    [<Test>]
    member this.``Ahmes: OR works as expected``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Or
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        assertAhmesState [Accumulator (234uy ||| 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<Test>]
    member this.``Ahmes: AND works as expected``() =
        cpu.Registers.Accumulator <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.And
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        assertAhmesState [Accumulator (234uy &&& 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<Test>]
    member this.``Ahmes: NOT works as expected``() =
        cpu.Registers.Accumulator <- 85uy  (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Not
        Step cpu
        assertAhmesState [Accumulator 170uy (* 10101010 *); ProgramCounter 1uy; MemoryReads 1]

    [<Test>]
    member this.``Ahmes: Logical ops do not touch C, V, B``() =
        // Set the extra flags, run an OR that changes AC, verify only N and Z move.
        cpu.Registers.Accumulator <- 0x0Fuy
        cpu.Registers.Flags.Carry <- true
        cpu.Registers.Flags.Overflow <- true
        cpu.Registers.Flags.Borrow <- true
        cpu.Memory.Data.[0] <- byte Instruction.Or
        cpu.Memory.Data.[1] <- 10uy
        cpu.Memory.Data.[10] <- 0xF0uy
        Step cpu
        assertAhmesState [Accumulator 0xFFuy; FlagsNegative true; FlagsZero false;
                          FlagsCarry true; FlagsOverflow true; FlagsBorrow true]

    [<Test>]
    member this.``Ahmes: SUB without borrow``() =
        // 30 - 10 = 20; B=0, V=0
        cpu.Registers.Accumulator <- 30uy
        cpu.Memory.Data.[0] <- byte Instruction.Sub
        cpu.Memory.Data.[1] <- 10uy
        cpu.Memory.Data.[10] <- 10uy
        Step cpu
        assertAhmesState [Accumulator 20uy; ProgramCounter 2uy; MemoryReads 3;
                          FlagsBorrow false; FlagsOverflow false;
                          FlagsNegative false; FlagsZero false]

    [<Test>]
    member this.``Ahmes: SUB producing borrow``() =
        // 10 - 30 = -20 -> 0xEC (236); B=1, no signed overflow
        cpu.Registers.Accumulator <- 10uy
        cpu.Memory.Data.[0] <- byte Instruction.Sub
        cpu.Memory.Data.[1] <- 10uy
        cpu.Memory.Data.[10] <- 30uy
        Step cpu
        assertAhmesState [Accumulator 236uy; FlagsBorrow true; FlagsOverflow false;
                          FlagsNegative true; FlagsZero false]

    [<Test>]
    member this.``Ahmes: SUB with signed overflow (pos - neg -> neg)``() =
        // 0x7F - 0x80 = 0x7F + 0x80 = 0xFF; result negative; V=1, B=1 (since 127 < 128 unsigned)
        cpu.Registers.Accumulator <- 0x7Fuy
        cpu.Memory.Data.[0] <- byte Instruction.Sub
        cpu.Memory.Data.[1] <- 10uy
        cpu.Memory.Data.[10] <- 0x80uy
        Step cpu
        assertAhmesState [Accumulator 0xFFuy; FlagsOverflow true; FlagsBorrow true;
                          FlagsNegative true; FlagsZero false]

    [<Test>]
    member this.``Ahmes: SUB yielding zero sets Zero``() =
        cpu.Registers.Accumulator <- 5uy
        cpu.Memory.Data.[0] <- byte Instruction.Sub
        cpu.Memory.Data.[1] <- 10uy
        cpu.Memory.Data.[10] <- 5uy
        Step cpu
        assertAhmesState [Accumulator 0uy; FlagsZero true; FlagsNegative false;
                          FlagsBorrow false; FlagsOverflow false]

    [<Test>]
    member this.``Ahmes: JMP changes Program Counter``() =
        testJumpOperation Instruction.Jmp true

    [<Test>]
    member this.``Ahmes: JN jumps only if Negative flag is set``() =
        cpu.Registers.Flags.Negative <- false
        testJumpOperation Instruction.Jn false

        Reset cpu
        cpu.Registers.Flags.Negative <- true
        testJumpOperation Instruction.Jn true

    [<Test>]
    member this.``Ahmes: JP jumps only if Negative flag is clear``() =
        cpu.Registers.Flags.Negative <- true
        testJumpOperation Instruction.Jp false

        Reset cpu
        cpu.Registers.Flags.Negative <- false
        testJumpOperation Instruction.Jp true

    [<Test>]
    member this.``Ahmes: JZ jumps only if Zero flag is set``() =
        cpu.Registers.Flags.Zero <- false
        testJumpOperation Instruction.Jz false

        Reset cpu
        cpu.Registers.Flags.Zero <- true
        testJumpOperation Instruction.Jz true

    [<Test>]
    member this.``Ahmes: JNZ jumps only if Zero flag is clear``() =
        cpu.Registers.Flags.Zero <- true
        testJumpOperation Instruction.Jnz false

        Reset cpu
        cpu.Registers.Flags.Zero <- false
        testJumpOperation Instruction.Jnz true

    [<Test>]
    member this.``Ahmes: JC jumps only if Carry flag is set``() =
        cpu.Registers.Flags.Carry <- false
        testJumpOperation Instruction.Jc false

        Reset cpu
        cpu.Registers.Flags.Carry <- true
        testJumpOperation Instruction.Jc true

    [<Test>]
    member this.``Ahmes: JNC jumps only if Carry flag is clear``() =
        cpu.Registers.Flags.Carry <- true
        testJumpOperation Instruction.Jnc false

        Reset cpu
        cpu.Registers.Flags.Carry <- false
        testJumpOperation Instruction.Jnc true

    [<Test>]
    member this.``Ahmes: JV jumps only if Overflow flag is set``() =
        cpu.Registers.Flags.Overflow <- false
        testJumpOperation Instruction.Jv false

        Reset cpu
        cpu.Registers.Flags.Overflow <- true
        testJumpOperation Instruction.Jv true

    [<Test>]
    member this.``Ahmes: JNV jumps only if Overflow flag is clear``() =
        cpu.Registers.Flags.Overflow <- true
        testJumpOperation Instruction.Jnv false

        Reset cpu
        cpu.Registers.Flags.Overflow <- false
        testJumpOperation Instruction.Jnv true

    [<Test>]
    member this.``Ahmes: JB jumps only if Borrow flag is set``() =
        cpu.Registers.Flags.Borrow <- false
        testJumpOperation Instruction.Jb false

        Reset cpu
        cpu.Registers.Flags.Borrow <- true
        testJumpOperation Instruction.Jb true

    [<Test>]
    member this.``Ahmes: JNB jumps only if Borrow flag is clear``() =
        cpu.Registers.Flags.Borrow <- true
        testJumpOperation Instruction.Jnb false

        Reset cpu
        cpu.Registers.Flags.Borrow <- false
        testJumpOperation Instruction.Jnb true

    [<Test>]
    member this.``Ahmes: SHR shifts right, zero-fills top, carries LSB``() =
        cpu.Registers.Accumulator <- 0x03uy  // 0000 0011
        cpu.Memory.Data.[0] <- byte Instruction.Shr
        Step cpu
        assertAhmesState [Accumulator 0x01uy; FlagsCarry true; FlagsNegative false;
                          FlagsZero false; ProgramCounter 1uy; MemoryReads 1]

        Reset cpu
        cpu.Registers.Accumulator <- 0x80uy  // 1000 0000
        cpu.Memory.Data.[0] <- byte Instruction.Shr
        Step cpu
        assertAhmesState [Accumulator 0x40uy; FlagsCarry false; FlagsNegative false; FlagsZero false]

    [<Test>]
    member this.``Ahmes: SHR yielding zero``() =
        cpu.Registers.Accumulator <- 0x01uy
        cpu.Memory.Data.[0] <- byte Instruction.Shr
        Step cpu
        assertAhmesState [Accumulator 0x00uy; FlagsCarry true; FlagsZero true; FlagsNegative false]

    [<Test>]
    member this.``Ahmes: SHL shifts left, zero-fills bottom, carries MSB``() =
        cpu.Registers.Accumulator <- 0x81uy  // 1000 0001
        cpu.Memory.Data.[0] <- byte Instruction.Shl
        Step cpu
        assertAhmesState [Accumulator 0x02uy; FlagsCarry true; FlagsNegative false;
                          FlagsZero false; ProgramCounter 1uy; MemoryReads 1]

        Reset cpu
        cpu.Registers.Accumulator <- 0x40uy  // 0100 0000 -> 1000 0000
        cpu.Memory.Data.[0] <- byte Instruction.Shl
        Step cpu
        assertAhmesState [Accumulator 0x80uy; FlagsCarry false; FlagsNegative true; FlagsZero false]

    [<Test>]
    member this.``Ahmes: SHL yielding zero``() =
        cpu.Registers.Accumulator <- 0x80uy
        cpu.Memory.Data.[0] <- byte Instruction.Shl
        Step cpu
        assertAhmesState [Accumulator 0x00uy; FlagsCarry true; FlagsZero true; FlagsNegative false]

    [<Test>]
    member this.``Ahmes: ROR rotates right through carry``() =
        // Start with AC=0x01, C=0: outgoing bit (1) becomes new C, incoming (0) becomes new MSB.
        cpu.Registers.Accumulator <- 0x01uy
        cpu.Registers.Flags.Carry <- false
        cpu.Memory.Data.[0] <- byte Instruction.Ror
        Step cpu
        assertAhmesState [Accumulator 0x00uy; FlagsCarry true; FlagsZero true; FlagsNegative false]

        // Now AC=0x00, C=1: outgoing bit (0) becomes new C, incoming (1) becomes new MSB (0x80).
        cpu.Memory.Data.[1] <- byte Instruction.Ror
        Step cpu
        assertAhmesState [Accumulator 0x80uy; FlagsCarry false; FlagsNegative true; FlagsZero false]

    [<Test>]
    member this.``Ahmes: ROR preserves all bits across 9 rotations``() =
        cpu.Registers.Accumulator <- 0xB5uy  // 1011 0101
        cpu.Registers.Flags.Carry <- false
        for i = 0 to 8 do
            cpu.Memory.Data.[i] <- byte Instruction.Ror
            Step cpu
        // 9 rotations of a 9-bit ring (AC + C) returns to start
        assertAhmesState [Accumulator 0xB5uy; FlagsCarry false]

    [<Test>]
    member this.``Ahmes: ROL rotates left through carry``() =
        // Start with AC=0x80, C=0: outgoing MSB (1) becomes new C, incoming (0) becomes new LSB.
        cpu.Registers.Accumulator <- 0x80uy
        cpu.Registers.Flags.Carry <- false
        cpu.Memory.Data.[0] <- byte Instruction.Rol
        Step cpu
        assertAhmesState [Accumulator 0x00uy; FlagsCarry true; FlagsZero true; FlagsNegative false]

        // Now AC=0x00, C=1: outgoing MSB (0) becomes new C, incoming (1) becomes new LSB (0x01).
        cpu.Memory.Data.[1] <- byte Instruction.Rol
        Step cpu
        assertAhmesState [Accumulator 0x01uy; FlagsCarry false; FlagsNegative false; FlagsZero false]

    [<Test>]
    member this.``Ahmes: ROL preserves all bits across 9 rotations``() =
        cpu.Registers.Accumulator <- 0xB5uy
        cpu.Registers.Flags.Carry <- true
        for i = 0 to 8 do
            cpu.Memory.Data.[i] <- byte Instruction.Rol
            Step cpu
        assertAhmesState [Accumulator 0xB5uy; FlagsCarry true]

    [<Test>]
    member this.``Ahmes: HLT sets Halted flag``() =
        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        Step cpu
        assertAhmesState [FlagsHalted false]
        Step cpu
        assertAhmesState [FlagsHalted true]
        Step cpu
        assertAhmesState [FlagsHalted false]
