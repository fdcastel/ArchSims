namespace Ufrgs.Inf.ArchSims.Emulators.Tests.RamsesEmulator

open System

open NUnit.Framework

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Ramses
open Ufrgs.Inf.ArchSims.Core.Tests.Utils
open Ufrgs.Inf.ArchSims.Emulators.RamsesEmulator

type RamsesEmulatorState =
    | Ra of byte
    | Rb of byte
    | Rx of byte
    | ProgramCounter of byte
    | MemoryReads of int
    | MemoryWrites of int
    | FlagsHalted of bool
    | FlagsNegative of bool
    | FlagsZero of bool
    | FlagsCarry of bool
    | MemoryAt of int * byte
    | None

[<TestFixture>]
type RamsesEmulatorTests() = 
    let cpu = CreateCpu()

    let getDisplayByte address =
        let str = [|char cpu.Host.Memory.Data.[address]; char cpu.Host.Memory.Data.[address + 1]|] |> System.String
        Convert.ToByte(str, 16)

    let assertEmulatorOutputIsConsistent() =
        byte cpu.Host.Registers.R.[0] |>== cpu.Registers.Ra
        byte cpu.Host.Registers.R.[1] |>== cpu.Registers.Rb
        byte cpu.Host.Registers.R.[2] |>== cpu.Registers.Rx
        byte cpu.Host.Registers.R.[3] |>== cpu.Registers.ProgramCounter
        cpu.Host.Registers.R.[4] |>== 0us
        cpu.Host.Registers.R.[5] |>== 0us
        cpu.Host.Registers.R.[6] |>== 10000us
        cpu.Host.Registers.Flags.Negative |>== cpu.Registers.Flags.Negative
        cpu.Host.Registers.Flags.Zero |>== cpu.Registers.Flags.Zero
        cpu.Host.Registers.Flags.Carry |>== cpu.Registers.Flags.Carry

        getDisplayByte 0xFFE0 |>== cpu.Registers.Ra
        getDisplayByte 0xFFE8 |>== cpu.Registers.Rb
        getDisplayByte 0xFFF0 |>== cpu.Registers.Rx
        getDisplayByte 0xFFF8 |>== cpu.Registers.ProgramCounter
        cpu.Host.Memory.Data.[0xFFFC] |>== if cpu.Registers.Flags.Halted   then byte 'H' else 0uy
        cpu.Host.Memory.Data.[0xFFFD] |>== if cpu.Registers.Flags.Negative then byte 'N' else 0uy
        cpu.Host.Memory.Data.[0xFFFE] |>== if cpu.Registers.Flags.Zero     then byte 'Z' else 0uy
        cpu.Host.Memory.Data.[0xFFFF] |>== if cpu.Registers.Flags.Carry    then byte 'C' else 0uy

    let assertRamsesState states =
        for state in states do
            match state with
            | Ra ra -> cpu.Registers.Ra |>== ra
            | Rb rb -> cpu.Registers.Rb |>== rb
            | Rx rx -> cpu.Registers.Rx |>== rx
            | ProgramCounter pc -> cpu.Registers.ProgramCounter |>== pc
            | MemoryReads reads -> () // Ignore
            | MemoryWrites writes -> () // Ignore
            | FlagsHalted h -> cpu.Registers.Flags.Halted |>== h
            | FlagsNegative n -> cpu.Registers.Flags.Negative |>== n
            | FlagsZero z -> cpu.Registers.Flags.Zero |>== z
            | FlagsCarry c -> cpu.Registers.Flags.Carry |>== c
            | MemoryAt (addr, v) -> cpu.Memory.Data.[addr] |>== v
            | None -> ()
        assertEmulatorOutputIsConsistent()

    let assertCpuStateIsClean() =
        assertRamsesState [ProgramCounter 0uy; Ra 0uy; Rb 0uy; Rx 0uy; 
            FlagsHalted false; FlagsNegative false; FlagsZero true; FlagsCarry false; 
            MemoryReads 0; MemoryWrites 0]
        cpu.Registers.InstructionRegister.OpCode |>== 0uy
        cpu.Registers.InstructionRegister.OperandAddress |>== 0uy
        cpu.Memory.Data.[0..255] |> Array.iter (equals 0uy) 

    let writeRegister register value =
        match register with
        | 0 -> cpu.Registers.Ra <- value
        | 1 -> cpu.Registers.Rb <- value
        | 2 -> cpu.Registers.Rx <- value
        | _ -> cpu.Registers.ProgramCounter <- value

    let testJumpOperation instruction jumpExpected =
        cpu.Memory.Data.[0] <- byte instruction
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        let expectedPc = if jumpExpected then 123uy else 2uy
        assertRamsesState [ProgramCounter expectedPc; MemoryReads 2]

    [<SetUp>]
    member this.Setup() =
        Reset cpu
        
    [<Test>]
    member this.``RamsesEmulator: New Cpu starts in clean state``() =
        assertCpuStateIsClean()
        
    [<Test>]
    member this.``RamsesEmulator: Program Counter wraps at end of memory``() =
        cpu.Registers.ProgramCounter <- byte (cpu.Memory.Data.Length - 1)
        Step cpu
        assertRamsesState [ProgramCounter 0uy; MemoryReads 1]

    [<Test>]
    member this.``RamsesEmulator: Reset() reverts to clean state``() =
        cpu.Registers.Ra <- 1uy
        cpu.Registers.Rb <- 2uy
        cpu.Registers.Rx <- 3uy
        cpu.Registers.ProgramCounter <- 1uy
        MemoryReadByte cpu.Memory 1 |> ignore
        byte Instruction.Hlt |> MemoryWriteByte cpu.Memory 1
        Step cpu
        assertRamsesState [Ra 1uy; Rb 2uy; Rx 3uy; ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1]
        Reset cpu
        assertCpuStateIsClean()
        
    [<Test>]
    member this.``RamsesEmulator: Flags Zero and Negative are set when a register changes``() =
        for r = 0 to 2 do
            cpu.Memory.Data.[0] <- byte Instruction.Not ||| (byte r <<< 2)
            cpu.Memory.Data.[1] <- byte Instruction.Not ||| (byte r <<< 2)
            for i = 0 to int System.Byte.MaxValue do
                byte i |> writeRegister r
                cpu.Registers.ProgramCounter <- 0uy
                Step cpu
                let noti = ~~~(byte i)
                assertRamsesState [FlagsNegative (noti > 127uy); FlagsZero (noti = 0uy)]
                Step cpu
                assertRamsesState [FlagsNegative (i > 127); FlagsZero (i = 0)]

    [<Test>]
    member this.``RamsesEmulator: AddressModes works as expected``() =
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Ra ||| byte AddressMode.Direct
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        Step cpu
        assertRamsesState [Ra 234uy; ProgramCounter 2uy; MemoryReads 3]

        Reset cpu
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Rb ||| byte AddressMode.Indirect
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        cpu.Memory.Data.[234] <- 245uy
        Step cpu
        assertRamsesState [Rb 245uy; ProgramCounter 2uy; MemoryReads 4]

        Reset cpu
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        assertRamsesState [Rx 123uy; ProgramCounter 2uy; MemoryReads 2]

        Reset cpu
        cpu.Registers.Rx <- 23uy
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Ra ||| byte AddressMode.Indexed
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[146] <- 234uy
        Step cpu
        assertRamsesState [Ra 234uy; Rx 23uy; ProgramCounter 2uy; MemoryReads 3]

    [<Test>]
    member this.``RamsesEmulator: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        Step cpu
        assertRamsesState [ProgramCounter 1uy; MemoryReads 1]

    [<Test>]
    member this.``RamsesEmulator: LDR loads value from memory into any register``() =
        for r = 0 to 2 do
            Reset cpu
            cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| (byte r <<< 2) ||| byte AddressMode.Immediate
            cpu.Memory.Data.[1] <- 123uy
            Step cpu
            let rCheck rr = if rr = r then 123uy else 0uy
            assertRamsesState [Ra (rCheck 0); Rb (rCheck 1); Rx (rCheck 2); ProgramCounter 2uy; MemoryReads 2]

    [<Test>]
    member this.``RamsesEmulator: STA stores value from any register into memory``() =
        for r = 0 to 2 do
            Reset cpu
            234uy |> writeRegister r
            cpu.Memory.Data.[0] <- byte Instruction.Str ||| (byte r <<< 2) ||| byte AddressMode.Direct
            cpu.Memory.Data.[1] <- 123uy
            cpu.Memory.Data.[122] <- 10uy
            cpu.Memory.Data.[123] <- 20uy
            cpu.Memory.Data.[124] <- 30uy
            Step cpu
            assertRamsesState [ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1; MemoryAt (122, 10uy); MemoryAt (123, 234uy); MemoryAt (124, 30uy)]

    [<Test>]
    member this.``RamsesEmulator: ADD works as expected``() =
        cpu.Registers.Ra <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Add ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 23uy
        Step cpu
        assertRamsesState [Ra (12uy + 23uy); ProgramCounter 2uy; MemoryReads 2; FlagsNegative false; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- byte(256 - 12)
        cpu.Memory.Data.[0] <- byte Instruction.Add ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- byte(256 - 23)
        Step cpu
        assertRamsesState [Ra (byte(256 - 12 - 23)); ProgramCounter 2uy; MemoryReads 2; FlagsNegative true; FlagsZero false; FlagsCarry true]

    [<Test>]
    member this.``RamsesEmulator: OR works as expected``() =
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Or
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        assertRamsesState [Ra (234uy ||| 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<Test>]
    member this.``RamsesEmulator: AND works as expected``() =
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.And
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        assertRamsesState [Ra (234uy &&& 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<Test>]
    member this.``RamsesEmulator: NOT works as expected``() =
        cpu.Registers.Ra <- 85uy  (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Not
        Step cpu
        assertRamsesState [Ra 170uy (* 10101010 *); ProgramCounter 1uy; MemoryReads 1]

    [<Test>]
    member this.``RamsesEmulator: SUB works as expected``() =
        cpu.Registers.Ra <- 23uy
        cpu.Memory.Data.[0] <- byte Instruction.Sub ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 12uy
        Step cpu
        assertRamsesState [Ra (23uy - 12uy); ProgramCounter 2uy; MemoryReads 2; FlagsNegative false; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Sub ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 23uy
        Step cpu
        assertRamsesState [Ra (byte(256 + 12 - 23)); ProgramCounter 2uy; MemoryReads 2; FlagsNegative true; FlagsZero false; FlagsCarry true]

    [<Test>]
    member this.``RamsesEmulator: JMP changes Program Counter``() =
        testJumpOperation Instruction.Jmp true

    [<Test>]
    member this.``RamsesEmulator: JN jumps only if Negative flag is set``() =
        cpu.Registers.Flags.Negative <- false
        testJumpOperation Instruction.Jn false

        Reset cpu
        cpu.Registers.Flags.Negative <- true
        testJumpOperation Instruction.Jn true

    [<Test>]
    member this.``RamsesEmulator: JZ jumps only if Zero flag is set``() =
        cpu.Registers.Flags.Zero <- false
        testJumpOperation Instruction.Jz false

        Reset cpu
        cpu.Registers.Flags.Zero <- true
        testJumpOperation Instruction.Jz true

    [<Test>]
    member this.``RamsesEmulator: JC jumps only if Carry flag is set``() =
        cpu.Registers.Flags.Carry <- false
        testJumpOperation Instruction.Jc false

        Reset cpu
        cpu.Registers.Flags.Carry <- true
        testJumpOperation Instruction.Jc true

    [<Test>]
    member this.``RamsesEmulator: JSR jumps and saves Program Counter``() =
        cpu.Memory.Data.[0] <- byte Instruction.Jsr
        cpu.Memory.Data.[1] <- 123uy

        cpu.Memory.Data.[122] <- 10uy
        cpu.Memory.Data.[123] <- 20uy
        cpu.Memory.Data.[124] <- 30uy
        Step cpu
        assertRamsesState [ProgramCounter 124uy; MemoryReads 2; MemoryWrites 1; MemoryAt (122, 10uy); MemoryAt (123, 2uy); MemoryAt (124, 30uy)]

    [<Test>]
    member this.``RamsesEmulator: NEG works as expected``() =
        cpu.Registers.Ra <- 23uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        Step cpu
        assertRamsesState [Ra (byte(256 - 23)); ProgramCounter 1uy; MemoryReads 1; FlagsNegative true; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        Step cpu
        assertRamsesState [Ra (byte(256 - 234)); ProgramCounter 1uy; MemoryReads 1; FlagsNegative false; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- 128uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        Step cpu
        assertRamsesState [Ra (byte(256 - 128)); ProgramCounter 1uy; MemoryReads 1; FlagsNegative true; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- 0uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        Step cpu
        assertRamsesState [Ra (byte(256 - 0)); ProgramCounter 1uy; MemoryReads 1; FlagsNegative false; FlagsZero true; FlagsCarry true]

    [<Test>]
    member this.``RamsesEmulator: SHR works as expected``() =
        cpu.Registers.Ra <- 85uy (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Shr
        Step cpu
        assertRamsesState [Ra 42uy; ProgramCounter 1uy; MemoryReads 1; FlagsNegative false; FlagsZero false; FlagsCarry true]

        cpu.Memory.Data.[1] <- byte Instruction.Shr
        Step cpu
        assertRamsesState [Ra 21uy; ProgramCounter 2uy; MemoryReads 2; FlagsNegative false; FlagsZero false; FlagsCarry false]

        cpu.Memory.Data.[2] <- byte Instruction.Shr
        Step cpu
        assertRamsesState [Ra 10uy; ProgramCounter 3uy; MemoryReads 3; FlagsNegative false; FlagsZero false; FlagsCarry true]

    [<Test>]
    member this.``RamsesEmulator: HLT sets Halted flag``() =
        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        Step cpu
        assertRamsesState [FlagsHalted false]
        Step cpu
        assertRamsesState [FlagsHalted true]
        Step cpu
        assertRamsesState [FlagsHalted false]
