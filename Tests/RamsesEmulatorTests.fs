namespace Ufrgs.Inf.ArchSims.Tests

open System

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Ramses
open Ufrgs.Inf.ArchSims.Core.RamsesEmulator

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
    | None

[<TestClass>]
type RamsesEmulatorTests() = 
    let cpu = CreateCpu()

    let getDisplayByte address =
        let str = [|char cpu.Host.Memory.Data.[address]; char cpu.Host.Memory.Data.[address + 1]|] |> System.String
        Convert.ToByte(str, 16)
        
    let AssertEmulatorOutputIsConsistent () =
        Assert.AreEqual(byte cpu.Host.Registers.R.[0], cpu.Registers.Ra)
        Assert.AreEqual(byte cpu.Host.Registers.R.[1], cpu.Registers.Rb)
        Assert.AreEqual(byte cpu.Host.Registers.R.[2], cpu.Registers.Rx)
        Assert.AreEqual(byte cpu.Host.Registers.R.[3], cpu.Registers.ProgramCounter)
        Assert.AreEqual(cpu.Host.Registers.R.[4], 0us)
        Assert.AreEqual(cpu.Host.Registers.R.[5], 0us)
        Assert.AreEqual(cpu.Host.Registers.R.[6], 10000us)
        Assert.AreEqual(cpu.Host.Registers.Flags.Negative, cpu.Registers.Flags.Negative)
        Assert.AreEqual(cpu.Host.Registers.Flags.Zero,     cpu.Registers.Flags.Zero)
        Assert.AreEqual(cpu.Host.Registers.Flags.Carry,    cpu.Registers.Flags.Carry)

        Assert.AreEqual(getDisplayByte 0xFFE0, cpu.Registers.Ra)
        Assert.AreEqual(getDisplayByte 0xFFE8, cpu.Registers.Rb)
        Assert.AreEqual(getDisplayByte 0xFFF0, cpu.Registers.Rx)
        Assert.AreEqual(getDisplayByte 0xFFF8, cpu.Registers.ProgramCounter)
        Assert.AreEqual(cpu.Host.Memory.Data.[0xFFFC], if cpu.Registers.Flags.Halted   then byte 'H' else 0uy)
        Assert.AreEqual(cpu.Host.Memory.Data.[0xFFFD], if cpu.Registers.Flags.Negative then byte 'N' else 0uy)
        Assert.AreEqual(cpu.Host.Memory.Data.[0xFFFE], if cpu.Registers.Flags.Zero     then byte 'Z' else 0uy)
        Assert.AreEqual(cpu.Host.Memory.Data.[0xFFFF], if cpu.Registers.Flags.Carry    then byte 'C' else 0uy)

    member this.AssertRamsesState states =
        for state in states do
            match state with
            | Ra ra -> Assert.AreEqual(ra, cpu.Registers.Ra)
            | Rb rb -> Assert.AreEqual(rb, cpu.Registers.Rb)
            | Rx rx -> Assert.AreEqual(rx, cpu.Registers.Rx)
            | ProgramCounter pc -> Assert.AreEqual(pc, cpu.Registers.ProgramCounter)
            | MemoryReads reads -> () // Ignore
            | MemoryWrites writes -> () // Ignore
            | FlagsHalted h -> Assert.AreEqual(h, cpu.Registers.Flags.Halted)
            | FlagsNegative n -> Assert.AreEqual(n, cpu.Registers.Flags.Negative)
            | FlagsZero z -> Assert.AreEqual(z, cpu.Registers.Flags.Zero)
            | FlagsCarry c -> Assert.AreEqual(c, cpu.Registers.Flags.Carry)
            | None -> ()
        AssertEmulatorOutputIsConsistent ()

    member this.AssertCpuStateIsClean() =
        this.AssertRamsesState [ProgramCounter 0uy; Ra 0uy; Rb 0uy; Rx 0uy; 
            FlagsHalted false; FlagsNegative false; FlagsZero true; FlagsCarry false; 
            MemoryReads 0; MemoryWrites 0]
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OpCode)
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OperandAddress)
        for i = 0 to 255 do
            Assert.AreEqual(0uy, cpu.Memory.Data.[i])

    member this.WriteRegister(register, value) =
        match register with
        | 0 -> cpu.Registers.Ra <- value
        | 1 -> cpu.Registers.Rb <- value
        | 2 -> cpu.Registers.Rx <- value
        | _ -> cpu.Registers.ProgramCounter <- value

    member this.TestJumpOperation(instruction: Instruction, jumpExpected) =
        cpu.Memory.Data.[0] <- byte instruction
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        let expectedPc = if jumpExpected then 123uy else 2uy
        this.AssertRamsesState [ProgramCounter expectedPc; MemoryReads 2]

    [<TestInitialize>]
    member this.Setup() =
        Reset cpu
        
    [<TestMethod>]
    member this.``RamsesEmulator: New Cpu starts in clean state``() =
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``RamsesEmulator: Program Counter wraps at end of memory``() =
        cpu.Registers.ProgramCounter <- byte (cpu.Memory.Data.Length - 1)
        Step cpu
        this.AssertRamsesState [ProgramCounter 0uy; MemoryReads 1]

    [<TestMethod>]
    member this.``RamsesEmulator: Reset() reverts to clean state``() =
        cpu.Registers.Ra <- 1uy
        cpu.Registers.Rb <- 2uy
        cpu.Registers.Rx <- 3uy
        cpu.Registers.ProgramCounter <- 1uy
        MemoryReadByte cpu.Memory 1 |> ignore
        byte Instruction.Hlt |> MemoryWriteByte cpu.Memory 1
        Step cpu
        this.AssertRamsesState [Ra 1uy; Rb 2uy; Rx 3uy; ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1]
        Reset cpu
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``RamsesEmulator: Flags Zero and Negative are set when a register changes``() =
        for r = 0 to 2 do
            cpu.Memory.Data.[0] <- byte Instruction.Not ||| (byte r <<< 2)
            cpu.Memory.Data.[1] <- byte Instruction.Not ||| (byte r <<< 2)
            for i = 0 to int System.Byte.MaxValue do
                this.WriteRegister(r, byte i)
                cpu.Registers.ProgramCounter <- 0uy
                Step cpu
                let noti = ~~~(byte i)
                this.AssertRamsesState [FlagsNegative (noti > 127uy); FlagsZero (noti = 0uy)]
                Step cpu
                this.AssertRamsesState [FlagsNegative (i > 127); FlagsZero (i = 0)]

    [<TestMethod>]
    member this.``RamsesEmulator: AddressModes works as expected``() =
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Ra ||| byte AddressMode.Direct
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        Step cpu
        this.AssertRamsesState [Ra 234uy; ProgramCounter 2uy; MemoryReads 3]

        Reset cpu
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Rb ||| byte AddressMode.Indirect
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 234uy
        cpu.Memory.Data.[234] <- 245uy
        Step cpu
        this.AssertRamsesState [Rb 245uy; ProgramCounter 2uy; MemoryReads 4]

        Reset cpu
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        this.AssertRamsesState [Rx 123uy; ProgramCounter 2uy; MemoryReads 2]

        Reset cpu
        cpu.Registers.Rx <- 23uy
        cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| byte Register.Ra ||| byte AddressMode.Indexed
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[146] <- 234uy
        Step cpu
        this.AssertRamsesState [Ra 234uy; Rx 23uy; ProgramCounter 2uy; MemoryReads 3]

    [<TestMethod>]
    member this.``RamsesEmulator: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        Step cpu
        this.AssertRamsesState [ProgramCounter 1uy; MemoryReads 1]

    [<TestMethod>]
    member this.``RamsesEmulator: LDR loads value from memory into any register``() =
        for r = 0 to 2 do
            Reset cpu
            cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| (byte r <<< 2) ||| byte AddressMode.Immediate
            cpu.Memory.Data.[1] <- 123uy
            Step cpu
            let rCheck rr = if rr = r then 123uy else 0uy
            this.AssertRamsesState [Ra (rCheck 0); Rb (rCheck 1); Rx (rCheck 2); ProgramCounter 2uy; MemoryReads 2]

    [<TestMethod>]
    member this.``RamsesEmulator: STA stores value from any register into memory``() =
        for r = 0 to 2 do
            Reset cpu
            this.WriteRegister(r, 234uy)
            cpu.Memory.Data.[0] <- byte Instruction.Str ||| (byte r <<< 2) ||| byte AddressMode.Direct
            cpu.Memory.Data.[1] <- 123uy
            cpu.Memory.Data.[122] <- 10uy
            cpu.Memory.Data.[123] <- 20uy
            cpu.Memory.Data.[124] <- 30uy
            Step cpu
            Assert.AreEqual(10uy, cpu.Memory.Data.[122])
            Assert.AreEqual(234uy, cpu.Memory.Data.[123])
            Assert.AreEqual(30uy, cpu.Memory.Data.[124])
            this.AssertRamsesState [ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1]

    [<TestMethod>]
    member this.``RamsesEmulator: ADD works as expected``() =
        cpu.Registers.Ra <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Add ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 23uy
        Step cpu
        this.AssertRamsesState [Ra (12uy + 23uy); ProgramCounter 2uy; MemoryReads 2; FlagsNegative false; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- byte(256 - 12)
        cpu.Memory.Data.[0] <- byte Instruction.Add ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- byte(256 - 23)
        Step cpu
        this.AssertRamsesState [Ra (byte(256 - 12 - 23)); ProgramCounter 2uy; MemoryReads 2; FlagsNegative true; FlagsZero false; FlagsCarry true]

    [<TestMethod>]
    member this.``RamsesEmulator: OR works as expected``() =
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Or
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        this.AssertRamsesState [Ra (234uy ||| 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<TestMethod>]
    member this.``RamsesEmulator: AND works as expected``() =
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.And
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        this.AssertRamsesState [Ra (234uy &&& 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<TestMethod>]
    member this.``RamsesEmulator: NOT works as expected``() =
        cpu.Registers.Ra <- 85uy  (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Not
        Step cpu
        this.AssertRamsesState [Ra 170uy (* 10101010 *); ProgramCounter 1uy; MemoryReads 1]

    [<TestMethod>]
    member this.``RamsesEmulator: SUB works as expected``() =
        cpu.Registers.Ra <- 23uy
        cpu.Memory.Data.[0] <- byte Instruction.Sub ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 12uy
        Step cpu
        this.AssertRamsesState [Ra (23uy - 12uy); ProgramCounter 2uy; MemoryReads 2; FlagsNegative false; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- 12uy
        cpu.Memory.Data.[0] <- byte Instruction.Sub ||| byte AddressMode.Immediate
        cpu.Memory.Data.[1] <- 23uy
        Step cpu
        this.AssertRamsesState [Ra (byte(256 + 12 - 23)); ProgramCounter 2uy; MemoryReads 2; FlagsNegative true; FlagsZero false; FlagsCarry true]

    [<TestMethod>]
    member this.``RamsesEmulator: JMP changes Program Counter``() =
        this.TestJumpOperation(Instruction.Jmp, true)

    [<TestMethod>]
    member this.``RamsesEmulator: JN jumps only if Negative flag is set``() =
        cpu.Registers.Flags.Negative <- false
        this.TestJumpOperation(Instruction.Jn, false)

        Reset cpu
        cpu.Registers.Flags.Negative <- true
        this.TestJumpOperation(Instruction.Jn, true)

    [<TestMethod>]
    member this.``RamsesEmulator: JZ jumps only if Zero flag is set``() =
        cpu.Registers.Flags.Zero <- false
        this.TestJumpOperation(Instruction.Jz, false)

        Reset cpu
        cpu.Registers.Flags.Zero <- true
        this.TestJumpOperation(Instruction.Jz, true)

    [<TestMethod>]
    member this.``RamsesEmulator: JC jumps only if Carry flag is set``() =
        cpu.Registers.Flags.Carry <- false
        this.TestJumpOperation(Instruction.Jc, false)

        Reset cpu
        cpu.Registers.Flags.Carry <- true
        this.TestJumpOperation(Instruction.Jc, true)

    [<TestMethod>]
    member this.``RamsesEmulator: JSR jumps and saves Program Counter``() =
        cpu.Memory.Data.[0] <- byte Instruction.Jsr
        cpu.Memory.Data.[1] <- 123uy

        cpu.Memory.Data.[122] <- 10uy
        cpu.Memory.Data.[123] <- 20uy
        cpu.Memory.Data.[124] <- 30uy
        Step cpu
        Assert.AreEqual(10uy, cpu.Memory.Data.[122])
        Assert.AreEqual(2uy, cpu.Memory.Data.[123])
        Assert.AreEqual(30uy, cpu.Memory.Data.[124])
        this.AssertRamsesState [ProgramCounter 124uy; MemoryReads 2; MemoryWrites 1]

    [<TestMethod>]
    member this.``RamsesEmulator: NEG works as expected``() =
        cpu.Registers.Ra <- 23uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        Step cpu
        this.AssertRamsesState [Ra (byte(256 - 23)); ProgramCounter 1uy; MemoryReads 1; FlagsNegative true; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        Step cpu
        this.AssertRamsesState [Ra (byte(256 - 234)); ProgramCounter 1uy; MemoryReads 1; FlagsNegative false; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- 128uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        Step cpu
        this.AssertRamsesState [Ra (byte(256 - 128)); ProgramCounter 1uy; MemoryReads 1; FlagsNegative true; FlagsZero false; FlagsCarry false]

        Reset cpu
        cpu.Registers.Ra <- 0uy
        cpu.Memory.Data.[0] <- byte Instruction.Neg
        Step cpu
        this.AssertRamsesState [Ra (byte(256 - 0)); ProgramCounter 1uy; MemoryReads 1; FlagsNegative false; FlagsZero true; FlagsCarry true]

    [<TestMethod>]
    member this.``RamsesEmulator: SHR works as expected``() =
        cpu.Registers.Ra <- 85uy (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Shr
        Step cpu
        this.AssertRamsesState [Ra 42uy; ProgramCounter 1uy; MemoryReads 1; FlagsNegative false; FlagsZero false; FlagsCarry true]

        cpu.Memory.Data.[1] <- byte Instruction.Shr
        Step cpu
        this.AssertRamsesState [Ra 21uy; ProgramCounter 2uy; MemoryReads 2; FlagsNegative false; FlagsZero false; FlagsCarry false]

        cpu.Memory.Data.[2] <- byte Instruction.Shr
        Step cpu
        this.AssertRamsesState [Ra 10uy; ProgramCounter 3uy; MemoryReads 3; FlagsNegative false; FlagsZero false; FlagsCarry true]

    [<TestMethod>]
    member this.``RamsesEmulator: HLT sets Halted flag``() =
        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        Step cpu
        this.AssertRamsesState [FlagsHalted false]
        Step cpu
        this.AssertRamsesState [FlagsHalted true]
        Step cpu
        this.AssertRamsesState [FlagsHalted false]
