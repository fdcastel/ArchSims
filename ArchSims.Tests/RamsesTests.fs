namespace Ufrgs.Inf.ArchSims.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Ramses
open Ufrgs.Inf.ArchSims.Assemblers.RamsesAssembler

type RamsesState =
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
type RamsesTests() = 
    let cpu = CreateCpu()

    member this.AssertRamsesState states =
        for state in states do
            match state with
            | Ra ra -> Assert.AreEqual(ra, cpu.Registers.Ra)
            | Rb rb -> Assert.AreEqual(rb, cpu.Registers.Rb)
            | Rx rx -> Assert.AreEqual(rx, cpu.Registers.Rx)
            | ProgramCounter pc -> Assert.AreEqual(pc, cpu.Registers.ProgramCounter)
            | MemoryReads reads -> Assert.AreEqual(reads, cpu.Memory.ReadCount)
            | MemoryWrites writes -> Assert.AreEqual(writes, cpu.Memory.WriteCount)
            | FlagsHalted h -> Assert.AreEqual(h, cpu.Registers.Flags.Halted)
            | FlagsNegative n -> Assert.AreEqual(n, cpu.Registers.Flags.Negative)
            | FlagsZero z -> Assert.AreEqual(z, cpu.Registers.Flags.Zero)
            | FlagsCarry c -> Assert.AreEqual(c, cpu.Registers.Flags.Carry)
            | None -> ()

    member this.AssertCpuStateIsClean() =
        this.AssertRamsesState [ProgramCounter 0uy; Ra 0uy; Rb 0uy; Rx 0uy; 
            FlagsHalted false; FlagsNegative false; FlagsZero true; FlagsCarry false; 
            MemoryReads 0; MemoryWrites 0]
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OpCode)
        Assert.AreEqual(0uy, cpu.Registers.InstructionRegister.OperandAddress)
        for i = 0 to cpu.Memory.Data.Length - 1 do
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
    member this.``Ramses: New Cpu starts in clean state``() =
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Ramses: Program Counter wraps at end of memory``() =
        cpu.Registers.ProgramCounter <- byte (cpu.Memory.Data.Length - 1)
        Step cpu
        this.AssertRamsesState [ProgramCounter 0uy; MemoryReads 1]

    [<TestMethod>]
    member this.``Ramses: Reset() reverts to clean state``() =
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
    member this.``Ramses: Flags Zero and Negative are set when a register changes``() =
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
    member this.``Ramses: AddressModes works as expected``() =
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
    member this.``Ramses: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        Step cpu
        this.AssertRamsesState [ProgramCounter 1uy; MemoryReads 1]

    [<TestMethod>]
    member this.``Ramses: LDR loads value from memory into any register``() =
        for r = 0 to 2 do
            Reset cpu
            cpu.Memory.Data.[0] <- byte Instruction.Ldr ||| (byte r <<< 2) ||| byte AddressMode.Immediate
            cpu.Memory.Data.[1] <- 123uy
            Step cpu
            let rCheck rr = if rr = r then 123uy else 0uy
            this.AssertRamsesState [Ra (rCheck 0); Rb (rCheck 1); Rx (rCheck 2); ProgramCounter 2uy; MemoryReads 2]

    [<TestMethod>]
    member this.``Ramses: STA stores value from any register into memory``() =
        for r = 0 to 2 do
            Reset cpu
            this.WriteRegister(r, 234uy)
            cpu.Memory.Data.[0] <- byte Instruction.Str ||| (byte r <<< 2) ||| byte AddressMode.Direct
            cpu.Memory.Data.[1] <- 123uy
            Step cpu
            Assert.AreEqual(234uy, cpu.Memory.Data.[123])
            this.AssertRamsesState [ProgramCounter 2uy; MemoryReads 2; MemoryWrites 1]

    [<TestMethod>]
    member this.``Ramses: ADD works as expected``() =
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
    member this.``Ramses: OR works as expected``() =
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.Or
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        this.AssertRamsesState [Ra (234uy ||| 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<TestMethod>]
    member this.``Ramses: AND works as expected``() =
        cpu.Registers.Ra <- 234uy
        cpu.Memory.Data.[0] <- byte Instruction.And
        cpu.Memory.Data.[1] <- 123uy
        cpu.Memory.Data.[123] <- 12uy
        Step cpu
        this.AssertRamsesState [Ra (234uy &&& 12uy); ProgramCounter 2uy; MemoryReads 3]

    [<TestMethod>]
    member this.``Ramses: NOT works as expected``() =
        cpu.Registers.Ra <- 85uy  (* 01010101 *)
        cpu.Memory.Data.[0] <- byte Instruction.Not
        Step cpu
        this.AssertRamsesState [Ra 170uy (* 10101010 *); ProgramCounter 1uy; MemoryReads 1]

    [<TestMethod>]
    member this.``Ramses: SUB works as expected``() =
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
    member this.``Ramses: JMP changes Program Counter``() =
        this.TestJumpOperation(Instruction.Jmp, true)

    [<TestMethod>]
    member this.``Ramses: JN jumps only if Negative flag is set``() =
        cpu.Registers.Flags.Negative <- false
        this.TestJumpOperation(Instruction.Jn, false)

        Reset cpu
        cpu.Registers.Flags.Negative <- true
        this.TestJumpOperation(Instruction.Jn, true)

    [<TestMethod>]
    member this.``Ramses: JZ jumps only if Zero flag is set``() =
        cpu.Registers.Flags.Zero <- false
        this.TestJumpOperation(Instruction.Jz, false)

        Reset cpu
        cpu.Registers.Flags.Zero <- true
        this.TestJumpOperation(Instruction.Jz, true)

    [<TestMethod>]
    member this.``Ramses: JC jumps only if Carry flag is set``() =
        cpu.Registers.Flags.Carry <- false
        this.TestJumpOperation(Instruction.Jc, false)

        Reset cpu
        cpu.Registers.Flags.Carry <- true
        this.TestJumpOperation(Instruction.Jc, true)

    [<TestMethod>]
    member this.``Ramses: JSR jumps and saves Program Counter``() =
        cpu.Memory.Data.[0] <- byte Instruction.Jsr
        cpu.Memory.Data.[1] <- 123uy
        Step cpu
        Assert.AreEqual(2uy, cpu.Memory.Data.[123])
        this.AssertRamsesState [ProgramCounter 124uy; MemoryReads 2; MemoryWrites 1]

    [<TestMethod>]
    member this.``Ramses: NEG works as expected``() =
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
    member this.``Ramses: SHR works as expected``() =
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
    member this.``Ramses: HLT sets Halted flag``() =
        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        Step cpu
        this.AssertRamsesState [FlagsHalted false]
        Step cpu
        this.AssertRamsesState [FlagsHalted true]
        Step cpu
        this.AssertRamsesState [FlagsHalted false]

    [<TestMethod>]
    member this.``Ramses: AssembleInstruction works as expected``() =
        Assert.AreEqual([byte Instruction.Nop], AssembleInstruction "NOP")
        Assert.AreEqual([byte Instruction.Hlt], AssembleInstruction "HLT")

        Assert.AreEqual([byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Direct; 12uy], AssembleInstruction "STR A 12")
        Assert.AreEqual([byte Instruction.Str ||| byte Register.Rb ||| byte AddressMode.Indirect; 23uy], AssembleInstruction "STR B 23,I")
        Assert.AreEqual([byte Instruction.Str ||| byte Register.Rx ||| byte AddressMode.Immediate; 34uy], AssembleInstruction "STR X #34")
        Assert.AreEqual([byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Indexed; 45uy], AssembleInstruction "STR A 45,X")

        Assert.AreEqual([byte Instruction.Not ||| byte Register.Ra], AssembleInstruction "NOT A")
        Assert.AreEqual([byte Instruction.Not ||| byte Register.Rb], AssembleInstruction "NOT B")
        Assert.AreEqual([byte Instruction.Not ||| byte Register.Rx], AssembleInstruction "NOT X")

        Assert.AreEqual([byte Instruction.Jmp ||| byte AddressMode.Direct; 12uy], AssembleInstruction "JMP 12")
        Assert.AreEqual([byte Instruction.Jmp ||| byte AddressMode.Indirect; 23uy], AssembleInstruction "JMP 23,I")
        Assert.AreEqual([byte Instruction.Jmp ||| byte AddressMode.Immediate; 34uy], AssembleInstruction "JMP #34")
        Assert.AreEqual([byte Instruction.Jmp ||| byte AddressMode.Indexed; 45uy], AssembleInstruction "JMP 45,X")

        Assert.AreEqual([byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 0uy], AssembleInstruction "LDR X #0")
        Assert.AreEqual([byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 127uy], AssembleInstruction "LDR X #127")
        Assert.AreEqual([byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 128uy], AssembleInstruction "LDR X #128")
        Assert.AreEqual([byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 255uy], AssembleInstruction "LDR X #255")

    [<TestMethod>]
    member this.``Ramses: DisassembleInstruction works as expected``() =
        Assert.AreEqual("NOP", DisassembleInstruction [byte Instruction.Nop])
        Assert.AreEqual("NOP", DisassembleInstruction [byte Instruction.Nop + 5uy])
        Assert.AreEqual("HLT", DisassembleInstruction [byte Instruction.Hlt])

        Assert.AreEqual("STR A 12", DisassembleInstruction [byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Direct; 12uy])
        Assert.AreEqual("STR B 23,I", DisassembleInstruction [byte Instruction.Str ||| byte Register.Rb ||| byte AddressMode.Indirect; 23uy])
        Assert.AreEqual("STR X #34", DisassembleInstruction [byte Instruction.Str ||| byte Register.Rx ||| byte AddressMode.Immediate; 34uy])
        Assert.AreEqual("STR A 45,X", DisassembleInstruction [byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Indexed; 45uy])

        Assert.AreEqual("NOT A", DisassembleInstruction [byte Instruction.Not ||| byte Register.Ra])
        Assert.AreEqual("NOT B", DisassembleInstruction [byte Instruction.Not ||| byte Register.Rb])
        Assert.AreEqual("NOT X", DisassembleInstruction [byte Instruction.Not ||| byte Register.Rx])

        Assert.AreEqual("JMP 12", DisassembleInstruction [byte Instruction.Jmp ||| byte AddressMode.Direct; 12uy])
        Assert.AreEqual("JMP 23,I", DisassembleInstruction [byte Instruction.Jmp ||| byte AddressMode.Indirect; 23uy])
        Assert.AreEqual("JMP #34", DisassembleInstruction [byte Instruction.Jmp ||| byte AddressMode.Immediate; 34uy])
        Assert.AreEqual("JMP 45,X", DisassembleInstruction [byte Instruction.Jmp ||| byte AddressMode.Indexed; 45uy])

        Assert.AreEqual("LDR X #0", DisassembleInstruction [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 0uy])
        Assert.AreEqual("LDR X #127", DisassembleInstruction [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 127uy])
        Assert.AreEqual("LDR X #128", DisassembleInstruction [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 128uy])
        Assert.AreEqual("LDR X #255", DisassembleInstruction [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 255uy])

    [<TestMethod>]
    member this.``Ramses: AssembleProgram works as expected``() =
        let program = """
            LDR A 10
            LDR B :L1
            LDR X :L1,I
            HLT              ; End of program
        @10
            123              ; Ra value
        :L1
            14               ; Rb value
        @14
            66               ; Rx value
        """

        let expectedProgram = [|byte Instruction.Ldr ||| byte Register.Ra ||| byte AddressMode.Direct;
                                10uy;
                                byte Instruction.Ldr ||| byte Register.Rb ||| byte AddressMode.Direct;
                                11uy;
                                byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Indirect;
                                11uy;
                                byte Instruction.Hlt;
                                0uy;
                                0uy;
                                0uy;
                                123uy;
                                14uy
                                0uy;
                                0uy;
                                66uy|]

        AssembleProgram cpu program
        let memoryArea = Array.sub cpu.Memory.Data 0 15
        Assert.AreEqual(0, Array.compareWith (fun a b -> if a = b then 0 else 1) expectedProgram memoryArea)

        Step cpu
        Step cpu
        Step cpu
        Step cpu
        this.AssertRamsesState [Ra 123uy; Rb 14uy; Rx 66uy; ProgramCounter 7uy; FlagsHalted true]

    [<TestMethod>]
    [<ExpectedException(typeof<System.Exception>, "Label indefinido: L1")>]
    member this.``Ramses: AssembleProgram fails with undeclared label``() =
        AssembleProgram cpu "LDR A :L1"
