namespace Ufrgs.Inf.ArchSims.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Memory
open Ufrgs.Inf.ArchSims.Cesar

type CesarState =
    | R0 of uint16
    | R1 of uint16
    | R2 of uint16
    | R3 of uint16
    | R4 of uint16
    | R5 of uint16
    | R6 of uint16
    | ProgramCounter of uint16
    | MemoryReads of int
    | MemoryWrites of int
    | FlagsHalted of bool
    | FlagsNegative of bool
    | FlagsZero of bool
    | FlagsOverflow of bool
    | FlagsCarry of bool
    | None

[<TestClass>]
type CesarTests() = 
    let cpu = CreateCpu()

    member this.AssertCesarState states =
        for state in states do
            match state with
            | R0 r0 -> Assert.AreEqual(r0, cpu.Registers.R.[0])
            | R1 r1 -> Assert.AreEqual(r1, cpu.Registers.R.[1])
            | R2 r2 -> Assert.AreEqual(r2, cpu.Registers.R.[2])
            | R3 r3 -> Assert.AreEqual(r3, cpu.Registers.R.[3])
            | R4 r4 -> Assert.AreEqual(r4, cpu.Registers.R.[4])
            | R5 r5 -> Assert.AreEqual(r5, cpu.Registers.R.[5])
            | R6 r6 -> Assert.AreEqual(r6, cpu.Registers.R.[6])
            | ProgramCounter pc -> Assert.AreEqual(pc, cpu.Registers.R.[7])
            | MemoryReads reads -> Assert.AreEqual(reads, cpu.Memory.ReadCount)
            | MemoryWrites writes -> Assert.AreEqual(writes, cpu.Memory.WriteCount)
            | FlagsHalted h -> Assert.AreEqual(h, cpu.Registers.Flags.Halted)
            | FlagsNegative n -> Assert.AreEqual(n, cpu.Registers.Flags.Negative)
            | FlagsZero z -> Assert.AreEqual(z, cpu.Registers.Flags.Zero)
            | FlagsOverflow z -> Assert.AreEqual(z, cpu.Registers.Flags.Overflow)
            | FlagsCarry c -> Assert.AreEqual(c, cpu.Registers.Flags.Carry)
            | None -> ()

    member this.AssertCpuStateIsClean() =
        this.AssertCesarState [R0 0us; R1 0us; R2 0us; R3 0us; R4 0us; R5 0us; R6 0us; ProgramCounter 0us; 
            FlagsHalted false; FlagsNegative false; FlagsZero true; FlagsOverflow false; FlagsCarry false; 
            MemoryReads 0; MemoryWrites 0]
        Assert.AreEqual(0, cpu.Registers.InstructionRegister.Data.Length)
        for i = 0 to cpu.Memory.Data.Length - 1 do
            Assert.AreEqual(0uy, cpu.Memory.Data.[i])

    member this.TestAddressMode(addressMode) =
        Reset cpu
        let encodedInstruction = EncodeInstructionTwoOperand Instruction.Mov addressMode Register.R1 AddressMode.Register Register.R0
        cpu.Memory.Data.[0] <- byte (encodedInstruction >>> 8)
        cpu.Memory.Data.[1] <- byte encodedInstruction

        let expectedState = 
            match addressMode with
            | AddressMode.Register ->
                cpu.Registers.R.[1] <- 0x1234us
                [R0 0x1234us; R1 0x1234us; ProgramCounter 2us; MemoryReads 2]

            | AddressMode.RegPostInc ->
                cpu.Memory.Data.[10] <- 0x12uy
                cpu.Memory.Data.[11] <- 0x34uy
                cpu.Registers.R.[1] <- 10us
                [R0 0x1234us; R1 12us; ProgramCounter 2us; MemoryReads 4]

            | AddressMode.RegPreDec ->
                cpu.Memory.Data.[10] <- 0x12uy
                cpu.Memory.Data.[11] <- 0x34uy
                cpu.Registers.R.[1] <- 12us
                [R0 0x1234us; R1 10us; ProgramCounter 2us; MemoryReads 4]

            | AddressMode.Indexed ->
                cpu.Memory.Data.[2] <- 0uy
                cpu.Memory.Data.[3] <- 4uy
                cpu.Memory.Data.[10] <- 0x12uy
                cpu.Memory.Data.[11] <- 0x34uy
                cpu.Registers.R.[1] <- 6us
                [R0 0x1234us; R1 6us; ProgramCounter 4us; MemoryReads 6]

            | AddressMode.RegisterIndirect ->
                cpu.Memory.Data.[10] <- 0x12uy
                cpu.Memory.Data.[11] <- 0x34uy
                cpu.Registers.R.[1] <- 10us
                [R0 0x1234us; R1 10us; ProgramCounter 2us; MemoryReads 4]

            | AddressMode.RegPostIncIndirect ->
                cpu.Memory.Data.[10] <- 0uy
                cpu.Memory.Data.[11] <- 20uy
                cpu.Memory.Data.[20] <- 0x12uy
                cpu.Memory.Data.[21] <- 0x34uy
                cpu.Registers.R.[1] <- 10us
                [R0 0x1234us; R1 12us; ProgramCounter 2us; MemoryReads 6]

            | AddressMode.RegPreDecIndirect ->
                cpu.Memory.Data.[10] <- 0uy
                cpu.Memory.Data.[11] <- 20uy
                cpu.Memory.Data.[20] <- 0x12uy
                cpu.Memory.Data.[21] <- 0x34uy
                cpu.Registers.R.[1] <- 12us
                [R0 0x1234us; R1 10us; ProgramCounter 2us; MemoryReads 6]

            | AddressMode.IndexedIndirect ->
                cpu.Memory.Data.[2] <- 0uy
                cpu.Memory.Data.[3] <- 4uy
                cpu.Memory.Data.[10] <- 0uy
                cpu.Memory.Data.[11] <- 20uy
                cpu.Memory.Data.[20] <- 0x12uy
                cpu.Memory.Data.[21] <- 0x34uy
                cpu.Registers.R.[1] <- 6us
                [R0 0x1234us; R1 6us; ProgramCounter 4us; MemoryReads 8]

            | _ -> failwith "Invalid AddressMode"

        Step cpu
        this.AssertCesarState expectedState 

    member this.TestBranchOperation(instruction: Instruction, branchExpected, expectedState) =
        let address = int cpu.Registers.R.[7]
        let memoryReads = cpu.Memory.ReadCount

        cpu.Memory.Data.[address] <- byte instruction
        cpu.Memory.Data.[address + 1] <- 3uy
        Step cpu

        let expectedPc = uint16 address + (if branchExpected then 5us else 2us)
        this.AssertCesarState [ProgramCounter expectedPc; MemoryReads (memoryReads + 2)]
        this.AssertCesarState expectedState

    member this.TestClrGroupOperation(instruction: Instruction, value, expectedResult, expectedState) =
        let address = int cpu.Registers.R.[7]
        let memoryReads = cpu.Memory.ReadCount

        cpu.Memory.Data.[address] <- byte instruction
        cpu.Memory.Data.[address + 1] <- byte Register.R4
        cpu.Registers.R.[4] <- value
        Step cpu

        this.AssertCesarState [R4 expectedResult; ProgramCounter (uint16 address + 2us); 
                               FlagsNegative (expectedResult > 0x7FFFus); FlagsZero (expectedResult = 0us);
                               MemoryReads (memoryReads + 2)]
        this.AssertCesarState expectedState

    member this.TestMovGroupOperation(instruction: Instruction, sourceValue, targetValue, expectedResult, expectedState) =
        let encodedInstruction = EncodeInstructionTwoOperand instruction AddressMode.Register Register.R5 AddressMode.Register Register.R6
        cpu.Memory.Data.[0] <- byte (encodedInstruction >>> 8)
        cpu.Memory.Data.[1] <- byte encodedInstruction
        cpu.Registers.R.[5] <- sourceValue
        cpu.Registers.R.[6] <- targetValue
        Step cpu

        let expectedR6 = if instruction = Instruction.Cmp then targetValue else expectedResult

        this.AssertCesarState [R5 sourceValue; R6 expectedR6; ProgramCounter 2us; MemoryReads 2]
        this.AssertCesarState expectedState

    member this.TestCmpOperation(sourceValue, targetValue) =
        Reset cpu
        this.TestMovGroupOperation(Instruction.Cmp, sourceValue, targetValue, 0us, [])

        let source = if sourceValue > 0x7FFFus then -(0x10000 - int sourceValue) else int sourceValue
        let target = if targetValue > 0x7FFFus then -(0x10000 - int targetValue) else int targetValue
        let expectedState = [R5 sourceValue; R6 targetValue]

        this.TestBranchOperation(Instruction.Bne, source <> target, expectedState)
        this.TestBranchOperation(Instruction.Beq, (source = target), expectedState)
        this.TestBranchOperation(Instruction.Bge, source >= target, expectedState)
        this.TestBranchOperation(Instruction.Blt, source <  target, expectedState)
        this.TestBranchOperation(Instruction.Bgt, source >  target, expectedState)
        this.TestBranchOperation(Instruction.Ble, source <= target, expectedState)

    [<TestInitialize>]
    member this.Setup() =
        Reset cpu
        
    [<TestMethod>]
    member this.``Cesar: New Cpu starts in clean state``() =
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Cesar: Program Counter wraps at end of memory``() =
        cpu.Registers.R.[7] <- uint16 (cpu.Memory.Data.Length - 1)
        Step cpu
        this.AssertCesarState [ProgramCounter 0us; MemoryReads 1]

    [<TestMethod>]
    member this.``Cesar: Reset() reverts to clean state``() =
        cpu.Registers.R.[0] <- 1us
        cpu.Registers.R.[1] <- 2us
        cpu.Registers.R.[2] <- 3us
        cpu.Registers.R.[7] <- 1us
        MemoryReadByte cpu.Memory 1 |> ignore
        byte Instruction.Hlt |> MemoryWriteByte cpu.Memory 1
        Step cpu
        this.AssertCesarState [R0 1us; R1 2us; R2 3us; ProgramCounter 2us; MemoryReads 2; MemoryWrites 1]
        Reset cpu
        this.AssertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Cesar: AddressModes works as expected``() =
        this.TestAddressMode(AddressMode.Register)
        this.TestAddressMode(AddressMode.RegPostInc)
        this.TestAddressMode(AddressMode.RegPreDec)
        this.TestAddressMode(AddressMode.Indexed)
        this.TestAddressMode(AddressMode.RegisterIndirect)
        this.TestAddressMode(AddressMode.RegPostIncIndirect)
        this.TestAddressMode(AddressMode.RegPreDecIndirect)
        this.TestAddressMode(AddressMode.IndexedIndirect)

    [<TestMethod>]
    member this.``Cesar: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        Step cpu
        this.AssertCesarState [ProgramCounter 1us; MemoryReads 1]

    [<TestMethod>]
    member this.``Cesar: CCC clears flags``() =
        for flags = 0 to 15 do
            cpu.Registers.R.[7] <- 0us
            cpu.Registers.Flags.Negative <- true
            cpu.Registers.Flags.Zero <- true
            cpu.Registers.Flags.Overflow <- true
            cpu.Registers.Flags.Carry <- true

            cpu.Memory.Data.[0] <- byte Instruction.Ccc ||| byte flags
            Step cpu

            let mustClearNegative = (flags &&& int Flag.Negative) <> 0
            let mustClearZero = (flags &&& int Flag.Zero) <> 0
            let mustClearOverflow = (flags &&& int Flag.Overflow) <> 0
            let mustClearCarry = (flags &&& int Flag.Carry) <> 0
            
            this.AssertCesarState [ FlagsNegative (not mustClearNegative);
                                    FlagsZero (not mustClearZero);
                                    FlagsOverflow (not mustClearOverflow);
                                    FlagsCarry (not mustClearCarry) ]

    [<TestMethod>]
    member this.``Cesar: SCC sets flags``() =
        for flags = 0 to 15 do
            cpu.Registers.R.[7] <- 0us
            cpu.Registers.Flags.Negative <- false
            cpu.Registers.Flags.Zero <- false
            cpu.Registers.Flags.Overflow <- false
            cpu.Registers.Flags.Carry <- false

            cpu.Memory.Data.[0] <- byte Instruction.Scc ||| byte flags
            Step cpu

            let mustSetNegative = (flags &&& int Flag.Negative) <> 0
            let mustSetZero = (flags &&& int Flag.Zero) <> 0
            let mustSetOverflow = (flags &&& int Flag.Overflow) <> 0
            let mustSetCarry = (flags &&& int Flag.Carry) <> 0
            
            this.AssertCesarState [ FlagsNegative mustSetNegative;
                                    FlagsZero mustSetZero;
                                    FlagsOverflow mustSetOverflow;
                                    FlagsCarry mustSetCarry ]

    [<TestMethod>]
    member this.``Cesar: Branches instructions works as expected``() =
        this.TestBranchOperation(Instruction.Br, true, [])

        for flags = 0 to 15 do
            cpu.Registers.R.[7] <- 0us
            cpu.Registers.Flags.Negative <- (flags &&& int Flag.Negative) <> 0
            cpu.Registers.Flags.Zero <- (flags &&& int Flag.Zero) <> 0
            cpu.Registers.Flags.Overflow <- (flags &&& int Flag.Overflow) <> 0
            cpu.Registers.Flags.Carry <- (flags &&& int Flag.Carry) <> 0

            this.TestBranchOperation(Instruction.Bne, not cpu.Registers.Flags.Zero, [])
            this.TestBranchOperation(Instruction.Beq, cpu.Registers.Flags.Zero, [])
            this.TestBranchOperation(Instruction.Bpl, not cpu.Registers.Flags.Negative, [])
            this.TestBranchOperation(Instruction.Bmi, cpu.Registers.Flags.Negative, [])
            this.TestBranchOperation(Instruction.Bvc, not cpu.Registers.Flags.Overflow, [])
            this.TestBranchOperation(Instruction.Bvs, cpu.Registers.Flags.Overflow, [])
            this.TestBranchOperation(Instruction.Bcc, not cpu.Registers.Flags.Carry, [])
            this.TestBranchOperation(Instruction.Bcs, cpu.Registers.Flags.Carry, [])
            this.TestBranchOperation(Instruction.Bge, cpu.Registers.Flags.Negative = cpu.Registers.Flags.Overflow, [])
            this.TestBranchOperation(Instruction.Blt, cpu.Registers.Flags.Negative <> cpu.Registers.Flags.Overflow, [])
            this.TestBranchOperation(Instruction.Bgt, (cpu.Registers.Flags.Negative = cpu.Registers.Flags.Overflow) && not cpu.Registers.Flags.Zero, [])
            this.TestBranchOperation(Instruction.Ble, (cpu.Registers.Flags.Negative <> cpu.Registers.Flags.Overflow) || cpu.Registers.Flags.Zero, [])
            this.TestBranchOperation(Instruction.Bhi, not cpu.Registers.Flags.Carry && not cpu.Registers.Flags.Zero, [])
            this.TestBranchOperation(Instruction.Bls, cpu.Registers.Flags.Carry || cpu.Registers.Flags.Zero, [])

    [<TestMethod>]
    member this.``Cesar: JMP changes Program Counter``() =
        cpu.Memory.Data.[0] <- byte Instruction.Jmp
        cpu.Memory.Data.[1] <- byte AddressMode.RegisterIndirect ||| byte Register.R1
        cpu.Registers.R.[1] <- 123us
        Step cpu
        this.AssertCesarState [R1 123us; ProgramCounter 123us; MemoryReads 2]

    [<TestMethod>]
    member this.``Cesar: SOB subtracts one and branch``() =
        cpu.Memory.Data.[10] <- byte Instruction.Sob ||| byte Register.R2
        cpu.Memory.Data.[11] <- 2uy
        cpu.Registers.R.[2] <- 3us
        cpu.Registers.R.[7] <- 10us

        Step cpu
        this.AssertCesarState [R2 2us; ProgramCounter 10us; MemoryReads 2]
        Step cpu
        this.AssertCesarState [R2 1us; ProgramCounter 10us; MemoryReads 4]
        Step cpu
        this.AssertCesarState [R2 0us; ProgramCounter 12us; MemoryReads 6]

    [<TestMethod>]
    member this.``Cesar: JSR jumps to subroutine``() =
        cpu.Memory.Data.[0] <- byte Instruction.Jsr ||| byte Register.R3
        cpu.Memory.Data.[1] <- Indirect ||| byte Register.R1
        cpu.Registers.R.[1] <- 123us
        cpu.Registers.R.[3] <- 0x1234us
        Step cpu
        Assert.AreEqual(0x12uy, cpu.Memory.Data.[65534])
        Assert.AreEqual(0x34uy, cpu.Memory.Data.[65535])
        this.AssertCesarState [R1 123us; R3 2us; R6 65534us; ProgramCounter 123us; MemoryReads 2]
        
    [<TestMethod>]
    member this.``Cesar: RTS returns from subroutine``() =
        this.``Cesar: JSR jumps to subroutine``()
        cpu.Memory.Data.[123] <- byte Instruction.Rts ||| byte Register.R3
        Step cpu
        this.AssertCesarState [R1 123us; R3 0x1234us; R6 0us; ProgramCounter 2us; MemoryReads 5; MemoryWrites 2]

    [<TestMethod>]
    member this.``Cesar: CLR group instructions works as expected``() =
        let testValues = [| 1us; 127us; 128us; 129us; 255us; 256us; 257us; 32767us; 32768us; 32769us; 65534us; 65535us |]
        for value in testValues do
            cpu.Registers.R.[7] <- 0us

            this.TestClrGroupOperation(Instruction.Clr, value, 0us, [])
            this.TestClrGroupOperation(Instruction.Not, value, ~~~value, [FlagsCarry true])
            this.TestClrGroupOperation(Instruction.Inc, value, value + 1us, [FlagsOverflow (value = 32767us); FlagsCarry (value = 65535us)])
            this.TestClrGroupOperation(Instruction.Dec, value, value - 1us, [FlagsOverflow (value = 32768us); FlagsCarry (value = 0us)])
            this.TestClrGroupOperation(Instruction.Tst, value, value, [])
            this.TestClrGroupOperation(Instruction.Neg, value, uint16 (65536 - int value), [FlagsOverflow (value = 32768us); FlagsCarry (value <> 0us)])

            cpu.Registers.Flags.Carry <- false
            this.TestClrGroupOperation(Instruction.Ror, value, value >>> 1, [FlagsOverflow (value &&& 1us <> 0us); FlagsCarry (value &&& 1us <> 0us)])
            cpu.Registers.Flags.Carry <- true
            this.TestClrGroupOperation(Instruction.Ror, value, value >>> 1 ||| 0x8000us, [FlagsOverflow (value &&& 1us = 0us); FlagsCarry (value &&& 1us <> 0us)])

            // ToDo: ROL, ASR, ASL

            cpu.Registers.Flags.Carry <- false
            this.TestClrGroupOperation(Instruction.Adc, value, value, [])
            cpu.Registers.Flags.Carry <- true
            this.TestClrGroupOperation(Instruction.Adc, value, value + 1us, [FlagsOverflow (value = 32767us); FlagsCarry (value = 65535us)])

            cpu.Registers.Flags.Carry <- false
            this.TestClrGroupOperation(Instruction.Sbc, value, value, [])
            cpu.Registers.Flags.Carry <- true
            this.TestClrGroupOperation(Instruction.Sbc, value, value - 1us, [FlagsOverflow (value = 32768us); FlagsCarry (value <> 0us)])

    [<TestMethod>]
    member this.``Cesar: ADD adds source into target``() =
        this.TestMovGroupOperation(Instruction.Add, 12us, 23us, 12us + 23us, [FlagsNegative false; FlagsCarry false])
        Reset cpu
        this.TestMovGroupOperation(Instruction.Add, uint16(65536 - 12), uint16(65536 - 23), uint16(65536 - 12 - 23), [FlagsNegative true; FlagsCarry true])

    [<TestMethod>]
    member this.``Cesar: SUB subtracts source from target``() =
        this.TestMovGroupOperation(Instruction.Sub, 12us, 23us, 23us - 12us, [FlagsNegative false; FlagsCarry false (*!*)])
        Reset cpu
        this.TestMovGroupOperation(Instruction.Sub, 23us, 12us, uint16(65536 + 12 - 23), [FlagsNegative true; FlagsCarry true])

    [<TestMethod>]
    member this.``Cesar: CMP compares two operands``() =
        this.TestMovGroupOperation(Instruction.Sub, 12us, 23us, 23us - 12us, [FlagsNegative false; FlagsZero false; FlagsCarry false])
        Reset cpu
        this.TestMovGroupOperation(Instruction.Sub, 23us, 12us, uint16(65536 + 12 - 23), [FlagsNegative true; FlagsZero false; FlagsCarry true])

    [<TestMethod>]
    member this.``Cesar: AND works as expected``() =
        this.TestMovGroupOperation(Instruction.And, 12us, 234us, 12us &&& 234us, [FlagsNegative false; FlagsCarry false])

    [<TestMethod>]
    member this.``Cesar: OR works as expected``() =
        this.TestMovGroupOperation(Instruction.Or, 12us, 234us, 12us ||| 234us, [FlagsNegative false; FlagsCarry false])

    [<TestMethod>]
    member this.``Cesar: HLT sets Halted flag``() =
        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        Step cpu
        this.AssertCesarState [FlagsHalted false]
        Step cpu
        this.AssertCesarState [FlagsHalted true]
        Step cpu
        this.AssertCesarState [FlagsHalted false]
