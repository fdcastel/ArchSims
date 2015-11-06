namespace Ufrgs.Inf.ArchSims.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Cesar
open Ufrgs.Inf.ArchSims.Assemblers.CesarAssembler

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

        let newAddress = address + (if branchExpected then 5 else 2)
        this.AssertCesarState [ProgramCounter (uint16 newAddress); MemoryReads (memoryReads + 2)]
        this.AssertCesarState expectedState

        cpu.Memory.Data.[newAddress] <- byte instruction
        cpu.Memory.Data.[newAddress + 1] <- 253uy
        Step cpu

        let finalAddress = newAddress + (if branchExpected then -1 else 2)
        this.AssertCesarState [ProgramCounter (uint16 finalAddress); MemoryReads (memoryReads + 4)]
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
    member this.``Cesar: AddressModes with R7 works as expected``() =
        cpu.Memory.Data.[0] <- byte Instruction.Not
        cpu.Memory.Data.[1] <- byte Register.R7 ||| byte AddressMode.Register
        Step cpu
        this.AssertCesarState [ProgramCounter 65533us; MemoryReads 2]

        Reset cpu
        cpu.Registers.R.[7] <- 2us
        cpu.Memory.Data.[2] <- byte Instruction.Not
        cpu.Memory.Data.[3] <- byte Register.R7 ||| byte AddressMode.RegPostInc
        Step cpu
        this.AssertCesarState [ProgramCounter 6us; MemoryReads 4; MemoryWrites 2]
        Assert.AreEqual(255uy, cpu.Memory.Data.[4])
        Assert.AreEqual(255uy, cpu.Memory.Data.[5])

        Reset cpu
        cpu.Registers.R.[7] <- 6us
        cpu.Memory.Data.[6] <- byte Instruction.Not
        cpu.Memory.Data.[7] <- byte Register.R7 ||| byte AddressMode.RegPreDec
        Step cpu
        this.AssertCesarState [ProgramCounter 6us; MemoryReads 4; MemoryWrites 2]
        Assert.AreEqual(126uy, cpu.Memory.Data.[6])
        Assert.AreEqual(232uy, cpu.Memory.Data.[7])

        Reset cpu
        cpu.Registers.R.[7] <- 8us
        cpu.Memory.Data.[8] <- byte Instruction.Not
        cpu.Memory.Data.[9] <- byte Register.R7 ||| byte AddressMode.Indexed
        cpu.Memory.Data.[10] <- 0uy
        cpu.Memory.Data.[11] <- 2uy
        Step cpu
        this.AssertCesarState [ProgramCounter 12us; MemoryReads 6; MemoryWrites 2]
        Assert.AreEqual(255uy, cpu.Memory.Data.[14])
        Assert.AreEqual(255uy, cpu.Memory.Data.[15])

        Reset cpu
        cpu.Registers.R.[7] <- 16us
        cpu.Memory.Data.[16] <- byte Instruction.Not
        cpu.Memory.Data.[17] <- byte Register.R7 ||| byte AddressMode.RegisterIndirect
        Step cpu
        this.AssertCesarState [ProgramCounter 18us; MemoryReads 4; MemoryWrites 2]
        Assert.AreEqual(255uy, cpu.Memory.Data.[18])
        Assert.AreEqual(255uy, cpu.Memory.Data.[19])

        Reset cpu
        cpu.Registers.R.[7] <- 20us
        cpu.Memory.Data.[20] <- byte Instruction.Not
        cpu.Memory.Data.[21] <- byte Register.R7 ||| byte AddressMode.RegPostIncIndirect
        Step cpu
        this.AssertCesarState [ProgramCounter 24us; MemoryReads 6; MemoryWrites 2]
        Assert.AreEqual(255uy, cpu.Memory.Data.[0])
        Assert.AreEqual(255uy, cpu.Memory.Data.[0])

        Reset cpu
        cpu.Registers.R.[7] <- 24us
        cpu.Memory.Data.[24] <- byte Instruction.Not
        cpu.Memory.Data.[25] <- byte Register.R7 ||| byte AddressMode.RegPreDecIndirect
        Step cpu
        this.AssertCesarState [ProgramCounter 24us; MemoryReads 6; MemoryWrites 2]
        Assert.AreEqual(255uy, cpu.Memory.Data.[33079])
        Assert.AreEqual(255uy, cpu.Memory.Data.[33080])

        Reset cpu
        cpu.Registers.R.[7] <- 26us
        cpu.Memory.Data.[26] <- byte Instruction.Not
        cpu.Memory.Data.[27] <- byte Register.R7 ||| byte AddressMode.IndexedIndirect
        cpu.Memory.Data.[28] <- 0uy
        cpu.Memory.Data.[29] <- 2uy
        Step cpu
        this.AssertCesarState [ProgramCounter 30us; MemoryReads 8; MemoryWrites 2]
        Assert.AreEqual(255uy, cpu.Memory.Data.[0])
        Assert.AreEqual(255uy, cpu.Memory.Data.[1])

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
        cpu.Registers.R.[1] <- 10us
        Step cpu
        this.AssertCesarState [R1 10us; ProgramCounter 10us; MemoryReads 2]

        Reset cpu
        cpu.Memory.Data.[0] <- byte Instruction.Jmp
        cpu.Memory.Data.[1] <- byte AddressMode.RegPostIncIndirect ||| byte Register.R7
        cpu.Memory.Data.[2] <- 0uy
        cpu.Memory.Data.[3] <- 10uy
        Step cpu
        this.AssertCesarState [ProgramCounter 10us; MemoryReads 4]

        Reset cpu
        cpu.Memory.Data.[0] <- byte Instruction.Jmp
        cpu.Memory.Data.[1] <- byte AddressMode.RegPostInc ||| byte Register.R7
        cpu.Memory.Data.[2] <- 0uy
        cpu.Memory.Data.[3] <- 10uy
        Step cpu
        this.AssertCesarState [ProgramCounter 2us; MemoryReads 2]

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
        this.TestMovGroupOperation(Instruction.Sub, 12us, 23us, 23us - 12us, [FlagsNegative false; FlagsCarry false])
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

    [<TestMethod>]
    member this.``Cesar: Display memory area addresses works at byte level``() =
        // Reads from memory area
        cpu.Memory.Data.[0] <- 48uy    // BR 8 
        cpu.Memory.Data.[1] <- 8uy
        cpu.Memory.Data.[65499] <- 123uy
        cpu.Memory.Data.[65500] <- 234uy
        cpu.Memory.Data.[65535] <- 100uy
        Step cpu

        cpu.Memory.Data.[10] <- 155uy  // MOV 0, R0
        cpu.Memory.Data.[11] <- 192uy
        cpu.Memory.Data.[12] <- 0uy
        cpu.Memory.Data.[13] <- 0uy
        Step cpu
        this.AssertCesarState [R0 12296us]

        cpu.Memory.Data.[14] <- 155uy  // MOV 65534, R1
        cpu.Memory.Data.[15] <- 193uy
        cpu.Memory.Data.[16] <- 255uy
        cpu.Memory.Data.[17] <- 254uy
        Step cpu
        this.AssertCesarState [R1 0us]

        cpu.Memory.Data.[18] <- 155uy  // MOV 65535, R2
        cpu.Memory.Data.[19] <- 194uy
        cpu.Memory.Data.[20] <- 255uy
        cpu.Memory.Data.[21] <- 255uy
        Step cpu
        this.AssertCesarState [R2 100us]

        cpu.Memory.Data.[22] <- 155uy  // MOV 65500, R3
        cpu.Memory.Data.[23] <- 195uy
        cpu.Memory.Data.[24] <- 255uy
        cpu.Memory.Data.[25] <- 220uy
        Step cpu
        this.AssertCesarState [R3 234us]

        cpu.Memory.Data.[26] <- 155uy  // MOV 65499, R4
        cpu.Memory.Data.[27] <- 196uy
        cpu.Memory.Data.[28] <- 255uy
        cpu.Memory.Data.[29] <- 219uy
        Step cpu
        this.AssertCesarState [R4 123us]         // ??

    [<TestMethod>]
    member this.``Cesar: AssembleInstruction works as expected``() =
        Assert.AreEqual([byte Instruction.Nop], AssembleInstruction "NOP")
        Assert.AreEqual([byte Instruction.Hlt], AssembleInstruction "HLT")

        Assert.AreEqual([byte Instruction.Ccc], AssembleInstruction "CCC")
        Assert.AreEqual([byte Instruction.Ccc ||| byte Flag.Negative ||| byte Flag.Zero ||| byte Flag.Overflow ||| byte Flag.Carry], AssembleInstruction "CCC NZVC")
        Assert.AreEqual([byte Instruction.Scc], AssembleInstruction "SCC")
        Assert.AreEqual([byte Instruction.Scc ||| byte Flag.Zero ||| byte Flag.Carry], AssembleInstruction "SCC ZC")

        Assert.AreEqual([byte Instruction.Br; 0uy], AssembleInstruction "BR 0")
        Assert.AreEqual([byte Instruction.Bne; 10uy], AssembleInstruction "BNE 10")
        Assert.AreEqual([byte Instruction.Bne; 246uy], AssembleInstruction "BNE -10")
        Assert.AreEqual([byte Instruction.Bne; 246uy], AssembleInstruction "BNE 246")

        Assert.AreEqual([byte Instruction.Jmp; byte Register.R7], AssembleInstruction "JMP R7") // Invalid!
        Assert.AreEqual([byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostInc], AssembleInstruction "JMP (R7)+")
        Assert.AreEqual([byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect], AssembleInstruction "JMP ((R7)+)")

        Assert.AreEqual([byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostInc; 0uy; 10uy], AssembleInstruction "JMP #10") // Valid, but not useful
        Assert.AreEqual([byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect; 0uy; 10uy], AssembleInstruction "JMP 10")
        Assert.AreEqual([byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostInc; 255uy; 246uy], AssembleInstruction "JMP #-10") // Valid, but not useful
        Assert.AreEqual([byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect; 255uy; 246uy], AssembleInstruction "JMP -10")

        Assert.AreEqual([byte Instruction.Sob ||| byte Register.R1; 123uy], AssembleInstruction "SOB R1, 123")
        Assert.AreEqual([byte Instruction.Sob ||| byte Register.R2; 234uy], AssembleInstruction "SOB R2, 234")

        Assert.AreEqual([byte Instruction.Jsr ||| byte Register.R1; byte Register.R7], AssembleInstruction "JSR R1, R7") // Invalid!
        Assert.AreEqual([byte Instruction.Jsr ||| byte Register.R2; byte Register.R7 ||| byte AddressMode.RegPostInc], AssembleInstruction "JSR R2, (R7)+")
        Assert.AreEqual([byte Instruction.Jsr ||| byte Register.R3; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect], AssembleInstruction "JSR R3, ((R7)+)")

        Assert.AreEqual([byte Instruction.Rts ||| byte Register.R4], AssembleInstruction "RTS R4")

        Assert.AreEqual([byte Instruction.Not; byte Register.R7 ||| byte AddressMode.Register], AssembleInstruction "NOT R7")
        Assert.AreEqual([byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostInc], AssembleInstruction "NOT (R7)+")
        Assert.AreEqual([byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPreDec], AssembleInstruction "NOT -(R7)")
        Assert.AreEqual([byte Instruction.Not; byte Register.R7 ||| byte AddressMode.Indexed; 0uy; 2uy], AssembleInstruction "NOT 2(R7)")
        Assert.AreEqual([byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegisterIndirect], AssembleInstruction "NOT (R7)")
        Assert.AreEqual([byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect], AssembleInstruction "NOT ((R7)+)")
        Assert.AreEqual([byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPreDecIndirect], AssembleInstruction "NOT (-(R7))")
        Assert.AreEqual([byte Instruction.Not; byte Register.R7 ||| byte AddressMode.IndexedIndirect; 0uy; 2uy], AssembleInstruction "NOT (2(R7))")

        let g = EncodeInstructionTwoOperand Instruction.Mov AddressMode.Register Register.R1 AddressMode.RegPostInc Register.R2
        Assert.AreEqual([byte (g >>> 8); byte g], AssembleInstruction "MOV R1, (R2)+")

        let h = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegisterIndirect Register.R1 AddressMode.RegPreDec Register.R2
        Assert.AreEqual([byte (h >>> 8); byte h], AssembleInstruction "MOV (R1), -(R2)")

        let i = EncodeInstructionTwoOperand Instruction.Mov AddressMode.Indexed Register.R1 AddressMode.RegPostIncIndirect Register.R2
        Assert.AreEqual([byte (i >>> 8); byte i; 0uy; 10uy], AssembleInstruction "MOV 10(R1), ((R2)+)")

        let j = EncodeInstructionTwoOperand Instruction.Mov AddressMode.IndexedIndirect Register.R1 AddressMode.IndexedIndirect Register.R2
        Assert.AreEqual([byte (j >>> 8); byte j; 0uy; 10uy; 0uy; 20uy], AssembleInstruction "MOV (10(R1)), (20(R2))")

        let k = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegPreDecIndirect Register.R1 AddressMode.IndexedIndirect Register.R2
        Assert.AreEqual([byte (k >>> 8); byte k; 0uy; 20uy], AssembleInstruction "MOV (-(R1)), (20(R2))")

        Assert.AreEqual([147uy; 193uy; 128uy; 0uy], AssembleInstruction "MOV #32768, R1")
        Assert.AreEqual([147uy; 193uy; 128uy; 0uy], AssembleInstruction "MOV #-32768, R1")
        Assert.AreEqual([147uy; 193uy; 255uy; 255uy], AssembleInstruction "MOV #65535, R1")
        Assert.AreEqual([147uy; 193uy; 255uy; 255uy], AssembleInstruction "MOV #-1, R1")

    [<TestMethod>]
    member this.``Cesar: DisassembleInstruction works as expected``() =
        Assert.AreEqual("NOP", DisassembleInstruction [byte Instruction.Nop])
        Assert.AreEqual("NOP", DisassembleInstruction [byte Instruction.Nop + 5uy])
        Assert.AreEqual("HLT", DisassembleInstruction [byte Instruction.Hlt])

        Assert.AreEqual("CCC", DisassembleInstruction [byte Instruction.Ccc])
        Assert.AreEqual("CCC NZVC", DisassembleInstruction [byte Instruction.Ccc ||| byte Flag.Negative ||| byte Flag.Zero ||| byte Flag.Overflow ||| byte Flag.Carry])
        Assert.AreEqual("SCC", DisassembleInstruction [byte Instruction.Scc])
        Assert.AreEqual("SCC ZC", DisassembleInstruction [byte Instruction.Scc ||| byte Flag.Zero ||| byte Flag.Carry])

        Assert.AreEqual("BR  0", DisassembleInstruction [byte Instruction.Br; 0uy])
        Assert.AreEqual("BNE 10", DisassembleInstruction [byte Instruction.Bne; 10uy])
        Assert.AreEqual("BNE 246", DisassembleInstruction [byte Instruction.Bne; 246uy])

        Assert.AreEqual("JMP ?", DisassembleInstruction [byte Instruction.Jmp; byte Register.R7]) // Invalid!
        Assert.AreEqual("JMP (R7)+", DisassembleInstruction [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostInc])
        Assert.AreEqual("JMP ((R7)+)", DisassembleInstruction [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect])

        Assert.AreEqual("SOB R1, 123", DisassembleInstruction [byte Instruction.Sob ||| byte Register.R1; 123uy])
        Assert.AreEqual("SOB R2, 234", DisassembleInstruction [byte Instruction.Sob ||| byte Register.R2; 234uy])

        Assert.AreEqual("JSR R1, ?", DisassembleInstruction [byte Instruction.Jsr ||| byte Register.R1; byte Register.R7]) // Invalid!
        Assert.AreEqual("JSR R2, (R7)+", DisassembleInstruction [byte Instruction.Jsr ||| byte Register.R2; byte Register.R7 ||| byte AddressMode.RegPostInc])
        Assert.AreEqual("JSR R3, ((R7)+)", DisassembleInstruction [byte Instruction.Jsr ||| byte Register.R3; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect])

        Assert.AreEqual("RTS R4", DisassembleInstruction [byte Instruction.Rts ||| byte Register.R4])

        Assert.AreEqual("NOT R7", DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.Register])
        Assert.AreEqual("NOT (R7)+", DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostInc])
        Assert.AreEqual("NOT -(R7)", DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPreDec])
        Assert.AreEqual("NOT 2(R7)", DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.Indexed; 0uy; 2uy])
        Assert.AreEqual("NOT (R7)", DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegisterIndirect])
        Assert.AreEqual("NOT ((R7)+)", DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect])
        Assert.AreEqual("NOT (-(R7))", DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPreDecIndirect])
        Assert.AreEqual("NOT (2(R7))", DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.IndexedIndirect; 0uy; 2uy])

        let g = EncodeInstructionTwoOperand Instruction.Mov AddressMode.Register Register.R1 AddressMode.RegPostInc Register.R2
        Assert.AreEqual("MOV R1, (R2)+", DisassembleInstruction [byte (g >>> 8); byte g])

        let h = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegisterIndirect Register.R1 AddressMode.RegPreDec Register.R2
        Assert.AreEqual("MOV (R1), -(R2)", DisassembleInstruction [byte (h >>> 8); byte h])

        let i = EncodeInstructionTwoOperand Instruction.Mov AddressMode.Indexed Register.R1 AddressMode.RegPostIncIndirect Register.R2
        Assert.AreEqual("MOV 10(R1), ((R2)+)", DisassembleInstruction [byte (i >>> 8); byte i; 0uy; 10uy])

        let j = EncodeInstructionTwoOperand Instruction.Mov AddressMode.IndexedIndirect Register.R1 AddressMode.IndexedIndirect Register.R2
        Assert.AreEqual("MOV (10(R1)), (20(R2))", DisassembleInstruction [byte (j >>> 8); byte j; 0uy; 10uy; 0uy; 20uy])

        let k = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegPreDecIndirect Register.R1 AddressMode.IndexedIndirect Register.R2
        Assert.AreEqual("MOV (-(R1)), (20(R2))", DisassembleInstruction [byte (k >>> 8); byte k; 0uy; 20uy])

    [<TestMethod>]
    member this.``Cesar: AssembleProgram works as expected``() =
        let program = """
            MOV #10, R1
            MOV 1000, R2
            MOV :L1, R3
            MOV :L2, R4
            MOV :L2, R5
        :L1
            HLT              ; End of program
            NOP
            NOP
            BR :L1           ; Test branch/jump backwards
            SOB R1, :L1
            JMP :L1
            BR :L3           ; Test branch/jump forward
            SOB R3, :L3
            JMP :L3
            NOP
            NOP
        :L3
            JMP :L2
        @1000
            0                ; R2 value
            123
        :L2
            1234             ; R4,R5 value
        """

        let expectedProgram = [|147uy;193uy;0uy;10uy;          // MOV #10, R1
                                155uy;194uy;3uy;232uy;         // MOV 1000, R2
                                155uy;195uy;0uy;20uy;          // MOV :L1, R3     (:L1 = 20)
                                155uy;196uy;3uy;234uy;         // MOV :L2, R4     (:L2 = 1002)
                                155uy;197uy;3uy;234uy;         // MOV :L2, R5
                                240uy;                         // HLT
                                0uy;                           // NOP
                                0uy;                           // NOP
                                48uy;251uy;                    // BR :L1          (-5)
                                81uy;7uy;                      // SOB R1, :L1     (7)
                                64uy;47uy;0uy;20uy;            // JMP :L1
                                48uy;8uy;                      // BR :L3          (8)
                                83uy;250uy;                    // SOB R3, :L3     (-6)
                                64uy;47uy;0uy;41uy;            // JMP :L3         (:L3 = 41)
                                0uy;                           // NOP
                                0uy;                           // NOP
                                64uy;47uy;3uy;234uy;|]         // JMP :L2

        let expectedData = [|0uy;123uy;4uy;210uy|]

        AssembleProgram cpu program
        let programArea = Array.sub cpu.Memory.Data 0 45
        Assert.AreEqual(0, Array.compareWith (fun a b -> if a = b then 0 else 1) expectedProgram programArea)

        let dataArea = Array.sub cpu.Memory.Data 1000 4
        Assert.AreEqual(0, Array.compareWith (fun a b -> if a = b then 0 else 1) expectedData dataArea)

        Step cpu
        Step cpu
        Step cpu
        Step cpu
        Step cpu
        Step cpu
        this.AssertCesarState [R1 10us; R2 123us; R3 61440us; R4 1234us; R5 1234us; ProgramCounter 21us; FlagsHalted true]

    [<TestMethod>]
    [<ExpectedException(typeof<System.Exception>, "Label indefinido: L1")>]
    member this.``Cesar: AssembleProgram fails with undeclared label``() =
        AssembleProgram cpu "JMP :L1"

    [<TestMethod>]
    [<ExpectedException(typeof<System.Exception>, "Label inacessível a partir de um branch: L1")>]
    member this.``Cesar: AssembleProgram fails with far branches``() =
        AssembleProgram cpu """
            BR :L1
        @1000
        :L1
            HLT
        """
