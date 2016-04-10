namespace Ufrgs.Inf.ArchSims.Core.Tests.Cesar

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Cesar

open Ufrgs.Inf.ArchSims.Core.Tests.Utils

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
    | MemoryAt of int * byte
    | None

[<TestClass>]
type CesarTests() = 
    let mutable cpu = CreateCpu()

    let assertCesarState states =
        for state in states do
            match state with
            | R0 r0 -> cpu.Registers.R.[0] |>== r0
            | R1 r1 -> cpu.Registers.R.[1] |>== r1
            | R2 r2 -> cpu.Registers.R.[2] |>== r2
            | R3 r3 -> cpu.Registers.R.[3] |>== r3
            | R4 r4 -> cpu.Registers.R.[4] |>== r4
            | R5 r5 -> cpu.Registers.R.[5] |>== r5
            | R6 r6 -> cpu.Registers.R.[6] |>== r6
            | ProgramCounter pc -> cpu.Registers.R.[7] |>== pc
            | MemoryReads reads -> cpu.Memory.ReadCount |>== reads
            | MemoryWrites writes -> cpu.Memory.WriteCount |>== writes
            | FlagsHalted h -> cpu.Registers.Flags.Halted |>== h
            | FlagsNegative n -> cpu.Registers.Flags.Negative |>== n
            | FlagsZero z -> cpu.Registers.Flags.Zero |>== z
            | FlagsOverflow z -> cpu.Registers.Flags.Overflow |>== z
            | FlagsCarry c -> cpu.Registers.Flags.Carry |>== c
            | MemoryAt (addr, v) -> cpu.Memory.Data.[addr] |>== v
            | None -> ()

    let assertCpuStateIsClean() =
        assertCesarState [R0 0us; R1 0us; R2 0us; R3 0us; R4 0us; R5 0us; R6 0us; ProgramCounter 0us; 
            FlagsHalted false; FlagsNegative false; FlagsZero true; FlagsOverflow false; FlagsCarry false; 
            MemoryReads 0; MemoryWrites 0]
        cpu.Registers.InstructionRegister |>== { Data = [|0uy|]; SourceOperand = NoOp; TargetOperand = NoOp }
        cpu.Memory.Data |> Array.iter (equals 0uy) 

    let testAddressMode addressMode =
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
        assertCesarState expectedState 

    let testBranchOperation instruction branchExpected expectedState =
        let address = int cpu.Registers.R.[7]
        let memoryReads = cpu.Memory.ReadCount

        cpu.Memory.Data.[address] <- byte instruction
        cpu.Memory.Data.[address + 1] <- 3uy
        Step cpu

        let newAddress = address + (if branchExpected then 5 else 2)
        assertCesarState [ProgramCounter (uint16 newAddress); MemoryReads (memoryReads + 2)]
        assertCesarState expectedState

        cpu.Memory.Data.[newAddress] <- byte instruction
        cpu.Memory.Data.[newAddress + 1] <- 253uy
        Step cpu

        let finalAddress = newAddress + (if branchExpected then -1 else 2)
        assertCesarState [ProgramCounter (uint16 finalAddress); MemoryReads (memoryReads + 4)]
        assertCesarState expectedState

    let testClrGroupOperation instruction value expectedResult expectedState =
        let address = int cpu.Registers.R.[7]
        let memoryReads = cpu.Memory.ReadCount

        cpu.Memory.Data.[address] <- byte instruction
        cpu.Memory.Data.[address + 1] <- byte Register.R4
        cpu.Registers.R.[4] <- value
        Step cpu

        assertCesarState [R4 expectedResult; ProgramCounter (uint16 address + 2us); 
                               FlagsNegative (expectedResult > 0x7FFFus); FlagsZero (expectedResult = 0us);
                               MemoryReads (memoryReads + 2)]
        assertCesarState expectedState

    let testMovGroupOperation instruction sourceValue targetValue expectedResult expectedState =
        let encodedInstruction = EncodeInstructionTwoOperand instruction AddressMode.Register Register.R5 AddressMode.Register Register.R6
        cpu.Memory.Data.[0] <- byte (encodedInstruction >>> 8)
        cpu.Memory.Data.[1] <- byte encodedInstruction
        cpu.Registers.R.[5] <- sourceValue
        cpu.Registers.R.[6] <- targetValue
        Step cpu

        let expectedR6 = if instruction = Instruction.Cmp then targetValue else expectedResult

        assertCesarState [R5 sourceValue; R6 expectedR6; ProgramCounter 2us; MemoryReads 2]
        assertCesarState expectedState

    let testCmpOperation sourceValue targetValue =
        Reset cpu
        testMovGroupOperation Instruction.Cmp sourceValue targetValue 0us []

        let source = if sourceValue > 0x7FFFus then -(0x10000 - int sourceValue) else int sourceValue
        let target = if targetValue > 0x7FFFus then -(0x10000 - int targetValue) else int targetValue
        let expectedState = [R5 sourceValue; R6 targetValue]

        testBranchOperation Instruction.Bne (source <> target) expectedState
        testBranchOperation Instruction.Beq (source = target) expectedState
        testBranchOperation Instruction.Bge (source >= target) expectedState
        testBranchOperation Instruction.Blt (source <  target) expectedState
        testBranchOperation Instruction.Bgt (source >  target) expectedState
        testBranchOperation Instruction.Ble (source <= target) expectedState

    member this.Cpu
        with get () = cpu
        and set (value) = cpu <- value

    member this.AssertCesarState states = assertCesarState states
        
    [<TestInitialize>]
    member this.Setup() =
        cpu <- CreateCpu()
        
    [<TestMethod>]
    member this.``Cesar: New Cpu starts in clean state``() =
        assertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Cesar: Program Counter wraps at end of memory``() =
        cpu.Registers.R.[7] <- uint16 (cpu.Memory.Data.Length - 1)
        Step cpu
        assertCesarState [ProgramCounter 0us; MemoryReads 1]

    [<TestMethod>]
    member this.``Cesar: Reset() reverts to clean state``() =
        cpu.Registers.R.[0] <- 1us
        cpu.Registers.R.[1] <- 2us
        cpu.Registers.R.[2] <- 3us
        cpu.Registers.R.[7] <- 1us
        MemoryReadByte cpu.Memory 1 |> ignore
        byte Instruction.Hlt |> MemoryWriteByte cpu.Memory 1
        Step cpu
        assertCesarState [R0 1us; R1 2us; R2 3us; ProgramCounter 2us; MemoryReads 2; MemoryWrites 1]
        Reset cpu
        assertCpuStateIsClean()
        
    [<TestMethod>]
    member this.``Cesar: AddressModes works as expected``() =
        testAddressMode(AddressMode.Register)
        testAddressMode(AddressMode.RegPostInc)
        testAddressMode(AddressMode.RegPreDec)
        testAddressMode(AddressMode.Indexed)
        testAddressMode(AddressMode.RegisterIndirect)
        testAddressMode(AddressMode.RegPostIncIndirect)
        testAddressMode(AddressMode.RegPreDecIndirect)
        testAddressMode(AddressMode.IndexedIndirect)

    [<TestMethod>]
    member this.``Cesar: AddressModes with R7 works as expected``() =
        cpu.Memory.Data.[0] <- byte Instruction.Not
        cpu.Memory.Data.[1] <- byte Register.R7 ||| byte AddressMode.Register
        Step cpu
        assertCesarState [ProgramCounter 65533us; MemoryReads 2]

        Reset cpu
        cpu.Registers.R.[7] <- 2us
        cpu.Memory.Data.[2] <- byte Instruction.Not
        cpu.Memory.Data.[3] <- byte Register.R7 ||| byte AddressMode.RegPostInc
        Step cpu
        assertCesarState [ProgramCounter 6us; MemoryReads 4; MemoryWrites 2; MemoryAt (4, 255uy); MemoryAt (5, 255uy)]

        Reset cpu
        cpu.Registers.R.[7] <- 6us
        cpu.Memory.Data.[6] <- byte Instruction.Not
        cpu.Memory.Data.[7] <- byte Register.R7 ||| byte AddressMode.RegPreDec
        Step cpu
        assertCesarState [ProgramCounter 6us; MemoryReads 4; MemoryWrites 2; MemoryAt (6, 126uy); MemoryAt (7, 232uy)]

        Reset cpu
        cpu.Registers.R.[7] <- 8us
        cpu.Memory.Data.[8] <- byte Instruction.Not
        cpu.Memory.Data.[9] <- byte Register.R7 ||| byte AddressMode.Indexed
        cpu.Memory.Data.[10] <- 0uy
        cpu.Memory.Data.[11] <- 2uy
        Step cpu
        assertCesarState [ProgramCounter 12us; MemoryReads 6; MemoryWrites 2; MemoryAt (14, 255uy); MemoryAt (15, 255uy)]

        Reset cpu
        cpu.Registers.R.[7] <- 16us
        cpu.Memory.Data.[16] <- byte Instruction.Not
        cpu.Memory.Data.[17] <- byte Register.R7 ||| byte AddressMode.RegisterIndirect
        Step cpu
        assertCesarState [ProgramCounter 18us; MemoryReads 4; MemoryWrites 2; MemoryAt (18, 255uy); MemoryAt (19, 255uy)]

        Reset cpu
        cpu.Registers.R.[7] <- 20us
        cpu.Memory.Data.[20] <- byte Instruction.Not
        cpu.Memory.Data.[21] <- byte Register.R7 ||| byte AddressMode.RegPostIncIndirect
        Step cpu
        assertCesarState [ProgramCounter 24us; MemoryReads 6; MemoryWrites 2; MemoryAt (0, 255uy); MemoryAt (1, 255uy)]

        Reset cpu
        cpu.Registers.R.[7] <- 24us
        cpu.Memory.Data.[24] <- byte Instruction.Not
        cpu.Memory.Data.[25] <- byte Register.R7 ||| byte AddressMode.RegPreDecIndirect
        Step cpu
        assertCesarState [ProgramCounter 24us; MemoryReads 6; MemoryWrites 2; MemoryAt (33079, 255uy); MemoryAt (33080, 255uy)]

        Reset cpu
        cpu.Registers.R.[7] <- 26us
        cpu.Memory.Data.[26] <- byte Instruction.Not
        cpu.Memory.Data.[27] <- byte Register.R7 ||| byte AddressMode.IndexedIndirect
        cpu.Memory.Data.[28] <- 0uy
        cpu.Memory.Data.[29] <- 2uy
        Step cpu
        assertCesarState [ProgramCounter 30us; MemoryReads 8; MemoryWrites 2; MemoryAt (0, 255uy); MemoryAt (1, 255uy)]

    [<TestMethod>]
    member this.``Cesar: NOP does nothing``() =
        cpu.Memory.Data.[0] <- byte Instruction.Nop
        Step cpu
        assertCesarState [ProgramCounter 1us; MemoryReads 1]

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
            
            assertCesarState [ FlagsNegative (not mustClearNegative);
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
            
            assertCesarState [ FlagsNegative mustSetNegative;
                                    FlagsZero mustSetZero;
                                    FlagsOverflow mustSetOverflow;
                                    FlagsCarry mustSetCarry ]

    [<TestMethod>]
    member this.``Cesar: Branches instructions works as expected``() =
        testBranchOperation Instruction.Br true []

        for flags = 0 to 15 do
            cpu.Registers.R.[7] <- 0us
            cpu.Registers.Flags.Negative <- (flags &&& int Flag.Negative) <> 0
            cpu.Registers.Flags.Zero <- (flags &&& int Flag.Zero) <> 0
            cpu.Registers.Flags.Overflow <- (flags &&& int Flag.Overflow) <> 0
            cpu.Registers.Flags.Carry <- (flags &&& int Flag.Carry) <> 0

            testBranchOperation Instruction.Bne (not cpu.Registers.Flags.Zero) []
            testBranchOperation Instruction.Beq cpu.Registers.Flags.Zero []
            testBranchOperation Instruction.Bpl (not cpu.Registers.Flags.Negative) []
            testBranchOperation Instruction.Bmi cpu.Registers.Flags.Negative []
            testBranchOperation Instruction.Bvc (not cpu.Registers.Flags.Overflow) []
            testBranchOperation Instruction.Bvs cpu.Registers.Flags.Overflow []
            testBranchOperation Instruction.Bcc (not cpu.Registers.Flags.Carry) []
            testBranchOperation Instruction.Bcs cpu.Registers.Flags.Carry []
            testBranchOperation Instruction.Bge (cpu.Registers.Flags.Negative = cpu.Registers.Flags.Overflow) []
            testBranchOperation Instruction.Blt (cpu.Registers.Flags.Negative <> cpu.Registers.Flags.Overflow) []
            testBranchOperation Instruction.Bgt ((cpu.Registers.Flags.Negative = cpu.Registers.Flags.Overflow) && not cpu.Registers.Flags.Zero) []
            testBranchOperation Instruction.Ble ((cpu.Registers.Flags.Negative <> cpu.Registers.Flags.Overflow) || cpu.Registers.Flags.Zero) []
            testBranchOperation Instruction.Bhi (not cpu.Registers.Flags.Carry && not cpu.Registers.Flags.Zero) []
            testBranchOperation Instruction.Bls (cpu.Registers.Flags.Carry || cpu.Registers.Flags.Zero) []

    [<TestMethod>]
    member this.``Cesar: JMP changes Program Counter``() =
        cpu.Memory.Data.[0] <- byte Instruction.Jmp
        cpu.Memory.Data.[1] <- byte AddressMode.RegisterIndirect ||| byte Register.R1
        cpu.Registers.R.[1] <- 10us
        Step cpu
        assertCesarState [R1 10us; ProgramCounter 10us; MemoryReads 2]

        Reset cpu
        cpu.Memory.Data.[0] <- byte Instruction.Jmp
        cpu.Memory.Data.[1] <- byte AddressMode.RegPostIncIndirect ||| byte Register.R7
        cpu.Memory.Data.[2] <- 0uy
        cpu.Memory.Data.[3] <- 10uy
        Step cpu
        assertCesarState [ProgramCounter 10us; MemoryReads 4]

        Reset cpu
        cpu.Memory.Data.[0] <- byte Instruction.Jmp
        cpu.Memory.Data.[1] <- byte AddressMode.RegPostInc ||| byte Register.R7
        cpu.Memory.Data.[2] <- 0uy
        cpu.Memory.Data.[3] <- 10uy
        Step cpu
        assertCesarState [ProgramCounter 2us; MemoryReads 2]

    [<TestMethod>]
    member this.``Cesar: SOB subtracts one and branch``() =
        cpu.Memory.Data.[10] <- byte Instruction.Sob ||| byte Register.R2
        cpu.Memory.Data.[11] <- 2uy
        cpu.Registers.R.[2] <- 3us
        cpu.Registers.R.[7] <- 10us

        Step cpu
        assertCesarState [R2 2us; ProgramCounter 10us; MemoryReads 2]
        Step cpu
        assertCesarState [R2 1us; ProgramCounter 10us; MemoryReads 4]
        Step cpu
        assertCesarState [R2 0us; ProgramCounter 12us; MemoryReads 6]

    [<TestMethod>]
    member this.``Cesar: JSR jumps to subroutine``() =
        cpu.Memory.Data.[0] <- byte Instruction.Jsr ||| byte Register.R3
        cpu.Memory.Data.[1] <- Indirect ||| byte Register.R1
        cpu.Registers.R.[1] <- 123us
        cpu.Registers.R.[3] <- 0x1234us
        Step cpu
        assertCesarState [R1 123us; R3 2us; R6 65534us; ProgramCounter 123us; MemoryReads 2; MemoryAt (65534, 0x12uy); MemoryAt (65535, 0x34uy)]
        
    [<TestMethod>]
    member this.``Cesar: RTS returns from subroutine``() =
        this.``Cesar: JSR jumps to subroutine``()
        cpu.Memory.Data.[123] <- byte Instruction.Rts ||| byte Register.R3
        Step cpu
        assertCesarState [R1 123us; R3 0x1234us; R6 0us; ProgramCounter 2us; MemoryReads 5; MemoryWrites 2]

    [<TestMethod>]
    member this.``Cesar: CLR group instructions works as expected``() =
        let testValues = [| 1us; 127us; 128us; 129us; 255us; 256us; 257us; 32767us; 32768us; 32769us; 65534us; 65535us |]
        for value in testValues do
            cpu.Registers.R.[7] <- 0us

            testClrGroupOperation Instruction.Clr value 0us []
            testClrGroupOperation Instruction.Not value ~~~value [FlagsCarry true]
            testClrGroupOperation Instruction.Inc value (value + 1us) [FlagsOverflow (value = 32767us); FlagsCarry (value = 65535us)]
            testClrGroupOperation Instruction.Dec value (value - 1us) [FlagsOverflow (value = 32768us); FlagsCarry (value = 0us)]
            testClrGroupOperation Instruction.Tst value value []
            testClrGroupOperation Instruction.Neg value (uint16 (65536 - int value)) [FlagsOverflow (value = 32768us); FlagsCarry (value <> 0us)]

            cpu.Registers.Flags.Carry <- false
            testClrGroupOperation Instruction.Ror value (value >>> 1) [FlagsOverflow (value &&& 1us <> 0us); FlagsCarry (value &&& 1us <> 0us)]
            cpu.Registers.Flags.Carry <- true
            testClrGroupOperation Instruction.Ror value (value >>> 1 ||| 0x8000us) [FlagsOverflow (value &&& 1us = 0us); FlagsCarry (value &&& 1us <> 0us)]

            // ToDo: ROL, ASR, ASL

            cpu.Registers.Flags.Carry <- false
            testClrGroupOperation Instruction.Adc value value []
            cpu.Registers.Flags.Carry <- true
            testClrGroupOperation Instruction.Adc value (value + 1us) [FlagsOverflow (value = 32767us); FlagsCarry (value = 65535us)]

            cpu.Registers.Flags.Carry <- false
            testClrGroupOperation Instruction.Sbc value value []
            cpu.Registers.Flags.Carry <- true
            testClrGroupOperation Instruction.Sbc value (value - 1us) [FlagsOverflow (value = 32768us); FlagsCarry (value <> 0us)]

    [<TestMethod>]
    member this.``Cesar: ADD adds source into target``() =
        testMovGroupOperation Instruction.Add 12us 23us (12us + 23us) [FlagsNegative false; FlagsCarry false]
        Reset cpu
        testMovGroupOperation Instruction.Add (uint16(65536 - 12)) (uint16(65536 - 23)) (uint16(65536 - 12 - 23)) [FlagsNegative true; FlagsCarry true]

    [<TestMethod>]
    member this.``Cesar: SUB subtracts source from target``() =
        testMovGroupOperation Instruction.Sub 12us 23us (23us - 12us) [FlagsNegative false; FlagsCarry false]
        Reset cpu
        testMovGroupOperation Instruction.Sub 23us 12us (uint16(65536 + 12 - 23)) [FlagsNegative true; FlagsCarry true]

    [<TestMethod>]
    member this.``Cesar: CMP compares two operands``() =
        testMovGroupOperation Instruction.Sub 12us 23us (23us - 12us) [FlagsNegative false; FlagsZero false; FlagsCarry false]
        Reset cpu
        testMovGroupOperation Instruction.Sub 23us 12us (uint16(65536 + 12 - 23)) [FlagsNegative true; FlagsZero false; FlagsCarry true]

    [<TestMethod>]
    member this.``Cesar: AND works as expected``() =
        testMovGroupOperation Instruction.And 12us 234us (12us &&& 234us) [FlagsNegative false; FlagsCarry false]

    [<TestMethod>]
    member this.``Cesar: OR works as expected``() =
        testMovGroupOperation Instruction.Or 12us 234us (12us ||| 234us) [FlagsNegative false; FlagsCarry false]

    [<TestMethod>]
    member this.``Cesar: HLT sets Halted flag``() =
        cpu.Memory.Data.[1] <- byte Instruction.Hlt
        Step cpu
        assertCesarState [FlagsHalted false]
        Step cpu
        assertCesarState [FlagsHalted true]
        Step cpu
        assertCesarState [FlagsHalted false]

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
        assertCesarState [R0 12296us]

        cpu.Memory.Data.[14] <- 155uy  // MOV 65534, R1
        cpu.Memory.Data.[15] <- 193uy
        cpu.Memory.Data.[16] <- 255uy
        cpu.Memory.Data.[17] <- 254uy
        Step cpu
        assertCesarState [R1 0us]

        cpu.Memory.Data.[18] <- 155uy  // MOV 65535, R2
        cpu.Memory.Data.[19] <- 194uy
        cpu.Memory.Data.[20] <- 255uy
        cpu.Memory.Data.[21] <- 255uy
        Step cpu
        assertCesarState [R2 100us]

        cpu.Memory.Data.[22] <- 155uy  // MOV 65500, R3
        cpu.Memory.Data.[23] <- 195uy
        cpu.Memory.Data.[24] <- 255uy
        cpu.Memory.Data.[25] <- 220uy
        Step cpu
        assertCesarState [R3 234us]

        cpu.Memory.Data.[26] <- 155uy  // MOV 65499, R4
        cpu.Memory.Data.[27] <- 196uy
        cpu.Memory.Data.[28] <- 255uy
        cpu.Memory.Data.[29] <- 219uy
        Step cpu
        assertCesarState [R4 123us]         // ??

    [<TestMethod>]
    member this.``Cesar: DisassembleInstruction works as expected``() =
        DisassembleInstruction [byte Instruction.Nop] |>== "NOP"
        DisassembleInstruction [byte Instruction.Nop + 5uy] |>== "NOP"
        DisassembleInstruction [byte Instruction.Hlt] |>== "HLT"

        DisassembleInstruction [byte Instruction.Ccc] |>== "CCC"
        DisassembleInstruction [byte Instruction.Ccc ||| byte Flag.Negative ||| byte Flag.Zero ||| byte Flag.Overflow ||| byte Flag.Carry] |>== "CCC NZVC"
        DisassembleInstruction [byte Instruction.Scc] |>== "SCC"
        DisassembleInstruction [byte Instruction.Scc ||| byte Flag.Zero ||| byte Flag.Carry] |>== "SCC ZC"

        DisassembleInstruction [byte Instruction.Br; 0uy] |>== "BR  0"
        DisassembleInstruction [byte Instruction.Bne; 10uy] |>== "BNE 10"
        DisassembleInstruction [byte Instruction.Bne; 246uy] |>== "BNE 246"

        DisassembleInstruction [byte Instruction.Jmp; byte Register.R7] |>== "JMP ?" // Invalid!
        DisassembleInstruction [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostInc] |>== "JMP (R7)+"
        DisassembleInstruction [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect] |>== "JMP ((R7)+)"

        DisassembleInstruction [byte Instruction.Sob ||| byte Register.R1; 123uy] |>== "SOB R1, 123"
        DisassembleInstruction [byte Instruction.Sob ||| byte Register.R2; 234uy] |>== "SOB R2, 234"

        DisassembleInstruction [byte Instruction.Jsr ||| byte Register.R1; byte Register.R7] |>== "JSR R1, ?" // Invalid!
        DisassembleInstruction [byte Instruction.Jsr ||| byte Register.R2; byte Register.R7 ||| byte AddressMode.RegPostInc] |>== "JSR R2, (R7)+"
        DisassembleInstruction [byte Instruction.Jsr ||| byte Register.R3; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect] |>== "JSR R3, ((R7)+)"

        DisassembleInstruction [byte Instruction.Rts ||| byte Register.R4] |>== "RTS R4"

        DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.Register] |>== "NOT R7"
        DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostInc] |>== "NOT (R7)+"
        DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPreDec] |>== "NOT -(R7)"
        DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.Indexed; 0uy; 2uy] |>== "NOT 2(R7)"
        DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegisterIndirect] |>== "NOT (R7)"
        DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect] |>== "NOT ((R7)+)"
        DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPreDecIndirect] |>== "NOT (-(R7))"
        DisassembleInstruction [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.IndexedIndirect; 0uy; 2uy] |>== "NOT (2(R7))"

        let g = EncodeInstructionTwoOperand Instruction.Mov AddressMode.Register Register.R1 AddressMode.RegPostInc Register.R2
        DisassembleInstruction [byte (g >>> 8); byte g] |>== "MOV R1, (R2)+"

        let h = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegisterIndirect Register.R1 AddressMode.RegPreDec Register.R2
        DisassembleInstruction [byte (h >>> 8); byte h] |>== "MOV (R1), -(R2)"

        let i = EncodeInstructionTwoOperand Instruction.Mov AddressMode.Indexed Register.R1 AddressMode.RegPostIncIndirect Register.R2
        DisassembleInstruction [byte (i >>> 8); byte i; 0uy; 10uy] |>== "MOV 10(R1), ((R2)+)"

        let j = EncodeInstructionTwoOperand Instruction.Mov AddressMode.IndexedIndirect Register.R1 AddressMode.IndexedIndirect Register.R2
        DisassembleInstruction [byte (j >>> 8); byte j; 0uy; 10uy; 0uy; 20uy] |>== "MOV (10(R1)), (20(R2))"

        let k = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegPreDecIndirect Register.R1 AddressMode.IndexedIndirect Register.R2
        DisassembleInstruction [byte (k >>> 8); byte k; 0uy; 20uy] |>== "MOV (-(R1)), (20(R2))"

