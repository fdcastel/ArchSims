namespace Ufrgs.Inf.ArchSims.Core

open Ufrgs.Inf.ArchSims.Core.Memory

module Cesar =

    type Instruction = 
    | Nop = 0x00uy // 0000 0000
    | Ccc = 0x10uy // 0001 0000
    | Scc = 0x20uy // 0010 0000

    | Br  = 0x30uy // 0011 0000
    | Bne = 0x31uy // 0011 0001
    | Beq = 0x32uy // 0011 0010
    | Bpl = 0x33uy // 0011 0011
    | Bmi = 0x34uy // 0011 0100
    | Bvc = 0x35uy // 0011 0101
    | Bvs = 0x36uy // 0011 0110
    | Bcc = 0x37uy // 0011 0111
    | Bcs = 0x38uy // 0011 1000
    | Bge = 0x39uy // 0011 1001
    | Blt = 0x3Auy // 0011 1010
    | Bgt = 0x3Buy // 0011 1011
    | Ble = 0x3Cuy // 0011 1100
    | Bhi = 0x3Duy // 0011 1101
    | Bls = 0x3Euy // 0011 1110

    | Jmp = 0x40uy // 0100 0000
    | Sob = 0x50uy // 0101 0000
    | Jsr = 0x60uy // 0110 0000
    | Rts = 0x70uy // 0111 0000

    | Clr = 0x80uy // 1000 0000
    | Not = 0x81uy // 1000 0001
    | Inc = 0x82uy // 1000 0010
    | Dec = 0x83uy // 1000 0011
    | Neg = 0x84uy // 1000 0100
    | Tst = 0x85uy // 1000 0101
    | Ror = 0x86uy // 1000 0110
    | Rol = 0x87uy // 1000 0111
    | Asr = 0x88uy // 1000 1000
    | Asl = 0x89uy // 1000 1001
    | Adc = 0x8Auy // 1000 1010
    | Sbc = 0x8Buy // 1000 1011

    | Mov = 0x90uy // 1001 0000
    | Add = 0xA0uy // 1010 0000
    | Sub = 0xB0uy // 1011 0000
    | Cmp = 0xC0uy // 1100 0000
    | And = 0xD0uy // 1101 0000
    | Or  = 0xE0uy // 1110 0000

    | Hlt = 0xF0uy // 1111 0000

    type Register =
    | R0 = 0x00uy // 0000 0000
    | R1 = 0x01uy // 0000 0001
    | R2 = 0x02uy // 0000 0010
    | R3 = 0x03uy // 0000 0011
    | R4 = 0x04uy // 0000 0100
    | R5 = 0x05uy // 0000 0101
    | R6 = 0x06uy // 0000 0110
    | R7 = 0x07uy // 0000 0111

    type AddressMode =
    | Register           = 0b00000000uy
    | RegPostInc         = 0b00001000uy
    | RegPreDec          = 0b00010000uy
    | Indexed            = 0b00011000uy
    | RegisterIndirect   = 0b00100000uy
    | RegPostIncIndirect = 0b00101000uy
    | RegPreDecIndirect  = 0b00110000uy
    | IndexedIndirect    = 0b00111000uy

    let Indirect  = byte AddressMode.RegisterIndirect
    let Immediate = byte AddressMode.RegPostInc ||| byte Register.R7
    let Direct    = Immediate ||| Indirect
        
    let InstructionMask    = 0b11110000uy
    let SubInstructionMask = 0b00001111uy

    let AddressModeMask    = 0b00111000uy
    let RegisterMask       = 0b00000111uy

    let DisplayMemoryAddress = 0xFFDAus // Start of display memory mapped area 

    type Flag =
    | Negative = 0b00001000uy
    | Zero     = 0b00000100uy
    | Overflow = 0b00000010uy
    | Carry    = 0b00000001uy

    type Flags = {
        mutable Halted: bool
        mutable Negative: bool
        mutable Zero: bool
        mutable Overflow: bool
        mutable Carry: bool
    }

    type Operand =
    | NoOp
    | Reg of Register
    | Addr of uint16

    type InstructionRegister = {
        mutable Data: byte array
        mutable SourceOperand: Operand
        mutable TargetOperand: Operand
    }

    type Registers = {
        R: uint16 array
        InstructionRegister: InstructionRegister
        Flags: Flags
    }

    let CreateRegisters() = {
        R = [| 0us; 0us; 0us; 0us; 0us; 0us; 0us; 0us |]
        InstructionRegister = { Data = [||]; SourceOperand = NoOp; TargetOperand = NoOp }
        Flags = { Halted = false; Negative = false; Zero = true; Overflow = false; Carry = false }
    }

    let RegistersReset registers = 
        Array.fill registers.R 0 registers.R.Length 0us
        registers.InstructionRegister.Data <- [||]
        registers.InstructionRegister.SourceOperand <- NoOp
        registers.InstructionRegister.TargetOperand <- NoOp
        registers.Flags.Halted <- false
        registers.Flags.Negative <- false
        registers.Flags.Zero <- true
        registers.Flags.Overflow <- false
        registers.Flags.Carry <- false    
    
    type Cpu = {
        Registers: Registers
        Memory: Memory
    }

    let CreateCpu() = {
        Registers = CreateRegisters()
        Memory = CreateMemory 65536
    }

    let EncodeInstructionOneOperand (instruction:Instruction) (targetMode:AddressMode) (targetRegister:Register) =
        (uint16 instruction <<< 8) ||| uint16 (byte targetMode &&& AddressModeMask) ||| uint16 (byte targetRegister &&& RegisterMask)
            
    let EncodeInstructionTwoOperand (instruction:Instruction) (sourceMode:AddressMode) (sourceRegister:Register) (targetMode:AddressMode) (targetRegister:Register) =
        // 1OP: iiii iiii 00mm mrrr
        // 2OP: iiii MMMR RRmm mrrr

        //   i: instruction
        // m/r: mode/register of target operand
        // M/R: mode/register of source operand
        match instruction with
        | Instruction.Mov
        | Instruction.Add
        | Instruction.Sub
        | Instruction.Cmp
        | Instruction.And
        | Instruction.Or ->
            let firstPart = EncodeInstructionOneOperand instruction targetMode targetRegister
            let result = firstPart ||| (uint16 (byte sourceMode &&& AddressModeMask) <<< 6) ||| (uint16 (byte sourceRegister &&& RegisterMask) <<< 6)
            result
        | _ -> 
            failwith "This function can encode only instructions with two operands."

    let Fetch cpu = 
        let appendByteToInstructionRegister value =
            cpu.Registers.InstructionRegister.Data <- Array.append cpu.Registers.InstructionRegister.Data [|value|]
            
        let appendWordToInstructionRegister (value:uint16) =
            cpu.Registers.InstructionRegister.Data <- Array.append cpu.Registers.InstructionRegister.Data [|byte value >>> 8; byte value|]

        let DecodeOperand mode register =
            let mutable reg = &cpu.Registers.R.[int register]
            let mutable pc = &cpu.Registers.R.[7]
            match mode with
            | AddressMode.Register -> 
                Reg register

            | AddressMode.RegPostInc ->
                let result = reg
                reg <- result + 2us
                Addr result

            | AddressMode.RegPreDec ->
                let result = reg - 2us
                reg <- result
                Addr result

            | AddressMode.Indexed ->
                let address = MemoryReadWordBigEndian cpu.Memory (int pc)
                pc <- pc + 2us
                appendWordToInstructionRegister address
                Addr (reg + address)

            | AddressMode.RegisterIndirect ->
                Addr reg

            | AddressMode.RegPostIncIndirect ->
                let result = MemoryReadWordBigEndian cpu.Memory (int reg)
                reg <- reg + 2us
                Addr result

            | AddressMode.RegPreDecIndirect ->
                reg <- reg - 2us
                Addr (MemoryReadWordBigEndian cpu.Memory (int reg))

            | AddressMode.IndexedIndirect ->
                let addressIndirect = MemoryReadWordBigEndian cpu.Memory (int pc)
                pc <- pc + 2us
                let addressIndexed = reg + addressIndirect
                appendWordToInstructionRegister addressIndirect
                Addr (MemoryReadWordBigEndian cpu.Memory (int addressIndexed))

            | _ -> failwith "Invalid AddressMode"

        let readByteFromProgramCounterAndAdvance() =
            let result = MemoryReadByte cpu.Memory (int cpu.Registers.R.[7])
            cpu.Registers.R.[7] <- cpu.Registers.R.[7] + 1us
            result

        let firstOpCode = readByteFromProgramCounterAndAdvance()
        cpu.Registers.InstructionRegister.Data <- [|firstOpCode|]
        cpu.Registers.InstructionRegister.SourceOperand <- NoOp
        cpu.Registers.InstructionRegister.TargetOperand <- NoOp

        let instruction = LanguagePrimitives.EnumOfValue (firstOpCode &&& InstructionMask)
        match instruction with
        | Instruction.Nop
        | Instruction.Ccc
        | Instruction.Scc
        | Instruction.Rts
        | Instruction.Hlt -> () // Instructions without operand
        | _ -> // Instructions with operand
            let secondOpCode = readByteFromProgramCounterAndAdvance()
            appendByteToInstructionRegister secondOpCode

            match instruction with
            | Instruction.Br
            | Instruction.Sob -> () // Branch operand
            | _ -> // One or two operands
                match instruction with
                | Instruction.Mov
                | Instruction.Add
                | Instruction.Sub
                | Instruction.Cmp
                | Instruction.And
                | Instruction.Or -> // Second operand
                    let sourceMode = LanguagePrimitives.EnumOfValue ((firstOpCode <<< 2) &&& AddressModeMask)
                    let sourceRegister = LanguagePrimitives.EnumOfValue (((firstOpCode &&& 0x01uy) <<< 2) ||| (secondOpCode >>> 6) &&& RegisterMask)
                    cpu.Registers.InstructionRegister.SourceOperand <- DecodeOperand sourceMode sourceRegister
                | _ -> ()
                // First operand
                let targetMode = LanguagePrimitives.EnumOfValue (secondOpCode &&& AddressModeMask)
                let targetRegister = LanguagePrimitives.EnumOfValue (secondOpCode &&& RegisterMask)
                cpu.Registers.InstructionRegister.TargetOperand <- DecodeOperand targetMode targetRegister
            
    let Execute cpu =
        let isNegative value = 
            value > 0x7FFFus

        let aluAdd (a:uint16) (b:uint16) carryIn =
            let fullResult = int a + int b + if carryIn then 1 else 0
            cpu.Registers.Flags.Carry <- fullResult > 0xFFFF

            let result = uint16 fullResult
            cpu.Registers.Flags.Overflow <- (isNegative a && isNegative b && not (isNegative result)) ||
                                            (not (isNegative a) && not (isNegative b) && isNegative result)
            result                

        let readValueFromOperand operand =
            match operand with
            | Reg register -> cpu.Registers.R.[int register]
            | Addr address -> 
                if address >= DisplayMemoryAddress then     // In display memory area: consider only 8-bits operands 
                    uint16 (MemoryReadByte cpu.Memory (int address))
                else
                    MemoryReadWordBigEndian cpu.Memory (int address)
            | _ -> 0us

        let writeValueToOperand operand value =
            match operand with
            | Reg register -> cpu.Registers.R.[int register] <- value
            | Addr address -> 
                if address >= DisplayMemoryAddress then     // In display memory area: consider only 8-bits operands 
                    byte value |> MemoryWriteByte cpu.Memory (int address)
                else
                    value |> MemoryWriteWordBigEndian cpu.Memory (int address)
            | _ -> ()

        let firstOpCode = cpu.Registers.InstructionRegister.Data.[0]
        let instruction = LanguagePrimitives.EnumOfValue (firstOpCode &&& InstructionMask)
        let register = firstOpCode &&& RegisterMask
            
        let mutable reg = &cpu.Registers.R.[int register]
        let mutable sp = &cpu.Registers.R.[6]
        let mutable pc = &cpu.Registers.R.[7]
        let mutable n = &cpu.Registers.Flags.Negative
        let mutable z = &cpu.Registers.Flags.Zero
        let mutable v = &cpu.Registers.Flags.Overflow
        let mutable c = &cpu.Registers.Flags.Carry

        match instruction with
        | Instruction.Ccc
        | Instruction.Scc ->   // CCC & SCC
            if firstOpCode &&& byte Flag.Negative <> 0uy then
                n <- instruction = Instruction.Scc
            if firstOpCode &&& byte Flag.Zero <> 0uy then
                z <- instruction = Instruction.Scc
            if firstOpCode &&& byte Flag.Overflow <> 0uy then
                v <- instruction = Instruction.Scc
            if firstOpCode &&& byte Flag.Carry <> 0uy then
                c <- instruction = Instruction.Scc

        | Instruction.Br ->   // Branches group
            let branchIf condition = 
                if condition then
                    let branchOperand = int8 cpu.Registers.InstructionRegister.Data.[1]
                    cpu.Registers.R.[7] <- cpu.Registers.R.[7] + (uint16 branchOperand)

            let subInstruction = Instruction.Br ||| LanguagePrimitives.EnumOfValue (firstOpCode &&& SubInstructionMask)
            match subInstruction with
            | Instruction.Br  -> branchIf true
            | Instruction.Bne -> branchIf (not z)
            | Instruction.Beq -> branchIf z
            | Instruction.Bpl -> branchIf (not n)
            | Instruction.Bmi -> branchIf n
            | Instruction.Bvc -> branchIf (not v)
            | Instruction.Bvs -> branchIf v
            | Instruction.Bcc -> branchIf (not c)
            | Instruction.Bcs -> branchIf c
            | Instruction.Bge -> branchIf (n = v)
            | Instruction.Blt -> branchIf (n <> v)
            | Instruction.Bgt -> branchIf ( (n = v) && (not z) )
            | Instruction.Ble -> branchIf ( (n <> v) || z )
            | Instruction.Bhi -> branchIf ( (not c) && (not z) )
            | Instruction.Bls -> branchIf (c || z)
            | _ -> ()

        | Instruction.Jmp ->   // JMP
            match cpu.Registers.InstructionRegister.TargetOperand with
            | Addr address -> pc <- address
            | _ -> () // Mode 0 (register) is not allowed.
            
        | Instruction.Sob ->   // SOB
            reg <- reg - 1us
            if reg <> 0us then
                let branchOperand = uint16 cpu.Registers.InstructionRegister.Data.[1]
                pc <- pc - branchOperand
            
        | Instruction.Jsr ->   // JSR
            match cpu.Registers.InstructionRegister.TargetOperand with
            | Addr address -> 
                sp <- sp - 2us
                reg |> MemoryWriteWordBigEndian cpu.Memory (int sp)
                reg <- pc
                pc <- address
            | _ -> () // Mode 0 (register) is not allowed.
            
        | Instruction.Rts ->   // RTS
            pc <- reg
            reg <- MemoryReadWordBigEndian cpu.Memory (int sp)
            sp <- sp + 2us

        | Instruction.Clr ->   // CLR group
            let subInstruction = Instruction.Clr ||| LanguagePrimitives.EnumOfValue (firstOpCode &&& SubInstructionMask)

            let mutable targetValue = 0us
            if subInstruction <> Instruction.Clr then  // Do not read operand in CLR
                targetValue <- readValueFromOperand cpu.Registers.InstructionRegister.TargetOperand
                  
            match subInstruction with
            | Instruction.Clr ->
                c <- false
                v <- false
                targetValue <- 0us
                    
            | Instruction.Not ->
                c <- true
                v <- false
                targetValue <- ~~~targetValue

            | Instruction.Inc ->
                targetValue <- aluAdd targetValue 1us false

            | Instruction.Dec ->
                targetValue <- aluAdd targetValue 0xFFFFus false
                c <- not c

            | Instruction.Neg ->
                targetValue <- aluAdd ~~~targetValue 1us false
                c <- not c

            | Instruction.Tst ->
                c <- false
                v <- false

            | Instruction.Ror ->
                let higherBit = if c then 0x8000us else 0us
                c <- (targetValue &&& 1us) <> 0us
                targetValue <- targetValue >>> 1 ||| higherBit
                n <- isNegative targetValue
                v <- c <> n

            | Instruction.Rol ->
                let lowerBit = if c then 1us else 0us
                c <- (targetValue &&& 0x8000us) <> 0us
                targetValue <- targetValue <<< 1 ||| lowerBit
                n <- isNegative targetValue
                v <- c <> n

            | Instruction.Asr ->
                let higherBit = targetValue &&& 0x8000us
                c <- (targetValue &&& 1us) <> 0us
                targetValue <- targetValue >>> 1 ||| higherBit
                n <- not (isNegative targetValue)
                v <- c <> n

            | Instruction.Asl ->
                c <- (targetValue &&& 0x8000us) <> 0us
                targetValue <- targetValue <<< 1
                n <- not (isNegative targetValue)
                v <- c <> n

            | Instruction.Adc ->
                let addCar = if c then 1us else 0us
                targetValue <- aluAdd targetValue addCar false

            | Instruction.Sbc ->
                let negCar = if c then 0xFFFEus else 0xFFFFus
                targetValue <- aluAdd targetValue negCar true

            | _ -> ()
                
            n <- isNegative targetValue
            z <- targetValue = 0us

            if subInstruction <> Instruction.Tst then  // Do not write result in TST
                writeValueToOperand cpu.Registers.InstructionRegister.TargetOperand targetValue

        | Instruction.Mov 
        | Instruction.Add 
        | Instruction.Sub 
        | Instruction.Cmp 
        | Instruction.And 
        | Instruction.Or ->    // MOV group
            let sourceValue = readValueFromOperand cpu.Registers.InstructionRegister.SourceOperand

            let mutable targetValue = 0us
            if instruction <> Instruction.Mov then  // Do not read target operand in MOV
                targetValue <- readValueFromOperand cpu.Registers.InstructionRegister.TargetOperand
                  
            match instruction with
            | Instruction.Mov ->
                targetValue <- sourceValue
                cpu.Registers.Flags.Overflow <- false

            | Instruction.Add ->
                targetValue <- aluAdd sourceValue targetValue false

            | Instruction.Sub ->
                targetValue <- aluAdd ~~~sourceValue targetValue true
                c <- not c

            | Instruction.Cmp ->
                targetValue <- aluAdd sourceValue ~~~targetValue true
                c <- not c

            | Instruction.And ->
                targetValue <- targetValue &&& sourceValue
                v <- false

            | Instruction.Or ->
                targetValue <- targetValue ||| sourceValue
                v <- false

            | _ -> ()

            cpu.Registers.Flags.Negative <- isNegative targetValue
            cpu.Registers.Flags.Zero <- targetValue = 0us

            if instruction <> Instruction.Cmp then  // Do not write result in CMP
                writeValueToOperand cpu.Registers.InstructionRegister.TargetOperand targetValue

        | Instruction.Hlt      // HLT, NOP or unknown instruction: nothing to do
        | Instruction.Nop
        | _ -> ()

        cpu.Registers.Flags.Halted <- instruction = Instruction.Hlt

    let Step cpu =
        Fetch cpu
        Execute cpu

    let Reset cpu =
        cpu.Registers |> RegistersReset
        cpu.Memory |> MemoryReset 
