namespace Ufrgs.Inf.ArchSims.Core

open LanguagePrimitives

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
        InstructionRegister = { Data = [|0uy|]; SourceOperand = NoOp; TargetOperand = NoOp }
        Flags = { Halted = false; Negative = false; Zero = true; Overflow = false; Carry = false }
    }

    let RegistersReset registers = 
        Array.fill registers.R 0 registers.R.Length 0us
        registers.InstructionRegister.Data <- [|0uy|]
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
            failwith "This function only encode instructions with two operands."

    let Fetch cpu = 
        let _r = cpu.Registers.R
        let _ir = cpu.Registers.InstructionRegister

        let appendByteToInstructionRegister value =
            _ir.Data <- Array.append _ir.Data [|value|]
            
        let appendWordToInstructionRegister (value:uint16) =
            _ir.Data <- Array.append _ir.Data [|byte value >>> 8; byte value|]

        let readRegisterAndInc register = 
            let result = _r.[int register]
            _r.[int register] <- result + 2us
            result

        let decRegisterAndRead register = 
            let result = _r.[int register] - 2us
            _r.[int register] <- result
            result

        let readWordAndInc register = 
            let result = MemoryReadWordBigEndian cpu.Memory (int _r.[int register])
            _r.[int register] <- _r.[int register] + 2us
            result

        let decAndReadWord register = 
            _r.[int register] <- _r.[int register] - 2us
            MemoryReadWordBigEndian cpu.Memory (int _r.[int register])

        let DecodeOperand mode register =
            match mode with
            | AddressMode.Register -> 
                Reg register

            | AddressMode.RegPostInc ->
                Addr (readRegisterAndInc register)

            | AddressMode.RegPreDec ->
                Addr (decRegisterAndRead register)

            | AddressMode.Indexed ->
                let address = readWordAndInc Register.R7
                appendWordToInstructionRegister address
                Addr (_r.[int register] + address)

            | AddressMode.RegisterIndirect ->
                Addr _r.[int register]

            | AddressMode.RegPostIncIndirect ->
                Addr (readWordAndInc register)

            | AddressMode.RegPreDecIndirect ->
                Addr (decAndReadWord register)

            | AddressMode.IndexedIndirect ->
                let addressIndirect = readWordAndInc Register.R7
                appendWordToInstructionRegister addressIndirect
                let addressIndexed = _r.[int register] + addressIndirect
                Addr (MemoryReadWordBigEndian cpu.Memory (int addressIndexed))

            | _ -> failwith "Invalid AddressMode"

        let readByteFromProgramCounterAndAdvance() =
            let result = MemoryReadByte cpu.Memory (int _r.[7])
            _r.[7] <- _r.[7] + 1us
            result

        let firstOpCode = readByteFromProgramCounterAndAdvance()
        _ir.Data <- [|firstOpCode|]
        _ir.SourceOperand <- NoOp
        _ir.TargetOperand <- NoOp

        let instruction = firstOpCode &&& InstructionMask |> EnumOfValue
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
                    let sourceMode = (firstOpCode <<< 2) &&& AddressModeMask |> EnumOfValue
                    let sourceRegister = ((firstOpCode &&& 0x01uy) <<< 2) ||| (secondOpCode >>> 6) &&& RegisterMask |> EnumOfValue
                    _ir.SourceOperand <- DecodeOperand sourceMode sourceRegister
                | _ -> ()
                // First operand
                let targetMode = secondOpCode &&& AddressModeMask |> EnumOfValue
                let targetRegister = secondOpCode &&& RegisterMask |> EnumOfValue
                _ir.TargetOperand <- DecodeOperand targetMode targetRegister
            
    let Execute cpu =
        let _r = cpu.Registers.R
        let _ir = cpu.Registers.InstructionRegister
        let _flags = cpu.Registers.Flags

        let isNegative value = 
            value > 0x7FFFus

        let aluAdd (a:uint16) (b:uint16) carryIn =
            let fullResult = int a + int b + if carryIn then 1 else 0
            _flags.Carry <- fullResult > 0xFFFF

            let result = uint16 fullResult
            _flags.Overflow <- (     isNegative a  &&      isNegative b  && not (isNegative result)) ||
                               (not (isNegative a) && not (isNegative b) &&      isNegative result)
            result                

        let readValueFromOperand operand =
            match operand with
            | Reg register -> _r.[int register]
            | Addr address -> 
                if address >= DisplayMemoryAddress then     // In display memory area: consider only 8-bits operands 
                    MemoryReadByte cpu.Memory (int address) |> uint16
                else
                    MemoryReadWordBigEndian cpu.Memory (int address)
            | _ -> 0us

        let writeValueToOperand operand value =
            match operand with
            | Reg register -> _r.[int register] <- value
            | Addr address -> 
                if address >= DisplayMemoryAddress then     // In display memory area: consider only 8-bits operands 
                    byte value |> MemoryWriteByte cpu.Memory (int address)
                else
                    value |> MemoryWriteWordBigEndian cpu.Memory (int address)
            | _ -> ()

        let firstOpCode = _ir.Data.[0]
        let instruction = firstOpCode &&& InstructionMask |> EnumOfValue
        let register = firstOpCode &&& RegisterMask
           
        match instruction with
        | Instruction.Ccc
        | Instruction.Scc ->   // CCC & SCC
            if firstOpCode &&& byte Flag.Negative <> 0uy then
                _flags.Negative <- instruction = Instruction.Scc
            if firstOpCode &&& byte Flag.Zero <> 0uy then
                _flags.Zero <- instruction = Instruction.Scc
            if firstOpCode &&& byte Flag.Overflow <> 0uy then
                _flags.Overflow <- instruction = Instruction.Scc
            if firstOpCode &&& byte Flag.Carry <> 0uy then
                _flags.Carry <- instruction = Instruction.Scc

        | Instruction.Br ->   // Branches group
            let branchIf condition = 
                if condition then
                    let branchOperand = int8 _ir.Data.[1]
                    _r.[7] <- _r.[7] + (uint16 branchOperand)

            let branchIfNot condition = 
                branchIf (not condition)

            let subInstruction = Instruction.Br ||| EnumOfValue (firstOpCode &&& SubInstructionMask)
            match subInstruction with
            | Instruction.Br  -> branchIf       true
            | Instruction.Bne -> branchIfNot    _flags.Zero
            | Instruction.Beq -> branchIf       _flags.Zero
            | Instruction.Bpl -> branchIfNot    _flags.Negative
            | Instruction.Bmi -> branchIf       _flags.Negative
            | Instruction.Bvc -> branchIfNot    _flags.Overflow
            | Instruction.Bvs -> branchIf       _flags.Overflow
            | Instruction.Bcc -> branchIfNot    _flags.Carry
            | Instruction.Bcs -> branchIf       _flags.Carry
            | Instruction.Bge -> branchIf      (_flags.Negative = _flags.Overflow)
            | Instruction.Blt -> branchIfNot   (_flags.Negative = _flags.Overflow)
            | Instruction.Bgt -> branchIf    ( (_flags.Negative = _flags.Overflow) && (not _flags.Zero) )
            | Instruction.Ble -> branchIfNot ( (_flags.Negative = _flags.Overflow) && (not _flags.Zero) )
            | Instruction.Bhi -> branchIfNot   (_flags.Carry || _flags.Zero)
            | Instruction.Bls -> branchIf      (_flags.Carry || _flags.Zero)
            | _ -> ()

        | Instruction.Jmp ->   // JMP
            match _ir.TargetOperand with
            | Addr address -> _r.[7] <- address
            | _ -> () // Mode 0 (register) is not allowed.
            
        | Instruction.Sob ->   // SOB
            _r.[int register] <- _r.[int register] - 1us
            if _r.[int register] <> 0us then
                let branchOperand = uint16 _ir.Data.[1]
                _r.[7] <- _r.[7] - branchOperand
            
        | Instruction.Jsr ->   // JSR
            match _ir.TargetOperand with
            | Addr address -> 
                _r.[6] <- _r.[6] - 2us
                _r.[int register] |> MemoryWriteWordBigEndian cpu.Memory (int _r.[6])
                _r.[int register] <- _r.[7]
                _r.[7] <- address
            | _ -> () // Mode 0 (register) is not allowed.
            
        | Instruction.Rts ->   // RTS
            _r.[7] <- _r.[int register]
            _r.[int register] <- MemoryReadWordBigEndian cpu.Memory (int _r.[6])
            _r.[6] <- _r.[6] + 2us

        | Instruction.Clr ->   // CLR group
            let subInstruction = Instruction.Clr ||| EnumOfValue (firstOpCode &&& SubInstructionMask)

            let mutable targetValue = 0us
            if subInstruction <> Instruction.Clr then  // Do not read operand in CLR
                targetValue <- readValueFromOperand _ir.TargetOperand
                  
            match subInstruction with
            | Instruction.Clr ->
                _flags.Carry <- false
                _flags.Overflow <- false
                targetValue <- 0us
                    
            | Instruction.Not ->
                _flags.Carry <- true
                _flags.Overflow <- false
                targetValue <- ~~~targetValue

            | Instruction.Inc ->
                targetValue <- aluAdd targetValue 1us false

            | Instruction.Dec ->
                targetValue <- aluAdd targetValue 0xFFFFus false
                _flags.Carry <- not _flags.Carry

            | Instruction.Neg ->
                targetValue <- aluAdd ~~~targetValue 1us false
                _flags.Carry <- not _flags.Carry

            | Instruction.Tst ->
                _flags.Carry <- false
                _flags.Overflow <- false

            | Instruction.Ror ->
                let higherBit = if _flags.Carry then 0x8000us else 0us
                _flags.Carry <- (targetValue &&& 1us) <> 0us
                targetValue <- targetValue >>> 1 ||| higherBit
                _flags.Negative <- isNegative targetValue
                _flags.Overflow <- _flags.Carry <> _flags.Negative

            | Instruction.Rol ->
                let lowerBit = if _flags.Carry then 1us else 0us
                _flags.Carry <- (targetValue &&& 0x8000us) <> 0us
                targetValue <- targetValue <<< 1 ||| lowerBit
                _flags.Negative <- isNegative targetValue
                _flags.Overflow <- _flags.Carry <> _flags.Negative

            | Instruction.Asr ->
                let higherBit = targetValue &&& 0x8000us
                _flags.Carry <- (targetValue &&& 1us) <> 0us
                targetValue <- targetValue >>> 1 ||| higherBit
                _flags.Negative <- not (isNegative targetValue)
                _flags.Overflow <- _flags.Carry <> _flags.Negative

            | Instruction.Asl ->
                _flags.Carry <- (targetValue &&& 0x8000us) <> 0us
                targetValue <- targetValue <<< 1
                _flags.Negative <- not (isNegative targetValue)
                _flags.Overflow <- _flags.Carry <> _flags.Negative

            | Instruction.Adc ->
                let addCar = if _flags.Carry then 1us else 0us
                targetValue <- aluAdd targetValue addCar false

            | Instruction.Sbc ->
                let negCar = if _flags.Carry then 0xFFFEus else 0xFFFFus
                targetValue <- aluAdd targetValue negCar true

            | _ -> ()
                
            _flags.Negative <- isNegative targetValue
            _flags.Zero <- targetValue = 0us

            if subInstruction <> Instruction.Tst then  // Do not write result in TST
                writeValueToOperand _ir.TargetOperand targetValue

        | Instruction.Mov 
        | Instruction.Add 
        | Instruction.Sub 
        | Instruction.Cmp 
        | Instruction.And 
        | Instruction.Or ->    // MOV group
            let sourceValue = readValueFromOperand _ir.SourceOperand

            let mutable targetValue = 0us
            if instruction <> Instruction.Mov then  // Do not read target operand in MOV
                targetValue <- readValueFromOperand _ir.TargetOperand
                  
            match instruction with
            | Instruction.Mov ->
                targetValue <- sourceValue
                _flags.Overflow <- false

            | Instruction.Add ->
                targetValue <- aluAdd sourceValue targetValue false

            | Instruction.Sub ->
                targetValue <- aluAdd ~~~sourceValue targetValue true
                _flags.Carry <- not _flags.Carry

            | Instruction.Cmp ->
                targetValue <- aluAdd sourceValue ~~~targetValue true
                _flags.Carry <- not _flags.Carry

            | Instruction.And ->
                targetValue <- targetValue &&& sourceValue
                _flags.Overflow <- false

            | Instruction.Or ->
                targetValue <- targetValue ||| sourceValue
                _flags.Overflow <- false

            | _ -> ()

            _flags.Negative <- isNegative targetValue
            _flags.Zero <- targetValue = 0us

            if instruction <> Instruction.Cmp then  // Do not write result in CMP
                writeValueToOperand _ir.TargetOperand targetValue

        | Instruction.Hlt      // HLT, NOP or unknown instruction: nothing to do
        | Instruction.Nop
        | _ -> ()

        _flags.Halted <- instruction = Instruction.Hlt

    let Step cpu =
        Fetch cpu
        Execute cpu

    let Reset cpu =
        cpu.Registers |> RegistersReset
        cpu.Memory |> MemoryReset

    let DisassembleInstruction content =
        let data = Array.ofList content
        if data.Length > 0 then
            let firstOpCode = data.[0]
            let mutable nextOperandIndex = 2

            let flags() =
                let printFlag character mask = 
                    if (firstOpCode &&& byte mask) <> 0uy then character else ""

                printFlag "N" Flag.Negative +
                printFlag "Z" Flag.Zero +
                printFlag "V" Flag.Overflow +
                printFlag "C" Flag.Carry

            let register(reg) =
                sprintf "R%i" (reg &&& RegisterMask)

            let branchOperand() = 
                sprintf "%i" data.[1]

            let getNextOperand() =
                let result = (uint16 data.[nextOperandIndex] <<< 8) ||| (uint16 data.[nextOperandIndex + 1])
                nextOperandIndex <- nextOperandIndex + 2
                result

            let decodeOperand mode reg =
                let r = register(reg)
                match EnumOfValue mode with
                | AddressMode.Register           -> r
                | AddressMode.RegPostInc         -> sprintf "(%s)+" r
                | AddressMode.RegPreDec          -> sprintf "-(%s)" r
                | AddressMode.Indexed            -> sprintf "%i(%s)" (getNextOperand()) r
                | AddressMode.RegisterIndirect   -> sprintf "(%s)" r
                | AddressMode.RegPostIncIndirect -> sprintf "((%s)+)" r
                | AddressMode.RegPreDecIndirect  -> sprintf "(-(%s))" r
                | AddressMode.IndexedIndirect    -> sprintf "(%i(%s))" (getNextOperand()) r
                | _ -> failwith "Invalid AddressMode"
                
            let targetOperand() =
                let secondOpCode = data.[1]
                let mode = secondOpCode &&& AddressModeMask
                let reg = secondOpCode &&& RegisterMask
                decodeOperand mode reg

            let jumpOperand() =
                let secondOpCode = data.[1]
                let mode = secondOpCode &&& AddressModeMask
                match LanguagePrimitives.EnumOfValue mode with
                | AddressMode.Register -> "?" // Illegal mode for jumps
                | _ -> targetOperand()                    

            let sourceOperand() =
                let secondOpCode = data.[1]
                let mode = (firstOpCode <<< 2) &&& AddressModeMask
                let reg = ((firstOpCode &&& 0x01uy) <<< 2) ||| (secondOpCode >>> 6) &&& RegisterMask
                decodeOperand mode reg

            let instruction = firstOpCode &&& InstructionMask |> EnumOfValue
            match instruction with
            | Instruction.Ccc -> ("CCC " + flags()).Trim()
            | Instruction.Scc -> ("SCC " + flags()).Trim()

            | Instruction.Br  -> 
                let subInstruction = Instruction.Br ||| EnumOfValue (firstOpCode &&& SubInstructionMask)
                match subInstruction with
                | Instruction.Br  -> "BR  " + branchOperand()
                | Instruction.Bne -> "BNE " + branchOperand()
                | Instruction.Beq -> "BEQ " + branchOperand()
                | Instruction.Bpl -> "BPL " + branchOperand()
                | Instruction.Bmi -> "BMI " + branchOperand()
                | Instruction.Bvc -> "BVC " + branchOperand()
                | Instruction.Bvs -> "BVS " + branchOperand()
                | Instruction.Bcc -> "BCC " + branchOperand()
                | Instruction.Bcs -> "BCS " + branchOperand()
                | Instruction.Bge -> "BGE " + branchOperand()
                | Instruction.Blt -> "BLT " + branchOperand()
                | Instruction.Bgt -> "BGT " + branchOperand()
                | Instruction.Ble -> "BLE " + branchOperand()
                | Instruction.Bhi -> "BHI " + branchOperand()
                | Instruction.Bls -> "BLS " + branchOperand()
                | _ -> "?"

            | Instruction.Jmp -> "JMP " + jumpOperand()
            | Instruction.Sob -> "SOB " + register(data.[0]) + ", " + branchOperand()
            | Instruction.Jsr -> "JSR " + register(data.[0]) + ", " + jumpOperand()
            | Instruction.Rts -> "RTS " + register(data.[0])

            | Instruction.Clr ->
                let subInstruction = Instruction.Clr ||| EnumOfValue (firstOpCode &&& SubInstructionMask)
                match subInstruction with
                | Instruction.Clr -> "CLR " + targetOperand()
                | Instruction.Not -> "NOT " + targetOperand()
                | Instruction.Inc -> "INC " + targetOperand()
                | Instruction.Dec -> "DEC " + targetOperand()
                | Instruction.Neg -> "NEG " + targetOperand()
                | Instruction.Tst -> "TST " + targetOperand()
                | Instruction.Ror -> "ROR " + targetOperand()
                | Instruction.Rol -> "ROL " + targetOperand()
                | Instruction.Asr -> "ASR " + targetOperand()
                | Instruction.Asl -> "ASL " + targetOperand()
                | Instruction.Adc -> "ADC " + targetOperand()
                | Instruction.Sbc -> "SBC " + targetOperand()
                | _ -> "?"

            | Instruction.Mov -> "MOV " + sourceOperand() + ", " + targetOperand()
            | Instruction.Add -> "ADD " + sourceOperand() + ", " + targetOperand()
            | Instruction.Sub -> "SUB " + sourceOperand() + ", " + targetOperand()
            | Instruction.Cmp -> "CMP " + sourceOperand() + ", " + targetOperand()
            | Instruction.And -> "AND " + sourceOperand() + ", " + targetOperand()
            | Instruction.Or  -> "OR  " + sourceOperand() + ", " + targetOperand()

            | Instruction.Hlt -> "HLT"
            | Instruction.Nop
            | _ -> "NOP"
        else 
            ""
