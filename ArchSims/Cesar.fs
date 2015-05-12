namespace Ufrgs.Inf.ArchSims

open Ufrgs.Inf.ArchSims.Common

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
    | Register           = 0x00uy // 0000 0000
    | RegPostInc         = 0x08uy // 0000 1000
    | RegPreDec          = 0x10uy // 0001 0000
    | Indexed            = 0x18uy // 0001 1000
    | RegisterIndirect   = 0x20uy // 0010 0000
    | RegPostIncIndirect = 0x28uy // 0010 1000
    | RegPreDecIndirect  = 0x30uy // 0011 0000
    | IndexedIndirect    = 0x38uy // 0011 1000

    let Indirect  = 0x20uy //0010 0000
    let Immediate = byte AddressMode.RegPostInc ||| byte Register.R7
    let Direct    = Immediate ||| Indirect
        
    let InstructionMask    = 0xF0uy // 1111 0000
    let SubInstructionMask = 0x0Fuy // 0000 1111
    let RegisterMask       = 0x07uy // 0000 0111
    let AddressModeMask    = 0x38uy // 0011 1000

    let DisplayMemoryAddress = 0xFFDAus // Start of display memory mapped area 

    type Flag =
    | Negative = 0x08uy // 0000 1000
    | Zero     = 0x04uy // 0000 0100
    | Overflow = 0x02uy // 0000 0010
    | Carry    = 0x01uy // 0000 0001

    type Flags = {
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
        Flags = { Negative = false; Zero = true; Overflow = false; Carry = false }
    }

    let ClearRegisters registers = 
        Array.fill registers.R 0 registers.R.Length 0us
        registers.InstructionRegister.Data <- [||]
        registers.InstructionRegister.SourceOperand <- NoOp
        registers.InstructionRegister.TargetOperand <- NoOp
        registers.Flags.Negative <- false
        registers.Flags.Zero <- true
        registers.Flags.Overflow <- false
        registers.Flags.Carry <- false    
    
    type Cpu() =
        member val public Registers = CreateRegisters() with get, set
        member val public Memory = CreateMemory 65536 with get
        member val public Debugger = CreateDebugger() with get, set

        member this.ReturnProgramCounterAndAdvance() =
            let result = this.Registers.R.[7]
            this.Registers.R.[7] <- this.Registers.R.[7] + 1us            
            result

        member this.AppendByteToInstructionRegister value =
            this.Registers.InstructionRegister.Data <- Array.append this.Registers.InstructionRegister.Data [|value|]
            
        member this.AppendWordToInstructionRegister (value:uint16) =
            this.Registers.InstructionRegister.Data <- Array.append this.Registers.InstructionRegister.Data [|byte value >>> 8; byte value|]

        member this.EncodeInstructionOneOperand (instruction:Instruction) (targetMode:AddressMode) (targetRegister:Register) =
            (uint16 instruction <<< 8) ||| uint16 (byte targetMode &&& AddressModeMask) ||| uint16 (byte targetRegister &&& RegisterMask)
            
        member this.EncodeInstructionTwoOperand (instruction:Instruction) (sourceMode:AddressMode) (sourceRegister:Register) (targetMode:AddressMode) (targetRegister:Register) =
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
                let firstPart = this.EncodeInstructionOneOperand instruction targetMode targetRegister
                let result = firstPart ||| (uint16 (byte sourceMode &&& AddressModeMask) <<< 6) ||| (uint16 (byte sourceRegister &&& RegisterMask) <<< 6)
                result
            | _ -> 
                failwith "This function can encode only instructions with two operands."

        member this.Fetch() =
            let DecodeOperand mode register =
                let mutable reg = &this.Registers.R.[int register]
                let mutable pc = &this.Registers.R.[7]
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
                    let address = int pc |> ReadWordBigEndian this.Memory
                    pc <- pc + 2us
                    this.AppendWordToInstructionRegister address
                    Addr (reg + address)

                | AddressMode.RegisterIndirect ->
                    Addr reg

                | AddressMode.RegPostIncIndirect ->
                    let result = int reg |> ReadWordBigEndian this.Memory
                    reg <- reg + 2us
                    Addr result

                | AddressMode.RegPreDecIndirect ->
                    reg <- reg - 2us
                    Addr (int reg |> ReadWordBigEndian this.Memory)

                | AddressMode.IndexedIndirect ->
                    let addressIndirect = int pc |> ReadWordBigEndian this.Memory
                    pc <- pc + 2us
                    let addressIndexed = reg + addressIndirect
                    this.AppendWordToInstructionRegister addressIndirect
                    Addr (int addressIndexed |> ReadWordBigEndian this.Memory)

                | _ -> failwith "Invalid AddressMode"

            let readByteFromProgramCounterAndAdvance() =
                let result = int this.Registers.R.[7] |> ReadByte this.Memory
                this.Registers.R.[7] <- this.Registers.R.[7] + 1us
                result

            let firstOpCode = readByteFromProgramCounterAndAdvance()
            this.Registers.InstructionRegister.Data <- [|firstOpCode|]
            this.Registers.InstructionRegister.SourceOperand <- NoOp
            this.Registers.InstructionRegister.TargetOperand <- NoOp

            let instruction = LanguagePrimitives.EnumOfValue (firstOpCode &&& InstructionMask)
            match instruction with
            | Instruction.Nop
            | Instruction.Ccc
            | Instruction.Scc
            | Instruction.Rts
            | Instruction.Hlt -> () // Instructions without operand
            | _ -> // Instructions with operand
                let secondOpCode = readByteFromProgramCounterAndAdvance()
                this.AppendByteToInstructionRegister secondOpCode

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
                        this.Registers.InstructionRegister.SourceOperand <- DecodeOperand sourceMode sourceRegister
                    | _ -> ()
                    // First operand
                    let targetMode = LanguagePrimitives.EnumOfValue (secondOpCode &&& AddressModeMask)
                    let targetRegister = LanguagePrimitives.EnumOfValue (secondOpCode &&& RegisterMask)
                    this.Registers.InstructionRegister.TargetOperand <- DecodeOperand targetMode targetRegister
            
        member this.Execute() =
            let branchIf condition = 
                let branchOperand = uint16 this.Registers.InstructionRegister.Data.[1]
                if condition then
                    this.Registers.R.[7] <- this.Registers.R.[7] + branchOperand

            let isNegative value = 
                value > 0x7FFFus

            let aluAdd (a:uint16) (b:uint16) carryIn =
                let fullResult = int a + int b + if carryIn then 1 else 0
                this.Registers.Flags.Carry <- fullResult > 0xFFFF

                let result = uint16 fullResult
                this.Registers.Flags.Overflow <- (isNegative a && isNegative b && not (isNegative result)) ||
                                                 (not (isNegative a) && not (isNegative b) && isNegative result)
                result                

            let readValueFromOperand operand =
                match operand with
                | Reg register -> this.Registers.R.[int register]
                | Addr address -> 
                    if address >= DisplayMemoryAddress then     // In display memory area: consider only 8-bits operands 
                        uint16 (int address |> ReadByte this.Memory)
                    else
                        int address |> ReadWordBigEndian this.Memory
                | _ -> 0us

            let writeValueToOperand operand value =
                match operand with
                | Reg register -> this.Registers.R.[int register] <- value
                | Addr address -> 
                    if address >= DisplayMemoryAddress then     // In display memory area: consider only 8-bits operands 
                        byte value |> WriteByte this.Memory (int address)
                    else
                        value |> WriteWordBigEndian this.Memory (int address)
                | _ -> ()

            let firstOpCode = this.Registers.InstructionRegister.Data.[0]
            let instruction = LanguagePrimitives.EnumOfValue (firstOpCode &&& InstructionMask)
            let register = firstOpCode &&& RegisterMask
            
            let mutable reg = &this.Registers.R.[int register]
            let mutable sp = &this.Registers.R.[6]
            let mutable pc = &this.Registers.R.[7]
            let mutable n = &this.Registers.Flags.Negative
            let mutable z = &this.Registers.Flags.Zero
            let mutable v = &this.Registers.Flags.Overflow
            let mutable c = &this.Registers.Flags.Carry

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
                let subInstruction = Instruction.Br ||| LanguagePrimitives.EnumOfValue (firstOpCode &&& SubInstructionMask)
                match subInstruction with
                | Instruction.Br -> branchIf true
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
                match this.Registers.InstructionRegister.TargetOperand with
                | Addr address -> pc <- address
                | _ -> () // Mode 0 (register) is not allowed.
            
            | Instruction.Sob ->   // SOB
                reg <- reg - 1us
                if reg <> 0us then
                    let branchOperand = uint16 this.Registers.InstructionRegister.Data.[1]
                    pc <- pc - branchOperand
            
            | Instruction.Jsr ->   // JSR
                match this.Registers.InstructionRegister.TargetOperand with
                | Addr address -> 
                    sp <- sp - 2us
                    reg |> WriteWordBigEndian this.Memory (int sp)
                    reg <- pc
                    pc <- address
                | _ -> () // Mode 0 (register) is not allowed.
            
            | Instruction.Rts ->   // RTS
                pc <- reg
                reg <- int sp |> ReadWordBigEndian this.Memory
                sp <- sp + 2us

            | Instruction.Clr ->   // CLR group
                let subInstruction = Instruction.Clr ||| LanguagePrimitives.EnumOfValue (firstOpCode &&& SubInstructionMask)

                let mutable targetValue = 0us
                if subInstruction <> Instruction.Clr then  // Do not read operand in CLR
                    targetValue <- readValueFromOperand this.Registers.InstructionRegister.TargetOperand
                  
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
                    writeValueToOperand this.Registers.InstructionRegister.TargetOperand targetValue

            | Instruction.Mov 
            | Instruction.Add 
            | Instruction.Sub 
            | Instruction.Cmp 
            | Instruction.And 
            | Instruction.Or ->   // MOV group
                let sourceValue = readValueFromOperand this.Registers.InstructionRegister.SourceOperand

                let mutable targetValue = 0us
                if instruction <> Instruction.Mov then  // Do not read target operand in MOV
                    targetValue <- readValueFromOperand this.Registers.InstructionRegister.TargetOperand
                  
                match instruction with
                | Instruction.Mov ->
                    targetValue <- sourceValue
                    this.Registers.Flags.Overflow <- false

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

                this.Registers.Flags.Negative <- isNegative targetValue
                this.Registers.Flags.Zero <- targetValue = 0us

                if instruction <> Instruction.Cmp then  // Do not write result in CMP
                    writeValueToOperand this.Registers.InstructionRegister.TargetOperand targetValue

            | Instruction.Hlt ->   // HLT
                this.Debugger.Halted <- true
                
            | Instruction.Nop      // NOP (or unknown instruction): nothing to do
            | _ -> ()

        member this.Step() =
            this.Debugger |> ClearDebuggerFlags
            if this.Debugger.Breakpoints.Contains (int this.Registers.R.[7]) then
                this.Debugger.BreakpointHit <- true

            this.Fetch()
            this.Execute()

            this.Debugger.InstructionCount <- this.Debugger.InstructionCount + 1

        member this.Reset() =
            this.Registers |> ClearRegisters
            this.Memory |> ClearMemory 
            this.Debugger |> ClearDebugger 

        member this.Run() =
            this.Run 10000000

        member this.Run maximumInstructions =
            this.Debugger |> ClearDebuggerFlags
            while (not this.Debugger.Halted) && (not this.Debugger.BreakpointHit) do
                this.Step()
                if this.Debugger.InstructionCount >= maximumInstructions then
                    raise CpuRunningForeverException
