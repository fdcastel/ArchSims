namespace Ufrgs.Inf.ArchSims

open Ufrgs.Inf.ArchSims.Memory

module Ramses =

    type Instruction = 
    | Nop = 0x00uy // 0000 0000
    | Str = 0x10uy // 0001 0000
    | Ldr = 0x20uy // 0010 0000
    | Add = 0x30uy // 0011 0000
    | Or  = 0x40uy // 0100 0000
    | And = 0x50uy // 0101 0000
    | Not = 0x60uy // 0110 0000
    | Sub = 0x70uy // 0111 0000
    | Jmp = 0x80uy // 1000 0000
    | Jn  = 0x90uy // 1001 0000
    | Jz  = 0xA0uy // 1010 0000
    | Jc  = 0xB0uy // 1011 0000
    | Jsr = 0xC0uy // 1100 0000
    | Neg = 0xD0uy // 1101 0000
    | Shr = 0xE0uy // 1110 0000
    | Hlt = 0xF0uy // 1111 0000

    type Register =
    | Ra = 0x00uy // 0000 0000
    | Rb = 0x04uy // 0000 0100
    | Rx = 0x08uy // 0000 1000
    | Pc = 0x0Cuy // 0000 1100

    type AddressMode =
    | Direct    = 0x00uy // 0000 0000
    | Indirect  = 0x01uy // 0000 0001
    | Immediate = 0x02uy // 0000 0010
    | Indexed   = 0x03uy // 0000 0011
        
    let InstructionMask = 0xF0uy // 1111 0000
    let RegisterMask    = 0x0Cuy // 0000 1100
    let AddressModeMask = 0x03uy // 0000 0011

    type InstructionRegister = {
        mutable OpCode: byte;
        mutable OperandAddress: byte
    }

    type Flags = {
        mutable Halted: bool
        mutable Negative: bool
        mutable Zero: bool
        mutable Carry: bool
    }

    type Registers = {
        mutable Ra: byte
        mutable Rb: byte
        mutable Rx: byte
        mutable ProgramCounter: byte
        InstructionRegister: InstructionRegister
        Flags: Flags
    }

    let CreateRegisters() = {
        Ra = 0uy
        Rb = 0uy
        Rx = 0uy
        ProgramCounter = 0uy
        InstructionRegister = { OpCode = 0uy; OperandAddress = 0uy }
        Flags = { Halted = false; Negative = false; Zero = true; Carry = false }
    }

    let RegistersReset registers = 
        registers.Ra <- 0uy
        registers.Rb <- 0uy
        registers.Rx <- 0uy
        registers.ProgramCounter <- 0uy
        registers.InstructionRegister.OpCode <- 0uy
        registers.InstructionRegister.OperandAddress <- 0uy            
        registers.Flags.Halted <- false
        registers.Flags.Negative <- false
        registers.Flags.Zero <- true
        registers.Flags.Carry <- false
    
    type Cpu = {
        Registers: Registers
        Memory: Memory
    }

    let CreateCpu() = {
        Registers = CreateRegisters()
        Memory = CreateMemory 256
    }

    let Fetch cpu = 
        let returnProgramCounterAndAdvance() =
            let result = cpu.Registers.ProgramCounter
            cpu.Registers.ProgramCounter <- cpu.Registers.ProgramCounter + 1uy            
            result

        let readByteFromProgramCounterAndAdvance() =
            let result = MemoryReadByte cpu.Memory (int cpu.Registers.ProgramCounter)
            cpu.Registers.ProgramCounter <- cpu.Registers.ProgramCounter + 1uy            
            result

        cpu.Registers.InstructionRegister.OpCode <- readByteFromProgramCounterAndAdvance()

        let instruction = LanguagePrimitives.EnumOfValue (cpu.Registers.InstructionRegister.OpCode &&& InstructionMask)
        let addressMode = LanguagePrimitives.EnumOfValue (cpu.Registers.InstructionRegister.OpCode &&& AddressModeMask)
        cpu.Registers.InstructionRegister.OperandAddress <- 
            match instruction with
            | Instruction.Str
            | Instruction.Ldr
            | Instruction.Add
            | Instruction.Or
            | Instruction.And
            | Instruction.Sub
            | Instruction.Jmp
            | Instruction.Jn
            | Instruction.Jz 
            | Instruction.Jc
            | Instruction.Jsr -> // Instructions with operand
                match addressMode with
                | AddressMode.Direct -> readByteFromProgramCounterAndAdvance()
                | AddressMode.Indirect -> MemoryReadByte cpu.Memory (int (readByteFromProgramCounterAndAdvance()))
                | AddressMode.Immediate -> if instruction >= Instruction.Jmp then readByteFromProgramCounterAndAdvance() else returnProgramCounterAndAdvance()
                | AddressMode.Indexed -> cpu.Registers.Rx + readByteFromProgramCounterAndAdvance()
                | _ -> failwith "Invalid AddressMode"
            | _ -> 0uy // Instructions without operand

    let Execute cpu =
        let instruction = LanguagePrimitives.EnumOfValue (cpu.Registers.InstructionRegister.OpCode &&& InstructionMask)
        let register = LanguagePrimitives.EnumOfValue (cpu.Registers.InstructionRegister.OpCode &&& RegisterMask)

        let readOperand() =
            MemoryReadByte cpu.Memory (int cpu.Registers.InstructionRegister.OperandAddress)

        let registerValue = 
            match register with
            | Register.Ra -> cpu.Registers.Ra
            | Register.Rb -> cpu.Registers.Rb
            | Register.Rx -> cpu.Registers.Rx
            | _ -> cpu.Registers.ProgramCounter

        let writeRegister value = 
            match register with
            | Register.Ra -> cpu.Registers.Ra <- value
            | Register.Rb -> cpu.Registers.Rb <- value
            | Register.Rx -> cpu.Registers.Rx <- value
            | _ -> cpu.Registers.ProgramCounter <- value
            cpu.Registers.Flags.Zero <- value = 0uy
            cpu.Registers.Flags.Negative <- value > 0x7Fuy

        let writeRegisterAndCarry carryFun (value:int) = 
            byte value |> writeRegister 
            cpu.Registers.Flags.Carry <- carryFun value

        let jumpIf condition = 
            if condition then
                cpu.Registers.ProgramCounter <- cpu.Registers.InstructionRegister.OperandAddress                

        match instruction with
        | Instruction.Str ->   // STR r oper
            registerValue |> MemoryWriteByte cpu.Memory (int cpu.Registers.InstructionRegister.OperandAddress)

        | Instruction.Ldr ->   // LDR r oper
            readOperand() |> writeRegister

        | Instruction.Add ->   // ADD r oper
            int registerValue + int (readOperand()) |> writeRegisterAndCarry (fun value -> value > 0xFF)

        | Instruction.Or ->    // OR r oper
            registerValue ||| readOperand() |> writeRegister

        | Instruction.And ->   // AND r oper
            registerValue &&& readOperand() |> writeRegister

        | Instruction.Not ->   // NOT r
            ~~~registerValue |> writeRegister

        | Instruction.Sub ->   // SUB r oper
            int registerValue + 0x100 - int (readOperand()) |> writeRegisterAndCarry (fun value -> value > 0xFF)
            cpu.Registers.Flags.Carry <- not cpu.Registers.Flags.Carry

        | Instruction.Jmp ->   // JMP addr: PC <- addr
            jumpIf true
                    
        | Instruction.Jn ->    // JN addr: IF N=1 PC <- addr
            jumpIf cpu.Registers.Flags.Negative

        | Instruction.Jz ->    // JZ addr: IF Z=1 PC <- addr
            jumpIf cpu.Registers.Flags.Zero
                
        | Instruction.Jc ->    // JC addr: IF C=1 PC <- addr
            jumpIf cpu.Registers.Flags.Carry
                
        | Instruction.Jsr ->   // JSR addr: MEM(addr) <- PC, PC <- addr + 1
            cpu.Registers.ProgramCounter |> MemoryWriteByte cpu.Memory (int cpu.Registers.InstructionRegister.OperandAddress)
            cpu.Registers.ProgramCounter <- cpu.Registers.InstructionRegister.OperandAddress + 1uy
                
        | Instruction.Neg ->   // NEG r 
            int ((~~~registerValue) + 1uy) |> writeRegisterAndCarry (fun x -> registerValue = 0uy)

        | Instruction.Shr ->   // SHR r
            int (registerValue >>> 1) |> writeRegisterAndCarry (fun x -> (registerValue &&& 1uy) <> 0uy)

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
