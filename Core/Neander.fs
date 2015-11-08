namespace Ufrgs.Inf.ArchSims.Core

open LanguagePrimitives

open Ufrgs.Inf.ArchSims.Core.Memory

module Neander =

    type Instruction = 
    | Nop = 0x00uy // 0000 0000
    | Sta = 0x10uy // 0001 0000
    | Lda = 0x20uy // 0010 0000
    | Add = 0x30uy // 0011 0000
    | Or  = 0x40uy // 0100 0000
    | And = 0x50uy // 0101 0000
    | Not = 0x60uy // 0110 0000
    | Jmp = 0x80uy // 1000 0000
    | Jn  = 0x90uy // 1001 0000
    | Jz  = 0xA0uy // 1010 0000
    | Hlt = 0xF0uy // 1111 0000

    type InstructionRegister = {
        mutable OpCode: byte
        mutable OperandAddress: byte
    }

    type Flags = {
        mutable Halted: bool
        mutable Negative: bool
        mutable Zero: bool
    }

    type Registers = {
        mutable ProgramCounter: byte
        mutable Accumulator: byte
        
        InstructionRegister: InstructionRegister
        Flags: Flags
    }

    let CreateRegisters() = {
        ProgramCounter = 0uy
        Accumulator = 0uy
        InstructionRegister = { OpCode = 0uy; OperandAddress = 0uy }
        Flags = { Halted = false; Negative = false; Zero = true }
    }

    let RegistersReset registers = 
        registers.ProgramCounter <- 0uy
        registers.Accumulator <- 0uy
        registers.InstructionRegister.OpCode <- 0uy
        registers.InstructionRegister.OperandAddress <- 0uy            
        registers.Flags.Halted <- false
        registers.Flags.Negative <- false
        registers.Flags.Zero <- true
    
    type Cpu = {
        Registers: Registers
        Memory: Memory
    }

    let CreateCpu() = {
        Registers = CreateRegisters()
        Memory = CreateMemory 256
    }

    let Fetch cpu = 
        let readByteFromProgramCounterAndAdvance() =
            let result = MemoryReadByte cpu.Memory (int cpu.Registers.ProgramCounter)
            cpu.Registers.ProgramCounter <- cpu.Registers.ProgramCounter + 1uy            
            result

        cpu.Registers.InstructionRegister.OpCode <- readByteFromProgramCounterAndAdvance()
        cpu.Registers.InstructionRegister.OperandAddress <- 
            match EnumOfValue cpu.Registers.InstructionRegister.OpCode with
            | Instruction.Sta
            | Instruction.Lda
            | Instruction.Add
            | Instruction.Or
            | Instruction.And
            | Instruction.Jmp
            | Instruction.Jn
            | Instruction.Jz -> readByteFromProgramCounterAndAdvance()  // Instructions with operand
            | _ -> 0uy // Instructions without operand

    let Execute cpu =
        let readOperand() =
            MemoryReadByte cpu.Memory (int cpu.Registers.InstructionRegister.OperandAddress)

        let writeAccumulator value = 
            cpu.Registers.Accumulator <- value
            cpu.Registers.Flags.Zero <- value = 0uy
            cpu.Registers.Flags.Negative <- value > 0x7Fuy

        let jumpIf condition = 
            if condition then
                cpu.Registers.ProgramCounter <- cpu.Registers.InstructionRegister.OperandAddress                

        let instruction = EnumOfValue cpu.Registers.InstructionRegister.OpCode
        match instruction with
        | Instruction.Sta ->   // STA addr: MEM(addr) <- AC
            cpu.Registers.Accumulator |> MemoryWriteByte cpu.Memory (int cpu.Registers.InstructionRegister.OperandAddress) 

        | Instruction.Lda ->   // LDA addr: AC <- MEM(addr)
            readOperand() |> writeAccumulator 

        | Instruction.Add ->   // ADD addr: AC <- AC + MEM(addr)
            cpu.Registers.Accumulator + readOperand() |> writeAccumulator

        | Instruction.Or ->    // OR addr: AC <- AC or MEM(addr) 
            cpu.Registers.Accumulator ||| readOperand() |> writeAccumulator

        | Instruction.And ->   // AND addr: AC <- AC and MEM(addr) 
            cpu.Registers.Accumulator &&& readOperand() |> writeAccumulator

        | Instruction.Not ->   // NOT: AC <- not AC
            ~~~cpu.Registers.Accumulator |> writeAccumulator

        | Instruction.Jmp ->   // JMP addr: PC <- addr
            jumpIf true
                    
        | Instruction.Jn ->    // JN addr: IF N=1 PC <- addr
            jumpIf cpu.Registers.Flags.Negative

        | Instruction.Jz ->    // JZ addr: IF Z=1 PC <- addr
            jumpIf cpu.Registers.Flags.Zero
                
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
