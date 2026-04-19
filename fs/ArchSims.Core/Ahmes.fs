namespace Ufrgs.Inf.ArchSims.Core

open LanguagePrimitives

open Ufrgs.Inf.ArchSims.Core.Memory

module Ahmes =

    type Instruction =
    | Nop = 0x00uy // 0000 0000
    | Sta = 0x10uy // 0001 0000
    | Lda = 0x20uy // 0010 0000
    | Add = 0x30uy // 0011 0000
    | Or  = 0x40uy // 0100 0000
    | And = 0x50uy // 0101 0000
    | Not = 0x60uy // 0110 0000
    | Sub = 0x70uy // 0111 0000
    | Jmp = 0x80uy // 1000 0000
    | Jn  = 0x90uy // 1001 0000
    | Jp  = 0x94uy // 1001 0100
    | Jv  = 0x98uy // 1001 1000
    | Jnv = 0x9Cuy // 1001 1100
    | Jz  = 0xA0uy // 1010 0000
    | Jnz = 0xA4uy // 1010 0100
    | Jc  = 0xB0uy // 1011 0000
    | Jnc = 0xB4uy // 1011 0100
    | Jb  = 0xB8uy // 1011 1000
    | Jnb = 0xBCuy // 1011 1100
    | Shr = 0xE0uy // 1110 0000
    | Shl = 0xE1uy // 1110 0001
    | Ror = 0xE2uy // 1110 0010
    | Rol = 0xE3uy // 1110 0011
    | Hlt = 0xF0uy // 1111 0000

    type InstructionRegister = {
        mutable OpCode: byte
        mutable OperandAddress: byte
    }

    type Flags = {
        mutable Halted: bool
        mutable Negative: bool
        mutable Zero: bool
        mutable Carry: bool
        mutable Overflow: bool
        mutable Borrow: bool
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
        Flags = { Halted = false; Negative = false; Zero = true; Carry = false; Overflow = false; Borrow = false }
    }

    let RegistersReset registers =
        registers.ProgramCounter <- 0uy
        registers.Accumulator <- 0uy
        registers.InstructionRegister.OpCode <- 0uy
        registers.InstructionRegister.OperandAddress <- 0uy
        registers.Flags.Halted <- false
        registers.Flags.Negative <- false
        registers.Flags.Zero <- true
        registers.Flags.Carry <- false
        registers.Flags.Overflow <- false
        registers.Flags.Borrow <- false

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
            | Instruction.Sub
            | Instruction.Jmp
            | Instruction.Jn
            | Instruction.Jp
            | Instruction.Jv
            | Instruction.Jnv
            | Instruction.Jz
            | Instruction.Jnz
            | Instruction.Jc
            | Instruction.Jnc
            | Instruction.Jb
            | Instruction.Jnb -> readByteFromProgramCounterAndAdvance()  // Instructions with operand
            | _ -> 0uy // Instructions without operand

    let Execute cpu =
        let readOperand() =
            MemoryReadByte cpu.Memory (int cpu.Registers.InstructionRegister.OperandAddress)

        let isNegativeByte (value: byte) = value > 0x7Fuy

        let writeAccumulator value =
            cpu.Registers.Accumulator <- value
            cpu.Registers.Flags.Zero <- value = 0uy
            cpu.Registers.Flags.Negative <- isNegativeByte value

        let addAndUpdateFlags (a: byte) (b: byte) =
            let full = int a + int b
            cpu.Registers.Flags.Carry <- full > 0xFF
            let result = byte full
            let signA = isNegativeByte a
            let signB = isNegativeByte b
            let signR = isNegativeByte result
            cpu.Registers.Flags.Overflow <- (signA = signB) && (signA <> signR)
            result

        let subAndUpdateFlags (a: byte) (b: byte) =
            let full = int a - int b
            cpu.Registers.Flags.Borrow <- full < 0
            let result = byte (full &&& 0xFF)
            let signA = isNegativeByte a
            let signB = isNegativeByte b
            let signR = isNegativeByte result
            cpu.Registers.Flags.Overflow <- (signA <> signB) && (signA <> signR)
            result

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
            addAndUpdateFlags cpu.Registers.Accumulator (readOperand()) |> writeAccumulator

        | Instruction.Or ->    // OR addr: AC <- AC or MEM(addr)
            cpu.Registers.Accumulator ||| readOperand() |> writeAccumulator

        | Instruction.And ->   // AND addr: AC <- AC and MEM(addr)
            cpu.Registers.Accumulator &&& readOperand() |> writeAccumulator

        | Instruction.Not ->   // NOT: AC <- not AC
            ~~~cpu.Registers.Accumulator |> writeAccumulator

        | Instruction.Sub ->   // SUB addr: AC <- AC - MEM(addr)
            subAndUpdateFlags cpu.Registers.Accumulator (readOperand()) |> writeAccumulator

        | Instruction.Jmp -> jumpIf true                                            // JMP addr
        | Instruction.Jn  -> jumpIf cpu.Registers.Flags.Negative                    // JN  addr: IF N=1
        | Instruction.Jp  -> jumpIf (not cpu.Registers.Flags.Negative)              // JP  addr: IF N=0
        | Instruction.Jv  -> jumpIf cpu.Registers.Flags.Overflow                    // JV  addr: IF V=1
        | Instruction.Jnv -> jumpIf (not cpu.Registers.Flags.Overflow)              // JNV addr: IF V=0
        | Instruction.Jz  -> jumpIf cpu.Registers.Flags.Zero                        // JZ  addr: IF Z=1
        | Instruction.Jnz -> jumpIf (not cpu.Registers.Flags.Zero)                  // JNZ addr: IF Z=0
        | Instruction.Jc  -> jumpIf cpu.Registers.Flags.Carry                       // JC  addr: IF C=1
        | Instruction.Jnc -> jumpIf (not cpu.Registers.Flags.Carry)                 // JNC addr: IF C=0
        | Instruction.Jb  -> jumpIf cpu.Registers.Flags.Borrow                      // JB  addr: IF B=1
        | Instruction.Jnb -> jumpIf (not cpu.Registers.Flags.Borrow)                // JNB addr: IF B=0

        | Instruction.Shr ->   // SHR: Logical shift right; C <- AC[0]; AC <- AC >> 1; AC[7] <- 0
            let ac = cpu.Registers.Accumulator
            cpu.Registers.Flags.Carry <- (ac &&& 0x01uy) <> 0uy
            writeAccumulator (ac >>> 1)

        | Instruction.Shl ->   // SHL: Logical shift left; C <- AC[7]; AC <- AC << 1; AC[0] <- 0
            let ac = cpu.Registers.Accumulator
            cpu.Registers.Flags.Carry <- (ac &&& 0x80uy) <> 0uy
            writeAccumulator (ac <<< 1)

        | Instruction.Ror ->   // ROR: Rotate right through carry
            let ac = cpu.Registers.Accumulator
            let newHighBit = if cpu.Registers.Flags.Carry then 0x80uy else 0x00uy
            cpu.Registers.Flags.Carry <- (ac &&& 0x01uy) <> 0uy
            writeAccumulator ((ac >>> 1) ||| newHighBit)

        | Instruction.Rol ->   // ROL: Rotate left through carry
            let ac = cpu.Registers.Accumulator
            let newLowBit = if cpu.Registers.Flags.Carry then 0x01uy else 0x00uy
            cpu.Registers.Flags.Carry <- (ac &&& 0x80uy) <> 0uy
            writeAccumulator ((ac <<< 1) ||| newLowBit)

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
