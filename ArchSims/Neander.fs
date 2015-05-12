namespace Ufrgs.Inf.ArchSims

open Ufrgs.Inf.ArchSims.Common

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
        mutable OpCode: byte;
        mutable OperandAddress: byte
    }

    type Flags = {
        mutable Negative: bool
        mutable Zero: bool
    }

    type Registers = {
        mutable ProgramCounter: byte;
        mutable Accumulator: byte;
        
        InstructionRegister: InstructionRegister
        Flags: Flags
    }

    let CreateRegisters() = {
        ProgramCounter = 0uy
        Accumulator = 0uy
        InstructionRegister = { OpCode = 0uy; OperandAddress = 0uy }
        Flags = { Negative = false; Zero = true }
    }

    let ClearRegisters registers = 
        registers.ProgramCounter <- 0uy
        registers.Accumulator <- 0uy
        registers.InstructionRegister.OpCode <- 0uy
        registers.InstructionRegister.OperandAddress <- 0uy            
        registers.Flags.Negative <- false
        registers.Flags.Zero <- true
    
    type Cpu() =
        member val public Registers = CreateRegisters() with get, set
        member val public Memory = CreateMemory 256 with get
        member val public Debugger = CreateDebugger() with get, set

        member this.Fetch() =
            let readByteFromProgramCounterAndAdvance() =
                let result = int this.Registers.ProgramCounter |> ReadByte this.Memory
                this.Registers.ProgramCounter <- this.Registers.ProgramCounter + 1uy            
                result

            this.Registers.InstructionRegister.OpCode <- readByteFromProgramCounterAndAdvance()
            this.Registers.InstructionRegister.OperandAddress <- 
                match LanguagePrimitives.EnumOfValue this.Registers.InstructionRegister.OpCode with
                | Instruction.Sta
                | Instruction.Lda
                | Instruction.Add
                | Instruction.Or
                | Instruction.And
                | Instruction.Jmp
                | Instruction.Jn
                | Instruction.Jz -> readByteFromProgramCounterAndAdvance()  // Instructions with operand
                | _ -> 0uy // Instructions without operand

        member this.Execute() =
            let readOperand() =
                int this.Registers.InstructionRegister.OperandAddress |> ReadByte this.Memory

            let writeAccumulator value = 
                this.Registers.Accumulator <- value
                this.Registers.Flags.Zero <- value = 0uy
                this.Registers.Flags.Negative <- value > 0x7Fuy

            let jumpIf condition = 
                if condition then
                    this.Registers.ProgramCounter <- this.Registers.InstructionRegister.OperandAddress                

            match LanguagePrimitives.EnumOfValue this.Registers.InstructionRegister.OpCode with
            | Instruction.Sta ->   // STA addr: MEM(addr) <- AC
                this.Registers.Accumulator |> WriteByte this.Memory (int this.Registers.InstructionRegister.OperandAddress) 

            | Instruction.Lda ->   // LDA addr: AC <- MEM(addr)
                readOperand() |> writeAccumulator 

            | Instruction.Add ->   // ADD addr: AC <- AC + MEM(addr)
                this.Registers.Accumulator + readOperand() |> writeAccumulator

            | Instruction.Or ->    // OR addr: AC <- AC or MEM(addr) 
                this.Registers.Accumulator ||| readOperand() |> writeAccumulator

            | Instruction.And ->   // AND addr: AC <- AC and MEM(addr) 
                this.Registers.Accumulator &&& readOperand() |> writeAccumulator

            | Instruction.Not ->   // NOT: AC <- not AC
                ~~~this.Registers.Accumulator |> writeAccumulator

            | Instruction.Jmp ->   // JMP addr: PC <- addr
                jumpIf true
                    
            | Instruction.Jn ->    // JN addr: IF N=1 PC <- addr
                jumpIf this.Registers.Flags.Negative

            | Instruction.Jz ->    // JZ addr: IF Z=1 PC <- addr
                jumpIf this.Registers.Flags.Zero
                
            | Instruction.Hlt ->   // HLT
                this.Debugger.Halted <- true
                
            | Instruction.Nop      // NOP (or unknown instruction): nothing to do
            | _ -> ()

        member this.Step() =
            this.Debugger |> ClearDebuggerFlags 
            if this.Debugger.Breakpoints.Contains (int this.Registers.ProgramCounter) then
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
