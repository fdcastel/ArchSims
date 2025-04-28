import {
    Memory,
    createMemory, // Although defined in memory.ts, it's used by createNeanderCpu
    memoryReadByte,
    memoryWriteByte,
    memoryReset,
} from "./memory";

// Corresponds to F# Neander.Instruction
export enum NeanderInstruction {
    Nop = 0x00, // 0000 0000
    Sta = 0x10, // 0001 0000
    Lda = 0x20, // 0010 0000
    Add = 0x30, // 0011 0000
    Or  = 0x40, // 0100 0000
    And = 0x50, // 0101 0000
    Not = 0x60, // 0110 0000
    Jmp = 0x80, // 1000 0000
    Jn  = 0x90, // 1001 0000
    Jz  = 0xA0, // 1010 0000
    Hlt = 0xF0, // 1111 0000
}

// Corresponds to F# Neander.InstructionRegister
export interface NeanderInstructionRegister {
    opCode: number; // byte
    operandAddress: number; // byte
}

// Corresponds to F# Neander.Flags
export interface NeanderFlags {
    halted: boolean;
    negative: boolean;
    zero: boolean;
}

// Corresponds to F# Neander.Registers
export interface NeanderRegisters {
    programCounter: number; // byte
    accumulator: number; // byte
    instructionRegister: NeanderInstructionRegister;
    flags: NeanderFlags;
}

// Corresponds to F# Neander.Cpu
export interface NeanderCpu {
    registers: NeanderRegisters;
    memory: Memory;
}

// Corresponds to F# Neander.CreateRegisters
export function createNeanderRegisters(): NeanderRegisters {
    return {
        programCounter: 0,
        accumulator: 0,
        instructionRegister: { opCode: NeanderInstruction.Nop, operandAddress: 0 }, // Start with NOP
        flags: { halted: false, negative: false, zero: true },
    };
}

// Corresponds to F# Neander.RegistersReset
export function neanderRegistersReset(registers: NeanderRegisters): void {
    registers.programCounter = 0;
    registers.accumulator = 0;
    registers.instructionRegister.opCode = NeanderInstruction.Nop;
    registers.instructionRegister.operandAddress = 0;
    registers.flags.halted = false;
    registers.flags.negative = false;
    registers.flags.zero = true;
}

// Corresponds to F# Neander.CreateCpu
export function createNeanderCpu(): NeanderCpu {
    return {
        registers: createNeanderRegisters(),
        memory: createMemory(256), // Neander has 256 bytes of memory
    };
}

// Corresponds to F# Neander.Fetch
function neanderFetch(cpu: NeanderCpu): void {
    const readByteFromProgramCounterAndAdvance = (): number => {
        const result = memoryReadByte(cpu.memory, cpu.registers.programCounter);
        // Ensure PC wraps around at 256 for 8-bit behavior
        cpu.registers.programCounter = (cpu.registers.programCounter + 1) & 0xFF;
        return result;
    };

    cpu.registers.instructionRegister.opCode = readByteFromProgramCounterAndAdvance();

    // Determine if the instruction needs an operand address
    switch (cpu.registers.instructionRegister.opCode) {
        case NeanderInstruction.Sta:
        case NeanderInstruction.Lda:
        case NeanderInstruction.Add:
        case NeanderInstruction.Or:
        case NeanderInstruction.And:
        case NeanderInstruction.Jmp:
        case NeanderInstruction.Jn:
        case NeanderInstruction.Jz:
            cpu.registers.instructionRegister.operandAddress = readByteFromProgramCounterAndAdvance();
            break;
        default:
            cpu.registers.instructionRegister.operandAddress = 0; // No operand needed
            break;
    }
}

// Corresponds to F# Neander.Execute
function neanderExecute(cpu: NeanderCpu): void {
    const readOperand = (): number => {
        return memoryReadByte(cpu.memory, cpu.registers.instructionRegister.operandAddress);
    };

    const writeAccumulator = (value: number): void => {
        // Ensure value stays within 8 bits
        const byteValue = value & 0xFF;
        cpu.registers.accumulator = byteValue;
        cpu.registers.flags.zero = byteValue === 0;
        // Check if the most significant bit (bit 7) is set for negativity
        cpu.registers.flags.negative = (byteValue & 0x80) !== 0;
    };

    const jumpIf = (condition: boolean): void => {
        if (condition) {
            cpu.registers.programCounter = cpu.registers.instructionRegister.operandAddress;
        }
    };

    const instruction: NeanderInstruction = cpu.registers.instructionRegister.opCode;

    switch (instruction) {
        case NeanderInstruction.Sta: // STA addr: MEM(addr) <- AC
            memoryWriteByte(cpu.memory, cpu.registers.instructionRegister.operandAddress, cpu.registers.accumulator);
            break;

        case NeanderInstruction.Lda: // LDA addr: AC <- MEM(addr)
            writeAccumulator(readOperand());
            break;

        case NeanderInstruction.Add: // ADD addr: AC <- AC + MEM(addr)
            // Perform addition and keep it within 8 bits
            writeAccumulator(cpu.registers.accumulator + readOperand());
            break;

        case NeanderInstruction.Or: // OR addr: AC <- AC or MEM(addr)
            writeAccumulator(cpu.registers.accumulator | readOperand());
            break;

        case NeanderInstruction.And: // AND addr: AC <- AC and MEM(addr)
            writeAccumulator(cpu.registers.accumulator & readOperand());
            break;

        case NeanderInstruction.Not: // NOT: AC <- not AC
            // Bitwise NOT, ensure result is treated as 8-bit unsigned
            writeAccumulator(~cpu.registers.accumulator);
            break;

        case NeanderInstruction.Jmp: // JMP addr: PC <- addr
            jumpIf(true);
            break;

        case NeanderInstruction.Jn: // JN addr: IF N=1 PC <- addr
            jumpIf(cpu.registers.flags.negative);
            break;

        case NeanderInstruction.Jz: // JZ addr: IF Z=1 PC <- addr
            jumpIf(cpu.registers.flags.zero);
            break;

        case NeanderInstruction.Hlt: // HLT: Set halted flag
            // Handled after the switch
            break;

        case NeanderInstruction.Nop: // NOP or unknown instruction: nothing to do
        default:
            // Do nothing for NOP or any potentially unrecognized opcode
            break;
    }

    // Update halted flag only if HLT instruction was executed
    if (instruction === NeanderInstruction.Hlt) {
         cpu.registers.flags.halted = true;
    }
}

// --- Execute Cycle ---
export function neanderStep(cpu: NeanderCpu): void {
    // F# version does not check halted flag - we match that behavior here
    neanderFetch(cpu);
    neanderExecute(cpu);
}

// Corresponds to F# Neander.Reset
export function neanderReset(cpu: NeanderCpu): void {
    neanderRegistersReset(cpu.registers);
    memoryReset(cpu.memory);
}
