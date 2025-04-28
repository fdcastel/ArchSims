// ts/archsims-core/src/ramses.ts

import { Memory, createMemory, memoryReadByte, memoryWriteByte, memoryReset } from './memory';

// --- Constants ---

export enum RamsesInstruction {
    Nop = 0x00, // 0000 0000
    Str = 0x10, // 0001 0000 (Store Register)
    Ldr = 0x20, // 0010 0000 (Load Register)
    Add = 0x30, // 0011 0000 (Add to Register)
    Or  = 0x40, // 0100 0000 (OR with Register)
    And = 0x50, // 0101 0000 (AND with Register)
    Not = 0x60, // 0110 0000 (NOT Register)
    Sub = 0x70, // 0111 0000 (Subtract from Register)
    Jmp = 0x80, // 1000 0000 (Jump Unconditional)
    Jn  = 0x90, // 1001 0000 (Jump if Negative)
    Jz  = 0xA0, // 1010 0000 (Jump if Zero)
    Jc  = 0xB0, // 1011 0000 (Jump if Carry)
    Jsr = 0xC0, // 1100 0000 (Jump to Subroutine)
    Neg = 0xD0, // 1101 0000 (Negate Register)
    Shr = 0xE0, // 1110 0000 (Shift Right Register)
    Hlt = 0xF0, // 1111 0000 (Halt)
}

export enum RamsesRegister {
    Ra = 0x00, // 0000 0000 (Register A)
    Rb = 0x04, // 0000 0100 (Register B)
    Rx = 0x08, // 0000 1000 (Index Register X)
    Pc = 0x0C, // 0000 1100 (Program Counter - Not directly addressable in instructions, but used internally)
}

export enum RamsesAddressMode {
    Direct    = 0x00, // 0000 0000 (Operand is address)
    Indirect  = 0x01, // 0000 0001 (Operand is address containing address)
    Immediate = 0x02, // 0000 0010 (Operand is value itself)
    Indexed   = 0x03, // 0000 0011 (Operand is address + Rx)
}

export const RAMSES_INSTRUCTION_MASK = 0b11110000; // 0xF0
export const RAMSES_REGISTER_MASK    = 0b00001100; // 0x0C
export const RAMSES_ADDRESS_MODE_MASK = 0b00000011; // 0x03

// --- Types ---

export interface RamsesInstructionRegister {
    opCode: number; // Full byte including instruction, register, and mode
    operandAddress: number; // Calculated effective address or immediate value address
}

export interface RamsesFlags {
    halted: boolean;
    negative: boolean; // Result is negative (MSB set)
    zero: boolean;     // Result is zero
    carry: boolean;    // Carry out from arithmetic/shift operation
}

export interface RamsesRegisters {
    ra: number; // Register A (8-bit)
    rb: number; // Register B (8-bit)
    rx: number; // Index Register X (8-bit)
    pc: number; // Program Counter (8-bit)
    instructionRegister: RamsesInstructionRegister;
    flags: RamsesFlags;
}

export interface RamsesCpu {
    registers: RamsesRegisters;
    memory: Memory;
}

// --- Functions ---

export function createRamsesRegisters(): RamsesRegisters {
    return {
        ra: 0,
        rb: 0,
        rx: 0,
        pc: 0,
        instructionRegister: { opCode: 0, operandAddress: 0 },
        flags: { halted: false, negative: false, zero: true, carry: false },
    };
}

export function ramsesRegistersReset(registers: RamsesRegisters): void {
    registers.ra = 0;
    registers.rb = 0;
    registers.rx = 0;
    registers.pc = 0;
    registers.instructionRegister.opCode = 0;
    registers.instructionRegister.operandAddress = 0;
    registers.flags.halted = false;
    registers.flags.negative = false;
    registers.flags.zero = true;
    registers.flags.carry = false;
}

export function createRamsesCpu(): RamsesCpu {
    return {
        registers: createRamsesRegisters(),
        memory: createMemory(256), // Ramses has 256 bytes of memory
    };
}

export function ramsesReset(cpu: RamsesCpu): void {
    ramsesRegistersReset(cpu.registers);
    memoryReset(cpu.memory);
}

// --- Fetch Cycle ---
export function ramsesFetch(cpu: RamsesCpu): void {
    const regs = cpu.registers;
    const mem = cpu.memory;
    const ir = regs.instructionRegister;

    function readPcByteAndAdvance(): number {
        const byte = memoryReadByte(mem, regs.pc);
        regs.pc = (regs.pc + 1) & 0xFF; // Increment PC (8-bit wrap)
        return byte;
    }

    ir.opCode = readPcByteAndAdvance();

    const instructionBase = ir.opCode & RAMSES_INSTRUCTION_MASK;
    const addressMode = ir.opCode & RAMSES_ADDRESS_MODE_MASK;

    // Determine if instruction uses an operand and calculate effective address
    // This implementation follows the F# version's memory access pattern
    switch (instructionBase) {
        // Instructions *with* an operand field
        case RamsesInstruction.Str:
        case RamsesInstruction.Ldr:
        case RamsesInstruction.Add:
        case RamsesInstruction.Or:
        case RamsesInstruction.And:
        case RamsesInstruction.Sub:
        case RamsesInstruction.Jmp:
        case RamsesInstruction.Jn:
        case RamsesInstruction.Jz:
        case RamsesInstruction.Jc:
        case RamsesInstruction.Jsr:
            {
                // Read the byte following the opcode
                const operandByte = readPcByteAndAdvance();
                
                switch (addressMode) {
                    case RamsesAddressMode.Direct: // Operand is the address
                        ir.operandAddress = operandByte;
                        break;
                        
                    case RamsesAddressMode.Indirect: // Operand is address containing the address
                        // F# version reads from memory here
                        ir.operandAddress = memoryReadByte(mem, operandByte);
                        break;
                        
                    case RamsesAddressMode.Immediate: // Operand is the value itself
                        // For jumps, the operand byte is the target address
                        if (instructionBase >= RamsesInstruction.Jmp && instructionBase <= RamsesInstruction.Jsr) {
                            ir.operandAddress = operandByte;
                        } else {
                            // For non-jumps with immediate mode, the operand value is at PC-1
                            ir.operandAddress = regs.pc - 1;
                            // This matches the F# implementation behavior - we don't do any additional PC adjustment
                        }
                        break;
                        
                    case RamsesAddressMode.Indexed: // Operand is address + Rx
                        ir.operandAddress = (regs.rx + operandByte) & 0xFF;
                        break;
                        
                    default:
                        console.error(`Invalid address mode: ${addressMode}`);
                        ir.operandAddress = 0;
                        break;
                }
            }
            break;

        // Instructions *without* an operand field (NOT, NEG, SHR, HLT, NOP)
        case RamsesInstruction.Not:
        case RamsesInstruction.Neg:
        case RamsesInstruction.Shr:
        case RamsesInstruction.Hlt:
        case RamsesInstruction.Nop:
        default: // Includes NOP and potential unknowns
            ir.operandAddress = 0; // Not used
            break;
    }
}

// --- Execute Cycle ---
export function ramsesExecute(cpu: RamsesCpu): void {
    const regs = cpu.registers;
    const mem = cpu.memory;
    const ir = regs.instructionRegister;
    const flags = regs.flags;

    // F# does not check halted flag here - removed the check to match F# behavior

    const instructionBase = ir.opCode & RAMSES_INSTRUCTION_MASK;
    const registerSelect = ir.opCode & RAMSES_REGISTER_MASK;

    // Helper to read operand value based on calculated operandAddress
    function readOperandValue(): number {
        return memoryReadByte(mem, ir.operandAddress);
    }

    // Helper to get the value of the selected register (A, B, or X)
    function getRegisterValue(): number {
        switch (registerSelect) {
            case RamsesRegister.Ra: return regs.ra;
            case RamsesRegister.Rb: return regs.rb;
            case RamsesRegister.Rx: return regs.rx;
            // case RamsesRegister.Pc: // PC not directly usable in most instructions
            default:
                console.warn(`Invalid register selection in opcode: 0x${ir.opCode.toString(16)}`);
                return 0; // Should not happen
        }
    }

    // Helper to write a value to the selected register and update N/Z flags
    function writeRegisterValue(value: number): void {
        const byteValue = value & 0xFF;
        switch (registerSelect) {
            case RamsesRegister.Ra: regs.ra = byteValue; break;
            case RamsesRegister.Rb: regs.rb = byteValue; break;
            case RamsesRegister.Rx: regs.rx = byteValue; break;
            // case RamsesRegister.Pc: // PC not directly writable this way
            default:
                console.warn(`Invalid register selection in opcode: 0x${ir.opCode.toString(16)}`);
                break;
        }
        // Update flags based on the written value
        flags.zero = byteValue === 0;
        flags.negative = (byteValue & 0x80) !== 0; // Check MSB
    }

    // Helper for arithmetic operations that update Carry flag
    function writeRegisterAndCarry(value: number, carry: boolean): void {
        writeRegisterValue(value);
        flags.carry = carry;
    }

    // Helper for jump instructions
    function jumpIf(condition: boolean): void {
        if (condition) {
            regs.pc = ir.operandAddress;
        }
    }

    // --- Instruction Execution ---
    let operand = 0;
    let regValue = 0;
    let result = 0;

    switch (instructionBase) {
        case RamsesInstruction.Nop:
            // Do nothing
            break;

        case RamsesInstruction.Str: // STR r, oper
            regValue = getRegisterValue();
            memoryWriteByte(mem, ir.operandAddress, regValue);
            break;

        case RamsesInstruction.Ldr: // LDR r, oper
            operand = readOperandValue();
            writeRegisterValue(operand);
            break;

        case RamsesInstruction.Add: // ADD r, oper
            regValue = getRegisterValue();
            operand = readOperandValue();
            result = regValue + operand;
            writeRegisterAndCarry(result, result > 0xFF);
            break;

        case RamsesInstruction.Or: // OR r, oper
            regValue = getRegisterValue();
            operand = readOperandValue();
            result = regValue | operand;
            writeRegisterValue(result); // Carry unaffected
            break;

        case RamsesInstruction.And: // AND r, oper
            regValue = getRegisterValue();
            operand = readOperandValue();
            result = regValue & operand;
            writeRegisterValue(result); // Carry unaffected
            break;

        case RamsesInstruction.Not: // NOT r
            regValue = getRegisterValue();
            result = ~regValue;
            writeRegisterValue(result); // Carry unaffected
            break;

        case RamsesInstruction.Sub: // SUB r, oper (r = r - oper)
            regValue = getRegisterValue();
            operand = readOperandValue();
            // F# implementation treats carry as borrow (set if operand > regValue)
            // and simply sets the inverse of carry after addition
            result = regValue - operand;
            writeRegisterValue(result);
            // F# sets carry flag if a borrow occurs (operand > regValue)
            flags.carry = operand > regValue;
            break;

        case RamsesInstruction.Jmp: // JMP oper
            jumpIf(true);
            break;

        case RamsesInstruction.Jn: // JN oper
            jumpIf(flags.negative);
            break;

        case RamsesInstruction.Jz: // JZ oper
            jumpIf(flags.zero);
            break;

        case RamsesInstruction.Jc: // JC oper
            jumpIf(flags.carry);
            break;

        case RamsesInstruction.Jsr: // JSR oper
            // Store return address (PC after JSR instruction) at operand address
            memoryWriteByte(mem, ir.operandAddress, regs.pc);
            // Jump to address *after* the stored return address
            regs.pc = (ir.operandAddress + 1) & 0xFF;
            break;

        case RamsesInstruction.Neg: // NEG r (r = -r)
            regValue = getRegisterValue();
            result = -regValue; // Native 2's complement negation
            // Carry is set if original value was 0 (F# logic: `registerValue = 0uy`)
            writeRegisterAndCarry(result, regValue === 0);
            break;

        case RamsesInstruction.Shr: // SHR r (r = r >>> 1)
            regValue = getRegisterValue();
            const lsb = regValue & 1; // Capture LSB for carry
            result = regValue >>> 1; // Unsigned right shift
            writeRegisterAndCarry(result, lsb !== 0);
            break;

        case RamsesInstruction.Hlt: // HLT
            flags.halted = true;
            break;

        default:
            // Unknown instruction encountered during execute
            console.error(`Unknown instruction executed: 0x${ir.opCode.toString(16)}`);
            flags.halted = true; // Halt on unknown instruction
            break;
    }
}

// --- Step Function ---
export function ramsesStep(cpu: RamsesCpu): void {
    // F# version does not check halted flag - we match that behavior here
    ramsesFetch(cpu);
    ramsesExecute(cpu);
}

// --- Disassembly ---
export function ramsesDisassembleInstruction(content: number[]): [string, number] {
    if (content.length === 0) return ["", 0];

    const opCode = content[0];
    const instructionBase = opCode & RAMSES_INSTRUCTION_MASK;
    const registerSelect = opCode & RAMSES_REGISTER_MASK;
    const addressMode = opCode & RAMSES_ADDRESS_MODE_MASK;

    let mnemonic = "???";
    let operandStr = "";
    let size = 1; // Default size for 1-byte instructions

    function getRegStr(): string {
        switch (registerSelect) {
            case RamsesRegister.Ra: return "A";
            case RamsesRegister.Rb: return "B";
            case RamsesRegister.Rx: return "X";
            case RamsesRegister.Pc: return "PC"; // Should not appear often
            default: return "?";
        }
    }

    function getOperandStr(): string {
        if (content.length < 2) return "??"; // Missing operand byte
        const operandByte = content[1];
        switch (addressMode) {
            case RamsesAddressMode.Direct: return `${operandByte}`;
            case RamsesAddressMode.Indirect: return `${operandByte},I`;
            case RamsesAddressMode.Immediate: return `#${operandByte}`;
            case RamsesAddressMode.Indexed: return `${operandByte},X`;
            default: return `${operandByte},?`;
        }
    }

    switch (instructionBase) {
        case RamsesInstruction.Nop: mnemonic = "NOP"; break;
        case RamsesInstruction.Hlt: mnemonic = "HLT"; break;
        case RamsesInstruction.Not: mnemonic = "NOT"; operandStr = getRegStr(); break;
        case RamsesInstruction.Neg: mnemonic = "NEG"; operandStr = getRegStr(); break;
        case RamsesInstruction.Shr: mnemonic = "SHR"; operandStr = getRegStr(); break;

        // Instructions with register and operand
        case RamsesInstruction.Str: mnemonic = "STR"; operandStr = `${getRegStr()}, ${getOperandStr()}`; size = 2; break;
        case RamsesInstruction.Ldr: mnemonic = "LDR"; operandStr = `${getRegStr()}, ${getOperandStr()}`; size = 2; break;
        case RamsesInstruction.Add: mnemonic = "ADD"; operandStr = `${getRegStr()}, ${getOperandStr()}`; size = 2; break;
        case RamsesInstruction.Or:  mnemonic = "OR "; operandStr = `${getRegStr()}, ${getOperandStr()}`; size = 2; break; // Note space for alignment
        case RamsesInstruction.And: mnemonic = "AND"; operandStr = `${getRegStr()}, ${getOperandStr()}`; size = 2; break;
        case RamsesInstruction.Sub: mnemonic = "SUB"; operandStr = `${getRegStr()}, ${getOperandStr()}`; size = 2; break;

        // Instructions with operand only
        case RamsesInstruction.Jmp: mnemonic = "JMP"; operandStr = getOperandStr(); size = 2; break;
        case RamsesInstruction.Jn:  mnemonic = "JN "; operandStr = getOperandStr(); size = 2; break; // Note space
        case RamsesInstruction.Jz:  mnemonic = "JZ "; operandStr = getOperandStr(); size = 2; break; // Note space
        case RamsesInstruction.Jc:  mnemonic = "JC "; operandStr = getOperandStr(); size = 2; break; // Note space
        case RamsesInstruction.Jsr: mnemonic = "JSR"; operandStr = getOperandStr(); size = 2; break;

        default:
             mnemonic = `DB 0x${opCode.toString(16).padStart(2,'0')}`; // Treat unknown as data byte
             break;
    }

    // Combine mnemonic and operand string, ensuring alignment similar to F# output
    const fullMnemonic = `${mnemonic}${operandStr ? ' ' + operandStr : ''}`.padEnd(10); // Pad for alignment

    return [fullMnemonic.trimEnd(), size]; // Trim trailing space if no operand
}

export function ramsesDisassembleInstructions(content: number[]): [string, number][] {
     const results: [string, number][] = [];
     let index = 0;
     while (index < content.length) {
         const [text, size] = ramsesDisassembleInstruction(content.slice(index));
         if (size === 0) break; // Should not happen with valid input
         results.push([text, size]);
         index += size;
     }
     return results;
}
