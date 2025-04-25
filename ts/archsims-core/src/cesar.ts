// ts/archsims-core/src/cesar.ts

import { Memory, createMemory, memoryReadByte, memoryWriteByte, memoryReset, 
        memoryReadWordBigEndian, memoryWriteWordBigEndian } from './memory';

// --- Constants ---

export enum CesarInstruction {
    Nop = 0x00, // 0000 0000
    Ccc = 0x10, // 0001 0000
    Scc = 0x20, // 0010 0000

    Br  = 0x30, // 0011 0000
    Bne = 0x31, // 0011 0001
    Beq = 0x32, // 0011 0010
    Bpl = 0x33, // 0011 0011
    Bmi = 0x34, // 0011 0100
    Bvc = 0x35, // 0011 0101
    Bvs = 0x36, // 0011 0110
    Bcc = 0x37, // 0011 0111
    Bcs = 0x38, // 0011 1000
    Bge = 0x39, // 0011 1001
    Blt = 0x3A, // 0011 1010
    Bgt = 0x3B, // 0011 1011
    Ble = 0x3C, // 0011 1100
    Bhi = 0x3D, // 0011 1101
    Bls = 0x3E, // 0011 1110

    Jmp = 0x40, // 0100 0000
    Sob = 0x50, // 0101 0000
    Jsr = 0x60, // 0110 0000
    Rts = 0x70, // 0111 0000

    Clr = 0x80, // 1000 0000
    Not = 0x81, // 1000 0001
    Inc = 0x82, // 1000 0010
    Dec = 0x83, // 1000 0011
    Neg = 0x84, // 1000 0100
    Tst = 0x85, // 1000 0101
    Ror = 0x86, // 1000 0110
    Rol = 0x87, // 1000 0111
    Asr = 0x88, // 1000 1000
    Asl = 0x89, // 1000 1001
    Adc = 0x8A, // 1000 1010
    Sbc = 0x8B, // 1000 1011

    Mov = 0x90, // 1001 0000
    Add = 0xA0, // 1010 0000
    Sub = 0xB0, // 1011 0000
    Cmp = 0xC0, // 1100 0000
    And = 0xD0, // 1101 0000
    Or  = 0xE0, // 1110 0000

    Hlt = 0xF0, // 1111 0000
}

export enum CesarRegister {
    R0 = 0x00, // 0000 0000
    R1 = 0x01, // 0000 0001
    R2 = 0x02, // 0000 0010
    R3 = 0x03, // 0000 0011
    R4 = 0x04, // 0000 0100
    R5 = 0x05, // 0000 0101
    R6 = 0x06, // 0000 0110
    R7 = 0x07, // 0000 0111
}

export enum CesarAddressMode {
    Register           = 0b00000000,
    RegPostInc         = 0b00001000,
    RegPreDec          = 0b00010000,
    Indexed            = 0b00011000,
    RegisterIndirect   = 0b00100000,
    RegPostIncIndirect = 0b00101000,
    RegPreDecIndirect  = 0b00110000,
    IndexedIndirect    = 0b00111000,
}

export enum CesarFlag {
    Negative = 0b00001000,
    Zero     = 0b00000100,
    Overflow = 0b00000010,
    Carry    = 0b00000001,
}

// Special address modes and constants
export const CESAR_INDIRECT  = CesarAddressMode.RegisterIndirect;
export const CESAR_IMMEDIATE = CesarAddressMode.RegPostInc | CesarRegister.R7;
export const CESAR_DIRECT    = CESAR_IMMEDIATE | CESAR_INDIRECT;

// Masks
export const CESAR_INSTRUCTION_MASK    = 0b11110000;
export const CESAR_SUB_INSTRUCTION_MASK = 0b00001111;
export const CESAR_ADDRESS_MODE_MASK    = 0b00111000;
export const CESAR_REGISTER_MASK       = 0b00000111;

// Memory mapped I/O
export const KEYBOARD_MEMORY_ADDRESS = 0xFFDA; // Start of keyboard memory mapped area
export const DISPLAY_MEMORY_ADDRESS  = 0xFFDC; // Start of display memory mapped area

// --- Types ---

// Type for representing operation operands
export type Operand =
    | { type: 'NoOp' }
    | { type: 'Reg', register: CesarRegister }
    | { type: 'Addr', address: number };  // 16-bit address (0-65535)

// Flags interface
export interface CesarFlags {
    halted: boolean;
    negative: boolean;
    zero: boolean;
    overflow: boolean;
    carry: boolean;
}

// Instruction register interface
export interface CesarInstructionRegister {
    data: Uint8Array;
    sourceOperand: Operand;
    targetOperand: Operand;
}

// Registers interface
export interface CesarRegisters {
    r: Uint16Array;  // Array of 8 registers (R0-R7)
    instructionRegister: CesarInstructionRegister;
    flags: CesarFlags;
}

// CPU interface
export interface CesarCpu {
    registers: CesarRegisters;
    memory: Memory;
}

// --- Functions ---

/**
 * Creates a new set of Cesar CPU registers initialized to default values.
 * @returns A new CesarRegisters object
 */
export function createCesarRegisters(): CesarRegisters {
    return {
        r: new Uint16Array(8), // 8 registers initialized to 0
        instructionRegister: {
            data: new Uint8Array(1),  // Start with a 1-byte array
            sourceOperand: { type: 'NoOp' },
            targetOperand: { type: 'NoOp' }
        },
        flags: {
            halted: false,
            negative: false,
            zero: true,
            overflow: false,
            carry: false
        }
    };
}

/**
 * Resets Cesar CPU registers to their initial state.
 * @param registers The registers to reset
 */
export function cesarRegistersReset(registers: CesarRegisters): void {
    registers.r.fill(0);
    registers.instructionRegister.data = new Uint8Array(1);
    registers.instructionRegister.sourceOperand = { type: 'NoOp' };
    registers.instructionRegister.targetOperand = { type: 'NoOp' };
    registers.flags.halted = false;
    registers.flags.negative = false;
    registers.flags.zero = true;
    registers.flags.overflow = false;
    registers.flags.carry = false;
}

/**
 * Creates a new Cesar CPU with memory.
 * @returns A new CesarCpu object
 */
export function createCesarCpu(): CesarCpu {
    return {
        registers: createCesarRegisters(),
        memory: createMemory(65536),  // Cesar has 64KB memory (16-bit addressing)
    };
}

/**
 * Resets the Cesar CPU to its initial state.
 * @param cpu The CPU to reset
 */
export function cesarReset(cpu: CesarCpu): void {
    cesarRegistersReset(cpu.registers);
    memoryReset(cpu.memory);
}

/**
 * Encodes a Cesar instruction with one operand.
 * @param instruction The instruction to encode
 * @param targetMode The addressing mode for the target operand
 * @param targetRegister The register for the target operand
 * @returns The encoded instruction (16-bit)
 */
export function encodeInstructionOneOperand(
    instruction: CesarInstruction, 
    targetMode: CesarAddressMode, 
    targetRegister: CesarRegister
): number {
    return (instruction << 8) | (targetMode & CESAR_ADDRESS_MODE_MASK) | (targetRegister & CESAR_REGISTER_MASK);
}

/**
 * Encodes a Cesar instruction with two operands.
 * @param instruction The instruction to encode
 * @param sourceMode The addressing mode for the source operand
 * @param sourceRegister The register for the source operand
 * @param targetMode The addressing mode for the target operand
 * @param targetRegister The register for the target operand
 * @returns The encoded instruction (16-bit)
 */
export function encodeInstructionTwoOperand(
    instruction: CesarInstruction,
    sourceMode: CesarAddressMode,
    sourceRegister: CesarRegister,
    targetMode: CesarAddressMode,
    targetRegister: CesarRegister
): number {
    // 1OP: iiii iiii 00mm mrrr
    // 2OP: iiii MMMR RRmm mrrr

    //   i: instruction
    // m/r: mode/register of target operand
    // M/R: mode/register of source operand
    
    switch (instruction) {
        case CesarInstruction.Mov:
        case CesarInstruction.Add:
        case CesarInstruction.Sub:
        case CesarInstruction.Cmp:
        case CesarInstruction.And:
        case CesarInstruction.Or:
            const firstPart = encodeInstructionOneOperand(instruction, targetMode, targetRegister);
            return firstPart | ((sourceMode & CESAR_ADDRESS_MODE_MASK) << 6) | ((sourceRegister & CESAR_REGISTER_MASK) << 6);
        default:
            throw new Error("This function only encodes instructions with two operands.");
    }
}

/**
 * Implements the Cesar CPU fetch cycle:
 * - Reads the next instruction from memory at the PC (R7)
 * - Decodes the instruction and its operands
 * - Updates the instruction register
 * @param cpu The CPU to fetch the next instruction for
 */
export function cesarFetch(cpu: CesarCpu): void {
    const regs = cpu.registers;
    // Reset halted flag at the beginning of fetch cycle to match F# behavior
    regs.flags.halted = false;

    const r = regs.r;
    const ir = regs.instructionRegister;
    const mem = cpu.memory;

    // Helper function to append a byte to the instruction register
    function appendByteToInstructionRegister(value: number): void {
        const newData = new Uint8Array(ir.data.length + 1);
        newData.set(ir.data);
        newData[ir.data.length] = value;
        ir.data = newData;
    }

    // Helper function to append a word (16-bit value) to the instruction register
    function appendWordToInstructionRegister(value: number): void {
        const newData = new Uint8Array(ir.data.length + 2);
        newData.set(ir.data);
        newData[ir.data.length] = (value >>> 8) & 0xFF;  // High byte
        newData[ir.data.length + 1] = value & 0xFF;      // Low byte
        ir.data = newData;
    }

    // Helper function to read a register value and increment it by 2
    function readRegisterAndInc(register: CesarRegister): number {
        const result = r[register];
        r[register] = (result + 2) & 0xFFFF;  // Ensure 16-bit value
        return result;
    }

    // Helper function to decrement a register value by 2 and return the new value
    function decRegisterAndRead(register: CesarRegister): number {
        r[register] = (r[register] - 2) & 0xFFFF;  // Ensure 16-bit value
        return r[register];
    }

    // Helper function to read a 16-bit word from memory at the register's address and increment the register by 2
    function readWordAndInc(register: CesarRegister): number {
        const result = memoryReadWordBigEndian(mem, r[register]);
        r[register] = (r[register] + 2) & 0xFFFF;  // Ensure 16-bit value
        return result;
    }

    // Helper function to decrement a register value by 2 and read a 16-bit word from the new address
    function decAndReadWord(register: CesarRegister): number {
        r[register] = (r[register] - 2) & 0xFFFF;  // Ensure 16-bit value
        return memoryReadWordBigEndian(mem, r[register]);
    }

    // Helper function to decode an operand based on the addressing mode and register
    function decodeOperand(mode: CesarAddressMode, register: CesarRegister): Operand {
        // Explicitly handle immediate addressing (#imm) which uses RegPostInc with R7
        if (mode === CesarAddressMode.RegPostInc && register === CesarRegister.R7) {
            const immediateAddress = r[CesarRegister.R7]; // Address where immediate value is stored
            const immediateValue = memoryReadWordBigEndian(mem, immediateAddress); // Read the value
            r[CesarRegister.R7] = (immediateAddress + 2) & 0xFFFF; // Advance PC past the immediate value
            appendWordToInstructionRegister(immediateValue); // Add immediate value to IR data
            // For JMP #imm, the 'address' is the immediate value itself
            return { type: 'Addr', address: immediateValue }; 
        }

        switch (mode) {
            case CesarAddressMode.Register:
                return { type: 'Reg', register };

            case CesarAddressMode.RegPostInc: { 
                // Handles non-R7 cases for RegPostInc
                const address = readRegisterAndInc(register); // Reads R[reg], increments R[reg] by 2
                // Return the address from which the value should be read later during execute
                return { type: 'Addr', address: address };
            }

            case CesarAddressMode.RegPreDec:
                return { type: 'Addr', address: decRegisterAndRead(register) };

            case CesarAddressMode.Indexed: {
                const address = readWordAndInc(CesarRegister.R7);
                appendWordToInstructionRegister(address);
                return { type: 'Addr', address: (r[register] + address) & 0xFFFF };
            }

            case CesarAddressMode.RegisterIndirect:
                return { type: 'Addr', address: r[register] };

            case CesarAddressMode.RegPostIncIndirect: {
                const address = readWordAndInc(register);
                if (register === CesarRegister.R7) {
                    appendWordToInstructionRegister(address);
                }
                return { type: 'Addr', address };
            }

            case CesarAddressMode.RegPreDecIndirect:
                return { type: 'Addr', address: decAndReadWord(register) };

            case CesarAddressMode.IndexedIndirect: {
                const addressIndirect = readWordAndInc(CesarRegister.R7);
                appendWordToInstructionRegister(addressIndirect);
                const addressIndexed = (r[register] + addressIndirect) & 0xFFFF;
                return { type: 'Addr', address: memoryReadWordBigEndian(mem, addressIndexed) };
            }

            default:
                throw new Error(`Invalid AddressMode: ${mode}`);
        }
    }

    // Read the byte at the Program Counter (R7) and advance PC
    function readByteFromProgramCounterAndAdvance(): number {
        const result = memoryReadByte(mem, r[7]);
        r[7] = (r[7] + 1) & 0xFFFF;  // Ensure 16-bit value
        return result;
    }

    // Read the first opcode byte
    const firstOpCode = readByteFromProgramCounterAndAdvance();
    ir.data = new Uint8Array([firstOpCode]);
    ir.sourceOperand = { type: 'NoOp' };
    ir.targetOperand = { type: 'NoOp' };

    // Extract the instruction from the opcode
    const instruction = firstOpCode & CESAR_INSTRUCTION_MASK;

    // Process instruction based on its type
    switch (instruction) {
        case CesarInstruction.Nop:
        case CesarInstruction.Ccc:
        case CesarInstruction.Scc:
        case CesarInstruction.Rts:
        case CesarInstruction.Hlt:
            // Instructions without operands - nothing more to do
            break;

        default:
            // All other instructions have at least one operand
            const secondOpCode = readByteFromProgramCounterAndAdvance();
            appendByteToInstructionRegister(secondOpCode);

            switch (instruction) {
                case CesarInstruction.Br:
                case CesarInstruction.Sob:
                    // Branch operand - already handled by reading the second byte
                    break;

                default:
                    // One or two operands
                    switch (instruction) {
                        case CesarInstruction.Mov:
                        case CesarInstruction.Add:
                        case CesarInstruction.Sub:
                        case CesarInstruction.Cmp:
                        case CesarInstruction.And:
                        case CesarInstruction.Or:
                            // Two operand instructions
                            const sourceMode = ((firstOpCode << 2) & CESAR_ADDRESS_MODE_MASK) as CesarAddressMode;
                            const sourceReg = (((firstOpCode & 0x01) << 2) | (secondOpCode >>> 6)) & CESAR_REGISTER_MASK as CesarRegister;
                            ir.sourceOperand = decodeOperand(sourceMode, sourceReg);
                            break;
                    }

                    // First operand (present in all instructions except NOP, CCC, SCC, RTS, HLT, BR, SOB)
                    const targetMode = (secondOpCode & CESAR_ADDRESS_MODE_MASK) as CesarAddressMode;
                    const targetRegister = (secondOpCode & CESAR_REGISTER_MASK) as CesarRegister;
                    ir.targetOperand = decodeOperand(targetMode, targetRegister);
                    break;
            }
            break;
    }
}

/**
 * Implements the Cesar CPU execute cycle:
 * - Executes the instruction in the instruction register
 * - Updates CPU registers and flags based on the execution
 * - Performs memory operations as required by the instruction
 * @param cpu The CPU to execute the current instruction for
 */
export function cesarExecute(cpu: CesarCpu): void {
    const regs = cpu.registers;
    const r = regs.r;
    const ir = regs.instructionRegister;
    const flags = regs.flags;
    const mem = cpu.memory;

    // Helper function to check if a 16-bit value is negative (sign bit set)
    function isNegative(value: number): boolean {
        return (value & 0x8000) !== 0;
    }

    // Helper function for ALU addition with carry and overflow detection
    function aluAdd(a: number, b: number, carryIn: boolean): number {
        const fullResult = a + b + (carryIn ? 1 : 0);
        flags.carry = fullResult > 0xFFFF;

        const result = fullResult & 0xFFFF; // Keep as 16-bit value
        flags.overflow = (isNegative(a) && isNegative(b) && !isNegative(result)) ||
                         (!isNegative(a) && !isNegative(b) && isNegative(result));
        return result;
    }

    // Helper function to read a value from an operand
    function readValueFromOperand(operand: Operand): number {
        switch (operand.type) {
            case 'Reg':
                return r[operand.register];
            case 'Addr':
                if (operand.address >= KEYBOARD_MEMORY_ADDRESS) {
                    // In 8-bit memory area
                    return memoryReadByte(mem, operand.address);
                } else {
                    return memoryReadWordBigEndian(mem, operand.address);
                }
            case 'NoOp':
            default:
                return 0;
        }
    }

    // Helper function to write a value to an operand
    function writeValueToOperand(operand: Operand, value: number): void {
        switch (operand.type) {
            case 'Reg':
                r[operand.register] = value & 0xFFFF; // Ensure 16-bit value
                break;
            case 'Addr':
                if (operand.address >= KEYBOARD_MEMORY_ADDRESS) {
                    // In 8-bit memory area
                    memoryWriteByte(mem, operand.address, value);
                } else {
                    memoryWriteWordBigEndian(mem, operand.address, value);
                }
                break;
            case 'NoOp':
            default:
                // Do nothing
                break;
        }
    }

    const firstOpCode = ir.data[0];
    const instruction = firstOpCode & CESAR_INSTRUCTION_MASK;
    const register = firstOpCode & CESAR_REGISTER_MASK;

    switch (instruction) {
        case CesarInstruction.Ccc:
        case CesarInstruction.Scc:
            // CCC & SCC - Clear or Set Condition Codes
            if ((firstOpCode & CesarFlag.Negative) !== 0) {
                flags.negative = instruction === CesarInstruction.Scc;
            }
            if ((firstOpCode & CesarFlag.Zero) !== 0) {
                flags.zero = instruction === CesarInstruction.Scc;
            }
            if ((firstOpCode & CesarFlag.Overflow) !== 0) {
                flags.overflow = instruction === CesarInstruction.Scc;
            }
            if ((firstOpCode & CesarFlag.Carry) !== 0) {
                flags.carry = instruction === CesarInstruction.Scc;
            }
            break;

        case CesarInstruction.Br:
            // Branch instructions group
            const branchIf = (condition: boolean) => {
                if (condition) {
                    // Convert to signed 8-bit value (-128 to 127)
                    const branchOperand = ir.data[1];
                    const signedOffset = branchOperand <= 0x7F ? branchOperand : branchOperand - 256;
                    r[CesarRegister.R7] = (r[CesarRegister.R7] + signedOffset) & 0xFFFF;
                }
            };

            const branchIfNot = (condition: boolean) => {
                branchIf(!condition);
            };

            const brSubInstruction = instruction | (firstOpCode & CESAR_SUB_INSTRUCTION_MASK);
            switch (brSubInstruction) {
                case CesarInstruction.Br:  branchIf(true); break;
                case CesarInstruction.Bne: branchIfNot(flags.zero); break;
                case CesarInstruction.Beq: branchIf(flags.zero); break;
                case CesarInstruction.Bpl: branchIfNot(flags.negative); break;
                case CesarInstruction.Bmi: branchIf(flags.negative); break;
                case CesarInstruction.Bvc: branchIfNot(flags.overflow); break;
                case CesarInstruction.Bvs: branchIf(flags.overflow); break;
                case CesarInstruction.Bcc: branchIfNot(flags.carry); break;
                case CesarInstruction.Bcs: branchIf(flags.carry); break;
                case CesarInstruction.Bge: branchIf(flags.negative === flags.overflow); break;
                case CesarInstruction.Blt: branchIfNot(flags.negative === flags.overflow); break;
                case CesarInstruction.Bgt: branchIf((flags.negative === flags.overflow) && !flags.zero); break;
                case CesarInstruction.Ble: branchIfNot((flags.negative === flags.overflow) && !flags.zero); break;
                case CesarInstruction.Bhi: branchIfNot(flags.carry || flags.zero); break;
                case CesarInstruction.Bls: branchIf(flags.carry || flags.zero); break;
            }
            break;

        case CesarInstruction.Jmp:
            // JMP - Jump to address
            if (ir.targetOperand.type === 'Addr') {
                r[CesarRegister.R7] = ir.targetOperand.address;
            }
            // Mode 0 (register) is not allowed for JMP
            break;

        case CesarInstruction.Sob:
            // SOB - Subtract One and Branch if not zero
            r[register] = (r[register] - 1) & 0xFFFF;
            if (r[register] !== 0) {
                const branchOperand = ir.data[1];
                r[CesarRegister.R7] = (r[CesarRegister.R7] - branchOperand) & 0xFFFF;
            }
            break;

        case CesarInstruction.Jsr:
            // JSR - Jump to Subroutine
            if (ir.targetOperand.type === 'Addr') {
                r[CesarRegister.R6] = (r[CesarRegister.R6] - 2) & 0xFFFF;
                memoryWriteWordBigEndian(mem, r[CesarRegister.R6], r[register]);
                r[register] = r[CesarRegister.R7];
                r[CesarRegister.R7] = ir.targetOperand.address;
            }
            // Mode 0 (register) is not allowed for JSR
            break;

        case CesarInstruction.Rts:
            // RTS - Return from Subroutine
            r[CesarRegister.R7] = r[register];
            r[register] = memoryReadWordBigEndian(mem, r[CesarRegister.R6]);
            r[CesarRegister.R6] = (r[CesarRegister.R6] + 2) & 0xFFFF;
            break;

        case CesarInstruction.Clr:
            // CLR group - Single operand instructions
            const clrSubInstruction = instruction | (firstOpCode & CESAR_SUB_INSTRUCTION_MASK);
            
            let clrTargetValue = 0;
            if (clrSubInstruction !== CesarInstruction.Clr) {  
                // Do not read operand in CLR
                clrTargetValue = readValueFromOperand(ir.targetOperand);
            }
            
            switch (clrSubInstruction) {
                case CesarInstruction.Clr:
                    flags.carry = false;
                    flags.overflow = false;
                    clrTargetValue = 0;
                    break;
                    
                case CesarInstruction.Not:
                    flags.carry = true; // F# implementation sets carry for NOT
                    flags.overflow = false;
                    clrTargetValue = (~clrTargetValue) & 0xFFFF;
                    break;

                case CesarInstruction.Inc:
                    clrTargetValue = aluAdd(clrTargetValue, 1, false);
                    break;

                case CesarInstruction.Dec:
                    clrTargetValue = aluAdd(clrTargetValue, 0xFFFF, false);
                    flags.carry = !flags.carry; // Invert carry for subtraction
                    break;

                case CesarInstruction.Neg:
                    clrTargetValue = aluAdd((~clrTargetValue) & 0xFFFF, 1, false);
                    flags.carry = !flags.carry; // Invert carry for negation
                    break;

                case CesarInstruction.Tst:
                    flags.carry = false;
                    flags.overflow = false;
                    break;

                case CesarInstruction.Ror:
                    const higherBitRor = flags.carry ? 0x8000 : 0;
                    flags.carry = (clrTargetValue & 1) !== 0;
                    clrTargetValue = ((clrTargetValue >>> 1) | higherBitRor) & 0xFFFF;
                    flags.negative = isNegative(clrTargetValue);
                    flags.overflow = flags.carry !== flags.negative;
                    break;

                case CesarInstruction.Rol:
                    const lowerBitRol = flags.carry ? 1 : 0;
                    flags.carry = isNegative(clrTargetValue);
                    clrTargetValue = ((clrTargetValue << 1) | lowerBitRol) & 0xFFFF;
                    flags.negative = isNegative(clrTargetValue);
                    flags.overflow = flags.carry !== flags.negative;
                    break;

                case CesarInstruction.Asr:
                    const higherBitAsr = clrTargetValue & 0x8000; // Preserve sign bit
                    flags.carry = (clrTargetValue & 1) !== 0;
                    clrTargetValue = ((clrTargetValue >>> 1) | higherBitAsr) & 0xFFFF;
                    flags.negative = isNegative(clrTargetValue);
                    flags.overflow = flags.carry !== flags.negative;
                    break;

                case CesarInstruction.Asl:
                    flags.carry = isNegative(clrTargetValue);
                    clrTargetValue = (clrTargetValue << 1) & 0xFFFF;
                    flags.negative = isNegative(clrTargetValue);
                    flags.overflow = flags.carry !== flags.negative;
                    break;

                case CesarInstruction.Adc:
                    const addCar = flags.carry ? 1 : 0;
                    clrTargetValue = aluAdd(clrTargetValue, addCar, false);
                    break;

                case CesarInstruction.Sbc:
                    const negCar = flags.carry ? 0xFFFE : 0xFFFF;
                    clrTargetValue = aluAdd(clrTargetValue, negCar, true);
                    break;
            }
            
            flags.negative = isNegative(clrTargetValue);
            flags.zero = clrTargetValue === 0;

            if (clrSubInstruction !== CesarInstruction.Tst) {
                // Do not write result in TST
                writeValueToOperand(ir.targetOperand, clrTargetValue);
            }
            break;

        case CesarInstruction.Mov:
        case CesarInstruction.Add:
        case CesarInstruction.Sub:
        case CesarInstruction.Cmp:
        case CesarInstruction.And:
        case CesarInstruction.Or:
            // MOV group - Two operand instructions
            const sourceValue = readValueFromOperand(ir.sourceOperand);
            
            let movTargetValue = 0;
            if (instruction !== CesarInstruction.Mov) {
                // Do not read target operand in MOV
                movTargetValue = readValueFromOperand(ir.targetOperand);
            }
            
            switch (instruction) {
                case CesarInstruction.Mov:
                    movTargetValue = sourceValue;
                    flags.overflow = false;
                    break;

                case CesarInstruction.Add:
                    movTargetValue = aluAdd(movTargetValue, sourceValue, false);
                    break;

                case CesarInstruction.Sub:
                    movTargetValue = aluAdd((~sourceValue) & 0xFFFF, movTargetValue, true);
                    flags.carry = !flags.carry; // Invert carry for subtraction
                    break;

                case CesarInstruction.Cmp:
                    // Calculate source - target
                    movTargetValue = aluAdd(sourceValue, (~movTargetValue) & 0xFFFF, true);
                    flags.carry = !flags.carry; // Invert carry for comparison (subtraction)
                    break;

                case CesarInstruction.And:
                    movTargetValue = movTargetValue & sourceValue;
                    flags.overflow = false;
                    break;

                case CesarInstruction.Or:
                    movTargetValue = movTargetValue | sourceValue;
                    flags.overflow = false;
                    break;
            }

            flags.negative = isNegative(movTargetValue);
            flags.zero = movTargetValue === 0;

            if (instruction !== CesarInstruction.Cmp) {
                // Do not write result in CMP
                writeValueToOperand(ir.targetOperand, movTargetValue);
            }
            break;

        case CesarInstruction.Hlt:
            // HLT - Halt the CPU
            flags.halted = true;
            break;

        case CesarInstruction.Nop:
        default:
            // NOP or unknown instruction - do nothing
            break;
    }
}

/**
 * Executes a single Fetch-Execute cycle of the Cesar CPU.
 * @param cpu The CPU to step
 */
export function cesarStep(cpu: CesarCpu): void {
    // F# version does not check halted flag before execution - we maintain that behavior
    cesarFetch(cpu);
    cesarExecute(cpu);
}

/**
 * Disassembles a single instruction from a byte array, returning the mnemonic and instruction size.
 * @param content The byte array containing the instruction to disassemble
 * @returns A tuple containing the disassembled instruction string and its size in bytes
 */
export function cesarDisassembleInstruction(content: number[]): [string, number] {
    if (!content || content.length === 0) {
        return ["", 0];
    }

    const data = [...content]; // Make a copy to avoid modifying the original
    const firstOpCode = data[0];
    const instruction = firstOpCode & CESAR_INSTRUCTION_MASK;

    // Initial size determination based on instruction type
    let size = 0;
    switch (instruction) {
        case CesarInstruction.Ccc:
        case CesarInstruction.Scc:
        case CesarInstruction.Rts:
        case CesarInstruction.Hlt:
        case CesarInstruction.Nop:
            size = 1;
            break;
        default:
            size = 2; // Most instructions are at least 2 bytes
            break;
    }

    // Helper function to format flags
    function formatFlags(): string {
        const flags = [];
        if ((firstOpCode & CesarFlag.Negative) !== 0) flags.push('N');
        if ((firstOpCode & CesarFlag.Zero) !== 0) flags.push('Z');
        if ((firstOpCode & CesarFlag.Overflow) !== 0) flags.push('V');
        if ((firstOpCode & CesarFlag.Carry) !== 0) flags.push('C');
        return flags.join('');
    }

    // Helper function to format register names
    function formatRegister(reg: number): string {
        return `R${reg & CESAR_REGISTER_MASK}`;
    }

    // Helper function to safely read data at specified position
    function readData(addr: number): number {
        return addr < data.length ? data[addr] : 0;
    }

    // Helper function to read the next operand (16-bit value)
    function getNextOperand(): number {
        const result = (readData(size) << 8) | readData(size + 1);
        size += 2;
        return result;
    }

    // Helper function to decode and format an operand based on mode and register
    function decodeOperand(mode: number, reg: number): string {
        const r = formatRegister(reg);
        const m = mode & CESAR_ADDRESS_MODE_MASK;

        // For R7 with certain modes, we need to account for immediate data in instruction size
        if (reg === CesarRegister.R7) {
            switch (m) {
                case CesarAddressMode.RegPostInc:
                case CesarAddressMode.RegPostIncIndirect:
                    size += 2;
                    break;
            }
        }

        switch (m) {
            case CesarAddressMode.Register:           return r;
            case CesarAddressMode.RegPostInc:         return `(${r})+`;
            case CesarAddressMode.RegPreDec:          return `-(${r})`;
            case CesarAddressMode.Indexed: {
                const offset = getNextOperand();
                return `${offset}(${r})`;
            }
            case CesarAddressMode.RegisterIndirect:   return `(${r})`;
            case CesarAddressMode.RegPostIncIndirect: return `((${r})+)`;
            case CesarAddressMode.RegPreDecIndirect:  return `(--(${r}))`;
            case CesarAddressMode.IndexedIndirect: {
                const offset = getNextOperand();
                return `(${offset}(${r}))`;
            }
            default: return "?"; // Invalid AddressMode
        }
    }

    // Helper function to format branch operands
    function formatBranchOperand(): string {
        return `${readData(1)}`;
    }

    // Helper function to format target operand
    function formatTargetOperand(): string {
        const secondOpCode = readData(1);
        const mode = secondOpCode & CESAR_ADDRESS_MODE_MASK;
        const reg = secondOpCode & CESAR_REGISTER_MASK;
        return decodeOperand(mode, reg);
    }

    // Helper function for jump operands (which have restrictions)
    function formatJumpOperand(): string {
        const secondOpCode = readData(1);
        const mode = secondOpCode & CESAR_ADDRESS_MODE_MASK;

        if (mode === CesarAddressMode.Register) {
            return "?"; // Illegal mode for jumps
        } else {
            return formatTargetOperand();
        }
    }

    // Helper function to format source operand
    function formatSourceOperand(): string {
        const secondOpCode = readData(1);
        const mode = (firstOpCode << 2) & CESAR_ADDRESS_MODE_MASK;
        const reg = (((firstOpCode & 0x01) << 2) | (secondOpCode >>> 6)) & CESAR_REGISTER_MASK;
        return decodeOperand(mode, reg);
    }

    // Main disassembly logic
    let output = "";
    
    switch (instruction) {
        case CesarInstruction.Ccc:
            output = "CCC " + formatFlags();
            break;
        case CesarInstruction.Scc:
            output = "SCC " + formatFlags();
            break;
        case CesarInstruction.Br: {
            const subInstruction = instruction | (firstOpCode & CESAR_SUB_INSTRUCTION_MASK);
            switch (subInstruction) {
                case CesarInstruction.Br:  output = "BR  " + formatBranchOperand(); break;
                case CesarInstruction.Bne: output = "BNE " + formatBranchOperand(); break;
                case CesarInstruction.Beq: output = "BEQ " + formatBranchOperand(); break;
                case CesarInstruction.Bpl: output = "BPL " + formatBranchOperand(); break;
                case CesarInstruction.Bmi: output = "BMI " + formatBranchOperand(); break;
                case CesarInstruction.Bvc: output = "BVC " + formatBranchOperand(); break;
                case CesarInstruction.Bvs: output = "BVS " + formatBranchOperand(); break;
                case CesarInstruction.Bcc: output = "BCC " + formatBranchOperand(); break;
                case CesarInstruction.Bcs: output = "BCS " + formatBranchOperand(); break;
                case CesarInstruction.Bge: output = "BGE " + formatBranchOperand(); break;
                case CesarInstruction.Blt: output = "BLT " + formatBranchOperand(); break;
                case CesarInstruction.Bgt: output = "BGT " + formatBranchOperand(); break;
                case CesarInstruction.Ble: output = "BLE " + formatBranchOperand(); break;
                case CesarInstruction.Bhi: output = "BHI " + formatBranchOperand(); break;
                case CesarInstruction.Bls: output = "BLS " + formatBranchOperand(); break;
                default: output = "?"; break;
            }
            break;
        }
        case CesarInstruction.Jmp:
            output = "JMP " + formatJumpOperand();
            break;
        case CesarInstruction.Sob:
            output = "SOB " + formatRegister(data[0]) + ", " + formatBranchOperand();
            break;
        case CesarInstruction.Jsr:
            output = "JSR " + formatRegister(data[0]) + ", " + formatJumpOperand();
            break;
        case CesarInstruction.Rts:
            output = "RTS " + formatRegister(data[0]);
            break;
        case CesarInstruction.Clr: {
            const subInstruction = instruction | (firstOpCode & CESAR_SUB_INSTRUCTION_MASK);
            switch (subInstruction) {
                case CesarInstruction.Clr: output = "CLR " + formatTargetOperand(); break;
                case CesarInstruction.Not: output = "NOT " + formatTargetOperand(); break;
                case CesarInstruction.Inc: output = "INC " + formatTargetOperand(); break;
                case CesarInstruction.Dec: output = "DEC " + formatTargetOperand(); break;
                case CesarInstruction.Neg: output = "NEG " + formatTargetOperand(); break;
                case CesarInstruction.Tst: output = "TST " + formatTargetOperand(); break;
                case CesarInstruction.Ror: output = "ROR " + formatTargetOperand(); break;
                case CesarInstruction.Rol: output = "ROL " + formatTargetOperand(); break;
                case CesarInstruction.Asr: output = "ASR " + formatTargetOperand(); break;
                case CesarInstruction.Asl: output = "ASL " + formatTargetOperand(); break;
                case CesarInstruction.Adc: output = "ADC " + formatTargetOperand(); break;
                case CesarInstruction.Sbc: output = "SBC " + formatTargetOperand(); break;
                default: output = "?"; break;
            }
            break;
        }
        case CesarInstruction.Mov:
            output = "MOV " + formatSourceOperand() + ", " + formatTargetOperand();
            break;
        case CesarInstruction.Add:
            output = "ADD " + formatSourceOperand() + ", " + formatTargetOperand();
            break;
        case CesarInstruction.Sub:
            output = "SUB " + formatSourceOperand() + ", " + formatTargetOperand();
            break;
        case CesarInstruction.Cmp:
            output = "CMP " + formatSourceOperand() + ", " + formatTargetOperand();
            break;
        case CesarInstruction.And:
            output = "AND " + formatSourceOperand() + ", " + formatTargetOperand();
            break;
        case CesarInstruction.Or:
            output = "OR  " + formatSourceOperand() + ", " + formatTargetOperand();
            break;
        case CesarInstruction.Hlt:
            output = "HLT";
            break;
        case CesarInstruction.Nop:
        default:
            output = "NOP";
            break;
    }

    // Check if we're trying to disassemble beyond the content length
    if (size > content.length) {
        return ["", 0];
    }

    return [output.trim(), size];
}

/**
 * Disassembles multiple instructions from a byte array, returning an array of [instruction, size] tuples
 * @param content The byte array containing the instructions to disassemble
 * @returns An array of tuples, each containing a disassembled instruction string and its size in bytes
 */
export function cesarDisassembleInstructions(content: number[]): [string, number][] {
    const result: [string, number][] = [];
    let remaining = [...content];

    while (remaining.length > 0) {
        const [instruction, size] = cesarDisassembleInstruction(remaining);
        if (size === 0) break;
        
        result.push([instruction, size]);
        remaining = remaining.slice(size);
    }

    return result;
}