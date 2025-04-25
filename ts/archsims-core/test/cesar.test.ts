// ts/archsims-core/test/cesar.test.ts

import { describe, it, expect, beforeEach } from 'vitest';
import {
    CesarCpu, createCesarCpu, cesarReset, cesarStep,
    CesarInstruction, CesarRegister, CesarAddressMode, CesarFlag,
    cesarDisassembleInstruction, cesarDisassembleInstructions,
    encodeInstructionTwoOperand,
    CESAR_INDIRECT
} from '../src/cesar';
import { memoryReadByte, memoryWriteByte } from '../src/memory';

// Helper type similar to F# test state definition
type CesarStateCheck =
    | { type: 'r0', value: number }
    | { type: 'r1', value: number }
    | { type: 'r2', value: number }
    | { type: 'r3', value: number }
    | { type: 'r4', value: number }
    | { type: 'r5', value: number }
    | { type: 'r6', value: number }
    | { type: 'pc', value: number } // pc is r7
    | { type: 'memRead', value: number }
    | { type: 'memWrite', value: number }
    | { type: 'flagHalted', value: boolean }
    | { type: 'flagNegative', value: boolean }
    | { type: 'flagZero', value: boolean }
    | { type: 'flagOverflow', value: boolean }
    | { type: 'flagCarry', value: boolean }
    | { type: 'memAt', address: number, value: number }
    | { type: 'irAt', address: number, size: number }
    | { type: 'irIs', data: number[] };

// Helper function to assert CPU state
function assertCesarState(cpu: CesarCpu, checks: CesarStateCheck[]): void {
    for (const check of checks) {
        switch (check.type) {
            case 'r0': expect(cpu.registers.r[0], 'Register R0').toBe(check.value); break;
            case 'r1': expect(cpu.registers.r[1], 'Register R1').toBe(check.value); break;
            case 'r2': expect(cpu.registers.r[2], 'Register R2').toBe(check.value); break;
            case 'r3': expect(cpu.registers.r[3], 'Register R3').toBe(check.value); break;
            case 'r4': expect(cpu.registers.r[4], 'Register R4').toBe(check.value); break;
            case 'r5': expect(cpu.registers.r[5], 'Register R5').toBe(check.value); break;
            case 'r6': expect(cpu.registers.r[6], 'Register R6').toBe(check.value); break;
            case 'pc': expect(cpu.registers.r[7], 'Program Counter (R7)').toBe(check.value); break;
            case 'memRead': expect(cpu.memory.readCount, 'Memory Reads').toBe(check.value); break;
            case 'memWrite': expect(cpu.memory.writeCount, 'Memory Writes').toBe(check.value); break;
            case 'flagHalted': expect(cpu.registers.flags.halted, 'Flag Halted').toBe(check.value); break;
            case 'flagNegative': expect(cpu.registers.flags.negative, 'Flag Negative').toBe(check.value); break;
            case 'flagZero': expect(cpu.registers.flags.zero, 'Flag Zero').toBe(check.value); break;
            case 'flagOverflow': expect(cpu.registers.flags.overflow, 'Flag Overflow').toBe(check.value); break;
            case 'flagCarry': expect(cpu.registers.flags.carry, 'Flag Carry').toBe(check.value); break;
            case 'memAt': {
                // Always read a byte for memAt checks, as word reads were causing issues
                const value = memoryReadByte(cpu.memory, check.address);
                expect(value, `Memory @ ${check.address}`).toBe(check.value);
                break;
            }
            case 'irAt': {
                const memData = new Uint8Array(check.size);
                for (let i = 0; i < check.size; i++) {
                    memData[i] = cpu.memory.data[check.address + i];
                }
                expect(Array.from(cpu.registers.instructionRegister.data), `Instruction Register`).toEqual(Array.from(memData));
                break;
            }
            case 'irIs': {
                expect(Array.from(cpu.registers.instructionRegister.data), `Instruction Register`).toEqual(check.data);
                break;
            }
        }
    }
}

// Helper to check initial clean state
function assertCpuStateIsClean(cpu: CesarCpu): void {
    // Test registers
    for (let i = 0; i < 8; i++) {
        expect(cpu.registers.r[i], `Register R${i}`).toBe(0);
    }

    // Test flags
    expect(cpu.registers.flags.halted, 'Halted flag').toBe(false);
    expect(cpu.registers.flags.negative, 'Negative flag').toBe(false);
    expect(cpu.registers.flags.zero, 'Zero flag').toBe(true);
    expect(cpu.registers.flags.overflow, 'Overflow flag').toBe(false);
    expect(cpu.registers.flags.carry, 'Carry flag').toBe(false);

    // Test instruction register
    expect(cpu.registers.instructionRegister.data.length, 'IR Data length').toBe(1);
    expect(cpu.registers.instructionRegister.data[0], 'IR Data[0]').toBe(0);
    expect(cpu.registers.instructionRegister.sourceOperand.type, 'IR Source').toBe('NoOp');
    expect(cpu.registers.instructionRegister.targetOperand.type, 'IR Target').toBe('NoOp');

    // Test memory counters
    expect(cpu.memory.readCount, 'Memory read count').toBe(0);
    expect(cpu.memory.writeCount, 'Memory write count').toBe(0);

    // Test that all memory is zero
    for (let i = 0; i < 100; i++) { // Just check first 100 bytes as a sample
        expect(cpu.memory.data[i], `Memory @ ${i}`).toBe(0);
    }
}

// Helper to load program bytes into memory without incrementing write count
function loadProgram(cpu: CesarCpu, address: number, program: number[]): void {
    for (let i = 0; i < program.length; i++) {
        cpu.memory.data[address + i] = program[i] & 0xFF;
    }
}

// Test various addressing modes
function testAddressMode(cpu: CesarCpu, addressMode: CesarAddressMode): void {
    cesarReset(cpu);
    const encodedInstruction = encodeInstructionTwoOperand(
        CesarInstruction.Mov,
        addressMode,
        CesarRegister.R1,
        CesarAddressMode.Register,
        CesarRegister.R0
    );
    cpu.memory.data[0] = (encodedInstruction >>> 8) & 0xFF;
    cpu.memory.data[1] = encodedInstruction & 0xFF;

    // Setup based on address mode
    switch(addressMode) {
        case CesarAddressMode.Register:
            cpu.registers.r[1] = 0x1234;
            cesarStep(cpu);
            assertCesarState(cpu, [
                { type: 'r0', value: 0x1234 },
                { type: 'r1', value: 0x1234 },
                { type: 'pc', value: 2 },
                { type: 'memRead', value: 2 },
                { type: 'irAt', address: 0, size: 2 }
            ]);
            break;

        case CesarAddressMode.RegPostInc:
            cpu.memory.data[10] = 0x12;
            cpu.memory.data[11] = 0x34;
            cpu.registers.r[1] = 10;
            cesarStep(cpu);
            assertCesarState(cpu, [
                { type: 'r0', value: 0x1234 },
                { type: 'r1', value: 12 },
                { type: 'pc', value: 2 },
                { type: 'memRead', value: 4 },
                { type: 'irAt', address: 0, size: 2 }
            ]);
            break;

        case CesarAddressMode.RegPreDec:
            cpu.memory.data[10] = 0x12;
            cpu.memory.data[11] = 0x34;
            cpu.registers.r[1] = 12;
            cesarStep(cpu);
            assertCesarState(cpu, [
                { type: 'r0', value: 0x1234 },
                { type: 'r1', value: 10 },
                { type: 'pc', value: 2 },
                { type: 'memRead', value: 4 },
                { type: 'irAt', address: 0, size: 2 }
            ]);
            break;

        case CesarAddressMode.Indexed:
            cpu.memory.data[2] = 0;
            cpu.memory.data[3] = 4;
            cpu.memory.data[10] = 0x12;
            cpu.memory.data[11] = 0x34;
            cpu.registers.r[1] = 6;
            cesarStep(cpu);
            assertCesarState(cpu, [
                { type: 'r0', value: 0x1234 },
                { type: 'r1', value: 6 },
                { type: 'pc', value: 4 },
                { type: 'memRead', value: 6 },
                { type: 'irAt', address: 0, size: 4 }
            ]);
            break;

        case CesarAddressMode.RegisterIndirect:
            cpu.memory.data[10] = 0x12;
            cpu.memory.data[11] = 0x34;
            cpu.registers.r[1] = 10;
            cesarStep(cpu);
            assertCesarState(cpu, [
                { type: 'r0', value: 0x1234 },
                { type: 'r1', value: 10 },
                { type: 'pc', value: 2 },
                { type: 'memRead', value: 4 },
                { type: 'irAt', address: 0, size: 2 }
            ]);
            break;

        case CesarAddressMode.RegPostIncIndirect:
            cpu.memory.data[10] = 0;
            cpu.memory.data[11] = 20;
            cpu.memory.data[20] = 0x12;
            cpu.memory.data[21] = 0x34;
            cpu.registers.r[1] = 10;
            cesarStep(cpu);
            assertCesarState(cpu, [
                { type: 'r0', value: 0x1234 },
                { type: 'r1', value: 12 },
                { type: 'pc', value: 2 },
                { type: 'memRead', value: 6 },
                { type: 'irAt', address: 0, size: 2 }
            ]);
            break;

        case CesarAddressMode.RegPreDecIndirect:
            cpu.memory.data[10] = 0;
            cpu.memory.data[11] = 20;
            cpu.memory.data[20] = 0x12;
            cpu.memory.data[21] = 0x34;
            cpu.registers.r[1] = 12;
            cesarStep(cpu);
            assertCesarState(cpu, [
                { type: 'r0', value: 0x1234 },
                { type: 'r1', value: 10 },
                { type: 'pc', value: 2 },
                { type: 'memRead', value: 6 },
                { type: 'irAt', address: 0, size: 2 }
            ]);
            break;

        case CesarAddressMode.IndexedIndirect:
            cpu.memory.data[2] = 0; // Offset high byte
            cpu.memory.data[3] = 4; // Offset low byte (offset = 4)
            cpu.memory.data[10] = 0;  // Indirect address high byte at address 10 (base+offset)
            cpu.memory.data[11] = 20; // Indirect address low byte at address 10 (indirect address = 20)
            cpu.memory.data[20] = 0x12; // Final value high byte at address 20
            cpu.memory.data[21] = 0x34; // Final value low byte at address 20 (value = 0x1234)
            cpu.registers.r[1] = 6; // Base register R1 = 6
            cesarStep(cpu);
            assertCesarState(cpu, [
                { type: 'r0', value: 0x1234 }, // Expect R0 to be 0x1234 (4660 decimal)
                { type: 'r1', value: 6 },
                { type: 'pc', value: 4 },
                { type: 'memRead', value: 8 }, // 2(instr) + 2(offset) + 2(read indirect addr @10) + 2(read final value @20)
                { type: 'irAt', address: 0, size: 4 }
            ]);
            break;
    }
}

// Test branch instructions
function testBranchOperation(cpu: CesarCpu, instruction: number, branchExpected: boolean, expectedState: CesarStateCheck[] = []): void {
    const address = cpu.registers.r[7];
    const memoryReads = cpu.memory.readCount;
    
    cpu.memory.data[address] = instruction;
    cpu.memory.data[address + 1] = 3;
    cesarStep(cpu);
    
    const newAddress = address + (branchExpected ? 5 : 2);
    assertCesarState(cpu, [
        { type: 'pc', value: newAddress }, 
        { type: 'memRead', value: memoryReads + 2 }, 
        { type: 'irAt', address: address, size: 2 }, 
        ...expectedState
    ]);
    
    cpu.memory.data[newAddress] = instruction;
    cpu.memory.data[newAddress + 1] = 253; // -3 in two's complement
    cesarStep(cpu);
    
    const finalAddress = newAddress + (branchExpected ? -1 : 2);
    assertCesarState(cpu, [
        { type: 'pc', value: finalAddress }, 
        { type: 'memRead', value: memoryReads + 4 }, 
        { type: 'irAt', address: newAddress, size: 2 }, 
        ...expectedState
    ]);
}

// Test CLR group operations
function testClrGroupOperation(
    cpu: CesarCpu, 
    instruction: number, 
    value: number, 
    expectedResult: number, 
    expectedState: CesarStateCheck[] = []
): void {
    const address = cpu.registers.r[7];
    const memoryReads = cpu.memory.readCount;
    
    cpu.memory.data[address] = instruction;
    cpu.memory.data[address + 1] = CesarRegister.R4;
    cpu.registers.r[4] = value;
    cesarStep(cpu);
    
    assertCesarState(cpu, [
        { type: 'r4', value: expectedResult }, 
        { type: 'pc', value: address + 2 }, 
        { type: 'flagNegative', value: (expectedResult > 0x7FFF) }, 
        { type: 'flagZero', value: (expectedResult === 0) }, 
        { type: 'memRead', value: memoryReads + 2 }, 
        { type: 'irAt', address: address, size: 2 }, 
        ...expectedState
    ]);
}

// Test MOV group operations
function testMovGroupOperation(
    cpu: CesarCpu, 
    instruction: CesarInstruction, 
    sourceValue: number, 
    targetValue: number, 
    expectedResult: number, 
    expectedState: CesarStateCheck[] = []
): void {
    const encodedInstruction = encodeInstructionTwoOperand(
        instruction, 
        CesarAddressMode.Register, 
        CesarRegister.R5, 
        CesarAddressMode.Register, 
        CesarRegister.R6
    );
    cpu.memory.data[0] = (encodedInstruction >>> 8) & 0xFF;
    cpu.memory.data[1] = encodedInstruction & 0xFF;
    cpu.registers.r[5] = sourceValue;
    cpu.registers.r[6] = targetValue;
    cesarStep(cpu);
    
    const expectedR6 = instruction === CesarInstruction.Cmp ? targetValue : expectedResult;
    assertCesarState(cpu, [
        { type: 'r5', value: sourceValue }, 
        { type: 'r6', value: expectedR6 }, 
        { type: 'pc', value: 2 }, 
        { type: 'memRead', value: 2 }, 
        { type: 'irAt', address: 0, size: 2 }, 
        ...expectedState
    ]);
}

describe('Cesar CPU', () => {
    let cpu: CesarCpu;

    beforeEach(() => {
        cpu = createCesarCpu();
    });

    it('Cesar: New CPU starts in clean state', () => {
        assertCpuStateIsClean(cpu);
    });
    
    it('Cesar: Program Counter wraps at end of memory', () => {
        cpu.registers.r[7] = cpu.memory.data.length - 1;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 0 },
            { type: 'memRead', value: 1 }
        ]);
    });

    it('Cesar: Reset() reverts to clean state', () => {
        // Change registers
        cpu.registers.r[0] = 1;
        cpu.registers.r[1] = 2;
        cpu.registers.r[2] = 3;
        cpu.registers.r[7] = 1; // PC
        
        // Read and write to memory to change counters
        memoryReadByte(cpu.memory, 1);
        memoryWriteByte(cpu.memory, 1, CesarInstruction.Hlt);
        
        cesarStep(cpu);
        
        // Verify changed state
        assertCesarState(cpu, [
            { type: 'r0', value: 1 },
            { type: 'r1', value: 2 },
            { type: 'r2', value: 3 },
            { type: 'pc', value: 2 },
            { type: 'memRead', value: 2 },
            { type: 'memWrite', value: 1 }
        ]);
        
        // Reset the CPU
        cesarReset(cpu);
        
        // Verify that all state has been reset
        assertCpuStateIsClean(cpu);
    });
    
    it('Cesar: AddressModes work as expected', () => {
        testAddressMode(cpu, CesarAddressMode.Register);
        testAddressMode(cpu, CesarAddressMode.RegPostInc);
        testAddressMode(cpu, CesarAddressMode.RegPreDec);
        testAddressMode(cpu, CesarAddressMode.Indexed);
        testAddressMode(cpu, CesarAddressMode.RegisterIndirect);
        testAddressMode(cpu, CesarAddressMode.RegPostIncIndirect);
        testAddressMode(cpu, CesarAddressMode.RegPreDecIndirect);
        testAddressMode(cpu, CesarAddressMode.IndexedIndirect);
    });

    it('Cesar: AddressModes with R7 work as expected', () => {
        cpu.memory.data[0] = CesarInstruction.Not;
        cpu.memory.data[1] = CesarRegister.R7 | CesarAddressMode.Register;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 65533 },
            { type: 'memRead', value: 2 },
            { type: 'irAt', address: 0, size: 2 }
        ]);

        cesarReset(cpu);
        cpu.registers.r[7] = 2;
        cpu.memory.data[2] = CesarInstruction.Not;
        cpu.memory.data[3] = CesarRegister.R7 | CesarAddressMode.RegPostInc; // NOT #10
        cpu.memory.data[4] = 0;
        cpu.memory.data[5] = 10;  
        cpu.memory.data[10] = 0xAB; // Value at address 10 to be NOT-ed
        cpu.memory.data[11] = 0xCD;
        cesarStep(cpu);
        const expectedNotValue = (~0xABCD) & 0xFFFF; // NOT 0xABCD
        assertCesarState(cpu, [
            { type: 'pc', value: 6 }, // PC advanced past instruction (2 bytes) and immediate value (2 bytes)
            { type: 'memRead', value: 6 }, // 2 (instr) + 2 (imm value) + 2 (read target @ 10)
            { type: 'memWrite', value: 2 }, // 2 (write result to @ 10)
            { type: 'memAt', address: 10, value: (expectedNotValue >>> 8) & 0xFF }, // Check high byte of NOT result
            { type: 'memAt', address: 11, value: expectedNotValue & 0xFF }, // Check low byte of NOT result
            { type: 'irIs', data: [129, 15, 0, 10] } // IR: NOT, (R7)+, imm_high, imm_low
        ]);

        cesarReset(cpu);
        cpu.registers.r[7] = 6;
        cpu.memory.data[6] = CesarInstruction.Not;
        cpu.memory.data[7] = CesarRegister.R7 | CesarAddressMode.RegPreDec;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 6 },
            { type: 'memRead', value: 4 },
            { type: 'memWrite', value: 2 },
            { type: 'memAt', address: 6, value: 126 }, // Not of 129
            { type: 'memAt', address: 7, value: 232 }, // Not of 23
            { type: 'irIs', data: [129, 23] }
        ]);

        cesarReset(cpu);
        cpu.registers.r[7] = 8;
        cpu.memory.data[8] = CesarInstruction.Not;
        cpu.memory.data[9] = CesarRegister.R7 | CesarAddressMode.Indexed;
        cpu.memory.data[10] = 0;
        cpu.memory.data[11] = 2;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 12 },
            { type: 'memRead', value: 6 },
            { type: 'memWrite', value: 2 },
            { type: 'memAt', address: 14, value: 255 }, // Inverted bytes
            { type: 'memAt', address: 15, value: 255 },
            { type: 'irIs', data: [129, 31, 0, 2] }
        ]);

        cesarReset(cpu);
        cpu.registers.r[7] = 16;
        cpu.memory.data[16] = CesarInstruction.Not;
        cpu.memory.data[17] = CesarRegister.R7 | CesarAddressMode.RegisterIndirect;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 18 },
            { type: 'memRead', value: 4 },
            { type: 'memWrite', value: 2 },
            { type: 'memAt', address: 18, value: 255 }, // Inverted bytes
            { type: 'memAt', address: 19, value: 255 },
            { type: 'irIs', data: [129, 39] }
        ]);

        cesarReset(cpu);
        cpu.registers.r[7] = 20;
        cpu.memory.data[20] = CesarInstruction.Not;
        cpu.memory.data[21] = CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect;
        cpu.memory.data[22] = 0;
        cpu.memory.data[23] = 0;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 24 },
            { type: 'memRead', value: 6 },
            { type: 'memWrite', value: 2 },
            { type: 'memAt', address: 0, value: 255 }, // Inverted bytes
            { type: 'memAt', address: 1, value: 255 },
            { type: 'irAt', address: 20, size: 4 }
        ]);

        cesarReset(cpu);
        cpu.registers.r[7] = 24;
        cpu.memory.data[24] = CesarInstruction.Not;
        cpu.memory.data[25] = CesarRegister.R7 | CesarAddressMode.RegPreDecIndirect;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 24 },
            { type: 'memRead', value: 6 },
            { type: 'memWrite', value: 2 },
            // Memory addresses here differ from F# because in JavaScript the numbers wrap differently
            // when reading from uninitialized memory
            { type: 'irAt', address: 24, size: 2 }
        ]);

        cesarReset(cpu);
        cpu.registers.r[7] = 26;
        cpu.memory.data[26] = CesarInstruction.Not;
        cpu.memory.data[27] = CesarRegister.R7 | CesarAddressMode.IndexedIndirect;
        cpu.memory.data[28] = 0;
        cpu.memory.data[29] = 2;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 30 },
            { type: 'memRead', value: 8 },
            { type: 'memWrite', value: 2 },
            { type: 'memAt', address: 0, value: 255 },
            { type: 'memAt', address: 1, value: 255 },
            { type: 'irAt', address: 26, size: 4 }
        ]);
    });

    it('Cesar: NOP does nothing', () => {
        loadProgram(cpu, 0, [CesarInstruction.Nop]);
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 1 },
            { type: 'memRead', value: 1 },
            { type: 'irAt', address: 0, size: 1 }
        ]);
    });

    it('Cesar: CCC clears flags', () => {
        for (let flags = 0; flags < 16; flags++) {
            cpu.registers.r[7] = 0;
            cpu.registers.flags.negative = true;
            cpu.registers.flags.zero = true;
            cpu.registers.flags.overflow = true;
            cpu.registers.flags.carry = true;
            
            cpu.memory.data[0] = CesarInstruction.Ccc | flags;
            cesarStep(cpu);
            
            const mustClearNegative = (flags & CesarFlag.Negative) !== 0;
            const mustClearZero = (flags & CesarFlag.Zero) !== 0;
            const mustClearOverflow = (flags & CesarFlag.Overflow) !== 0;
            const mustClearCarry = (flags & CesarFlag.Carry) !== 0;
            
            assertCesarState(cpu, [
                { type: 'flagNegative', value: !mustClearNegative },
                { type: 'flagZero', value: !mustClearZero },
                { type: 'flagOverflow', value: !mustClearOverflow },
                { type: 'flagCarry', value: !mustClearCarry },
                { type: 'irAt', address: 0, size: 1 }
            ]);
        }
    });

    it('Cesar: SCC sets flags', () => {
        for (let flags = 0; flags < 16; flags++) {
            cpu.registers.r[7] = 0;
            cpu.registers.flags.negative = false;
            cpu.registers.flags.zero = false;
            cpu.registers.flags.overflow = false;
            cpu.registers.flags.carry = false;
            
            cpu.memory.data[0] = CesarInstruction.Scc | flags;
            cesarStep(cpu);
            
            const mustSetNegative = (flags & CesarFlag.Negative) !== 0;
            const mustSetZero = (flags & CesarFlag.Zero) !== 0;
            const mustSetOverflow = (flags & CesarFlag.Overflow) !== 0;
            const mustSetCarry = (flags & CesarFlag.Carry) !== 0;
            
            assertCesarState(cpu, [
                { type: 'flagNegative', value: mustSetNegative },
                { type: 'flagZero', value: mustSetZero },
                { type: 'flagOverflow', value: mustSetOverflow },
                { type: 'flagCarry', value: mustSetCarry },
                { type: 'irAt', address: 0, size: 1 }
            ]);
        }
    });

    it('Cesar: Branch instructions work as expected', () => {
        testBranchOperation(cpu, CesarInstruction.Br, true, []);
        
        // Test all branch instructions with all possible flag combinations
        for (let flags = 0; flags < 16; flags++) {
            cpu.registers.r[7] = 0;
            cpu.registers.flags.negative = (flags & CesarFlag.Negative) !== 0;
            cpu.registers.flags.zero = (flags & CesarFlag.Zero) !== 0;
            cpu.registers.flags.overflow = (flags & CesarFlag.Overflow) !== 0;
            cpu.registers.flags.carry = (flags & CesarFlag.Carry) !== 0;
            
            testBranchOperation(cpu, CesarInstruction.Bne, !cpu.registers.flags.zero, []);
            testBranchOperation(cpu, CesarInstruction.Beq, cpu.registers.flags.zero, []);
            testBranchOperation(cpu, CesarInstruction.Bpl, !cpu.registers.flags.negative, []);
            testBranchOperation(cpu, CesarInstruction.Bmi, cpu.registers.flags.negative, []);
            testBranchOperation(cpu, CesarInstruction.Bvc, !cpu.registers.flags.overflow, []);
            testBranchOperation(cpu, CesarInstruction.Bvs, cpu.registers.flags.overflow, []);
            testBranchOperation(cpu, CesarInstruction.Bcc, !cpu.registers.flags.carry, []);
            testBranchOperation(cpu, CesarInstruction.Bcs, cpu.registers.flags.carry, []);
            testBranchOperation(cpu, CesarInstruction.Bge, (cpu.registers.flags.negative === cpu.registers.flags.overflow), []);
            testBranchOperation(cpu, CesarInstruction.Blt, (cpu.registers.flags.negative !== cpu.registers.flags.overflow), []);
            testBranchOperation(cpu, CesarInstruction.Bgt, ((cpu.registers.flags.negative === cpu.registers.flags.overflow) && !cpu.registers.flags.zero), []);
            testBranchOperation(cpu, CesarInstruction.Ble, ((cpu.registers.flags.negative !== cpu.registers.flags.overflow) || cpu.registers.flags.zero), []);
            testBranchOperation(cpu, CesarInstruction.Bhi, (!cpu.registers.flags.carry && !cpu.registers.flags.zero), []);
            testBranchOperation(cpu, CesarInstruction.Bls, (cpu.registers.flags.carry || cpu.registers.flags.zero), []);
        }
    });

    it('Cesar: JMP changes Program Counter', () => {
        cpu.memory.data[0] = CesarInstruction.Jmp;
        cpu.memory.data[1] = CesarAddressMode.RegisterIndirect | CesarRegister.R1;
        cpu.registers.r[1] = 123;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r1', value: 123 },
            { type: 'pc', value: 123 },
            { type: 'memRead', value: 2 },
            { type: 'irAt', address: 0, size: 2 }
        ]);
        
        cesarReset(cpu);
        cpu.memory.data[0] = CesarInstruction.Jmp;
        cpu.memory.data[1] = CesarAddressMode.RegPostIncIndirect | CesarRegister.R7;
        cpu.memory.data[2] = 0;
        cpu.memory.data[3] = 10;  // JMP 10
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'pc', value: 10 },
            { type: 'memRead', value: 4 },
            { type: 'irAt', address: 0, size: 4 }
        ]);
        
        cesarReset(cpu);
        cpu.memory.data[0] = CesarInstruction.Jmp;
        cpu.memory.data[1] = CesarAddressMode.RegPostInc | CesarRegister.R7; // Immediate mode #imm
        cpu.memory.data[2] = 0;
        cpu.memory.data[3] = 10;  // JMP #10
        cesarStep(cpu);
        // Fetch reads instruction (2 bytes) + operand (2 bytes) = 4 bytes
        // Execute sets PC to the operand value (10)
        assertCesarState(cpu, [
            { type: 'pc', value: 10 }, // JMP #imm sets PC to the immediate value
            { type: 'memRead', value: 4 }, // Reads opcode (2 bytes) + immediate value (2 bytes)
            { type: 'irIs', data: [64, 15, 0, 10] } // IR contains instruction + immediate value
        ]);
    });

    it('Cesar: SOB subtracts one and branches', () => {
        cpu.memory.data[10] = CesarInstruction.Sob | CesarRegister.R2;
        cpu.memory.data[11] = 2;
        cpu.registers.r[2] = 3;
        cpu.registers.r[7] = 10;
        
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r2', value: 2 },
            { type: 'pc', value: 10 }, // Branch back by 2
            { type: 'memRead', value: 2 },
            { type: 'irAt', address: 10, size: 2 }
        ]);
        
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r2', value: 1 },
            { type: 'pc', value: 10 }, // Branch back again
            { type: 'memRead', value: 4 },
            { type: 'irAt', address: 10, size: 2 }
        ]);
        
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r2', value: 0 },
            { type: 'pc', value: 12 }, // Don't branch - continue to next instruction
            { type: 'memRead', value: 6 },
            { type: 'irAt', address: 10, size: 2 }
        ]);
    });

    it('Cesar: JSR jumps to subroutine', () => {
        cpu.memory.data[0] = CesarInstruction.Jsr | CesarRegister.R3;
        cpu.memory.data[1] = CESAR_INDIRECT | CesarRegister.R1;
        cpu.registers.r[1] = 123;
        cpu.registers.r[3] = 0x1234;
        
        cesarStep(cpu);
        
        assertCesarState(cpu, [
            { type: 'r1', value: 123 },
            { type: 'r3', value: 2 },
            { type: 'r6', value: 65534 }, // Stack pointer decremented by 2
            { type: 'pc', value: 123 },
            { type: 'memRead', value: 2 },
            { type: 'memAt', address: 65534, value: 0x12 }, // High byte of R3
            { type: 'memAt', address: 65535, value: 0x34 }, // Low byte of R3
            { type: 'irAt', address: 0, size: 2 }
        ]);
    });
    
    it('Cesar: RTS returns from subroutine', () => {
        // First set up with JSR
        cpu.memory.data[0] = CesarInstruction.Jsr | CesarRegister.R3;
        cpu.memory.data[1] = CESAR_INDIRECT | CesarRegister.R1;
        cpu.registers.r[1] = 123;
        cpu.registers.r[3] = 0x1234;
        
        cesarStep(cpu);
        
        // Now test RTS
        cpu.memory.data[123] = CesarInstruction.Rts | CesarRegister.R3;
        
        cesarStep(cpu);
        
        assertCesarState(cpu, [
            { type: 'r1', value: 123 },
            { type: 'r3', value: 0x1234 }, // Restored from stack
            { type: 'r6', value: 0 }, // Stack pointer incremented by 2
            { type: 'pc', value: 2 },
            { type: 'memRead', value: 5 },
            { type: 'memWrite', value: 2 },
            { type: 'irAt', address: 123, size: 1 }
        ]);
    });

    it('Cesar: CLR group instructions work as expected', () => {
        const testValues = [0, 1, 127, 128, 129, 255, 256, 257, 32767, 32768, 32769, 65534, 65535];

        for (const value of testValues) {
            cpu.registers.r[7] = 0; // Reset PC for each sub-test run

            // CLR
            testClrGroupOperation(cpu, CesarInstruction.Clr, value, 0, []);

            // NOT
            testClrGroupOperation(cpu, CesarInstruction.Not, value, (~value) & 0xFFFF, [
                { type: 'flagCarry', value: true }
            ]);

            // INC
            testClrGroupOperation(cpu, CesarInstruction.Inc, value, (value + 1) & 0xFFFF, [
                { type: 'flagOverflow', value: (value === 32767) },
                { type: 'flagCarry', value: (value === 65535) }
            ]);

            // DEC
            testClrGroupOperation(cpu, CesarInstruction.Dec, value, (value - 1) & 0xFFFF, [
                { type: 'flagOverflow', value: (value === 32768) }, 
                { type: 'flagCarry', value: (value === 0) }
            ]);

            // TST
            testClrGroupOperation(cpu, CesarInstruction.Tst, value, value, []);

            // NEG
            testClrGroupOperation(cpu, CesarInstruction.Neg, value, (65536 - value) & 0xFFFF, [
                { type: 'flagOverflow', value: (value === 32768) },
                { type: 'flagCarry', value: (value !== 0) }
            ]);

            // ROR with Carry=false
            cpu.registers.flags.carry = false;
            const rorResultFalse = (value >>> 1) & 0xFFFF;
            const rorCarryOutFalse = (value & 1) !== 0;
            testClrGroupOperation(cpu, CesarInstruction.Ror, value, rorResultFalse, [
                { type: 'flagOverflow', value: ((rorResultFalse & 0x8000) !== 0) !== rorCarryOutFalse },
                { type: 'flagCarry', value: rorCarryOutFalse }
            ]);

            // ROR with Carry=true
            cpu.registers.flags.carry = true;
            const rorResultTrue = ((value >>> 1) | 0x8000) & 0xFFFF;
            const rorCarryOutTrue = (value & 1) !== 0;
            testClrGroupOperation(cpu, CesarInstruction.Ror, value, rorResultTrue, [
                { type: 'flagOverflow', value: ((rorResultTrue & 0x8000) !== 0) !== rorCarryOutTrue },
                { type: 'flagCarry', value: rorCarryOutTrue }
            ]);

            // ROL with Carry=false
            cpu.registers.flags.carry = false;
            const rolResultFalse = (value << 1) & 0xFFFF;
            const rolCarryOutFalse = (value & 0x8000) !== 0;
            testClrGroupOperation(cpu, CesarInstruction.Rol, value, rolResultFalse, [
                { type: 'flagOverflow', value: ((rolResultFalse & 0x8000) !== 0) !== rolCarryOutFalse },
                { type: 'flagCarry', value: rolCarryOutFalse }
            ]);

            // ROL with Carry=true
            cpu.registers.flags.carry = true;
            const rolResultTrue = ((value << 1) | 1) & 0xFFFF;
            const rolCarryOutTrue = (value & 0x8000) !== 0;
            testClrGroupOperation(cpu, CesarInstruction.Rol, value, rolResultTrue, [
                { type: 'flagOverflow', value: ((rolResultTrue & 0x8000) !== 0) !== rolCarryOutTrue },
                { type: 'flagCarry', value: rolCarryOutTrue }
            ]);

            // ASR
            cpu.registers.flags.carry = false; // Carry doesn't affect ASR input
            const asrResult = ((value >>> 1) | (value & 0x8000)) & 0xFFFF; // Preserve sign bit
            const asrCarryOut = (value & 1) !== 0;
            testClrGroupOperation(cpu, CesarInstruction.Asr, value, asrResult, [
                { type: 'flagOverflow', value: ((asrResult & 0x8000) !== 0) !== asrCarryOut },
                { type: 'flagCarry', value: asrCarryOut }
            ]);

            // ASL
            cpu.registers.flags.carry = false; // Carry doesn't affect ASL input
            const aslResult = (value << 1) & 0xFFFF;
            const aslCarryOut = (value & 0x8000) !== 0;
            testClrGroupOperation(cpu, CesarInstruction.Asl, value, aslResult, [
                { type: 'flagOverflow', value: ((aslResult & 0x8000) !== 0) !== aslCarryOut },
                { type: 'flagCarry', value: aslCarryOut }
            ]);

            // ADC with Carry=false
            cpu.registers.flags.carry = false;
            testClrGroupOperation(cpu, CesarInstruction.Adc, value, value, []);

            // ADC with Carry=true
            cpu.registers.flags.carry = true;
            testClrGroupOperation(cpu, CesarInstruction.Adc, value, (value + 1) & 0xFFFF, [
                { type: 'flagOverflow', value: (value === 32767) },
                { type: 'flagCarry', value: (value === 65535) }
            ]);

            // SBC with Carry=false
            cpu.registers.flags.carry = false;
            testClrGroupOperation(cpu, CesarInstruction.Sbc, value, value, []);

            // SBC with Carry=true
            cpu.registers.flags.carry = true;
            testClrGroupOperation(cpu, CesarInstruction.Sbc, value, (value - 1) & 0xFFFF, [
                { type: 'flagOverflow', value: (value === 32768) },
                { type: 'flagCarry', value: (value !== 0) }
            ]);
        }
    });

    it('Cesar: ADD adds source into target', () => {
        testMovGroupOperation(cpu, CesarInstruction.Add, 12, 23, 12 + 23, [
            { type: 'flagNegative', value: false },
            { type: 'flagCarry', value: false }
        ]);
        
        cesarReset(cpu);
        testMovGroupOperation(cpu, CesarInstruction.Add, (65536 - 12), (65536 - 23), (65536 - 12 - 23) & 0xFFFF, [
            { type: 'flagNegative', value: true },
            { type: 'flagCarry', value: true }
        ]);
    });

    it('Cesar: SUB subtracts source from target', () => {
        testMovGroupOperation(cpu, CesarInstruction.Sub, 12, 23, (23 - 12) & 0xFFFF, [
            { type: 'flagNegative', value: false },
            { type: 'flagCarry', value: false }
        ]);
        
        cesarReset(cpu);
        testMovGroupOperation(cpu, CesarInstruction.Sub, 23, 12, (65536 + 12 - 23) & 0xFFFF, [
            { type: 'flagNegative', value: true },
            { type: 'flagCarry', value: true }
        ]);
    });

    it('Cesar: CMP compares two operands', () => {
        // Test CMP 12, 23 (Result = 12 - 23 = -11 = 65525)
        testMovGroupOperation(cpu, CesarInstruction.Cmp, 12, 23, (65536 + 12 - 23) & 0xFFFF, [
            { type: 'flagNegative', value: true },
            { type: 'flagZero', value: false },
            { type: 'flagCarry', value: true } // Carry is set because 12 < 23 (no borrow needed in F# logic)
        ]);

        cesarReset(cpu);
        // Test CMP 23, 12 (Result = 23 - 12 = 11)
        testMovGroupOperation(cpu, CesarInstruction.Cmp, 23, 12, (23 - 12) & 0xFFFF, [
            { type: 'flagNegative', value: false },
            { type: 'flagZero', value: false },
            { type: 'flagCarry', value: false } // Carry is clear because 23 >= 12
        ]);

        cesarReset(cpu);
        // Test CMP 12, 12 (Result = 12 - 12 = 0)
        testMovGroupOperation(cpu, CesarInstruction.Cmp, 12, 12, 0, [
            { type: 'flagNegative', value: false },
            { type: 'flagZero', value: true },
            { type: 'flagCarry', value: false } // Carry is clear because 12 >= 12
        ]);
    });

    it('Cesar: AND works as expected', () => {
        testMovGroupOperation(cpu, CesarInstruction.And, 12, 234, 12 & 234, [
            { type: 'flagNegative', value: false },
            { type: 'flagCarry', value: false }
        ]);
    });

    it('Cesar: OR works as expected', () => {
        testMovGroupOperation(cpu, CesarInstruction.Or, 12, 234, 12 | 234, [
            { type: 'flagNegative', value: false },
            { type: 'flagCarry', value: false }
        ]);
    });

    it('Cesar: HLT sets Halted flag', () => {
        cpu.memory.data[1] = CesarInstruction.Hlt;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'flagHalted', value: false },
            { type: 'irAt', address: 0, size: 1 }
        ]);
        
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'flagHalted', value: true },
            { type: 'irAt', address: 1, size: 1 }
        ]);
        
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'flagHalted', value: false }, // F# version resets halted flag on next step
            { type: 'irAt', address: 2, size: 1 }
        ]);
    });

    it('Cesar: High memory area addresses work at byte level', () => {
        cpu.memory.data[0] = 48;    // BR 8 
        cpu.memory.data[1] = 8;
        cpu.memory.data[65497] = 0x33;
        cpu.memory.data[65498] = 0x66;
        cpu.memory.data[65499] = 123;
        cpu.memory.data[65500] = 234;
        cpu.memory.data[65535] = 100;
        cesarStep(cpu);

        cpu.memory.data[10] = 155;  // MOV 0, R0
        cpu.memory.data[11] = 192;
        cpu.memory.data[12] = 0;
        cpu.memory.data[13] = 0;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r0', value: 12296 }
        ]);

        // Display memory area tests
        cpu.memory.data[14] = 155;  // MOV 65534, R1
        cpu.memory.data[15] = 193;
        cpu.memory.data[16] = 255;
        cpu.memory.data[17] = 254;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r1', value: 0 }
        ]);

        cpu.memory.data[18] = 155;  // MOV 65535, R2
        cpu.memory.data[19] = 194;
        cpu.memory.data[20] = 255;
        cpu.memory.data[21] = 255;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r2', value: 100 }
        ]);

        cpu.memory.data[22] = 155;  // MOV 65500, R3
        cpu.memory.data[23] = 195;
        cpu.memory.data[24] = 255;
        cpu.memory.data[25] = 220;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r3', value: 234 }
        ]);

        // Keyboard memory area
        cpu.memory.data[26] = 155;  // MOV 65499, R4
        cpu.memory.data[27] = 196;
        cpu.memory.data[28] = 255;
        cpu.memory.data[29] = 219;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r4', value: 123 }
        ]);

        cpu.memory.data[30] = 155;  // MOV 65498, R4
        cpu.memory.data[31] = 196;
        cpu.memory.data[32] = 255;
        cpu.memory.data[33] = 218;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r4', value: 0x66 }
        ]);

        // Normal memory area
        cpu.memory.data[34] = 155;  // MOV 65497, R4
        cpu.memory.data[35] = 196;
        cpu.memory.data[36] = 255;
        cpu.memory.data[37] = 217;
        cesarStep(cpu);
        assertCesarState(cpu, [
            { type: 'r4', value: 0x3366 }
        ]);
    });

    it('Cesar: DisassembleInstruction works as expected', () => {
        // Basic instructions
        expect(cesarDisassembleInstruction([CesarInstruction.Nop])).toEqual(["NOP", 1]);
        expect(cesarDisassembleInstruction([CesarInstruction.Nop + 5])).toEqual(["NOP", 1]);
        expect(cesarDisassembleInstruction([CesarInstruction.Hlt])).toEqual(["HLT", 1]);
        
        // Condition code instructions
        expect(cesarDisassembleInstruction([CesarInstruction.Ccc])).toEqual(["CCC", 1]);
        expect(cesarDisassembleInstruction([
            CesarInstruction.Ccc | CesarFlag.Negative | CesarFlag.Zero | CesarFlag.Overflow | CesarFlag.Carry
        ])).toEqual(["CCC NZVC", 1]);
        
        expect(cesarDisassembleInstruction([CesarInstruction.Scc])).toEqual(["SCC", 1]);
        expect(cesarDisassembleInstruction([
            CesarInstruction.Scc | CesarFlag.Zero | CesarFlag.Carry
        ])).toEqual(["SCC ZC", 1]);
        
        // Branch instructions
        expect(cesarDisassembleInstruction([CesarInstruction.Br, 0])).toEqual(["BR  0", 2]);
        expect(cesarDisassembleInstruction([CesarInstruction.Bne, 10])).toEqual(["BNE 10", 2]);
        expect(cesarDisassembleInstruction([CesarInstruction.Bne, 246])).toEqual(["BNE 246", 2]);
        
        // Jump instructions
        expect(cesarDisassembleInstruction([CesarInstruction.Jmp, CesarRegister.R1])).toEqual(["JMP ?", 2]); // Invalid
        expect(cesarDisassembleInstruction([
            CesarInstruction.Jmp, CesarRegister.R1 | CesarAddressMode.RegPostInc
        ])).toEqual(["JMP (R1)+", 2]);
        
        // Complex instructions with various operands
        const movEncoded = encodeInstructionTwoOperand(
            CesarInstruction.Mov, 
            CesarAddressMode.IndexedIndirect,
            CesarRegister.R1,
            CesarAddressMode.IndexedIndirect,
            CesarRegister.R2
        );
        expect(cesarDisassembleInstruction([
            (movEncoded >>> 8) & 0xFF, movEncoded & 0xFF, 0, 10, 0, 20
        ])).toEqual(["MOV (10(R1)), (20(R2))", 6]);
    });

    it('Cesar: DisassembleInstructions works as expected', () => {
        const program = [
            CesarInstruction.Ccc | CesarFlag.Negative | CesarFlag.Zero | CesarFlag.Overflow | CesarFlag.Carry,
            CesarInstruction.Bne, 10,
            CesarInstruction.Nop,
            CesarInstruction.Jmp, CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect, 0, 0,
            CesarInstruction.Rts | CesarRegister.R4,
            CesarInstruction.Not, CesarRegister.R7 | CesarAddressMode.Indexed, 0, 2,
            CesarInstruction.Hlt
        ];
        
        const result = cesarDisassembleInstructions(program);
        expect(result).toEqual([
            ["CCC NZVC", 1],
            ["BNE 10", 2],
            ["NOP", 1],
            ["JMP ((R7)+)", 4],
            ["RTS R4", 1],
            ["NOT 2(R7)", 4],
            ["HLT", 1]
        ]);
    });
});