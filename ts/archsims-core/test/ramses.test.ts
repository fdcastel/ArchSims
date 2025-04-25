// ts/archsims-core/test/ramses.test.ts

import { describe, it, expect, beforeEach } from 'vitest';
import {
    RamsesCpu, createRamsesCpu, ramsesReset, ramsesStep,
    RamsesInstruction, RamsesRegister, RamsesAddressMode, RamsesFlags,
    ramsesDisassembleInstruction, ramsesDisassembleInstructions
} from '../src/ramses';
import { memoryReadByte, memoryWriteByte } from '../src/memory';

// Helper type similar to F# test state definition
type RamsesStateCheck =
    | { type: 'ra', value: number }
    | { type: 'rb', value: number }
    | { type: 'rx', value: number }
    | { type: 'pc', value: number }
    | { type: 'memRead', value: number }
    | { type: 'memWrite', value: number }
    | { type: 'flagHalted', value: boolean }
    | { type: 'flagNegative', value: boolean }
    | { type: 'flagZero', value: boolean }
    | { type: 'flagCarry', value: boolean }
    | { type: 'memAt', address: number, value: number };

// Helper function to assert CPU state
function assertRamsesState(cpu: RamsesCpu, checks: RamsesStateCheck[]): void {
    for (const check of checks) {
        switch (check.type) {
            case 'ra': expect(cpu.registers.ra, 'Register Ra').toBe(check.value); break;
            case 'rb': expect(cpu.registers.rb, 'Register Rb').toBe(check.value); break;
            case 'rx': expect(cpu.registers.rx, 'Register Rx').toBe(check.value); break;
            case 'pc': expect(cpu.registers.pc, 'Program Counter').toBe(check.value); break;
            case 'memRead': expect(cpu.memory.readCount, 'Memory Reads').toBe(check.value); break;
            case 'memWrite': expect(cpu.memory.writeCount, 'Memory Writes').toBe(check.value); break;
            case 'flagHalted': expect(cpu.registers.flags.halted, 'Flag Halted').toBe(check.value); break;
            case 'flagNegative': expect(cpu.registers.flags.negative, 'Flag Negative').toBe(check.value); break;
            case 'flagZero': expect(cpu.registers.flags.zero, 'Flag Zero').toBe(check.value); break;
            case 'flagCarry': expect(cpu.registers.flags.carry, 'Flag Carry').toBe(check.value); break;
            case 'memAt': expect(memoryReadByte(cpu.memory, check.address), `Memory @ ${check.address}`).toBe(check.value); break;
        }
    }
}

// Helper to check initial clean state
function assertCpuStateIsClean(cpu: RamsesCpu): void {
    assertRamsesState(cpu, [
        { type: 'pc', value: 0 }, { type: 'ra', value: 0 }, { type: 'rb', value: 0 }, { type: 'rx', value: 0 },
        { type: 'flagHalted', value: false }, { type: 'flagNegative', value: false }, { type: 'flagZero', value: true }, { type: 'flagCarry', value: false },
        { type: 'memRead', value: 0 }, { type: 'memWrite', value: 0 }
    ]);
    expect(cpu.registers.instructionRegister.opCode, 'IR OpCode').toBe(0);
    expect(cpu.registers.instructionRegister.operandAddress, 'IR OperandAddress').toBe(0);
    // Check if all memory is zero
    for (let i = 0; i < cpu.memory.data.length; i++) {
        expect(cpu.memory.data[i], `Memory @ ${i} initial`).toBe(0);
    }
}

// Helper to write to registers directly for setup
function writeRegister(cpu: RamsesCpu, register: RamsesRegister, value: number): void {
    switch (register) {
        case RamsesRegister.Ra: cpu.registers.ra = value & 0xFF; break;
        case RamsesRegister.Rb: cpu.registers.rb = value & 0xFF; break;
        case RamsesRegister.Rx: cpu.registers.rx = value & 0xFF; break;
        case RamsesRegister.Pc: cpu.registers.pc = value & 0xFF; break;
    }
}

describe('Ramses CPU', () => {
    let cpu: RamsesCpu;

    beforeEach(() => {
        cpu = createRamsesCpu();
    });

    it('Ramses: New Cpu starts in clean state', () => {
        assertCpuStateIsClean(cpu);
    });

    it('Ramses: Program Counter wraps at end of memory', () => {
        cpu.registers.pc = cpu.memory.data.length - 1; // 255
        memoryWriteByte(cpu.memory, 255, RamsesInstruction.Nop); // Write NOP at last address
        ramsesStep(cpu);
        assertRamsesState(cpu, [{ type: 'pc', value: 0 }, { type: 'memRead', value: 1 }]);
    });

    it('Ramses: Reset() reverts to clean state', () => {
        writeRegister(cpu, RamsesRegister.Ra, 1);
        writeRegister(cpu, RamsesRegister.Rb, 2);
        writeRegister(cpu, RamsesRegister.Rx, 3);
        cpu.registers.pc = 1;
        memoryReadByte(cpu.memory, 1); // Simulate a read
        memoryWriteByte(cpu.memory, 1, RamsesInstruction.Hlt);
        ramsesStep(cpu);
        // State after HLT execution
        assertRamsesState(cpu, [
            { type: 'ra', value: 1 }, { type: 'rb', value: 2 }, { type: 'rx', value: 3 },
            { type: 'pc', value: 2 }, // PC advanced past HLT
            { type: 'memRead', value: 2 }, // Initial read + HLT fetch
            { type: 'memWrite', value: 1 }, // Write HLT
            { type: 'flagHalted', value: true } // Halted
        ]);

        ramsesReset(cpu);
        assertCpuStateIsClean(cpu);
    });

    it('Ramses: Flags Zero and Negative are set when a register changes', () => {
        const registers = [RamsesRegister.Ra, RamsesRegister.Rb, RamsesRegister.Rx];
        for (const reg of registers) {
            memoryWriteByte(cpu.memory, 0, RamsesInstruction.Not | reg); // NOT r
            memoryWriteByte(cpu.memory, 1, RamsesInstruction.Not | reg); // NOT r again (to revert)

            for (let i = 0; i <= 255; i++) {
                writeRegister(cpu, reg, i);
                cpu.registers.pc = 0;
                ramsesStep(cpu); // Execute first NOT
                const notI = (~i) & 0xFF;
                assertRamsesState(cpu, [
                    { type: 'flagNegative', value: (notI & 0x80) !== 0 },
                    { type: 'flagZero', value: notI === 0 }
                ]);

                // PC should be 1 now
                expect(cpu.registers.pc).toBe(1);
                ramsesStep(cpu); // Execute second NOT
                assertRamsesState(cpu, [
                    { type: 'flagNegative', value: (i & 0x80) !== 0 },
                    { type: 'flagZero', value: i === 0 }
                ]);
                 expect(cpu.registers.pc).toBe(2); // PC advanced past second NOT
            }
        }
    });

    it('Ramses: AddressModes works as expected (using LDR)', () => {
        // Direct
        ramsesReset(cpu);
        // Use direct memory manipulation to avoid write counts
        cpu.memory.data[0] = RamsesInstruction.Ldr | RamsesRegister.Ra | RamsesAddressMode.Direct;
        cpu.memory.data[1] = 123; // Operand: address 123
        cpu.memory.data[123] = 234; // Value at address 123
        ramsesStep(cpu);
        assertRamsesState(cpu, [{ type: 'ra', value: 234 }, { type: 'pc', value: 2 }, { type: 'memRead', value: 3 }]); // Fetch opcode, operand, value

        // Indirect
        ramsesReset(cpu);
        cpu.memory.data[0] = RamsesInstruction.Ldr | RamsesRegister.Rb | RamsesAddressMode.Indirect;
        cpu.memory.data[1] = 123; // Operand: address 123
        cpu.memory.data[123] = 234; // Address at address 123
        cpu.memory.data[234] = 245; // Value at address 234
        ramsesStep(cpu);
        assertRamsesState(cpu, [{ type: 'rb', value: 245 }, { type: 'pc', value: 2 }, { type: 'memRead', value: 4 }]); // Fetch opcode, operand, indirect addr, value

        // Immediate
        ramsesReset(cpu);
        cpu.memory.data[0] = RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate;
        cpu.memory.data[1] = 123; // Operand: value 123
        ramsesStep(cpu);
        assertRamsesState(cpu, [{ type: 'rx', value: 123 }, { type: 'pc', value: 2 }, { type: 'memRead', value: 3 }]); // Fetch opcode, operand, value from operand itself

        // Indexed
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Rx, 23); // Rx = 23
        cpu.memory.data[0] = RamsesInstruction.Ldr | RamsesRegister.Ra | RamsesAddressMode.Indexed;
        cpu.memory.data[1] = 123; // Operand: base address 123
        cpu.memory.data[146] = 234; // Value at address 123 + 23 = 146
        ramsesStep(cpu);
        assertRamsesState(cpu, [{ type: 'ra', value: 234 }, { type: 'rx', value: 23 }, { type: 'pc', value: 2 }, { type: 'memRead', value: 3 }]); // Fetch opcode, operand, value
    });

    it('Ramses: NOP does nothing', () => {
        cpu.memory.data[0] = RamsesInstruction.Nop;
        ramsesStep(cpu);
        assertRamsesState(cpu, [{ type: 'pc', value: 1 }, { type: 'memRead', value: 1 }, { type: 'memWrite', value: 0 }]);
        // Check registers and flags remain unchanged from initial state
        assertRamsesState(cpu, [
            { type: 'ra', value: 0 }, { type: 'rb', value: 0 }, { type: 'rx', value: 0 },
            { type: 'flagHalted', value: false }, { type: 'flagNegative', value: false }, { type: 'flagZero', value: true }, { type: 'flagCarry', value: false }
        ]);
    });

    it('Ramses: LDR loads value from memory into any register', () => {
        const registers = [RamsesRegister.Ra, RamsesRegister.Rb, RamsesRegister.Rx];
        for (const reg of registers) {
            ramsesReset(cpu);
            cpu.memory.data[0] = RamsesInstruction.Ldr | reg | RamsesAddressMode.Immediate;
            cpu.memory.data[1] = 123;
            ramsesStep(cpu);

            const checks: RamsesStateCheck[] = [
                { type: 'pc', value: 2 }, { type: 'memRead', value: 3 }, // Updated to 3 to match the F# behavior
                { type: 'ra', value: reg === RamsesRegister.Ra ? 123 : 0 },
                { type: 'rb', value: reg === RamsesRegister.Rb ? 123 : 0 },
                { type: 'rx', value: reg === RamsesRegister.Rx ? 123 : 0 },
                // Check flags based on loaded value 123 (0x7B)
                { type: 'flagNegative', value: false },
                { type: 'flagZero', value: false },
            ];
            assertRamsesState(cpu, checks);
        }
    });

    it('Ramses: STR stores value from any register into memory', () => {
        const registers = [RamsesRegister.Ra, RamsesRegister.Rb, RamsesRegister.Rx];
        for (const reg of registers) {
            ramsesReset(cpu);
            writeRegister(cpu, reg, 234); // Value to store
            // Use direct memory access to avoid incrementing write count
            cpu.memory.data[0] = RamsesInstruction.Str | reg | RamsesAddressMode.Direct;
            cpu.memory.data[1] = 123; // Target address
            cpu.memory.data[122] = 10; // Value before target
            cpu.memory.data[123] = 20; // Initial value at target
            cpu.memory.data[124] = 30; // Value after target
            ramsesStep(cpu);
            assertRamsesState(cpu, [
                { type: 'pc', value: 2 }, { type: 'memRead', value: 2 }, { type: 'memWrite', value: 1 },
                { type: 'memAt', address: 122, value: 10 },
                { type: 'memAt', address: 123, value: 234 }, // Value should be updated
                { type: 'memAt', address: 124, value: 30 }
            ]);
        }
    });

    it('Ramses: ADD works as expected', () => {
        // No carry
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Ra, 12);
        cpu.memory.data[0] = RamsesInstruction.Add | RamsesRegister.Ra | RamsesAddressMode.Immediate;
        cpu.memory.data[1] = 23;
        ramsesStep(cpu);
        assertRamsesState(cpu, [
            { type: 'ra', value: 12 + 23 }, { type: 'pc', value: 2 }, { type: 'memRead', value: 3 },
            { type: 'flagNegative', value: false }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: false }
        ]);

        // With carry
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Ra, 250); // 0xFA
        cpu.memory.data[0] = RamsesInstruction.Add | RamsesRegister.Ra | RamsesAddressMode.Immediate;
        cpu.memory.data[1] = 15; // 0x0F
        ramsesStep(cpu); // 250 + 15 = 265 = 0x109
        assertRamsesState(cpu, [
            { type: 'ra', value: 9 }, { type: 'pc', value: 2 }, { type: 'memRead', value: 3 },
            { type: 'flagNegative', value: false }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: true }
        ]);
    });

    it('Ramses: SUB works as expected', () => {
        // No borrow
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Ra, 23);
        cpu.memory.data[0] = RamsesInstruction.Sub | RamsesAddressMode.Immediate;
        cpu.memory.data[1] = 12;
        ramsesStep(cpu);
        assertRamsesState(cpu, [
            { type: 'ra', value: 23 - 12 }, { type: 'pc', value: 2 }, { type: 'memRead', value: 3 },
            { type: 'flagNegative', value: false }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: false }
        ]);

        // With borrow
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Ra, 12);
        cpu.memory.data[0] = RamsesInstruction.Sub | RamsesAddressMode.Immediate;
        cpu.memory.data[1] = 23;
        ramsesStep(cpu);
        assertRamsesState(cpu, [
            { type: 'ra', value: (256 + 12 - 23) & 0xFF }, { type: 'pc', value: 2 }, { type: 'memRead', value: 3 },
            { type: 'flagNegative', value: true }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: true }
        ]);
    });

    // Helper for jump tests
    function testJumpOperation(instruction: RamsesInstruction, condition: keyof RamsesFlags, shouldJumpWhen: boolean): void {
        // Case 1: Condition false, should not jump
        ramsesReset(cpu);
        cpu.registers.flags[condition] = !shouldJumpWhen;
        memoryWriteByte(cpu.memory, 0, instruction | RamsesAddressMode.Direct); // Use Direct mode for simplicity
        memoryWriteByte(cpu.memory, 1, 123); // Target address
        ramsesStep(cpu);
        assertRamsesState(cpu, [{ type: 'pc', value: 2 }, { type: 'memRead', value: 2 }]); // PC just advances

        // Case 2: Condition true, should jump
        ramsesReset(cpu);
        cpu.registers.flags[condition] = shouldJumpWhen;
        memoryWriteByte(cpu.memory, 0, instruction | RamsesAddressMode.Direct);
        memoryWriteByte(cpu.memory, 1, 123); // Target address
        ramsesStep(cpu);
        assertRamsesState(cpu, [{ type: 'pc', value: 123 }, { type: 'memRead', value: 2 }]); // PC jumps to 123
    }

    it('Ramses: JMP changes Program Counter', () => {
         ramsesReset(cpu);
         memoryWriteByte(cpu.memory, 0, RamsesInstruction.Jmp | RamsesAddressMode.Direct);
         memoryWriteByte(cpu.memory, 1, 123); // Target address
         ramsesStep(cpu);
         assertRamsesState(cpu, [{ type: 'pc', value: 123 }, { type: 'memRead', value: 2 }]);
    });

    it('Ramses: JN jumps only if Negative flag is set', () => {
        testJumpOperation(RamsesInstruction.Jn, 'negative', true);
    });

    it('Ramses: JZ jumps only if Zero flag is set', () => {
        testJumpOperation(RamsesInstruction.Jz, 'zero', true);
    });

    it('Ramses: JC jumps only if Carry flag is set', () => {
        testJumpOperation(RamsesInstruction.Jc, 'carry', true);
    });

    it('Ramses: JSR jumps and saves Program Counter', () => {
        ramsesReset(cpu);
        cpu.memory.data[0] = RamsesInstruction.Jsr;
        cpu.memory.data[1] = 123;
        cpu.memory.data[122] = 10;
        cpu.memory.data[123] = 20;
        cpu.memory.data[124] = 30;
        ramsesStep(cpu);
        assertRamsesState(cpu, [
            { type: 'pc', value: 124 }, { type: 'memRead', value: 2 }, { type: 'memWrite', value: 1 },
            { type: 'memAt', address: 122, value: 10 },
            { type: 'memAt', address: 123, value: 2 }, // Should store PC value (2) after instruction
            { type: 'memAt', address: 124, value: 30 }
        ]);
    });

    it('Ramses: NEG works as expected', () => {
        // Positive -> Negative
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Ra, 23);
        memoryWriteByte(cpu.memory, 0, RamsesInstruction.Neg | RamsesRegister.Ra);
        ramsesStep(cpu); // -23 = 233 (0xE9)
        assertRamsesState(cpu, [
            { type: 'ra', value: 233 }, { type: 'pc', value: 1 }, { type: 'memRead', value: 1 },
            { type: 'flagNegative', value: true }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: false } // Carry false for non-zero input
        ]);

        // Negative -> Positive
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Ra, 234); // -22
        memoryWriteByte(cpu.memory, 0, RamsesInstruction.Neg | RamsesRegister.Ra);
        ramsesStep(cpu); // -(-22) = 22
        assertRamsesState(cpu, [
            { type: 'ra', value: 22 }, { type: 'pc', value: 1 }, { type: 'memRead', value: 1 },
            { type: 'flagNegative', value: false }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: false }
        ]);

        // Max Negative -> Max Negative (Overflow case, but Ramses doesn't have Overflow flag)
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Ra, 128); // -128
        memoryWriteByte(cpu.memory, 0, RamsesInstruction.Neg | RamsesRegister.Ra);
        ramsesStep(cpu); // -(-128) = 128
        assertRamsesState(cpu, [
            { type: 'ra', value: 128 }, { type: 'pc', value: 1 }, { type: 'memRead', value: 1 },
            { type: 'flagNegative', value: true }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: false }
        ]);

        // Zero -> Zero (Carry set)
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Ra, 0);
        memoryWriteByte(cpu.memory, 0, RamsesInstruction.Neg | RamsesRegister.Ra);
        ramsesStep(cpu); // -0 = 0
        assertRamsesState(cpu, [
            { type: 'ra', value: 0 }, { type: 'pc', value: 1 }, { type: 'memRead', value: 1 },
            { type: 'flagNegative', value: false }, { type: 'flagZero', value: true }, { type: 'flagCarry', value: true } // Carry true for zero input
        ]);
    });

    it('Ramses: SHR works as expected', () => {
        // Shift 1, carry set
        ramsesReset(cpu);
        writeRegister(cpu, RamsesRegister.Ra, 0b01010101); // 85
        memoryWriteByte(cpu.memory, 0, RamsesInstruction.Shr | RamsesRegister.Ra);
        ramsesStep(cpu); // Result: 0b00101010 (42), Carry=1
        assertRamsesState(cpu, [
            { type: 'ra', value: 42 }, { type: 'pc', value: 1 }, { type: 'memRead', value: 1 },
            { type: 'flagNegative', value: false }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: true }
        ]);

        // Shift 0, carry clear
        memoryWriteByte(cpu.memory, 1, RamsesInstruction.Shr | RamsesRegister.Ra);
        ramsesStep(cpu); // Result: 0b00010101 (21), Carry=0
        assertRamsesState(cpu, [
            { type: 'ra', value: 21 }, { type: 'pc', value: 2 }, { type: 'memRead', value: 2 },
            { type: 'flagNegative', value: false }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: false }
        ]);

        // Shift 1, carry set
        memoryWriteByte(cpu.memory, 2, RamsesInstruction.Shr | RamsesRegister.Ra);
        ramsesStep(cpu); // Result: 0b00001010 (10), Carry=1
        assertRamsesState(cpu, [
            { type: 'ra', value: 10 }, { type: 'pc', value: 3 }, { type: 'memRead', value: 3 },
            { type: 'flagNegative', value: false }, { type: 'flagZero', value: false }, { type: 'flagCarry', value: true }
        ]);
    });

    it('Ramses: HLT sets Halted flag', () => {
        ramsesReset(cpu);
        cpu.memory.data[0] = RamsesInstruction.Nop; // NOP at 0
        cpu.memory.data[1] = RamsesInstruction.Hlt; // HLT at 1
        cpu.memory.data[2] = RamsesInstruction.Nop; // NOP at 2

        ramsesStep(cpu); // Execute NOP at 0
        assertRamsesState(cpu, [{ type: 'pc', value: 1 }, { type: 'flagHalted', value: false }]);

        ramsesStep(cpu); // Execute HLT at 1
        assertRamsesState(cpu, [{ type: 'pc', value: 2 }, { type: 'flagHalted', value: true }]);

        // In F# implementation, stepping a halted CPU still runs fetch/execute
        ramsesStep(cpu); // Should fetch and execute even though halted
        assertRamsesState(cpu, [{ type: 'pc', value: 3 }, { type: 'flagHalted', value: true }]);

        // Manually clear halt flag
        cpu.registers.flags.halted = false;
        ramsesStep(cpu); // Execute next instruction (at PC=3)
        assertRamsesState(cpu, [{ type: 'pc', value: 4 }, { type: 'flagHalted', value: false }]);
    });

    // --- Disassembly Tests ---

    it('Ramses: DisassembleInstruction works as expected', () => {
        expect(ramsesDisassembleInstruction([RamsesInstruction.Nop])).toEqual(["NOP", 1]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Nop | 5])).toEqual(["NOP", 1]); // Extra bits ignored for NOP
        expect(ramsesDisassembleInstruction([RamsesInstruction.Hlt])).toEqual(["HLT", 1]);

        expect(ramsesDisassembleInstruction([RamsesInstruction.Str | RamsesRegister.Ra | RamsesAddressMode.Direct, 12])).toEqual(["STR A, 12", 2]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Str | RamsesRegister.Rb | RamsesAddressMode.Indirect, 23])).toEqual(["STR B, 23,I", 2]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Str | RamsesRegister.Rx | RamsesAddressMode.Immediate, 34])).toEqual(["STR X, #34", 2]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Str | RamsesRegister.Ra | RamsesAddressMode.Indexed, 45])).toEqual(["STR A, 45,X", 2]);

        expect(ramsesDisassembleInstruction([RamsesInstruction.Not | RamsesRegister.Ra])).toEqual(["NOT A", 1]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Not | RamsesRegister.Rb])).toEqual(["NOT B", 1]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Not | RamsesRegister.Rx])).toEqual(["NOT X", 1]);

        expect(ramsesDisassembleInstruction([RamsesInstruction.Jmp | RamsesAddressMode.Direct, 12])).toEqual(["JMP 12", 2]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Jmp | RamsesAddressMode.Indirect, 23])).toEqual(["JMP 23,I", 2]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Jmp | RamsesAddressMode.Immediate, 34])).toEqual(["JMP #34", 2]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Jmp | RamsesAddressMode.Indexed, 45])).toEqual(["JMP 45,X", 2]);

        expect(ramsesDisassembleInstruction([RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate, 0])).toEqual(["LDR X, #0", 2]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate, 127])).toEqual(["LDR X, #127", 2]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate, 128])).toEqual(["LDR X, #128", 2]);
        expect(ramsesDisassembleInstruction([RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate, 255])).toEqual(["LDR X, #255", 2]);
    });

     it('Ramses: DisassembleInstructions works as expected', () => {
        const content = [
            RamsesInstruction.Str | RamsesRegister.Ra | RamsesAddressMode.Direct, 12,
            RamsesInstruction.Not | RamsesRegister.Rb,
            RamsesInstruction.Nop,
            RamsesInstruction.Jmp | RamsesAddressMode.Indirect, 23,
            RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate, 255,
            RamsesInstruction.Hlt
        ];

        const expected = [
            ["STR A, 12", 2],
            ["NOT B", 1],
            ["NOP", 1],
            ["JMP 23,I", 2],
            ["LDR X, #255", 2],
            ["HLT", 1]
        ];

        expect(ramsesDisassembleInstructions(content)).toEqual(expected);
    });

});
