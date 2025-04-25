import { describe, it, expect, beforeEach } from 'vitest';
import {
    NeanderCpu,
    createNeanderCpu,
    neanderStep,
    neanderReset,
    memoryLoad,
    NeanderInstruction,
    memoryReadByte,
    memoryWriteByte,
} from '../src/index';

// Helper function to load program bytes into memory
function loadProgram(cpu: NeanderCpu, program: number[]) {
    memoryLoad(cpu.memory, 0, program);
}

// Helper to get CPU state for assertions
function getCpuState(cpu: NeanderCpu) {
    return {
        pc: cpu.registers.programCounter,
        ac: cpu.registers.accumulator,
        zero: cpu.registers.flags.zero,
        negative: cpu.registers.flags.negative,
        halted: cpu.registers.flags.halted,
        mem: (addr: number) => memoryReadByte(cpu.memory, addr),
        memRead: cpu.memory.readCount,
        memWrite: cpu.memory.writeCount
    };
}

describe('Neander CPU', () => {
    let cpu: NeanderCpu;

    beforeEach(() => {
        cpu = createNeanderCpu();
    });

    it('should initialize with default values', () => {
        const state = getCpuState(cpu);
        expect(state.pc).toBe(0);
        expect(state.ac).toBe(0);
        expect(state.zero).toBe(true);
        expect(state.negative).toBe(false);
        expect(state.halted).toBe(false);
        expect(cpu.memory.data.length).toBe(256);
        expect(cpu.memory.data.every(byte => byte === 0)).toBe(true);
    });

    it('should reset correctly', () => {
        // Modify state
        cpu.registers.programCounter = 10;
        cpu.registers.accumulator = 20;
        cpu.registers.flags.zero = false;
        cpu.registers.flags.negative = true;
        cpu.registers.flags.halted = true;
        memoryLoad(cpu.memory, 0, [1, 2, 3]);

        neanderReset(cpu);

        const state = getCpuState(cpu);
        expect(state.pc).toBe(0);
        expect(state.ac).toBe(0);
        expect(state.zero).toBe(true);
        expect(state.negative).toBe(false);
        expect(state.halted).toBe(false);
        // Check if memory is also reset (assuming memoryReset clears it)
        expect(state.mem(0)).toBe(0);
        expect(state.mem(1)).toBe(0);
        expect(state.mem(2)).toBe(0);
    });

    it('should execute NOP', () => {
        loadProgram(cpu, [NeanderInstruction.Nop]);
        neanderStep(cpu);
        const state = getCpuState(cpu);
        expect(state.pc).toBe(1); // PC advances past NOP
        expect(state.ac).toBe(0);
        expect(state.halted).toBe(false);
        expect(state.memRead).toBe(1); // One memory read for opcode
        expect(state.memWrite).toBe(0); // No memory writes
    });

    it('should execute HLT', () => {
        loadProgram(cpu, [NeanderInstruction.Hlt]);
        neanderStep(cpu);
        const state = getCpuState(cpu);
        expect(state.pc).toBe(1); // PC advances past HLT
        expect(state.halted).toBe(true);
        expect(state.memRead).toBe(1); // One memory read for opcode
        expect(state.memWrite).toBe(0); // No memory writes
        
        // Stepping again should execute fetch+execute again (F# behavior)
        neanderStep(cpu);
        const newState = getCpuState(cpu);
        expect(newState.pc).toBe(2); // PC advances after second step
        expect(newState.memRead).toBe(2); // One more memory read
    });

    it('should execute STA', () => {
        cpu.registers.accumulator = 234;
        loadProgram(cpu, [
            NeanderInstruction.Sta, 0x0A
        ]);
        neanderStep(cpu); // STA 0x0A
        const state = getCpuState(cpu);
        expect(state.ac).toBe(234);
        expect(state.pc).toBe(2);
        expect(state.memRead).toBe(2); // F#: 2 reads (opcode + operand)
        expect(state.memWrite).toBe(1); // F#: 1 write
        expect(cpu.memory.data[0x0A]).toBe(234);
    });

    it('should execute ADD', () => {
        cpu.registers.accumulator = 12;
        cpu.memory.data[0] = NeanderInstruction.Add;
        cpu.memory.data[1] = 123;
        cpu.memory.data[123] = 23;
        neanderStep(cpu); // ADD 123
        const state = getCpuState(cpu);
        expect(state.ac).toBe(35);
        expect(state.pc).toBe(2);
        expect(state.memRead).toBe(3); // F#: 3 reads (opcode + operand + data)
        expect(state.memWrite).toBe(0);
    });

    it('should execute OR', () => {
        cpu.registers.accumulator = 234;
        cpu.memory.data[0] = NeanderInstruction.Or;
        cpu.memory.data[1] = 123;
        cpu.memory.data[123] = 12;
        neanderStep(cpu); // OR 123
        const state = getCpuState(cpu);
        expect(state.ac).toBe(234 | 12);
        expect(state.pc).toBe(2);
        expect(state.memRead).toBe(3); // F#: 3 reads
        expect(state.memWrite).toBe(0);
    });

    it('should execute AND', () => {
        cpu.registers.accumulator = 234;
        cpu.memory.data[0] = NeanderInstruction.And;
        cpu.memory.data[1] = 123;
        cpu.memory.data[123] = 12;
        neanderStep(cpu); // AND 123
        const state = getCpuState(cpu);
        expect(state.ac).toBe(234 & 12);
        expect(state.pc).toBe(2);
        expect(state.memRead).toBe(3); // F#: 3 reads
        expect(state.memWrite).toBe(0);
    });

    it('should execute NOT', () => {
        cpu.registers.accumulator = 85;
        cpu.memory.data[0] = NeanderInstruction.Not;
        neanderStep(cpu); // NOT
        const state = getCpuState(cpu);
        expect(state.ac).toBe((~85) & 0xFF);
        expect(state.pc).toBe(1);
        expect(state.memRead).toBe(1); // F#: 1 read
        expect(state.memWrite).toBe(0);
    });

    it('should execute JN when Negative flag is set', () => {
        cpu.registers.flags.negative = true;
        cpu.memory.data[0] = NeanderInstruction.Jn;
        cpu.memory.data[1] = 123;
        neanderStep(cpu); // JN 123
        const state = getCpuState(cpu);
        expect(state.pc).toBe(123);
        expect(state.memRead).toBe(2); // F#: 2 reads
        expect(state.memWrite).toBe(0);
    });

    it('should not execute JN when Negative flag is clear', () => {
        cpu.registers.flags.negative = false;
        cpu.memory.data[0] = NeanderInstruction.Jn;
        cpu.memory.data[1] = 123;
        neanderStep(cpu); // JN 123
        const state = getCpuState(cpu);
        expect(state.pc).toBe(2);
        expect(state.memRead).toBe(2); // F#: 2 reads
        expect(state.memWrite).toBe(0);
    });

    it('should execute JZ when Zero flag is set', () => {
        cpu.registers.flags.zero = true;
        cpu.memory.data[0] = NeanderInstruction.Jz;
        cpu.memory.data[1] = 123;
        neanderStep(cpu); // JZ 123
        const state = getCpuState(cpu);
        expect(state.pc).toBe(123);
        expect(state.memRead).toBe(2); // F#: 2 reads
        expect(state.memWrite).toBe(0);
    });

    it('should not execute JZ when Zero flag is clear', () => {
        cpu.registers.flags.zero = false;
        cpu.memory.data[0] = NeanderInstruction.Jz;
        cpu.memory.data[1] = 123;
        neanderStep(cpu); // JZ 123
        const state = getCpuState(cpu);
        expect(state.pc).toBe(2);
        expect(state.memRead).toBe(2); // F#: 2 reads
        expect(state.memWrite).toBe(0);
    });

    // Add more tests for edge cases, flag interactions, memory boundaries etc.
    // Example: Test loading value from last memory address
    it('should handle LDA from last memory address', () => {
        loadProgram(cpu, [NeanderInstruction.Lda, 0xFF, NeanderInstruction.Hlt]);
        memoryWriteByte(cpu.memory, 0xFF, 99);
        neanderStep(cpu); // LDA 0xFF
        expect(getCpuState(cpu).ac).toBe(99);
        expect(getCpuState(cpu).pc).toBe(2);
        neanderStep(cpu); // HLT
        expect(getCpuState(cpu).halted).toBe(true);
    });

    // Example: Test STA to last memory address
     it('should handle STA to last memory address', () => {
        // LDA #77, STA 0xFF, HLT
        loadProgram(cpu, [
            NeanderInstruction.Lda, 0x80, // Addr of 77
            NeanderInstruction.Sta, 0xFF,
            NeanderInstruction.Hlt
        ]);
         memoryWriteByte(cpu.memory, 0x80, 77);

        neanderStep(cpu); // LDA -> AC=77
        neanderStep(cpu); // STA 0xFF -> MEM[255]=77
        expect(getCpuState(cpu).mem(0xFF)).toBe(77);
        expect(getCpuState(cpu).pc).toBe(4);
        neanderStep(cpu); // HLT
        expect(getCpuState(cpu).halted).toBe(true);
    });

     // Example: Test PC wrapping (Reverted to match F# logic)
     it('should wrap Program Counter at 255', () => {
        // Set PC to the last address
        cpu.registers.programCounter = 255; // cpu.memory.data.length - 1

        // Execute one step. During the fetch phase of this step,
        // the instruction at address 255 will be read, and the PC
        // should increment and wrap around to 0.
        neanderStep(cpu);

        // Assert that the PC is now 0
        expect(getCpuState(cpu).pc).toBe(0);

        // Added: F# test checks for 1 read (fetching the instruction at 255)
        expect(getCpuState(cpu).memRead).toBe(1);
        expect(getCpuState(cpu).memWrite).toBe(0);
    });

    // Comprehensive flag test corresponding to F# "Flags Zero and Negative are set when Accumulator changes"
    it('should set Zero and Negative flags correctly for all possible accumulator values', () => {
        // Setup program with two NOT instructions in sequence
        loadProgram(cpu, [
            NeanderInstruction.Not,
            NeanderInstruction.Not
        ]);
        
        // Test all possible byte values (0-255)
        for (let i = 0; i <= 255; i++) {
            // Set accumulator to the test value
            cpu.registers.accumulator = i;
            // Reset PC to start of program
            cpu.registers.programCounter = 0;
            
            // First NOT instruction (~i)
            neanderStep(cpu);
            const notI = (~i) & 0xFF; // Bitwise NOT of i, constrained to 8 bits
            expect(getCpuState(cpu).ac).toBe(notI);
            expect(getCpuState(cpu).negative).toBe((notI & 0x80) !== 0); // Check if MSB is set
            expect(getCpuState(cpu).zero).toBe(notI === 0); // Check if result is zero
            
            // Second NOT instruction (~(~i) == i)
            neanderStep(cpu);
            expect(getCpuState(cpu).ac).toBe(i); // Should be back to original value
            expect(getCpuState(cpu).negative).toBe((i & 0x80) !== 0); // Check if MSB is set for original value
            expect(getCpuState(cpu).zero).toBe(i === 0); // Check if original value is zero
        }
    });
});
