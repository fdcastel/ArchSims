import { describe, expect, it } from 'vitest';
import {
  createNeanderCpu,
  NeanderInstruction,
  neanderStep,
} from '@/core/neander';

describe('neander smoke', () => {
  it('starts in clean state with Zero=true', () => {
    const cpu = createNeanderCpu();
    expect(cpu.registers.programCounter).toBe(0);
    expect(cpu.registers.accumulator).toBe(0);
    expect(cpu.registers.flags.halted).toBe(false);
    expect(cpu.registers.flags.negative).toBe(false);
    expect(cpu.registers.flags.zero).toBe(true);
    expect(cpu.memory.data.length).toBe(256);
  });

  it('PC wraps at end of memory', () => {
    const cpu = createNeanderCpu();
    cpu.registers.programCounter = 255;
    neanderStep(cpu);
    expect(cpu.registers.programCounter).toBe(0);
  });

  it('LDA loads memory into accumulator', () => {
    const cpu = createNeanderCpu();
    cpu.memory.data[0] = NeanderInstruction.Lda;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 234;
    neanderStep(cpu);
    expect(cpu.registers.accumulator).toBe(234);
    expect(cpu.registers.flags.negative).toBe(true);
    expect(cpu.registers.flags.zero).toBe(false);
  });

  it('ADD wraps at byte boundary', () => {
    const cpu = createNeanderCpu();
    cpu.registers.accumulator = 0xff;
    cpu.memory.data[0] = NeanderInstruction.Add;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 0x02;
    neanderStep(cpu);
    expect(cpu.registers.accumulator).toBe(0x01);
  });

  it('NOT inverts accumulator', () => {
    const cpu = createNeanderCpu();
    cpu.registers.accumulator = 0x55;
    cpu.memory.data[0] = NeanderInstruction.Not;
    neanderStep(cpu);
    expect(cpu.registers.accumulator).toBe(0xaa);
  });

  it('HLT sets halted, next NOP clears it', () => {
    const cpu = createNeanderCpu();
    cpu.memory.data[0] = NeanderInstruction.Hlt;
    cpu.memory.data[1] = NeanderInstruction.Nop;
    neanderStep(cpu);
    expect(cpu.registers.flags.halted).toBe(true);
    neanderStep(cpu);
    expect(cpu.registers.flags.halted).toBe(false);
  });
});
