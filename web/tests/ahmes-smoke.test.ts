import { describe, expect, it } from 'vitest';
import { AhmesInstruction, ahmesStep, createAhmesCpu } from '@/core/ahmes';

describe('ahmes smoke', () => {
  it('starts clean with all flags at defaults', () => {
    const cpu = createAhmesCpu();
    const f = cpu.registers.flags;
    expect(f.halted).toBe(false);
    expect(f.negative).toBe(false);
    expect(f.zero).toBe(true);
    expect(f.carry).toBe(false);
    expect(f.overflow).toBe(false);
    expect(f.borrow).toBe(false);
  });

  it('SUB sets Borrow but not Carry', () => {
    const cpu = createAhmesCpu();
    cpu.registers.accumulator = 10;
    cpu.memory.data[0] = AhmesInstruction.Sub;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 30;
    ahmesStep(cpu);
    expect(cpu.registers.accumulator).toBe(236);
    expect(cpu.registers.flags.borrow).toBe(true);
    expect(cpu.registers.flags.carry).toBe(false);
  });

  it('ADD with overflow positive+positive=negative', () => {
    const cpu = createAhmesCpu();
    cpu.registers.accumulator = 0x7f;
    cpu.memory.data[0] = AhmesInstruction.Add;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 0x01;
    ahmesStep(cpu);
    expect(cpu.registers.accumulator).toBe(0x80);
    expect(cpu.registers.flags.overflow).toBe(true);
    expect(cpu.registers.flags.carry).toBe(false);
  });

  it('SHR carries out LSB and zero-fills MSB', () => {
    const cpu = createAhmesCpu();
    cpu.registers.accumulator = 0x03;
    cpu.memory.data[0] = AhmesInstruction.Shr;
    ahmesStep(cpu);
    expect(cpu.registers.accumulator).toBe(0x01);
    expect(cpu.registers.flags.carry).toBe(true);
  });

  it('ROR rotates through carry across 9 steps and returns to start', () => {
    const cpu = createAhmesCpu();
    cpu.registers.accumulator = 0xb5;
    cpu.registers.flags.carry = false;
    for (let i = 0; i < 9; i++) {
      cpu.memory.data[i] = AhmesInstruction.Ror;
    }
    for (let i = 0; i < 9; i++) {
      ahmesStep(cpu);
    }
    expect(cpu.registers.accumulator).toBe(0xb5);
    expect(cpu.registers.flags.carry).toBe(false);
  });

  it('Logical OR leaves C, V, B untouched', () => {
    const cpu = createAhmesCpu();
    cpu.registers.accumulator = 0x0f;
    cpu.registers.flags.carry = true;
    cpu.registers.flags.overflow = true;
    cpu.registers.flags.borrow = true;
    cpu.memory.data[0] = AhmesInstruction.Or;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 0xf0;
    ahmesStep(cpu);
    expect(cpu.registers.accumulator).toBe(0xff);
    expect(cpu.registers.flags.carry).toBe(true);
    expect(cpu.registers.flags.overflow).toBe(true);
    expect(cpu.registers.flags.borrow).toBe(true);
  });
});
