import { describe, expect, it } from 'vitest';
import {
  RamsesAddressMode,
  RamsesInstruction,
  RamsesRegister,
  createRamsesCpu,
  ramsesDisassembleInstruction,
  ramsesStep,
} from '@/core/ramses';

const encode = (instruction: number, register = 0, mode = 0): number =>
  instruction | register | mode;

describe('ramses smoke', () => {
  it('starts clean with three registers at zero', () => {
    const cpu = createRamsesCpu();
    expect(cpu.registers.ra).toBe(0);
    expect(cpu.registers.rb).toBe(0);
    expect(cpu.registers.rx).toBe(0);
    expect(cpu.registers.flags.zero).toBe(true);
    expect(cpu.memory.data.length).toBe(256);
  });

  it('LDR Rb, immediate loads the literal', () => {
    const cpu = createRamsesCpu();
    cpu.memory.data[0] = encode(RamsesInstruction.Ldr, RamsesRegister.Rb, RamsesAddressMode.Immediate);
    cpu.memory.data[1] = 42;
    ramsesStep(cpu);
    expect(cpu.registers.rb).toBe(42);
  });

  it('LDR Ra, indexed adds Rx to operand', () => {
    const cpu = createRamsesCpu();
    cpu.registers.rx = 3;
    cpu.memory.data[0] = encode(RamsesInstruction.Ldr, RamsesRegister.Ra, RamsesAddressMode.Indexed);
    cpu.memory.data[1] = 100;
    cpu.memory.data[103] = 0x55;
    ramsesStep(cpu);
    expect(cpu.registers.ra).toBe(0x55);
  });

  it('SUB sets Carry as borrow (Carry=1 means underflow)', () => {
    const cpu = createRamsesCpu();
    cpu.registers.ra = 5;
    cpu.memory.data[0] = encode(RamsesInstruction.Sub, RamsesRegister.Ra, RamsesAddressMode.Immediate);
    cpu.memory.data[1] = 10;
    ramsesStep(cpu);
    expect(cpu.registers.ra).toBe((5 - 10 + 256) & 0xff);
    expect(cpu.registers.flags.carry).toBe(true);
  });

  it('JSR writes return PC into the called address and jumps to addr+1', () => {
    const cpu = createRamsesCpu();
    cpu.memory.data[0] = encode(RamsesInstruction.Jsr, 0, RamsesAddressMode.Direct);
    cpu.memory.data[1] = 100;
    ramsesStep(cpu);
    expect(cpu.memory.data[100]).toBe(2); // PC after fetch was 2
    expect(cpu.registers.programCounter).toBe(101);
  });

  it('disassembles an indexed STR', () => {
    const word = encode(RamsesInstruction.Str, RamsesRegister.Rx, RamsesAddressMode.Indexed);
    const { text, size } = ramsesDisassembleInstruction([word, 50]);
    expect(text).toBe('STR X 50,X');
    expect(size).toBe(2);
  });
});
