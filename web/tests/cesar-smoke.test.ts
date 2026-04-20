import { describe, expect, it } from 'vitest';
import {
  CesarAddressMode,
  CesarDisplayMemoryAddress,
  CesarFlagBit,
  CesarInstruction,
  CesarRegister,
  cesarStep,
  createCesarCpu,
  encodeCesarInstructionOneOperand,
  encodeCesarInstructionTwoOperand,
} from '@/core/cesar';

const writeWord = (data: Uint8Array, address: number, value: number): void => {
  data[address] = (value >>> 8) & 0xff;
  data[address + 1] = value & 0xff;
};

describe('cesar smoke', () => {
  it('starts clean with 64 KiB memory and Z=true', () => {
    const cpu = createCesarCpu();
    expect(cpu.memory.data.length).toBe(65536);
    expect(Array.from(cpu.registers.r).every((v) => v === 0)).toBe(true);
    expect(cpu.registers.flags.zero).toBe(true);
  });

  it('NOP advances PC by 1', () => {
    const cpu = createCesarCpu();
    cpu.memory.data[0] = CesarInstruction.Nop;
    cesarStep(cpu);
    expect(cpu.registers.r[7]).toBe(1);
  });

  it('SCC sets flags by mask, CCC clears them', () => {
    const cpu = createCesarCpu();
    cpu.memory.data[0] = CesarInstruction.Scc | (CesarFlagBit.Negative | CesarFlagBit.Carry);
    cpu.memory.data[1] = CesarInstruction.Ccc | CesarFlagBit.Carry;
    cesarStep(cpu);
    expect(cpu.registers.flags.negative).toBe(true);
    expect(cpu.registers.flags.carry).toBe(true);
    cesarStep(cpu);
    expect(cpu.registers.flags.negative).toBe(true);
    expect(cpu.registers.flags.carry).toBe(false);
  });

  it('MOV #5, R0 loads a literal word via (R7)+ mode', () => {
    const cpu = createCesarCpu();
    const word = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegPostInc,
      CesarRegister.R7,
      CesarAddressMode.Register,
      CesarRegister.R0,
    );
    writeWord(cpu.memory.data, 0, word);
    writeWord(cpu.memory.data, 2, 5);
    cesarStep(cpu);
    expect(cpu.registers.r[0]).toBe(5);
    expect(cpu.registers.r[7]).toBe(4);
  });

  it('ADD R0, R1 sums register values', () => {
    const cpu = createCesarCpu();
    cpu.registers.r[0] = 10;
    cpu.registers.r[1] = 20;
    const word = encodeCesarInstructionTwoOperand(
      CesarInstruction.Add,
      CesarAddressMode.Register,
      CesarRegister.R0,
      CesarAddressMode.Register,
      CesarRegister.R1,
    );
    writeWord(cpu.memory.data, 0, word);
    cesarStep(cpu);
    expect(cpu.registers.r[1]).toBe(30);
  });

  it('BR +4 advances PC past the branch operand by the offset', () => {
    const cpu = createCesarCpu();
    cpu.memory.data[0] = CesarInstruction.Br;
    cpu.memory.data[1] = 4;
    cesarStep(cpu);
    expect(cpu.registers.r[7]).toBe(2 + 4);
  });

  it('JSR R7, addr / RTS R7 round-trips through the stack', () => {
    const cpu = createCesarCpu();
    cpu.registers.r[6] = 0x100;
    // ((R7)+) = "Direct" mode in Cesar — operand is the inline 16-bit address.
    const jsr = encodeCesarInstructionOneOperand(
      CesarInstruction.Jsr | CesarRegister.R7,
      CesarAddressMode.RegPostIncIndirect,
      CesarRegister.R7,
    );
    writeWord(cpu.memory.data, 0, jsr);
    writeWord(cpu.memory.data, 2, 0x50);
    cpu.memory.data[0x50] = CesarInstruction.Rts | CesarRegister.R7;
    cesarStep(cpu); // JSR
    expect(cpu.registers.r[7]).toBe(0x50);
    expect(cpu.registers.r[6]).toBe(0xfe);
    cesarStep(cpu); // RTS
    expect(cpu.registers.r[7]).toBe(4);
    expect(cpu.registers.r[6]).toBe(0x100);
  });

  it('writes to the display area as bytes, not words', () => {
    const cpu = createCesarCpu();
    // MOV #'H', @0xFFDC — source = (R7)+ (immediate), target = ((R7)+) (direct).
    const word = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegPostInc,
      CesarRegister.R7,
      CesarAddressMode.RegPostIncIndirect,
      CesarRegister.R7,
    );
    writeWord(cpu.memory.data, 0, word);
    writeWord(cpu.memory.data, 2, 0x0048); // 'H' (low byte)
    writeWord(cpu.memory.data, 4, CesarDisplayMemoryAddress);
    cesarStep(cpu);
    expect(cpu.memory.data[CesarDisplayMemoryAddress]).toBe(0x48);
    expect(cpu.memory.data[CesarDisplayMemoryAddress + 1]).toBe(0); // not overwritten
  });
});
