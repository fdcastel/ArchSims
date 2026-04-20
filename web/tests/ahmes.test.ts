import { beforeEach, describe, expect, it } from 'vitest';
import {
  AhmesInstruction,
  ahmesReset,
  ahmesStep,
  createAhmesCpu,
  type AhmesCpu,
} from '@/core/ahmes';
import { memoryReadByte, memoryWriteByte } from '@/core/memory';
import { expectAhmesState } from './helpers';

const expectClean = (cpu: AhmesCpu): void => {
  expectAhmesState(cpu, {
    programCounter: 0,
    accumulator: 0,
    halted: false,
    negative: false,
    zero: true,
    carry: false,
    overflow: false,
    borrow: false,
    memoryReads: 0,
    memoryWrites: 0,
  });
  expect(cpu.registers.instructionRegister).toEqual({ opCode: 0, operandAddress: 0 });
  for (const byte of cpu.memory.data) expect(byte).toBe(0);
};

const stepJump = (cpu: AhmesCpu, instruction: number, jumpExpected: boolean): void => {
  cpu.memory.data[0] = instruction;
  cpu.memory.data[1] = 123;
  ahmesStep(cpu);
  expectAhmesState(cpu, {
    programCounter: jumpExpected ? 123 : 2,
    memoryReads: 2,
  });
};

describe('ahmes', () => {
  let cpu: AhmesCpu;

  beforeEach(() => {
    cpu = createAhmesCpu();
  });

  it('new CPU starts in clean state', () => {
    expectClean(cpu);
  });

  it('Program Counter wraps at end of memory', () => {
    cpu.registers.programCounter = cpu.memory.data.length - 1;
    ahmesStep(cpu);
    expectAhmesState(cpu, { programCounter: 0, memoryReads: 1 });
  });

  it('Reset reverts to clean state', () => {
    cpu.registers.accumulator = 1;
    cpu.registers.programCounter = 1;
    cpu.registers.flags.carry = true;
    cpu.registers.flags.overflow = true;
    cpu.registers.flags.borrow = true;
    memoryReadByte(cpu.memory, 1);
    memoryWriteByte(cpu.memory, 1, AhmesInstruction.Hlt);
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 1,
      programCounter: 2,
      memoryReads: 2,
      memoryWrites: 1,
    });
    ahmesReset(cpu);
    expectClean(cpu);
  });

  it('Flags Zero and Negative are set when Accumulator changes', () => {
    cpu.memory.data[0] = AhmesInstruction.Not;
    cpu.memory.data[1] = AhmesInstruction.Not;
    for (let i = 0; i <= 0xff; i++) {
      cpu.registers.accumulator = i;
      cpu.registers.programCounter = 0;
      ahmesStep(cpu);
      const noti = ~i & 0xff;
      expectAhmesState(cpu, {
        negative: noti > 127,
        zero: noti === 0,
      });
      ahmesStep(cpu);
      expectAhmesState(cpu, {
        negative: i > 127,
        zero: i === 0,
      });
    }
  });

  it('NOP does nothing', () => {
    cpu.memory.data[0] = AhmesInstruction.Nop;
    ahmesStep(cpu);
    expectAhmesState(cpu, { programCounter: 1, memoryReads: 1 });
  });

  it('LDA loads value from memory into Accumulator', () => {
    cpu.memory.data[0] = AhmesInstruction.Lda;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 234;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 234,
      programCounter: 2,
      memoryReads: 3,
      negative: true,
      zero: false,
    });
  });

  it('STA stores value from Accumulator into memory', () => {
    cpu.registers.accumulator = 234;
    cpu.memory.data[0] = AhmesInstruction.Sta;
    cpu.memory.data[1] = 123;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 234,
      programCounter: 2,
      memoryReads: 2,
      memoryWrites: 1,
      memoryAt: [[123, 234]],
    });
  });

  it('ADD without carry', () => {
    cpu.registers.accumulator = 12;
    cpu.memory.data[0] = AhmesInstruction.Add;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 23;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 12 + 23,
      programCounter: 2,
      memoryReads: 3,
      negative: false,
      zero: false,
      carry: false,
      overflow: false,
    });
  });

  it('ADD with unsigned carry (no signed overflow)', () => {
    cpu.registers.accumulator = 0xff;
    cpu.memory.data[0] = AhmesInstruction.Add;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 0x02;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x01,
      carry: true,
      overflow: false,
      negative: false,
      zero: false,
    });
  });

  it('ADD with signed overflow (positive + positive -> negative)', () => {
    cpu.registers.accumulator = 0x7f;
    cpu.memory.data[0] = AhmesInstruction.Add;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 0x01;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x80,
      overflow: true,
      carry: false,
      negative: true,
      zero: false,
    });
  });

  it('ADD with signed overflow (negative + negative -> positive)', () => {
    cpu.registers.accumulator = 0x80;
    cpu.memory.data[0] = AhmesInstruction.Add;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 0xff;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x7f,
      overflow: true,
      carry: true,
      negative: false,
      zero: false,
    });
  });

  it('ADD yielding zero sets Zero flag and Carry', () => {
    cpu.registers.accumulator = 0xff;
    cpu.memory.data[0] = AhmesInstruction.Add;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 0x01;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x00,
      zero: true,
      carry: true,
      overflow: false,
      negative: false,
    });
  });

  it('OR works as expected', () => {
    cpu.registers.accumulator = 234;
    cpu.memory.data[0] = AhmesInstruction.Or;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 12;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 234 | 12,
      programCounter: 2,
      memoryReads: 3,
    });
  });

  it('AND works as expected', () => {
    cpu.registers.accumulator = 234;
    cpu.memory.data[0] = AhmesInstruction.And;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 12;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 234 & 12,
      programCounter: 2,
      memoryReads: 3,
    });
  });

  it('NOT works as expected', () => {
    cpu.registers.accumulator = 85;
    cpu.memory.data[0] = AhmesInstruction.Not;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 170,
      programCounter: 1,
      memoryReads: 1,
    });
  });

  it('Logical ops do not touch C, V, B', () => {
    cpu.registers.accumulator = 0x0f;
    cpu.registers.flags.carry = true;
    cpu.registers.flags.overflow = true;
    cpu.registers.flags.borrow = true;
    cpu.memory.data[0] = AhmesInstruction.Or;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 0xf0;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0xff,
      negative: true,
      zero: false,
      carry: true,
      overflow: true,
      borrow: true,
    });
  });

  it('SUB without borrow', () => {
    cpu.registers.accumulator = 30;
    cpu.memory.data[0] = AhmesInstruction.Sub;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 10;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 20,
      programCounter: 2,
      memoryReads: 3,
      borrow: false,
      overflow: false,
      negative: false,
      zero: false,
    });
  });

  it('SUB producing borrow', () => {
    cpu.registers.accumulator = 10;
    cpu.memory.data[0] = AhmesInstruction.Sub;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 30;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 236,
      borrow: true,
      overflow: false,
      negative: true,
      zero: false,
    });
  });

  it('SUB with signed overflow (pos - neg -> neg)', () => {
    cpu.registers.accumulator = 0x7f;
    cpu.memory.data[0] = AhmesInstruction.Sub;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 0x80;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0xff,
      overflow: true,
      borrow: true,
      negative: true,
      zero: false,
    });
  });

  it('SUB yielding zero sets Zero', () => {
    cpu.registers.accumulator = 5;
    cpu.memory.data[0] = AhmesInstruction.Sub;
    cpu.memory.data[1] = 10;
    cpu.memory.data[10] = 5;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0,
      zero: true,
      negative: false,
      borrow: false,
      overflow: false,
    });
  });

  it('JMP changes Program Counter', () => {
    stepJump(cpu, AhmesInstruction.Jmp, true);
  });

  it('JN jumps only if Negative flag is set', () => {
    cpu.registers.flags.negative = false;
    stepJump(cpu, AhmesInstruction.Jn, false);
    ahmesReset(cpu);
    cpu.registers.flags.negative = true;
    stepJump(cpu, AhmesInstruction.Jn, true);
  });

  it('JP jumps only if Negative flag is clear', () => {
    cpu.registers.flags.negative = true;
    stepJump(cpu, AhmesInstruction.Jp, false);
    ahmesReset(cpu);
    cpu.registers.flags.negative = false;
    stepJump(cpu, AhmesInstruction.Jp, true);
  });

  it('JZ jumps only if Zero flag is set', () => {
    cpu.registers.flags.zero = false;
    stepJump(cpu, AhmesInstruction.Jz, false);
    ahmesReset(cpu);
    cpu.registers.flags.zero = true;
    stepJump(cpu, AhmesInstruction.Jz, true);
  });

  it('JNZ jumps only if Zero flag is clear', () => {
    cpu.registers.flags.zero = true;
    stepJump(cpu, AhmesInstruction.Jnz, false);
    ahmesReset(cpu);
    cpu.registers.flags.zero = false;
    stepJump(cpu, AhmesInstruction.Jnz, true);
  });

  it('JC jumps only if Carry flag is set', () => {
    cpu.registers.flags.carry = false;
    stepJump(cpu, AhmesInstruction.Jc, false);
    ahmesReset(cpu);
    cpu.registers.flags.carry = true;
    stepJump(cpu, AhmesInstruction.Jc, true);
  });

  it('JNC jumps only if Carry flag is clear', () => {
    cpu.registers.flags.carry = true;
    stepJump(cpu, AhmesInstruction.Jnc, false);
    ahmesReset(cpu);
    cpu.registers.flags.carry = false;
    stepJump(cpu, AhmesInstruction.Jnc, true);
  });

  it('JV jumps only if Overflow flag is set', () => {
    cpu.registers.flags.overflow = false;
    stepJump(cpu, AhmesInstruction.Jv, false);
    ahmesReset(cpu);
    cpu.registers.flags.overflow = true;
    stepJump(cpu, AhmesInstruction.Jv, true);
  });

  it('JNV jumps only if Overflow flag is clear', () => {
    cpu.registers.flags.overflow = true;
    stepJump(cpu, AhmesInstruction.Jnv, false);
    ahmesReset(cpu);
    cpu.registers.flags.overflow = false;
    stepJump(cpu, AhmesInstruction.Jnv, true);
  });

  it('JB jumps only if Borrow flag is set', () => {
    cpu.registers.flags.borrow = false;
    stepJump(cpu, AhmesInstruction.Jb, false);
    ahmesReset(cpu);
    cpu.registers.flags.borrow = true;
    stepJump(cpu, AhmesInstruction.Jb, true);
  });

  it('JNB jumps only if Borrow flag is clear', () => {
    cpu.registers.flags.borrow = true;
    stepJump(cpu, AhmesInstruction.Jnb, false);
    ahmesReset(cpu);
    cpu.registers.flags.borrow = false;
    stepJump(cpu, AhmesInstruction.Jnb, true);
  });

  it('SHR shifts right, zero-fills top, carries LSB', () => {
    cpu.registers.accumulator = 0x03;
    cpu.memory.data[0] = AhmesInstruction.Shr;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x01,
      carry: true,
      negative: false,
      zero: false,
      programCounter: 1,
      memoryReads: 1,
    });

    ahmesReset(cpu);
    cpu.registers.accumulator = 0x80;
    cpu.memory.data[0] = AhmesInstruction.Shr;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x40,
      carry: false,
      negative: false,
      zero: false,
    });
  });

  it('SHR yielding zero', () => {
    cpu.registers.accumulator = 0x01;
    cpu.memory.data[0] = AhmesInstruction.Shr;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x00,
      carry: true,
      zero: true,
      negative: false,
    });
  });

  it('SHL shifts left, zero-fills bottom, carries MSB', () => {
    cpu.registers.accumulator = 0x81;
    cpu.memory.data[0] = AhmesInstruction.Shl;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x02,
      carry: true,
      negative: false,
      zero: false,
      programCounter: 1,
      memoryReads: 1,
    });

    ahmesReset(cpu);
    cpu.registers.accumulator = 0x40;
    cpu.memory.data[0] = AhmesInstruction.Shl;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x80,
      carry: false,
      negative: true,
      zero: false,
    });
  });

  it('SHL yielding zero', () => {
    cpu.registers.accumulator = 0x80;
    cpu.memory.data[0] = AhmesInstruction.Shl;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x00,
      carry: true,
      zero: true,
      negative: false,
    });
  });

  it('ROR rotates right through carry', () => {
    cpu.registers.accumulator = 0x01;
    cpu.registers.flags.carry = false;
    cpu.memory.data[0] = AhmesInstruction.Ror;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x00,
      carry: true,
      zero: true,
      negative: false,
    });

    cpu.memory.data[1] = AhmesInstruction.Ror;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x80,
      carry: false,
      negative: true,
      zero: false,
    });
  });

  it('ROR preserves all bits across 9 rotations', () => {
    cpu.registers.accumulator = 0xb5;
    cpu.registers.flags.carry = false;
    for (let i = 0; i <= 8; i++) {
      cpu.memory.data[i] = AhmesInstruction.Ror;
      ahmesStep(cpu);
    }
    expectAhmesState(cpu, { accumulator: 0xb5, carry: false });
  });

  it('ROL rotates left through carry', () => {
    cpu.registers.accumulator = 0x80;
    cpu.registers.flags.carry = false;
    cpu.memory.data[0] = AhmesInstruction.Rol;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x00,
      carry: true,
      zero: true,
      negative: false,
    });

    cpu.memory.data[1] = AhmesInstruction.Rol;
    ahmesStep(cpu);
    expectAhmesState(cpu, {
      accumulator: 0x01,
      carry: false,
      negative: false,
      zero: false,
    });
  });

  it('ROL preserves all bits across 9 rotations', () => {
    cpu.registers.accumulator = 0xb5;
    cpu.registers.flags.carry = true;
    for (let i = 0; i <= 8; i++) {
      cpu.memory.data[i] = AhmesInstruction.Rol;
      ahmesStep(cpu);
    }
    expectAhmesState(cpu, { accumulator: 0xb5, carry: true });
  });

  it('HLT sets Halted flag, clears on next non-HLT step', () => {
    cpu.memory.data[1] = AhmesInstruction.Hlt;
    ahmesStep(cpu);
    expectAhmesState(cpu, { halted: false });
    ahmesStep(cpu);
    expectAhmesState(cpu, { halted: true });
    ahmesStep(cpu);
    expectAhmesState(cpu, { halted: false });
  });
});
