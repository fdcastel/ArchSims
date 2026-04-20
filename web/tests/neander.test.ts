import { beforeEach, describe, expect, it } from 'vitest';
import {
  NeanderInstruction,
  createNeanderCpu,
  neanderReset,
  neanderStep,
  type NeanderCpu,
} from '@/core/neander';
import { memoryReadByte, memoryWriteByte } from '@/core/memory';
import { expectNeanderState } from './helpers';

const expectClean = (cpu: NeanderCpu): void => {
  expectNeanderState(cpu, {
    programCounter: 0,
    accumulator: 0,
    halted: false,
    negative: false,
    zero: true,
    memoryReads: 0,
    memoryWrites: 0,
  });
  expect(cpu.registers.instructionRegister).toEqual({ opCode: 0, operandAddress: 0 });
  for (const byte of cpu.memory.data) expect(byte).toBe(0);
};

const stepJump = (cpu: NeanderCpu, instruction: number, jumpExpected: boolean): void => {
  cpu.memory.data[0] = instruction;
  cpu.memory.data[1] = 123;
  neanderStep(cpu);
  expectNeanderState(cpu, {
    programCounter: jumpExpected ? 123 : 2,
    memoryReads: 2,
  });
};

describe('neander', () => {
  let cpu: NeanderCpu;

  beforeEach(() => {
    cpu = createNeanderCpu();
  });

  it('new CPU starts in clean state', () => {
    expectClean(cpu);
  });

  it('Program Counter wraps at end of memory', () => {
    cpu.registers.programCounter = cpu.memory.data.length - 1;
    neanderStep(cpu);
    expectNeanderState(cpu, { programCounter: 0, memoryReads: 1 });
  });

  it('Reset reverts to clean state', () => {
    cpu.registers.accumulator = 1;
    cpu.registers.programCounter = 1;
    memoryReadByte(cpu.memory, 1);
    memoryWriteByte(cpu.memory, 1, NeanderInstruction.Hlt);
    neanderStep(cpu);
    expectNeanderState(cpu, {
      accumulator: 1,
      programCounter: 2,
      memoryReads: 2,
      memoryWrites: 1,
    });
    neanderReset(cpu);
    expectClean(cpu);
  });

  it('Flags Zero and Negative are set when Accumulator changes', () => {
    cpu.memory.data[0] = NeanderInstruction.Not;
    cpu.memory.data[1] = NeanderInstruction.Not;
    for (let i = 0; i <= 0xff; i++) {
      cpu.registers.accumulator = i;
      cpu.registers.programCounter = 0;
      neanderStep(cpu);
      const noti = ~i & 0xff;
      expectNeanderState(cpu, {
        negative: noti > 127,
        zero: noti === 0,
      });
      neanderStep(cpu);
      expectNeanderState(cpu, {
        negative: i > 127,
        zero: i === 0,
      });
    }
  });

  it('NOP does nothing', () => {
    cpu.memory.data[0] = NeanderInstruction.Nop;
    neanderStep(cpu);
    expectNeanderState(cpu, { programCounter: 1, memoryReads: 1 });
  });

  it('LDA loads value from memory into Accumulator', () => {
    cpu.memory.data[0] = NeanderInstruction.Lda;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 234;
    neanderStep(cpu);
    expectNeanderState(cpu, {
      accumulator: 234,
      programCounter: 2,
      memoryReads: 3,
    });
  });

  it('STA stores value from Accumulator into memory', () => {
    cpu.registers.accumulator = 234;
    cpu.memory.data[0] = NeanderInstruction.Sta;
    cpu.memory.data[1] = 123;
    neanderStep(cpu);
    expectNeanderState(cpu, {
      accumulator: 234,
      programCounter: 2,
      memoryReads: 2,
      memoryWrites: 1,
      memoryAt: [[123, 234]],
    });
  });

  it('ADD works as expected', () => {
    cpu.registers.accumulator = 12;
    cpu.memory.data[0] = NeanderInstruction.Add;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 23;
    neanderStep(cpu);
    expectNeanderState(cpu, {
      accumulator: 12 + 23,
      programCounter: 2,
      memoryReads: 3,
    });
  });

  it('OR works as expected', () => {
    cpu.registers.accumulator = 234;
    cpu.memory.data[0] = NeanderInstruction.Or;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 12;
    neanderStep(cpu);
    expectNeanderState(cpu, {
      accumulator: 234 | 12,
      programCounter: 2,
      memoryReads: 3,
    });
  });

  it('AND works as expected', () => {
    cpu.registers.accumulator = 234;
    cpu.memory.data[0] = NeanderInstruction.And;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 12;
    neanderStep(cpu);
    expectNeanderState(cpu, {
      accumulator: 234 & 12,
      programCounter: 2,
      memoryReads: 3,
    });
  });

  it('NOT works as expected', () => {
    cpu.registers.accumulator = 85; // 01010101
    cpu.memory.data[0] = NeanderInstruction.Not;
    neanderStep(cpu);
    expectNeanderState(cpu, {
      accumulator: 170, // 10101010
      programCounter: 1,
      memoryReads: 1,
    });
  });

  it('JMP changes Program Counter', () => {
    stepJump(cpu, NeanderInstruction.Jmp, true);
  });

  it('JN jumps only if Negative flag is set', () => {
    cpu.registers.flags.negative = false;
    stepJump(cpu, NeanderInstruction.Jn, false);
    neanderReset(cpu);
    cpu.registers.flags.negative = true;
    stepJump(cpu, NeanderInstruction.Jn, true);
  });

  it('JZ jumps only if Zero flag is set', () => {
    cpu.registers.flags.zero = false;
    stepJump(cpu, NeanderInstruction.Jz, false);
    neanderReset(cpu);
    cpu.registers.flags.zero = true;
    stepJump(cpu, NeanderInstruction.Jz, true);
  });

  it('HLT sets Halted flag, clears on next non-HLT step', () => {
    cpu.memory.data[1] = NeanderInstruction.Hlt;
    neanderStep(cpu);
    expectNeanderState(cpu, { halted: false });
    neanderStep(cpu);
    expectNeanderState(cpu, { halted: true });
    neanderStep(cpu);
    expectNeanderState(cpu, { halted: false });
  });
});
