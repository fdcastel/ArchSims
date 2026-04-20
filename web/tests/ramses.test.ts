import { beforeEach, describe, expect, it } from 'vitest';
import {
  RamsesAddressMode,
  RamsesInstruction,
  RamsesRegister,
  createRamsesCpu,
  ramsesDisassembleInstruction,
  ramsesDisassembleInstructions,
  ramsesReset,
  ramsesStep,
  type RamsesCpu,
} from '@/core/ramses';
import { memoryReadByte, memoryWriteByte } from '@/core/memory';
import { expectRamsesState } from './helpers';

const expectClean = (cpu: RamsesCpu): void => {
  expectRamsesState(cpu, {
    programCounter: 0,
    ra: 0,
    rb: 0,
    rx: 0,
    halted: false,
    negative: false,
    zero: true,
    carry: false,
    memoryReads: 0,
    memoryWrites: 0,
  });
  expect(cpu.registers.instructionRegister).toEqual({ opCode: 0, operandAddress: 0 });
  for (const byte of cpu.memory.data) expect(byte).toBe(0);
};

const writeRegister = (cpu: RamsesCpu, register: number, value: number): void => {
  switch (register) {
    case 0:
      cpu.registers.ra = value;
      break;
    case 1:
      cpu.registers.rb = value;
      break;
    case 2:
      cpu.registers.rx = value;
      break;
    default:
      cpu.registers.programCounter = value;
      break;
  }
};

const stepJump = (cpu: RamsesCpu, instruction: number, jumpExpected: boolean): void => {
  cpu.memory.data[0] = instruction;
  cpu.memory.data[1] = 123;
  ramsesStep(cpu);
  expectRamsesState(cpu, {
    programCounter: jumpExpected ? 123 : 2,
    memoryReads: 2,
  });
};

describe('ramses', () => {
  let cpu: RamsesCpu;

  beforeEach(() => {
    cpu = createRamsesCpu();
  });

  it('new CPU starts in clean state', () => {
    expectClean(cpu);
  });

  it('Program Counter wraps at end of memory', () => {
    cpu.registers.programCounter = cpu.memory.data.length - 1;
    ramsesStep(cpu);
    expectRamsesState(cpu, { programCounter: 0, memoryReads: 1 });
  });

  it('Reset reverts to clean state', () => {
    cpu.registers.ra = 1;
    cpu.registers.rb = 2;
    cpu.registers.rx = 3;
    cpu.registers.programCounter = 1;
    memoryReadByte(cpu.memory, 1);
    memoryWriteByte(cpu.memory, 1, RamsesInstruction.Hlt);
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 1,
      rb: 2,
      rx: 3,
      programCounter: 2,
      memoryReads: 2,
      memoryWrites: 1,
    });
    ramsesReset(cpu);
    expectClean(cpu);
  });

  it('Flags Zero and Negative are set when a register changes', () => {
    for (let r = 0; r <= 2; r++) {
      cpu.memory.data[0] = RamsesInstruction.Not | (r << 2);
      cpu.memory.data[1] = RamsesInstruction.Not | (r << 2);
      for (let i = 0; i <= 0xff; i++) {
        writeRegister(cpu, r, i);
        cpu.registers.programCounter = 0;
        ramsesStep(cpu);
        const noti = ~i & 0xff;
        expectRamsesState(cpu, {
          negative: noti > 127,
          zero: noti === 0,
        });
        ramsesStep(cpu);
        expectRamsesState(cpu, {
          negative: i > 127,
          zero: i === 0,
        });
      }
    }
  });

  it('AddressModes works as expected', () => {
    cpu.memory.data[0] = RamsesInstruction.Ldr | RamsesRegister.Ra | RamsesAddressMode.Direct;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 234;
    ramsesStep(cpu);
    expectRamsesState(cpu, { ra: 234, programCounter: 2, memoryReads: 3 });

    ramsesReset(cpu);
    cpu.memory.data[0] = RamsesInstruction.Ldr | RamsesRegister.Rb | RamsesAddressMode.Indirect;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 234;
    cpu.memory.data[234] = 245;
    ramsesStep(cpu);
    expectRamsesState(cpu, { rb: 245, programCounter: 2, memoryReads: 4 });

    ramsesReset(cpu);
    cpu.memory.data[0] = RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate;
    cpu.memory.data[1] = 123;
    ramsesStep(cpu);
    expectRamsesState(cpu, { rx: 123, programCounter: 2, memoryReads: 2 });

    ramsesReset(cpu);
    cpu.registers.rx = 23;
    cpu.memory.data[0] = RamsesInstruction.Ldr | RamsesRegister.Ra | RamsesAddressMode.Indexed;
    cpu.memory.data[1] = 123;
    cpu.memory.data[146] = 234;
    ramsesStep(cpu);
    expectRamsesState(cpu, { ra: 234, rx: 23, programCounter: 2, memoryReads: 3 });
  });

  it('NOP does nothing', () => {
    cpu.memory.data[0] = RamsesInstruction.Nop;
    ramsesStep(cpu);
    expectRamsesState(cpu, { programCounter: 1, memoryReads: 1 });
  });

  it('LDR loads value from memory into any register', () => {
    for (let r = 0; r <= 2; r++) {
      ramsesReset(cpu);
      cpu.memory.data[0] = RamsesInstruction.Ldr | (r << 2) | RamsesAddressMode.Immediate;
      cpu.memory.data[1] = 123;
      ramsesStep(cpu);
      const rCheck = (rr: number): number => (rr === r ? 123 : 0);
      expectRamsesState(cpu, {
        ra: rCheck(0),
        rb: rCheck(1),
        rx: rCheck(2),
        programCounter: 2,
        memoryReads: 2,
      });
    }
  });

  it('STR stores value from any register into memory', () => {
    for (let r = 0; r <= 2; r++) {
      ramsesReset(cpu);
      writeRegister(cpu, r, 234);
      cpu.memory.data[0] = RamsesInstruction.Str | (r << 2) | RamsesAddressMode.Direct;
      cpu.memory.data[1] = 123;
      cpu.memory.data[122] = 10;
      cpu.memory.data[123] = 20;
      cpu.memory.data[124] = 30;
      ramsesStep(cpu);
      expectRamsesState(cpu, {
        programCounter: 2,
        memoryReads: 2,
        memoryWrites: 1,
        memoryAt: [
          [122, 10],
          [123, 234],
          [124, 30],
        ],
      });
    }
  });

  it('ADD works as expected', () => {
    cpu.registers.ra = 12;
    cpu.memory.data[0] = RamsesInstruction.Add | RamsesAddressMode.Immediate;
    cpu.memory.data[1] = 23;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 12 + 23,
      programCounter: 2,
      memoryReads: 2,
      negative: false,
      zero: false,
      carry: false,
    });

    ramsesReset(cpu);
    cpu.registers.ra = (256 - 12) & 0xff;
    cpu.memory.data[0] = RamsesInstruction.Add | RamsesAddressMode.Immediate;
    cpu.memory.data[1] = (256 - 23) & 0xff;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: (256 - 12 - 23) & 0xff,
      programCounter: 2,
      memoryReads: 2,
      negative: true,
      zero: false,
      carry: true,
    });
  });

  it('OR works as expected', () => {
    cpu.registers.ra = 234;
    cpu.memory.data[0] = RamsesInstruction.Or;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 12;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 234 | 12,
      programCounter: 2,
      memoryReads: 3,
    });
  });

  it('AND works as expected', () => {
    cpu.registers.ra = 234;
    cpu.memory.data[0] = RamsesInstruction.And;
    cpu.memory.data[1] = 123;
    cpu.memory.data[123] = 12;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 234 & 12,
      programCounter: 2,
      memoryReads: 3,
    });
  });

  it('NOT works as expected', () => {
    cpu.registers.ra = 85; // 01010101
    cpu.memory.data[0] = RamsesInstruction.Not;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 170, // 10101010
      programCounter: 1,
      memoryReads: 1,
    });
  });

  it('SUB works as expected', () => {
    cpu.registers.ra = 23;
    cpu.memory.data[0] = RamsesInstruction.Sub | RamsesAddressMode.Immediate;
    cpu.memory.data[1] = 12;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 23 - 12,
      programCounter: 2,
      memoryReads: 2,
      negative: false,
      zero: false,
      carry: false,
    });

    ramsesReset(cpu);
    cpu.registers.ra = 12;
    cpu.memory.data[0] = RamsesInstruction.Sub | RamsesAddressMode.Immediate;
    cpu.memory.data[1] = 23;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: (256 + 12 - 23) & 0xff,
      programCounter: 2,
      memoryReads: 2,
      negative: true,
      zero: false,
      carry: true,
    });
  });

  it('JMP changes Program Counter', () => {
    stepJump(cpu, RamsesInstruction.Jmp, true);
  });

  it('JN jumps only if Negative flag is set', () => {
    cpu.registers.flags.negative = false;
    stepJump(cpu, RamsesInstruction.Jn, false);
    ramsesReset(cpu);
    cpu.registers.flags.negative = true;
    stepJump(cpu, RamsesInstruction.Jn, true);
  });

  it('JZ jumps only if Zero flag is set', () => {
    cpu.registers.flags.zero = false;
    stepJump(cpu, RamsesInstruction.Jz, false);
    ramsesReset(cpu);
    cpu.registers.flags.zero = true;
    stepJump(cpu, RamsesInstruction.Jz, true);
  });

  it('JC jumps only if Carry flag is set', () => {
    cpu.registers.flags.carry = false;
    stepJump(cpu, RamsesInstruction.Jc, false);
    ramsesReset(cpu);
    cpu.registers.flags.carry = true;
    stepJump(cpu, RamsesInstruction.Jc, true);
  });

  it('JSR jumps and saves Program Counter', () => {
    cpu.memory.data[0] = RamsesInstruction.Jsr;
    cpu.memory.data[1] = 123;
    cpu.memory.data[122] = 10;
    cpu.memory.data[123] = 20;
    cpu.memory.data[124] = 30;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      programCounter: 124,
      memoryReads: 2,
      memoryWrites: 1,
      memoryAt: [
        [122, 10],
        [123, 2],
        [124, 30],
      ],
    });
  });

  it('NEG works as expected', () => {
    cpu.registers.ra = 23;
    cpu.memory.data[0] = RamsesInstruction.Neg;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: (256 - 23) & 0xff,
      programCounter: 1,
      memoryReads: 1,
      negative: true,
      zero: false,
      carry: false,
    });

    ramsesReset(cpu);
    cpu.registers.ra = 234;
    cpu.memory.data[0] = RamsesInstruction.Neg;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: (256 - 234) & 0xff,
      programCounter: 1,
      memoryReads: 1,
      negative: false,
      zero: false,
      carry: false,
    });

    ramsesReset(cpu);
    cpu.registers.ra = 128;
    cpu.memory.data[0] = RamsesInstruction.Neg;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: (256 - 128) & 0xff,
      programCounter: 1,
      memoryReads: 1,
      negative: true,
      zero: false,
      carry: false,
    });

    ramsesReset(cpu);
    cpu.registers.ra = 0;
    cpu.memory.data[0] = RamsesInstruction.Neg;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: (256 - 0) & 0xff,
      programCounter: 1,
      memoryReads: 1,
      negative: false,
      zero: true,
      carry: true,
    });
  });

  it('SHR works as expected', () => {
    cpu.registers.ra = 85; // 01010101
    cpu.memory.data[0] = RamsesInstruction.Shr;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 42,
      programCounter: 1,
      memoryReads: 1,
      negative: false,
      zero: false,
      carry: true,
    });

    cpu.memory.data[1] = RamsesInstruction.Shr;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 21,
      programCounter: 2,
      memoryReads: 2,
      negative: false,
      zero: false,
      carry: false,
    });

    cpu.memory.data[2] = RamsesInstruction.Shr;
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 10,
      programCounter: 3,
      memoryReads: 3,
      negative: false,
      zero: false,
      carry: true,
    });
  });

  it('HLT sets Halted flag', () => {
    cpu.memory.data[1] = RamsesInstruction.Hlt;
    ramsesStep(cpu);
    expectRamsesState(cpu, { halted: false });
    ramsesStep(cpu);
    expectRamsesState(cpu, { halted: true });
    ramsesStep(cpu);
    expectRamsesState(cpu, { halted: false });
  });

  it('DisassembleInstruction works as expected', () => {
    expect(ramsesDisassembleInstruction([RamsesInstruction.Nop])).toEqual({
      text: 'NOP',
      size: 1,
    });
    expect(ramsesDisassembleInstruction([RamsesInstruction.Nop + 5])).toEqual({
      text: 'NOP',
      size: 1,
    });
    expect(ramsesDisassembleInstruction([RamsesInstruction.Hlt])).toEqual({
      text: 'HLT',
      size: 1,
    });

    expect(
      ramsesDisassembleInstruction([
        RamsesInstruction.Str | RamsesRegister.Ra | RamsesAddressMode.Direct,
        12,
      ]),
    ).toEqual({ text: 'STR A 12', size: 2 });
    expect(
      ramsesDisassembleInstruction([
        RamsesInstruction.Str | RamsesRegister.Rb | RamsesAddressMode.Indirect,
        23,
      ]),
    ).toEqual({ text: 'STR B 23,I', size: 2 });
    expect(
      ramsesDisassembleInstruction([
        RamsesInstruction.Str | RamsesRegister.Rx | RamsesAddressMode.Immediate,
        34,
      ]),
    ).toEqual({ text: 'STR X #34', size: 2 });
    expect(
      ramsesDisassembleInstruction([
        RamsesInstruction.Str | RamsesRegister.Ra | RamsesAddressMode.Indexed,
        45,
      ]),
    ).toEqual({ text: 'STR A 45,X', size: 2 });

    expect(
      ramsesDisassembleInstruction([RamsesInstruction.Not | RamsesRegister.Ra]),
    ).toEqual({ text: 'NOT A', size: 1 });
    expect(
      ramsesDisassembleInstruction([RamsesInstruction.Not | RamsesRegister.Rb]),
    ).toEqual({ text: 'NOT B', size: 1 });
    expect(
      ramsesDisassembleInstruction([RamsesInstruction.Not | RamsesRegister.Rx]),
    ).toEqual({ text: 'NOT X', size: 1 });

    expect(
      ramsesDisassembleInstruction([RamsesInstruction.Jmp | RamsesAddressMode.Direct, 12]),
    ).toEqual({ text: 'JMP 12', size: 2 });
    expect(
      ramsesDisassembleInstruction([RamsesInstruction.Jmp | RamsesAddressMode.Indirect, 23]),
    ).toEqual({ text: 'JMP 23,I', size: 2 });
    expect(
      ramsesDisassembleInstruction([RamsesInstruction.Jmp | RamsesAddressMode.Immediate, 34]),
    ).toEqual({ text: 'JMP #34', size: 2 });
    expect(
      ramsesDisassembleInstruction([RamsesInstruction.Jmp | RamsesAddressMode.Indexed, 45]),
    ).toEqual({ text: 'JMP 45,X', size: 2 });

    expect(
      ramsesDisassembleInstruction([
        RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
        0,
      ]),
    ).toEqual({ text: 'LDR X #0', size: 2 });
    expect(
      ramsesDisassembleInstruction([
        RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
        127,
      ]),
    ).toEqual({ text: 'LDR X #127', size: 2 });
    expect(
      ramsesDisassembleInstruction([
        RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
        128,
      ]),
    ).toEqual({ text: 'LDR X #128', size: 2 });
    expect(
      ramsesDisassembleInstruction([
        RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
        255,
      ]),
    ).toEqual({ text: 'LDR X #255', size: 2 });
  });

  it('DisassembleInstructions works as expected', () => {
    const content = [
      RamsesInstruction.Str | RamsesRegister.Ra | RamsesAddressMode.Direct,
      12,
      RamsesInstruction.Not | RamsesRegister.Rb,
      RamsesInstruction.Nop,
      RamsesInstruction.Jmp | RamsesAddressMode.Indirect,
      23,
      RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
      255,
      RamsesInstruction.Hlt,
    ];

    expect(ramsesDisassembleInstructions(content)).toEqual([
      { text: 'STR A 12', size: 2 },
      { text: 'NOT B', size: 1 },
      { text: 'NOP', size: 1 },
      { text: 'JMP 23,I', size: 2 },
      { text: 'LDR X #255', size: 2 },
      { text: 'HLT', size: 1 },
    ]);
  });
});
