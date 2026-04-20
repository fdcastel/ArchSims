import { beforeEach, describe, expect, it } from 'vitest';
import {
  CesarAddressMode,
  CesarFlagBit,
  CesarInstruction,
  CesarRegister,
  cesarDisassembleInstruction,
  cesarDisassembleInstructions,
  cesarReset,
  cesarStep,
  createCesarCpu,
  encodeCesarInstructionTwoOperand,
  type CesarCpu,
} from '@/core/cesar';
import { memoryReadByte, memoryWriteByte } from '@/core/memory';
import { expectCesarState } from './helpers';

const expectClean = (cpu: CesarCpu): void => {
  expectCesarState(cpu, {
    r0: 0,
    r1: 0,
    r2: 0,
    r3: 0,
    r4: 0,
    r5: 0,
    r6: 0,
    programCounter: 0,
    halted: false,
    negative: false,
    zero: true,
    overflow: false,
    carry: false,
    memoryReads: 0,
    memoryWrites: 0,
  });
  expect(cpu.registers.instructionRegister).toEqual({
    data: [0],
    sourceOperand: { kind: 'none' },
    targetOperand: { kind: 'none' },
  });
  for (const byte of cpu.memory.data) expect(byte).toBe(0);
};

const testAddressMode = (cpu: CesarCpu, addressMode: number): void => {
  cesarReset(cpu);
  const encoded = encodeCesarInstructionTwoOperand(
    CesarInstruction.Mov,
    addressMode,
    CesarRegister.R1,
    CesarAddressMode.Register,
    CesarRegister.R0,
  );
  cpu.memory.data[0] = (encoded >>> 8) & 0xff;
  cpu.memory.data[1] = encoded & 0xff;

  let expected: Parameters<typeof expectCesarState>[1];
  switch (addressMode) {
    case CesarAddressMode.Register: {
      cpu.registers.r[1] = 0x1234;
      expected = {
        r0: 0x1234,
        r1: 0x1234,
        programCounter: 2,
        memoryReads: 2,
        instructionRegisterAt: [0, 2],
      };
      break;
    }
    case CesarAddressMode.RegPostInc: {
      cpu.memory.data[10] = 0x12;
      cpu.memory.data[11] = 0x34;
      cpu.registers.r[1] = 10;
      expected = {
        r0: 0x1234,
        r1: 12,
        programCounter: 2,
        memoryReads: 4,
        instructionRegisterAt: [0, 2],
      };
      break;
    }
    case CesarAddressMode.RegPreDec: {
      cpu.memory.data[10] = 0x12;
      cpu.memory.data[11] = 0x34;
      cpu.registers.r[1] = 12;
      expected = {
        r0: 0x1234,
        r1: 10,
        programCounter: 2,
        memoryReads: 4,
        instructionRegisterAt: [0, 2],
      };
      break;
    }
    case CesarAddressMode.Indexed: {
      cpu.memory.data[2] = 0;
      cpu.memory.data[3] = 4;
      cpu.memory.data[10] = 0x12;
      cpu.memory.data[11] = 0x34;
      cpu.registers.r[1] = 6;
      expected = {
        r0: 0x1234,
        r1: 6,
        programCounter: 4,
        memoryReads: 6,
        instructionRegisterAt: [0, 4],
      };
      break;
    }
    case CesarAddressMode.RegisterIndirect: {
      cpu.memory.data[10] = 0x12;
      cpu.memory.data[11] = 0x34;
      cpu.registers.r[1] = 10;
      expected = {
        r0: 0x1234,
        r1: 10,
        programCounter: 2,
        memoryReads: 4,
        instructionRegisterAt: [0, 2],
      };
      break;
    }
    case CesarAddressMode.RegPostIncIndirect: {
      cpu.memory.data[10] = 0;
      cpu.memory.data[11] = 20;
      cpu.memory.data[20] = 0x12;
      cpu.memory.data[21] = 0x34;
      cpu.registers.r[1] = 10;
      expected = {
        r0: 0x1234,
        r1: 12,
        programCounter: 2,
        memoryReads: 6,
        instructionRegisterAt: [0, 2],
      };
      break;
    }
    case CesarAddressMode.RegPreDecIndirect: {
      cpu.memory.data[10] = 0;
      cpu.memory.data[11] = 20;
      cpu.memory.data[20] = 0x12;
      cpu.memory.data[21] = 0x34;
      cpu.registers.r[1] = 12;
      expected = {
        r0: 0x1234,
        r1: 10,
        programCounter: 2,
        memoryReads: 6,
        instructionRegisterAt: [0, 2],
      };
      break;
    }
    case CesarAddressMode.IndexedIndirect: {
      cpu.memory.data[2] = 0;
      cpu.memory.data[3] = 4;
      cpu.memory.data[10] = 0;
      cpu.memory.data[11] = 20;
      cpu.memory.data[20] = 0x12;
      cpu.memory.data[21] = 0x34;
      cpu.registers.r[1] = 6;
      expected = {
        r0: 0x1234,
        r1: 6,
        programCounter: 4,
        memoryReads: 8,
        instructionRegisterAt: [0, 4],
      };
      break;
    }
    default:
      throw new Error('Invalid AddressMode');
  }

  cesarStep(cpu);
  expectCesarState(cpu, expected);
};

const testBranchOperation = (
  cpu: CesarCpu,
  instruction: number,
  branchExpected: boolean,
  extraState: Parameters<typeof expectCesarState>[1],
): void => {
  const address = cpu.registers.r[7]!;
  const memoryReads = cpu.memory.readCount;

  cpu.memory.data[address] = instruction;
  cpu.memory.data[address + 1] = 3;
  cesarStep(cpu);

  const newAddress = address + (branchExpected ? 5 : 2);
  expectCesarState(cpu, {
    ...extraState,
    programCounter: newAddress,
    memoryReads: memoryReads + 2,
    instructionRegisterAt: [address, 2],
  });

  cpu.memory.data[newAddress] = instruction;
  cpu.memory.data[newAddress + 1] = 253;
  cesarStep(cpu);

  const finalAddress = newAddress + (branchExpected ? -1 : 2);
  expectCesarState(cpu, {
    ...extraState,
    programCounter: finalAddress,
    memoryReads: memoryReads + 4,
    instructionRegisterAt: [newAddress, 2],
  });
};

const testClrGroupOperation = (
  cpu: CesarCpu,
  instruction: number,
  value: number,
  expectedResult: number,
  extraState: Parameters<typeof expectCesarState>[1],
): void => {
  const address = cpu.registers.r[7]!;
  const memoryReads = cpu.memory.readCount;

  cpu.memory.data[address] = instruction;
  cpu.memory.data[address + 1] = CesarRegister.R4;
  cpu.registers.r[4] = value;
  cesarStep(cpu);

  expectCesarState(cpu, {
    ...extraState,
    r4: expectedResult & 0xffff,
    programCounter: (address + 2) & 0xffff,
    negative: (expectedResult & 0xffff) > 0x7fff,
    zero: (expectedResult & 0xffff) === 0,
    memoryReads: memoryReads + 2,
    instructionRegisterAt: [address, 2],
  });
};

const testMovGroupOperation = (
  cpu: CesarCpu,
  instruction: number,
  sourceValue: number,
  targetValue: number,
  expectedResult: number,
  extraState: Parameters<typeof expectCesarState>[1],
): void => {
  const encoded = encodeCesarInstructionTwoOperand(
    instruction,
    CesarAddressMode.Register,
    CesarRegister.R5,
    CesarAddressMode.Register,
    CesarRegister.R6,
  );
  cpu.memory.data[0] = (encoded >>> 8) & 0xff;
  cpu.memory.data[1] = encoded & 0xff;
  cpu.registers.r[5] = sourceValue;
  cpu.registers.r[6] = targetValue;
  cesarStep(cpu);

  const expectedR6 = instruction === CesarInstruction.Cmp ? targetValue : expectedResult;
  expectCesarState(cpu, {
    ...extraState,
    r5: sourceValue & 0xffff,
    r6: expectedR6 & 0xffff,
    programCounter: 2,
    memoryReads: 2,
    instructionRegisterAt: [0, 2],
  });
};

describe('cesar', () => {
  let cpu: CesarCpu;

  beforeEach(() => {
    cpu = createCesarCpu();
  });

  it('new CPU starts in clean state', () => {
    expectClean(cpu);
  });

  it('Program Counter wraps at end of memory', () => {
    cpu.registers.r[7] = cpu.memory.data.length - 1;
    cesarStep(cpu);
    expectCesarState(cpu, { programCounter: 0, memoryReads: 1 });
  });

  it('Reset reverts to clean state', () => {
    cpu.registers.r[0] = 1;
    cpu.registers.r[1] = 2;
    cpu.registers.r[2] = 3;
    cpu.registers.r[7] = 1;
    memoryReadByte(cpu.memory, 1);
    memoryWriteByte(cpu.memory, 1, CesarInstruction.Hlt);
    cesarStep(cpu);
    expectCesarState(cpu, {
      r0: 1,
      r1: 2,
      r2: 3,
      programCounter: 2,
      memoryReads: 2,
      memoryWrites: 1,
    });
    cesarReset(cpu);
    expectClean(cpu);
  });

  it('AddressModes works as expected', () => {
    testAddressMode(cpu, CesarAddressMode.Register);
    testAddressMode(cpu, CesarAddressMode.RegPostInc);
    testAddressMode(cpu, CesarAddressMode.RegPreDec);
    testAddressMode(cpu, CesarAddressMode.Indexed);
    testAddressMode(cpu, CesarAddressMode.RegisterIndirect);
    testAddressMode(cpu, CesarAddressMode.RegPostIncIndirect);
    testAddressMode(cpu, CesarAddressMode.RegPreDecIndirect);
    testAddressMode(cpu, CesarAddressMode.IndexedIndirect);
  });

  it('AddressModes with R7 works as expected', () => {
    cpu.memory.data[0] = CesarInstruction.Not;
    cpu.memory.data[1] = CesarRegister.R7 | CesarAddressMode.Register;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 65533,
      memoryReads: 2,
      instructionRegisterAt: [0, 2],
    });

    cesarReset(cpu);
    cpu.registers.r[7] = 2;
    cpu.memory.data[2] = CesarInstruction.Not;
    cpu.memory.data[3] = CesarRegister.R7 | CesarAddressMode.RegPostInc;
    cpu.memory.data[4] = 0;
    cpu.memory.data[5] = 10;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 6,
      memoryReads: 4,
      memoryWrites: 2,
      memoryAt: [
        [4, 255],
        [5, 245],
      ],
      instructionRegisterIs: [129, 15, 0, 4],
    });

    cesarReset(cpu);
    cpu.registers.r[7] = 6;
    cpu.memory.data[6] = CesarInstruction.Not;
    cpu.memory.data[7] = CesarRegister.R7 | CesarAddressMode.RegPreDec;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 6,
      memoryReads: 4,
      memoryWrites: 2,
      memoryAt: [
        [6, 126],
        [7, 232],
      ],
      instructionRegisterIs: [129, 23],
    });

    cesarReset(cpu);
    cpu.registers.r[7] = 8;
    cpu.memory.data[8] = CesarInstruction.Not;
    cpu.memory.data[9] = CesarRegister.R7 | CesarAddressMode.Indexed;
    cpu.memory.data[10] = 0;
    cpu.memory.data[11] = 2;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 12,
      memoryReads: 6,
      memoryWrites: 2,
      memoryAt: [
        [14, 255],
        [15, 255],
      ],
      instructionRegisterIs: [129, 31, 0, 2],
    });

    cesarReset(cpu);
    cpu.registers.r[7] = 16;
    cpu.memory.data[16] = CesarInstruction.Not;
    cpu.memory.data[17] = CesarRegister.R7 | CesarAddressMode.RegisterIndirect;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 18,
      memoryReads: 4,
      memoryWrites: 2,
      memoryAt: [
        [18, 255],
        [19, 255],
      ],
      instructionRegisterIs: [129, 39],
    });

    cesarReset(cpu);
    cpu.registers.r[7] = 20;
    cpu.memory.data[20] = CesarInstruction.Not;
    cpu.memory.data[21] = CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect;
    cpu.memory.data[22] = 0;
    cpu.memory.data[23] = 0;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 24,
      memoryReads: 6,
      memoryWrites: 2,
      memoryAt: [
        [0, 255],
        [1, 255],
      ],
      instructionRegisterAt: [20, 4],
    });

    cesarReset(cpu);
    cpu.registers.r[7] = 24;
    cpu.memory.data[24] = CesarInstruction.Not;
    cpu.memory.data[25] = CesarRegister.R7 | CesarAddressMode.RegPreDecIndirect;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 24,
      memoryReads: 6,
      memoryWrites: 2,
      memoryAt: [
        [33079, 255],
        [33080, 255],
      ],
      instructionRegisterAt: [24, 2],
    });

    cesarReset(cpu);
    cpu.registers.r[7] = 26;
    cpu.memory.data[26] = CesarInstruction.Not;
    cpu.memory.data[27] = CesarRegister.R7 | CesarAddressMode.IndexedIndirect;
    cpu.memory.data[28] = 0;
    cpu.memory.data[29] = 2;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 30,
      memoryReads: 8,
      memoryWrites: 2,
      memoryAt: [
        [0, 255],
        [1, 255],
      ],
      instructionRegisterAt: [26, 4],
    });
  });

  it('NOP does nothing', () => {
    cpu.memory.data[0] = CesarInstruction.Nop;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 1,
      memoryReads: 1,
      instructionRegisterAt: [0, 1],
    });
  });

  it('CCC clears flags', () => {
    for (let flags = 0; flags <= 15; flags++) {
      cpu.registers.r[7] = 0;
      cpu.registers.flags.negative = true;
      cpu.registers.flags.zero = true;
      cpu.registers.flags.overflow = true;
      cpu.registers.flags.carry = true;

      cpu.memory.data[0] = CesarInstruction.Ccc | flags;
      cesarStep(cpu);

      const mustClearNegative = (flags & CesarFlagBit.Negative) !== 0;
      const mustClearZero = (flags & CesarFlagBit.Zero) !== 0;
      const mustClearOverflow = (flags & CesarFlagBit.Overflow) !== 0;
      const mustClearCarry = (flags & CesarFlagBit.Carry) !== 0;

      expectCesarState(cpu, {
        negative: !mustClearNegative,
        zero: !mustClearZero,
        overflow: !mustClearOverflow,
        carry: !mustClearCarry,
        instructionRegisterAt: [0, 1],
      });
    }
  });

  it('SCC sets flags', () => {
    for (let flags = 0; flags <= 15; flags++) {
      cpu.registers.r[7] = 0;
      cpu.registers.flags.negative = false;
      cpu.registers.flags.zero = false;
      cpu.registers.flags.overflow = false;
      cpu.registers.flags.carry = false;

      cpu.memory.data[0] = CesarInstruction.Scc | flags;
      cesarStep(cpu);

      const mustSetNegative = (flags & CesarFlagBit.Negative) !== 0;
      const mustSetZero = (flags & CesarFlagBit.Zero) !== 0;
      const mustSetOverflow = (flags & CesarFlagBit.Overflow) !== 0;
      const mustSetCarry = (flags & CesarFlagBit.Carry) !== 0;

      expectCesarState(cpu, {
        negative: mustSetNegative,
        zero: mustSetZero,
        overflow: mustSetOverflow,
        carry: mustSetCarry,
        instructionRegisterAt: [0, 1],
      });
    }
  });

  it('Branches instructions works as expected', () => {
    testBranchOperation(cpu, CesarInstruction.Br, true, {});

    for (let flags = 0; flags <= 15; flags++) {
      cpu.registers.r[7] = 0;
      cpu.registers.flags.negative = (flags & CesarFlagBit.Negative) !== 0;
      cpu.registers.flags.zero = (flags & CesarFlagBit.Zero) !== 0;
      cpu.registers.flags.overflow = (flags & CesarFlagBit.Overflow) !== 0;
      cpu.registers.flags.carry = (flags & CesarFlagBit.Carry) !== 0;

      const f = cpu.registers.flags;
      testBranchOperation(cpu, CesarInstruction.Bne, !f.zero, {});
      testBranchOperation(cpu, CesarInstruction.Beq, f.zero, {});
      testBranchOperation(cpu, CesarInstruction.Bpl, !f.negative, {});
      testBranchOperation(cpu, CesarInstruction.Bmi, f.negative, {});
      testBranchOperation(cpu, CesarInstruction.Bvc, !f.overflow, {});
      testBranchOperation(cpu, CesarInstruction.Bvs, f.overflow, {});
      testBranchOperation(cpu, CesarInstruction.Bcc, !f.carry, {});
      testBranchOperation(cpu, CesarInstruction.Bcs, f.carry, {});
      testBranchOperation(cpu, CesarInstruction.Bge, f.negative === f.overflow, {});
      testBranchOperation(cpu, CesarInstruction.Blt, f.negative !== f.overflow, {});
      testBranchOperation(cpu, CesarInstruction.Bgt, f.negative === f.overflow && !f.zero, {});
      testBranchOperation(cpu, CesarInstruction.Ble, f.negative !== f.overflow || f.zero, {});
      testBranchOperation(cpu, CesarInstruction.Bhi, !f.carry && !f.zero, {});
      testBranchOperation(cpu, CesarInstruction.Bls, f.carry || f.zero, {});
    }
  });

  it('JMP changes Program Counter', () => {
    cpu.memory.data[0] = CesarInstruction.Jmp;
    cpu.memory.data[1] = CesarAddressMode.RegisterIndirect | CesarRegister.R1;
    cpu.registers.r[1] = 10;
    cesarStep(cpu);
    expectCesarState(cpu, {
      r1: 10,
      programCounter: 10,
      memoryReads: 2,
      instructionRegisterAt: [0, 2],
    });

    cesarReset(cpu);
    cpu.memory.data[0] = CesarInstruction.Jmp;
    cpu.memory.data[1] = CesarAddressMode.RegPostIncIndirect | CesarRegister.R7;
    cpu.memory.data[2] = 0;
    cpu.memory.data[3] = 10;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 10,
      memoryReads: 4,
      instructionRegisterAt: [0, 4],
    });

    cesarReset(cpu);
    cpu.memory.data[0] = CesarInstruction.Jmp;
    cpu.memory.data[1] = CesarAddressMode.RegPostInc | CesarRegister.R7;
    cpu.memory.data[2] = 0;
    cpu.memory.data[3] = 10;
    cesarStep(cpu);
    expectCesarState(cpu, {
      programCounter: 2,
      memoryReads: 2,
      instructionRegisterIs: [64, 15, 0, 2],
    });
  });

  it('SOB subtracts one and branch', () => {
    cpu.memory.data[10] = CesarInstruction.Sob | CesarRegister.R2;
    cpu.memory.data[11] = 2;
    cpu.registers.r[2] = 3;
    cpu.registers.r[7] = 10;

    cesarStep(cpu);
    expectCesarState(cpu, {
      r2: 2,
      programCounter: 10,
      memoryReads: 2,
      instructionRegisterAt: [10, 2],
    });
    cesarStep(cpu);
    expectCesarState(cpu, {
      r2: 1,
      programCounter: 10,
      memoryReads: 4,
      instructionRegisterAt: [10, 2],
    });
    cesarStep(cpu);
    expectCesarState(cpu, {
      r2: 0,
      programCounter: 12,
      memoryReads: 6,
      instructionRegisterAt: [10, 2],
    });
  });

  it('JSR jumps to subroutine and RTS returns', () => {
    cpu.memory.data[0] = CesarInstruction.Jsr | CesarRegister.R3;
    cpu.memory.data[1] = CesarAddressMode.RegisterIndirect | CesarRegister.R1;
    cpu.registers.r[1] = 123;
    cpu.registers.r[3] = 0x1234;
    cesarStep(cpu);
    expectCesarState(cpu, {
      r1: 123,
      r3: 2,
      r6: 65534,
      programCounter: 123,
      memoryReads: 2,
      memoryAt: [
        [65534, 0x12],
        [65535, 0x34],
      ],
      instructionRegisterAt: [0, 2],
    });

    cpu.memory.data[123] = CesarInstruction.Rts | CesarRegister.R3;
    cesarStep(cpu);
    expectCesarState(cpu, {
      r1: 123,
      r3: 0x1234,
      r6: 0,
      programCounter: 2,
      memoryReads: 5,
      memoryWrites: 2,
      instructionRegisterAt: [123, 1],
    });
  });

  it('CLR group instructions works as expected', () => {
    const testValues = [
      1, 127, 128, 129, 255, 256, 257, 32767, 32768, 32769, 65534, 65535,
    ];
    for (const value of testValues) {
      cpu.registers.r[7] = 0;

      testClrGroupOperation(cpu, CesarInstruction.Clr, value, 0, {});
      testClrGroupOperation(cpu, CesarInstruction.Not, value, ~value & 0xffff, { carry: true });
      testClrGroupOperation(cpu, CesarInstruction.Inc, value, (value + 1) & 0xffff, {
        overflow: value === 32767,
        carry: value === 65535,
      });
      testClrGroupOperation(cpu, CesarInstruction.Dec, value, (value - 1) & 0xffff, {
        overflow: value === 32768,
        carry: value === 0,
      });
      testClrGroupOperation(cpu, CesarInstruction.Tst, value, value, {});
      testClrGroupOperation(cpu, CesarInstruction.Neg, value, (65536 - value) & 0xffff, {
        overflow: value === 32768,
        carry: value !== 0,
      });

      cpu.registers.flags.carry = false;
      testClrGroupOperation(cpu, CesarInstruction.Ror, value, (value >>> 1) & 0xffff, {
        overflow: (value & 1) !== 0,
        carry: (value & 1) !== 0,
      });
      cpu.registers.flags.carry = true;
      testClrGroupOperation(cpu, CesarInstruction.Ror, value, ((value >>> 1) | 0x8000) & 0xffff, {
        overflow: (value & 1) === 0,
        carry: (value & 1) !== 0,
      });

      // ToDo: ROL, ASR, ASL

      cpu.registers.flags.carry = false;
      testClrGroupOperation(cpu, CesarInstruction.Adc, value, value, {});
      cpu.registers.flags.carry = true;
      testClrGroupOperation(cpu, CesarInstruction.Adc, value, (value + 1) & 0xffff, {
        overflow: value === 32767,
        carry: value === 65535,
      });

      cpu.registers.flags.carry = false;
      testClrGroupOperation(cpu, CesarInstruction.Sbc, value, value, {});
      cpu.registers.flags.carry = true;
      testClrGroupOperation(cpu, CesarInstruction.Sbc, value, (value - 1) & 0xffff, {
        overflow: value === 32768,
        carry: value !== 0,
      });
    }
  });

  it('ADD adds source into target', () => {
    testMovGroupOperation(cpu, CesarInstruction.Add, 12, 23, 12 + 23, {
      negative: false,
      carry: false,
    });
    cesarReset(cpu);
    testMovGroupOperation(
      cpu,
      CesarInstruction.Add,
      (65536 - 12) & 0xffff,
      (65536 - 23) & 0xffff,
      (65536 - 12 - 23) & 0xffff,
      { negative: true, carry: true },
    );
  });

  it('SUB subtracts source from target', () => {
    testMovGroupOperation(cpu, CesarInstruction.Sub, 12, 23, 23 - 12, {
      negative: false,
      carry: false,
    });
    cesarReset(cpu);
    testMovGroupOperation(cpu, CesarInstruction.Sub, 23, 12, (65536 + 12 - 23) & 0xffff, {
      negative: true,
      carry: true,
    });
  });

  it('CMP compares two operands', () => {
    testMovGroupOperation(cpu, CesarInstruction.Sub, 12, 23, 23 - 12, {
      negative: false,
      zero: false,
      carry: false,
    });
    cesarReset(cpu);
    testMovGroupOperation(cpu, CesarInstruction.Sub, 23, 12, (65536 + 12 - 23) & 0xffff, {
      negative: true,
      zero: false,
      carry: true,
    });
  });

  it('AND works as expected', () => {
    testMovGroupOperation(cpu, CesarInstruction.And, 12, 234, 12 & 234, {
      negative: false,
      carry: false,
    });
  });

  it('OR works as expected', () => {
    testMovGroupOperation(cpu, CesarInstruction.Or, 12, 234, 12 | 234, {
      negative: false,
      carry: false,
    });
  });

  it('HLT sets Halted flag', () => {
    cpu.memory.data[1] = CesarInstruction.Hlt;
    cesarStep(cpu);
    expectCesarState(cpu, { halted: false, instructionRegisterAt: [0, 1] });
    cesarStep(cpu);
    expectCesarState(cpu, { halted: true, instructionRegisterAt: [1, 1] });
    cesarStep(cpu);
    expectCesarState(cpu, { halted: false, instructionRegisterAt: [2, 1] });
  });

  it('High memory area addresses works at byte level', () => {
    cpu.memory.data[0] = 48; // BR 8
    cpu.memory.data[1] = 8;
    cpu.memory.data[65497] = 0x33;
    cpu.memory.data[65498] = 0x66;
    cpu.memory.data[65499] = 123;
    cpu.memory.data[65500] = 234;
    cpu.memory.data[65535] = 100;
    cesarStep(cpu);

    cpu.memory.data[10] = 155; // MOV 0, R0
    cpu.memory.data[11] = 192;
    cpu.memory.data[12] = 0;
    cpu.memory.data[13] = 0;
    cesarStep(cpu);
    expectCesarState(cpu, { r0: 12296 });

    // Display memory area
    cpu.memory.data[14] = 155; // MOV 65534, R1
    cpu.memory.data[15] = 193;
    cpu.memory.data[16] = 255;
    cpu.memory.data[17] = 254;
    cesarStep(cpu);
    expectCesarState(cpu, { r1: 0 });

    cpu.memory.data[18] = 155; // MOV 65535, R2
    cpu.memory.data[19] = 194;
    cpu.memory.data[20] = 255;
    cpu.memory.data[21] = 255;
    cesarStep(cpu);
    expectCesarState(cpu, { r2: 100 });

    cpu.memory.data[22] = 155; // MOV 65500, R3
    cpu.memory.data[23] = 195;
    cpu.memory.data[24] = 255;
    cpu.memory.data[25] = 220;
    cesarStep(cpu);
    expectCesarState(cpu, { r3: 234 });

    // Keyboard memory area
    cpu.memory.data[26] = 155; // MOV 65499, R4
    cpu.memory.data[27] = 196;
    cpu.memory.data[28] = 255;
    cpu.memory.data[29] = 219;
    cesarStep(cpu);
    expectCesarState(cpu, { r4: 123 });

    cpu.memory.data[30] = 155; // MOV 65498, R4
    cpu.memory.data[31] = 196;
    cpu.memory.data[32] = 255;
    cpu.memory.data[33] = 218;
    cesarStep(cpu);
    expectCesarState(cpu, { r4: 0x66 });

    // Normal memory area
    cpu.memory.data[34] = 155; // MOV 65497, R4
    cpu.memory.data[35] = 196;
    cpu.memory.data[36] = 255;
    cpu.memory.data[37] = 217;
    cesarStep(cpu);
    expectCesarState(cpu, { r4: 0x3366 });
  });

  it('DisassembleInstruction works as expected', () => {
    expect(cesarDisassembleInstruction([CesarInstruction.Nop])).toEqual({ text: 'NOP', size: 1 });
    expect(cesarDisassembleInstruction([CesarInstruction.Nop + 5])).toEqual({
      text: 'NOP',
      size: 1,
    });
    expect(cesarDisassembleInstruction([CesarInstruction.Hlt])).toEqual({ text: 'HLT', size: 1 });

    expect(cesarDisassembleInstruction([CesarInstruction.Ccc])).toEqual({ text: 'CCC', size: 1 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Ccc |
          CesarFlagBit.Negative |
          CesarFlagBit.Zero |
          CesarFlagBit.Overflow |
          CesarFlagBit.Carry,
      ]),
    ).toEqual({ text: 'CCC NZVC', size: 1 });
    expect(cesarDisassembleInstruction([CesarInstruction.Scc])).toEqual({ text: 'SCC', size: 1 });
    expect(
      cesarDisassembleInstruction([CesarInstruction.Scc | CesarFlagBit.Zero | CesarFlagBit.Carry]),
    ).toEqual({ text: 'SCC ZC', size: 1 });

    expect(cesarDisassembleInstruction([CesarInstruction.Br, 0])).toEqual({
      text: 'BR  0',
      size: 2,
    });
    expect(cesarDisassembleInstruction([CesarInstruction.Bne, 10])).toEqual({
      text: 'BNE 10',
      size: 2,
    });
    expect(cesarDisassembleInstruction([CesarInstruction.Bne, 246])).toEqual({
      text: 'BNE 246',
      size: 2,
    });

    expect(cesarDisassembleInstruction([CesarInstruction.Jmp, CesarRegister.R1])).toEqual({
      text: 'JMP ?',
      size: 2,
    });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Jmp,
        CesarRegister.R1 | CesarAddressMode.RegPostInc,
      ]),
    ).toEqual({ text: 'JMP (R1)+', size: 2 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Jmp,
        CesarRegister.R7 | CesarAddressMode.RegPostInc,
        0,
        0,
      ]),
    ).toEqual({ text: 'JMP (R7)+', size: 4 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Jmp,
        CesarRegister.R1 | CesarAddressMode.RegPostIncIndirect,
      ]),
    ).toEqual({ text: 'JMP ((R1)+)', size: 2 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Jmp,
        CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
        0,
        0,
      ]),
    ).toEqual({ text: 'JMP ((R7)+)', size: 4 });

    expect(
      cesarDisassembleInstruction([CesarInstruction.Sob | CesarRegister.R1, 123]),
    ).toEqual({ text: 'SOB R1, 123', size: 2 });
    expect(
      cesarDisassembleInstruction([CesarInstruction.Sob | CesarRegister.R2, 234]),
    ).toEqual({ text: 'SOB R2, 234', size: 2 });

    expect(
      cesarDisassembleInstruction([CesarInstruction.Jsr | CesarRegister.R1, CesarRegister.R7]),
    ).toEqual({ text: 'JSR R1, ?', size: 2 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Jsr | CesarRegister.R2,
        CesarRegister.R5 | CesarAddressMode.RegPostInc,
      ]),
    ).toEqual({ text: 'JSR R2, (R5)+', size: 2 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Jsr | CesarRegister.R2,
        CesarRegister.R7 | CesarAddressMode.RegPostInc,
        0,
        0,
      ]),
    ).toEqual({ text: 'JSR R2, (R7)+', size: 4 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Jsr | CesarRegister.R3,
        CesarRegister.R5 | CesarAddressMode.RegPostIncIndirect,
      ]),
    ).toEqual({ text: 'JSR R3, ((R5)+)', size: 2 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Jsr | CesarRegister.R3,
        CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
        0,
        0,
      ]),
    ).toEqual({ text: 'JSR R3, ((R7)+)', size: 4 });

    expect(cesarDisassembleInstruction([CesarInstruction.Rts | CesarRegister.R4])).toEqual({
      text: 'RTS R4',
      size: 1,
    });

    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.RegPostInc,
        0,
        15,
      ]),
    ).toEqual({ text: 'NOT (R7)+', size: 4 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
        0,
        15,
      ]),
    ).toEqual({ text: 'NOT ((R7)+)', size: 4 });

    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.Register,
      ]),
    ).toEqual({ text: 'NOT R7', size: 2 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.RegPostInc,
        0,
        0,
      ]),
    ).toEqual({ text: 'NOT (R7)+', size: 4 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.RegPreDec,
      ]),
    ).toEqual({ text: 'NOT -(R7)', size: 2 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.Indexed,
        0,
        2,
      ]),
    ).toEqual({ text: 'NOT 2(R7)', size: 4 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.RegisterIndirect,
      ]),
    ).toEqual({ text: 'NOT (R7)', size: 2 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
        0,
        0,
      ]),
    ).toEqual({ text: 'NOT ((R7)+)', size: 4 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.RegPreDecIndirect,
      ]),
    ).toEqual({ text: 'NOT (-(R7))', size: 2 });
    expect(
      cesarDisassembleInstruction([
        CesarInstruction.Not,
        CesarRegister.R7 | CesarAddressMode.IndexedIndirect,
        0,
        2,
      ]),
    ).toEqual({ text: 'NOT (2(R7))', size: 4 });

    const e = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegPostInc,
      CesarRegister.R7,
      CesarAddressMode.Register,
      CesarRegister.R1,
    );
    expect(
      cesarDisassembleInstruction([(e >>> 8) & 0xff, e & 0xff, 0, 10]),
    ).toEqual({ text: 'MOV (R7)+, R1', size: 4 });

    const f = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegPostIncIndirect,
      CesarRegister.R7,
      CesarAddressMode.Register,
      CesarRegister.R2,
    );
    expect(
      cesarDisassembleInstruction([(f >>> 8) & 0xff, f & 0xff, 3, 232]),
    ).toEqual({ text: 'MOV ((R7)+), R2', size: 4 });

    const g = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.Register,
      CesarRegister.R1,
      CesarAddressMode.RegPostInc,
      CesarRegister.R2,
    );
    expect(cesarDisassembleInstruction([(g >>> 8) & 0xff, g & 0xff])).toEqual({
      text: 'MOV R1, (R2)+',
      size: 2,
    });

    const h = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegisterIndirect,
      CesarRegister.R1,
      CesarAddressMode.RegPreDec,
      CesarRegister.R2,
    );
    expect(cesarDisassembleInstruction([(h >>> 8) & 0xff, h & 0xff])).toEqual({
      text: 'MOV (R1), -(R2)',
      size: 2,
    });

    const i = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.Indexed,
      CesarRegister.R1,
      CesarAddressMode.RegPostIncIndirect,
      CesarRegister.R2,
    );
    expect(
      cesarDisassembleInstruction([(i >>> 8) & 0xff, i & 0xff, 0, 10]),
    ).toEqual({ text: 'MOV 10(R1), ((R2)+)', size: 4 });

    const j = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R1,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R2,
    );
    expect(
      cesarDisassembleInstruction([(j >>> 8) & 0xff, j & 0xff, 0, 10, 0, 20]),
    ).toEqual({ text: 'MOV (10(R1)), (20(R2))', size: 6 });

    const k = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegPreDecIndirect,
      CesarRegister.R1,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R2,
    );
    expect(
      cesarDisassembleInstruction([(k >>> 8) & 0xff, k & 0xff, 0, 20]),
    ).toEqual({ text: 'MOV (-(R1)), (20(R2))', size: 4 });
  });

  it('DisassembleInstructions works as expected', () => {
    const j = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R1,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R2,
    );

    const p1 = [
      CesarInstruction.Ccc |
        CesarFlagBit.Negative |
        CesarFlagBit.Zero |
        CesarFlagBit.Overflow |
        CesarFlagBit.Carry,
      CesarInstruction.Bne,
      10,
      CesarInstruction.Nop,
      CesarInstruction.Jmp,
      CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
      0,
      0,
      CesarInstruction.Rts | CesarRegister.R4,
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.Indexed,
      0,
      2,
      (j >>> 8) & 0xff,
      j & 0xff,
      0,
      10,
      0,
      20,
      CesarInstruction.Hlt,
    ];

    expect(cesarDisassembleInstructions(p1)).toEqual([
      { text: 'CCC NZVC', size: 1 },
      { text: 'BNE 10', size: 2 },
      { text: 'NOP', size: 1 },
      { text: 'JMP ((R7)+)', size: 4 },
      { text: 'RTS R4', size: 1 },
      { text: 'NOT 2(R7)', size: 4 },
      { text: 'MOV (10(R1)), (20(R2))', size: 6 },
      { text: 'HLT', size: 1 },
    ]);

    const p2 = [
      147, 193, 0, 10, // MOV #10, R1
      155, 194, 3, 232, // MOV 1000, R2
      155, 195, 0, 20, // MOV :L1, R3
      155, 196, 3, 234, // MOV :L2, R4
      155, 197, 3, 234, // MOV :L2, R5
      240, // HLT
      0, // NOP
      0, // NOP
      48, 251, // BR :L1
      81, 7, // SOB R1, :L1
      64, 47, 0, 20, // JMP :L1
      48, 8, // BR :L3
      83, 250, // SOB R3, :L3
      64, 47, 0, 41, // JMP :L3
      0, // NOP
      0, // NOP
      64, 47, 3, 234, // JMP :L2
    ];

    expect(cesarDisassembleInstructions(p2)).toEqual([
      { text: 'MOV (R7)+, R1', size: 4 },
      { text: 'MOV ((R7)+), R2', size: 4 },
      { text: 'MOV ((R7)+), R3', size: 4 },
      { text: 'MOV ((R7)+), R4', size: 4 },
      { text: 'MOV ((R7)+), R5', size: 4 },
      { text: 'HLT', size: 1 },
      { text: 'NOP', size: 1 },
      { text: 'NOP', size: 1 },
      { text: 'BR  251', size: 2 },
      { text: 'SOB R1, 7', size: 2 },
      { text: 'JMP ((R7)+)', size: 4 },
      { text: 'BR  8', size: 2 },
      { text: 'SOB R3, 250', size: 2 },
      { text: 'JMP ((R7)+)', size: 4 },
      { text: 'NOP', size: 1 },
      { text: 'NOP', size: 1 },
      { text: 'JMP ((R7)+)', size: 4 },
    ]);
  });
});
