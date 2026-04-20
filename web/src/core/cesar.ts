import {
  createMemory,
  memoryReadByte,
  memoryReadWordBigEndian,
  memoryReset,
  memoryWriteByte,
  memoryWriteWordBigEndian,
  type Memory,
} from './memory';

export const CesarInstruction = {
  Nop: 0x00,
  Ccc: 0x10,
  Scc: 0x20,
  Br: 0x30,
  Bne: 0x31,
  Beq: 0x32,
  Bpl: 0x33,
  Bmi: 0x34,
  Bvc: 0x35,
  Bvs: 0x36,
  Bcc: 0x37,
  Bcs: 0x38,
  Bge: 0x39,
  Blt: 0x3a,
  Bgt: 0x3b,
  Ble: 0x3c,
  Bhi: 0x3d,
  Bls: 0x3e,
  Jmp: 0x40,
  Sob: 0x50,
  Jsr: 0x60,
  Rts: 0x70,
  Clr: 0x80,
  Not: 0x81,
  Inc: 0x82,
  Dec: 0x83,
  Neg: 0x84,
  Tst: 0x85,
  Ror: 0x86,
  Rol: 0x87,
  Asr: 0x88,
  Asl: 0x89,
  Adc: 0x8a,
  Sbc: 0x8b,
  Mov: 0x90,
  Add: 0xa0,
  Sub: 0xb0,
  Cmp: 0xc0,
  And: 0xd0,
  Or: 0xe0,
  Hlt: 0xf0,
} as const;

export type CesarInstruction = (typeof CesarInstruction)[keyof typeof CesarInstruction];

export const CesarRegister = {
  R0: 0,
  R1: 1,
  R2: 2,
  R3: 3,
  R4: 4,
  R5: 5,
  R6: 6,
  R7: 7,
} as const;

export type CesarRegister = (typeof CesarRegister)[keyof typeof CesarRegister];

export const CesarAddressMode = {
  Register: 0b000_000,
  RegPostInc: 0b001_000,
  RegPreDec: 0b010_000,
  Indexed: 0b011_000,
  RegisterIndirect: 0b100_000,
  RegPostIncIndirect: 0b101_000,
  RegPreDecIndirect: 0b110_000,
  IndexedIndirect: 0b111_000,
} as const;

export type CesarAddressMode = (typeof CesarAddressMode)[keyof typeof CesarAddressMode];

export const CesarFlagBit = {
  Negative: 0b1000,
  Zero: 0b0100,
  Overflow: 0b0010,
  Carry: 0b0001,
} as const;

export const CesarInstructionMask = 0b1111_0000;
export const CesarSubInstructionMask = 0b0000_1111;
export const CesarAddressModeMask = 0b0011_1000;
export const CesarRegisterMask = 0b0000_0111;

export const CesarKeyboardMemoryAddress = 0xffda;
export const CesarDisplayMemoryAddress = 0xffdc;

const CesarMemorySize = 65536;

type Operand =
  | { kind: 'none' }
  | { kind: 'reg'; register: number }
  | { kind: 'addr'; address: number };

const NoOp: Operand = { kind: 'none' };

export interface CesarInstructionRegister {
  data: number[];
  sourceOperand: Operand;
  targetOperand: Operand;
}

export interface CesarFlags {
  halted: boolean;
  negative: boolean;
  zero: boolean;
  overflow: boolean;
  carry: boolean;
}

export interface CesarRegisters {
  r: Uint16Array;
  instructionRegister: CesarInstructionRegister;
  flags: CesarFlags;
}

export interface CesarCpu {
  registers: CesarRegisters;
  memory: Memory;
}

export function createCesarRegisters(): CesarRegisters {
  return {
    r: new Uint16Array(8),
    instructionRegister: { data: [0], sourceOperand: NoOp, targetOperand: NoOp },
    flags: { halted: false, negative: false, zero: true, overflow: false, carry: false },
  };
}

export function cesarRegistersReset(registers: CesarRegisters): void {
  registers.r.fill(0);
  registers.instructionRegister.data = [0];
  registers.instructionRegister.sourceOperand = NoOp;
  registers.instructionRegister.targetOperand = NoOp;
  registers.flags.halted = false;
  registers.flags.negative = false;
  registers.flags.zero = true;
  registers.flags.overflow = false;
  registers.flags.carry = false;
}

export function createCesarCpu(): CesarCpu {
  return { registers: createCesarRegisters(), memory: createMemory(CesarMemorySize) };
}

const isNegative16 = (value: number): boolean => (value & 0xffff) > 0x7fff;

export function encodeCesarInstructionOneOperand(
  instruction: number,
  targetMode: number,
  targetRegister: number,
): number {
  return (
    ((instruction & 0xff) << 8) |
    (targetMode & CesarAddressModeMask) |
    (targetRegister & CesarRegisterMask)
  );
}

export function encodeCesarInstructionTwoOperand(
  instruction: number,
  sourceMode: number,
  sourceRegister: number,
  targetMode: number,
  targetRegister: number,
): number {
  switch (instruction) {
    case CesarInstruction.Mov:
    case CesarInstruction.Add:
    case CesarInstruction.Sub:
    case CesarInstruction.Cmp:
    case CesarInstruction.And:
    case CesarInstruction.Or: {
      const firstPart = encodeCesarInstructionOneOperand(instruction, targetMode, targetRegister);
      return (
        firstPart |
        ((sourceMode & CesarAddressModeMask) << 6) |
        ((sourceRegister & CesarRegisterMask) << 6)
      );
    }
    default:
      throw new Error('encodeCesarInstructionTwoOperand only encodes two-operand instructions.');
  }
}

export function cesarFetch(cpu: CesarCpu): void {
  const r = cpu.registers.r;
  const ir = cpu.registers.instructionRegister;

  const appendByte = (value: number): void => {
    ir.data.push(value & 0xff);
  };
  const appendWord = (value: number): void => {
    ir.data.push((value >>> 8) & 0xff);
    ir.data.push(value & 0xff);
  };

  const readRegisterAndInc = (register: number): number => {
    const result = r[register] ?? 0;
    r[register] = (result + 2) & 0xffff;
    return result;
  };
  const decRegisterAndRead = (register: number): number => {
    const result = ((r[register] ?? 0) - 2) & 0xffff;
    r[register] = result;
    return result;
  };
  const readWordAndInc = (register: number): number => {
    const result = memoryReadWordBigEndian(cpu.memory, r[register] ?? 0);
    r[register] = ((r[register] ?? 0) + 2) & 0xffff;
    return result;
  };
  const decAndReadWord = (register: number): number => {
    r[register] = ((r[register] ?? 0) - 2) & 0xffff;
    return memoryReadWordBigEndian(cpu.memory, r[register] ?? 0);
  };

  const decodeOperand = (mode: number, register: number): Operand => {
    switch (mode) {
      case CesarAddressMode.Register:
        return { kind: 'reg', register };
      case CesarAddressMode.RegPostInc: {
        const address = readRegisterAndInc(register);
        if (register === CesarRegister.R7) appendWord(address);
        return { kind: 'addr', address };
      }
      case CesarAddressMode.RegPreDec:
        return { kind: 'addr', address: decRegisterAndRead(register) };
      case CesarAddressMode.Indexed: {
        const displacement = readWordAndInc(CesarRegister.R7);
        appendWord(displacement);
        return { kind: 'addr', address: ((r[register] ?? 0) + displacement) & 0xffff };
      }
      case CesarAddressMode.RegisterIndirect:
        return { kind: 'addr', address: r[register] ?? 0 };
      case CesarAddressMode.RegPostIncIndirect: {
        const address = readWordAndInc(register);
        if (register === CesarRegister.R7) appendWord(address);
        return { kind: 'addr', address };
      }
      case CesarAddressMode.RegPreDecIndirect:
        return { kind: 'addr', address: decAndReadWord(register) };
      case CesarAddressMode.IndexedIndirect: {
        const indirectAddr = readWordAndInc(CesarRegister.R7);
        appendWord(indirectAddr);
        const indexedAddr = ((r[register] ?? 0) + indirectAddr) & 0xffff;
        return { kind: 'addr', address: memoryReadWordBigEndian(cpu.memory, indexedAddr) };
      }
      default:
        throw new Error(`Invalid Cesar AddressMode: ${mode}`);
    }
  };

  const readByteAndAdvance = (): number => {
    const value = memoryReadByte(cpu.memory, r[7] ?? 0);
    r[7] = ((r[7] ?? 0) + 1) & 0xffff;
    return value;
  };

  const firstOpCode = readByteAndAdvance();
  ir.data = [firstOpCode];
  ir.sourceOperand = NoOp;
  ir.targetOperand = NoOp;

  const instruction = firstOpCode & CesarInstructionMask;
  switch (instruction) {
    case CesarInstruction.Nop:
    case CesarInstruction.Ccc:
    case CesarInstruction.Scc:
    case CesarInstruction.Rts:
    case CesarInstruction.Hlt:
      return;
  }

  const secondOpCode = readByteAndAdvance();
  appendByte(secondOpCode);

  if (instruction === CesarInstruction.Br || instruction === CesarInstruction.Sob) {
    return;
  }

  switch (instruction) {
    case CesarInstruction.Mov:
    case CesarInstruction.Add:
    case CesarInstruction.Sub:
    case CesarInstruction.Cmp:
    case CesarInstruction.And:
    case CesarInstruction.Or: {
      const sourceMode = (firstOpCode << 2) & CesarAddressModeMask;
      const sourceRegister =
        (((firstOpCode & 0x01) << 2) | (secondOpCode >>> 6)) & CesarRegisterMask;
      ir.sourceOperand = decodeOperand(sourceMode, sourceRegister);
      break;
    }
  }
  const targetMode = secondOpCode & CesarAddressModeMask;
  const targetRegister = secondOpCode & CesarRegisterMask;
  ir.targetOperand = decodeOperand(targetMode, targetRegister);
}

export function cesarExecute(cpu: CesarCpu): void {
  const r = cpu.registers.r;
  const ir = cpu.registers.instructionRegister;
  const flags = cpu.registers.flags;

  const aluAdd = (a: number, b: number, carryIn: boolean): number => {
    const full = (a & 0xffff) + (b & 0xffff) + (carryIn ? 1 : 0);
    flags.carry = full > 0xffff;
    const result = full & 0xffff;
    const sa = isNegative16(a);
    const sb = isNegative16(b);
    const sr = isNegative16(result);
    flags.overflow = (sa && sb && !sr) || (!sa && !sb && sr);
    return result;
  };

  const readValueFromOperand = (operand: Operand): number => {
    switch (operand.kind) {
      case 'reg':
        return r[operand.register] ?? 0;
      case 'addr':
        if (operand.address >= CesarKeyboardMemoryAddress) {
          return memoryReadByte(cpu.memory, operand.address) & 0xff;
        }
        return memoryReadWordBigEndian(cpu.memory, operand.address);
      default:
        return 0;
    }
  };

  const writeValueToOperand = (operand: Operand, value: number): void => {
    switch (operand.kind) {
      case 'reg':
        r[operand.register] = value & 0xffff;
        break;
      case 'addr':
        if (operand.address >= CesarKeyboardMemoryAddress) {
          memoryWriteByte(cpu.memory, operand.address, value & 0xff);
        } else {
          memoryWriteWordBigEndian(cpu.memory, operand.address, value & 0xffff);
        }
        break;
      case 'none':
        break;
    }
  };

  const firstOpCode = ir.data[0] ?? 0;
  const instruction = firstOpCode & CesarInstructionMask;
  const register = firstOpCode & CesarRegisterMask;

  switch (instruction) {
    case CesarInstruction.Ccc:
    case CesarInstruction.Scc: {
      const set = instruction === CesarInstruction.Scc;
      if ((firstOpCode & CesarFlagBit.Negative) !== 0) flags.negative = set;
      if ((firstOpCode & CesarFlagBit.Zero) !== 0) flags.zero = set;
      if ((firstOpCode & CesarFlagBit.Overflow) !== 0) flags.overflow = set;
      if ((firstOpCode & CesarFlagBit.Carry) !== 0) flags.carry = set;
      break;
    }

    case CesarInstruction.Br: {
      const subInstruction = (CesarInstruction.Br | (firstOpCode & CesarSubInstructionMask)) & 0xff;
      const branchByte = ir.data[1] ?? 0;
      const offset = branchByte > 0x7f ? branchByte - 0x100 : branchByte;
      const branchIf = (cond: boolean): void => {
        if (cond) r[7] = ((r[7] ?? 0) + offset) & 0xffff;
      };
      const branchIfNot = (cond: boolean): void => branchIf(!cond);

      switch (subInstruction) {
        case CesarInstruction.Br:
          branchIf(true);
          break;
        case CesarInstruction.Bne:
          branchIfNot(flags.zero);
          break;
        case CesarInstruction.Beq:
          branchIf(flags.zero);
          break;
        case CesarInstruction.Bpl:
          branchIfNot(flags.negative);
          break;
        case CesarInstruction.Bmi:
          branchIf(flags.negative);
          break;
        case CesarInstruction.Bvc:
          branchIfNot(flags.overflow);
          break;
        case CesarInstruction.Bvs:
          branchIf(flags.overflow);
          break;
        case CesarInstruction.Bcc:
          branchIfNot(flags.carry);
          break;
        case CesarInstruction.Bcs:
          branchIf(flags.carry);
          break;
        case CesarInstruction.Bge:
          branchIf(flags.negative === flags.overflow);
          break;
        case CesarInstruction.Blt:
          branchIfNot(flags.negative === flags.overflow);
          break;
        case CesarInstruction.Bgt:
          branchIf(flags.negative === flags.overflow && !flags.zero);
          break;
        case CesarInstruction.Ble:
          branchIfNot(flags.negative === flags.overflow && !flags.zero);
          break;
        case CesarInstruction.Bhi:
          branchIfNot(flags.carry || flags.zero);
          break;
        case CesarInstruction.Bls:
          branchIf(flags.carry || flags.zero);
          break;
      }
      break;
    }

    case CesarInstruction.Jmp: {
      // Register-mode targets are illegal — only Addr-form jumps execute.
      if (ir.targetOperand.kind === 'addr') r[7] = ir.targetOperand.address;
      break;
    }

    case CesarInstruction.Sob: {
      r[register] = ((r[register] ?? 0) - 1) & 0xffff;
      if ((r[register] ?? 0) !== 0) {
        const branchOperand = ir.data[1] ?? 0;
        r[7] = ((r[7] ?? 0) - branchOperand) & 0xffff;
      }
      break;
    }

    case CesarInstruction.Jsr: {
      if (ir.targetOperand.kind === 'addr') {
        r[6] = ((r[6] ?? 0) - 2) & 0xffff;
        memoryWriteWordBigEndian(cpu.memory, r[6] ?? 0, r[register] ?? 0);
        r[register] = r[7] ?? 0;
        r[7] = ir.targetOperand.address;
      }
      break;
    }

    case CesarInstruction.Rts: {
      r[7] = r[register] ?? 0;
      r[register] = memoryReadWordBigEndian(cpu.memory, r[6] ?? 0);
      r[6] = ((r[6] ?? 0) + 2) & 0xffff;
      break;
    }

    case CesarInstruction.Clr: {
      const subInstruction = (CesarInstruction.Clr | (firstOpCode & CesarSubInstructionMask)) & 0xff;
      let target = 0;
      if (subInstruction !== CesarInstruction.Clr) {
        target = readValueFromOperand(ir.targetOperand);
      }

      switch (subInstruction) {
        case CesarInstruction.Clr:
          flags.carry = false;
          flags.overflow = false;
          target = 0;
          break;
        case CesarInstruction.Not:
          flags.carry = true;
          flags.overflow = false;
          target = ~target & 0xffff;
          break;
        case CesarInstruction.Inc:
          target = aluAdd(target, 1, false);
          break;
        case CesarInstruction.Dec:
          target = aluAdd(target, 0xffff, false);
          flags.carry = !flags.carry;
          break;
        case CesarInstruction.Neg:
          target = aluAdd(~target & 0xffff, 1, false);
          flags.carry = !flags.carry;
          break;
        case CesarInstruction.Tst:
          flags.carry = false;
          flags.overflow = false;
          break;
        case CesarInstruction.Ror: {
          const higherBit = flags.carry ? 0x8000 : 0;
          flags.carry = (target & 1) !== 0;
          target = ((target >>> 1) | higherBit) & 0xffff;
          flags.negative = isNegative16(target);
          flags.overflow = flags.carry !== flags.negative;
          break;
        }
        case CesarInstruction.Rol: {
          const lowerBit = flags.carry ? 1 : 0;
          flags.carry = (target & 0x8000) !== 0;
          target = ((target << 1) | lowerBit) & 0xffff;
          flags.negative = isNegative16(target);
          flags.overflow = flags.carry !== flags.negative;
          break;
        }
        case CesarInstruction.Asr: {
          const higherBit = target & 0x8000;
          flags.carry = (target & 1) !== 0;
          target = ((target >>> 1) | higherBit) & 0xffff;
          flags.negative = !isNegative16(target);
          flags.overflow = flags.carry !== flags.negative;
          break;
        }
        case CesarInstruction.Asl:
          flags.carry = (target & 0x8000) !== 0;
          target = (target << 1) & 0xffff;
          flags.negative = !isNegative16(target);
          flags.overflow = flags.carry !== flags.negative;
          break;
        case CesarInstruction.Adc:
          target = aluAdd(target, flags.carry ? 1 : 0, false);
          break;
        case CesarInstruction.Sbc: {
          const negCarry = flags.carry ? 0xfffe : 0xffff;
          target = aluAdd(target, negCarry, true);
          break;
        }
      }

      flags.negative = isNegative16(target);
      flags.zero = target === 0;

      if (subInstruction !== CesarInstruction.Tst) {
        writeValueToOperand(ir.targetOperand, target);
      }
      break;
    }

    case CesarInstruction.Mov:
    case CesarInstruction.Add:
    case CesarInstruction.Sub:
    case CesarInstruction.Cmp:
    case CesarInstruction.And:
    case CesarInstruction.Or: {
      const sourceValue = readValueFromOperand(ir.sourceOperand);
      let target = 0;
      if (instruction !== CesarInstruction.Mov) {
        target = readValueFromOperand(ir.targetOperand);
      }

      switch (instruction) {
        case CesarInstruction.Mov:
          target = sourceValue;
          flags.overflow = false;
          break;
        case CesarInstruction.Add:
          target = aluAdd(sourceValue, target, false);
          break;
        case CesarInstruction.Sub:
          target = aluAdd(~sourceValue & 0xffff, target, true);
          flags.carry = !flags.carry;
          break;
        case CesarInstruction.Cmp:
          target = aluAdd(sourceValue, ~target & 0xffff, true);
          flags.carry = !flags.carry;
          break;
        case CesarInstruction.And:
          target = target & sourceValue & 0xffff;
          flags.overflow = false;
          break;
        case CesarInstruction.Or:
          target = (target | sourceValue) & 0xffff;
          flags.overflow = false;
          break;
      }

      flags.negative = isNegative16(target);
      flags.zero = target === 0;

      if (instruction !== CesarInstruction.Cmp) {
        writeValueToOperand(ir.targetOperand, target);
      }
      break;
    }

    default:
      // Hlt, Nop, or unknown: nothing else to do.
      break;
  }

  flags.halted = instruction === CesarInstruction.Hlt;
}

export function cesarStep(cpu: CesarCpu): void {
  cesarFetch(cpu);
  cesarExecute(cpu);
}

export function cesarReset(cpu: CesarCpu): void {
  cesarRegistersReset(cpu.registers);
  memoryReset(cpu.memory);
}

interface DisassembledInstruction {
  text: string;
  size: number;
}

export function cesarDisassembleInstruction(content: ArrayLike<number>): DisassembledInstruction {
  if (content.length === 0) return { text: '', size: 0 };
  const data: number[] = [];
  for (let i = 0; i < content.length; i++) data.push(content[i] ?? 0);
  const firstOpCode = data[0] ?? 0;
  const instruction = firstOpCode & CesarInstructionMask;

  let size: number;
  switch (instruction) {
    case CesarInstruction.Ccc:
    case CesarInstruction.Scc:
    case CesarInstruction.Rts:
    case CesarInstruction.Hlt:
    case CesarInstruction.Nop:
      size = 1;
      break;
    default:
      size = 2;
      break;
  }

  const flagsLabel = (): string => {
    let out = '';
    if ((firstOpCode & CesarFlagBit.Negative) !== 0) out += 'N';
    if ((firstOpCode & CesarFlagBit.Zero) !== 0) out += 'Z';
    if ((firstOpCode & CesarFlagBit.Overflow) !== 0) out += 'V';
    if ((firstOpCode & CesarFlagBit.Carry) !== 0) out += 'C';
    return out;
  };

  const registerName = (reg: number): string => `R${reg & CesarRegisterMask}`;
  const readData = (offset: number): number => (offset < data.length ? (data[offset] ?? 0) : 0);

  const getNextOperand = (): number => {
    const result = (readData(size) << 8) | readData(size + 1);
    size += 2;
    return result & 0xffff;
  };

  const decodeOperand = (mode: number, reg: number): string => {
    const r = registerName(reg);
    if ((reg & CesarRegisterMask) === 7) {
      if (mode === CesarAddressMode.RegPostInc || mode === CesarAddressMode.RegPostIncIndirect) {
        size += 2;
      }
    }
    switch (mode) {
      case CesarAddressMode.Register:
        return r;
      case CesarAddressMode.RegPostInc:
        return `(${r})+`;
      case CesarAddressMode.RegPreDec:
        return `-(${r})`;
      case CesarAddressMode.Indexed:
        return `${getNextOperand()}(${r})`;
      case CesarAddressMode.RegisterIndirect:
        return `(${r})`;
      case CesarAddressMode.RegPostIncIndirect:
        return `((${r})+)`;
      case CesarAddressMode.RegPreDecIndirect:
        return `(-(${r}))`;
      case CesarAddressMode.IndexedIndirect:
        return `(${getNextOperand()}(${r}))`;
      default:
        return '?';
    }
  };

  const branchOperand = (): string => `${readData(1)}`;
  const targetOperand = (): string => {
    const second = readData(1);
    return decodeOperand(second & CesarAddressModeMask, second & CesarRegisterMask);
  };
  const jumpOperand = (): string => {
    const second = readData(1);
    if ((second & CesarAddressModeMask) === CesarAddressMode.Register) return '?';
    return targetOperand();
  };
  const sourceOperand = (): string => {
    const second = readData(1);
    const mode = (firstOpCode << 2) & CesarAddressModeMask;
    const reg = (((firstOpCode & 0x01) << 2) | (second >>> 6)) & CesarRegisterMask;
    return decodeOperand(mode, reg);
  };

  let output: string;
  switch (instruction) {
    case CesarInstruction.Ccc:
      output = `CCC ${flagsLabel()}`.trim();
      break;
    case CesarInstruction.Scc:
      output = `SCC ${flagsLabel()}`.trim();
      break;
    case CesarInstruction.Br: {
      const sub = (CesarInstruction.Br | (firstOpCode & CesarSubInstructionMask)) & 0xff;
      const labels: Record<number, string> = {
        [CesarInstruction.Br]: 'BR ',
        [CesarInstruction.Bne]: 'BNE',
        [CesarInstruction.Beq]: 'BEQ',
        [CesarInstruction.Bpl]: 'BPL',
        [CesarInstruction.Bmi]: 'BMI',
        [CesarInstruction.Bvc]: 'BVC',
        [CesarInstruction.Bvs]: 'BVS',
        [CesarInstruction.Bcc]: 'BCC',
        [CesarInstruction.Bcs]: 'BCS',
        [CesarInstruction.Bge]: 'BGE',
        [CesarInstruction.Blt]: 'BLT',
        [CesarInstruction.Bgt]: 'BGT',
        [CesarInstruction.Ble]: 'BLE',
        [CesarInstruction.Bhi]: 'BHI',
        [CesarInstruction.Bls]: 'BLS',
      };
      output = `${labels[sub] ?? '?'} ${branchOperand()}`;
      break;
    }
    case CesarInstruction.Jmp:
      output = `JMP ${jumpOperand()}`;
      break;
    case CesarInstruction.Sob:
      output = `SOB ${registerName(firstOpCode)}, ${branchOperand()}`;
      break;
    case CesarInstruction.Jsr:
      output = `JSR ${registerName(firstOpCode)}, ${jumpOperand()}`;
      break;
    case CesarInstruction.Rts:
      output = `RTS ${registerName(firstOpCode)}`;
      break;
    case CesarInstruction.Clr: {
      const sub = (CesarInstruction.Clr | (firstOpCode & CesarSubInstructionMask)) & 0xff;
      const labels: Record<number, string> = {
        [CesarInstruction.Clr]: 'CLR',
        [CesarInstruction.Not]: 'NOT',
        [CesarInstruction.Inc]: 'INC',
        [CesarInstruction.Dec]: 'DEC',
        [CesarInstruction.Neg]: 'NEG',
        [CesarInstruction.Tst]: 'TST',
        [CesarInstruction.Ror]: 'ROR',
        [CesarInstruction.Rol]: 'ROL',
        [CesarInstruction.Asr]: 'ASR',
        [CesarInstruction.Asl]: 'ASL',
        [CesarInstruction.Adc]: 'ADC',
        [CesarInstruction.Sbc]: 'SBC',
      };
      output = `${labels[sub] ?? '?'} ${targetOperand()}`;
      break;
    }
    case CesarInstruction.Mov:
      output = `MOV ${sourceOperand()}, ${targetOperand()}`;
      break;
    case CesarInstruction.Add:
      output = `ADD ${sourceOperand()}, ${targetOperand()}`;
      break;
    case CesarInstruction.Sub:
      output = `SUB ${sourceOperand()}, ${targetOperand()}`;
      break;
    case CesarInstruction.Cmp:
      output = `CMP ${sourceOperand()}, ${targetOperand()}`;
      break;
    case CesarInstruction.And:
      output = `AND ${sourceOperand()}, ${targetOperand()}`;
      break;
    case CesarInstruction.Or:
      output = `OR  ${sourceOperand()}, ${targetOperand()}`;
      break;
    case CesarInstruction.Hlt:
      output = 'HLT';
      break;
    default:
      output = 'NOP';
      break;
  }

  if (size > content.length) return { text: '', size: 0 };
  return { text: output, size };
}

export function cesarDisassembleInstructions(
  content: ArrayLike<number>,
): DisassembledInstruction[] {
  const out: DisassembledInstruction[] = [];
  let offset = 0;
  while (offset < content.length) {
    const slice: number[] = [];
    for (let i = offset; i < content.length; i++) slice.push(content[i] ?? 0);
    const { text, size } = cesarDisassembleInstruction(slice);
    if (size === 0) break;
    out.push({ text, size });
    offset += size;
  }
  return out;
}
