import {
  createMemory,
  memoryReadByte,
  memoryReset,
  memoryWriteByte,
  type Memory,
} from './memory';

export const RamsesInstruction = {
  Nop: 0x00,
  Str: 0x10,
  Ldr: 0x20,
  Add: 0x30,
  Or: 0x40,
  And: 0x50,
  Not: 0x60,
  Sub: 0x70,
  Jmp: 0x80,
  Jn: 0x90,
  Jz: 0xa0,
  Jc: 0xb0,
  Jsr: 0xc0,
  Neg: 0xd0,
  Shr: 0xe0,
  Hlt: 0xf0,
} as const;

export type RamsesInstruction = (typeof RamsesInstruction)[keyof typeof RamsesInstruction];

export const RamsesRegister = {
  Ra: 0x00,
  Rb: 0x04,
  Rx: 0x08,
  Pc: 0x0c,
} as const;

export type RamsesRegister = (typeof RamsesRegister)[keyof typeof RamsesRegister];

export const RamsesAddressMode = {
  Direct: 0x00,
  Indirect: 0x01,
  Immediate: 0x02,
  Indexed: 0x03,
} as const;

export type RamsesAddressMode = (typeof RamsesAddressMode)[keyof typeof RamsesAddressMode];

export const RamsesInstructionMask = 0b11110000;
export const RamsesRegisterMask = 0b00001100;
export const RamsesAddressModeMask = 0b00000011;

export interface RamsesInstructionRegister {
  opCode: number;
  operandAddress: number;
}

export interface RamsesFlags {
  halted: boolean;
  negative: boolean;
  zero: boolean;
  carry: boolean;
}

export interface RamsesRegisters {
  ra: number;
  rb: number;
  rx: number;
  programCounter: number;
  instructionRegister: RamsesInstructionRegister;
  flags: RamsesFlags;
}

export interface RamsesCpu {
  registers: RamsesRegisters;
  memory: Memory;
}

export function createRamsesRegisters(): RamsesRegisters {
  return {
    ra: 0,
    rb: 0,
    rx: 0,
    programCounter: 0,
    instructionRegister: { opCode: 0, operandAddress: 0 },
    flags: { halted: false, negative: false, zero: true, carry: false },
  };
}

export function ramsesRegistersReset(registers: RamsesRegisters): void {
  registers.ra = 0;
  registers.rb = 0;
  registers.rx = 0;
  registers.programCounter = 0;
  registers.instructionRegister.opCode = 0;
  registers.instructionRegister.operandAddress = 0;
  registers.flags.halted = false;
  registers.flags.negative = false;
  registers.flags.zero = true;
  registers.flags.carry = false;
}

export function createRamsesCpu(): RamsesCpu {
  return { registers: createRamsesRegisters(), memory: createMemory(256) };
}

function instructionTakesOperand(instruction: number): boolean {
  switch (instruction) {
    case RamsesInstruction.Str:
    case RamsesInstruction.Ldr:
    case RamsesInstruction.Add:
    case RamsesInstruction.Or:
    case RamsesInstruction.And:
    case RamsesInstruction.Sub:
    case RamsesInstruction.Jmp:
    case RamsesInstruction.Jn:
    case RamsesInstruction.Jz:
    case RamsesInstruction.Jc:
    case RamsesInstruction.Jsr:
      return true;
    default:
      return false;
  }
}

export function ramsesFetch(cpu: RamsesCpu): void {
  const r = cpu.registers;

  const returnPcAndAdvance = (): number => {
    const value = r.programCounter;
    r.programCounter = (r.programCounter + 1) & 0xff;
    return value;
  };
  const readByteAndAdvance = (): number => {
    const value = memoryReadByte(cpu.memory, r.programCounter);
    r.programCounter = (r.programCounter + 1) & 0xff;
    return value;
  };

  r.instructionRegister.opCode = readByteAndAdvance();

  const instruction = r.instructionRegister.opCode & RamsesInstructionMask;
  const addressMode = r.instructionRegister.opCode & RamsesAddressModeMask;

  if (!instructionTakesOperand(instruction)) {
    r.instructionRegister.operandAddress = 0;
    return;
  }

  let operandAddress = 0;
  switch (addressMode) {
    case RamsesAddressMode.Direct:
      operandAddress = readByteAndAdvance();
      break;
    case RamsesAddressMode.Indirect:
      operandAddress = memoryReadByte(cpu.memory, readByteAndAdvance());
      break;
    case RamsesAddressMode.Immediate:
      // Jumps treat the byte as the destination address; ALU ops treat the
      // PC slot itself as the operand address so readOperand reads the literal.
      operandAddress =
        instruction >= RamsesInstruction.Jmp ? readByteAndAdvance() : returnPcAndAdvance();
      break;
    case RamsesAddressMode.Indexed:
      operandAddress = (r.rx + readByteAndAdvance()) & 0xff;
      break;
  }
  r.instructionRegister.operandAddress = operandAddress;
}

export function ramsesExecute(cpu: RamsesCpu): void {
  const r = cpu.registers;
  const ir = r.instructionRegister;
  const flags = r.flags;
  const op = ir.opCode;
  const instruction = op & RamsesInstructionMask;
  const register = op & RamsesRegisterMask;

  const readOperand = (): number => memoryReadByte(cpu.memory, ir.operandAddress);

  const readRegister = (): number => {
    switch (register) {
      case RamsesRegister.Ra:
        return r.ra;
      case RamsesRegister.Rb:
        return r.rb;
      case RamsesRegister.Rx:
        return r.rx;
      default:
        return r.programCounter;
    }
  };

  const writeRegister = (value: number): void => {
    const v = value & 0xff;
    switch (register) {
      case RamsesRegister.Ra:
        r.ra = v;
        break;
      case RamsesRegister.Rb:
        r.rb = v;
        break;
      case RamsesRegister.Rx:
        r.rx = v;
        break;
      default:
        r.programCounter = v;
        break;
    }
    flags.zero = v === 0;
    flags.negative = v > 0x7f;
  };

  const writeRegisterAndCarry = (
    fullValue: number,
    carryFn: (full: number) => boolean,
  ): void => {
    writeRegister(fullValue);
    flags.carry = carryFn(fullValue);
  };

  const jumpIf = (cond: boolean): void => {
    if (cond) r.programCounter = ir.operandAddress;
  };

  switch (instruction) {
    case RamsesInstruction.Str:
      memoryWriteByte(cpu.memory, ir.operandAddress, readRegister());
      break;
    case RamsesInstruction.Ldr:
      writeRegister(readOperand());
      break;
    case RamsesInstruction.Add:
      writeRegisterAndCarry(readRegister() + readOperand(), (full) => full > 0xff);
      break;
    case RamsesInstruction.Or:
      writeRegister(readRegister() | readOperand());
      break;
    case RamsesInstruction.And:
      writeRegister(readRegister() & readOperand());
      break;
    case RamsesInstruction.Not:
      writeRegister(~readRegister());
      break;
    case RamsesInstruction.Sub:
      writeRegisterAndCarry(readRegister() + 0x100 - readOperand(), (full) => full > 0xff);
      flags.carry = !flags.carry;
      break;
    case RamsesInstruction.Jmp:
      jumpIf(true);
      break;
    case RamsesInstruction.Jn:
      jumpIf(flags.negative);
      break;
    case RamsesInstruction.Jz:
      jumpIf(flags.zero);
      break;
    case RamsesInstruction.Jc:
      jumpIf(flags.carry);
      break;
    case RamsesInstruction.Jsr:
      memoryWriteByte(cpu.memory, ir.operandAddress, r.programCounter);
      r.programCounter = (ir.operandAddress + 1) & 0xff;
      break;
    case RamsesInstruction.Neg: {
      const oldRegister = readRegister();
      writeRegisterAndCarry(((~oldRegister) + 1) & 0xff, () => oldRegister === 0);
      break;
    }
    case RamsesInstruction.Shr: {
      const oldRegister = readRegister();
      writeRegisterAndCarry(oldRegister >>> 1, () => (oldRegister & 1) !== 0);
      break;
    }
    default:
      // Hlt, Nop, or unknown: nothing else to do.
      break;
  }

  flags.halted = instruction === RamsesInstruction.Hlt;
}

export function ramsesStep(cpu: RamsesCpu): void {
  ramsesFetch(cpu);
  ramsesExecute(cpu);
}

export function ramsesReset(cpu: RamsesCpu): void {
  ramsesRegistersReset(cpu.registers);
  memoryReset(cpu.memory);
}

interface DisassembledInstruction {
  text: string;
  size: number;
}

export function ramsesDisassembleInstruction(content: ArrayLike<number>): DisassembledInstruction {
  if (content.length === 0) return { text: '', size: 0 };
  const first = content[0] ?? 0;
  const next = content.length > 1 ? (content[1] ?? 0) : 0;

  const registerSuffix = (): string => {
    switch (first & RamsesRegisterMask) {
      case RamsesRegister.Ra:
        return ' A';
      case RamsesRegister.Rb:
        return ' B';
      case RamsesRegister.Rx:
        return ' X';
      case RamsesRegister.Pc:
        return ' PC';
      default:
        return ' ?';
    }
  };

  const operandSuffix = (): string => {
    switch (first & RamsesAddressModeMask) {
      case RamsesAddressMode.Direct:
        return ` ${next}`;
      case RamsesAddressMode.Indirect:
        return ` ${next},I`;
      case RamsesAddressMode.Immediate:
        return ` #${next}`;
      case RamsesAddressMode.Indexed:
        return ` ${next},X`;
      default:
        return ` ${next},?`;
    }
  };

  switch (first & RamsesInstructionMask) {
    case RamsesInstruction.Str:
      return { text: 'STR' + registerSuffix() + operandSuffix(), size: 2 };
    case RamsesInstruction.Ldr:
      return { text: 'LDR' + registerSuffix() + operandSuffix(), size: 2 };
    case RamsesInstruction.Add:
      return { text: 'ADD' + registerSuffix() + operandSuffix(), size: 2 };
    case RamsesInstruction.Or:
      return { text: 'OR ' + registerSuffix() + operandSuffix(), size: 2 };
    case RamsesInstruction.And:
      return { text: 'AND' + registerSuffix() + operandSuffix(), size: 2 };
    case RamsesInstruction.Not:
      return { text: 'NOT' + registerSuffix(), size: 1 };
    case RamsesInstruction.Sub:
      return { text: 'SUB' + registerSuffix() + operandSuffix(), size: 2 };
    case RamsesInstruction.Jmp:
      return { text: 'JMP' + operandSuffix(), size: 2 };
    case RamsesInstruction.Jn:
      return { text: 'JN ' + operandSuffix(), size: 2 };
    case RamsesInstruction.Jz:
      return { text: 'JZ ' + operandSuffix(), size: 2 };
    case RamsesInstruction.Jc:
      return { text: 'JC ' + operandSuffix(), size: 2 };
    case RamsesInstruction.Jsr:
      return { text: 'JSR' + operandSuffix(), size: 2 };
    case RamsesInstruction.Neg:
      return { text: 'NEG' + registerSuffix(), size: 1 };
    case RamsesInstruction.Shr:
      return { text: 'SHR' + registerSuffix(), size: 1 };
    case RamsesInstruction.Hlt:
      return { text: 'HLT', size: 1 };
    default:
      return { text: 'NOP', size: 1 };
  }
}

export function ramsesDisassembleInstructions(
  content: ArrayLike<number>,
): DisassembledInstruction[] {
  const out: DisassembledInstruction[] = [];
  let offset = 0;
  while (offset < content.length) {
    const slice: number[] = [];
    for (let i = offset; i < content.length; i++) slice.push(content[i] ?? 0);
    const { text, size } = ramsesDisassembleInstruction(slice);
    if (size === 0) break;
    out.push({ text, size });
    offset += size;
  }
  return out;
}
