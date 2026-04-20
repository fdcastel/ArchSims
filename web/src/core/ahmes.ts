import {
  createMemory,
  memoryReadByte,
  memoryReset,
  memoryWriteByte,
  type Memory,
} from './memory';

export const AhmesInstruction = {
  Nop: 0x00,
  Sta: 0x10,
  Lda: 0x20,
  Add: 0x30,
  Or: 0x40,
  And: 0x50,
  Not: 0x60,
  Sub: 0x70,
  Jmp: 0x80,
  Jn: 0x90,
  Jp: 0x94,
  Jv: 0x98,
  Jnv: 0x9c,
  Jz: 0xa0,
  Jnz: 0xa4,
  Jc: 0xb0,
  Jnc: 0xb4,
  Jb: 0xb8,
  Jnb: 0xbc,
  Shr: 0xe0,
  Shl: 0xe1,
  Ror: 0xe2,
  Rol: 0xe3,
  Hlt: 0xf0,
} as const;

export type AhmesInstruction = (typeof AhmesInstruction)[keyof typeof AhmesInstruction];

export interface AhmesInstructionRegister {
  opCode: number;
  operandAddress: number;
}

export interface AhmesFlags {
  halted: boolean;
  negative: boolean;
  zero: boolean;
  carry: boolean;
  overflow: boolean;
  borrow: boolean;
}

export interface AhmesRegisters {
  programCounter: number;
  accumulator: number;
  instructionRegister: AhmesInstructionRegister;
  flags: AhmesFlags;
}

export interface AhmesCpu {
  registers: AhmesRegisters;
  memory: Memory;
}

export function createAhmesRegisters(): AhmesRegisters {
  return {
    programCounter: 0,
    accumulator: 0,
    instructionRegister: { opCode: 0, operandAddress: 0 },
    flags: {
      halted: false,
      negative: false,
      zero: true,
      carry: false,
      overflow: false,
      borrow: false,
    },
  };
}

export function ahmesRegistersReset(registers: AhmesRegisters): void {
  registers.programCounter = 0;
  registers.accumulator = 0;
  registers.instructionRegister.opCode = 0;
  registers.instructionRegister.operandAddress = 0;
  registers.flags.halted = false;
  registers.flags.negative = false;
  registers.flags.zero = true;
  registers.flags.carry = false;
  registers.flags.overflow = false;
  registers.flags.borrow = false;
}

export function createAhmesCpu(): AhmesCpu {
  return { registers: createAhmesRegisters(), memory: createMemory(256) };
}

function instructionTakesOperand(opCode: number): boolean {
  switch (opCode) {
    case AhmesInstruction.Sta:
    case AhmesInstruction.Lda:
    case AhmesInstruction.Add:
    case AhmesInstruction.Or:
    case AhmesInstruction.And:
    case AhmesInstruction.Sub:
    case AhmesInstruction.Jmp:
    case AhmesInstruction.Jn:
    case AhmesInstruction.Jp:
    case AhmesInstruction.Jv:
    case AhmesInstruction.Jnv:
    case AhmesInstruction.Jz:
    case AhmesInstruction.Jnz:
    case AhmesInstruction.Jc:
    case AhmesInstruction.Jnc:
    case AhmesInstruction.Jb:
    case AhmesInstruction.Jnb:
      return true;
    default:
      return false;
  }
}

export function ahmesFetch(cpu: AhmesCpu): void {
  const advance = (): number => {
    const value = memoryReadByte(cpu.memory, cpu.registers.programCounter);
    cpu.registers.programCounter = (cpu.registers.programCounter + 1) & 0xff;
    return value;
  };
  const ir = cpu.registers.instructionRegister;
  ir.opCode = advance();
  ir.operandAddress = instructionTakesOperand(ir.opCode) ? advance() : 0;
}

const isNegativeByte = (value: number): boolean => (value & 0xff) > 0x7f;

export function ahmesExecute(cpu: AhmesCpu): void {
  const r = cpu.registers;
  const ir = r.instructionRegister;
  const op = ir.opCode;
  const flags = r.flags;
  const readOperand = (): number => memoryReadByte(cpu.memory, ir.operandAddress);

  const writeAc = (value: number): void => {
    const v = value & 0xff;
    r.accumulator = v;
    flags.zero = v === 0;
    flags.negative = v > 0x7f;
  };

  const addAndUpdateFlags = (a: number, b: number): number => {
    const full = (a & 0xff) + (b & 0xff);
    flags.carry = full > 0xff;
    const result = full & 0xff;
    const sa = isNegativeByte(a);
    const sb = isNegativeByte(b);
    const sr = isNegativeByte(result);
    flags.overflow = sa === sb && sa !== sr;
    return result;
  };

  const subAndUpdateFlags = (a: number, b: number): number => {
    const full = (a & 0xff) - (b & 0xff);
    flags.borrow = full < 0;
    const result = full & 0xff;
    const sa = isNegativeByte(a);
    const sb = isNegativeByte(b);
    const sr = isNegativeByte(result);
    flags.overflow = sa !== sb && sa !== sr;
    return result;
  };

  const jumpIf = (cond: boolean): void => {
    if (cond) r.programCounter = ir.operandAddress;
  };

  switch (op) {
    case AhmesInstruction.Sta:
      memoryWriteByte(cpu.memory, ir.operandAddress, r.accumulator);
      break;
    case AhmesInstruction.Lda:
      writeAc(readOperand());
      break;
    case AhmesInstruction.Add:
      writeAc(addAndUpdateFlags(r.accumulator, readOperand()));
      break;
    case AhmesInstruction.Or:
      writeAc(r.accumulator | readOperand());
      break;
    case AhmesInstruction.And:
      writeAc(r.accumulator & readOperand());
      break;
    case AhmesInstruction.Not:
      writeAc(~r.accumulator);
      break;
    case AhmesInstruction.Sub:
      writeAc(subAndUpdateFlags(r.accumulator, readOperand()));
      break;
    case AhmesInstruction.Jmp:
      jumpIf(true);
      break;
    case AhmesInstruction.Jn:
      jumpIf(flags.negative);
      break;
    case AhmesInstruction.Jp:
      jumpIf(!flags.negative);
      break;
    case AhmesInstruction.Jv:
      jumpIf(flags.overflow);
      break;
    case AhmesInstruction.Jnv:
      jumpIf(!flags.overflow);
      break;
    case AhmesInstruction.Jz:
      jumpIf(flags.zero);
      break;
    case AhmesInstruction.Jnz:
      jumpIf(!flags.zero);
      break;
    case AhmesInstruction.Jc:
      jumpIf(flags.carry);
      break;
    case AhmesInstruction.Jnc:
      jumpIf(!flags.carry);
      break;
    case AhmesInstruction.Jb:
      jumpIf(flags.borrow);
      break;
    case AhmesInstruction.Jnb:
      jumpIf(!flags.borrow);
      break;
    case AhmesInstruction.Shr: {
      const ac = r.accumulator;
      flags.carry = (ac & 0x01) !== 0;
      writeAc(ac >>> 1);
      break;
    }
    case AhmesInstruction.Shl: {
      const ac = r.accumulator;
      flags.carry = (ac & 0x80) !== 0;
      writeAc(ac << 1);
      break;
    }
    case AhmesInstruction.Ror: {
      const ac = r.accumulator;
      const newHighBit = flags.carry ? 0x80 : 0x00;
      flags.carry = (ac & 0x01) !== 0;
      writeAc((ac >>> 1) | newHighBit);
      break;
    }
    case AhmesInstruction.Rol: {
      const ac = r.accumulator;
      const newLowBit = flags.carry ? 0x01 : 0x00;
      flags.carry = (ac & 0x80) !== 0;
      writeAc((ac << 1) | newLowBit);
      break;
    }
    default:
      // Hlt, Nop, or unknown opcode: nothing else to do.
      break;
  }

  flags.halted = op === AhmesInstruction.Hlt;
}

export function ahmesStep(cpu: AhmesCpu): void {
  ahmesFetch(cpu);
  ahmesExecute(cpu);
}

export function ahmesReset(cpu: AhmesCpu): void {
  ahmesRegistersReset(cpu.registers);
  memoryReset(cpu.memory);
}
