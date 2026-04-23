import {
  createMemory,
  memoryReadByte,
  memoryReset,
  memoryWriteByte,
  type Memory,
} from './memory';

export const NeanderInstruction = {
  Nop: 0x00,
  Sta: 0x10,
  Lda: 0x20,
  Add: 0x30,
  Or: 0x40,
  And: 0x50,
  Not: 0x60,
  Jmp: 0x80,
  Jn: 0x90,
  Jz: 0xa0,
  Hlt: 0xf0,
} as const;

export type NeanderInstruction = (typeof NeanderInstruction)[keyof typeof NeanderInstruction];

export interface NeanderInstructionRegister {
  opCode: number;
  operandAddress: number;
}

export interface NeanderFlags {
  halted: boolean;
  negative: boolean;
  zero: boolean;
}

export interface NeanderRegisters {
  programCounter: number;
  accumulator: number;
  instructionRegister: NeanderInstructionRegister;
  flags: NeanderFlags;
}

export interface NeanderCpu {
  registers: NeanderRegisters;
  memory: Memory;
}

export function createNeanderRegisters(): NeanderRegisters {
  return {
    programCounter: 0,
    accumulator: 0,
    instructionRegister: { opCode: 0, operandAddress: 0 },
    flags: { halted: false, negative: false, zero: true },
  };
}

export function neanderRegistersReset(registers: NeanderRegisters): void {
  registers.programCounter = 0;
  registers.accumulator = 0;
  registers.instructionRegister.opCode = 0;
  registers.instructionRegister.operandAddress = 0;
  registers.flags.halted = false;
  registers.flags.negative = false;
  registers.flags.zero = true;
}

export function createNeanderCpu(): NeanderCpu {
  return { registers: createNeanderRegisters(), memory: createMemory(256) };
}

function instructionTakesOperand(opCode: number): boolean {
  switch (opCode) {
    case NeanderInstruction.Sta:
    case NeanderInstruction.Lda:
    case NeanderInstruction.Add:
    case NeanderInstruction.Or:
    case NeanderInstruction.And:
    case NeanderInstruction.Jmp:
    case NeanderInstruction.Jn:
    case NeanderInstruction.Jz:
      return true;
    default:
      return false;
  }
}

export function neanderFetch(cpu: NeanderCpu): void {
  const advance = (): number => {
    const value = memoryReadByte(cpu.memory, cpu.registers.programCounter);
    cpu.registers.programCounter = (cpu.registers.programCounter + 1) & 0xff;
    return value;
  };
  const ir = cpu.registers.instructionRegister;
  ir.opCode = advance();
  ir.operandAddress = instructionTakesOperand(ir.opCode) ? advance() : 0;
}

export function neanderExecute(cpu: NeanderCpu): void {
  const r = cpu.registers;
  const ir = r.instructionRegister;
  const op = ir.opCode;
  const readOperand = (): number => memoryReadByte(cpu.memory, ir.operandAddress);
  const writeAc = (value: number): void => {
    const v = value & 0xff;
    r.accumulator = v;
    r.flags.zero = v === 0;
    r.flags.negative = v > 0x7f;
  };
  const jumpIf = (cond: boolean): void => {
    if (cond) r.programCounter = ir.operandAddress;
  };

  switch (op) {
    case NeanderInstruction.Sta:
      memoryWriteByte(cpu.memory, ir.operandAddress, r.accumulator);
      break;
    case NeanderInstruction.Lda:
      writeAc(readOperand());
      break;
    case NeanderInstruction.Add:
      writeAc(r.accumulator + readOperand());
      break;
    case NeanderInstruction.Or:
      writeAc(r.accumulator | readOperand());
      break;
    case NeanderInstruction.And:
      writeAc(r.accumulator & readOperand());
      break;
    case NeanderInstruction.Not:
      writeAc(~r.accumulator);
      break;
    case NeanderInstruction.Jmp:
      jumpIf(true);
      break;
    case NeanderInstruction.Jn:
      jumpIf(r.flags.negative);
      break;
    case NeanderInstruction.Jz:
      jumpIf(r.flags.zero);
      break;
    default:
      // Hlt, Nop, or unknown opcode: nothing else to do.
      break;
  }

  r.flags.halted = op === NeanderInstruction.Hlt;
}

export function neanderStep(cpu: NeanderCpu): void {
  neanderFetch(cpu);
  neanderExecute(cpu);
}

export function neanderReset(cpu: NeanderCpu): void {
  neanderRegistersReset(cpu.registers);
  memoryReset(cpu.memory);
}

interface DisassembledInstruction {
  text: string;
  size: number;
  mnemonic: string;
}

const NEANDER_MNEMONICS: Record<number, string> = {
  [NeanderInstruction.Nop]: 'NOP',
  [NeanderInstruction.Sta]: 'STA',
  [NeanderInstruction.Lda]: 'LDA',
  [NeanderInstruction.Add]: 'ADD',
  [NeanderInstruction.Or]: 'OR ',
  [NeanderInstruction.And]: 'AND',
  [NeanderInstruction.Not]: 'NOT',
  [NeanderInstruction.Jmp]: 'JMP',
  [NeanderInstruction.Jn]: 'JN ',
  [NeanderInstruction.Jz]: 'JZ ',
  [NeanderInstruction.Hlt]: 'HLT',
};

export function neanderDisassembleInstruction(
  content: ArrayLike<number>,
): DisassembledInstruction {
  if (content.length === 0) return { text: '', size: 0, mnemonic: '' };
  const op = content[0] ?? 0;
  const mnemonic = NEANDER_MNEMONICS[op] ?? 'NOP';
  if (instructionTakesOperand(op)) {
    if (content.length < 2) return { text: '', size: 0, mnemonic };
    const operand = content[1] ?? 0;
    return { text: `${mnemonic.trimEnd()} ${operand}`, size: 2, mnemonic };
  }
  return { text: mnemonic.trimEnd(), size: 1, mnemonic };
}

export function neanderDisassembleInstructions(
  content: ArrayLike<number>,
): DisassembledInstruction[] {
  const out: DisassembledInstruction[] = [];
  let offset = 0;
  while (offset < content.length) {
    const slice: number[] = [];
    for (let i = offset; i < content.length; i++) slice.push(content[i] ?? 0);
    const { text, size, mnemonic } = neanderDisassembleInstruction(slice);
    if (size === 0) break;
    out.push({ text, size, mnemonic });
    offset += size;
  }
  return out;
}
