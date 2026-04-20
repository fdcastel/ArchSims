import {
  RamsesAddressMode,
  RamsesInstruction,
  RamsesRegister,
} from '@/core/ramses';

export interface AssembledProgram {
  bytes: Uint8Array;
  addrToLine: Map<number, number>;
  labels: Map<string, number>;
  instrAddrs: number[];
}

interface Operand {
  mode: number;
  literal: number | null;
  label: string | null;
}

const REGISTERS: Record<string, number> = {
  A: RamsesRegister.Ra,
  B: RamsesRegister.Rb,
  X: RamsesRegister.Rx,
  PC: RamsesRegister.Pc,
};

const SIMPLE_INSTRUCTIONS: Record<string, number> = {
  NOP: RamsesInstruction.Nop,
  HLT: RamsesInstruction.Hlt,
};

const REGISTER_ONLY_INSTRUCTIONS: Record<string, number> = {
  NOT: RamsesInstruction.Not,
  NEG: RamsesInstruction.Neg,
  SHR: RamsesInstruction.Shr,
};

const OPERAND_ONLY_INSTRUCTIONS: Record<string, number> = {
  JMP: RamsesInstruction.Jmp,
  JN: RamsesInstruction.Jn,
  JZ: RamsesInstruction.Jz,
  JC: RamsesInstruction.Jc,
  JSR: RamsesInstruction.Jsr,
};

const REGISTER_AND_OPERAND_INSTRUCTIONS: Record<string, number> = {
  STR: RamsesInstruction.Str,
  LDR: RamsesInstruction.Ldr,
  ADD: RamsesInstruction.Add,
  OR: RamsesInstruction.Or,
  AND: RamsesInstruction.And,
  SUB: RamsesInstruction.Sub,
};

interface ScannerState {
  pos: number;
  line: number;
}

class Scanner {
  pos = 0;
  line = 1;
  constructor(public src: string) {}

  isAtEnd(): boolean {
    return this.pos >= this.src.length;
  }

  peek(): string {
    return this.src[this.pos] ?? '';
  }

  advance(): string {
    const c = this.src[this.pos] ?? '';
    this.pos++;
    if (c === '\n') this.line++;
    return c;
  }

  save(): ScannerState {
    return { pos: this.pos, line: this.line };
  }

  restore(state: ScannerState): void {
    this.pos = state.pos;
    this.line = state.line;
  }

  skipSpacesAndComments(): void {
    while (!this.isAtEnd()) {
      const c = this.peek();
      if (c === ' ' || c === '\t' || c === '\r' || c === '\n') {
        this.advance();
      } else if (c === ';') {
        while (!this.isAtEnd() && this.peek() !== '\n') this.advance();
      } else {
        break;
      }
    }
  }

  matchIdent(): string | null {
    const start = this.pos;
    if (start >= this.src.length) return null;
    const first = this.src[start] ?? '';
    if (!isLetter(first)) return null;
    let end = start + 1;
    while (end < this.src.length && isLetterOrDigit(this.src[end] ?? '')) end++;
    const ident = this.src.substring(start, end);
    for (let i = 0; i < ident.length; i++) this.advance();
    return ident;
  }

  matchUint8(): number | null {
    const start = this.pos;
    let end = start;
    while (end < this.src.length && isDigit(this.src[end] ?? '')) end++;
    if (end === start) return null;
    const numStr = this.src.substring(start, end);
    const value = Number.parseInt(numStr, 10);
    if (!Number.isFinite(value) || value < 0 || value > 255) {
      throw new Error(`Linha ${this.line}: valor fora da faixa 0-255: ${numStr}`);
    }
    for (let i = 0; i < numStr.length; i++) this.advance();
    return value;
  }
}

const isLetter = (c: string): boolean => /[A-Za-z]/.test(c);
const isDigit = (c: string): boolean => c >= '0' && c <= '9';
const isLetterOrDigit = (c: string): boolean => isLetter(c) || isDigit(c);

function parseRegister(s: Scanner): number {
  s.skipSpacesAndComments();
  const ident = s.matchIdent();
  if (ident === null) {
    throw new Error(`Linha ${s.line}: registrador esperado`);
  }
  const upper = ident.toUpperCase();
  const reg = REGISTERS[upper];
  if (reg === undefined) {
    throw new Error(`Linha ${s.line}: registrador desconhecido: ${ident}`);
  }
  return reg;
}

function parseOperand(s: Scanner): Operand {
  s.skipSpacesAndComments();
  let immediate = false;
  if (s.peek() === '#') {
    s.advance();
    immediate = true;
  }

  let label: string | null = null;
  let literal: number | null = null;
  if (s.peek() === ':') {
    s.advance();
    label = s.matchIdent();
    if (label === null) {
      throw new Error(`Linha ${s.line}: identificador esperado após ':'`);
    }
  } else {
    literal = s.matchUint8();
    if (literal === null) {
      throw new Error(`Linha ${s.line}: operando esperado`);
    }
  }

  if (immediate) {
    return { mode: RamsesAddressMode.Immediate, literal, label };
  }

  const save = s.save();
  if (s.peek() === ',') {
    s.advance();
    const suffix = s.peek().toUpperCase();
    if (suffix === 'X') {
      s.advance();
      return { mode: RamsesAddressMode.Indexed, literal, label };
    }
    if (suffix === 'I') {
      s.advance();
      return { mode: RamsesAddressMode.Indirect, literal, label };
    }
    s.restore(save);
  }

  return { mode: RamsesAddressMode.Direct, literal, label };
}

interface InstructionResult {
  bytes: number[];
  references: Array<{ label: string; offsetInBytes: number }>;
}

function assembleInstructionFromScanner(
  s: Scanner,
  ident: string,
): InstructionResult | null {
  const upper = ident.toUpperCase();

  if (upper in REGISTER_AND_OPERAND_INSTRUCTIONS) {
    const inst = REGISTER_AND_OPERAND_INSTRUCTIONS[upper]!;
    const reg = parseRegister(s);
    const op = parseOperand(s);
    const opcode = inst | reg | op.mode;
    const operandByte = op.literal ?? 0;
    const refs = op.label !== null ? [{ label: op.label, offsetInBytes: 1 }] : [];
    return { bytes: [opcode, operandByte], references: refs };
  }

  if (upper in OPERAND_ONLY_INSTRUCTIONS) {
    const inst = OPERAND_ONLY_INSTRUCTIONS[upper]!;
    const op = parseOperand(s);
    const opcode = inst | op.mode;
    const operandByte = op.literal ?? 0;
    const refs = op.label !== null ? [{ label: op.label, offsetInBytes: 1 }] : [];
    return { bytes: [opcode, operandByte], references: refs };
  }

  if (upper in REGISTER_ONLY_INSTRUCTIONS) {
    const inst = REGISTER_ONLY_INSTRUCTIONS[upper]!;
    const reg = parseRegister(s);
    return { bytes: [inst | reg], references: [] };
  }

  if (upper in SIMPLE_INSTRUCTIONS) {
    return { bytes: [SIMPLE_INSTRUCTIONS[upper]!], references: [] };
  }

  return null;
}

export function assembleInstruction(input: string): number[] {
  const s = new Scanner(input);
  s.skipSpacesAndComments();
  const ident = s.matchIdent();
  if (ident === null) {
    throw new Error(`Linha ${s.line}: instrução esperada`);
  }
  const result = assembleInstructionFromScanner(s, ident);
  if (result === null) {
    throw new Error(`Linha ${s.line}: instrução desconhecida: ${ident}`);
  }
  return result.bytes;
}

export function assembleProgram(input: string): AssembledProgram {
  const bytes = new Uint8Array(256);
  const addrToLine = new Map<number, number>();
  const labels = new Map<string, number>();
  const deferred = new Map<string, number[]>();
  const instrAddrs: number[] = [];

  const s = new Scanner(input);
  let address = 0;

  const writeByte = (line: number, value: number): void => {
    bytes[address] = value & 0xff;
    addrToLine.set(address, line);
    address = (address + 1) & 0xff;
  };

  const recordLabelRef = (label: string, fixupAddr: number): number => {
    const known = labels.get(label);
    if (known !== undefined) return known;
    const list = deferred.get(label) ?? [];
    list.push(fixupAddr);
    deferred.set(label, list);
    return 0;
  };

  const defineLabel = (name: string): void => {
    if (labels.has(name)) {
      throw new Error(`Label duplicado: ${name}`);
    }
    const value = address & 0xff;
    labels.set(name, value);
    const fixups = deferred.get(name);
    if (fixups) {
      for (const a of fixups) bytes[a] = value;
      deferred.delete(name);
    }
  };

  while (true) {
    s.skipSpacesAndComments();
    if (s.isAtEnd()) break;

    const line = s.line;
    const c = s.peek();

    if (c === '@') {
      s.advance();
      const a = s.matchUint8();
      if (a === null) {
        throw new Error(`Linha ${line}: endereço esperado após '@'`);
      }
      address = a;
      continue;
    }

    if (c === ':') {
      s.advance();
      const name = s.matchIdent();
      if (name === null) {
        throw new Error(`Linha ${line}: identificador esperado após ':'`);
      }
      defineLabel(name);
      continue;
    }

    if (isLetter(c)) {
      const ident = s.matchIdent();
      if (ident === null) {
        throw new Error(`Linha ${line}: identificador esperado`);
      }
      const result = assembleInstructionFromScanner(s, ident);
      if (result === null) {
        throw new Error(`Linha ${line}: instrução desconhecida: ${ident}`);
      }
      instrAddrs.push(address);
      const startAddr = address;
      for (const b of result.bytes) writeByte(line, b);
      for (const { label, offsetInBytes } of result.references) {
        const target = (startAddr + offsetInBytes) & 0xff;
        bytes[target] = recordLabelRef(label, target);
      }
      continue;
    }

    if (isDigit(c)) {
      const value = s.matchUint8();
      if (value === null) {
        throw new Error(`Linha ${line}: número esperado`);
      }
      writeByte(line, value);
      continue;
    }

    throw new Error(`Linha ${line}: caractere inesperado '${c}'`);
  }

  if (deferred.size > 0) {
    const names = Array.from(deferred.keys());
    if (names.length === 1) {
      throw new Error(`Label indefinido: ${names[0]}`);
    }
    throw new Error(`Labels indefinidos: ${names.join(', ')}`);
  }

  return { bytes, addrToLine, labels, instrAddrs };
}
