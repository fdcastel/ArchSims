import {
  CesarAddressMode,
  CesarFlagBit,
  CesarInstruction,
  encodeCesarInstructionOneOperand,
  encodeCesarInstructionTwoOperand,
} from '@/core/cesar';

export interface AssembledProgram {
  bytes: Uint8Array;
  addrToLine: Map<number, number>;
  labels: Map<string, number>;
  instrAddrs: number[];
}

type Operand =
  | { kind: 'reg'; register: number; mode: number }
  | { kind: 'regIndex'; register: number; mode: number; index: number }
  | { kind: 'direct'; address: number }
  | { kind: 'immediate'; value: number }
  | { kind: 'labelRef'; label: string };

const SIMPLE: Record<string, number> = {
  NOP: CesarInstruction.Nop,
  HLT: CesarInstruction.Hlt,
};

const CHANGE_FLAGS: Record<string, number> = {
  CCC: CesarInstruction.Ccc,
  SCC: CesarInstruction.Scc,
};

const BRANCH_ONLY: Record<string, number> = {
  BR: CesarInstruction.Br,
  BNE: CesarInstruction.Bne,
  BEQ: CesarInstruction.Beq,
  BPL: CesarInstruction.Bpl,
  BMI: CesarInstruction.Bmi,
  BVC: CesarInstruction.Bvc,
  BVS: CesarInstruction.Bvs,
  BCC: CesarInstruction.Bcc,
  BCS: CesarInstruction.Bcs,
  BGE: CesarInstruction.Bge,
  BLT: CesarInstruction.Blt,
  BGT: CesarInstruction.Bgt,
  BLE: CesarInstruction.Ble,
  BHI: CesarInstruction.Bhi,
  BLS: CesarInstruction.Bls,
};

const ONE_OPERAND: Record<string, number> = {
  CLR: CesarInstruction.Clr,
  NOT: CesarInstruction.Not,
  INC: CesarInstruction.Inc,
  DEC: CesarInstruction.Dec,
  NEG: CesarInstruction.Neg,
  TST: CesarInstruction.Tst,
  ROR: CesarInstruction.Ror,
  ROL: CesarInstruction.Rol,
  ASR: CesarInstruction.Asr,
  ASL: CesarInstruction.Asl,
  ADC: CesarInstruction.Adc,
  SBC: CesarInstruction.Sbc,
};

const TWO_OPERANDS: Record<string, number> = {
  MOV: CesarInstruction.Mov,
  ADD: CesarInstruction.Add,
  SUB: CesarInstruction.Sub,
  CMP: CesarInstruction.Cmp,
  AND: CesarInstruction.And,
  OR: CesarInstruction.Or,
};

const isLetter = (c: string): boolean => /[A-Za-z]/.test(c);
const isDigit = (c: string): boolean => c >= '0' && c <= '9';
const isLetterOrDigit = (c: string): boolean => isLetter(c) || isDigit(c);

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

  matchSignedInt(): number | null {
    const save = this.save();
    let sign = 1;
    if (this.peek() === '-') {
      sign = -1;
      this.advance();
    } else if (this.peek() === '+') {
      this.advance();
    }
    const start = this.pos;
    while (!this.isAtEnd() && isDigit(this.peek())) this.advance();
    if (this.pos === start) {
      this.restore(save);
      return null;
    }
    const numStr = this.src.substring(start, this.pos);
    return sign * Number.parseInt(numStr, 10);
  }
}

function tryParseRegister(s: Scanner): number | null {
  const save = s.save();
  const c = s.peek();
  if (c !== 'R' && c !== 'r') return null;
  s.advance();
  const d = s.peek();
  if (d < '0' || d > '7') {
    s.restore(save);
    return null;
  }
  s.advance();
  return Number.parseInt(d, 10);
}

function tryPattern<T>(s: Scanner, fn: () => T | null): T | null {
  const save = s.save();
  const result = fn();
  if (result === null) s.restore(save);
  return result;
}

function parseOperand(s: Scanner): Operand {
  s.skipSpacesAndComments();

  // 1. :LABEL
  if (s.peek() === ':') {
    const label = tryPattern(s, () => {
      s.advance();
      return s.matchIdent();
    });
    if (label !== null) {
      return { kind: 'labelRef', label };
    }
  }

  // 2. ((R)+) — RegPostIncIndirect
  const p2 = tryPattern<Operand>(s, () => {
    if (s.peek() !== '(') return null;
    s.advance();
    if (s.peek() !== '(') return null;
    s.advance();
    const r = tryParseRegister(s);
    if (r === null) return null;
    if (s.peek() !== ')') return null;
    s.advance();
    if (s.peek() !== '+') return null;
    s.advance();
    if (s.peek() !== ')') return null;
    s.advance();
    return { kind: 'reg', register: r, mode: CesarAddressMode.RegPostIncIndirect };
  });
  if (p2 !== null) return p2;

  // 3. (-(R)) — RegPreDecIndirect
  const p3 = tryPattern<Operand>(s, () => {
    if (s.peek() !== '(') return null;
    s.advance();
    if (s.peek() !== '-') return null;
    s.advance();
    if (s.peek() !== '(') return null;
    s.advance();
    const r = tryParseRegister(s);
    if (r === null) return null;
    if (s.peek() !== ')') return null;
    s.advance();
    if (s.peek() !== ')') return null;
    s.advance();
    return { kind: 'reg', register: r, mode: CesarAddressMode.RegPreDecIndirect };
  });
  if (p3 !== null) return p3;

  // 4. (N(R)) — IndexedIndirect
  const p4 = tryPattern<Operand>(s, () => {
    if (s.peek() !== '(') return null;
    s.advance();
    const n = s.matchSignedInt();
    if (n === null) return null;
    if (s.peek() !== '(') return null;
    s.advance();
    const r = tryParseRegister(s);
    if (r === null) return null;
    if (s.peek() !== ')') return null;
    s.advance();
    if (s.peek() !== ')') return null;
    s.advance();
    return {
      kind: 'regIndex',
      register: r,
      mode: CesarAddressMode.IndexedIndirect,
      index: n,
    };
  });
  if (p4 !== null) return p4;

  // 5. (R)+ — RegPostInc
  const p5 = tryPattern<Operand>(s, () => {
    if (s.peek() !== '(') return null;
    s.advance();
    const r = tryParseRegister(s);
    if (r === null) return null;
    if (s.peek() !== ')') return null;
    s.advance();
    if (s.peek() !== '+') return null;
    s.advance();
    return { kind: 'reg', register: r, mode: CesarAddressMode.RegPostInc };
  });
  if (p5 !== null) return p5;

  // 6. -(R) — RegPreDec
  const p6 = tryPattern<Operand>(s, () => {
    if (s.peek() !== '-') return null;
    s.advance();
    if (s.peek() !== '(') return null;
    s.advance();
    const r = tryParseRegister(s);
    if (r === null) return null;
    if (s.peek() !== ')') return null;
    s.advance();
    return { kind: 'reg', register: r, mode: CesarAddressMode.RegPreDec };
  });
  if (p6 !== null) return p6;

  // 7. N(R) — Indexed
  const p7 = tryPattern<Operand>(s, () => {
    const n = s.matchSignedInt();
    if (n === null) return null;
    if (s.peek() !== '(') return null;
    s.advance();
    const r = tryParseRegister(s);
    if (r === null) return null;
    if (s.peek() !== ')') return null;
    s.advance();
    return { kind: 'regIndex', register: r, mode: CesarAddressMode.Indexed, index: n };
  });
  if (p7 !== null) return p7;

  // 8. (R) — RegisterIndirect
  const p8 = tryPattern<Operand>(s, () => {
    if (s.peek() !== '(') return null;
    s.advance();
    const r = tryParseRegister(s);
    if (r === null) return null;
    if (s.peek() !== ')') return null;
    s.advance();
    return { kind: 'reg', register: r, mode: CesarAddressMode.RegisterIndirect };
  });
  if (p8 !== null) return p8;

  // 9. R — Register
  const p9 = tryParseRegister(s);
  if (p9 !== null) {
    return { kind: 'reg', register: p9, mode: CesarAddressMode.Register };
  }

  // 10. #N — Immediate
  if (s.peek() === '#') {
    const value = tryPattern(s, () => {
      s.advance();
      return s.matchSignedInt();
    });
    if (value !== null) {
      return { kind: 'immediate', value };
    }
  }

  // 11. N — Direct
  const direct = s.matchSignedInt();
  if (direct !== null) {
    return { kind: 'direct', address: direct };
  }

  throw new Error(`Linha ${s.line}: operando esperado`);
}

interface BranchOperand {
  literal: number | null;
  label: string | null;
}

function parseBranchOperand(s: Scanner): BranchOperand {
  s.skipSpacesAndComments();
  if (s.peek() === ':') {
    const save = s.save();
    s.advance();
    const label = s.matchIdent();
    if (label !== null) {
      return { literal: null, label };
    }
    s.restore(save);
  }
  const lit = s.matchSignedInt();
  if (lit === null) {
    throw new Error(`Linha ${s.line}: operando de branch esperado`);
  }
  return { literal: lit, label: null };
}

function parseRegisterToken(s: Scanner): number {
  s.skipSpacesAndComments();
  const r = tryParseRegister(s);
  if (r === null) {
    throw new Error(`Linha ${s.line}: registrador esperado (R0..R7)`);
  }
  return r;
}

function expectComma(s: Scanner): void {
  s.skipSpacesAndComments();
  if (s.peek() !== ',') {
    throw new Error(`Linha ${s.line}: ',' esperada`);
  }
  s.advance();
}

function parseFlagsList(s: Scanner): number {
  s.skipSpacesAndComments();
  let flags = 0;
  while (!s.isAtEnd()) {
    const c = s.peek().toUpperCase();
    if (c === 'N') flags |= CesarFlagBit.Negative;
    else if (c === 'Z') flags |= CesarFlagBit.Zero;
    else if (c === 'V') flags |= CesarFlagBit.Overflow;
    else if (c === 'C') flags |= CesarFlagBit.Carry;
    else break;
    s.advance();
  }
  return flags;
}

interface LabelRef {
  kind: 'word' | 'branch';
  label: string;
  offsetInBytes: number;
  isSob: boolean;
}

interface InstructionResult {
  bytes: number[];
  references: LabelRef[];
}

function pushWord(out: number[], value: number): void {
  out.push((value >>> 8) & 0xff);
  out.push(value & 0xff);
}

interface DecodedOperand {
  register: number;
  mode: number;
  address: number | null;
  label: string | null;
}

function decodeOperand(op: Operand): DecodedOperand {
  switch (op.kind) {
    case 'reg':
      return { register: op.register, mode: op.mode, address: null, label: null };
    case 'regIndex':
      return { register: op.register, mode: op.mode, address: op.index, label: null };
    case 'direct':
      return {
        register: 7,
        mode: CesarAddressMode.RegPostIncIndirect,
        address: op.address,
        label: null,
      };
    case 'immediate':
      return {
        register: 7,
        mode: CesarAddressMode.RegPostInc,
        address: op.value,
        label: null,
      };
    case 'labelRef':
      return {
        register: 7,
        mode: CesarAddressMode.RegPostIncIndirect,
        address: 0,
        label: op.label,
      };
  }
}

function encodeBranch(opcode: number, operand: BranchOperand): InstructionResult {
  if (operand.label !== null) {
    return {
      bytes: [opcode & 0xff, 0],
      references: [{ kind: 'branch', label: operand.label, offsetInBytes: 1, isSob: false }],
    };
  }
  return { bytes: [opcode & 0xff, (operand.literal ?? 0) & 0xff], references: [] };
}

function encodeSob(opcode: number, register: number, operand: BranchOperand): InstructionResult {
  if (operand.label !== null) {
    return {
      bytes: [(opcode | register) & 0xff, 0],
      references: [{ kind: 'branch', label: operand.label, offsetInBytes: 1, isSob: true }],
    };
  }
  return { bytes: [(opcode | register) & 0xff, (operand.literal ?? 0) & 0xff], references: [] };
}

function encodeOneOperandLike(
  baseInstruction: number,
  operand: Operand,
  registerInHighByte = 0,
): InstructionResult {
  const decoded = decodeOperand(operand);
  const word = encodeCesarInstructionOneOperand(baseInstruction, decoded.mode, decoded.register);
  const high = ((word >>> 8) & 0xff) | (registerInHighByte & 0xff);
  const low = word & 0xff;
  const bytes: number[] = [high, low];
  const references: LabelRef[] = [];
  if (decoded.address !== null) {
    pushWord(bytes, decoded.address & 0xffff);
    if (decoded.label !== null) {
      references.push({ kind: 'word', label: decoded.label, offsetInBytes: 2, isSob: false });
    }
  }
  return { bytes, references };
}

function encodeTwoOperands(
  baseInstruction: number,
  source: Operand,
  target: Operand,
): InstructionResult {
  const s = decodeOperand(source);
  const t = decodeOperand(target);
  const word = encodeCesarInstructionTwoOperand(
    baseInstruction,
    s.mode,
    s.register,
    t.mode,
    t.register,
  );
  const bytes: number[] = [(word >>> 8) & 0xff, word & 0xff];
  const references: LabelRef[] = [];
  let cursor = 2;
  if (s.address !== null) {
    pushWord(bytes, s.address & 0xffff);
    if (s.label !== null) {
      references.push({ kind: 'word', label: s.label, offsetInBytes: cursor, isSob: false });
    }
    cursor += 2;
  }
  if (t.address !== null) {
    pushWord(bytes, t.address & 0xffff);
    if (t.label !== null) {
      references.push({ kind: 'word', label: t.label, offsetInBytes: cursor, isSob: false });
    }
  }
  return { bytes, references };
}

function assembleInstructionFromScanner(
  s: Scanner,
  ident: string,
): InstructionResult | null {
  const upper = ident.toUpperCase();

  if (upper in TWO_OPERANDS) {
    const inst = TWO_OPERANDS[upper]!;
    const source = parseOperand(s);
    expectComma(s);
    const target = parseOperand(s);
    return encodeTwoOperands(inst, source, target);
  }

  if (upper in ONE_OPERAND) {
    const inst = ONE_OPERAND[upper]!;
    const op = parseOperand(s);
    return encodeOneOperandLike(inst, op);
  }

  if (upper === 'RTS') {
    const r = parseRegisterToken(s);
    return { bytes: [(CesarInstruction.Rts | r) & 0xff], references: [] };
  }

  if (upper === 'JSR') {
    const r = parseRegisterToken(s);
    expectComma(s);
    const op = parseOperand(s);
    return encodeOneOperandLike(CesarInstruction.Jsr, op, r);
  }

  if (upper === 'SOB') {
    const r = parseRegisterToken(s);
    expectComma(s);
    const branch = parseBranchOperand(s);
    return encodeSob(CesarInstruction.Sob, r, branch);
  }

  if (upper === 'JMP') {
    const op = parseOperand(s);
    return encodeOneOperandLike(CesarInstruction.Jmp, op);
  }

  if (upper in BRANCH_ONLY) {
    const inst = BRANCH_ONLY[upper]!;
    const branch = parseBranchOperand(s);
    return encodeBranch(inst, branch);
  }

  if (upper in CHANGE_FLAGS) {
    const inst = CHANGE_FLAGS[upper]!;
    const flags = parseFlagsList(s);
    return { bytes: [(inst | flags) & 0xff], references: [] };
  }

  if (upper in SIMPLE) {
    return { bytes: [SIMPLE[upper]!], references: [] };
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

const CESAR_MEMORY_SIZE = 65536;

export function assembleProgram(input: string): AssembledProgram {
  const bytes = new Uint8Array(CESAR_MEMORY_SIZE);
  const addrToLine = new Map<number, number>();
  const labels = new Map<string, number>();
  const instrAddrs: number[] = [];

  interface DeferredEntry {
    instructionStart: number;
    offsetInBytes: number;
    isSob: boolean;
    kind: 'word' | 'branch';
  }
  const deferred = new Map<string, DeferredEntry[]>();

  const s = new Scanner(input);
  let address = 0;

  const writeByte = (line: number, value: number): void => {
    bytes[address] = value & 0xff;
    addrToLine.set(address, line);
    address = (address + 1) & 0xffff;
  };

  const resolveBranchDelta = (label: string, instrStart: number, isSob: boolean, target: number): number => {
    let delta = target - (instrStart + 2);
    if (isSob) delta = -delta;
    if (delta < -128 || delta > 127) {
      throw new Error(`Label inacessível a partir de um branch: ${label}`);
    }
    return delta & 0xff;
  };

  const applyReference = (
    instructionStart: number,
    line: number,
    ref: LabelRef,
  ): void => {
    const known = labels.get(ref.label);
    if (known !== undefined) {
      if (ref.kind === 'branch') {
        bytes[(instructionStart + ref.offsetInBytes) & 0xffff] = resolveBranchDelta(
          ref.label,
          instructionStart,
          ref.isSob,
          known,
        );
      } else {
        const hi = (instructionStart + ref.offsetInBytes) & 0xffff;
        const lo = (instructionStart + ref.offsetInBytes + 1) & 0xffff;
        bytes[hi] = (known >>> 8) & 0xff;
        bytes[lo] = known & 0xff;
      }
      addrToLine.set((instructionStart + ref.offsetInBytes) & 0xffff, line);
      if (ref.kind === 'word') {
        addrToLine.set((instructionStart + ref.offsetInBytes + 1) & 0xffff, line);
      }
      return;
    }
    const list = deferred.get(ref.label) ?? [];
    list.push({
      instructionStart,
      offsetInBytes: ref.offsetInBytes,
      isSob: ref.isSob,
      kind: ref.kind,
    });
    deferred.set(ref.label, list);
  };

  const defineLabel = (name: string): void => {
    if (labels.has(name)) {
      throw new Error(`Label duplicado: ${name}`);
    }
    const value = address & 0xffff;
    labels.set(name, value);
    const fixups = deferred.get(name);
    if (fixups) {
      for (const f of fixups) {
        if (f.kind === 'branch') {
          bytes[(f.instructionStart + f.offsetInBytes) & 0xffff] = resolveBranchDelta(
            name,
            f.instructionStart,
            f.isSob,
            value,
          );
        } else {
          const hi = (f.instructionStart + f.offsetInBytes) & 0xffff;
          const lo = (f.instructionStart + f.offsetInBytes + 1) & 0xffff;
          bytes[hi] = (value >>> 8) & 0xff;
          bytes[lo] = value & 0xff;
        }
      }
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
      const a = s.matchSignedInt();
      if (a === null) {
        throw new Error(`Linha ${line}: endereço esperado após '@'`);
      }
      address = a & 0xffff;
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
      const instructionStart = address;
      instrAddrs.push(instructionStart);
      for (const b of result.bytes) writeByte(line, b);
      for (const ref of result.references) {
        applyReference(instructionStart, line, ref);
      }
      continue;
    }

    if (c === '-' || c === '+' || isDigit(c)) {
      const value = s.matchSignedInt();
      if (value === null) {
        throw new Error(`Linha ${line}: número esperado`);
      }
      if (value >= 0 && value <= 0xff) {
        writeByte(line, value);
      } else {
        writeByte(line, (value >>> 8) & 0xff);
        writeByte(line, value & 0xff);
      }
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
