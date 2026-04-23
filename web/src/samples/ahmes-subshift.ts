import { AhmesInstruction } from '@/core/ahmes';
import type { Sample } from './types';

// Compute 80 - 30 = 50, store, then SHR and ROR-through-carry to show
// how the new Ahmes flags interact (C vs B, rotate path through carry).
function buildBytes(): Uint8Array {
  const b = new Uint8Array(256);
  b[0x00] = AhmesInstruction.Lda;
  b[0x01] = 0x80;
  b[0x02] = AhmesInstruction.Sub;
  b[0x03] = 0x81;
  b[0x04] = AhmesInstruction.Sta;
  b[0x05] = 0x82;
  b[0x06] = AhmesInstruction.Shr;
  b[0x07] = AhmesInstruction.Ror;
  b[0x08] = AhmesInstruction.Sta;
  b[0x09] = 0x83;
  b[0x0a] = AhmesInstruction.Hlt;
  b[0x80] = 0x50; // 80
  b[0x81] = 0x1e; // 30
  return b;
}

const SOURCE = `; Ahmes SUB + SHR + ROR demo
; 80 - 30 = 50 (SUB sets Borrow, not Carry),
; then SHR (bit 0 -> C) and ROR (rotate through carry).
        LDA 0x80        ; AC = 80
        SUB 0x81        ; AC = 50, B cleared on no-borrow
        STA 0x82        ; store difference
        SHR             ; shift AC right, bit 0 -> C
        ROR             ; rotate AC right through carry
        STA 0x83        ; store shifted/rotated AC
        HLT
; Data
@0x80   0x50            ; 80
        0x1E            ; 30
`;

const ADDR_TO_LINE = new Map<number, number>([
  [0x00, 4],
  [0x02, 5],
  [0x04, 6],
  [0x06, 7],
  [0x07, 8],
  [0x08, 9],
  [0x0a, 10],
  [0x80, 12],
  [0x81, 13],
]);

export const AhmesSubShiftSample: Sample = {
  id: 'ahmes-subshift',
  name: 'SUB + SHR + ROR',
  description: '80 − 30 = 50, then shift and rotate through carry.',
  machine: 'ahmes',
  sourceText: SOURCE,
  bytes: buildBytes(),
  addrToLine: ADDR_TO_LINE,
};
