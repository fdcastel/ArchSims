import { NeanderInstruction } from '@/core/neander';
import type { Sample } from './types';

// Sum of 4 bytes at 0x80..0x83, store result at 0x84, halt.
function buildBytes(): Uint8Array {
  const b = new Uint8Array(256);
  b[0x00] = NeanderInstruction.Lda;
  b[0x01] = 0x80;
  b[0x02] = NeanderInstruction.Add;
  b[0x03] = 0x81;
  b[0x04] = NeanderInstruction.Add;
  b[0x05] = 0x82;
  b[0x06] = NeanderInstruction.Add;
  b[0x07] = 0x83;
  b[0x08] = NeanderInstruction.Sta;
  b[0x09] = 0x84;
  b[0x0a] = NeanderInstruction.Hlt;
  b[0x80] = 0x02;
  b[0x81] = 0x04;
  b[0x82] = 0x08;
  b[0x83] = 0x10;
  return b;
}

const SOURCE = `; Neander sum-of-memory demo
; Loads byte at 0x80 into AC, accumulates 0x81..0x83,
; stores result at 0x84, halts.
        LDA 0x80
        ADD 0x81
        ADD 0x82
        ADD 0x83
        STA 0x84
        HLT
; Data
@0x80   0x02
        0x04
        0x08
        0x10
`;

// Hand-rolled addr→line map so the SourceView can align the caret.
// Line numbers are 1-based and match SOURCE above.
const ADDR_TO_LINE = new Map<number, number>([
  [0x00, 4],
  [0x02, 5],
  [0x04, 6],
  [0x06, 7],
  [0x08, 8],
  [0x0a, 9],
  [0x80, 11],
  [0x81, 12],
  [0x82, 13],
  [0x83, 14],
]);

export const NeanderSumSample: Sample = {
  id: 'neander-sum',
  name: 'Sum of memory',
  description: 'Sum 4 bytes at 0x80..0x83, store at 0x84.',
  machine: 'neander',
  sourceText: SOURCE,
  bytes: buildBytes(),
  addrToLine: ADDR_TO_LINE,
};
