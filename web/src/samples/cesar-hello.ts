import { assembleProgram } from '@/assemblers/cesar';
import type { Sample } from './types';

// Write "HELLO, CESAR!" to the memory-mapped display strip at 0xFFDC..0xFFE8.
// Each MOV #<ASCII>, <addr> writes one byte to the display.
//
// Kept as raw MOV-per-char so the SourceView lights up the matching display
// cell each step; a loop would obscure the per-character animation.
const MSG = 'HELLO, CESAR!';
const DISPLAY_BASE = 0xffdc;

function buildSource(): string {
  const lines: string[] = [':Hello'];
  for (let i = 0; i < MSG.length; i++) {
    const ch = MSG.charCodeAt(i);
    const addr = DISPLAY_BASE + i;
    lines.push(`\tMOV #${ch}, ${addr}\t; '${MSG[i]}' -> 0x${addr.toString(16).toUpperCase()}`);
  }
  lines.push(':Halt');
  lines.push('\tHLT');
  return lines.join('\n') + '\n';
}

const SOURCE = buildSource();
const assembled = assembleProgram(SOURCE);

export const CesarHelloSample: Sample = {
  id: 'cesar-hello',
  name: 'Hello.Cesar',
  description: 'Write "HELLO, CESAR!" to the display strip.',
  machine: 'cesar',
  sourceText: SOURCE,
  bytes: assembled.bytes,
  addrToLine: assembled.addrToLine,
  labels: assembled.labels,
};
