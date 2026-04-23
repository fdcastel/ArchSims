import { assembleProgram } from '@/assemblers/ramses';
import type { Sample } from './types';

// Matches Samples/BitShift.Ramses.txt at repo root — shifts 0x80 right in
// each register in turn until zero, then halts. Highlights the Ramses
// register-file + jump-on-zero flow.
const SOURCE = `:StartRA
\tLDR A #128
:RepeatRA
\tSHR A
\tJZ :StartRB
\tJMP :RepeatRA
:StartRB
\tLDR B #128
:RepeatRB
\tSHR B
\tJZ :StartRX
\tJMP :RepeatRB
:StartRX
\tLDR X #128
:RepeatRX
\tSHR X
\tJZ :Finish
\tJMP :RepeatRX

:Finish
\tHLT`;

const assembled = assembleProgram(SOURCE);

export const RamsesBitShiftSample: Sample = {
  id: 'ramses-bitshift',
  name: 'BitShift.Ramses',
  description: 'Shift 0x80 right in RA, RB, then RX.',
  machine: 'ramses',
  sourceText: SOURCE,
  bytes: assembled.bytes,
  addrToLine: assembled.addrToLine,
  labels: assembled.labels,
};
