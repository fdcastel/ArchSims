import { assembleProgram } from '@/assemblers/cesar';
import type { Sample } from './types';

// Matches Samples/BitShift.Cesar.txt — rotates bits through carry across
// R0..R7 by self-modifying the INC operand byte, then HLT at 32774.
const SOURCE = `:Start
\tMOV #32768, R0
:Loop
\tROR R0
\tBNE :Loop

\tROR R1           ; Advances carry to next register
\tINC 4            ; self-modifying code
\tINC 8

\tBR :Loop
@32774
\tHLT
`;

const assembled = assembleProgram(SOURCE);

export const CesarBitShiftSample: Sample = {
  id: 'cesar-bitshift',
  name: 'BitShift.Cesar',
  description: 'Rotate carry across registers via self-modifying INC.',
  machine: 'cesar',
  sourceText: SOURCE,
  bytes: assembled.bytes,
  addrToLine: assembled.addrToLine,
  labels: assembled.labels,
};
