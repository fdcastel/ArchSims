import { describe, expect, it } from 'vitest';
import { assembleInstruction, assembleProgram } from '@/assemblers/cesar';
import {
  CesarAddressMode,
  CesarFlagBit,
  CesarInstruction,
  CesarRegister,
  cesarStep,
  createCesarCpu,
  encodeCesarInstructionTwoOperand,
} from '@/core/cesar';
import { expectCesarState } from './helpers';

describe('cesar assembler parity', () => {
  it('AssembleInstruction works as expected', () => {
    expect(assembleInstruction('NOP')).toEqual([CesarInstruction.Nop]);
    expect(assembleInstruction('HLT')).toEqual([CesarInstruction.Hlt]);

    expect(assembleInstruction('CCC')).toEqual([CesarInstruction.Ccc]);
    expect(assembleInstruction('CCC NZVC')).toEqual([
      CesarInstruction.Ccc |
        CesarFlagBit.Negative |
        CesarFlagBit.Zero |
        CesarFlagBit.Overflow |
        CesarFlagBit.Carry,
    ]);
    expect(assembleInstruction('SCC')).toEqual([CesarInstruction.Scc]);
    expect(assembleInstruction('SCC ZC')).toEqual([
      CesarInstruction.Scc | CesarFlagBit.Zero | CesarFlagBit.Carry,
    ]);

    expect(assembleInstruction('BR 0')).toEqual([CesarInstruction.Br, 0]);
    expect(assembleInstruction('BNE 10')).toEqual([CesarInstruction.Bne, 10]);
    expect(assembleInstruction('BNE -10')).toEqual([CesarInstruction.Bne, 246]);
    expect(assembleInstruction('BNE 246')).toEqual([CesarInstruction.Bne, 246]);

    expect(assembleInstruction('JMP R7')).toEqual([CesarInstruction.Jmp, CesarRegister.R7]);
    expect(assembleInstruction('JMP (R7)+')).toEqual([
      CesarInstruction.Jmp,
      CesarRegister.R7 | CesarAddressMode.RegPostInc,
    ]);
    expect(assembleInstruction('JMP ((R7)+)')).toEqual([
      CesarInstruction.Jmp,
      CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
    ]);

    expect(assembleInstruction('JMP #10')).toEqual([
      CesarInstruction.Jmp,
      CesarRegister.R7 | CesarAddressMode.RegPostInc,
      0,
      10,
    ]);
    expect(assembleInstruction('JMP 10')).toEqual([
      CesarInstruction.Jmp,
      CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
      0,
      10,
    ]);
    expect(assembleInstruction('JMP #-10')).toEqual([
      CesarInstruction.Jmp,
      CesarRegister.R7 | CesarAddressMode.RegPostInc,
      255,
      246,
    ]);
    expect(assembleInstruction('JMP -10')).toEqual([
      CesarInstruction.Jmp,
      CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
      255,
      246,
    ]);

    expect(assembleInstruction('SOB R1, 123')).toEqual([
      CesarInstruction.Sob | CesarRegister.R1,
      123,
    ]);
    expect(assembleInstruction('SOB R2, 234')).toEqual([
      CesarInstruction.Sob | CesarRegister.R2,
      234,
    ]);

    expect(assembleInstruction('JSR R1, R7')).toEqual([
      CesarInstruction.Jsr | CesarRegister.R1,
      CesarRegister.R7,
    ]);
    expect(assembleInstruction('JSR R2, (R7)+')).toEqual([
      CesarInstruction.Jsr | CesarRegister.R2,
      CesarRegister.R7 | CesarAddressMode.RegPostInc,
    ]);
    expect(assembleInstruction('JSR R3, ((R7)+)')).toEqual([
      CesarInstruction.Jsr | CesarRegister.R3,
      CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
    ]);

    expect(assembleInstruction('RTS R4')).toEqual([CesarInstruction.Rts | CesarRegister.R4]);

    expect(assembleInstruction('NOT #15')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.RegPostInc,
      0,
      15,
    ]);
    expect(assembleInstruction('NOT 15')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
      0,
      15,
    ]);

    expect(assembleInstruction('NOT R7')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.Register,
    ]);
    expect(assembleInstruction('NOT (R7)+')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.RegPostInc,
    ]);
    expect(assembleInstruction('NOT -(R7)')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.RegPreDec,
    ]);
    expect(assembleInstruction('NOT 2(R7)')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.Indexed,
      0,
      2,
    ]);
    expect(assembleInstruction('NOT (R7)')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.RegisterIndirect,
    ]);
    expect(assembleInstruction('NOT ((R7)+)')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
    ]);
    expect(assembleInstruction('NOT (-(R7))')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.RegPreDecIndirect,
    ]);
    expect(assembleInstruction('NOT (2(R7))')).toEqual([
      CesarInstruction.Not,
      CesarRegister.R7 | CesarAddressMode.IndexedIndirect,
      0,
      2,
    ]);

    const e = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegPostInc,
      CesarRegister.R7,
      CesarAddressMode.Register,
      CesarRegister.R1,
    );
    expect(assembleInstruction('MOV #10, R1')).toEqual([(e >>> 8) & 0xff, e & 0xff, 0, 10]);

    const f = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegPostIncIndirect,
      CesarRegister.R7,
      CesarAddressMode.Register,
      CesarRegister.R2,
    );
    expect(assembleInstruction('MOV 1000, R2')).toEqual([(f >>> 8) & 0xff, f & 0xff, 3, 232]);

    const g = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.Register,
      CesarRegister.R1,
      CesarAddressMode.RegPostInc,
      CesarRegister.R2,
    );
    expect(assembleInstruction('MOV R1, (R2)+')).toEqual([(g >>> 8) & 0xff, g & 0xff]);

    const h = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegisterIndirect,
      CesarRegister.R1,
      CesarAddressMode.RegPreDec,
      CesarRegister.R2,
    );
    expect(assembleInstruction('MOV (R1), -(R2)')).toEqual([(h >>> 8) & 0xff, h & 0xff]);

    const i = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.Indexed,
      CesarRegister.R1,
      CesarAddressMode.RegPostIncIndirect,
      CesarRegister.R2,
    );
    expect(assembleInstruction('MOV 10(R1), ((R2)+)')).toEqual([
      (i >>> 8) & 0xff,
      i & 0xff,
      0,
      10,
    ]);

    const j = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R1,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R2,
    );
    expect(assembleInstruction('MOV (10(R1)), (20(R2))')).toEqual([
      (j >>> 8) & 0xff,
      j & 0xff,
      0,
      10,
      0,
      20,
    ]);

    const k = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.RegPreDecIndirect,
      CesarRegister.R1,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R2,
    );
    expect(assembleInstruction('MOV (-(R1)), (20(R2))')).toEqual([
      (k >>> 8) & 0xff,
      k & 0xff,
      0,
      20,
    ]);

    expect(assembleInstruction('MOV #32768, R1')).toEqual([147, 193, 128, 0]);
    expect(assembleInstruction('MOV #-32768, R1')).toEqual([147, 193, 128, 0]);
    expect(assembleInstruction('MOV #65535, R1')).toEqual([147, 193, 255, 255]);
    expect(assembleInstruction('MOV #-1, R1')).toEqual([147, 193, 255, 255]);
  });

  it('AssembleProgram works as expected', () => {
    const program = `
        MOV #10, R1
        MOV 1000, R2
        MOV :L1, R3
        MOV :L2, R4
        MOV :L2, R5
    :L1
        HLT              ; End of program
        NOP
        NOP
        BR :L1           ; Test branch/jump backwards
        SOB R1, :L1
        JMP :L1
        BR :L3           ; Test branch/jump forward
        SOB R3, :L3
        JMP :L3
        NOP
        NOP
    :L3
        JMP :L2
    @1000
        0                ; R2 value
        123
    :L2
        1234             ; R4,R5 value
    `;

    const expectedProgram = new Uint8Array([
      147, 193, 0, 10,
      155, 194, 3, 232,
      155, 195, 0, 20,
      155, 196, 3, 234,
      155, 197, 3, 234,
      240,
      0,
      0,
      48, 251,
      81, 7,
      64, 47, 0, 20,
      48, 8,
      83, 250,
      64, 47, 0, 41,
      0,
      0,
      64, 47, 3, 234,
    ]);
    const expectedData = new Uint8Array([0, 123, 4, 210]);

    const result = assembleProgram(program);
    expect(result.bytes.subarray(0, 45)).toEqual(expectedProgram);
    expect(result.bytes.subarray(1000, 1004)).toEqual(expectedData);

    const cpu = createCesarCpu();
    cpu.memory.data.set(result.bytes);
    cesarStep(cpu);
    cesarStep(cpu);
    cesarStep(cpu);
    cesarStep(cpu);
    cesarStep(cpu);
    cesarStep(cpu);
    expectCesarState(cpu, {
      r1: 10,
      r2: 123,
      r3: 61440,
      r4: 1234,
      r5: 1234,
      programCounter: 21,
      halted: true,
    });
  });

  it('AssembleProgram fails with undeclared label', () => {
    expect(() => assembleProgram('JMP :L1')).toThrowError(/Label indefinido: L1/);
  });

  it('AssembleProgram fails with far branches', () => {
    const program = `
        BR :L1
    @1000
    :L1
        HLT
    `;
    expect(() => assembleProgram(program)).toThrowError(
      /Label inacessível a partir de um branch: L1/,
    );
  });
});
