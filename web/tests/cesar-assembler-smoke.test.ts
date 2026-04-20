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

describe('cesar assembler smoke', () => {
  it('encodes simple, flag, branch, JMP, SOB, JSR, RTS, NOT forms', () => {
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
    expect(assembleInstruction('JSR R3, ((R7)+)')).toEqual([
      CesarInstruction.Jsr | CesarRegister.R3,
      CesarRegister.R7 | CesarAddressMode.RegPostIncIndirect,
    ]);

    expect(assembleInstruction('RTS R4')).toEqual([CesarInstruction.Rts | CesarRegister.R4]);
  });

  it('encodes all 8 addressing-mode syntaxes for a one-operand op', () => {
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
  });

  it('encodes two-operand forms with and without address suffixes', () => {
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
      CesarAddressMode.Indexed,
      CesarRegister.R1,
      CesarAddressMode.RegPostIncIndirect,
      CesarRegister.R2,
    );
    expect(assembleInstruction('MOV 10(R1), ((R2)+)')).toEqual([
      (h >>> 8) & 0xff,
      h & 0xff,
      0,
      10,
    ]);

    const i = encodeCesarInstructionTwoOperand(
      CesarInstruction.Mov,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R1,
      CesarAddressMode.IndexedIndirect,
      CesarRegister.R2,
    );
    expect(assembleInstruction('MOV (10(R1)), (20(R2))')).toEqual([
      (i >>> 8) & 0xff,
      i & 0xff,
      0,
      10,
      0,
      20,
    ]);

    expect(assembleInstruction('MOV #-1, R1')).toEqual([147, 193, 255, 255]);
    expect(assembleInstruction('MOV #65535, R1')).toEqual([147, 193, 255, 255]);
  });

  it('assembles a multi-instruction program with forward/backward labels', () => {
    const program = `
      MOV #10, R1
      MOV 1000, R2
      MOV :L1, R3
      MOV :L2, R4
      MOV :L2, R5
    :L1
      HLT
      NOP
      NOP
      BR :L1
      SOB R1, :L1
      JMP :L1
      BR :L3
      SOB R3, :L3
      JMP :L3
      NOP
      NOP
    :L3
      JMP :L2
    @1000
      0
      123
    :L2
      1234
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
    expect(result.labels.get('L1')).toBe(20);
    expect(result.labels.get('L2')).toBe(1002);
    expect(result.labels.get('L3')).toBe(41);

    const cpu = createCesarCpu();
    cpu.memory.data.set(result.bytes);
    cesarStep(cpu);
    cesarStep(cpu);
    cesarStep(cpu);
    cesarStep(cpu);
    cesarStep(cpu);
    cesarStep(cpu);
    expect(cpu.registers.r[1]).toBe(10);
    expect(cpu.registers.r[2]).toBe(123);
    expect(cpu.registers.r[3]).toBe(61440);
    expect(cpu.registers.r[4]).toBe(1234);
    expect(cpu.registers.r[5]).toBe(1234);
    expect(cpu.registers.r[7]).toBe(21);
    expect(cpu.registers.flags.halted).toBe(true);
  });

  it('throws "Label indefinido" for unresolved references', () => {
    expect(() => assembleProgram('JMP :L1')).toThrowError(/Label indefinido: L1/);
  });

  it('throws "Label inacessível" when a branch target is out of range', () => {
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
