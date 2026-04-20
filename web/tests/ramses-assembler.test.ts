import { describe, expect, it } from 'vitest';
import { assembleInstruction, assembleProgram } from '@/assemblers/ramses';
import {
  RamsesAddressMode,
  RamsesInstruction,
  RamsesRegister,
  createRamsesCpu,
  ramsesStep,
} from '@/core/ramses';
import { expectRamsesState } from './helpers';

describe('ramses assembler parity', () => {
  it('AssembleInstruction works as expected', () => {
    expect(assembleInstruction('NOP')).toEqual([RamsesInstruction.Nop]);
    expect(assembleInstruction('HLT')).toEqual([RamsesInstruction.Hlt]);

    expect(assembleInstruction('STR A 12')).toEqual([
      RamsesInstruction.Str | RamsesRegister.Ra | RamsesAddressMode.Direct,
      12,
    ]);
    expect(assembleInstruction('STR B 23,I')).toEqual([
      RamsesInstruction.Str | RamsesRegister.Rb | RamsesAddressMode.Indirect,
      23,
    ]);
    expect(assembleInstruction('STR X #34')).toEqual([
      RamsesInstruction.Str | RamsesRegister.Rx | RamsesAddressMode.Immediate,
      34,
    ]);
    expect(assembleInstruction('STR A 45,X')).toEqual([
      RamsesInstruction.Str | RamsesRegister.Ra | RamsesAddressMode.Indexed,
      45,
    ]);

    expect(assembleInstruction('NOT A')).toEqual([RamsesInstruction.Not | RamsesRegister.Ra]);
    expect(assembleInstruction('NOT B')).toEqual([RamsesInstruction.Not | RamsesRegister.Rb]);
    expect(assembleInstruction('NOT X')).toEqual([RamsesInstruction.Not | RamsesRegister.Rx]);

    expect(assembleInstruction('JMP 12')).toEqual([
      RamsesInstruction.Jmp | RamsesAddressMode.Direct,
      12,
    ]);
    expect(assembleInstruction('JMP 23,I')).toEqual([
      RamsesInstruction.Jmp | RamsesAddressMode.Indirect,
      23,
    ]);
    expect(assembleInstruction('JMP #34')).toEqual([
      RamsesInstruction.Jmp | RamsesAddressMode.Immediate,
      34,
    ]);
    expect(assembleInstruction('JMP 45,X')).toEqual([
      RamsesInstruction.Jmp | RamsesAddressMode.Indexed,
      45,
    ]);

    expect(assembleInstruction('LDR X #0')).toEqual([
      RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
      0,
    ]);
    expect(assembleInstruction('LDR X #127')).toEqual([
      RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
      127,
    ]);
    expect(assembleInstruction('LDR X #128')).toEqual([
      RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
      128,
    ]);
    expect(assembleInstruction('LDR X #255')).toEqual([
      RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
      255,
    ]);
  });

  it('AssembleProgram works as expected', () => {
    const program = `
        LDR A 10
        LDR B :L1
        LDR X :L1,I
        HLT              ; End of program
    @10
        123              ; Ra value
    :L1
        14               ; Rb value
    @14
        66               ; Rx value
    `;

    const expectedProgram = new Uint8Array([
      RamsesInstruction.Ldr | RamsesRegister.Ra | RamsesAddressMode.Direct,
      10,
      RamsesInstruction.Ldr | RamsesRegister.Rb | RamsesAddressMode.Direct,
      11,
      RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Indirect,
      11,
      RamsesInstruction.Hlt,
      0,
      0,
      0,
      123,
      14,
      0,
      0,
      66,
    ]);

    const result = assembleProgram(program);
    expect(result.bytes.subarray(0, 15)).toEqual(expectedProgram);

    const cpu = createRamsesCpu();
    cpu.memory.data.set(result.bytes);
    ramsesStep(cpu);
    ramsesStep(cpu);
    ramsesStep(cpu);
    ramsesStep(cpu);
    expectRamsesState(cpu, {
      ra: 123,
      rb: 14,
      rx: 66,
      programCounter: 7,
      halted: true,
    });
  });

  it('AssembleProgram fails with undeclared label', () => {
    expect(() => assembleProgram('LDR A :L1')).toThrowError(/Label indefinido: L1/);
  });
});
