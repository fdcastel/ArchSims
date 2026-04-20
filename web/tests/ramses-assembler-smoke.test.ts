import { describe, expect, it } from 'vitest';
import { assembleInstruction, assembleProgram } from '@/assemblers/ramses';
import {
  RamsesAddressMode,
  RamsesInstruction,
  RamsesRegister,
  createRamsesCpu,
  ramsesStep,
} from '@/core/ramses';

describe('ramses assembler smoke', () => {
  it('assembles single instructions across modes', () => {
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

    expect(assembleInstruction('NOT A')).toEqual([
      RamsesInstruction.Not | RamsesRegister.Ra,
    ]);
    expect(assembleInstruction('JMP #34')).toEqual([
      RamsesInstruction.Jmp | RamsesAddressMode.Immediate,
      34,
    ]);
    expect(assembleInstruction('LDR X #255')).toEqual([
      RamsesInstruction.Ldr | RamsesRegister.Rx | RamsesAddressMode.Immediate,
      255,
    ]);
  });

  it('assembles a program with forward labels and @ directives', () => {
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

    const expected = new Uint8Array([
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
    expect(result.bytes.subarray(0, 15)).toEqual(expected);
    expect(result.labels.get('L1')).toBe(11);

    const cpu = createRamsesCpu();
    cpu.memory.data.set(result.bytes);
    ramsesStep(cpu);
    ramsesStep(cpu);
    ramsesStep(cpu);
    ramsesStep(cpu);
    expect(cpu.registers.ra).toBe(123);
    expect(cpu.registers.rb).toBe(14);
    expect(cpu.registers.rx).toBe(66);
    expect(cpu.registers.programCounter).toBe(7);
    expect(cpu.registers.flags.halted).toBe(true);
  });

  it('throws "Label indefinido" for unresolved references', () => {
    expect(() => assembleProgram('LDR A :L1')).toThrowError(/Label indefinido: L1/);
  });

  it('throws "Label duplicado" for duplicate definitions', () => {
    expect(() => assembleProgram(':L1\n:L1')).toThrowError(/Label duplicado: L1/);
  });

  it('records per-byte source-line mapping and instruction addresses', () => {
    const program = `LDR A 10
HLT
@5
42`;
    const result = assembleProgram(program);
    expect(result.addrToLine.get(0)).toBe(1);
    expect(result.addrToLine.get(1)).toBe(1);
    expect(result.addrToLine.get(2)).toBe(2);
    expect(result.addrToLine.get(5)).toBe(4);
    expect(result.instrAddrs).toEqual([0, 2]);
  });
});
