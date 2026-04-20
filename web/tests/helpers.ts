import { expect } from 'vitest';
import type { NeanderCpu } from '@/core/neander';
import type { AhmesCpu } from '@/core/ahmes';

interface BaseExpect {
  programCounter?: number;
  accumulator?: number;
  memoryReads?: number;
  memoryWrites?: number;
  halted?: boolean;
  negative?: boolean;
  zero?: boolean;
  memoryAt?: ReadonlyArray<readonly [number, number]>;
}

export type NeanderExpect = BaseExpect;

export interface AhmesExpect extends BaseExpect {
  carry?: boolean;
  overflow?: boolean;
  borrow?: boolean;
}

const checkBase = (
  cpu: NeanderCpu | AhmesCpu,
  e: BaseExpect,
): void => {
  if (e.programCounter !== undefined) expect(cpu.registers.programCounter).toBe(e.programCounter);
  if (e.accumulator !== undefined) expect(cpu.registers.accumulator).toBe(e.accumulator);
  if (e.memoryReads !== undefined) expect(cpu.memory.readCount).toBe(e.memoryReads);
  if (e.memoryWrites !== undefined) expect(cpu.memory.writeCount).toBe(e.memoryWrites);
  if (e.halted !== undefined) expect(cpu.registers.flags.halted).toBe(e.halted);
  if (e.negative !== undefined) expect(cpu.registers.flags.negative).toBe(e.negative);
  if (e.zero !== undefined) expect(cpu.registers.flags.zero).toBe(e.zero);
  if (e.memoryAt) {
    for (const [addr, val] of e.memoryAt) {
      expect(cpu.memory.data[addr]).toBe(val);
    }
  }
};

export const expectNeanderState = (cpu: NeanderCpu, e: NeanderExpect): void => {
  checkBase(cpu, e);
};

export const expectAhmesState = (cpu: AhmesCpu, e: AhmesExpect): void => {
  checkBase(cpu, e);
  if (e.carry !== undefined) expect(cpu.registers.flags.carry).toBe(e.carry);
  if (e.overflow !== undefined) expect(cpu.registers.flags.overflow).toBe(e.overflow);
  if (e.borrow !== undefined) expect(cpu.registers.flags.borrow).toBe(e.borrow);
};
