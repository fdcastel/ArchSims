import { expect } from 'vitest';
import type { NeanderCpu } from '@/core/neander';
import type { AhmesCpu } from '@/core/ahmes';
import type { RamsesCpu } from '@/core/ramses';
import type { CesarCpu } from '@/core/cesar';

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

export interface RamsesExpect {
  ra?: number;
  rb?: number;
  rx?: number;
  programCounter?: number;
  memoryReads?: number;
  memoryWrites?: number;
  halted?: boolean;
  negative?: boolean;
  zero?: boolean;
  carry?: boolean;
  memoryAt?: ReadonlyArray<readonly [number, number]>;
}

export interface CesarExpect {
  r0?: number;
  r1?: number;
  r2?: number;
  r3?: number;
  r4?: number;
  r5?: number;
  r6?: number;
  programCounter?: number;
  memoryReads?: number;
  memoryWrites?: number;
  halted?: boolean;
  negative?: boolean;
  zero?: boolean;
  overflow?: boolean;
  carry?: boolean;
  memoryAt?: ReadonlyArray<readonly [number, number]>;
  instructionRegisterAt?: readonly [number, number];
  instructionRegisterIs?: ReadonlyArray<number>;
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

export const expectRamsesState = (cpu: RamsesCpu, e: RamsesExpect): void => {
  if (e.ra !== undefined) expect(cpu.registers.ra).toBe(e.ra);
  if (e.rb !== undefined) expect(cpu.registers.rb).toBe(e.rb);
  if (e.rx !== undefined) expect(cpu.registers.rx).toBe(e.rx);
  if (e.programCounter !== undefined) expect(cpu.registers.programCounter).toBe(e.programCounter);
  if (e.memoryReads !== undefined) expect(cpu.memory.readCount).toBe(e.memoryReads);
  if (e.memoryWrites !== undefined) expect(cpu.memory.writeCount).toBe(e.memoryWrites);
  if (e.halted !== undefined) expect(cpu.registers.flags.halted).toBe(e.halted);
  if (e.negative !== undefined) expect(cpu.registers.flags.negative).toBe(e.negative);
  if (e.zero !== undefined) expect(cpu.registers.flags.zero).toBe(e.zero);
  if (e.carry !== undefined) expect(cpu.registers.flags.carry).toBe(e.carry);
  if (e.memoryAt) {
    for (const [addr, val] of e.memoryAt) {
      expect(cpu.memory.data[addr]).toBe(val);
    }
  }
};

export const expectCesarState = (cpu: CesarCpu, e: CesarExpect): void => {
  const r = cpu.registers.r;
  if (e.r0 !== undefined) expect(r[0]).toBe(e.r0);
  if (e.r1 !== undefined) expect(r[1]).toBe(e.r1);
  if (e.r2 !== undefined) expect(r[2]).toBe(e.r2);
  if (e.r3 !== undefined) expect(r[3]).toBe(e.r3);
  if (e.r4 !== undefined) expect(r[4]).toBe(e.r4);
  if (e.r5 !== undefined) expect(r[5]).toBe(e.r5);
  if (e.r6 !== undefined) expect(r[6]).toBe(e.r6);
  if (e.programCounter !== undefined) expect(r[7]).toBe(e.programCounter);
  if (e.memoryReads !== undefined) expect(cpu.memory.readCount).toBe(e.memoryReads);
  if (e.memoryWrites !== undefined) expect(cpu.memory.writeCount).toBe(e.memoryWrites);
  if (e.halted !== undefined) expect(cpu.registers.flags.halted).toBe(e.halted);
  if (e.negative !== undefined) expect(cpu.registers.flags.negative).toBe(e.negative);
  if (e.zero !== undefined) expect(cpu.registers.flags.zero).toBe(e.zero);
  if (e.overflow !== undefined) expect(cpu.registers.flags.overflow).toBe(e.overflow);
  if (e.carry !== undefined) expect(cpu.registers.flags.carry).toBe(e.carry);
  if (e.memoryAt) {
    for (const [addr, val] of e.memoryAt) {
      expect(cpu.memory.data[addr]).toBe(val);
    }
  }
  if (e.instructionRegisterAt) {
    const [addr, size] = e.instructionRegisterAt;
    const slice = Array.from(cpu.memory.data.subarray(addr, addr + size));
    expect(cpu.registers.instructionRegister.data).toEqual(slice);
  }
  if (e.instructionRegisterIs) {
    expect(cpu.registers.instructionRegister.data).toEqual(Array.from(e.instructionRegisterIs));
  }
};
