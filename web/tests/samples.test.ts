import { describe, expect, it } from 'vitest';
import { cesarStep, createCesarCpu, CesarDisplayMemoryAddress } from '@/core/cesar';
import { createNeanderCpu, neanderStep } from '@/core/neander';
import { createAhmesCpu, ahmesStep } from '@/core/ahmes';
import { createRamsesCpu, ramsesStep } from '@/core/ramses';
import { SAMPLES_BY_MACHINE } from '@/samples';
import { CesarHelloSample } from '@/samples/cesar-hello';
import { RamsesBitShiftSample } from '@/samples/ramses-bitshift';
import { CesarBitShiftSample } from '@/samples/cesar-bitshift';
import { NeanderSumSample } from '@/samples/neander-sum';
import { AhmesSubShiftSample } from '@/samples/ahmes-subshift';

const MAX_STEPS = 200_000;

function runUntilHalted(
  cpu: { registers: { flags: { halted: boolean } } },
  step: () => void,
): number {
  let n = 0;
  while (!cpu.registers.flags.halted && n < MAX_STEPS) {
    step();
    n++;
  }
  return n;
}

describe('bundled samples', () => {
  it('registers exactly one sample per 8-bit machine and two for cesar', () => {
    expect(SAMPLES_BY_MACHINE.neander).toHaveLength(1);
    expect(SAMPLES_BY_MACHINE.ahmes).toHaveLength(1);
    expect(SAMPLES_BY_MACHINE.ramses).toHaveLength(1);
    expect(SAMPLES_BY_MACHINE.cesar).toHaveLength(2);
  });

  it('neander-sum: accumulates 2+4+8+16 = 30 at 0x84', () => {
    const cpu = createNeanderCpu();
    cpu.memory.data.set(NeanderSumSample.bytes);
    runUntilHalted(cpu, () => neanderStep(cpu));
    expect(cpu.memory.data[0x84]).toBe(0x1e);
  });

  it('ahmes-subshift: 80-30=50, SHR+ROR stored at 0x83', () => {
    const cpu = createAhmesCpu();
    cpu.memory.data.set(AhmesSubShiftSample.bytes);
    runUntilHalted(cpu, () => ahmesStep(cpu));
    expect(cpu.memory.data[0x82]).toBe(50);
    // 50 = 0x32; SHR -> 0x19 (C=0); ROR through C=0 -> 0x0C.
    expect(cpu.memory.data[0x83]).toBe(0x0c);
  });

  it('ramses-bitshift: runs to HLT with all three registers zero', () => {
    const cpu = createRamsesCpu();
    cpu.memory.data.set(RamsesBitShiftSample.bytes);
    runUntilHalted(cpu, () => ramsesStep(cpu));
    expect(cpu.registers.flags.halted).toBe(true);
    expect(cpu.registers.ra).toBe(0);
    expect(cpu.registers.rb).toBe(0);
    expect(cpu.registers.rx).toBe(0);
  });

  it('cesar-bitshift: runs to HLT at 32774 (0x8006)', () => {
    const cpu = createCesarCpu();
    cpu.memory.data.set(CesarBitShiftSample.bytes);
    runUntilHalted(cpu, () => cesarStep(cpu));
    expect(cpu.registers.flags.halted).toBe(true);
  });

  it('cesar-hello: writes "HELLO, CESAR!" into the display area', () => {
    const cpu = createCesarCpu();
    cpu.memory.data.set(CesarHelloSample.bytes);
    runUntilHalted(cpu, () => cesarStep(cpu));
    expect(cpu.registers.flags.halted).toBe(true);
    const msg = 'HELLO, CESAR!';
    const actual: string[] = [];
    for (let i = 0; i < msg.length; i++) {
      actual.push(String.fromCharCode(cpu.memory.data[CesarDisplayMemoryAddress + i] ?? 0));
    }
    expect(actual.join('')).toBe(msg);
  });
});
