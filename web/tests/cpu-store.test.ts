import { describe, expect, it } from 'vitest';
import { get } from 'svelte/store';
import {
  NeanderInstruction,
  createNeanderCpu,
  neanderReset,
  neanderStep,
  type NeanderCpu,
} from '../src/core/neander';
import { memoryWriteByte } from '../src/core/memory';
import { createCpuStore } from '../src/stores/cpu';

function makeStore() {
  const cpu = createNeanderCpu();
  return createCpuStore<NeanderCpu>({
    cpu,
    step: neanderStep,
    reset: neanderReset,
  });
}

describe('cpuStore', () => {
  it('starts at tick 0 and exposes the live cpu ref', () => {
    const store = makeStore();
    const snap = get(store);
    expect(snap.tick).toBe(0);
    expect(snap.cpu).toBe(store.cpu);
    expect(snap.cpu.registers.programCounter).toBe(0);
  });

  it('step() advances the cpu and bumps the tick', () => {
    const store = makeStore();
    memoryWriteByte(store.cpu.memory, 0, NeanderInstruction.Hlt);

    store.step();

    expect(store.tick).toBe(1);
    expect(store.cpu.registers.flags.halted).toBe(true);
    const snap = get(store);
    expect(snap.tick).toBe(1);
    expect(snap.cpu).toBe(store.cpu);
  });

  it('emits a fresh snapshot to subscribers on every step', () => {
    const store = makeStore();
    memoryWriteByte(store.cpu.memory, 0, NeanderInstruction.Nop);
    memoryWriteByte(store.cpu.memory, 1, NeanderInstruction.Hlt);

    const ticks: number[] = [];
    const unsub = store.subscribe((s) => ticks.push(s.tick));
    store.step();
    store.step();
    unsub();

    expect(ticks).toEqual([0, 1, 2]);
  });

  it('reset() zeroes the cpu and the tick', () => {
    const store = makeStore();
    memoryWriteByte(store.cpu.memory, 0, NeanderInstruction.Hlt);
    store.step();
    expect(store.tick).toBe(1);
    expect(store.cpu.registers.flags.halted).toBe(true);

    store.reset();

    expect(store.tick).toBe(0);
    expect(store.cpu.registers.programCounter).toBe(0);
    expect(store.cpu.registers.flags.halted).toBe(false);
    expect(get(store).tick).toBe(0);
  });

  it('preserves the same cpu reference across step/reset (in-place mutation)', () => {
    const store = makeStore();
    const original = store.cpu;
    store.step();
    store.reset();
    expect(store.cpu).toBe(original);
    expect(get(store).cpu).toBe(original);
  });
});
