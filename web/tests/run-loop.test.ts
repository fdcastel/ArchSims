import { describe, expect, it } from 'vitest';
import { createRunLoop } from '../src/ui/panels/run-loop';

function makeFakeTimers() {
  type IntervalEntry = { cb: () => void; ms: number; id: number };
  type RafEntry = { cb: (ts: number) => void; id: number };
  let idSeq = 1;
  const intervals = new Map<number, IntervalEntry>();
  const rafs = new Map<number, RafEntry>();
  const api = {
    setInterval: (cb: () => void, ms: number) => {
      const id = idSeq++;
      intervals.set(id, { cb, ms, id });
      return id;
    },
    clearInterval: (handle: unknown) => {
      intervals.delete(handle as number);
    },
    requestAnimationFrame: (cb: (ts: number) => void) => {
      const id = idSeq++;
      rafs.set(id, { cb, id });
      return id;
    },
    cancelAnimationFrame: (handle: unknown) => {
      rafs.delete(handle as number);
    },
    tickInterval(id: number, times = 1): void {
      for (let i = 0; i < times; i += 1) {
        const entry = intervals.get(id);
        if (!entry) return;
        entry.cb();
      }
    },
    tickRaf(times = 1): void {
      for (let i = 0; i < times; i += 1) {
        const entry = [...rafs.values()][0];
        if (!entry) return;
        rafs.delete(entry.id);
        entry.cb(0);
      }
    },
    get intervals() {
      return intervals;
    },
    get rafs() {
      return rafs;
    },
  };
  return api;
}

describe('createRunLoop', () => {
  it('uses setInterval when speed > 0, stepping on each tick', () => {
    const timers = makeFakeTimers();
    let steps = 0;
    const loop = createRunLoop({
      step: () => (steps += 1),
      setInterval: timers.setInterval,
      clearInterval: timers.clearInterval,
      requestAnimationFrame: timers.requestAnimationFrame,
      cancelAnimationFrame: timers.cancelAnimationFrame,
    });
    loop.start(10);
    expect(loop.running).toBe(true);
    expect(timers.intervals.size).toBe(1);
    const id = [...timers.intervals.keys()][0] ?? 0;
    timers.tickInterval(id, 3);
    expect(steps).toBe(3);
    loop.stop();
    expect(loop.running).toBe(false);
    expect(timers.intervals.size).toBe(0);
  });

  it('computes ms from hz', () => {
    const timers = makeFakeTimers();
    const loop = createRunLoop({
      step: () => {},
      setInterval: timers.setInterval,
      clearInterval: timers.clearInterval,
      requestAnimationFrame: timers.requestAnimationFrame,
      cancelAnimationFrame: timers.cancelAnimationFrame,
    });
    loop.start(100);
    const entry = [...timers.intervals.values()][0];
    expect(entry?.ms).toBe(10);
    loop.stop();
  });

  it('uses requestAnimationFrame when speed is 0 (MAX)', () => {
    const timers = makeFakeTimers();
    let steps = 0;
    const loop = createRunLoop({
      step: () => (steps += 1),
      setInterval: timers.setInterval,
      clearInterval: timers.clearInterval,
      requestAnimationFrame: timers.requestAnimationFrame,
      cancelAnimationFrame: timers.cancelAnimationFrame,
      maxStepsPerFrame: 5,
    });
    loop.start(0);
    expect(timers.rafs.size).toBe(1);
    timers.tickRaf(1);
    expect(steps).toBe(5);
    expect(timers.rafs.size).toBe(1);
    loop.stop();
    expect(timers.rafs.size).toBe(0);
  });

  it('auto-stops when shouldBreak() returns true', () => {
    const timers = makeFakeTimers();
    let steps = 0;
    let halted = false;
    const loop = createRunLoop({
      step: () => {
        steps += 1;
        if (steps === 2) halted = true;
      },
      shouldBreak: () => halted,
      setInterval: timers.setInterval,
      clearInterval: timers.clearInterval,
      requestAnimationFrame: timers.requestAnimationFrame,
      cancelAnimationFrame: timers.cancelAnimationFrame,
    });
    loop.start(10);
    const id = [...timers.intervals.keys()][0] ?? 0;
    timers.tickInterval(id, 5);
    expect(steps).toBe(2);
    expect(loop.running).toBe(false);
  });

  it('restart replaces the prior handle', () => {
    const timers = makeFakeTimers();
    const loop = createRunLoop({
      step: () => {},
      setInterval: timers.setInterval,
      clearInterval: timers.clearInterval,
      requestAnimationFrame: timers.requestAnimationFrame,
      cancelAnimationFrame: timers.cancelAnimationFrame,
    });
    loop.start(10);
    loop.start(100);
    expect(timers.intervals.size).toBe(1);
    loop.stop();
  });
});
