import { writable, type Readable } from 'svelte/store';

export interface CpuBinding<C> {
  cpu: C;
  step(cpu: C): void;
  reset(cpu: C): void;
}

export interface CpuSnapshot<C> {
  cpu: C;
  tick: number;
}

export interface CpuStore<C> extends Readable<CpuSnapshot<C>> {
  step(): void;
  reset(): void;
  readonly cpu: C;
  readonly tick: number;
}

export function createCpuStore<C>(binding: CpuBinding<C>): CpuStore<C> {
  const { cpu, step: doStep, reset: doReset } = binding;
  let tick = 0;
  const { subscribe, set } = writable<CpuSnapshot<C>>({ cpu, tick });

  function emit(): void {
    set({ cpu, tick });
  }

  return {
    subscribe,
    step() {
      doStep(cpu);
      tick += 1;
      emit();
    },
    reset() {
      doReset(cpu);
      tick = 0;
      emit();
    },
    get cpu() {
      return cpu;
    },
    get tick() {
      return tick;
    },
  };
}
