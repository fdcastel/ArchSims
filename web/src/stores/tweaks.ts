import { writable, type Writable } from 'svelte/store';

export type Palette = 'amber' | 'green' | 'paper';
export type Base = 'hex' | 'dec' | 'bin';
export type Density = 'comfortable' | 'compact';

export interface Tweaks {
  palette: Palette;
  base: Base;
  density: Density;
  showAnnotations: boolean;
  /** Show fetch/decode/execute/flags sub-step animator (P4-06). */
  showFetchCycle: boolean;
}

export type Machine = 'neander' | 'ahmes' | 'ramses' | 'cesar';

export const TWEAKS_DEFAULTS: Tweaks = {
  palette: 'green',
  base: 'hex',
  density: 'compact',
  showAnnotations: true,
  showFetchCycle: false,
};

export const tweaksStorageKey = (machine: Machine): string => `${machine}.tweaks`;

function readFromStorage(machine: Machine): Tweaks | null {
  if (typeof localStorage === 'undefined') return null;
  try {
    const raw = localStorage.getItem(tweaksStorageKey(machine));
    if (!raw) return null;
    const parsed = JSON.parse(raw) as Record<string, unknown>;
    const result = { ...TWEAKS_DEFAULTS };
    for (const key of Object.keys(TWEAKS_DEFAULTS) as (keyof Tweaks)[]) {
      if (key in parsed) (result as Record<string, unknown>)[key] = parsed[key];
    }
    return result;
  } catch {
    return null;
  }
}

function writeToStorage(machine: Machine, value: Tweaks): void {
  if (typeof localStorage === 'undefined') return;
  try {
    localStorage.setItem(tweaksStorageKey(machine), JSON.stringify(value));
  } catch {
    // storage disabled / quota exceeded — silently drop the persist
  }
}

export interface TweaksStore extends Writable<Tweaks> {
  patch(partial: Partial<Tweaks>): void;
  reset(): void;
}

export function createTweaksStore(machine: Machine): TweaksStore {
  const initial = readFromStorage(machine) ?? { ...TWEAKS_DEFAULTS };
  const inner = writable<Tweaks>(initial);

  inner.subscribe((value) => writeToStorage(machine, value));

  return {
    subscribe: inner.subscribe,
    set: inner.set,
    update: inner.update,
    patch(partial) {
      inner.update((cur) => ({ ...cur, ...partial }));
    },
    reset() {
      inner.set({ ...TWEAKS_DEFAULTS });
    },
  };
}
