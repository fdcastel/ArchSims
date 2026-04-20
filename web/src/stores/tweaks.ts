import { writable, type Writable } from 'svelte/store';

export type Palette = 'amber' | 'green' | 'paper';
export type Base = 'hex' | 'dec' | 'bin';
export type Density = 'comfortable' | 'compact';
export type Frame = 'desktop' | 'mobile';

export interface Tweaks {
  palette: Palette;
  base: Base;
  density: Density;
  showAnnotations: boolean;
  frame: Frame;
}

export type Machine = 'neander' | 'ahmes' | 'ramses' | 'cesar';

export const TWEAKS_DEFAULTS: Tweaks = {
  palette: 'green',
  base: 'hex',
  density: 'compact',
  showAnnotations: true,
  frame: 'desktop',
};

export const tweaksStorageKey = (machine: Machine): string => `${machine}.tweaks`;

function readFromStorage(machine: Machine): Tweaks | null {
  if (typeof localStorage === 'undefined') return null;
  try {
    const raw = localStorage.getItem(tweaksStorageKey(machine));
    if (!raw) return null;
    const parsed = JSON.parse(raw) as Partial<Tweaks>;
    return { ...TWEAKS_DEFAULTS, ...parsed };
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
