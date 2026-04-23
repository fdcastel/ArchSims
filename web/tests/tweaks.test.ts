import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { get } from 'svelte/store';
import {
  createTweaksStore,
  tweaksStorageKey,
  TWEAKS_DEFAULTS,
  type Palette,
  type Tweaks,
} from '../src/stores/tweaks';

interface StorageLike {
  getItem(key: string): string | null;
  setItem(key: string, value: string): void;
  removeItem(key: string): void;
  clear(): void;
}

function makeMemoryStorage(): StorageLike {
  const map = new Map<string, string>();
  return {
    getItem: (k) => (map.has(k) ? (map.get(k) as string) : null),
    setItem: (k, v) => void map.set(k, String(v)),
    removeItem: (k) => void map.delete(k),
    clear: () => map.clear(),
  };
}

describe('tweaksStore', () => {
  let storage: StorageLike;

  beforeEach(() => {
    storage = makeMemoryStorage();
    (globalThis as { localStorage?: StorageLike }).localStorage = storage;
  });

  afterEach(() => {
    delete (globalThis as { localStorage?: StorageLike }).localStorage;
  });

  it('initializes with defaults when nothing is persisted', () => {
    const store = createTweaksStore('ramses');
    expect(get(store)).toEqual(TWEAKS_DEFAULTS);
  });

  it('persists the initial defaults to localStorage on creation', () => {
    createTweaksStore('ramses');
    expect(storage.getItem('ramses.tweaks')).toBe(JSON.stringify(TWEAKS_DEFAULTS));
  });

  it('uses the per-machine storage key', () => {
    expect(tweaksStorageKey('neander')).toBe('neander.tweaks');
    expect(tweaksStorageKey('ahmes')).toBe('ahmes.tweaks');
    expect(tweaksStorageKey('ramses')).toBe('ramses.tweaks');
    expect(tweaksStorageKey('cesar')).toBe('cesar.tweaks');
  });

  it('restores a fully persisted value', () => {
    const persisted: Tweaks = {
      palette: 'paper',
      base: 'bin',
      density: 'comfortable',
      showAnnotations: false,
      showFetchCycle: true,
    };
    storage.setItem('cesar.tweaks', JSON.stringify(persisted));

    const store = createTweaksStore('cesar');
    expect(get(store)).toEqual(persisted);
  });

  it('merges partial persisted values over defaults (forward compatible)', () => {
    storage.setItem('ahmes.tweaks', JSON.stringify({ palette: 'amber', base: 'bin' }));
    const store = createTweaksStore('ahmes');
    expect(get(store)).toEqual({
      ...TWEAKS_DEFAULTS,
      palette: 'amber',
      base: 'bin',
    });
  });

  it('silently drops unknown persisted keys (removed fields survive rollout)', () => {
    storage.setItem('ramses.tweaks', JSON.stringify({ palette: 'amber', frame: 'mobile' }));
    const store = createTweaksStore('ramses');
    expect(get(store)).toEqual({ ...TWEAKS_DEFAULTS, palette: 'amber' });
    expect((get(store) as unknown as { frame?: unknown }).frame).toBeUndefined();
  });

  it('falls back to defaults when the persisted value is malformed', () => {
    storage.setItem('ramses.tweaks', '{not valid json');
    const store = createTweaksStore('ramses');
    expect(get(store)).toEqual(TWEAKS_DEFAULTS);
  });

  it('.patch merges a subset and persists', () => {
    const store = createTweaksStore('ramses');
    store.patch({ palette: 'amber', base: 'bin' });
    expect(get(store)).toEqual({
      ...TWEAKS_DEFAULTS,
      palette: 'amber',
      base: 'bin',
    });
    expect(JSON.parse(storage.getItem('ramses.tweaks') ?? '{}')).toEqual({
      ...TWEAKS_DEFAULTS,
      palette: 'amber',
      base: 'bin',
    });
  });

  it('.reset restores defaults and persists them', () => {
    const store = createTweaksStore('cesar');
    store.patch({ palette: 'paper', showAnnotations: false });
    store.reset();
    expect(get(store)).toEqual(TWEAKS_DEFAULTS);
    expect(JSON.parse(storage.getItem('cesar.tweaks') ?? '{}')).toEqual(TWEAKS_DEFAULTS);
  });

  it('.set replaces the full value and persists', () => {
    const store = createTweaksStore('neander');
    const next: Tweaks = {
      palette: 'amber',
      base: 'dec',
      density: 'compact',
      showAnnotations: false,
      showFetchCycle: false,
    };
    store.set(next);
    expect(get(store)).toEqual(next);
    expect(JSON.parse(storage.getItem('neander.tweaks') ?? '{}')).toEqual(next);
  });

  it('subscribers are notified on every change', () => {
    const store = createTweaksStore('ramses');
    const seen: Palette[] = [];
    const unsub = store.subscribe((t) => seen.push(t.palette));
    store.patch({ palette: 'paper' });
    store.patch({ palette: 'amber' });
    unsub();
    expect(seen).toEqual([TWEAKS_DEFAULTS.palette, 'paper', 'amber']);
  });

  it('each machine key is isolated', () => {
    const neander = createTweaksStore('neander');
    const cesar = createTweaksStore('cesar');
    neander.patch({ palette: 'amber' });
    cesar.patch({ palette: 'paper' });
    expect(get(neander).palette).toBe('amber');
    expect(get(cesar).palette).toBe('paper');
    expect(JSON.parse(storage.getItem('neander.tweaks') ?? '{}').palette).toBe('amber');
    expect(JSON.parse(storage.getItem('cesar.tweaks') ?? '{}').palette).toBe('paper');
  });

  it('falls back to defaults without a localStorage global (SSR-safe)', () => {
    delete (globalThis as { localStorage?: StorageLike }).localStorage;
    const store = createTweaksStore('ramses');
    expect(get(store)).toEqual(TWEAKS_DEFAULTS);
    store.patch({ palette: 'paper' });
    expect(get(store).palette).toBe('paper');
  });
});
