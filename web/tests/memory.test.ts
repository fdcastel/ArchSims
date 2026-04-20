import { describe, expect, it } from 'vitest';
import {
  createMemory,
  memoryLoad,
  memoryReadByte,
  memoryReadWordBigEndian,
  memoryReset,
  memoryWriteByte,
  memoryWriteWordBigEndian,
} from '@/core/memory';

describe('memory', () => {
  it('starts with zeroed data and zero counters', () => {
    const m = createMemory(256);
    expect(m.data.length).toBe(256);
    expect(m.readCount).toBe(0);
    expect(m.writeCount).toBe(0);
    expect(Array.from(m.data).every((b) => b === 0)).toBe(true);
  });

  it('counts byte reads and writes independently', () => {
    const m = createMemory(16);
    memoryWriteByte(m, 3, 0xab);
    expect(memoryReadByte(m, 3)).toBe(0xab);
    expect(m.readCount).toBe(1);
    expect(m.writeCount).toBe(1);
  });

  it('truncates writes to a byte', () => {
    const m = createMemory(16);
    memoryWriteByte(m, 0, 0x1ff);
    expect(m.data[0]).toBe(0xff);
  });

  it('reads and writes 16-bit big-endian words', () => {
    const m = createMemory(16);
    memoryWriteWordBigEndian(m, 4, 0xbeef);
    expect(m.data[4]).toBe(0xbe);
    expect(m.data[5]).toBe(0xef);
    expect(memoryReadWordBigEndian(m, 4)).toBe(0xbeef);
    expect(m.readCount).toBe(2);
    expect(m.writeCount).toBe(2);
  });

  it('loads a byte sequence at an address', () => {
    const m = createMemory(16);
    memoryLoad(m, 2, [0x01, 0x02, 0x03]);
    expect(Array.from(m.data.slice(2, 5))).toEqual([0x01, 0x02, 0x03]);
    expect(m.writeCount).toBe(0); // load bypasses counters, matches F# Array.blit
  });

  it('reset zeroes data and counters', () => {
    const m = createMemory(8);
    memoryWriteByte(m, 0, 0xff);
    memoryReadByte(m, 0);
    memoryReset(m);
    expect(m.readCount).toBe(0);
    expect(m.writeCount).toBe(0);
    expect(Array.from(m.data).every((b) => b === 0)).toBe(true);
  });
});
