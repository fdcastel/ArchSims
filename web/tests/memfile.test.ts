import { describe, expect, it } from 'vitest';
import {
  memFileSignature,
  memFileSize,
  readMemFile,
  writeMemFile,
} from '@/core/memfile';

const headerBytes = (sig: string): number[] => [0x03, sig.charCodeAt(0), sig.charCodeAt(1), sig.charCodeAt(2)];

describe('memfile', () => {
  it('signatures and sizes match the F# CLI format', () => {
    expect(memFileSignature('neander')).toBe('NDR');
    expect(memFileSignature('ahmes')).toBe('NDR');
    expect(memFileSignature('ramses')).toBe('RMS');
    expect(memFileSignature('cesar')).toBe('C16');

    expect(memFileSize('neander')).toBe(256);
    expect(memFileSize('ahmes')).toBe(256);
    expect(memFileSize('ramses')).toBe(256);
    expect(memFileSize('cesar')).toBe(65536);
  });

  it('writeMemFile prefixes 256-byte payloads with 0x03 + RMS for Ramses', () => {
    const data = new Uint8Array(256);
    data[0] = 0xab;
    data[255] = 0xcd;
    const file = writeMemFile('ramses', data);
    expect(file.length).toBe(260);
    expect(Array.from(file.subarray(0, 4))).toEqual(headerBytes('RMS'));
    expect(file[4]).toBe(0xab);
    expect(file[259]).toBe(0xcd);
  });

  it('writeMemFile prefixes 65536-byte payloads with 0x03 + C16 for Cesar', () => {
    const data = new Uint8Array(65536);
    data[0] = 0x12;
    data[65535] = 0x34;
    const file = writeMemFile('cesar', data);
    expect(file.length).toBe(65540);
    expect(Array.from(file.subarray(0, 4))).toEqual(headerBytes('C16'));
    expect(file[4]).toBe(0x12);
    expect(file[65539]).toBe(0x34);
  });

  it('writeMemFile uses the stub NDR header for Neander/Ahmes', () => {
    const data = new Uint8Array(256);
    expect(Array.from(writeMemFile('neander', data).subarray(0, 4))).toEqual(headerBytes('NDR'));
    expect(Array.from(writeMemFile('ahmes', data).subarray(0, 4))).toEqual(headerBytes('NDR'));
  });

  it('writeMemFile rejects payloads of the wrong length', () => {
    expect(() => writeMemFile('ramses', new Uint8Array(255))).toThrowError(/esperado 256 bytes/);
    expect(() => writeMemFile('cesar', new Uint8Array(256))).toThrowError(/esperado 65536 bytes/);
  });

  it('readMemFile round-trips writeMemFile output', () => {
    const data = new Uint8Array(256);
    for (let i = 0; i < 256; i++) data[i] = (i * 7) & 0xff;
    const file = writeMemFile('ramses', data);
    const decoded = readMemFile('ramses', file);
    expect(decoded).toEqual(data);
  });

  it('readMemFile round-trips full 64 KiB Cesar memory', () => {
    const data = new Uint8Array(65536);
    for (let i = 0; i < 65536; i++) data[i] = (i ^ (i >>> 8)) & 0xff;
    const file = writeMemFile('cesar', data);
    const decoded = readMemFile('cesar', file);
    expect(decoded).toEqual(data);
    expect(decoded).not.toBe(data);
  });

  it('readMemFile rejects wrong total length', () => {
    expect(() => readMemFile('ramses', new Uint8Array(259))).toThrowError(/esperado 260 bytes/);
  });

  it('readMemFile rejects wrong prefix byte', () => {
    const file = writeMemFile('ramses', new Uint8Array(256));
    file[0] = 0x05;
    expect(() => readMemFile('ramses', file)).toThrowError(/prefixo esperado 0x03/);
  });

  it('readMemFile rejects mismatched signature', () => {
    const file = writeMemFile('ramses', new Uint8Array(256));
    expect(() => readMemFile('neander', file)).toThrowError(/assinatura esperada "NDR"/);
  });
});
