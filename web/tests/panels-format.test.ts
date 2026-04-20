import { describe, expect, it } from 'vitest';
import {
  fmtBin16,
  fmtBin8,
  fmtHex1,
  fmtHex2,
  fmtHex4,
  formatByte,
  formatWord,
} from '../src/ui/panels/format';

describe('panels/format', () => {
  it('fmtHex1 renders a single hex nibble uppercased', () => {
    expect(fmtHex1(0)).toBe('0');
    expect(fmtHex1(0xa)).toBe('A');
    expect(fmtHex1(0xff)).toBe('F');
  });

  it('fmtHex2 pads to two hex digits', () => {
    expect(fmtHex2(0)).toBe('00');
    expect(fmtHex2(0xf)).toBe('0F');
    expect(fmtHex2(0xfe)).toBe('FE');
    expect(fmtHex2(0x1ff)).toBe('FF');
  });

  it('fmtHex4 pads to four hex digits', () => {
    expect(fmtHex4(0)).toBe('0000');
    expect(fmtHex4(0x12)).toBe('0012');
    expect(fmtHex4(0xdead)).toBe('DEAD');
    expect(fmtHex4(0x1ffff)).toBe('FFFF');
  });

  it('fmtBin8 pads to 8 bits', () => {
    expect(fmtBin8(0)).toBe('00000000');
    expect(fmtBin8(0b10101010)).toBe('10101010');
    expect(fmtBin8(0xff)).toBe('11111111');
  });

  it('fmtBin16 pads to 16 bits', () => {
    expect(fmtBin16(0)).toBe('0000000000000000');
    expect(fmtBin16(0x0001)).toBe('0000000000000001');
    expect(fmtBin16(0xff00)).toBe('1111111100000000');
  });

  it('formatByte respects the base setting', () => {
    expect(formatByte(42, 'hex')).toBe('2A');
    expect(formatByte(42, 'dec')).toBe('042');
    expect(formatByte(42, 'bin')).toBe('00101010');
  });

  it('formatWord respects the base setting', () => {
    expect(formatWord(0x1234, 'hex')).toBe('1234');
    expect(formatWord(0x1234, 'dec')).toBe('04660');
    expect(formatWord(0x1234, 'bin')).toBe('0001001000110100');
  });
});
