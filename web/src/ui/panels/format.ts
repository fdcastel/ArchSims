import type { Base } from '../../stores/tweaks';

export function fmtHex(value: number, digits: number): string {
  return value.toString(16).toUpperCase().padStart(digits, '0');
}

export function fmtHex1(value: number): string {
  return fmtHex(value & 0xf, 1);
}

export function fmtHex2(value: number): string {
  return fmtHex(value & 0xff, 2);
}

export function fmtHex4(value: number): string {
  return fmtHex(value & 0xffff, 4);
}

export function fmtBin8(value: number): string {
  return (value & 0xff).toString(2).padStart(8, '0');
}

export function fmtBin16(value: number): string {
  return (value & 0xffff).toString(2).padStart(16, '0');
}

export function formatByte(value: number, base: Base): string {
  const v = value & 0xff;
  switch (base) {
    case 'hex':
      return fmtHex2(v);
    case 'dec':
      return String(v).padStart(3, '0');
    case 'bin':
      return fmtBin8(v);
  }
}

export function formatWord(value: number, base: Base): string {
  const v = value & 0xffff;
  switch (base) {
    case 'hex':
      return fmtHex4(v);
    case 'dec':
      return String(v).padStart(5, '0');
    case 'bin':
      return fmtBin16(v);
  }
}
