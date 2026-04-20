export interface Memory {
  data: Uint8Array;
  readCount: number;
  writeCount: number;
}

export function createMemory(length: number): Memory {
  return { data: new Uint8Array(length), readCount: 0, writeCount: 0 };
}

export function memoryReset(memory: Memory): void {
  memory.data.fill(0);
  memory.readCount = 0;
  memory.writeCount = 0;
}

export function memoryReadByte(memory: Memory, address: number): number {
  memory.readCount++;
  return memory.data[address] ?? 0;
}

export function memoryWriteByte(memory: Memory, address: number, value: number): void {
  memory.writeCount++;
  memory.data[address] = value;
}

export function memoryReadWordBigEndian(memory: Memory, address: number): number {
  const hi = memoryReadByte(memory, address) << 8;
  const lo = memoryReadByte(memory, address + 1);
  return (hi | lo) & 0xffff;
}

export function memoryWriteWordBigEndian(memory: Memory, address: number, value: number): void {
  memoryWriteByte(memory, address, (value >>> 8) & 0xff);
  memoryWriteByte(memory, address + 1, value & 0xff);
}

export function memoryLoad(memory: Memory, address: number, bytes: ArrayLike<number>): void {
  for (let i = 0; i < bytes.length; i++) {
    memory.data[address + i] = bytes[i] ?? 0;
  }
}
