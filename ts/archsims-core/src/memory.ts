export interface Memory {
    data: Uint8Array;
    readCount: number;
    writeCount: number;
}

export function createMemory(length: number): Memory {
    return {
        data: new Uint8Array(length),
        readCount: 0,
        writeCount: 0,
    };
}

export function memoryReset(memory: Memory): void {
    memory.data.fill(0);
    memory.readCount = 0;
    memory.writeCount = 0;
}

export function memoryReadByte(memory: Memory, address: number): number {
    if (address < 0 || address >= memory.data.length) {
        // Consider how to handle out-of-bounds access. Throw error? Return 0?
        // For now, mimicking potential F# array behavior (exception)
        throw new Error(`Memory read out of bounds at address ${address}`);
    }
    memory.readCount++; // Correct: Only increment readCount
    // Ensure the value returned is treated as an unsigned 8-bit integer
    return memory.data[address];
}

export function memoryWriteByte(memory: Memory, address: number, value: number): void {
    if (address < 0 || address >= memory.data.length) {
        throw new Error(`Memory write out of bounds at address ${address}`);
    }
    memory.writeCount++; // Correct: Only increment writeCount
    // Ensure only the lower 8 bits are stored
    memory.data[address] = value & 0xFF;
}

export function memoryLoad(memory: Memory, address: number, values: number[]): void {
    // Ensure values are bytes before loading
    const bytesToLoad = values.map(v => v & 0xFF);
    if (address < 0 || address + bytesToLoad.length > memory.data.length) {
        throw new Error(`Memory load out of bounds starting at address ${address} for ${bytesToLoad.length} bytes`);
    }
    memory.data.set(bytesToLoad, address);
}

// Corresponds to F# MemoryReadWordBigEndian
export function memoryReadWordBigEndian(memory: Memory, address: number): number {
    // Check bounds for reading two bytes
    if (address < 0 || address + 1 >= memory.data.length) {
        throw new Error(`Memory word read out of bounds at address ${address}`);
    }
    const hi = memoryReadByte(memory, address); // Reads byte at address, increments readCount
    const lo = memoryReadByte(memory, address + 1); // Reads byte at address + 1, increments readCount
    return (hi << 8) | lo;
}

// Corresponds to F# MemoryWriteWordBigEndian
export function memoryWriteWordBigEndian(memory: Memory, address: number, value: number): void {
    // Check bounds for writing two bytes
    if (address < 0 || address + 1 >= memory.data.length) {
        throw new Error(`Memory word write out of bounds at address ${address}`);
    }
    const hi = (value >>> 8) & 0xFF; // Extract high byte
    const lo = value & 0xFF;         // Extract low byte
    memoryWriteByte(memory, address, hi);     // Writes high byte, increments writeCount
    memoryWriteByte(memory, address + 1, lo); // Writes low byte, increments writeCount
}
