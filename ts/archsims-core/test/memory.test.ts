import { describe, it, expect, beforeEach } from 'vitest';
import {
    Memory,
    createMemory,
    memoryReset,
    memoryReadByte,
    memoryWriteByte,
    memoryLoad,
    memoryReadWordBigEndian,
    memoryWriteWordBigEndian
} from '../src/memory';

describe('Memory Operations', () => {
    let memory: Memory;

    beforeEach(() => {
        memory = createMemory(256); // Standard 256-byte memory for tests
    });

    it('should create memory initialized to zero', () => {
        expect(memory.data.length).toBe(256);
        expect(memory.data.every(byte => byte === 0)).toBe(true);
        expect(memory.readCount).toBe(0);
        expect(memory.writeCount).toBe(0);
    });

    it('should reset memory to zero and clear counts', () => {
        memoryWriteByte(memory, 10, 55);
        memoryReadByte(memory, 10);
        expect(memory.data[10]).toBe(55);
        expect(memory.readCount).toBe(1);
        expect(memory.writeCount).toBe(1);

        memoryReset(memory);

        expect(memory.data[10]).toBe(0);
        expect(memory.readCount).toBe(0);
        expect(memory.writeCount).toBe(0);
        expect(memory.data.every(byte => byte === 0)).toBe(true);
    });

    it('should read and write bytes correctly, incrementing counts', () => {
        memoryWriteByte(memory, 5, 123);
        expect(memory.data[5]).toBe(123);
        expect(memory.writeCount).toBe(1);
        expect(memory.readCount).toBe(0);

        const value = memoryReadByte(memory, 5);
        expect(value).toBe(123);
        expect(memory.readCount).toBe(1);
        expect(memory.writeCount).toBe(1);

        // Test writing value > 255 (should be masked)
        memoryWriteByte(memory, 6, 300); // 300 = 0x12C
        expect(memory.data[6]).toBe(0x2C); // Should store 44
        expect(memory.writeCount).toBe(2);
    });

    it('should throw error on out-of-bounds read/write', () => {
        expect(() => memoryReadByte(memory, 256)).toThrow('Memory read out of bounds');
        expect(() => memoryReadByte(memory, -1)).toThrow('Memory read out of bounds');
        expect(() => memoryWriteByte(memory, 256, 1)).toThrow('Memory write out of bounds');
        expect(() => memoryWriteByte(memory, -1, 1)).toThrow('Memory write out of bounds');
    });

    it('should load bytes correctly', () => {
        const dataToLoad = [10, 20, 30, 40];
        memoryLoad(memory, 100, dataToLoad);

        expect(memoryReadByte(memory, 100)).toBe(10);
        expect(memoryReadByte(memory, 101)).toBe(20);
        expect(memoryReadByte(memory, 102)).toBe(30);
        expect(memoryReadByte(memory, 103)).toBe(40);
        expect(memory.readCount).toBe(4);
        expect(memory.writeCount).toBe(0); // memoryLoad doesn't use memoryWriteByte
    });

    it('should throw error on out-of-bounds load', () => {
        const dataToLoad = [1, 2, 3, 4];
        expect(() => memoryLoad(memory, 254, dataToLoad)).toThrow('Memory load out of bounds');
        expect(() => memoryLoad(memory, -1, dataToLoad)).toThrow('Memory load out of bounds');
    });

    // --- Tests for new Word functions ---

    it('should write and read word big-endian correctly', () => {
        const valueToWrite = 0xABCD; // 43981
        memoryWriteWordBigEndian(memory, 50, valueToWrite);

        // Check underlying bytes
        expect(memory.data[50]).toBe(0xAB); // High byte first
        expect(memory.data[51]).toBe(0xCD); // Low byte second
        expect(memory.writeCount).toBe(2); // One call to writeWord calls writeByte twice
        expect(memory.readCount).toBe(0);

        const valueRead = memoryReadWordBigEndian(memory, 50);
        expect(valueRead).toBe(valueToWrite);
        expect(memory.readCount).toBe(2); // One call to readWord calls readByte twice
        expect(memory.writeCount).toBe(2);
    });

    it('should handle word write/read at memory boundaries', () => {
        // Write at start
        memoryWriteWordBigEndian(memory, 0, 0x1234);
        expect(memoryReadWordBigEndian(memory, 0)).toBe(0x1234);
        expect(memory.writeCount).toBe(2);
        expect(memory.readCount).toBe(2);

        // Write at end
        memoryWriteWordBigEndian(memory, 254, 0x5678);
        expect(memoryReadWordBigEndian(memory, 254)).toBe(0x5678);
        expect(memory.data[254]).toBe(0x56);
        expect(memory.data[255]).toBe(0x78);
        expect(memory.writeCount).toBe(4);
        expect(memory.readCount).toBe(4);
    });

    it('should throw error on out-of-bounds word read/write', () => {
        // Read attempts
        expect(() => memoryReadWordBigEndian(memory, 255)).toThrow('Memory word read out of bounds');
        expect(() => memoryReadWordBigEndian(memory, -1)).toThrow('Memory word read out of bounds');

        // Write attempts
        expect(() => memoryWriteWordBigEndian(memory, 255, 0x1122)).toThrow('Memory word write out of bounds');
        expect(() => memoryWriteWordBigEndian(memory, -1, 0x1122)).toThrow('Memory word write out of bounds');
    });
});
