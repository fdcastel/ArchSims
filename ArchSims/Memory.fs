namespace Ufrgs.Inf.ArchSims

module Memory =

    type Memory = {
        Data: byte array
        mutable ReadCount: int
        mutable WriteCount: int
    }

    let CreateMemory length = {
        Data = Array.zeroCreate length
        ReadCount = 0
        WriteCount = 0
    }
        
    let MemoryReset memory =
        Array.fill memory.Data 0 memory.Data.Length 0uy
        memory.ReadCount <- 0
        memory.WriteCount <- 0
        
    let MemoryReadByte memory address =
        memory.ReadCount <- memory.ReadCount + 1
        memory.Data.[address]

    let MemoryWriteByte memory address value =
        memory.WriteCount <- memory.WriteCount + 1
        memory.Data.[int address] <- value; 

    let MemoryReadWordBigEndian memory address =
        let hi = uint16 (MemoryReadByte memory address) <<< 8
        let lo = uint16 (MemoryReadByte memory (address + 1))
        hi ||| lo

    let MemoryWriteWordBigEndian memory address (value: uint16) =
        MemoryWriteByte memory address (byte (value >>> 8))
        MemoryWriteByte memory (address + 1) (byte value)
