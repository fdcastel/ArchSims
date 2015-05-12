namespace Ufrgs.Inf.ArchSims

open System.Collections.Generic

module Common =

    exception CpuRunningForeverException



    type Debugger = {
        mutable InstructionCount: int
        mutable Halted: bool
        mutable BreakpointHit: bool
        Breakpoints: HashSet<int>
    }

    let CreateDebugger() = {
        InstructionCount = 0
        Halted = false
        BreakpointHit = false
        Breakpoints = new HashSet<int>()
    }

    let ClearDebuggerFlags debugger =
        debugger.BreakpointHit <- false
        debugger.Halted <- false

    let ClearDebugger debugger =
        debugger.InstructionCount <- 0
        debugger.Breakpoints.Clear()
        ClearDebuggerFlags debugger



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
        
    let ClearMemory memory =
        Array.fill memory.Data 0 memory.Data.Length 0uy
        memory.ReadCount <- 0
        memory.WriteCount <- 0
        
    let ReadByte memory address =
        memory.ReadCount <- memory.ReadCount + 1
        memory.Data.[address]

    let WriteByte memory address value =
        memory.WriteCount <- memory.WriteCount + 1
        memory.Data.[int address] <- value; 

    let ReadWordBigEndian memory address =
        let hi = uint16 (ReadByte memory address) <<< 8
        let lo = uint16 (ReadByte memory (address + 1))
        hi ||| lo

    let WriteWordBigEndian memory address (value: uint16) =
        WriteByte memory address (byte (value >>> 8))
        WriteByte memory (address + 1) (byte value)
