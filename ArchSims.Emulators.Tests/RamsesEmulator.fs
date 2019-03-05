namespace Ufrgs.Inf.ArchSims.Emulators

open System.IO
open System.Reflection

open Ufrgs.Inf.ArchSims.Core
open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Assemblers.Cesar

module RamsesEmulator =

    // Memory addresses
    let memRa         = 256us + 1us
    let memRb         = 258us + 1us
    let memRx         = 260us + 1us
    let memPc         = 262us + 1us
    
    let memIrOpCode   = 264us + 1us
    let memIrOperand  = 266us + 1us
    
    let memHalted     = 268us + 1us
    let memNegative   = 270us + 1us
    let memZero       = 272us + 1us
    let memCarry      = 274us + 1us

    let procStep       = 300us
    let procRun        = 400us
    let procReset      = 500us

    let emulatorSource =
        let assembly = Assembly.GetExecutingAssembly();
        let resourceName = "ArchSims.Emulators.Tests.RamsesEmulator.Cesar.txt";
        use stream = assembly.GetManifestResourceStream(resourceName)
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    // ---
    type FlagsWrapper(cpu:Cesar.Cpu) =
        member this.Halted
           with get() = (cpu.Memory.Data.[int memHalted] = 1uy)
           and set value = cpu.Memory.Data.[int memHalted] <- if value then 1uy else 0uy

        member this.Negative
           with get() = (cpu.Memory.Data.[int memNegative] = 1uy)
           and set value = cpu.Memory.Data.[int memNegative] <- if value then 1uy else 0uy

        member this.Zero
           with get() = (cpu.Memory.Data.[int memZero] = 1uy)
           and set value = cpu.Memory.Data.[int memZero] <- if value then 1uy else 0uy

        member this.Carry
           with get() = (cpu.Memory.Data.[int memCarry] = 1uy)
           and set value = cpu.Memory.Data.[int memCarry] <- if value then 1uy else 0uy

    type InstructionRegisterWrapper(cpu:Cesar.Cpu) =
        member this.OpCode
           with get() = cpu.Memory.Data.[int memIrOpCode]

        member this.OperandAddress
           with get() = cpu.Memory.Data.[int memIrOperand]

    type RegistersWrapper(cpu:Cesar.Cpu) =
        let flags = new FlagsWrapper(cpu)
        let instructionRegister = new InstructionRegisterWrapper(cpu)

        member this.Ra
           with get() = cpu.Memory.Data.[int memRa]
           and set value = cpu.Memory.Data.[int memRa] <- value

        member this.Rb
           with get() = cpu.Memory.Data.[int memRb]
           and set value = cpu.Memory.Data.[int memRb] <- value

        member this.Rx
           with get() = cpu.Memory.Data.[int memRx]
           and set value = cpu.Memory.Data.[int memRx] <- value

        member this.ProgramCounter
           with get() = cpu.Memory.Data.[int memPc]
           and set value = cpu.Memory.Data.[int memPc] <- value

        member this.InstructionRegister
           with get() = instructionRegister

        member this.Flags
           with get() = flags

    type CpuWrapper() =
        let host = Cesar.CreateCpu()
        let registers = RegistersWrapper(host)

        do
            CesarAssembler.AssembleProgram host emulatorSource

        member this.Registers
           with get() =
              registers
        
        member this.Memory
           with get() =
              host.Memory
        
        member this.Host
           with get() =
              host
        
        member this.Step() = 
            host.Registers.R.[7] <- procStep
            let mutable halted = false
            while not halted do
                Cesar.Step host
                halted <- host.Registers.Flags.Halted
            registers.Flags.Halted

        member this.Reset() = 
            host.Registers.R.[7] <- procReset
            let mutable halted = false
            while not halted do
                Cesar.Step host
                halted <- host.Registers.Flags.Halted

    let CreateCpu() = 
        new CpuWrapper()

    let Step (cpu:CpuWrapper) =
        cpu.Step() |> ignore

    let Reset (cpu:CpuWrapper) =
        cpu.Reset()
