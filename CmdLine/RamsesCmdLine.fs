namespace Ufrgs.Inf.ArchSims.CmdLine

open System
open System.IO

open Ufrgs.Inf.ArchSims.Core.Debugger
open Ufrgs.Inf.ArchSims.Core.Ramses
open Ufrgs.Inf.ArchSims.Assemblers.Ramses.RamsesAssembler
open Ufrgs.Inf.ArchSims.CmdLine.Common

module RamsesCmdLine =

    let Run options =
        let printCpu cpu = 
            let printFlags character value = 
                if value then character else "-"

            let flags = printFlags "N" cpu.Registers.Flags.Negative +
                        printFlags "Z" cpu.Registers.Flags.Zero +
                        printFlags "C" cpu.Registers.Flags.Carry
        
            let ir = DisassembleInstruction [cpu.Registers.InstructionRegister.OpCode; cpu.Registers.InstructionRegister.OperandAddress]

            let toBinary (value:byte) =
                Int32.Parse(Convert.ToString(value, 2))

            if (options.ExecutionMode <> ExecutionMode.LastStep) || (cpu.Registers.Flags.Halted) then
                match options.OutputFormat with
                | OutputFormat.Decimal ->     printf "Ra:%3u Rb:%3u Rx:%3u PC:%3u IR: [%-12s] %s" cpu.Registers.Ra cpu.Registers.Rb cpu.Registers.Rx cpu.Registers.ProgramCounter ir flags
                | OutputFormat.Hexadecimal -> printf "Ra:%2X Rb:%2X Rx:%2X PC:%2X IR: [%-12s] %s" cpu.Registers.Ra cpu.Registers.Rb cpu.Registers.Rx cpu.Registers.ProgramCounter ir flags
                | OutputFormat.Binary ->      printf "Ra:%08u Rb:%08u Rx:%08u PC:%08u IR: [%-12s] %s" (toBinary cpu.Registers.Ra) (toBinary cpu.Registers.Rb) (toBinary cpu.Registers.Rx) (toBinary cpu.Registers.ProgramCounter) ir flags
                
                if (options.ExecutionMode = ExecutionMode.Interactive) && (not cpu.Registers.Flags.Halted) then
                    Console.SetCursorPosition(0, Console.CursorTop)
                    match options.Speed with
                    | Some s when s > 0 && s < 1000 -> Async.RunSynchronously (async { do! Async.Sleep (int (1000 / s)) })
                    | _ -> ()
                else
                    printfn ""

        let cpu = CreateCpu()
        let debugger = CreateDebugger (fun () -> int cpu.Registers.ProgramCounter) 
                                      (fun () -> Step cpu
                                                 printCpu cpu
                                                 cpu.Registers.Flags.Halted)

        AssembleProgram cpu (File.ReadAllText options.SourceFileName)
        printCpu cpu
        DebuggerRun debugger 1000
        let stopReason = match debugger.LastStop with
                         | DebuggerStopReason.Breakpoint     -> "Breakpoint hit"
                         | DebuggerStopReason.Halted         -> "Program halted"
                         | DebuggerStopReason.RunningForever -> "Running forever..."
                         | _ -> ""

        printfn ""
        printfn "Finished: %s" stopReason

    let Save options =
        let cpu = CreateCpu()
        AssembleProgram cpu (File.ReadAllText options.SourceFileName)
        MemorySaveToFile cpu.Memory [0x03uy; 0x52uy; 0x4Duy; 0x53uy] (* ^CRMS *) options.TargetFileName

    let Execute action =
        match action with
        | Run options -> Run options
        | Save options -> Save options

