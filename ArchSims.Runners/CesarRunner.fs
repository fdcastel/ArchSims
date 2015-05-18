namespace Ufrgs.Inf.ArchSims.Runners

open System
open System.IO

open Ufrgs.Inf.ArchSims.Core.Debugger
open Ufrgs.Inf.ArchSims.Core.Cesar
open Ufrgs.Inf.ArchSims.Assemblers.CesarAssembler
open Ufrgs.Inf.ArchSims.Runners.Common

module CesarRunner =

    let Run options =
        let printCpu cpu = 
            let printFlags character value = 
                if value then character else "-"

            let flags = printFlags "N" cpu.Registers.Flags.Negative +
                        printFlags "Z" cpu.Registers.Flags.Zero +
                        printFlags "V" cpu.Registers.Flags.Overflow +
                        printFlags "C" cpu.Registers.Flags.Carry
        
            let ir = DisassembleInstruction (List.ofArray cpu.Registers.InstructionRegister.Data)

            let toBinary (value) =
                Int64.Parse(Convert.ToString(int value, 2))

            if (options.ExecutionMode <> ExecutionMode.LastStep) || (cpu.Registers.Flags.Halted) then
                let r0 = cpu.Registers.R.[0]
                let r1 = cpu.Registers.R.[1]
                let r2 = cpu.Registers.R.[2]
                let r3 = cpu.Registers.R.[3]
                let r4 = cpu.Registers.R.[4]
                let r5 = cpu.Registers.R.[5]
                let r6 = cpu.Registers.R.[6]
                let r7 = cpu.Registers.R.[7]
                match options.OutputFormat with
                | OutputFormat.Decimal ->     printf "R0:%5u R1:%5u R2:%5u R3:%5u R4:%5u R5:%5u R6:%5u PC:%5u IR: [%-28s] %s" r0 r1 r2 r3 r4 r5 r6 r7 ir flags
                | OutputFormat.Hexadecimal -> printf "R0:%4X R1:%4X R2:%4X R3:%4X R4:%4X R5:%4X R6:%4X PC:%4X IR: [%-28s] %s" r0 r1 r2 r3 r4 r5 r6 r7 ir flags
                | OutputFormat.Binary ->      printf "R0:%016u R1:%016u R2:%016u R3:%016u\nR4:%016u R5:%016u R6:%016u PC:%016u\nIR: [%-28s] %s" (toBinary r0) (toBinary r1) (toBinary r2) (toBinary r3) (toBinary r4) (toBinary r5) (toBinary r6) (toBinary r7) ir flags
                
                if (options.ExecutionMode = ExecutionMode.Interactive) && (not cpu.Registers.Flags.Halted) then
                    match options.OutputFormat with
                    | OutputFormat.Binary -> Console.SetCursorPosition(0, Console.CursorTop - 2)
                    | _                   -> Console.SetCursorPosition(0, Console.CursorTop)
                    
                    match options.Speed with
                    | Some s when s > 0 && s < 1000 -> Async.RunSynchronously (async { do! Async.Sleep (int (1000 / s)) })
                    | _ -> ()
                else
                    printfn ""

        let cpu = CreateCpu()
        let debugger = CreateDebugger (fun () -> int cpu.Registers.R.[7]) 
                                      (fun () -> Step cpu
                                                 printCpu cpu
                                                 cpu.Registers.Flags.Halted)

        AssembleProgram cpu (File.ReadAllText options.FileName)
        printCpu cpu
        DebuggerRun debugger 1000
        let stopReason = match debugger.LastStop with
                         | DebuggerStopReason.Breakpoint     -> "Breakpoint hit"
                         | DebuggerStopReason.Halted         -> "Program halted"
                         | DebuggerStopReason.RunningForever -> "Running forever..."
                         | _ -> ""

        printfn "Finished: %s" stopReason

