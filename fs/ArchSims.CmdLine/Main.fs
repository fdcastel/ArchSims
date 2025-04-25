namespace Ufrgs.Inf.ArchSims.CmdLine

open System
open System.IO

open Ufrgs.Inf.ArchSims.CmdLine.Common

module Main =

    [<EntryPoint>]
    let Main args =
        try
            let pArgs = args |> ParseArguments

            let exeName = Path.GetFileNameWithoutExtension(Environment.GetCommandLineArgs().[0])
            let cpuFromExe = DecodeCpuType (exeName)

            let showHelp = pArgs ? ("h")
            let sourceArgument = pArgs ? ("")

            if showHelp.IsSome || sourceArgument.IsNone then
                printfn "Usage: %s <source> %s" exeName (if cpuFromExe.IsNone then "-Cpu <cpu> " else "")
                printfn "         [ -Run [-Mode <mode>] [-Output <output>] [-Speed <speed>] ]"
                printfn "         [ -Save <target> ]"
                printfn ""

                printfn "    <source>     Source file name (.txt)            (required)"
                if cpuFromExe.IsNone then 
                    printfn "    <cpu>        Neander, Ahmes, Ramses, Cesar      (required)"

                printfn ""
                printfn "  -Run           Run program (default)"
                printfn "    <mode>       LastStep, AllSteps, Interactive    (default: Interactive)"
                printfn "    <output>     Binary, Decimal, Hexadecimal       (default: Decimal)"
                printfn "    <speed>      instructions per second            (default: 10, only for Interactive)"
                printfn ""
                printfn "  -Save          Save memory to file"
                printfn "    <target>     Target file name (.mem)            (required)"
                exit 1

            let sourceFileName = Seq.head sourceArgument.Value

            let cpuFromArgs = match pArgs ? cpu with
                              | Some c -> DecodeCpuType (Seq.head c)
                              | None -> None

            let cpuType = match cpuFromExe with
                              | Some c -> Some c
                              | None -> cpuFromArgs

            let runOptions = {
                SourceFileName = sourceFileName

                ExecutionMode = match pArgs ? mode with
                                | Some m -> DecodeExecutionMode (Seq.head m)
                                | None -> ExecutionMode.Interactive

                OutputFormat = match pArgs ? output with
                                | Some f -> DecodeOutputFormat (Seq.head f)
                                | None -> OutputFormat.Decimal 

                Speed = match pArgs ? speed with
                                | Some s -> Some (Int32.Parse (Seq.head s))
                                | None -> None
            }

            let saveOptions targetFileName = {
                SourceFileName = sourceFileName; 
                TargetFileName = targetFileName
            }

            let action = match pArgs ? save with
                         | Some file -> Save (saveOptions (Seq.head file))
                         | None -> Run runOptions

            match cpuType with
            | Some CpuType.Neander -> failwith "Not implemented: Neander"
            | Some CpuType.Ahmes   -> failwith "Not implemented: Ahmes"
            | Some CpuType.Ramses  -> RamsesCmdLine.Execute action
            | Some CpuType.Cesar   -> CesarCmdLine.Execute action
            | None -> failwith "You must inform a -Cpu option."

            exit 0
        with
        | Failure message -> 
            printfn "Error: %s" message
            exit 1
        | :? System.IO.FileNotFoundException as ex ->
            printfn "File not found: %s" ex.FileName
            exit 1
