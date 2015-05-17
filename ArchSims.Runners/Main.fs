namespace Ufrgs.Inf.ArchSims.Runners

open System
open System.IO

open Ufrgs.Inf.ArchSims.Runners.Common
open Ufrgs.Inf.ArchSims.Runners.RamsesRunner

module Main =

    [<EntryPoint>]
    let Main args =
        try
            let pArgs = args |> ParseArguments

            let exeName = Environment.GetCommandLineArgs().[0]
            let cpuFromExe = DecodeCpuType (Path.GetFileNameWithoutExtension(exeName))

            let showHelp = pArgs ? ("h")
            let fileName = pArgs ? ("")

            if showHelp.IsSome || fileName.IsNone then
                printfn "Uso: %s <arquivo> %s" exeName (if cpuFromExe.IsNone then "-Cpu <Neander|Ahmes|Ramses|Cesar> " else "")
                printfn "     [-Mode <LastStep|AllSteps|Interactive>]   (default: Interactive)"
                printfn "     [-Output <Binary|Decimal|Hexadecimal>]    (default: Decimal)"
                printfn "     [-Speed <instruções-por-segundo>]         (default: 10, apenas para modo Interactive)"
                exit 1

            let cpuFromArgs = match pArgs ? cpu with
                              | Some c -> DecodeCpuType (Seq.head c)
                              | None -> None

            let cpuType = if cpuFromExe.IsSome then cpuFromExe else cpuFromArgs

            let options = {
                FileName = Seq.head fileName.Value

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

            match cpuType with
            | Some CpuType.Neander -> failwith "Não implementado: Neander"
            | Some CpuType.Ahmes   -> failwith "Não implementado: Ahmes"
            | Some CpuType.Ramses  -> Ufrgs.Inf.ArchSims.Runners.RamsesRunner.Run options
            | Some CpuType.Cesar   -> failwith "Não implementado: Cesar"
            | None -> failwith "Você deve informar uma cpu através da opção -Cpu."

            exit 0
        with
        | Failure message -> 
            printfn "Erro: %s" message
            exit 1
        | :? System.IO.FileNotFoundException as ex ->
            printfn "Arquivo não encontrado: %s" ex.FileName
            exit 1
