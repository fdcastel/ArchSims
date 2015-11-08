namespace Ufrgs.Inf.ArchSims.CmdLine

open System
open System.IO
open System.Text.RegularExpressions

open Ufrgs.Inf.ArchSims.Core.Memory

module Common =

    type CpuType = 
    | Neander
    | Ahmes
    | Ramses
    | Cesar

    type ExecutionMode = 
    | LastStep
    | AllSteps
    | Interactive

    type OutputFormat =
    | Binary
    | Decimal
    | Hexadecimal

    type RunOptions = {
        SourceFileName: string

        ExecutionMode: ExecutionMode
        OutputFormat: OutputFormat
        Speed: int option
    }

    type SaveOptions = {
        SourceFileName: string
        TargetFileName: string
    }

    type Action =
    | Run of RunOptions
    | Save of SaveOptions


    // Utility functions

    let (|IgnoreCase|_|) s1 s2 = 
        if String.Compare(s1, s2, StringComparison.OrdinalIgnoreCase) = 0 then
            Some()
        else
            None

    let DecodeCpuType string =
        match string with
        | IgnoreCase "N"
        | IgnoreCase "Neander" -> Some CpuType.Neander
        | IgnoreCase "A"
        | IgnoreCase "Ahmes" -> Some CpuType.Ahmes
        | IgnoreCase "R"
        | IgnoreCase "Ramses" -> Some CpuType.Ramses
        | IgnoreCase "C"
        | IgnoreCase "Cesar" -> Some CpuType.Cesar
        | _ -> None        

    let DecodeExecutionMode string =
        match string with
        | IgnoreCase "L"
        | IgnoreCase "LastStep" -> ExecutionMode.LastStep
        | IgnoreCase "A"
        | IgnoreCase "AllSteps" -> ExecutionMode.AllSteps
        | IgnoreCase "I"
        | IgnoreCase "Interactive"
        | _ -> ExecutionMode.Interactive

    let DecodeOutputFormat string =
        match string with
        | IgnoreCase "B"
        | IgnoreCase "Binary" -> OutputFormat.Binary
        | IgnoreCase "H"
        | IgnoreCase "Hexadecimal" -> OutputFormat.Hexadecimal
        | IgnoreCase "D"
        | IgnoreCase "Decimal"
        | _ -> OutputFormat.Decimal

    let MemorySaveToFile memory (prefix: byte list) fileName =
        use file = File.Open(fileName, FileMode.Create)
        use writer = new BinaryWriter(file);
        writer.Write(List.toArray prefix);
        writer.Write(memory.Data);



    // Simple command-line argument parser -- Source: http://www.fssnip.net/8g

    let (|Command|_|) (s:string) =
        let r = new Regex(@"^(?:-{1,2}|\/)(?<command>\w+)[=:]*(?<value>.*)$",RegexOptions.IgnoreCase)
        let m = r.Match(s)
        if m.Success then 
            Some(m.Groups.["command"].Value.ToLower(), m.Groups.["value"].Value)
        else
            None

    let ParseArguments (args:string seq) =
        args
        |> Seq.map (fun i -> match i with
                             | Command (n,v) -> (n,v) // command
                             | _ -> ("",i))           // data
        |> Seq.scan (fun (sn,_) (n,v) -> if n.Length>0 then (n,v) else (sn,v)) ("","")
        |> Seq.skip 1
        |> Seq.groupBy (fun (n,_) -> n)
        |> Seq.map (fun (n,s) -> (n, s |> Seq.map (fun (_,v) -> v) |> Seq.filter (fun i -> i.Length>0)))
        |> Map.ofSeq

    let (?) map key = 
        if Map.containsKey key map then 
            Some(map.[key])
        else
            None
