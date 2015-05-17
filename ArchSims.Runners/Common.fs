namespace Ufrgs.Inf.ArchSims.Runners

open System
open System.Text.RegularExpressions

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

    type CommonOptions = {
        FileName: string
        ExecutionMode: ExecutionMode
        OutputFormat: OutputFormat
        Speed: int option
    }

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



    // Source: http://www.fssnip.net/8g

    // The snippet shows a parser for command-line arguments supporting value lists for single commands. 

    // Calling with the following arguments: "Arg 1" "Arg 2" -test "Case 1" "Case 2" -show -skip "tag" 
    // produces the following map: map [("", seq ["Arg 1"; "Arg 2"]); ("show", seq []); ("skip", seq ["tag"]);("test", seq ["Case 1"; "Case 2"])] 
    // which can be used to find what data have been sent along with different commands. 
    
    // Calling with the following: "Arg 1" "Arg 2" /test="Case 1" "Case 2" --show /skip:tag produces the same result.

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
