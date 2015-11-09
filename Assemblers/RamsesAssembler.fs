namespace Ufrgs.Inf.ArchSims.Assemblers

open System.Collections.Generic

open FParsec

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Ramses

module RamsesAssembler =

    type Operand = 
    | Literal of byte * AddressMode
    | ReferenceLabel of string * AddressMode

    type InstructionType =
    | Simple of Instruction
    | RegisterOnly of Instruction * Register
    | OperandOnly of Instruction * Operand
    | RegisterAndOperand of Instruction * Register * Operand

    type CommandType =
    | At of byte
    | Label of string
    | Constant of byte
    | Assemble of InstructionType
      
    let pRegister = stringCIReturn "A" Register.Ra <|> 
                    stringCIReturn "B" Register.Rb <|> 
                    stringCIReturn "X" Register.Rx <|> 
                    stringCIReturn "PC" Register.Pc 

    let pLabel = skipString ":" >>. many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)

    let pOperand = attempt (pLabel .>> skipStringCI ",X" |>> (fun l -> ReferenceLabel (l, AddressMode.Indexed))) <|>
                   attempt (pLabel .>> skipStringCI ",I" |>> (fun l -> ReferenceLabel (l, AddressMode.Indirect))) <|>
                   attempt (skipStringCI "#" >>. pLabel  |>> (fun l -> ReferenceLabel (l, AddressMode.Immediate))) <|>
                   attempt (pLabel                       |>> (fun l -> ReferenceLabel (l, AddressMode.Direct))) <|>
                   attempt (puint8 .>> skipStringCI ",X" |>> (fun a -> Literal (a, AddressMode.Indexed))) <|>
                   attempt (puint8 .>> skipStringCI ",I" |>> (fun a -> Literal (a, AddressMode.Indirect))) <|>
                   attempt (skipStringCI "#" >>. puint8  |>> (fun a -> Literal (a, AddressMode.Immediate))) <|>
                           (puint8                       |>> (fun a -> Literal (a, AddressMode.Direct)))

    let pInstructionSimple = stringCIReturn "NOP" Instruction.Nop <|>
                             stringCIReturn "HLT" Instruction.Hlt

    let pInstructionRegisterOnly = stringCIReturn "NOT" Instruction.Not <|>
                                   stringCIReturn "NEG" Instruction.Neg <|>
                                   stringCIReturn "SHR" Instruction.Shr

    let pInstructionOperandOnly = stringCIReturn "JMP" Instruction.Jmp <|>
                                  stringCIReturn "JN"  Instruction.Jn  <|>
                                  stringCIReturn "JZ"  Instruction.Jz  <|>
                                  stringCIReturn "JC"  Instruction.Jc  <|>
                                  stringCIReturn "JSR" Instruction.Jsr

    let pInstructionRegisterAndOperand = stringCIReturn "STR" Instruction.Str <|>
                                         stringCIReturn "LDR" Instruction.Ldr <|>
                                         stringCIReturn "ADD" Instruction.Add <|>
                                         stringCIReturn "OR"  Instruction.Or  <|>
                                         stringCIReturn "AND" Instruction.And <|>
                                         stringCIReturn "SUB" Instruction.Sub

    let pComment = pstring ";" >>. skipRestOfLine true
    let pSpacing = skipSepBy spaces pComment

    let pRegister_ = pRegister .>> pSpacing
    let pOperand_ = pOperand .>> pSpacing
    let pInstructionSimple_ = pInstructionSimple .>> pSpacing
    let pInstructionRegisterOnly_ = pInstructionRegisterOnly .>> pSpacing
    let pInstructionOperandOnly_ = pInstructionOperandOnly .>> pSpacing
    let pInstructionRegisterAndOperand_ = pInstructionRegisterAndOperand .>> pSpacing

    let pAt_ = pstring "@" >>. puint8 .>> pSpacing
    let pLabel_ = pLabel .>> pSpacing
    let pConstant_ = puint8 .>> pSpacing
    let pInstruction_ = attempt (pipe3 pInstructionRegisterAndOperand_ pRegister_ pOperand_ (fun i r o -> RegisterAndOperand (i, r, o))) <|>
                        attempt (pipe2 pInstructionOperandOnly_ pOperand_                   (fun i o   -> OperandOnly (i, o))) <|>
                        attempt (pipe2 pInstructionRegisterOnly_ pRegister_                 (fun i r   -> RegisterOnly (i, r))) <|>
                        attempt (      pInstructionSimple_ |>>                              (fun i     -> Simple i))

    let pCommand = (pInstruction_ |>> (fun i -> Assemble i)) <|>
                   (pAt_          |>> (fun a -> At a)) <|>
                   (pLabel_       |>> (fun l -> Label l)) <|>
                   (pConstant_    |>> (fun c -> Constant c))

    let pProgram = pSpacing >>. many1 pCommand .>> eof
                            
    let EncodeInstruction instructionType getReferenceFunc =
        match instructionType with
        | InstructionType.Simple i -> [byte i]
        | InstructionType.RegisterOnly (i, r) -> [byte i ||| byte r]
        | InstructionType.OperandOnly (i, o) -> 
            match o with 
            | Operand.Literal (a, m) -> [byte i ||| byte m; a]
            | Operand.ReferenceLabel (l, m) -> [byte i ||| byte m; getReferenceFunc l 1uy]
        | InstructionType.RegisterAndOperand (i, r, o) -> 
            match o with 
            | Operand.Literal (a, m) -> [byte i ||| byte r ||| byte m; a]
            | Operand.ReferenceLabel (l, m) -> [byte i ||| byte r ||| byte m; getReferenceFunc l 1uy]
        
    let AssembleInstruction input =
        match run pInstruction_ input with
        | Success(result, _, _)   -> EncodeInstruction result (fun l _ -> 0uy)
        | Failure(errorMsg, _, _) -> failwith errorMsg

    type DeferredAddress = byte * byte

    let AssembleProgram cpu input =
        let mutable address = 0
        let labels = new Dictionary<string, byte>()
        let deferredLabels = new Dictionary<string, List<DeferredAddress>>()

        let getLabelAddress label offset =
            match labels.TryGetValue(label) with
            | true, labelValue -> labelValue
            | _ ->
                // Label reference used before its definition. Defer this address... (*)                
                let newValue = (byte address, offset)
                match deferredLabels.TryGetValue(label) with
                | true, deferredValues -> deferredValues.Add(newValue)
                | _ -> 
                    let deferredValues = new List<DeferredAddress>()
                    deferredValues.Add(newValue)
                    deferredLabels.Add(label, deferredValues)
                0uy

        let setLabelAddress label value =
            match labels.TryGetValue(label) with
            | true, _ -> failwith "Label duplicado: %s" label
            | _ ->
                labels.Add(label, value)
                match deferredLabels.TryGetValue(label) with
                | true, deferredValues -> 
                    // (*) ...and fix it now, when we have the value
                    for (a, o) in deferredValues do
                        cpu.Memory.Data.[int a + int o] <- byte value
                    deferredLabels.Remove(label) |> ignore
                | _ -> ()

        let assembleCommand command = 
            match command with
            | CommandType.At a ->
                address <- int a

            | CommandType.Label l ->
                setLabelAddress l (byte address)

            | CommandType.Constant c -> 
                cpu.Memory.Data.[address] <- c
                address <- address + 1

            | CommandType.Assemble i -> 
                let bytes = EncodeInstruction i getLabelAddress
                MemoryLoad cpu.Memory address bytes
                address <- address + bytes.Length
            
        match run pProgram input with
        | Success(commands, _, _) -> List.map assembleCommand commands |> ignore
        | Failure(errorMsg, _, _) -> failwith errorMsg

        match deferredLabels.Count with
        | 0 -> ()
        | 1 -> failwith (sprintf "Label indefinido: %s" (Seq.head deferredLabels.Keys))
        | _ -> failwith (sprintf "Labels indefinidos: %A" (Seq.toList deferredLabels.Keys))
