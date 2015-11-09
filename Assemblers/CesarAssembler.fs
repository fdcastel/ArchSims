namespace Ufrgs.Inf.ArchSims.Assemblers

open System.Collections.Generic

open FParsec

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Cesar

module CesarAssembler =

    type sByte = int16          // uint8 expanded to accept negative values
    type sWord = int32          // uint16 expanded to accept negative values

    type BranchOperand = 
    | BranchLiteral of sByte
    | BranchLabel of string

    type Operand = 
    | RegisterMode of Register * AddressMode
    | RegisterModeIndex of Register * AddressMode * sWord
    | Direct of sWord
    | Immediate of sWord
    | ReferenceLabel of string

    type InstructionType =
    | Simple of Instruction
    | ChangeFlags of Instruction * Flag list
    | BranchOnly of Instruction * BranchOperand
    | JumpOnly of Instruction * Operand
    | RegisterAndBranch of Instruction * Register * BranchOperand
    | RegisterAndJump of Instruction * Register * Operand
    | RegisterOnly of Instruction * Register
    | OneOperand of Instruction * Operand
    | TwoOperands of Instruction * Operand * Operand

    type CommandType =
    | At of uint16
    | Label of string
    | Constant of sWord
    | Assemble of InstructionType
      
    let pRegister = stringCIReturn "R0" Register.R0 <|>
                    stringCIReturn "R1" Register.R1 <|>
                    stringCIReturn "R2" Register.R2 <|> 
                    stringCIReturn "R3" Register.R3 <|> 
                    stringCIReturn "R4" Register.R4 <|> 
                    stringCIReturn "R5" Register.R5 <|> 
                    stringCIReturn "R6" Register.R6 <|> 
                    stringCIReturn "R7" Register.R7

    let pFlag = stringCIReturn "N" Flag.Negative <|>
                stringCIReturn "Z" Flag.Zero <|>
                stringCIReturn "V" Flag.Overflow <|>
                stringCIReturn "C" Flag.Carry

    let pFlags = many pFlag

    let pLabel = skipString ":" >>. many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)

    let pBranchOperand = attempt (pLabel |>> (fun l -> BranchLabel l)) <|>
                                 (pint16 |>> (fun a -> BranchLiteral a))

    let pOperand = attempt (pLabel                                                                            |>> (fun l     -> ReferenceLabel l)) <|>                                      // :Label
                   attempt (skipString "((" >>. pRegister .>> skipString ")+)"                                |>> (fun r     -> RegisterMode (r, AddressMode.RegPostIncIndirect))) <|>      // ((R7)+)
                   attempt (skipString "(-(" >>. pRegister .>> skipString "))"                                |>> (fun r     -> RegisterMode (r, AddressMode.RegPreDecIndirect))) <|>       // (-(R7))
                   attempt (skipString "(" >>. pint32 .>>. (skipString "(" >>. pRegister .>> skipString "))") |>> (fun (i,r) -> RegisterModeIndex (r, AddressMode.IndexedIndirect, i))) <|> // (2(R7))
                   attempt (skipString "(" >>. pRegister .>> skipString ")+"                                  |>> (fun r     -> RegisterMode (r, AddressMode.RegPostInc))) <|>              // (R7)+
                   attempt (skipString "-(" >>. pRegister .>> skipString ")"                                  |>> (fun r     -> RegisterMode (r, AddressMode.RegPreDec))) <|>               // -(R7)
                   attempt (pint32 .>>. (skipString "(" >>. pRegister .>> skipString ")")                     |>> (fun (i,r) -> RegisterModeIndex (r, AddressMode.Indexed, i))) <|>         // 2(R7)
                   attempt (skipString "(" >>. pRegister .>> skipString ")"                                   |>> (fun r     -> RegisterMode (r, AddressMode.RegisterIndirect))) <|>        // (R7)
                   attempt (pRegister                                                                         |>> (fun r     -> RegisterMode (r, AddressMode.Register))) <|>                // R7
                   attempt (skipString "#" >>. pint32                                                         |>> (fun c     -> Immediate c)) <|>                                           // #100
                           (pint32                                                                            |>> (fun a     -> Direct a))                                                  // 100

    let pInstructionSimple = stringCIReturn "NOP" Instruction.Nop <|>
                             stringCIReturn "HLT" Instruction.Hlt

    let pInstructionChangeFlags = stringCIReturn "CCC" Instruction.Ccc <|>
                                  stringCIReturn "SCC" Instruction.Scc

    let pInstructionBranchOnly = stringCIReturn "BR"  Instruction.Br <|>
                                 stringCIReturn "BNE" Instruction.Bne <|>
                                 stringCIReturn "BEQ" Instruction.Beq <|>
                                 stringCIReturn "BPL" Instruction.Bpl <|>
                                 stringCIReturn "BMI" Instruction.Bmi <|>
                                 stringCIReturn "BVC" Instruction.Bvc <|>
                                 stringCIReturn "BVS" Instruction.Bvs <|>
                                 stringCIReturn "BCC" Instruction.Bcc <|>
                                 stringCIReturn "BCS" Instruction.Bcs <|>
                                 stringCIReturn "BGE" Instruction.Bge <|>
                                 stringCIReturn "BLT" Instruction.Blt <|>
                                 stringCIReturn "BGT" Instruction.Bgt <|>
                                 stringCIReturn "BLE" Instruction.Ble <|>
                                 stringCIReturn "BHI" Instruction.Bhi <|>
                                 stringCIReturn "BLS" Instruction.Bls
    
    let pInstructionJumpOnly = stringCIReturn "JMP" Instruction.Jmp

    let pInstructionRegisterAndBranch = stringCIReturn "SOB" Instruction.Sob

    let pInstructionRegisterAndJump = stringCIReturn "JSR" Instruction.Jsr

    let pInstructionRegisterOnly = stringCIReturn "RTS" Instruction.Rts

    let pInstructionOneOperand = stringCIReturn "CLR" Instruction.Clr <|>
                                 stringCIReturn "NOT" Instruction.Not <|>
                                 stringCIReturn "INC" Instruction.Inc <|>
                                 stringCIReturn "DEC" Instruction.Dec <|>
                                 stringCIReturn "NEG" Instruction.Neg <|>
                                 stringCIReturn "TST" Instruction.Tst <|>
                                 stringCIReturn "ROR" Instruction.Ror <|>
                                 stringCIReturn "ROL" Instruction.Rol <|>
                                 stringCIReturn "ASR" Instruction.Asr <|>
                                 stringCIReturn "ASL" Instruction.Asl <|>
                                 stringCIReturn "ADC" Instruction.Adc <|>
                                 stringCIReturn "SBC" Instruction.Sbc

    let pInstructionTwoOperands = stringCIReturn "MOV" Instruction.Mov <|>
                                  stringCIReturn "ADD" Instruction.Add <|>
                                  stringCIReturn "SUB" Instruction.Sub <|>
                                  stringCIReturn "CMP" Instruction.Cmp <|>
                                  stringCIReturn "AND" Instruction.And <|>
                                  stringCIReturn "OR " Instruction.Or

    let pComment = pstring ";" >>. skipRestOfLine true
    let pSpacing = skipSepBy spaces pComment

    let pRegister_ = pRegister .>> pSpacing
    let pFlags_ = pFlags .>> pSpacing
    let pBranchOperand_ = pBranchOperand .>> pSpacing
    let pOperand_ = pOperand .>> pSpacing

    let pInstructionSimple_ = pInstructionSimple .>> pSpacing
    let pInstructionChangeFlags_ = pInstructionChangeFlags .>> pSpacing
    let pInstructionBranchOnly_ = pInstructionBranchOnly .>> pSpacing
    let pInstructionJumpOnly_ = pInstructionJumpOnly .>> pSpacing
    let pInstructionRegisterAndBranch_ = pInstructionRegisterAndBranch .>> pSpacing
    let pInstructionRegisterAndJump_ = pInstructionRegisterAndJump .>> pSpacing
    let pInstructionRegisterOnly_ = pInstructionRegisterOnly .>> pSpacing
    let pInstructionOneOperand_ = pInstructionOneOperand .>> pSpacing
    let pInstructionTwoOperands_ = pInstructionTwoOperands .>> pSpacing

    let pSep_ = skipString "," .>> spaces

    let pAt_ = pstring "@" >>. puint16 .>> pSpacing
    let pLabel_ = pLabel .>> pSpacing
    let pConstant_ = pint32 .>> pSpacing
    let pInstruction_ = attempt (pipe4 pInstructionTwoOperands_ pOperand_ pSep_ pOperand_              (fun i s _ t -> TwoOperands (i, s, t))) <|>
                        attempt (pipe2 pInstructionOneOperand_ pOperand_                               (fun i o     -> OneOperand (i, o))) <|>
                        attempt (pipe2 pInstructionRegisterOnly_ pRegister_                            (fun i r     -> RegisterOnly (i, r))) <|>
                        attempt (pipe4 pInstructionRegisterAndJump_ pRegister_ pSep_ pOperand_         (fun i r _ o -> RegisterAndJump (i, r, o))) <|>
                        attempt (pipe4 pInstructionRegisterAndBranch_ pRegister_ pSep_ pBranchOperand_ (fun i r _ o -> RegisterAndBranch (i, r, o))) <|>
                        attempt (pipe2 pInstructionJumpOnly_ pOperand_                                 (fun i o     -> JumpOnly (i, o))) <|>
                        attempt (pipe2 pInstructionBranchOnly_ pBranchOperand_                         (fun i o     -> BranchOnly (i, o))) <|>
                        attempt (pipe2 pInstructionChangeFlags_ pFlags_                                (fun i f     -> ChangeFlags (i, f))) <|>
                        attempt (      pInstructionSimple_ |>>                                         (fun i       -> Simple i))

    let pCommand = (pInstruction_ |>> (fun i -> Assemble i)) <|>
                   (pAt_          |>> (fun a -> At a)) <|>
                   (pLabel_       |>> (fun l -> Label l)) <|>
                   (pConstant_    |>> (fun c -> Constant c))

    let pProgram = pSpacing >>. many1 pCommand .>> eof

    let EncodeInstruction instructionType getReferenceFunc =

        let decodeBranchOperand branchOperand isSob =
            match branchOperand with
            | BranchLiteral b -> byte b
            | BranchLabel l -> byte (getReferenceFunc l 1uy isSob)

        let decodeOperand operand labelOffset =
            match operand with
            | RegisterMode (r, m) -> r, m, None
            | RegisterModeIndex (r, m, ind) -> r, m, Some (uint16 ind)
            | Direct a -> Register.R7, AddressMode.RegPostIncIndirect, Some (uint16 a)
            | Immediate c -> Register.R7, AddressMode.RegPostInc, Some (uint16 c)
            | ReferenceLabel l -> Register.R7, AddressMode.RegPostIncIndirect, Some (getReferenceFunc l labelOffset false)

        match instructionType with
        | InstructionType.Simple i -> [byte i]

        | InstructionType.ChangeFlags (i, f) -> 
            let empty: Flag = LanguagePrimitives.EnumOfValue 0uy
            let flags = match f with
                        | [] -> empty
                        | _ -> (List.reduce (fun r x -> r ||| x) f)
            [byte i ||| byte flags]

        | InstructionType.BranchOnly (i, o) -> 
            [byte i; decodeBranchOperand o false]

        | InstructionType.JumpOnly (i, o) -> 
            let r, m, a = decodeOperand o 2uy
            let result = EncodeInstructionOneOperand i m r
            match a with
            | Some v -> [byte (result >>> 8); byte result; byte (v >>> 8); byte v]
            | None -> [byte (result >>> 8); byte result]
            
        | InstructionType.RegisterAndBranch (i, r, o) ->
            [byte i ||| byte r; decodeBranchOperand o true]

        | InstructionType.RegisterAndJump (i, rj, o) ->
            let r, m, a = decodeOperand o 2uy
            let result = EncodeInstructionOneOperand i m r
            match a with
            | Some v -> [byte (result >>> 8) ||| byte rj; byte result; byte (v >>> 8); byte v]
            | None -> [byte (result >>> 8) ||| byte rj; byte result]

        | InstructionType.RegisterOnly (i, r) -> 
            [byte i ||| byte r]

        | InstructionType.OneOperand (i, o) ->
            let r, m, a = decodeOperand o 2uy
            let result = EncodeInstructionOneOperand i m r
            match a with
            | Some v -> [byte (result >>> 8); byte result; byte (v >>> 8); byte v]
            | None -> [byte (result >>> 8); byte result]

        | InstructionType.TwoOperands (i, s, t) ->
            let sr, sm, sa = decodeOperand s 2uy
            let tr, tm, ta = decodeOperand t (if sa.IsSome then 4uy else 2uy)
            let result = EncodeInstructionTwoOperand i sm sr tm tr
            match sa with
            | Some sav -> 
                match ta with
                | Some tav -> [byte (result >>> 8); byte result; byte (sav >>> 8); byte sav; byte (tav >>> 8); byte tav]
                | None -> [byte (result >>> 8); byte result; byte (sav >>> 8); byte sav]
            | None -> 
                match ta with
                | Some tav -> [byte (result >>> 8); byte result; byte (tav >>> 8); byte tav]
                | None -> [byte (result >>> 8); byte result]            
        
    let AssembleInstruction input =
        match run pInstruction_ input with
        | Success(result, _, _)   -> EncodeInstruction result (fun l _ _ -> 0us)
        | Failure(errorMsg, _, _) -> failwith errorMsg

    type DeferredAddress = uint16 * byte * bool // Label address, its relative position (offset) within the instruction, isSob

    let AssembleProgram cpu input =
        let mutable address = 0
        let labels = new Dictionary<string, uint16>()
        let deferredLabels = new Dictionary<string, List<DeferredAddress>>()

        let getLabelAddress label offset isSob =
            match labels.TryGetValue(label) with
            | true, labelValue -> 
                if offset = 1uy then                    // Offset = 1: Branch instruction.
                    let delta = (int labelValue - (address + 2)) * (if isSob then -1 else 1)
                    if delta < -128 || delta > 127 then
                        failwith (sprintf "Label inacessível a partir de um branch: %s" label)
                    uint16 delta
                else
                    labelValue
            | _ ->
                // Label reference used before its definition. Defer this address... (*)                
                let newValue = (uint16 address, offset, isSob)
                match deferredLabels.TryGetValue(label) with
                | true, deferredValues -> deferredValues.Add(newValue)
                | _ -> 
                    let deferredValues = new List<DeferredAddress>()
                    deferredValues.Add(newValue)
                    deferredLabels.Add(label, deferredValues)
                0us

        let setLabelAddress label value =
            match labels.TryGetValue(label) with
            | true, _ -> failwith (sprintf "Label duplicado: %s" label)
            | _ ->
                labels.Add(label, value)
                match deferredLabels.TryGetValue(label) with
                | true, deferredValues -> 
                    // (*) ...and fix it now, when we have the value
                    for (a, o, is) in deferredValues do
                        if o = 1uy then                    // Offset = 1: Branch instruction.
                            let delta = (int value - (int a + 2)) * (if is then -1 else 1)
                            if delta < -128 || delta > 127 then
                                failwith (sprintf "Label inacessível a partir de um branch: %s" label)
                            cpu.Memory.Data.[int a + int o] <- byte delta
                        else
                            cpu.Memory.Data.[int a + int o] <- byte (value >>> 8)
                            cpu.Memory.Data.[int a + int o + 1] <- byte value
                    deferredLabels.Remove(label) |> ignore
                | _ -> ()

        let assembleCommand command = 
            match command with
            | CommandType.At a ->
                address <- int a

            | CommandType.Label l ->
                setLabelAddress l (uint16 address)

            | CommandType.Constant c -> 
                if c >= 0 && c <= 0xFF then
                    cpu.Memory.Data.[address] <- byte c
                    address <- address + 1
                else
                    cpu.Memory.Data.[address] <- byte (c >>> 8)
                    cpu.Memory.Data.[address + 1] <- byte c
                    address <- address + 2

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
