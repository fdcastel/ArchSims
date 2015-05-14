namespace Ufrgs.Inf.ArchSims

open FParsec
open Ufrgs.Inf.ArchSims.Ramses

module RamsesAssembler =

    type Operand = byte * AddressMode

    type InstructionType =
    | Simple of Instruction
    | RegisterOnly of Instruction * Register
    | OperandOnly of Instruction * Operand
    | RegisterAndOperand of Instruction * Register * Operand
        
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

    let pRegister = stringCIReturn "A" Register.Ra <|> 
                    stringCIReturn "B" Register.Rb <|> 
                    stringCIReturn "X" Register.Rx <|> 
                    stringCIReturn "PC" Register.Pc 

    let pOperand = attempt (puint8 .>> pstringCI ",X" |>> (fun a -> (a, AddressMode.Indexed))) <|>
                   attempt (puint8 .>> pstringCI ",I" |>> (fun a -> (a, AddressMode.Indirect))) <|>
                   attempt (pstringCI "#" >>. puint8  |>> (fun a -> (a, AddressMode.Immediate))) <|>                           
                           (puint8                    |>> (fun a -> (a, AddressMode.Direct)))

    let pRegister_ = pRegister .>> spaces
    let pInstructionSimple_ = pInstructionSimple .>> spaces
    let pInstructionRegisterOnly_ = pInstructionRegisterOnly .>> spaces
    let pInstructionOperandOnly_ = pInstructionOperandOnly .>> spaces
    let pInstructionRegisterAndOperand_ = pInstructionRegisterAndOperand .>> spaces

    let pAssemble = attempt (pipe3 pInstructionRegisterAndOperand_ pRegister_ pOperand (fun i r o -> RegisterAndOperand (i, r, o))) <|>
                    attempt (pipe2 pInstructionOperandOnly_ pOperand                   (fun i o -> OperandOnly (i, o))) <|>
                    attempt (pipe2 pInstructionRegisterOnly_ pRegister                 (fun i r -> RegisterOnly (i, r))) <|>
                            (      pInstructionSimple_ |>>                             (fun i -> Simple i))
                            
    let Assemble input =
        match run pAssemble input with
        | Success(result, _, _)   -> 
            match result with
            | InstructionType.Simple i -> [byte i]
            | InstructionType.RegisterOnly (i, r) -> [byte i ||| byte r]
            | InstructionType.OperandOnly (i, (address, addressMode)) -> [byte i ||| byte addressMode; address]
            | InstructionType.RegisterAndOperand (i, r, (address, addressMode)) -> [byte i ||| byte r ||| byte addressMode; address]
        | Failure(errorMsg, _, _) -> failwith errorMsg

    let Disassemble content =
        match content with
        | [] -> ""
        | head::tail -> 
            let firstOpCode = head

            let register() =
                let register = LanguagePrimitives.EnumOfValue (firstOpCode &&& RegisterMask)
                match register with
                | Register.Ra -> sprintf " A"
                | Register.Rb -> sprintf " B"
                | Register.Rx -> sprintf " X"
                | Register.Pc -> sprintf " PC"
                | _ -> failwith "Invalid Register"

            let operand() =
                let value = match tail with
                            | [] -> 0uy
                            | h::t -> h
                let addressMode = LanguagePrimitives.EnumOfValue (firstOpCode &&& AddressModeMask)
                match addressMode with
                | AddressMode.Direct    -> sprintf " %i" value
                | AddressMode.Indirect  -> sprintf " %i,I" value
                | AddressMode.Immediate -> sprintf " #%i" value
                | AddressMode.Indexed   -> sprintf " %i,X" value
                | _ -> failwith "Invalid AddressMode"

            let instruction = LanguagePrimitives.EnumOfValue (firstOpCode &&& InstructionMask)
            match instruction with
            | Instruction.Str -> "STR" + register() + operand()
            | Instruction.Ldr -> "LDR" + register() + operand()
            | Instruction.Add -> "ADD" + register() + operand()
            | Instruction.Or  -> "OR " + register() + operand()
            | Instruction.And -> "AND" + register() + operand()
            | Instruction.Not -> "NOT" + register()
            | Instruction.Sub -> "SUB" + register() + operand()
            | Instruction.Jmp -> "JMP" + operand()
            | Instruction.Jn  -> "JN " + operand()
            | Instruction.Jz  -> "JZ " + operand()
            | Instruction.Jc  -> "JC " + operand()
            | Instruction.Jsr -> "JSR" + operand()
            | Instruction.Neg -> "NEG" + register() 
            | Instruction.Shr -> "SHR" + register()
            | Instruction.Hlt -> "HLT"
            | Instruction.Nop
            | _ -> "NOP"
