namespace Ufrgs.Inf.ArchSims.Assemblers.Tests.Cesar

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Cesar
open Ufrgs.Inf.ArchSims.Assemblers.Cesar.CesarAssembler

open Ufrgs.Inf.ArchSims.Core.Tests.Cesar
open Ufrgs.Inf.ArchSims.Core.Tests.Utils

[<TestClass>]
type CesarAssemblerTests() = 
    inherit CesarTests()

    [<TestMethod>]
    member this.``CesarAssembler: AssembleInstruction works as expected``() =
        AssembleInstruction "NOP" |>== [byte Instruction.Nop]
        AssembleInstruction "HLT" |>== [byte Instruction.Hlt]

        AssembleInstruction "CCC" |>== [byte Instruction.Ccc]
        AssembleInstruction "CCC NZVC" |>== [byte Instruction.Ccc ||| byte Flag.Negative ||| byte Flag.Zero ||| byte Flag.Overflow ||| byte Flag.Carry]
        AssembleInstruction "SCC" |>== [byte Instruction.Scc]
        AssembleInstruction "SCC ZC" |>== [byte Instruction.Scc ||| byte Flag.Zero ||| byte Flag.Carry]

        AssembleInstruction "BR 0" |>== [byte Instruction.Br; 0uy]
        AssembleInstruction "BNE 10" |>== [byte Instruction.Bne; 10uy]
        AssembleInstruction "BNE -10" |>== [byte Instruction.Bne; 246uy]
        AssembleInstruction "BNE 246" |>== [byte Instruction.Bne; 246uy]

        AssembleInstruction "JMP R7" |>== [byte Instruction.Jmp; byte Register.R7] // Invalid!
        AssembleInstruction "JMP (R7)+" |>== [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostInc]
        AssembleInstruction "JMP ((R7)+)" |>== [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect]

        AssembleInstruction "JMP #10" |>== [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostInc; 0uy; 10uy] // Valid, but not useful
        AssembleInstruction "JMP 10" |>== [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect; 0uy; 10uy]
        AssembleInstruction "JMP #-10" |>== [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostInc; 255uy; 246uy] // Valid, but not useful
        AssembleInstruction "JMP -10" |>== [byte Instruction.Jmp; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect; 255uy; 246uy]

        AssembleInstruction "SOB R1, 123" |>== [byte Instruction.Sob ||| byte Register.R1; 123uy]
        AssembleInstruction "SOB R2, 234" |>== [byte Instruction.Sob ||| byte Register.R2; 234uy]

        AssembleInstruction "JSR R1, R7" |>== [byte Instruction.Jsr ||| byte Register.R1; byte Register.R7] // Invalid!
        AssembleInstruction "JSR R2, (R7)+" |>== [byte Instruction.Jsr ||| byte Register.R2; byte Register.R7 ||| byte AddressMode.RegPostInc]
        AssembleInstruction "JSR R3, ((R7)+)" |>== [byte Instruction.Jsr ||| byte Register.R3; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect]

        AssembleInstruction "RTS R4" |>== [byte Instruction.Rts ||| byte Register.R4]

        AssembleInstruction "NOT #15" |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostInc; 0uy; 15uy]
        AssembleInstruction "NOT 15"  |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect; 0uy; 15uy]

        AssembleInstruction "NOT R7" |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.Register]
        AssembleInstruction "NOT (R7)+" |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostInc]
        AssembleInstruction "NOT -(R7)" |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPreDec]
        AssembleInstruction "NOT 2(R7)" |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.Indexed; 0uy; 2uy]
        AssembleInstruction "NOT (R7)" |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegisterIndirect]
        AssembleInstruction "NOT ((R7)+)" |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPostIncIndirect]
        AssembleInstruction "NOT (-(R7))" |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.RegPreDecIndirect]
        AssembleInstruction "NOT (2(R7))" |>== [byte Instruction.Not; byte Register.R7 ||| byte AddressMode.IndexedIndirect; 0uy; 2uy]

        let e = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegPostInc Register.R7 AddressMode.Register Register.R1
        AssembleInstruction "MOV #10, R1" |>== [byte (e >>> 8); byte e; 0uy; 10uy]

        let f = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegPostIncIndirect Register.R7 AddressMode.Register Register.R2
        AssembleInstruction "MOV 1000, R2" |>== [byte (f >>> 8); byte f; 3uy; 232uy]

        let g = EncodeInstructionTwoOperand Instruction.Mov AddressMode.Register Register.R1 AddressMode.RegPostInc Register.R2
        AssembleInstruction "MOV R1, (R2)+" |>== [byte (g >>> 8); byte g]

        let h = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegisterIndirect Register.R1 AddressMode.RegPreDec Register.R2
        AssembleInstruction "MOV (R1), -(R2)" |>== [byte (h >>> 8); byte h]

        let i = EncodeInstructionTwoOperand Instruction.Mov AddressMode.Indexed Register.R1 AddressMode.RegPostIncIndirect Register.R2
        AssembleInstruction "MOV 10(R1), ((R2)+)" |>== [byte (i >>> 8); byte i; 0uy; 10uy]

        let j = EncodeInstructionTwoOperand Instruction.Mov AddressMode.IndexedIndirect Register.R1 AddressMode.IndexedIndirect Register.R2
        AssembleInstruction "MOV (10(R1)), (20(R2))" |>== [byte (j >>> 8); byte j; 0uy; 10uy; 0uy; 20uy]

        let k = EncodeInstructionTwoOperand Instruction.Mov AddressMode.RegPreDecIndirect Register.R1 AddressMode.IndexedIndirect Register.R2
        AssembleInstruction "MOV (-(R1)), (20(R2))" |>== [byte (k >>> 8); byte k; 0uy; 20uy]

        AssembleInstruction "MOV #32768, R1" |>== [147uy; 193uy; 128uy; 0uy]
        AssembleInstruction "MOV #-32768, R1" |>== [147uy; 193uy; 128uy; 0uy]
        AssembleInstruction "MOV #65535, R1" |>== [147uy; 193uy; 255uy; 255uy]
        AssembleInstruction "MOV #-1, R1" |>== [147uy; 193uy; 255uy; 255uy]

    [<TestMethod>]
    member this.``CesarAssembler: AssembleProgram works as expected``() =
        let program = """
            MOV #10, R1
            MOV 1000, R2
            MOV :L1, R3
            MOV :L2, R4
            MOV :L2, R5
        :L1
            HLT              ; End of program
            NOP
            NOP
            BR :L1           ; Test branch/jump backwards
            SOB R1, :L1
            JMP :L1
            BR :L3           ; Test branch/jump forward
            SOB R3, :L3
            JMP :L3
            NOP
            NOP
        :L3
            JMP :L2
        @1000
            0                ; R2 value
            123
        :L2
            1234             ; R4,R5 value
        """

        let expectedProgram = [|147uy;193uy;0uy;10uy;          // MOV #10, R1
                                155uy;194uy;3uy;232uy;         // MOV 1000, R2
                                155uy;195uy;0uy;20uy;          // MOV :L1, R3     (:L1 = 20)
                                155uy;196uy;3uy;234uy;         // MOV :L2, R4     (:L2 = 1002)
                                155uy;197uy;3uy;234uy;         // MOV :L2, R5
                                240uy;                         // HLT
                                0uy;                           // NOP
                                0uy;                           // NOP
                                48uy;251uy;                    // BR :L1          (-5)
                                81uy;7uy;                      // SOB R1, :L1     (7)
                                64uy;47uy;0uy;20uy;            // JMP :L1
                                48uy;8uy;                      // BR :L3          (8)
                                83uy;250uy;                    // SOB R3, :L3     (-6)
                                64uy;47uy;0uy;41uy;            // JMP :L3         (:L3 = 41)
                                0uy;                           // NOP
                                0uy;                           // NOP
                                64uy;47uy;3uy;234uy;|]         // JMP :L2

        let expectedData = [|0uy;123uy;4uy;210uy|]

        AssembleProgram base.Cpu program
        let programArea = Array.sub base.Cpu.Memory.Data 0 45
        Array.compareWith (fun a b -> if a = b then 0 else 1) expectedProgram programArea |>== 0

        let dataArea = Array.sub base.Cpu.Memory.Data 1000 4
        Array.compareWith (fun a b -> if a = b then 0 else 1) expectedData dataArea |>== 0

        Step base.Cpu
        Step base.Cpu
        Step base.Cpu
        Step base.Cpu
        Step base.Cpu
        Step base.Cpu
        this.AssertCesarState [R1 10us; R2 123us; R3 61440us; R4 1234us; R5 1234us; ProgramCounter 21us; FlagsHalted true]

    [<TestMethod>]
    [<ExpectedException(typeof<System.Exception>, "Label indefinido: L1")>]
    member this.``CesarAssembler: AssembleProgram fails with undeclared label``() =
        AssembleProgram base.Cpu "JMP :L1"

    [<TestMethod>]
    [<ExpectedException(typeof<System.Exception>, "Label inacessível a partir de um branch: L1")>]
    member this.``CesarAssembler: AssembleProgram fails with far branches``() =
        AssembleProgram base.Cpu """
            BR :L1
        @1000
        :L1
            HLT
        """
