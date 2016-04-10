namespace Ufrgs.Inf.ArchSims.Assemblers.Tests.Ramses

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Ramses
open Ufrgs.Inf.ArchSims.Assemblers.Ramses.RamsesAssembler

open Ufrgs.Inf.ArchSims.Core.Tests.Ramses
open Ufrgs.Inf.ArchSims.Core.Tests.Utils

[<TestClass>]
type RamsesAssemblersTests() = 
    inherit RamsesTests()

    [<TestMethod>]
    member this.``RamsesAssembler: AssembleInstruction works as expected``() =
        AssembleInstruction "NOP" |>== [byte Instruction.Nop]
        AssembleInstruction "HLT" |>== [byte Instruction.Hlt]

        AssembleInstruction "STR A 12" |>== [byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Direct; 12uy]
        AssembleInstruction "STR B 23,I" |>== [byte Instruction.Str ||| byte Register.Rb ||| byte AddressMode.Indirect; 23uy]
        AssembleInstruction "STR X #34" |>== [byte Instruction.Str ||| byte Register.Rx ||| byte AddressMode.Immediate; 34uy]
        AssembleInstruction "STR A 45,X" |>== [byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Indexed; 45uy]

        AssembleInstruction "NOT A" |>== [byte Instruction.Not ||| byte Register.Ra]
        AssembleInstruction "NOT B" |>== [byte Instruction.Not ||| byte Register.Rb]
        AssembleInstruction "NOT X" |>== [byte Instruction.Not ||| byte Register.Rx]

        AssembleInstruction "JMP 12" |>== [byte Instruction.Jmp ||| byte AddressMode.Direct; 12uy]
        AssembleInstruction "JMP 23,I" |>== [byte Instruction.Jmp ||| byte AddressMode.Indirect; 23uy]
        AssembleInstruction "JMP #34" |>== [byte Instruction.Jmp ||| byte AddressMode.Immediate; 34uy]
        AssembleInstruction "JMP 45,X" |>== [byte Instruction.Jmp ||| byte AddressMode.Indexed; 45uy]

        AssembleInstruction "LDR X #0" |>== [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 0uy]
        AssembleInstruction "LDR X #127" |>== [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 127uy]
        AssembleInstruction "LDR X #128" |>== [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 128uy]
        AssembleInstruction "LDR X #255" |>== [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 255uy]

    [<TestMethod>]
    member this.``RamsesAssembler: AssembleProgram works as expected``() =
        let program = """
            LDR A 10
            LDR B :L1
            LDR X :L1,I
            HLT              ; End of program
        @10
            123              ; Ra value
        :L1
            14               ; Rb value
        @14
            66               ; Rx value
        """

        let expectedProgram = [|byte Instruction.Ldr ||| byte Register.Ra ||| byte AddressMode.Direct;
                                10uy;
                                byte Instruction.Ldr ||| byte Register.Rb ||| byte AddressMode.Direct;
                                11uy;
                                byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Indirect;
                                11uy;
                                byte Instruction.Hlt;
                                0uy;
                                0uy;
                                0uy;
                                123uy;
                                14uy
                                0uy;
                                0uy;
                                66uy|]

        AssembleProgram base.Cpu program
        let memoryArea = Array.sub base.Cpu.Memory.Data 0 15
        Array.compareWith (fun a b -> if a = b then 0 else 1) expectedProgram memoryArea |>== 0

        Step base.Cpu
        Step base.Cpu
        Step base.Cpu
        Step base.Cpu
        this.AssertRamsesState [Ra 123uy; Rb 14uy; Rx 66uy; ProgramCounter 7uy; FlagsHalted true]

    [<TestMethod>]
    [<ExpectedException(typeof<System.Exception>, "Label indefinido: L1")>]
    member this.``RamsesAssembler: AssembleProgram fails with undeclared label``() =
        AssembleProgram base.Cpu "LDR A :L1"
