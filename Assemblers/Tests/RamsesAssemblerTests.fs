﻿namespace Ufrgs.Inf.ArchSims.Assemblers.Tests.Ramses

open Microsoft.VisualStudio.TestTools.UnitTesting

open Ufrgs.Inf.ArchSims.Core.Memory
open Ufrgs.Inf.ArchSims.Core.Ramses
open Ufrgs.Inf.ArchSims.Assemblers.Ramses.RamsesAssembler

open Ufrgs.Inf.ArchSims.Core.Tests.Ramses

[<TestClass>]
type RamsesAssemblersTests() = 
    inherit RamsesTests()

    [<TestMethod>]
    member this.``RamsesAssembler: AssembleInstruction works as expected``() =
        Assert.AreEqual([byte Instruction.Nop], AssembleInstruction "NOP")
        Assert.AreEqual([byte Instruction.Hlt], AssembleInstruction "HLT")

        Assert.AreEqual([byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Direct; 12uy], AssembleInstruction "STR A 12")
        Assert.AreEqual([byte Instruction.Str ||| byte Register.Rb ||| byte AddressMode.Indirect; 23uy], AssembleInstruction "STR B 23,I")
        Assert.AreEqual([byte Instruction.Str ||| byte Register.Rx ||| byte AddressMode.Immediate; 34uy], AssembleInstruction "STR X #34")
        Assert.AreEqual([byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Indexed; 45uy], AssembleInstruction "STR A 45,X")

        Assert.AreEqual([byte Instruction.Not ||| byte Register.Ra], AssembleInstruction "NOT A")
        Assert.AreEqual([byte Instruction.Not ||| byte Register.Rb], AssembleInstruction "NOT B")
        Assert.AreEqual([byte Instruction.Not ||| byte Register.Rx], AssembleInstruction "NOT X")

        Assert.AreEqual([byte Instruction.Jmp ||| byte AddressMode.Direct; 12uy], AssembleInstruction "JMP 12")
        Assert.AreEqual([byte Instruction.Jmp ||| byte AddressMode.Indirect; 23uy], AssembleInstruction "JMP 23,I")
        Assert.AreEqual([byte Instruction.Jmp ||| byte AddressMode.Immediate; 34uy], AssembleInstruction "JMP #34")
        Assert.AreEqual([byte Instruction.Jmp ||| byte AddressMode.Indexed; 45uy], AssembleInstruction "JMP 45,X")

        Assert.AreEqual([byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 0uy], AssembleInstruction "LDR X #0")
        Assert.AreEqual([byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 127uy], AssembleInstruction "LDR X #127")
        Assert.AreEqual([byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 128uy], AssembleInstruction "LDR X #128")
        Assert.AreEqual([byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 255uy], AssembleInstruction "LDR X #255")

    [<TestMethod>]
    member this.``RamsesAssembler: DisassembleInstruction works as expected``() =
        Assert.AreEqual("NOP", DisassembleInstruction [byte Instruction.Nop])
        Assert.AreEqual("NOP", DisassembleInstruction [byte Instruction.Nop + 5uy])
        Assert.AreEqual("HLT", DisassembleInstruction [byte Instruction.Hlt])

        Assert.AreEqual("STR A 12", DisassembleInstruction [byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Direct; 12uy])
        Assert.AreEqual("STR B 23,I", DisassembleInstruction [byte Instruction.Str ||| byte Register.Rb ||| byte AddressMode.Indirect; 23uy])
        Assert.AreEqual("STR X #34", DisassembleInstruction [byte Instruction.Str ||| byte Register.Rx ||| byte AddressMode.Immediate; 34uy])
        Assert.AreEqual("STR A 45,X", DisassembleInstruction [byte Instruction.Str ||| byte Register.Ra ||| byte AddressMode.Indexed; 45uy])

        Assert.AreEqual("NOT A", DisassembleInstruction [byte Instruction.Not ||| byte Register.Ra])
        Assert.AreEqual("NOT B", DisassembleInstruction [byte Instruction.Not ||| byte Register.Rb])
        Assert.AreEqual("NOT X", DisassembleInstruction [byte Instruction.Not ||| byte Register.Rx])

        Assert.AreEqual("JMP 12", DisassembleInstruction [byte Instruction.Jmp ||| byte AddressMode.Direct; 12uy])
        Assert.AreEqual("JMP 23,I", DisassembleInstruction [byte Instruction.Jmp ||| byte AddressMode.Indirect; 23uy])
        Assert.AreEqual("JMP #34", DisassembleInstruction [byte Instruction.Jmp ||| byte AddressMode.Immediate; 34uy])
        Assert.AreEqual("JMP 45,X", DisassembleInstruction [byte Instruction.Jmp ||| byte AddressMode.Indexed; 45uy])

        Assert.AreEqual("LDR X #0", DisassembleInstruction [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 0uy])
        Assert.AreEqual("LDR X #127", DisassembleInstruction [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 127uy])
        Assert.AreEqual("LDR X #128", DisassembleInstruction [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 128uy])
        Assert.AreEqual("LDR X #255", DisassembleInstruction [byte Instruction.Ldr ||| byte Register.Rx ||| byte AddressMode.Immediate; 255uy])

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
        Assert.AreEqual(0, Array.compareWith (fun a b -> if a = b then 0 else 1) expectedProgram memoryArea)

        Step base.Cpu
        Step base.Cpu
        Step base.Cpu
        Step base.Cpu
        this.AssertRamsesState [Ra 123uy; Rb 14uy; Rx 66uy; ProgramCounter 7uy; FlagsHalted true]

    [<TestMethod>]
    [<ExpectedException(typeof<System.Exception>, "Label indefinido: L1")>]
    member this.``RamsesAssembler: AssembleProgram fails with undeclared label``() =
        AssembleProgram base.Cpu "LDR A :L1"