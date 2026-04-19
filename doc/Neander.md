# Neander — Architecture Manual

## 1. Overview

Neander is the simplest member of the UFRGS family of didactic machines (Neander → Ahmes → Ramses → Cesar). It is the minimum viable computer: one accumulator, one program counter, one addressing mode, eleven instructions, two status flags. Everything is 8-bit; memory is 256 bytes. The whole machine state fits in about a dozen values.

This simplicity is the point. Neander is meant to be the first computer a student ever sees the inside of — and the architecture rewards a UI that shows every register, every flag, and every byte of memory simultaneously on one screen.

Source of truth: [fs/ArchSims.Core/Neander.fs](../fs/ArchSims.Core/Neander.fs), [fs/ArchSims.Core/Memory.fs](../fs/ArchSims.Core/Memory.fs).

Note: the current codebase does not ship an assembler for Neander (only Ramses and Cesar have one). The CLI runner also does not dispatch Neander ([fs/ArchSims.CmdLine/Main.fs:77](../fs/ArchSims.CmdLine/Main.fs#L77) raises `Not implemented: Neander`). Programs are exercised through the `Neander` F# module directly and through the test suite ([fs/ArchSims.Core.Tests/NeanderTests.fs](../fs/ArchSims.Core.Tests/NeanderTests.fs)).

## 2. Programmer's Model

### 2.1 Registers

| Register | Width  | Role                                      |
| -------- | ------ | ----------------------------------------- |
| `AC`     | 8 bits | Accumulator — the only data register      |
| `PC`     | 8 bits | Program Counter — wraps 0xFF → 0x00       |

All data moves through `AC`. There are no index registers, no general registers, no stack.

### 2.2 Instruction Register (IR)

Two bytes:

- `OpCode` — the instruction byte just fetched,
- `OperandAddress` — the operand byte (`0` if the instruction takes no operand).

The IR is programmer-visible in the core types ([Neander.fs:22-25](../fs/ArchSims.Core/Neander.fs#L22-L25)) and is the single best widget to show "what will execute next".

### 2.3 Flags

| Flag       | Set by                                  | Reset value |
| ---------- | --------------------------------------- | ----------- |
| `Negative` | result > 0x7F (i.e. bit 7 = 1)          | `false`     |
| `Zero`     | result = 0                              | **`true`**  |
| `Halted`   | `HLT` just executed                     | `false`     |

`N` and `Z` are updated by every instruction that writes `AC`: `LDA`, `ADD`, `OR`, `AND`, `NOT`. `STA`, `JMP`, `JN`, `JZ`, `NOP`, `HLT` leave them alone. There is no carry flag in Neander — `ADD` wraps around modulo 256 silently. The distinction between "no carry at all" and Ramses's `C` flag is a useful teaching moment.

`Zero` starts set because the initial accumulator value is 0, which *is* zero. UIs should reflect that — the zero lamp comes on at reset.

### 2.4 Memory

- Flat, byte-addressable, 256 bytes (addresses 0x00..0xFF).
- No memory-mapped I/O and no stack.
- `PC` is 8-bit; incrementing past 0xFF wraps to 0x00 ([NeanderTests.fs:60](../fs/ArchSims.Core.Tests/NeanderTests.fs#L60)).

The memory type in [Memory.fs](../fs/ArchSims.Core/Memory.fs) also keeps `ReadCount` and `WriteCount`. The CLI doesn't print them, but a UI can use them for a "memory accesses this step" indicator.

## 3. Instruction Set

All instructions are one or two bytes. The first byte (the "opcode") fully identifies the instruction — Neander does not use sub-opcode bits, address-mode bits, or register-selection bits. The opcodes are chosen so that the high nibble varies — making them easy to decode visually ("0x3_ is always ADD").

### 3.1 Instruction table

| Mnemonic | Opcode (hex) | Bytes | Operand? | Writes | Flags affected | Effect |
| -------- | ------------ | ----- | -------- | ------ | -------------- | ------ |
| `NOP`    | `0x00`       | 1     | no       | —      | —              | Do nothing |
| `STA a`  | `0x10`       | 2     | yes      | MEM    | —              | `MEM[a] ← AC` |
| `LDA a`  | `0x20`       | 2     | yes      | AC     | N, Z           | `AC ← MEM[a]` |
| `ADD a`  | `0x30`       | 2     | yes      | AC     | N, Z           | `AC ← AC + MEM[a]` (mod 256) |
| `OR  a`  | `0x40`       | 2     | yes      | AC     | N, Z           | `AC ← AC OR MEM[a]` |
| `AND a`  | `0x50`       | 2     | yes      | AC     | N, Z           | `AC ← AC AND MEM[a]` |
| `NOT`    | `0x60`       | 1     | no       | AC     | N, Z           | `AC ← NOT AC` |
| `JMP a`  | `0x80`       | 2     | yes      | PC     | —              | `PC ← a` |
| `JN  a`  | `0x90`       | 2     | yes      | PC     | —              | If `N=1`, `PC ← a` |
| `JZ  a`  | `0xA0`       | 2     | yes      | PC     | —              | If `Z=1`, `PC ← a` |
| `HLT`    | `0xF0`       | 1     | no       | —      | Halted         | Stops the CPU |

(The gaps 0x70, 0xB0..0xE0 are reserved — Ahmes fills several of them with arithmetic and extended-conditional-jump extensions.)

Decoding is by exact match on the full opcode byte: any byte whose value is not one of the eleven above is treated as `NOP`/`HLT` by the F# implementation ([Neander.fs:128-130](../fs/ArchSims.Core/Neander.fs#L128-L130)). This makes the machine robust against garbage memory contents but is worth mentioning in a UI — "unknown opcode" should probably be rendered distinctly in the disassembly.

### 3.2 Addressing

There is exactly **one** addressing mode: **direct**. The operand byte is the address. No indirect, no immediate, no indexed, no relative. Immediate values are achieved by storing the constant in memory as data and using `LDA` to pull it into `AC`.

This is the second-biggest teaching point after flags: moving from Neander to Ramses (four addressing modes) or Cesar (eight) is mostly a story of "the operand no longer literally *is* the address".

## 4. Execution Cycle

`Step cpu` ([Neander.fs:134](../fs/ArchSims.Core/Neander.fs#L134)) performs `Fetch` then `Execute`. There are no interrupts, no cycle timings, no pipeline — one instruction per step.

**Fetch** ([Neander.fs:67](../fs/ArchSims.Core/Neander.fs#L67)):

1. Read byte at `PC`, store in `IR.OpCode`, increment `PC`.
2. If the instruction takes an operand (`STA`, `LDA`, `ADD`, `OR`, `AND`, `JMP`, `JN`, `JZ`), read another byte, store in `IR.OperandAddress`, increment `PC`. Otherwise `IR.OperandAddress ← 0`.

**Execute** ([Neander.fs:86](../fs/ArchSims.Core/Neander.fs#L86)):

1. Match on the opcode.
2. Perform the operation — read one more byte from `MEM[IR.OperandAddress]` for the ALU/load instructions, write `AC` for `STA`, or update `PC` for the jumps.
3. For every instruction that writes `AC`, call `writeAccumulator` which also updates `N` and `Z` ([Neander.fs:90-93](../fs/ArchSims.Core/Neander.fs#L90-L93)).
4. Set `Halted` iff the instruction was `HLT`.

Note: `Halted` is **cleared** by the next executed instruction ([Neander.fs:132](../fs/ArchSims.Core/Neander.fs#L132)). It indicates "the instruction just completed was `HLT`", not "the CPU is in a halted state forever". Running past an `HLT` drops the flag again. UI implication: treat `Halted` as a just-stopped signal; the debugger is what prevents further steps from happening ([Debugger.fs:35-36](../fs/ArchSims.Core/Debugger.fs#L35-L36)).

## 5. Memory Access Cost per Instruction

Useful for a UI that shows memory-bus activity:

| Instruction class            | Reads per step | Writes per step |
| ---------------------------- | -------------- | --------------- |
| `NOP`, `HLT`                 | 1              | 0               |
| `NOT`                        | 1              | 0               |
| `JMP`, `JN` (taken/not), `JZ`| 2              | 0               |
| `LDA`, `ADD`, `OR`, `AND`    | 3              | 0               |
| `STA`                        | 2              | 1               |

These counts are visible in the test assertions ([NeanderTests.fs:99-137](../fs/ArchSims.Core.Tests/NeanderTests.fs#L99-L137)) and can feed a "bus traffic" indicator.

## 6. Programming Notes

- **No immediate mode.** Constants must live somewhere in memory. A typical convention is to put data at the high end (e.g., 0xF0..0xFF) and code at the low end.
- **No carry.** `ADD` of `0xFF + 0x01` yields `0x00` with `Z=1` and no other trace. Multi-byte arithmetic is not possible without extending the ISA (Ahmes adds `C` and `SUB`).
- **Conditional jumps are absolute.** `JN 128` means "jump to address 128 if negative", regardless of where the `JN` itself lives. There are no relative branches.
- **Subroutines do not exist.** `JMP` is the only way to transfer control. A teaching exercise often compared against is Ramses's `JSR` — Neander shows why that was added.
- **Unused opcodes are silent.** Bytes like `0x70` (which Ramses uses for `SUB`) are executed as `NOP`. A curious student can discover the reserved range by running garbage bytes.

## 7. Small Example Programs

Neander has no shipping assembler in this repo, so examples are presented as hand-assembled bytes.

### 7.1 Add two numbers

```
Address  Bytes        Source             Comment
-------  -----------  ----------------   -------------------------------
  0      0x20 0x10    LDA 0x10           AC ← MEM[0x10]
  2      0x30 0x11    ADD 0x11           AC ← AC + MEM[0x11]
  4      0x10 0x12    STA 0x12           MEM[0x12] ← AC
  6      0xF0         HLT
  ...
 0x10    0x07                            First operand  (7)
 0x11    0x05                            Second operand (5)
 0x12    0x00                            Result slot
```

After running, `MEM[0x12] = 0x0C` (= 12), `AC = 0x0C`, `PC = 7`, `N = 0`, `Z = 0`, `Halted = true`.

### 7.2 Loop: count down from 10 to 0

```
Address  Bytes        Source             Comment
-------  -----------  ----------------   -------------------------------
  0      0x20 0x10    LDA 0x10           AC ← counter
  2      0xA0 0x0A    JZ  0x0A           If zero, jump to HLT
  4      0x30 0x11    ADD 0x11           AC ← AC + (-1)
  6      0x10 0x10    STA 0x10           counter ← AC
  8      0x80 0x00    JMP 0x00           Loop
 10      0xF0         HLT
 0x10    0x0A                            counter = 10
 0x11    0xFF                            two's-complement −1
```

`ADD 0x11` with the constant `0xFF` acts as decrement — a Neander idiom worth teaching up front.

## 8. Debugger API

The same `Debugger` module powers all four simulators ([Debugger.fs](../fs/ArchSims.Core/Debugger.fs)). For Neander, the natural wiring is:

```fsharp
CreateDebugger
    (fun () -> int cpu.Registers.ProgramCounter)
    (fun () -> Step cpu; cpu.Registers.Flags.Halted)
```

Exposed operations: `DebuggerStep`, `DebuggerRun maximumInstructions`, `DebuggerSetBreakpoint address`, `DebuggerClearBreakpoint address`, `DebuggerReset`. Stop reasons: `Halted`, `Breakpoint`, `RunningForever`, `None`.

A UI that wires to this gets step / run / breakpoints for free.

## 9. UI Design Suggestions

1. **One-screen rule.** 256 bytes fit in a 16×16 hex grid. Render memory, registers, flags, and a small disassembly panel all on one screen; scrolling defeats the teaching purpose of Neander.
2. **Memory grid with three overlays.** Color the cell at `PC` (next-to-execute), the cell at `IR.OperandAddress` (about to be read/written), and any cell written last step. Keep the scheme consistent across the whole simulator family.
3. **Register tiles.** Two tiles: `AC` and `PC`. Show each simultaneously in hex, decimal, and binary. The machine is small enough that showing all three bases costs nothing and helps with flag intuition (why does `ADD` flip `N` here?).
4. **Flag lamps.** Exactly three: `N`, `Z`, `Halted`. Render as LEDs. At reset `Z` is on — make sure this is reflected.
5. **IR decomposition widget.** Show the IR opcode byte as two hex nibbles with the high nibble labeled "instruction" (decodes to the mnemonic) and the low nibble "unused — always 0". This is a direct visual lead-in to Ramses, where the low nibble gains meaning.
6. **Disassembly strip.** Walk memory from address 0, decode 1- or 2-byte instructions inline, and render as an address/bytes/mnemonic list. Mark the instruction that will be fetched next. Since there is only one addressing mode, the disassembly is trivial — `LDA 123` with no decoration needed. (An on-the-fly disassembler following the pattern of `DisassembleInstruction` in [Ramses.fs:221](../fs/ArchSims.Core/Ramses.fs#L221) can be written in a few dozen lines.)
7. **Data/code ambiguity.** Neander has no way to tell code from data. A useful UI affordance: let the user mark byte ranges as "data (hex)" or "code" and render them differently in the memory grid. The CPU does not care — this is purely a teaching aid.
8. **"What if I changed this?" mode.** Because the machine is so small, clicking any memory cell to edit its byte and then stepping is a powerful exploration. The core exposes `cpu.Memory.Data` directly — a UI can mutate it freely between steps.
9. **Missing features are the lesson.** Consider tooltips on the gaps in the opcode table ("0x70 is used by Ahmes for SUB"). A UI that motivates the next simulator is a teaching tool; a UI that pretends the family doesn't exist is just a visualizer.
10. **Step-through animation.** Fetch is easy to split visually: (a) PC points to opcode, (b) opcode moves into IR, (c) PC advances, (d) operand byte moves into IR.OperandAddress, (e) PC advances again. Execute is one cycle: arrow from IR.OperandAddress to the memory cell, arrow back to AC (or the other way for STA). Slow-stepping this for beginners explains more than any text.
11. **Breakpoints.** The `DebuggerSetBreakpoint` API takes an address; clicking an address in the memory grid to toggle a breakpoint is the cheapest, clearest binding.
12. **No file format in this repo.** Neander binaries historically use a `03 4E 44 52` (ascii `NDR`) prefix, analogous to Ramses's `03 'R' 'M' 'S'` prefix seen at [RamsesCmdLine.fs:62](../fs/ArchSims.CmdLine/RamsesCmdLine.fs#L62). A future UI "Save memory" feature should follow that pattern so files are interchangeable with existing Neander tools.
