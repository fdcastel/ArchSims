# Ramses — Architecture Manual

## 1. Overview

Ramses is an 8-bit didactic computer, the third in the family of hypothetical machines used in UFRGS computer architecture courses (Neander → Ahmes → Ramses → Cesar). It keeps Neander's byte-sized word and 256-byte address space but adds:

- three general-purpose registers instead of a single accumulator,
- four addressing modes,
- subtraction, jump-on-carry, subroutine call, negate, shift-right instructions,
- a `Carry` flag.

Everything in Ramses is still 8-bit: the word size, each register, the Program Counter, and memory addresses. This keeps programs small and makes register/memory state easy to show on a single screen — a useful constraint for UI design.

Source of truth for this document: [fs/ArchSims.Core/Ramses.fs](../fs/ArchSims.Core/Ramses.fs), [fs/ArchSims.Core/Memory.fs](../fs/ArchSims.Core/Memory.fs), [fs/ArchSims.Assemblers/RamsesAssembler.fs](../fs/ArchSims.Assemblers/RamsesAssembler.fs).

## 2. Programmer's Model

### 2.1 Registers

| Register | Width  | Encoding (bits 3..2) | Role                                                  |
| -------- | ------ | -------------------- | ----------------------------------------------------- |
| `RA`     | 8 bits | `00`                 | General-purpose                                       |
| `RB`     | 8 bits | `01`                 | General-purpose                                       |
| `RX`     | 8 bits | `10`                 | General-purpose; also used as index in `Indexed` mode |
| `PC`     | 8 bits | `11`                 | Program Counter                                       |

`PC` is addressable as a register field in the encoding, but every instruction that writes through the register field simply writes whichever register the two-bit code selects. Practical programs almost always use `RA`, `RB`, `RX`.

The assembler accepts these names: `A`, `B`, `X`, `PC` (case-insensitive).

### 2.2 Instruction Register (IR)

A two-byte structure holding:

- `OpCode` — the instruction byte just fetched (with register and addressing-mode fields already embedded),
- `OperandAddress` — the effective 8-bit address computed during fetch (0 for no-operand instructions).

The IR is programmer-visible in the CLI output and should be visible in the UI — it is the best single explanation of "what is about to execute".

### 2.3 Flags

| Flag       | Set by                                                              | Cleared on reset |
| ---------- | ------------------------------------------------------------------- | ---------------- |
| `Negative` | result bit 7 = 1 (i.e. result > 0x7F)                               | false            |
| `Zero`     | result = 0                                                          | **true**         |
| `Carry`    | `ADD`/`SUB`: unsigned overflow; `NEG`: operand was 0; `SHR`: LSB    | false            |
| `Halted`   | `HLT` just executed                                                 | false            |

Note: `Zero` is `true` in the initial state because the accumulator-like value 0 *is* zero. UIs should reflect this — flag lamps start `-Z-`, not all off.

`N` and `Z` are updated by every register-writing instruction (`LDR`, `ADD`, `OR`, `AND`, `NOT`, `SUB`, `NEG`, `SHR`). `C` is touched only by `ADD`, `SUB`, `NEG`, `SHR`. `STR`, `JMP`, `Jxx`, `JSR`, `NOP`, `HLT` leave the flags alone.

### 2.4 Memory

- Flat, byte-addressable, 256 bytes (addresses 0x00..0xFF).
- No memory-mapped I/O and no stack; subroutines use a self-linking convention (see `JSR`).
- `PC` wraps naturally at 0xFF → 0x00.

The core instruments reads and writes via `MemoryReadByte`/`MemoryWriteByte`, keeping `ReadCount`/`WriteCount` counters that the CLI never prints but that a UI can display as a "memory accesses this step" indicator.

## 3. Instruction Encoding

Each instruction occupies one or two bytes. The first byte ("OpCode") packs three fields:

```
bit  7 6 5 4   3 2   1 0
     I I I I   R R   M M
     |         |     |
     |         |     +-- Address mode
     |         +-------- Register
     +------------------ Instruction
```

Masks (see `Ramses.fs`):

- `InstructionMask = 0b11110000` (0xF0)
- `RegisterMask    = 0b00001100` (0x0C)
- `AddressModeMask = 0b00000011` (0x03)

Instructions that take no register (`NOP`, `JMP`, `Jxx`, `JSR`, `HLT`) simply ignore the R field. Instructions without an operand (`NOP`, `NOT`, `NEG`, `SHR`, `HLT`) occupy one byte and ignore the M field.

### 3.1 Instruction Table

| Mnemonic | OpCode (hi nibble) | Bytes | Operand | Writes | Affects flags | Notes |
| -------- | ------------------ | ----- | ------- | ------ | ------------- | ----- |
| `NOP`    | `0000` (0x00)      | 1     | —       | —      | —             | No-op |
| `STR r`  | `0001` (0x10)      | 2     | yes     | MEM    | —             | `MEM[addr] ← r` |
| `LDR r`  | `0010` (0x20)      | 2     | yes     | r      | N, Z          | `r ← MEM[addr]` |
| `ADD r`  | `0011` (0x30)      | 2     | yes     | r      | N, Z, C       | `r ← r + MEM[addr]` |
| `OR  r`  | `0100` (0x40)      | 2     | yes     | r      | N, Z          | `r ← r OR MEM[addr]` |
| `AND r`  | `0101` (0x50)      | 2     | yes     | r      | N, Z          | `r ← r AND MEM[addr]` |
| `NOT r`  | `0110` (0x60)      | 1     | —       | r      | N, Z          | `r ← NOT r` |
| `SUB r`  | `0111` (0x70)      | 2     | yes     | r      | N, Z, C       | `r ← r − MEM[addr]`; `C` = *no borrow* |
| `JMP`    | `1000` (0x80)      | 2     | yes     | PC     | —             | Unconditional |
| `JN`     | `1001` (0x90)      | 2     | yes     | PC     | —             | If `N=1` |
| `JZ`     | `1010` (0xA0)      | 2     | yes     | PC     | —             | If `Z=1` |
| `JC`     | `1011` (0xB0)      | 2     | yes     | PC     | —             | If `C=1` |
| `JSR`    | `1100` (0xC0)      | 2     | yes     | MEM, PC| —             | Subroutine call (see below) |
| `NEG r`  | `1101` (0xD0)      | 1     | —       | r      | N, Z, C       | `r ← −r` (two's complement); `C=1` iff original r = 0 |
| `SHR r`  | `1110` (0xE0)      | 1     | —       | r      | N, Z, C       | Logical right shift; `C` ← former LSB |
| `HLT`    | `1111` (0xF0)      | 1     | —       | —      | Halted        | Stops the CPU |

### 3.2 Notes on individual instructions

- **`SUB`** uses an adder-with-carry trick (`r + 256 − operand`) and then inverts the carry so that `C=1` means "no borrow" — matching the behavior tested in [fs/ArchSims.Core.Tests/RamsesTests.fs](../fs/ArchSims.Core.Tests/RamsesTests.fs:184).
- **`JSR addr`** stores the current `PC` at `MEM[addr]` and sets `PC ← addr + 1`. Subroutines return by executing a `JMP` through that stored byte (usually `JMP :Label,I` at the end of the subroutine). There is no hardware stack.
- **`JMP #imm`** is syntactically legal but semantically degenerate: immediate mode for branch instructions reads the next byte as the target, so `JMP #5` is equivalent to `JMP 5`. The Fetch code checks `instruction >= Instruction.Jmp` for this special case ([Ramses.fs:126](../fs/ArchSims.Core/Ramses.fs#L126)).
- **`NEG`** is implemented as `~r + 1`; its carry rule is "set when the operand was zero" — the only case where negation produces a borrow.

## 4. Addressing Modes

Four modes, encoded in the two low bits of the OpCode:

| Mode        | Bits | Assembly suffix | Effective address                            | Extra memory reads |
| ----------- | ---- | --------------- | -------------------------------------------- | ------------------ |
| Direct      | `00` | `N`             | `N`                                          | 0 (fetch only)     |
| Indirect    | `01` | `N,I`           | `MEM[N]`                                     | +1                 |
| Immediate   | `02` | `#N`            | `N` itself (the operand byte)                | 0                  |
| Indexed     | `03` | `N,X`           | `(N + RX) mod 256`                           | 0                  |

Important subtleties visible in [Ramses.fs:123-128](../fs/ArchSims.Core/Ramses.fs#L123-L128):

- **Immediate + branch**: when the instruction opcode is `>= JMP` (0x80), immediate mode is treated as direct — the operand byte *is* the jump address. There is no "jump to literal" vs "jump to addr" distinction in practice.
- **Indirect is single-level**: `MEM[operand-byte]` is the final address; no further dereferencing.
- **Indexed** wraps modulo 256 because `RX + operand` is computed in 8 bits.

**UI tip** — when disassembling, show the effective address next to the mnemonic. The disassembler already produces e.g. `LDR A 123` vs `LDR A 123,I` vs `LDR A #123` vs `LDR A 123,X`. Supplementing that with `→ eff. addr = 146` during single-step makes indexed/indirect semantics tangible.

## 5. Execution Cycle

`Step cpu` performs `Fetch` then `Execute`. No interrupts, no cycle counting — one instruction per step.

**Fetch** ([Ramses.fs:95](../fs/ArchSims.Core/Ramses.fs#L95)):

1. Read byte at `PC`, store in `IR.OpCode`, increment `PC`.
2. If the instruction has an operand, compute the effective address according to the mode and store it in `IR.OperandAddress`. This may consume one more byte from `PC`.

**Execute** ([Ramses.fs:131](../fs/ArchSims.Core/Ramses.fs#L131)):

1. Decode register and instruction from `IR.OpCode`.
2. Perform the operation, update the target register/memory and the flags as per the instruction table.
3. Set `Halted` iff the instruction was `HLT`.

The reference implementation never throws on "invalid" opcodes — any byte whose high-nibble is not a recognized instruction is treated as `NOP`/`HLT`. The CLI's `RunningForever` stop reason after 1000 instructions ([fs/ArchSims.Core/Debugger.fs](../fs/ArchSims.Core/Debugger.fs)) guards against infinite loops.

## 6. Assembly Language

Parser: FParsec-based, defined in [RamsesAssembler.fs](../fs/ArchSims.Assemblers/RamsesAssembler.fs).

### 6.1 Lexical rules

- Case-insensitive mnemonics and register names.
- Labels start with `:` and follow with one letter then letters/digits: `:Start`, `:Loop1`.
- Comments start with `;` and run to end of line.
- Whitespace and comments separate tokens freely.

### 6.2 Directives / pseudo-ops

| Syntax        | Meaning                                               |
| ------------- | ----------------------------------------------------- |
| `@N`          | Set assembly cursor to address `N` (0..255)           |
| `:Label`      | Define a label at the current cursor                  |
| `N`           | A bare unsigned byte is emitted as a data constant    |

### 6.3 Instruction syntax

```
NOP | HLT
NOT r | NEG r | SHR r                        r ∈ {A, B, X, PC}
JMP op | JN op | JZ op | JC op | JSR op
STR r, op | LDR r, op | ADD r, op
OR  r, op | AND r, op | SUB r, op
```

Operand `op` is one of:

| Form     | Mode      | Example       |
| -------- | --------- | ------------- |
| `N`      | Direct    | `LDR A 10`    |
| `N,I`    | Indirect  | `LDR A 10,I`  |
| `#N`     | Immediate | `LDR A #10`   |
| `N,X`    | Indexed   | `LDR A 10,X`  |
| `:Label` | Direct    | `JMP :Loop`   |
| `:Label,I`, `:Label,X`, `#:Label` | as above, with the label's address as `N` |

Forward label references are resolved in a single deferred pass ([RamsesAssembler.fs:114](../fs/ArchSims.Assemblers/RamsesAssembler.fs#L114)). Duplicate or unresolved labels raise a Portuguese error message (`Label duplicado`, `Label indefinido`).

### 6.4 Example — `BitShift.Ramses.txt`

```
:StartRA
    LDR A #128
:RepeatRA
    SHR A
    JZ :StartRB
    JMP :RepeatRA
:StartRB
    LDR B #128
:RepeatRB
    SHR B
    JZ :StartRX
    JMP :RepeatRB
:StartRX
    LDR X #128
:RepeatRX
    SHR X
    JZ :Finish
    JMP :RepeatRX

:Finish
    HLT
```

Loads 0x80 into each register in turn and shifts right until zero — a visually obvious progression that makes a good first-launch demo for any UI.

## 7. Running a Program

CLI entry point: [fs/ArchSims.CmdLine/Main.fs](../fs/ArchSims.CmdLine/Main.fs) dispatches to [RamsesCmdLine.fs](../fs/ArchSims.CmdLine/RamsesCmdLine.fs).

```
ArchSims.CmdLine.exe BitShift.Ramses.txt -Cpu Ramses -Mode Interactive -Output Decimal -Speed 10
```

Output line format (decimal):

```
Ra:  0 Rb:  0 Rx:  0 PC:  2 IR: [LDR A 128  ] -Z-
```

- Three flag slots in fixed order: `N Z C` (each either the letter or `-`).
- `IR` column shows the disassembled current/last instruction.
- Binary output prints each register as eight decimal 0/1 digits (one line per register), useful for teaching bit operations.

Save mode produces a binary file with prefix `0x03 'R' 'M' 'S'` followed by the 256 bytes of memory — compatible with the historical Ramses `.mem` format.

## 8. UI Design Suggestions

A few recommendations that follow directly from the architecture:

1. **Fit on one screen.** 256 bytes fit comfortably in a 16×16 hex grid. Highlight the current `PC`, last read, last written (three distinct colors). The core already exposes `ReadCount`/`WriteCount` per step if you want a "changed cells" animation.
2. **Register panel.** Show `RA`, `RB`, `RX`, `PC` as four small tiles. Render each value in three bases simultaneously (hex, decimal, binary) — the architecture is small enough that showing all three costs nothing.
3. **Flag lamps.** Three flags plus `Halted`. Model them as LEDs. Remember `Z` starts lit.
4. **Instruction register view.** The IR holds both the opcode byte and the effective address. Decode it visually: split the opcode byte into `IIII / RR / MM` fields, color each bit-group, and show the mnemonic + effective operand underneath. This single widget explains the encoding to a student in a way a table cannot.
5. **Disassembly column.** `DisassembleInstructions` (in `Ramses.fs`) already walks a byte list and returns `(text, size)` pairs. Use it to render an "assembly view" alongside the hex grid and mark which bytes belong to the instruction currently in the IR.
6. **Step vs Run.** The debugger API (`DebuggerStep`, `DebuggerRun`, `DebuggerSetBreakpoint`, `DebuggerClearBreakpoint`) matches a typical UI triad of Step / Run / Break. The `RunningForever` sentinel after N steps gives you a natural "slow mode" progress indicator.
7. **Addressing-mode explainer.** When the user hovers an instruction with `,I`, `#`, or `,X`, draw an arrow from the instruction's operand byte to the effective memory cell. This is the single hardest concept in the Neander → Ramses step and a visual arrow teaches it in seconds.
8. **Assemble-on-the-fly.** `AssembleInstruction` accepts a single line and returns the bytes. A UI could expose this as "type a mnemonic, see its bytes appear" — a great exploration tool for the encoding table.
9. **No stack means no call tree.** `JSR` self-modifies memory. Visualizing it as an arrow that writes a return byte *into the called routine* (rather than "pushes") is honest and instructive.
10. **Speed control.** The CLI exposes `Speed` as instructions-per-second; keep that for parity and expose 1, 10, 100, 1000 presets plus a "as fast as possible" toggle.
