# Ahmes — Architecture Manual

> **Implementation status.** Ahmes is **not yet implemented** in this codebase. The only trace of it is the `CpuType.Ahmes` slot in [fs/ArchSims.CmdLine/Common.fs:13](../fs/ArchSims.CmdLine/Common.fs#L13) and the stub in [fs/ArchSims.CmdLine/Main.fs:78](../fs/ArchSims.CmdLine/Main.fs#L78) that raises `Not implemented: Ahmes`. There is no `Ahmes.fs` core module, no assembler, and no test suite. This manual describes the canonical UFRGS Ahmes specification so that a future implementation and its UI can be built against a shared reference.
>
> Where this document makes concrete bit-level claims (opcodes, flag masks), they follow the published UFRGS teaching materials and the consistent encoding pattern of Neander → Ahmes → Ramses. Before committing an implementation, cross-check specific opcode sub-values against the course's reference binaries and sample programs.

## 1. Overview

Ahmes is a strict extension of Neander — same 8-bit word, same 256-byte memory, same single accumulator, same single addressing mode. What it adds is **arithmetic breadth**: subtraction, shift and rotate instructions, a carry/borrow/overflow flag trio, and a full set of conditional jumps keyed on those new flags.

Think of Ahmes as "Neander for people who have discovered that one flag is not enough to do multi-byte arithmetic." Everything a programmer wrote for Neander still runs on Ahmes; the new instructions open up multi-precision arithmetic, bit-level manipulation, and signed comparisons.

Family position:

| Machine | New capability over predecessor                                         |
| ------- | ----------------------------------------------------------------------- |
| Neander | Baseline: single accumulator, one addressing mode, N/Z flags            |
| **Ahmes** | **`SUB`, shifts, rotates; `C`, `V`, `B` flags; extended conditional jumps** |
| Ramses  | Multiple registers (`RA`/`RB`/`RX`), four addressing modes, `JSR`       |
| Cesar   | 16-bit, eight registers, eight addressing modes, stack, memory-mapped I/O |

Because Ahmes preserves Neander's "one accumulator, one addressing mode" simplicity, a student's mental model carries over intact — the only new lesson is "more flags, more arithmetic, more branches."

## 2. Programmer's Model

### 2.1 Registers

Identical to Neander:

| Register | Width  | Role                                |
| -------- | ------ | ----------------------------------- |
| `AC`     | 8 bits | Accumulator — the only data register |
| `PC`     | 8 bits | Program Counter — wraps 0xFF → 0x00  |

### 2.2 Instruction Register (IR)

Identical shape to Neander: `OpCode` byte and `OperandAddress` byte (`0` if the instruction takes no operand).

### 2.3 Flags

Ahmes has **five** status flags plus `Halted`:

| Flag       | Meaning                                                               | Initial value |
| ---------- | --------------------------------------------------------------------- | ------------- |
| `Negative` | Result's bit 7 is set (result > 0x7F)                                 | `false`       |
| `Zero`     | Result is 0                                                           | **`true`**    |
| `Carry`    | Unsigned arithmetic/shift carry out of bit 7                          | `false`       |
| `Overflow` | Signed arithmetic overflow                                            | `false`       |
| `Borrow`   | `SUB` produced a borrow (i.e., unsigned result went below 0)          | `false`       |
| `Halted`   | `HLT` just executed; cleared by next instruction                      | `false`       |

Notes:

- `Carry` and `Borrow` are distinct in Ahmes (unlike Ramses, which collapses them into one `C` flag where "carry set after SUB means no borrow"). Ahmes models each operation's intuitive flag: `ADD` produces a `Carry`, `SUB` produces a `Borrow`.
- `Overflow` uses the standard signed-overflow rule (same-signs-in, different-sign-out).
- Shifts and rotates use `Carry` to receive the bit shifted out.
- `N` and `Z` are updated by every instruction that writes `AC`.

Suggested UI: show five flag lamps in a row labelled `N Z V C B` plus a distinct `HLT` indicator. Keep the order consistent across Ahmes/Ramses/Cesar even though each machine uses a different subset — the visual pattern is part of the teaching.

### 2.4 Memory

Unchanged from Neander: 256 bytes, flat, byte-addressable, no I/O mapping, no stack.

## 3. Instruction Set

Ahmes keeps all eleven Neander instructions **with the same opcodes** and adds new ones in the previously-unused opcode ranges.

### 3.1 Complete instruction table

Opcodes given as conventional UFRGS Ahmes encoding — high nibble is the primary selector, with sub-nibbles distinguishing jump-family variants and shift/rotate variants.

| Mnemonic | Opcode (hex) | Bytes | Operand? | Flags touched | Effect                         |
| -------- | ------------ | ----- | -------- | ------------- | ------------------------------ |
| `NOP`    | `0x00`       | 1     | no       | —             | Do nothing                     |
| `STA a`  | `0x10`       | 2     | yes      | —             | `MEM[a] ← AC`                  |
| `LDA a`  | `0x20`       | 2     | yes      | N, Z          | `AC ← MEM[a]`                  |
| `ADD a`  | `0x30`       | 2     | yes      | N, Z, **C, V**| `AC ← AC + MEM[a]`             |
| `OR  a`  | `0x40`       | 2     | yes      | N, Z          | `AC ← AC OR MEM[a]`            |
| `AND a`  | `0x50`       | 2     | yes      | N, Z          | `AC ← AC AND MEM[a]`           |
| `NOT`    | `0x60`       | 1     | no       | N, Z          | `AC ← NOT AC`                  |
| `SUB a`  | `0x70`       | 2     | yes      | N, Z, **B, V**| `AC ← AC − MEM[a]`             |
| `JMP a`  | `0x80`       | 2     | yes      | —             | `PC ← a`                       |
| `JN  a`  | `0x90`       | 2     | yes      | —             | If `N=1`, `PC ← a`             |
| `JP  a`  | `0x94`       | 2     | yes      | —             | If `N=0` (positive), `PC ← a`  |
| `JV  a`  | `0x98`       | 2     | yes      | —             | If `V=1`, `PC ← a`             |
| `JNV a`  | `0x9C`       | 2     | yes      | —             | If `V=0`, `PC ← a`             |
| `JZ  a`  | `0xA0`       | 2     | yes      | —             | If `Z=1`, `PC ← a`             |
| `JNZ a`  | `0xA4`       | 2     | yes      | —             | If `Z=0`, `PC ← a`             |
| `JC  a`  | `0xB0`       | 2     | yes      | —             | If `C=1`, `PC ← a`             |
| `JNC a`  | `0xB4`       | 2     | yes      | —             | If `C=0`, `PC ← a`             |
| `JB  a`  | `0xB8`       | 2     | yes      | —             | If `B=1`, `PC ← a`             |
| `JNB a`  | `0xBC`       | 2     | yes      | —             | If `B=0`, `PC ← a`             |
| `SHR`    | `0xE0`       | 1     | no       | N, Z, **C**   | Logical shift right: `C ← AC[0]`; `AC ← AC >> 1`; `AC[7] ← 0` |
| `SHL`    | `0xE1`       | 1     | no       | N, Z, **C**   | Logical shift left: `C ← AC[7]`; `AC ← AC << 1`; `AC[0] ← 0` |
| `ROR`    | `0xE2`       | 1     | no       | N, Z, **C**   | Rotate right through carry: `tmp ← C; C ← AC[0]; AC ← AC >> 1; AC[7] ← tmp` |
| `ROL`    | `0xE3`       | 1     | no       | N, Z, **C**   | Rotate left through carry: `tmp ← C; C ← AC[7]; AC ← AC << 1; AC[0] ← tmp` |
| `HLT`    | `0xF0`       | 1     | no       | Halted        | Stops the CPU                  |

Caveats to verify against UFRGS reference material before implementing:

- **Jump sub-opcodes.** The canonical Ahmes encoding uses the low two bits of the conditional-jump opcodes to select `positive`, `negative`, and the "is/not" flavors. The values above (`0x94`, `0x9C`, `0xA4`, `0xB4`, `0xB8`, `0xBC`) follow the bit-pattern convention `1001 FF 00`/`1010 F0 00`/`1011 FF 00` where `F` is the flag and the `0`/`1` selects "jump if set" / "jump if clear". Confirm with a course binary before shipping.
- **Shift/rotate sub-opcodes.** The `0xE0..0xE3` block is the common layout: `SHR`, `SHL`, `ROR`, `ROL` in sub-opcode order. Some materials reverse `ROR`/`ROL` — verify.
- **Flag rules for `SUB`.** UFRGS materials differ slightly on whether `SUB` sets `Carry` (as "inverted borrow", like Ramses) or leaves `Carry` alone and sets `Borrow`. The most common specification is the latter: `ADD` → `C`, `SUB` → `B`, and they are independent.

### 3.2 Addressing

Single addressing mode — direct, identical to Neander. The operand byte *is* the address; no indirect, no immediate, no indexed.

To use an immediate value, store it as a data byte in memory and `LDA` it into `AC`. To compute a multi-byte value, build it up byte by byte using `SUB`/`ADD` with memory-resident operands.

## 4. Execution Cycle

Identical structure to Neander:

1. **Fetch** — read opcode at `PC`, advance `PC`. If the opcode takes an operand, read and store it, advance `PC` again.
2. **Execute** — dispatch on the opcode, perform the operation, update flags, set `Halted` iff the instruction was `HLT`.

For shifts and rotates, the implementation must:

- Read `AC` before mutation.
- Save the outgoing bit to `Carry`.
- For rotates, read the incoming bit from `Carry` **before** overwriting it with the outgoing bit.
- Update `N`/`Z` from the final `AC`.

For `ADD` and `SUB`:

- Compute the result in 9 bits (i.e., as an `int`) so `Carry`/`Borrow` is available as the 9th bit.
- Compute `Overflow` from the standard signed-overflow rule: `V = (sign(A) == sign(B)) ∧ (sign(A) != sign(result))` for `ADD`; `V = (sign(A) != sign(B)) ∧ (sign(A) != sign(result))` for `SUB`.
- Truncate to 8 bits and write to `AC`.
- Update `N`/`Z`.

The `Halted` flag follows the Neander convention: set at end of `HLT` execution, cleared by the next instruction ([Neander.fs:132](../fs/ArchSims.Core/Neander.fs#L132) for the pattern Ahmes should mirror). The debugger sees `Halted=true` after the step and stops the run loop; a UI should render it as a "just stopped" state, not a permanent one.

## 5. Programming Notes

- **Two-byte addition.** With `SUB`, `ADD`, and `C`, Ahmes programs can do 16-bit unsigned arithmetic: add the low bytes, propagate `C` into the high-byte add by branching on `JC`. This is the first UFRGS simulator where multi-precision arithmetic is feasible.
- **Signed comparison.** `SUB` followed by `JN` compares for "less than (signed, no overflow)". With `V` also considered, students can build the equivalents of `JLT`/`JGE` seen later in Cesar.
- **Shift = multiply/divide by two.** `SHL` once is `×2` (with `C` catching overflow); `SHR` once is unsigned `÷2`. `ROR`/`ROL` through `C` are what you use when chaining shifts across multiple bytes.
- **No `SAR` (arithmetic shift right).** To divide a signed number by two with sign preservation, the programmer must first copy bit 7 and reapply it by hand — a nice exercise that motivates Cesar's `ASR`.

## 6. Small Example Programs

Ahmes does not yet have a shipping assembler in this repo. Programs should be shown as hand-assembled bytes, in the Neander style.

### 6.1 16-bit addition

Add the 16-bit number at `[0xE0..0xE1]` (big-endian: high byte at 0xE0, low at 0xE1) to the 16-bit number at `[0xE2..0xE3]`; store result at `[0xE4..0xE5]`.

```
Address  Bytes        Source                  Comment
-------  -----------  ---------------------   ------------------------------------
  0      0x20 0xE1    LDA 0xE1                AC ← low(A)
  2      0x30 0xE3    ADD 0xE3                AC ← AC + low(B); C = low carry-out
  4      0x10 0xE5    STA 0xE5                store low byte of result
  6      0xB4 0x10    JNC 0x10                if no carry, skip the +1 on high byte
  8      0x20 0xE0    LDA 0xE0                AC ← high(A)
 10      0x30 0xF0    ADD 0xF0                AC ← AC + 1   (constant 1 at 0xF0)
 12      0x30 0xE2    ADD 0xE2                AC ← AC + high(B)
 14      0x80 0x14    JMP 0x14
 16      0x20 0xE0    LDA 0xE0                no-carry path: just add the highs
 18      0x30 0xE2    ADD 0xE2
 20      0x10 0xE4    STA 0xE4
 22      0xF0         HLT
 0xF0    0x01                                 constant 1
```

Non-trivial. The point is that it is *possible* on Ahmes and was not possible on Neander.

### 6.2 Sign-preserving divide by two

```
Address  Bytes        Source                  Comment
-------  -----------  ---------------------   -------------------------------
  0      0x20 0x10    LDA 0x10                AC ← value
  2      0x90 0x0A    JN  0x0A                if negative, go to negative path
  4      0xE0         SHR                     non-negative: shift once
  5      0x10 0x10    STA 0x10
  7      0xF0         HLT
  ...
 10      0xE0         SHR                     negative: shift...
 11      0x50 0x11    AND 0x11                ... (workaround to re-set bit 7
 13      0x40 0x12    OR  0x12                     using OR 0x80 after SHR)
 15      0x10 0x10    STA 0x10
 17      0xF0         HLT
 0x10    0xF0                                 example value = −16 (signed)
 0x11    0x7F                                 mask
 0x12    0x80                                 sign bit
```

Showing the student why the next machine (Cesar, with `ASR`) bothers having a dedicated arithmetic shift.

## 7. UI Design Suggestions

Ahmes looks almost exactly like Neander on screen — the UI should emphasize the differences rather than redecorate what is already there.

1. **Extend the flag row.** Use the same lamp component as Neander but add `C`, `V`, `B`. Keep `N Z V C B` order for consistency with Ramses (`N Z _ C`) and Cesar (`N Z V C`). When the current instruction cannot touch a flag, fade its lamp; it prevents the false impression that e.g. `OR` updated `V`.
2. **Instruction-category badge.** Since the instruction set grew to 24 opcodes, color-code them in the disassembly: Neander-original (teal), arithmetic extension (orange), shift/rotate (purple), extended jumps (yellow). Students recognize "this is new" at a glance.
3. **Flag-change indicator.** On each step, briefly flash any flag lamp that changed state. In an ISA where `SUB` touches `N`/`Z`/`V`/`B` but not `C`, seeing *exactly* which lamps blink per instruction builds correct intuition faster than a cheatsheet.
4. **Carry/Borrow distinction.** Because Ahmes distinguishes `C` from `B`, make the two lamps visually distinct (different icon, not just different position). A student who has internalized Ramses's single `C` may otherwise expect one to feed the other.
5. **16-bit scratchpad widget.** Many Ahmes teaching exercises produce two-byte results. A small "joined cell" overlay in the memory grid — "these two bytes form a 16-bit value, here's its decimal/signed interpretation" — makes multi-byte arithmetic tractable without forcing mental hex arithmetic.
6. **Shift/rotate animation.** The shift/rotate instructions beg for an 8-cell bit strip widget showing `AC`'s bits, with an arrow from the outgoing end into `C` and (for rotates) from `C` back into the opposite end. This single animation conveys the difference between `SHR`/`SHL`/`ROR`/`ROL` more clearly than any prose.
7. **Conditional-jump predicate tooltip.** For every `Jxx`, show the predicate on hover (`JNC → if C=0, PC ← a`). Given that there are ten conditional-jump mnemonics, this drastically reduces look-up friction.
8. **Disassembler.** Following the pattern of `DisassembleInstruction` in [Ramses.fs:221](../fs/ArchSims.Core/Ramses.fs#L221), an Ahmes disassembler should be straightforward. It needs to decode the four-way sub-opcode switch for conditional jumps and the four-way sub-opcode switch for shifts/rotates — the only non-trivial decoding in the ISA.
9. **Binary file format.** If an Ahmes `.mem` loader is added later, follow the family convention: four-byte ASCII prefix `^C A H M` (`0x03 'A' 'H' 'M'`), then the 256 memory bytes. Cf. [RamsesCmdLine.fs:62](../fs/ArchSims.CmdLine/RamsesCmdLine.fs#L62) and [CesarCmdLine.fs:74](../fs/ArchSims.CmdLine/CesarCmdLine.fs#L74) — each simulator has a distinct prefix so loaders can refuse a mismatched file.
10. **Borrowing Neander UI.** Ahmes shares Neander's memory layout, cycle, and accumulator. A well-factored UI should reuse the memory grid, IR widget, register tiles, and debugger wiring from the Neander frontend, parameterizing only the flag row, the opcode decoder, and the disassembler. This also makes it easier to present "the same program running on Neander" side by side to show where Ahmes's new flags and instructions help.

## 8. Implementation Checklist (for a Future `Ahmes.fs`)

Because no core module exists yet, a concrete starting plan:

1. **`Ahmes.fs`** modelled on [Neander.fs](../fs/ArchSims.Core/Neander.fs): same `CreateCpu`, `Fetch`, `Execute`, `Step`, `Reset` signatures. Add `Carry`, `Overflow`, `Borrow` to `Flags`.
2. Instruction enum with all opcodes above. Consider a nested `ShiftOp` / `JumpOp` enum for the sub-opcode families, or match on the full byte — whichever keeps the pattern match legible.
3. **`AhmesTests.fs`** covering every instruction, flag combination, `PC` wraparound, and `Halted` semantics — following the shape of [NeanderTests.fs](../fs/ArchSims.Core.Tests/NeanderTests.fs) and [RamsesTests.fs](../fs/ArchSims.Core.Tests/RamsesTests.fs).
4. **`AhmesAssembler.fs`** using FParsec, following [RamsesAssembler.fs](../fs/ArchSims.Assemblers/RamsesAssembler.fs) with register parsing removed and shift/rotate mnemonics added.
5. **`AhmesCmdLine.fs`** following [RamsesCmdLine.fs](../fs/ArchSims.CmdLine/RamsesCmdLine.fs); add `C`, `V`, `B` to the printed flag line.
6. Wire the new entry point in [Main.fs:78](../fs/ArchSims.CmdLine/Main.fs#L78) replacing the `failwith`.
7. Verify against a canonical Ahmes reference program (the UFRGS course repository ships sample `.mem` binaries) before considering the implementation complete.
