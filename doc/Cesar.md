# Cesar — Architecture Manual

## 1. Overview

Cesar is the most complex of the UFRGS teaching machines: a 16-bit computer with eight registers, eight addressing modes, a two-operand instruction set, status flags with overflow, a call/return mechanism backed by a real stack, and memory-mapped keyboard/display areas. Its design is a pared-down PDP-11 — enough detail to teach "real" assembly without the weight of a production ISA.

Highlights:

- 16-bit word; 64 KiB (65 536 bytes) of byte-addressable memory; **big-endian** word storage.
- Eight general-purpose 16-bit registers `R0`..`R7`; `R7` **is** the Program Counter; `R6` is the Stack Pointer by convention (used by `JSR`/`RTS`).
- Eight addressing modes, uniform across instructions (including against `R7`, which gives you "immediate" and "absolute" for free).
- Flags `N`, `Z`, `V`, `C` plus a `Halted` bit.
- Memory-mapped I/O: the top 38 bytes of memory (0xFFDA..0xFFFF) are accessed at **byte** granularity; keyboard input is mapped at 0xFFDA, the display starts at 0xFFDC.

Source of truth: [fs/ArchSims.Core/Cesar.fs](../fs/ArchSims.Core/Cesar.fs), [fs/ArchSims.Core/Memory.fs](../fs/ArchSims.Core/Memory.fs), [fs/ArchSims.Assemblers/CesarAssembler.fs](../fs/ArchSims.Assemblers/CesarAssembler.fs).

## 2. Programmer's Model

### 2.1 Registers

| Register | Width   | Conventional role                    |
| -------- | ------- | ------------------------------------ |
| `R0`..`R5` | 16 bits | General-purpose                    |
| `R6`     | 16 bits | Stack Pointer (used by `JSR`/`RTS`)  |
| `R7`     | 16 bits | Program Counter                      |

`R7` is the PC *literally* — it is read and written through the normal register mechanism, and every addressing-mode combination that acts on `R7` has a programmer-visible meaning (see §4.2).

All registers are zero after reset. The 16-bit word is stored in memory **big-endian** (`MemoryReadWordBigEndian` / `MemoryWriteWordBigEndian` in [Memory.fs](../fs/ArchSims.Core/Memory.fs)).

### 2.2 Flags

| Flag       | Bit mask | Meaning                                              |
| ---------- | -------- | ---------------------------------------------------- |
| `Negative` | 0b1000   | Result's top bit (result > 0x7FFF)                   |
| `Zero`     | 0b0100   | Result = 0                                           |
| `Overflow` | 0b0010   | Signed arithmetic overflow                           |
| `Carry`    | 0b0001   | Unsigned arithmetic overflow (for `SUB`/`CMP`, "no borrow") |

Also: `Halted` — set by `HLT`, cleared by every subsequent instruction (see §5.5). The initial state has `Z = true`, all others `false`.

`CCC` and `SCC` expose the flags directly by letting the programmer choose which bits to clear or set — the low four bits of the opcode select flags by the same mask shown above.

### 2.3 Instruction Register (IR)

Unlike Ramses, the Cesar IR stores the *entire* encoded instruction, including any extra bytes consumed during decoding of addressing modes against `R7` (immediate operands, absolute addresses, indexing offsets). It is a `byte array` of length 1..6.

It also stores the decoded `SourceOperand` and `TargetOperand` as an `Operand` union:

- `NoOp` — not present,
- `Reg r` — the value lives in register `r`,
- `Addr a` — the value lives at memory address `a`.

This is programmer-visible via the CLI and should be surfaced in any UI: it captures "what this instruction is actually going to touch" in a single structure.

### 2.4 Memory layout

```
0x0000  ┌───────────────────────────┐
        │                           │
        │   Program + data + stack  │   Word-addressable (big-endian)
        │                           │
0xFFD9  ├───────────────────────────┤
0xFFDA  │  Keyboard (byte mapped)   │   2 bytes
0xFFDC  │  Display  (byte mapped)   │   36 bytes (0xFFDC..0xFFFF)
0xFFFF  └───────────────────────────┘
```

Addresses < 0xFFDA: reads and writes are 16-bit word operations (the low address byte and the next byte together form one word). Addresses ≥ 0xFFDA: reads and writes degrade to 8-bit byte operations — Cesar zero-extends on read and truncates on write ([Cesar.fs:297-315](../fs/ArchSims.Core/Cesar.fs#L297-L315)).

UI implication: the display area is 36 bytes of ASCII; drawing them as a fixed 36-character strip (or 6×6 / 4×9 grid) gives you a working "screen" that Cesar programs can print to with a single `MOV`. The keyboard pair at 0xFFDA is the conventional place to pipe a typed character into the simulator.

`R6` (stack pointer) is initialized to 0 after reset, which means the first `JSR` decrements it to 0xFFFE — already inside the memory-mapped area on the byte side of 0xFFDA only if you push very deep. Real programs normally start with `MOV #something, R6` to set the stack.

## 3. Instruction Set

Instructions are grouped by the high nibble of the first opcode byte. Within a group, the low nibble (for the one-operand and branch groups) or the lower byte (for two-operand instructions) select sub-operations or operands.

| High nibble | Group                   | Mnemonics                                                   |
| ----------- | ----------------------- | ----------------------------------------------------------- |
| `0x0`       | NOP                     | `NOP`                                                       |
| `0x1`       | Clear flags             | `CCC [NZVC]`                                                |
| `0x2`       | Set flags               | `SCC [NZVC]`                                                |
| `0x3`       | Conditional branches    | `BR`, `BNE`, `BEQ`, `BPL`, `BMI`, `BVC`, `BVS`, `BCC`, `BCS`, `BGE`, `BLT`, `BGT`, `BLE`, `BHI`, `BLS` |
| `0x4`       | Jump                    | `JMP`                                                       |
| `0x5`       | Subtract-and-branch     | `SOB`                                                       |
| `0x6`       | Subroutine call         | `JSR`                                                       |
| `0x7`       | Subroutine return       | `RTS`                                                       |
| `0x8`       | One-operand ALU         | `CLR`, `NOT`, `INC`, `DEC`, `NEG`, `TST`, `ROR`, `ROL`, `ASR`, `ASL`, `ADC`, `SBC` |
| `0x9`       | `MOV`                   | `MOV src, dst`                                              |
| `0xA`..`0xE`| Two-operand ALU         | `ADD`, `SUB`, `CMP`, `AND`, `OR`                            |
| `0xF`       | Halt                    | `HLT`                                                       |

### 3.1 Encoding

- **No-operand** (`NOP`, `RTS`, `HLT`): one byte. `RTS` embeds a register number in its low three bits (the register that will receive the return address).
- **Flag ops** (`CCC`, `SCC`): one byte; the low four bits are a flag mask.
- **Branches**: two bytes. Byte 0 = opcode, byte 1 = 8-bit signed displacement relative to the address *after* the branch.
- **`SOB`**: two bytes. Low three bits of byte 0 select the register; byte 1 = 8-bit **positive** displacement (always subtracts, i.e. branches backward).
- **One-operand** (`JMP`, `CLR` group): two bytes plus 0..2 extra bytes of immediate/absolute data, depending on the target's addressing mode. Layout:

    ```
    byte 0: i i i i i i i i   (opcode)
    byte 1: 0 0 m m m r r r   (target mode, target register)
    ```

- **Two-operand** (`MOV`, `ADD`, `SUB`, `CMP`, `AND`, `OR`): two bytes plus up to 4 extra bytes:

    ```
    byte 0: i i i i M M M R   (opcode + source mode + top bit of source register)
    byte 1: R R m m m r r r   (rest of source register, target mode, target register)
    ```

    `EncodeInstructionOneOperand` and `EncodeInstructionTwoOperand` in [Cesar.fs:148-166](../fs/ArchSims.Core/Cesar.fs#L148-L166) are the canonical encoders; use them rather than reconstructing the layout by hand.

### 3.2 Branch semantics

All 15 conditional branches (`BNE`..`BLS`) plus the unconditional `BR` use a **signed 8-bit displacement**, giving a range of −128..+127 bytes from the *next* instruction. The assembler enforces this range and raises `Label inacessível a partir de um branch` when a forward/backward label is out of reach ([CesarAssembler.fs:252](../fs/ArchSims.Assemblers/CesarAssembler.fs#L252)).

Condition decoding (see [Cesar.fs:333-359](../fs/ArchSims.Core/Cesar.fs#L333-L359)):

| Mnemonic | Condition                   | Meaning (source-level)           |
| -------- | --------------------------- | -------------------------------- |
| `BR`     | always                      | unconditional                    |
| `BNE`/`BEQ` | `Z` clear / set           | not equal / equal                |
| `BPL`/`BMI` | `N` clear / set           | positive / minus                 |
| `BVC`/`BVS` | `V` clear / set           | signed overflow clear / set      |
| `BCC`/`BCS` | `C` clear / set           | unsigned: no carry / carry       |
| `BGE`/`BLT` | `N = V` / `N ≠ V`         | signed ≥ / <                     |
| `BGT`/`BLE` | `(N = V) ∧ ¬Z` / `(N ≠ V) ∨ Z` | signed > / ≤                |
| `BHI`/`BLS` | `¬C ∧ ¬Z` / `C ∨ Z`       | unsigned > / ≤                   |

### 3.3 `JMP`

`JMP` takes a one-operand target but forbids pure-register mode (`mode = 0`) — jumping "into" a register has no meaning. The implementation silently does nothing in that case ([Cesar.fs:361-364](../fs/ArchSims.Core/Cesar.fs#L361-L364)); the disassembler prints `JMP ?`.

### 3.4 `SOB` (Subtract One and Branch if non-zero)

Single-instruction countdown loop. Byte 1 is an **unsigned** displacement **subtracted** from the next-instruction address, so `SOB` branches backward only — ideal for loop bodies. Stops branching when the register reaches zero.

### 3.5 `JSR` / `RTS`

`JSR r, target`:

1. `R6 ← R6 − 2` (decrement stack pointer)
2. `MEM[R6] ← r` (push the "link" register; usually `R7` for PC-linking)
3. `r ← R7` (save return address in the link register)
4. `R7 ← effective-address(target)` (jump)

`RTS r`:

1. `R7 ← r` (restore PC from link register)
2. `r ← MEM[R6]` (pop the saved link)
3. `R6 ← R6 + 2`

The idiom is `JSR R7, :Sub` paired with `RTS R7` — the link register doubles as the return path, so the stack frame is just the saved return address. Any other register can serve as link if the programmer wants to preserve it across the call.

### 3.6 One-operand ALU group (opcodes 0x80..0x8B)

| Mnemonic | Effect                                    | Flag rules (beyond N/Z) |
| -------- | ----------------------------------------- | ----------------------- |
| `CLR`    | `dst ← 0` (without reading it first)      | `C=0`, `V=0`            |
| `NOT`    | `dst ← ~dst`                              | `C=1`, `V=0`            |
| `INC`    | `dst ← dst + 1`                           | `C` on wrap-around, `V` on signed overflow |
| `DEC`    | `dst ← dst − 1`                           | `C=1` iff no borrow, `V` on signed overflow |
| `NEG`    | `dst ← −dst`                              | `C=1` unless dst was 0, `V` for 0x8000 |
| `TST`    | reads `dst`, writes nothing               | `C=0`, `V=0`            |
| `ROR`/`ROL` | 17-bit rotate through `C`              | `V ← C XOR N` (post-op) |
| `ASR`/`ASL` | Arithmetic shift right/left            | `V ← C XOR N` (post-op); `N` flipped (sign-preserve heuristic) |
| `ADC`    | `dst ← dst + C`                           | standard add            |
| `SBC`    | `dst ← dst − 1 + C` (subtract with inverted carry) | standard subtract   |

Note: `CLR` and `TST` are carefully special-cased — `CLR` does not read the operand first, `TST` does not write the result back. UIs should reflect that: `CLR` reads 0 memory words for the operand, `TST` reads but doesn't mark the cell as "written".

### 3.7 Two-operand ALU group (`MOV`, `ADD`, `SUB`, `CMP`, `AND`, `OR`)

All read both operands (except `MOV`, which does not read the target), compute a result, and write it back to the target (except `CMP`, which does not write). They all update `N` and `Z`; `ADD`/`SUB`/`CMP` also update `V` and `C` via the shared `aluAdd` helper; `MOV`/`AND`/`OR` explicitly zero `V`. `SUB` and `CMP` invert carry after the add-of-complement trick so that `C=1` corresponds to "no borrow".

## 4. Addressing Modes

Eight modes, uniformly available for both source and target operands. Encoded in a 3-bit field; the full 6-bit `mmm rrr` selector is used per operand.

| # | Mode                  | Bits  | Assembly   | Effective-operand semantics (for reg `Rr`)       | Extra bytes in IR (when `Rr = R7`) |
| - | --------------------- | ----- | ---------- | ------------------------------------------------ | ---------------------------------- |
| 0 | Register              | `000` | `R0`       | Operand **is** `Rr`                              | 0                                  |
| 1 | Register post-increment | `001` | `(R0)+`  | `Addr ← Rr; Rr ← Rr + 2`                         | +2 (immediate when `Rr = R7`)      |
| 2 | Register pre-decrement  | `010` | `-(R0)`  | `Rr ← Rr − 2; Addr ← Rr`                         | 0                                  |
| 3 | Indexed               | `011` | `N(R0)`    | Read word `N` from PC-stream; `Addr ← Rr + N`    | +2 always                          |
| 4 | Register indirect     | `100` | `(R0)`     | `Addr ← Rr`                                      | 0                                  |
| 5 | Register post-increment indirect | `101` | `((R0)+)` | `Addr ← MEM[Rr]; Rr ← Rr + 2`          | +2 (absolute when `Rr = R7`)       |
| 6 | Register pre-decrement indirect  | `110` | `(-(R0))` | `Rr ← Rr − 2; Addr ← MEM[Rr]`           | 0                                  |
| 7 | Indexed indirect      | `111` | `(N(R0))`  | Read word `N` from PC-stream; `Addr ← MEM[Rr + N]` | +2 always                        |

### 4.1 Three useful aliases

From [Cesar.fs:77-79](../fs/ArchSims.Core/Cesar.fs#L77-L79):

- `Indirect` = `RegisterIndirect` alone — "go through this register as a pointer".
- `Immediate` = `RegPostInc` + `R7` — the next word in the instruction stream is the literal value.
- `Direct` (absolute) = `RegPostIncIndirect` + `R7` — the next word is the address of the value.

These three are the reason Cesar doesn't need dedicated immediate/absolute encoding: combining `R7` with the right addressing mode recycles the same orthogonal machinery.

### 4.2 R7 interactions — read carefully

- `R7` is incremented by `Fetch` before the operand is decoded. Post-increment on `R7` therefore reads the word *after* the opcode bytes and advances `R7` past it — this is immediate mode.
- `RegPreDec` on `R7` moves the PC **backward**, which is almost never useful except for writing self-tests. The disassembler still prints `-(R7)` and the CPU executes it faithfully — the address ends up inside the instruction itself ([Cesar.fs tests, line 239-241](../fs/ArchSims.Core.Tests/CesarTests.fs#L239-L241)).
- `Indexed` on `R7` is a convenient way to build PC-relative data references without labels.
- `RegPostIncIndirect` on `R7` = absolute addressing. The next word is the address.

The test file [fs/ArchSims.Core.Tests/CesarTests.fs](../fs/ArchSims.Core.Tests/CesarTests.fs#L220-L283) enumerates every combination; consult it before writing a UI that tries to "pretty-print" `R7` modes.

### 4.3 Assembly surface

The parser ([CesarAssembler.fs:64-74](../fs/ArchSims.Assemblers/CesarAssembler.fs#L64-L74)) accepts:

| Surface syntax | Mode                                |
| -------------- | ----------------------------------- |
| `R0`           | Register                            |
| `(R0)+`        | Post-increment                      |
| `-(R0)`        | Pre-decrement                       |
| `N(R0)`        | Indexed                             |
| `(R0)`         | Register indirect                   |
| `((R0)+)`      | Post-increment indirect             |
| `(-(R0))`      | Pre-decrement indirect              |
| `(N(R0))`      | Indexed indirect                    |
| `#N`           | Immediate (compiled as `(R7)+`)     |
| `N`            | Direct/absolute (compiled as `((R7)+)`) |
| `:Label`       | Direct/absolute via label           |

## 5. Execution Cycle

### 5.1 Fetch

`Fetch` ([Cesar.fs:171](../fs/ArchSims.Core/Cesar.fs#L171)):

1. Read byte at `R7`, store as first byte of the IR, increment `R7`.
2. Classify the instruction from the high nibble.
3. If it takes operand bytes, read the second byte, append to IR, decode addressing-mode fields.
4. For two-operand instructions, decode and evaluate the **source** operand first (which may consume extra PC-stream bytes for `R7`-relative modes).
5. Decode and evaluate the **target** operand.
6. Append any extra PC-stream bytes consumed to the IR so that the disassembler and UI can render the full instruction.

Operand evaluation at fetch time is an important choice: side effects like `R7 ← R7 + 2` happen **during** fetch, not execute. This matters if a UI wants to show "before fetch / after fetch / after execute" states distinctly.

### 5.2 Execute

`Execute` dispatches on the high nibble and, within a sub-group, on the low nibble. See [Cesar.fs:280](../fs/ArchSims.Core/Cesar.fs#L280). Writes go to either a register (via `_r.[i]`) or memory (via `MemoryWriteWordBigEndian` / `MemoryWriteByte` depending on whether the address is below or at/above 0xFFDA).

### 5.3 ALU

All arithmetic goes through `aluAdd a b carryIn`:

- computes a 17-bit sum, sets `C` if it overflows,
- computes signed overflow `V` using the usual "same signs in, different sign out" rule,
- returns the 16-bit result.

`SUB` and `CMP` feed `~source + 1` through `aluAdd` with carry in and then invert the carry so `C=1` means "no borrow".

### 5.4 Stack and subroutines

`R6` is the stack pointer. `JSR`/`RTS` push/pop one word at a time. No other instruction implicitly touches `R6` — a UI does not need to draw a "stack view" at all unless the program uses `JSR`. When it does, highlighting `R6` and the words immediately above it in the memory map is the cheapest way to visualize the stack.

### 5.5 Halt semantics

`HLT` sets `_flags.Halted`. The bit is **cleared** by the next instruction executed (the last line of `Execute`). Translation: halted state is valid only *between* step calls. The debugger checks `Halted` after each step and stops the run loop when set. UIs should reflect `Halted` as the state after the step, not as a permanent CPU state — a user who presses "Step" after `HLT` will see the halt flag drop on the next cycle.

## 6. Assembly Language

Parser: [CesarAssembler.fs](../fs/ArchSims.Assemblers/CesarAssembler.fs). Same general shape as the Ramses assembler.

### 6.1 Directives

| Syntax    | Meaning                                                        |
| --------- | -------------------------------------------------------------- |
| `@N`      | Set cursor to address `N` (0..65535)                           |
| `:Label`  | Define a label at the current cursor                           |
| `N`       | Emit a data constant: 1 byte if `0..255`, otherwise a 16-bit word |

Word constants are stored **big-endian** to match memory.

### 6.2 Instruction syntax cheatsheet

```
NOP | HLT
CCC [flags] | SCC [flags]              flags ::= [N][Z][V][C]
BR N | BR :Label                       (15 branch mnemonics, same shape)
JMP op
SOB Rn, N | SOB Rn, :Label
JSR Rn, op
RTS Rn
CLR op | NOT op | INC op | DEC op | NEG op | TST op
ROR op | ROL op | ASR op | ASL op | ADC op | SBC op
MOV src, dst
ADD src, dst | SUB src, dst | CMP src, dst
AND src, dst | OR  src, dst
```

Parsing is case-insensitive for mnemonics and register names; labels and comments follow the same rules as Ramses.

Negative numbers are accepted everywhere (`pint32`), but constants outside 0..255 are emitted as full words. Example: `-1` as a one-operand target in a `MOV` becomes the 16-bit word `0xFFFF`.

### 6.3 Example — `BitShift.Cesar.txt`

```
:Start
    MOV #32768, R0
:Loop
    ROR R0
    BNE -4           ; 252 / :Loop
    
    ROR R1           ; Advances carry to next register / Finishes execution when in R7
    INC 4            ; Next register (self-modifying code)
    INC 8
    
    BR :Loop
@32774
    HLT              ; End of program
```

This program shifts a 1-bit through every register in turn by self-modifying the `ROR` instruction's operand byte. The last shift lands in `R7`, which jumps to address 32774 where the `HLT` lives. Illustrates three Cesar-specific ideas at once: byte-level self-modifying code, `R7` as an ordinary register, and using `BR`'s signed displacement negatively (`-4`) as a shorthand for a backward jump.

## 7. Running a Program

CLI: [fs/ArchSims.CmdLine/CesarCmdLine.fs](../fs/ArchSims.CmdLine/CesarCmdLine.fs). Same command shape as Ramses:

```
ArchSims.CmdLine.exe BitShift.Cesar.txt -Cpu Cesar -Mode Interactive -Output Hexadecimal -Speed 500
```

Decimal output line:

```
R0:32768 R1:    0 R2:    0 R3:    0 R4:    0 R5:    0 R6:    0 PC:    3 IR: [MOV (R7)+, R0      ] -Z--
```

Binary mode prints two lines per step (4 registers each) plus the IR line.

Save mode writes memory with prefix `0x03 'C' '1' '6'` — the historical Cesar `.mem` format.

## 8. UI Design Suggestions

1. **Register view.** Eight 16-bit registers fit in a single column. Label `R6` as "SP" and `R7` as "PC" (or display both names). Show each value in hex and decimal. Highlight the one that just changed; highlight `R6` and `R7` whenever `JSR`/`RTS`/branch/jump fires.
2. **Flag lamps plus shortcuts.** Four flags (N, Z, V, C) and `Halted`. Consider adding two one-click buttons next to them that assemble `CCC NZVC` / `SCC NZVC` and step — a didactic way to show `CCC`/`SCC` in isolation.
3. **Memory view — three zones.** The 64 KiB address space benefits from a zoomable hex dump where:
    - program area (default focus) is shown as disassembled instructions alongside bytes,
    - the stack (memory around `R6`) is shown as a separate "stack" panel sliding upward from 0xFFD9,
    - the 36-byte display area (0xFFDC..0xFFFF) is a dedicated strip rendered as ASCII. Keyboard area (0xFFDA..0xFFDB) gets an input box that writes that byte on submit.
4. **Display as a first-class widget.** The `Samples/Demo-Cesar.cmd` expects programs that "print" to 0xFFDC by `MOV`. A text-mode display showing those 36 bytes live, updating on every memory write in that range, is the single most motivating UI element — it is the difference between "calculator simulator" and "computer".
5. **Disassembly gutter.** `DisassembleInstruction` returns `(text, size)` pairs. Render the program area as a three-column view: address, raw bytes, disassembly. Highlight the instruction currently in the IR (using `R7` minus the IR size before this step, because fetch has already advanced `R7`).
6. **Addressing-mode explainer.** Eight modes are the main conceptual hurdle. On hover over an operand, animate the effective-address derivation: show `(R3)+` decomposing into "read R3", "fetch MEM[R3]", "R3 ← R3 + 2". The `Operand` union (`NoOp` / `Reg` / `Addr`) already carries the answer — just display it.
7. **Two-operand arrows.** For `MOV src, dst` and friends, draw a ghost arrow from source operand's resolved location to target. `CMP` is a useful special case: arrow stops at the ALU and nothing flows to the target — makes "CMP doesn't write" obvious.
8. **Stack visualization.** Trigger only when `R6` is first written (or non-zero). Render the stack as an upward-growing column of words starting at `R6`. Distinct arrows for `JSR` (decrement + push) vs `RTS` (pop + increment) are clearer than a generic animation.
9. **`SOB` loop counter.** A short visualization — decrement the target register on each `SOB` execution, show how many iterations remain. Cesar programs use this constantly.
10. **Branch range hints.** When the user types an assembly line like `BNE :Label`, the assembler may fail with `Label inacessível a partir de um branch`. A UI could preview the computed displacement in real time and color the mnemonic red when the target is out of the −128..+127 window.
11. **Self-modifying code.** Both sample programs rely on self-modifying bytes. A "last write" overlay on the program area is invaluable when the UI is stepped slowly — the user watches a new `ROR` operand byte get written and then re-executed.
12. **Memory access granularity indicator.** Word vs byte access is invisible in the code but important above 0xFFD9. Marking the transition line in the memory view (e.g., a different background color for 0xFFDA..0xFFFF) prevents confusion when a `MOV` writes a single byte instead of the expected word.
13. **Step-through fidelity.** Fetch has side effects (register increments, PC advance, operand pre-evaluation). If the UI offers a "sub-step" view, the natural stages are: (a) read opcode byte(s), (b) decode source operand, (c) decode target operand, (d) execute, (e) update flags. Each maps directly to lines in `Fetch`/`Execute`.
14. **Breakpoints.** `DebuggerSetBreakpoint address` works on the fetched `R7` value, so UI "click a disassembly line to break" maps cleanly. Keep the same API as Ramses for visual and code consistency.
15. **Speed.** Because Cesar can run larger programs, a logarithmic speed slider (1, 10, 100, 1000, 10000 ops/s, unlimited) works better than linear. At "unlimited" the CLI's 1000-step ceiling becomes a bottleneck — consider tuning `DebuggerRun` or exposing the step limit.
