# ArchSims Web UI & TypeScript Port — Implementation Plan

## How to use this document

This plan is **living**. Whoever works on a task updates it in the same commit:

1. **Flip the `Status` column** as work advances (`❌ OPEN` → `🔧 IN PROGRESS` → `✅ RESOLVED`; use `⏯️ DEFERRED` for blocked/paused work — add a one-line reason in Notes).
2. **Append concrete file paths / decisions to Notes** when they become known (e.g. "ported at [web/src/core/neander.ts](../web/src/core/neander.ts)"). Do not delete historical notes; append.
3. **Add new tasks at the bottom of the owning phase**, not inline — keeps diffs reviewable. Give each new task an ID that continues the phase's numbering (e.g. P3-13).
4. **Do not re-open tasks**. If scope grows, add a follow-up task and cross-reference it.
5. **Keep sections short.** If a task needs a long design note, link to a sibling doc in [doc/](.) instead of inlining it.
6. **Verification checklist** (bottom of file) is the gate for calling the whole effort done.

Status legend:
- ✅ **RESOLVED** — Implemented and tested
- 🔧 **IN PROGRESS** — Partially implemented or underway
- ❌ **OPEN** — Not yet addressed
- ⏯️ **DEFERRED** — Delayed / blocked

---

## Context

**Why.** ArchSims currently ships as an F# library + a CLI. The Weber manuals in [doc/](.) describe in detail the front-panel UI each machine was meant to have, but no UI exists. We want:

1. A **TypeScript port** of the F# core so the simulators run in the browser without any server or .NET runtime.
2. **Stunning, educational front-panel UIs** for all four machines (Neander, Ahmes, Ramses, Cesar).
3. A **single static site** built with **Astro + Svelte + TypeScript**, deployed to **Cloudflare Pages**. No server code, no databases.

**Visual direction is already established.** The user authored proof-of-concept front panels for Ramses and Cesar (currently at [tmp/design-refs/](../tmp/design-refs/), React + Babel standalone). We will port that design language to Svelte and extend it to Neander/Ahmes, which reuse Ramses's look with simplified chrome.

**Pedagogical intent is non-negotiable.** The UI must obey the "one-screen rule" for Neander, show the IR bit-decomposition for Ramses, animate addressing-mode arrows for Ramses/Cesar, and surface memory-mapped I/O (display + keyboard) for Cesar. These are called out in each manual and must not be dropped for aesthetic reasons.

**Authoritative references (read these before implementing):**
- ISA manuals: [doc/Neander.md](Neander.md), [doc/Ahmes.md](Ahmes.md), [doc/Ramses.md](Ramses.md), [doc/Cesar.md](Cesar.md). Each has a "UI Design Suggestions" section at the end.
- F# core (source of truth for port semantics): [fs/ArchSims.Core/](../fs/ArchSims.Core/)
- F# test suite (port these to Vitest): [fs/ArchSims.Core.Tests/](../fs/ArchSims.Core.Tests/), [fs/ArchSims.Assemblers.Tests/](../fs/ArchSims.Assemblers.Tests/)
- Design POCs: [tmp/design-refs/ramses/](../tmp/design-refs/ramses/), [tmp/design-refs/cesar/](../tmp/design-refs/cesar/)

---

## Target architecture

```
web/                            ← ALL web code lives here (static site + TS core)
├── astro.config.mjs            ← Astro + Svelte integration, Cloudflare output
├── package.json                ← workspace root; pnpm
├── tsconfig.json               ← strict mode; path alias @/core, @/ui
├── vitest.config.ts            ← parity tests for the core
├── public/                     ← favicon, open-graph, sample .mem files
├── src/
│   ├── core/                   ← TypeScript port of ArchSims.Core (framework-free)
│   │   ├── memory.ts
│   │   ├── neander.ts
│   │   ├── ahmes.ts
│   │   ├── ramses.ts
│   │   ├── cesar.ts
│   │   ├── debugger.ts
│   │   └── index.ts
│   ├── assemblers/             ← TypeScript port of ArchSims.Assemblers (Chevrotain or hand-rolled)
│   │   ├── ramses.ts
│   │   └── cesar.ts
│   ├── ui/                     ← Svelte components grouped by role
│   │   ├── primitives/         ← Lamp, Segmented, Toggle, PanelButton, Etch, Scanlines
│   │   ├── panels/             ← RegisterTile, FlagBank, IRDecoder, MemoryGrid, Disassembly, SourceView, Controls, ServiceDrawer
│   │   └── chassis/            ← Layout shells per-machine
│   ├── stores/                 ← Svelte stores: cpu, breakpoints, tweaks (palette/base/density/frame)
│   ├── samples/                ← Embedded sample programs (.ram / .ces text + pre-assembled .mem)
│   └── pages/                  ← Astro routes
│       ├── index.astro         ← landing: 4 machine cards + manuals
│       ├── neander.astro
│       ├── ahmes.astro
│       ├── ramses.astro
│       └── cesar.astro
└── tests/                      ← Vitest: parity tests ported from F# NUnit
    ├── neander.test.ts
    ├── ahmes.test.ts
    ├── ramses.test.ts
    ├── cesar.test.ts
    ├── ramses-assembler.test.ts
    └── cesar-assembler.test.ts
```

**Key design choices:**
- **Core is framework-free.** The `src/core/*.ts` files have zero imports from Svelte, Astro, or the DOM. This keeps semantic parity with F#, makes them trivially testable under Node, and would let us reuse them in a future CLI.
- **Svelte stores wrap the core.** A single `cpuStore` per machine page; mutations call core functions; stores emit a new snapshot after each `step()`.
- **Astro islands.** Each machine page is one big `<FrontPanel client:load />` Svelte island; the page shell itself (header, nav, manual link) is static HTML for instant paint.
- **No runtime frameworks on CDN.** The design POCs use React + Babel standalone via `<script>` tags. The production site is fully bundled — no standalone Babel, no CDN React.

---

## Phase 1 — TypeScript Core Port

**Goal:** one-to-one semantic port of the F# simulators, validated by a ported test suite.

| ID    | Status  | Task | Notes |
|-------|---------|------|-------|
| P1-01 | ✅ RESOLVED | Scaffold `web/` with pnpm, Astro (Svelte integration), TypeScript strict, Vitest, ESLint+Prettier, `@astrojs/cloudflare` adapter (static output) | Scaffolded at [web/](../web/): [package.json](../web/package.json), [astro.config.mjs](../web/astro.config.mjs), [tsconfig.json](../web/tsconfig.json), [vitest.config.ts](../web/vitest.config.ts), [eslint.config.js](../web/eslint.config.js), [.prettierrc.json](../web/.prettierrc.json). `output: 'static'` (no adapter wired yet — P6-01 owns Cloudflare deploy). `pnpm install` + `pnpm test` (empty suite passes via `passWithNoTests`). `.gitignore` updated for `web/node_modules`, `web/dist`, `web/.astro`, `web/.wrangler` |
| P1-02 | ✅ RESOLVED | Port `Memory.fs` → [web/src/core/memory.ts](../web/src/core/memory.ts) | Procedural API mirroring F# (`createMemory`, `memoryReadByte`, `memoryWriteByte`, `memoryReadWordBigEndian`, `memoryWriteWordBigEndian`, `memoryReset`, `memoryLoad`). `Uint8Array` backing store auto-truncates writes to a byte. Counters (`readCount`/`writeCount`) match F# semantics — `memoryLoad` bypasses them like `Array.blit`. Smoke test at [web/tests/memory.test.ts](../web/tests/memory.test.ts) (deeper coverage comes through CPU tests in P1-10/11) |
| P1-03 | ✅ RESOLVED | Port `Neander.fs` → [web/src/core/neander.ts](../web/src/core/neander.ts) | 11 opcodes via const map; PC wraps via `& 0xFF`. Halted clears whenever a non-HLT instruction executes (matches F# `Flags.Halted <- instruction = Hlt`). Smoke test at [web/tests/neander-smoke.test.ts](../web/tests/neander-smoke.test.ts); full F# test parity arrives in P1-10 |
| P1-04 | ✅ RESOLVED | Port `Ahmes.fs` → [web/src/core/ahmes.ts](../web/src/core/ahmes.ts) | All 23 opcodes ported. Carry and Borrow stay independent (ADD writes C, SUB writes B); logic ops (OR/AND/NOT) leave C/V/B alone — guarded by smoke test. Shift uses `>>>` for unsigned semantics; rotate-through-carry implemented per F# bit-shuffle. Smoke test at [web/tests/ahmes-smoke.test.ts](../web/tests/ahmes-smoke.test.ts); full parity in P1-10 |
| P1-05 | ✅ RESOLVED | Port `Ramses.fs` → [web/src/core/ramses.ts](../web/src/core/ramses.ts) | RA/RB/RX/PC encoded in IR bits 2–3; mode in low 2 bits. Immediate-mode handling preserves the F# split (jumps treat byte as destination, ALU ops treat the PC slot as the operand address). SUB sets Carry as a borrow flag (post-invert). JSR self-links: writes return PC into target byte, then `PC ← target + 1`. Disassembler ported. Smoke test at [web/tests/ramses-smoke.test.ts](../web/tests/ramses-smoke.test.ts); full parity in P1-11 |
| P1-06 | ✅ RESOLVED | Port `Cesar.fs` → [web/src/core/cesar.ts](../web/src/core/cesar.ts) | 16-bit ISA on `Uint16Array(8)`. All 8 addressing modes, 15 branches, JMP/SOB/JSR/RTS, full one-op (CLR..SBC) and two-op (MOV/ADD/SUB/CMP/AND/OR) groups, CCC/SCC, HLT/NOP. Memory-mapped I/O above `0xFFDA` is byte-only on read & write. Encoder helpers (`encodeCesarInstructionOneOperand`/`TwoOperand`) ported for tests. Disassembler ported with same operand-syntax (`R0`/`(R0)+`/`-(R0)`/`N(R0)` and indirect variants). Smoke test at [web/tests/cesar-smoke.test.ts](../web/tests/cesar-smoke.test.ts); full parity in P1-11 |
| P1-07 | ✅ RESOLVED | Port `Debugger.fs` → [web/src/core/debugger.ts](../web/src/core/debugger.ts) | CPU-agnostic: holds two callbacks (`cpuGetProgramCounter`, `cpuStep` returns `halted`) plus `instructionCount`, `lastStop`, `breakpoints`. Stop reasons as string-literal union (`'none' \| 'halted' \| 'breakpoint' \| 'runningForever'`). `debuggerRun(max)` resets `lastStop` then loops until non-`'none'`; ceiling check is `instructionCount >= startCount + max` so successive runs accumulate count correctly. Tests at [web/tests/debugger.test.ts](../web/tests/debugger.test.ts) wire a real Neander CPU (no mocks). |
| P1-08 | ✅ RESOLVED | Port `RamsesAssembler.fs` → [web/src/assemblers/ramses.ts](../web/src/assemblers/ramses.ts) | Hand-rolled recursive-descent (no Chevrotain — grammar fits in ~300 lines). `assembleInstruction(input)` returns `number[]` for single-instruction tests; `assembleProgram(input)` returns `{ bytes: Uint8Array(256), addrToLine, labels, instrAddrs }` to feed Phase 3's source view (P3-06) and the sample bundler (P5-01). Single-pass with forward-label deferral / fix-up; throws `Label indefinido:` / `Label duplicado:` to match F# error messages. Smoke test at [web/tests/ramses-assembler-smoke.test.ts](../web/tests/ramses-assembler-smoke.test.ts); full F# parity arrives in P1-12. |
| P1-09 | ✅ RESOLVED | Port `CesarAssembler.fs` → [web/src/assemblers/cesar.ts](../web/src/assemblers/cesar.ts) | Hand-rolled recursive descent with explicit save/restore for the 11 operand-pattern attempt order (`:LABEL`, `((R)+)`, `(-(R))`, `(N(R))`, `(R)+`, `-(R)`, `N(R)`, `(R)`, `R`, `#N`, `N`). Reuses `encodeCesarInstructionOneOperand`/`TwoOperand` from the core. JSR ORs the source register into the instruction's high byte; SOB delta is sign-flipped. Branch deferral validates the `[-128,127]` window both at immediate-resolve and label-define time, throwing `Label inacessível a partir de um branch:`. `assembleProgram` returns `{ bytes: Uint8Array(65536), addrToLine, labels, instrAddrs }` for Phase 3 / Phase 5. Smoke test at [web/tests/cesar-assembler-smoke.test.ts](../web/tests/cesar-assembler-smoke.test.ts); full F# parity in P1-12. |
| P1-10 | ✅ RESOLVED | Port `NeanderTests.fs` + `AhmesTests.fs` → Vitest | Shared helper at [web/tests/helpers.ts](../web/tests/helpers.ts) — `expectNeanderState`/`expectAhmesState` take a partial-record where `undefined` keys are skipped, mimicking the F# `\|>==` per-field DU style. Full F# parity (15 Neander tests, 40 Ahmes tests) at [web/tests/neander.test.ts](../web/tests/neander.test.ts) and [web/tests/ahmes.test.ts](../web/tests/ahmes.test.ts). The earlier smoke files stay alongside as quick-load sanity checks. |
| P1-11 | ✅ RESOLVED | Port `RamsesTests.fs` + `CesarTests.fs` → Vitest | Helpers extended in [web/tests/helpers.ts](../web/tests/helpers.ts) with `expectRamsesState` (Ra/Rb/Rx/Carry) and `expectCesarState` (R0..R6/Overflow/`instructionRegisterAt`/`instructionRegisterIs`). Ramses port at [web/tests/ramses.test.ts](../web/tests/ramses.test.ts) (23 tests; full parity with `RamsesTests.fs` — clean state, PC wrap, Reset, Z/N flag iteration over all 3 registers, all 4 address modes, LDR/STR loops over Ra/Rb/Rx, ADD/OR/AND/NOT/SUB, JMP/JN/JZ/JC, JSR self-link, NEG 4 cases, SHR multi-step, HLT cycle, DisassembleInstruction, DisassembleInstructions). Cesar port at [web/tests/cesar.test.ts](../web/tests/cesar.test.ts) (22 tests; full parity with `CesarTests.fs` — `testAddressMode` covers all 8 modes, `testBranchOperation` covers all 15 branches × 16 flag combos, `testClrGroupOperation` covers 12 values × 12 ops including Adc/Sbc with Carry both states, R7-special-case modes, JSR/RTS round-trip via R6, high-memory byte-level I/O at 0xFFDA/0xFFDC, full disassembler matrix). 147 tests total green; `tsc --noEmit` clean. Note: F# `CMP compares two operands` test mistakenly calls Sub helper — preserved for parity |
| P1-12 | ✅ RESOLVED | Port `RamsesAssemblerTests.fs` + `CesarAssemblerTests.fs` → Vitest | Parity tests at [web/tests/ramses-assembler.test.ts](../web/tests/ramses-assembler.test.ts) (3 tests: full AssembleInstruction encoding matrix, AssembleProgram with backward labels + `@` directives + execution, undeclared-label error) and [web/tests/cesar-assembler.test.ts](../web/tests/cesar-assembler.test.ts) (4 tests: full AssembleInstruction matrix incl. NOT in all 8 modes + MOV two-operand variants + signed-int edge cases `#-1`/`#-32768`/`#65535`, AssembleProgram with forward+backward labels + execution to HLT, undeclared-label error, far-branch error). Smoke tests at [web/tests/ramses-assembler-smoke.test.ts](../web/tests/ramses-assembler-smoke.test.ts) and [web/tests/cesar-assembler-smoke.test.ts](../web/tests/cesar-assembler-smoke.test.ts) retained for `addrToLine` / `instrAddrs` / duplicate-label coverage that the F# suite doesn't exercise. 154 tests total green |
| P1-13 | ✅ RESOLVED | Port `.mem` file I/O (browser): read/write `0x03 'R' 'M' 'S'` and `0x03 'C' '1' '6'` headers; stub header `0x03 'N' 'D' 'R'` for Neander/Ahmes | Pure functions on `Uint8Array` at [web/src/core/memfile.ts](../web/src/core/memfile.ts). API: `writeMemFile(kind, data)`, `readMemFile(kind, file)`, `memFileSignature(kind)`, `memFileSize(kind)` for `kind ∈ 'neander'\|'ahmes'\|'ramses'\|'cesar'`. Headers verbatim from F# `MemorySaveToFile` ([fs/ArchSims.CmdLine/Common.fs:85](../fs/ArchSims.CmdLine/Common.fs#L85), [RamsesCmdLine.fs:62](../fs/ArchSims.CmdLine/RamsesCmdLine.fs#L62), [CesarCmdLine.fs:74](../fs/ArchSims.CmdLine/CesarCmdLine.fs#L74)). Validates length, prefix byte, and signature with Portuguese error messages matching the rest of the assembler diagnostics. 10-test suite at [web/tests/memfile.test.ts](../web/tests/memfile.test.ts) covers round-trip for 256-byte and 64 KiB payloads, all four kinds, and every error path. 164 tests total green |

---

## Phase 2 — Web foundation & design system

**Goal:** reusable primitives that every front panel will consume. Port the visual language from [tmp/design-refs/](../tmp/design-refs/) to Svelte.

| ID    | Status  | Task | Notes |
|-------|---------|------|-------|
| P2-01 | ✅ RESOLVED | Astro route skeletons (`/`, `/neander`, `/ahmes`, `/ramses`, `/cesar`) with shared `<Layout>` + IBM Plex Mono preload | Shared shell at [web/src/layouts/Layout.astro](../web/src/layouts/Layout.astro) — `<title>`, description, IBM Plex Mono preload+stylesheet, global reset, dark chassis palette as a placeholder (gets replaced by P2-02's tokens). Landing page [web/src/pages/index.astro](../web/src/pages/index.astro) renders the 4 machine cards in progression order (`NEANDER → AHMES → RAMSES → CESAR`) with a per-card "Open panel" link and a "Read the manual" link. Per-machine skeletons at [web/src/pages/neander.astro](../web/src/pages/neander.astro), [ahmes.astro](../web/src/pages/ahmes.astro), [ramses.astro](../web/src/pages/ramses.astro), [cesar.astro](../web/src/pages/cesar.astro) — each is a placeholder shell cross-linking the Phase 4 task that owns its real panel. Manuals rendered via an `astro:content` glob loader at [web/src/content.config.ts](../web/src/content.config.ts) (`base: '../doc'`, filtered to `{Neander,Ahmes,Ramses,Cesar}.md` so the plan isn't exposed) + dynamic route at [web/src/pages/manuals/[...slug].astro](../web/src/pages/manuals/%5B...slug%5D.astro). Astro v5 slugifies glob ids — manuals live at `/manuals/{neander,ahmes,ramses,cesar}`. `tsconfig.json` no longer excludes `.astro/` so `tsc --noEmit` sees the generated `astro:content` types. `pnpm build` emits 9 static pages (5 shell + 4 manuals); `pnpm test` still 164/164 green |
| P2-02 | ✅ RESOLVED | Port CSS tokens from [tmp/design-refs/ramses/assets/styles.css](../tmp/design-refs/ramses/assets/styles.css) to `web/src/styles/tokens.css` | Tokens at [web/src/styles/tokens.css](../web/src/styles/tokens.css), imported globally from [web/src/layouts/Layout.astro](../web/src/layouts/Layout.astro). Covers: (a) `:root` custom properties (chassis surfaces, phosphor, accent, silk, readout, density-scale, grid-gap); (b) `amber` (default), `chassis-green` (CRT), `chassis-paper` (light) palette overrides verbatim from POC; (c) `density-compact` scale; (d) base html/body with IBM Plex Mono; (e) `.chassis` radial+linear gradient shell; (f) `.scanlines` overlay (`mix-blend-mode: overlay`, hidden for `paper` palette, and for `prefers-reduced-motion: reduce`); (g) `.etch` silkscreen label + `.panel-card` card chrome — the two generic selectors every primitive / panel will consume. Component-specific styles (lamp, IR bits, reg tiles, memory grid, controls) intentionally deferred into the owning component's scoped `<style>` block per P2-03+. Visual check: `pnpm build` emits CSS in every page bundle; no test regression (164/164 still green) |
| P2-03 | ✅ RESOLVED | Svelte primitives mirroring [tmp/design-refs/ramses/assets/primitives.jsx](../tmp/design-refs/ramses/assets/primitives.jsx): `Lamp.svelte`, `Segmented.svelte`, `Toggle.svelte`, `PanelButton.svelte`, `Etch.svelte`, `Scanlines.svelte` | Six Svelte 5 components at [web/src/ui/primitives/](../web/src/ui/primitives/), exported via [index.ts](../web/src/ui/primitives/index.ts). All use Svelte 5 runes (`$props()`, `$state`, `$derived`). API shape preserves the POC: `<Lamp on color label sub />` with color ∈ `amber\|green\|red\|ink`; `<Segmented text size color />` with size ∈ `xl\|md\|sm`; `<Toggle on onChange label disabled compact />`; `<PanelButton label sub onClick disabled variant held />` with variant ∈ `default\|green\|red\|amber`; `<Etch inline>{children}</Etch>` via snippet; `<Scanlines palette />` (component returns no content under `paper` via CSS). Component-specific CSS lives in each `<style>` block (scoped by Svelte) with `:global(.chassis-paper) ...` overrides for the light palette. `Toggle` adds `aria-pressed`; `Scanlines` sets `aria-hidden`. [PrimitivesDemo.svelte](../web/src/ui/primitives/PrimitivesDemo.svelte) imports all five visible primitives and is wired as a `client:load` island on `/neander` so `pnpm build` compiles every component (bundle outputs `PrimitivesDemo.*.js` at 9.96 kB). 164/164 tests still green. Real panel work lands in Phase 4 — primitives demo gets replaced by P4-01 |
| P2-04 | ✅ RESOLVED | Shared chassis shell: header (title, PWR/RUN/HLT lamps, serial number), footer (chain link `NEANDER → AHMES → RAMSES → CESAR`), `ServiceDrawer.svelte` | Two Svelte 5 components at [web/src/ui/chassis/](../web/src/ui/chassis/). **Chassis.svelte** is the wrapping shell: `{ machine, title, sub, running, halted, serial, tweaks, serviceOpen, children }` props (Svelte 5 `$props()` + `$bindable` for `serviceOpen`). Applies `chassis chassis-{palette} density-{density} frame-{frame} ann-{on\|off}` classes via `class:name={cond}` directives so Svelte scopes the palette-dependent selectors. Renders `<Scanlines />`, a header with two rotated diamonds (logo-mark), title + sub, a PWR/RUN/HLT lamp row wired to `running`/`halted` (run lamp uses a `$derived` accent color: `green` under green palette, `ink` under paper, else `amber`), and a serial plate. `<main>{@render children()}</main>` hosts the machine-specific panel. Footer chains `NEANDER → AHMES → RAMSES → CESAR` with current machine bolded and others as `<a href={/{m}}>`. **ServiceDrawer.svelte** is the slide-in drawer: `{ open (bindable), tweaks, diagnostics, memKind, memData, onMemLoad, onReset?, onLoadSample?, sampleLabel?, extraSections? }` props. Display section: palette/base/density/frame/annotations segmented toggles, each calling `tweaks.patch({ ... })`. Memory image section: SAVE .MEM downloads via `writeMemFile(memKind, bytes)` + `Blob` + `URL.createObjectURL`; LOAD .MEM runs `readMemFile` on a `FileReader.result` and surfaces decode errors inline; optional reload-sample action. Diagnostics grid (steps/reads/writes/halted) + optional FULL RESET. Backdrop handles click + Escape to close, drawer has `aria-hidden` binding. Scoped `<style>` with `:global(.chassis-paper) ...` overrides for every rule that changes between palettes. Wired to `/neander` via [ChassisDemo.svelte](../web/src/ui/chassis/ChassisDemo.svelte) (`client:load` island) so `pnpm build` actually compiles both components — bundle emits `ChassisDemo.*.js` at 17.54 kB. 181/181 tests green; `tsc --noEmit` clean. Full per-panel wiring lands in Phase 4 |
| P2-05 | ✅ RESOLVED | `tweaksStore` (palette/base/density/showAnnotations/frame) with `localStorage` persistence per-machine key (`ramses.tweaks`, etc.) | Factory at [web/src/stores/tweaks.ts](../web/src/stores/tweaks.ts). `createTweaksStore(machine)` returns a `TweaksStore` that extends `Writable<Tweaks>` with `.patch(Partial<Tweaks>)` and `.reset()` helpers. Defaults match the POC HTML snapshot (`palette:'green', base:'hex', density:'compact', showAnnotations:true, frame:'desktop'`). Storage key format `${machine}.tweaks` matches the POC verbatim so users' settings survive the port. `readFromStorage` merges persisted partials over defaults (forward-compatible when a new key is added) and falls back to defaults on malformed JSON. SSR-safe: creation works without a `localStorage` global — the initial subscribe-emit no-ops instead of throwing. 12-test suite at [web/tests/tweaks.test.ts](../web/tests/tweaks.test.ts) uses an in-memory `StorageLike` shim (real behavior, no mocks of the store itself) to cover defaults, persistence, partial merging, malformed JSON, `.patch`, `.reset`, `.set`, subscribe fan-out, per-machine isolation, and the SSR path. 176/176 tests green; `tsc --noEmit` clean |
| P2-06 | ✅ RESOLVED | `cpuStore` factory: wraps a core CPU instance, exposes reactive `$snapshot` derived on each `step()`/`reset()` | Generic factory at [web/src/stores/cpu.ts](../web/src/stores/cpu.ts). `createCpuStore<C>({ cpu, step, reset })` returns a `CpuStore<C>` whose `subscribe` emits `{ cpu, tick }`. Design: CPU is mutated in place (matches the F# port's mutable-record model) and the store bumps an integer `tick` counter after every `.step()` so Svelte's structural equality sees a fresh snapshot and re-notifies subscribers. `.reset()` calls the binding's reset and drops `tick` back to 0. Exposes `cpu` and `tick` as live getters for non-reactive call sites. Shape is CPU-agnostic — `{ step, reset }` bindings let every machine page wire up via its own core functions (e.g. `neanderStep`/`neanderReset`, `cesarStep`/`cesarReset`). 5-test suite at [web/tests/cpu-store.test.ts](../web/tests/cpu-store.test.ts) wires a real `NeanderCpu` (no mocks): initial tick/ref, step advances flags+tick, subscribers receive every tick, reset zeroes tick and CPU, cpu reference is preserved across step/reset. 181/181 tests green; `tsc --noEmit` clean |

---

## Phase 3 — Shared panel components

**Goal:** panel-level Svelte components that can be composed into any of the 4 front panels. These are what make the design "componentized" per the user's ask.

| ID    | Status  | Task | Notes |
|-------|---------|------|-------|
| P3-01 | ✅ RESOLVED | `RegisterTile.svelte` (name, value, hint, active-highlight, hex+dec+bin subrow) | Component at [web/src/ui/panels/RegisterTile.svelte](../web/src/ui/panels/RegisterTile.svelte). Props `{ name, value, width, active, accent, hint }` with `width ∈ 8 \| 16` — the 16-bit path is how Cesar's R0..R7 and PC will render. Derives hex/dec/bin forms from `value & mask` so callers can pass raw ints without pre-masking. Active-tile highlight via `class:reg-active`. Scoped `<style>` mirrors POC `.reg-tile` selectors verbatim |
| P3-02 | ✅ RESOLVED | `FlagBank.svelte` — row of lamps, configurable flag set | Component at [web/src/ui/panels/FlagBank.svelte](../web/src/ui/panels/FlagBank.svelte). Props `{ flags: FlagSpec[], accent, title }` where `FlagSpec = { label, sub?, on, color? }` — the caller builds the list so each machine supplies its own flag set (Neander `{N,Z,HLT}`, Ahmes `{N,Z,V,C,B,HLT}`, Ramses `{N,Z,C,HLT}`, Cesar `{N,Z,V,C,HLT}`). Per-flag override of lamp color (red for HLT) takes precedence over the bank accent. Shared `FlagSpec` type lives in [types.ts](../web/src/ui/panels/types.ts) |
| P3-03 | ❌ OPEN | `IRDecoder.svelte` — opcode byte with bit-group decomposition (Neander: `IIII/----`; Ramses: `IIII/RR/MM`; Ahmes: similar to Neander with sub-opcode annotation; Cesar: multi-byte, show source/target mode fields) | The POC only has Ramses's decoder. For each machine, the bit groups and mnemonic lookup differ — implement per-machine decoders behind a common interface. Reference: [tmp/design-refs/ramses/assets/cpu-panels.jsx:42](../tmp/design-refs/ramses/assets/cpu-panels.jsx#L42) |
| P3-04 | ❌ OPEN | `MemoryGrid.svelte` — 16×16 for 8-bit CPUs (Neander/Ahmes/Ramses); paged/virtualized view for Cesar's 64 KiB | Hot cells: PC highlight, last-read/last-write flashes, breakpoint gutter. Support base switching (hex/dec/bin). Reference: [tmp/design-refs/ramses/assets/memory-disasm.jsx](../tmp/design-refs/ramses/assets/memory-disasm.jsx) |
| P3-05 | ❌ OPEN | `Disassembly.svelte` — live disassembly around PC, click-to-breakpoint | Per-machine disassembler function lives in `web/src/core/*.ts` alongside the CPU (mirrors F# layout) |
| P3-06 | ❌ OPEN | `SourceView.svelte` — show `.ram`/`.ces` source with PC-aligned highlight when `addrToLine` map is present | Assembler output must produce `{ bytes, addrToLine, labels, instrAddrs }` to feed this |
| P3-07 | ❌ OPEN | `Controls.svelte` — Step / Run / Break / Reset + speed slider (0 = max via rAF batching, else `setInterval` with `1000/speed` ms) | Mirror POC logic. Reference: [tmp/design-refs/ramses/assets/app.jsx:76](../tmp/design-refs/ramses/assets/app.jsx#L76) run-loop |
| P3-08 | ❌ OPEN | `AddressingModeArrow.svelte` — on-hover SVG overlay drawing an arrow from operand byte in memory grid → effective address cell | Called out in [doc/Ramses.md:246](Ramses.md) and [doc/Cesar.md:351](Cesar.md). Not in the POC; this is new. Animate `(R3)+` decomposition step-by-step on Cesar hover |
| P3-09 | ❌ OPEN | `ShiftRotateAnimation.svelte` — 8-cell bit strip showing AC bits flowing into/out of carry for `SHR/SHL/ROR/ROL` | Ahmes-specific, requested by [doc/Ahmes.md:202](Ahmes.md) |
| P3-10 | ❌ OPEN | `DisplayPanel.svelte` — 36-byte ASCII strip rendered as a fixed-width "screen" for Cesar (0xFFDC..0xFFFF) | Updates live on memory writes in that range. [doc/Cesar.md:349](Cesar.md) |
| P3-11 | ❌ OPEN | `KeyboardInput.svelte` — input box that writes a byte to 0xFFDA on submit | [doc/Cesar.md:348](Cesar.md) |
| P3-12 | ❌ OPEN | `StackPanel.svelte` — upward-growing column of words starting at R6; appears only when R6 has been written or JSR executed | [doc/Cesar.md:353](Cesar.md) |

---

## Phase 4 — Per-computer front panels

**Goal:** assemble the primitives + panels into a polished front panel per machine. Neander is the first concrete target because it exercises the smallest surface.

| ID    | Status  | Task | Notes |
|-------|---------|------|-------|
| P4-01 | ❌ OPEN | `NeanderPanel.svelte` + `/neander.astro` route | Two register tiles (AC, PC), three flags (N/Z/HLT), 16×16 memory grid, IR decoder (with "low nibble = unused, always 0" hint per [doc/Neander.md:181](Neander.md)), tooltips on opcode gaps calling out "0x70 is used by Ahmes" ([doc/Neander.md:185](Neander.md)) |
| P4-02 | ❌ OPEN | `AhmesPanel.svelte` + `/ahmes.astro` route | Same skeleton as Neander + Carry/Borrow/Overflow lamps, shift/rotate bit-strip animation, 16-bit scratchpad overlay in memory grid ([doc/Ahmes.md:201](Ahmes.md)) |
| P4-03 | ❌ OPEN | `RamsesPanel.svelte` + `/ramses.astro` route — **this is the reference port of the existing POC** | Three register tiles (RA/RB/RX) + PC, 4-flag bank, IR decoder with IIII/RR/MM color groups, addressing-mode arrow animation, JSR self-link visualization (arrow writes return byte *into* called routine, not a stack push) per [doc/Ramses.md:248](Ramses.md). **Deliverable target: visually match [tmp/design-refs/ramses/screenshots/overview.png](../tmp/design-refs/ramses/screenshots/overview.png)** |
| P4-04 | ❌ OPEN | `CesarPanel.svelte` + `/cesar.astro` route | 8 register tiles (16-bit, label R6=SP / R7=PC), 5-flag bank, multi-byte IR decoder, memory pager with hex address column (00xx → FFxx), `DisplayPanel`, `KeyboardInput`, `StackPanel`, `SOB` loop counter visualization ([doc/Cesar.md:354](Cesar.md)), branch-range preview in source view ([doc/Cesar.md:355](Cesar.md)) |
| P4-05 | ❌ OPEN | Landing page (`/index.astro`) — 4 machine cards in the progression order, each linking to its panel + its manual in `doc/` | Use the same chassis aesthetic; cards are miniature "plaques" |
| P4-06 | ❌ OPEN | Fetch-cycle sub-step animation (opt-in via Service Drawer) — split each step into (a) read opcode (b) decode source (c) decode target (d) execute (e) update flags | [doc/Cesar.md:358](Cesar.md); worth implementing once on Cesar, then back-porting a simplified version to Neander |

---

## Phase 5 — Sample programs & content

| ID    | Status  | Task | Notes |
|-------|---------|------|-------|
| P5-01 | ❌ OPEN | Ship existing samples: [Samples/BitShift.Ramses.txt](../Samples/BitShift.Ramses.txt), [Samples/BitShift.Cesar.txt](../Samples/BitShift.Cesar.txt) — copy into `web/src/samples/` | Each sample bundles `{ name, sourceText, bytes, addrToLine, labels }`. Assemble at build time |
| P5-02 | ❌ OPEN | Author `Hello.Cesar` sample (writes "HELLO, CESAR!" to the display area) | POC embeds an equivalent demo at [tmp/design-refs/cesar/assets/app.jsx:7](../tmp/design-refs/cesar/assets/app.jsx#L7). Port the source to real `.ces` syntax using the ported assembler |
| P5-03 | ❌ OPEN | Author a 10-line Neander demo and a 10-line Ahmes demo for first-launch | Neander: classic "sum of memory" loop. Ahmes: showcase the new `SUB`/shift/rotate with C/B distinction |
| P5-04 | ❌ OPEN | Service Drawer: "Load sample" dropdown shows all samples for the current machine | Match POC's "Reload sample" affordance |

---

## Phase 6 — Deploy & polish

| ID    | Status  | Task | Notes |
|-------|---------|------|-------|
| P6-01 | ❌ OPEN | Configure `@astrojs/cloudflare` for static output, verify `pnpm build` produces `web/dist/` that `wrangler pages dev` serves identically | No Pages Functions; purely static |
| P6-02 | ❌ OPEN | Cloudflare Pages project config committed as `web/wrangler.toml` | Build command `pnpm build`, output `dist/` |
| P6-03 | ❌ OPEN | CI: GitHub Actions `.github/workflows/web.yml` — runs `pnpm install`, `pnpm -C web test`, `pnpm -C web build` on PR | Cache pnpm store and Astro build |
| P6-04 | ❌ OPEN | Accessibility pass: keyboard step controls, focus ring on interactive panel buttons, `aria-label` on lamps, reduced-motion media query disables scanlines + shift animations | Must not break the aesthetic; skeuomorphic can still be accessible |
| P6-05 | ❌ OPEN | Performance budget: first machine page ≤ 150 KB JS (gzipped), ≤ 50 KB CSS. Measure with `astro build --verbose` | Svelte + Astro should come in under this easily |
| P6-06 | ❌ OPEN | Open-graph images per machine (static PNG exports of the chassis) | Nice-to-have; makes shared links look sharp |

---

## Design critique / suggested improvements over the POC

Before anyone starts Phase 4, review these proposed changes against the POC so we don't accidentally re-create limitations:

1. **Runtime.** The POC uses React + Babel standalone via CDN. That's great for a single-file demo, bad for a deployable site (Babel in the browser is ~1 MB, and Cloudflare CDN cost is non-zero). Svelte + bundled output cuts this to ~40 KB runtime.
2. **CPU model.** The POC ships its own simplified `RamsesEmu` / `CesarEmu` in [emulator.js](../tmp/design-refs/ramses/assets/emulator.js). **Do not port this.** Use the TypeScript port of the F# core — it has test parity we want to preserve.
3. **Addressing-mode animation.** The manuals call for it ([doc/Ramses.md:246](Ramses.md), [doc/Cesar.md:351](Cesar.md)) but the POC does not implement it. We should — it is the single highest-leverage teaching affordance.
4. **Ahmes shift/rotate animation.** Called out in [doc/Ahmes.md:202](Ahmes.md), absent in the POC (POC covers only Ramses/Cesar). We add it.
5. **Fetch-cycle sub-stepping.** Called out in [doc/Cesar.md:358](Cesar.md), absent in POC. Worth doing; also explains Neander's execution model to beginners.
6. **Self-modifying-code "last write" overlay.** [doc/Cesar.md:356](Cesar.md) — the core already tracks `lastWrite`; surfacing it in the memory grid costs one CSS class.
7. **Neander/Ahmes get a "family chain" comparison link.** Each page's footer already shows `NEANDER → AHMES → RAMSES → CESAR`; make the next arrow clickable so students can hop to the same program on the next machine up when applicable.
8. **Data/code marking.** [doc/Neander.md:184](Neander.md) suggests letting users mark byte ranges as "data (hex)" vs "code". This belongs in the Service Drawer; cheap to add and transforms Neander from "calculator simulator" to actual exploration tool.

---

## Verification checklist

Done means:

- [ ] `pnpm -C web test` passes with **zero** F# tests lacking a TypeScript equivalent. Compare count: `dotnet test` number of tests vs `vitest run --reporter=verbose` count.
- [ ] `pnpm -C web build` produces a static `dist/` with no server-side assets (no `.js` in `dist/functions/`, no worker output).
- [ ] Locally: `pnpm -C web preview` loads [http://localhost:4321/](http://localhost:4321/) without network calls beyond Google Fonts (+ favicon). All four panel pages reach interactive state with the sample program halting cleanly.
- [ ] Visually: the Ramses panel renders a diff-free match to [tmp/design-refs/ramses/screenshots/overview.png](../tmp/design-refs/ramses/screenshots/overview.png) under the `amber` palette (inspect manually side-by-side). Ahmes and Neander use the same Ramses chrome with the simplifications noted in P4-01/P4-02. Cesar renders with its full layout (registers column, memory pager, display strip, stack panel, source view).
- [ ] Run [Samples/BitShift.Ramses.txt](../Samples/BitShift.Ramses.txt) and [Samples/BitShift.Cesar.txt](../Samples/BitShift.Cesar.txt) in-browser: observable behavior (registers cycling, display writing "HELLO, CESAR!") matches the F# CLI output for the same programs.
- [ ] Breakpoints: clicking a disassembly line toggles the gutter indicator and halts `Run` when PC reaches that address — on every panel.
- [ ] `.mem` round-trip: save on Ramses panel, load on the F# CLI (`dotnet run --project fs/ArchSims.CmdLine -- ...`), and vice versa. Verifies header-format compatibility.
- [ ] Cloudflare Pages preview deploy (branch deploy on PR) renders identically to local `preview`.
- [ ] Lighthouse: Performance ≥ 90, Accessibility ≥ 95 on each machine page.
