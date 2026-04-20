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
| P1-03 | ❌ OPEN | Port `Neander.fs` → [web/src/core/neander.ts](../web/src/core/neander.ts) | 11 opcodes; flags `Halted/Negative/Zero` (Zero starts `true`); PC wraps at 256. Reference: [fs/ArchSims.Core/Neander.fs](../fs/ArchSims.Core/Neander.fs) |
| P1-04 | ❌ OPEN | Port `Ahmes.fs` → [web/src/core/ahmes.ts](../web/src/core/ahmes.ts) | Extends Neander; adds `SUB`, `SHR/SHL/ROR/ROL`, extended branches; flags `N/Z/C/V/B/Halted` (C and B are independent — `ADD` sets C, `SUB` sets B). Reference: [doc/Ahmes.md](Ahmes.md) §4 |
| P1-05 | ❌ OPEN | Port `Ramses.fs` → [web/src/core/ramses.ts](../web/src/core/ramses.ts) | 3 regs (RA/RB/RX), 4 addressing modes encoded in low 2 bits of opcode; `JSR` self-links (no stack). Reference: [fs/ArchSims.Core/Ramses.fs](../fs/ArchSims.Core/Ramses.fs), [doc/Ramses.md](Ramses.md) §5 |
| P1-06 | ❌ OPEN | Port `Cesar.fs` → [web/src/core/cesar.ts](../web/src/core/cesar.ts) | 16-bit, R0–R7 (R6=SP, R7=PC), 8 addressing modes, two-operand ALU, 15 branches, `SOB`, stack `JSR/RTS`, memory-mapped I/O at 0xFFDA/0xFFDC. 64 KiB memory. Reference: [doc/Cesar.md](Cesar.md) |
| P1-07 | ❌ OPEN | Port `Debugger.fs` → [web/src/core/debugger.ts](../web/src/core/debugger.ts) | `step`, `run(maxSteps)`, `setBreakpoint`, `clearBreakpoint`; stop reasons `Halted/Breakpoint/RunningForever/None`; 1000-step ceiling sentinel |
| P1-08 | ❌ OPEN | Port `RamsesAssembler.fs` → [web/src/assemblers/ramses.ts](../web/src/assemblers/ramses.ts) | Single-pass with forward-label resolution. Recommend hand-rolled recursive descent (simpler than Chevrotain for this grammar). Reference: [fs/ArchSims.Assemblers/RamsesAssembler.fs](../fs/ArchSims.Assemblers/RamsesAssembler.fs) |
| P1-09 | ❌ OPEN | Port `CesarAssembler.fs` → [web/src/assemblers/cesar.ts](../web/src/assemblers/cesar.ts) | All 8 addressing-mode syntaxes (`R0`, `(R0)+`, `-(R0)`, `N(R0)`, `(R0)`, `((R0)+)`, `(-(R0))`, `(N(R0))`, aliases `#N`, `N`); signed branch range check |
| P1-10 | ❌ OPEN | Port `NeanderTests.fs` + `AhmesTests.fs` → Vitest | One `.test.ts` per CPU. Keep the `\|>==` assertion style as a small `expectState()` helper to minimize per-test churn |
| P1-11 | ❌ OPEN | Port `RamsesTests.fs` + `CesarTests.fs` → Vitest | Same pattern; use table-driven tests for the addressing-mode matrix. Parity target: zero F# tests without a TS equivalent |
| P1-12 | ❌ OPEN | Port `RamsesAssemblerTests.fs` + `CesarAssemblerTests.fs` → Vitest | Exercise label resolution, invalid branch range, all operand syntaxes |
| P1-13 | ❌ OPEN | Port `.mem` file I/O (browser): read/write `0x03 'R' 'M' 'S'` and `0x03 'C' '1' '6'` headers; stub header `0x03 'N' 'D' 'R'` for Neander/Ahmes | Pure functions on `Uint8Array`; lives in `web/src/core/memfile.ts`. Reference: [fs/ArchSims.CmdLine/RamsesCmdLine.fs](../fs/ArchSims.CmdLine/RamsesCmdLine.fs) |

---

## Phase 2 — Web foundation & design system

**Goal:** reusable primitives that every front panel will consume. Port the visual language from [tmp/design-refs/](../tmp/design-refs/) to Svelte.

| ID    | Status  | Task | Notes |
|-------|---------|------|-------|
| P2-01 | ❌ OPEN | Astro route skeletons (`/`, `/neander`, `/ahmes`, `/ramses`, `/cesar`) with shared `<Layout>` + IBM Plex Mono preload | Landing page: 4 machine cards, link to each manual in `doc/` (rendered via `astro:content` or static copy) |
| P2-02 | ❌ OPEN | Port CSS tokens from [tmp/design-refs/ramses/assets/styles.css](../tmp/design-refs/ramses/assets/styles.css) to `web/src/styles/tokens.css` | 3 palettes: `amber` (warm dark), `green` (CRT), `paper` (light). Preserve phosphor glow + scanline overlay |
| P2-03 | ❌ OPEN | Svelte primitives mirroring [tmp/design-refs/ramses/assets/primitives.jsx](../tmp/design-refs/ramses/assets/primitives.jsx): `Lamp.svelte`, `Segmented.svelte`, `Toggle.svelte`, `PanelButton.svelte`, `Etch.svelte`, `Scanlines.svelte` | Keep API shape (`on`, `color`, `label`, `sub`) so porting panel code is mechanical |
| P2-04 | ❌ OPEN | Shared chassis shell: header (title, PWR/RUN/HLT lamps, serial number), footer (chain link `NEANDER → AHMES → RAMSES → CESAR`), `ServiceDrawer.svelte` | Drawer holds palette/base/density/frame toggles + `.mem` load/save + diagnostics (steps/reads/writes). Port from [tmp/design-refs/ramses/assets/app.jsx](../tmp/design-refs/ramses/assets/app.jsx) `ServiceDrawer` |
| P2-05 | ❌ OPEN | `tweaksStore` (palette/base/density/showAnnotations/frame) with `localStorage` persistence per-machine key (`ramses.tweaks`, etc.) | Match the design POC's persistence keys so users who've played with the POC keep their settings |
| P2-06 | ❌ OPEN | `cpuStore` factory: wraps a core CPU instance, exposes reactive `$snapshot` derived on each `step()`/`reset()` | One factory reused by all 4 machine pages; minimizes per-page boilerplate |

---

## Phase 3 — Shared panel components

**Goal:** panel-level Svelte components that can be composed into any of the 4 front panels. These are what make the design "componentized" per the user's ask.

| ID    | Status  | Task | Notes |
|-------|---------|------|-------|
| P3-01 | ❌ OPEN | `RegisterTile.svelte` (name, value, hint, active-highlight, hex+dec+bin subrow) | Reference: [tmp/design-refs/ramses/assets/cpu-panels.jsx:3](../tmp/design-refs/ramses/assets/cpu-panels.jsx#L3) `RegisterTile`. Parameterize width (8 vs 16-bit) via prop |
| P3-02 | ❌ OPEN | `FlagBank.svelte` — row of lamps, configurable flag set | Must accept variable flag lists: Neander `{N,Z,HLT}`, Ahmes `{N,Z,V,C,B,HLT}`, Ramses `{N,Z,C,HLT}`, Cesar `{N,Z,V,C,HLT}`. Order of N/Z/V/C/B stays consistent per [doc/Ahmes.md](Ahmes.md) §7.2 |
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
