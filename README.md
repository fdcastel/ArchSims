# ArchSims

Simulators for use in computer architecture classes.

## Overview

This project is a [_clean room_](https://en.wikipedia.org/wiki/Clean_room_design) implementation of the [hypothetical machines](https://pt.wikipedia.org/wiki/M%C3%A1quinas_hipot%C3%A9ticas_da_Universidade_Federal_do_Rio_Grande_do_Sul) used in computer architecture classes at the [Institute of Informatics](http://www.inf.ufrgs.br) of Universidade Federal do Rio Grande do Sul.

The goal is to faithfully recreate the [instruction set architecture](https://en.wikipedia.org/wiki/Instruction_set_architecture) of each machine. Every implementation is backed by an extensive test suite and follows the original specifications provided by professors Raul Fernando Weber and Taisy Silva Weber.

The four machines form a progression of increasing complexity:

| Machine | Width  | Highlights                                                                 | Manual                       |
| ------- | ------ | -------------------------------------------------------------------------- | ---------------------------- |
| Neander | 8-bit  | Baseline: single accumulator, one addressing mode, `N`/`Z` flags           | [doc/Neander.md](doc/Neander.md) |
| Ahmes   | 8-bit  | Adds `SUB`, shifts, rotates, `C`/`V`/`B` flags, extended conditional jumps | [doc/Ahmes.md](doc/Ahmes.md)     |
| Ramses  | 8-bit  | Multiple registers (`RA`/`RB`/`RX`), four addressing modes, `JSR`          | [doc/Ramses.md](doc/Ramses.md)   |
| Cesar   | 16-bit | Eight registers, eight addressing modes, stack, memory-mapped I/O          | [doc/Cesar.md](doc/Cesar.md)     |

Each manual documents the programmer's model, instruction set, execution cycle, and UI design suggestions.

## Content

This repository contains two complete implementations: the original F# core plus an in-browser TypeScript port with skeuomorphic front-panel UIs.

### F# core ([fs/](fs))

  - Core implementations for Neander, Ahmes, Ramses, and Cesar ([fs/ArchSims.Core](fs/ArchSims.Core))
  - Assemblers for Ramses and Cesar ([fs/ArchSims.Assemblers](fs/ArchSims.Assemblers))
    - Neander and Ahmes ship without an assembler (by design)
  - A Ramses emulator for Cesar (_it was a cold and rainy day..._)
  - Thorough test cases ([fs/ArchSims.Core.Tests](fs/ArchSims.Core.Tests))
    - An adapter to test the Ramses emulator with the same test cases used for Ramses
  - A command-line utility for simple demos ([fs/ArchSims.CmdLine](fs/ArchSims.CmdLine))

### Web UI & TypeScript port ([web/](web))

  - Framework-free TypeScript port of the F# core ([web/src/core/](web/src/core))
  - TypeScript ports of the Ramses and Cesar assemblers ([web/src/assemblers/](web/src/assemblers))
  - Skeuomorphic Svelte front panels for each machine ([web/src/ui/](web/src/ui))
  - Bundled sample programs loadable from each panel's Service drawer ([web/src/samples/](web/src/samples))
  - Vitest parity suite mirroring the F# NUnit tests ([web/tests/](web/tests))
  - Static-site output (Astro) — no server, no backend

Roadmap and task status for the web port live in [doc/WEB_UI_PLAN.md](doc/WEB_UI_PLAN.md).

## Running the F# simulators

### Prerequisites

  - [.NET 9](https://dotnet.microsoft.com/download)
  - [PowerShell 7+](https://github.com/PowerShell/PowerShell) (to run the sample scripts)

Both are cross-platform, so the project and its samples run on Windows, macOS, and Linux.

### Run the tests

```
dotnet test
```

### Run the samples

Run the `.ps1` scripts from the [Samples](Samples) folder:

```
pwsh Samples/Demo-Ramses.ps1
pwsh Samples/Demo-Cesar.ps1
pwsh Samples/Demo-Cesar-Debug.ps1
```

## Running the web UI locally

### Prerequisites

  - [Node.js 20+](https://nodejs.org/)
  - [pnpm 9+](https://pnpm.io/installation)

### Install, test, develop

From the repository root:

```
pnpm -C web install        # install dependencies (first run only)
pnpm -C web test            # run the Vitest parity suite
pnpm -C web dev             # start the dev server at http://localhost:4321
pnpm -C web build           # produce a static site in web/dist/
pnpm -C web preview         # serve the built site locally
```

The dev server auto-opens the landing page with machine cards; each card links to its front panel (`/neander`, `/ahmes`, `/ramses`, `/cesar`) and its manual (`/manuals/<name>`). Every panel exposes a Service drawer (right-edge button) with palette, base, density, sample-loader, and `.mem` image save/load controls.

## Special thanks

  - Prof. Raul Fernando Weber (_in memoriam_)

![](img/Weber.jpg)
