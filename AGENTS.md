# AGENTS.md

Guidance for AI agents working on ArchSims. Read this before touching code.

## The plan is canonical

[doc/WEB_UI_PLAN.md](doc/WEB_UI_PLAN.md) is the source of truth for the web port. **Update it in the same commit as the work.** Flip Status (`❌ OPEN` → `🔧 IN PROGRESS` → `✅ RESOLVED`, `⏯️ DEFERRED` when blocked), append file paths / decisions to Notes, add new tasks at the bottom of their phase. Never re-open closed tasks — add a follow-up.

Other authoritative docs: [doc/Neander.md](doc/Neander.md), [doc/Ahmes.md](doc/Ahmes.md), [doc/Ramses.md](doc/Ramses.md), [doc/Cesar.md](doc/Cesar.md). F# code in [fs/](fs/) is the semantic reference for the TypeScript port.

## Commit discipline

- **Small commits.** One logical change per commit. A CPU port, a component, a test port — each is its own commit. If the diff no longer fits on one screen, split it.
- **Conventional Commits.** `type(scope): subject` — types: `feat`, `fix`, `refactor`, `test`, `docs`, `chore`, `build`, `ci`. Scopes match top-level dirs: `core`, `assemblers`, `ui`, `web`, `doc`, `fs`. Example: `feat(core): port Neander CPU to TypeScript`.
- Commit the plan update in the same commit as the work it describes.

## Testing is non-negotiable

**Not tested = not working.** Never claim code "works" because it compiles, type-checks, or you read it carefully. A feature without a test that fails before your change and passes after is unfinished work.

- Port every F# test to Vitest. Parity target: zero F# tests lacking a TS equivalent.
- `dotnet test` and `pnpm -C web test` must both be green before a task flips to ✅ RESOLVED.
- If you can't write a test for a change (UI polish, CSS), say so explicitly in the commit and verify manually in the browser.

## No mocks

**Tests hit real code paths.** Real `Memory`, real CPU instances, real assembler output, real `.mem` bytes. The F# suite already follows this rule — preserve it.

- No mock CPUs, no mock memory, no stubbed assembler output.
- Integration > unit when the boundary is internal. Only mock at true external edges (network, filesystem outside the project) — and the web app has none of those.
- If a test needs a "fake" because the real thing is awkward, fix the real thing.

## Before you finish

1. `dotnet test` passes.
2. `pnpm -C web test` passes (once `web/` exists).
3. Plan file updated in this commit.
4. Commit message follows Conventional Commits.
