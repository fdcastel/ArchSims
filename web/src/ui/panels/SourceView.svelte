<script lang="ts">
  import Etch from '../primitives/Etch.svelte';

  interface Props {
    /** Raw source text, one line per \n. */
    source: string;
    /** Map from byte-address to 1-based source line number. */
    addrToLine: Map<number, number>;
    /** Current PC address (post-fetch). */
    pc: number;
    /** Address of the currently-in-IR instruction (start). */
    irAddr?: number;
    /** Breakpoints set (addresses). Click on a line toggles a breakpoint at
     *  the lowest address whose addrToLine maps to that line. */
    breakpoints?: ReadonlySet<number>;
    onToggleBreakpoint?: (addr: number) => void;
    title?: string;
  }

  let {
    source,
    addrToLine,
    pc,
    irAddr = -1,
    breakpoints,
    onToggleBreakpoint,
    title = 'SOURCE',
  }: Props = $props();

  const lines = $derived(source.split('\n'));
  const bps = $derived(breakpoints ?? new Set<number>());

  // Build line-number → lowest-address lookup (reverse of addrToLine).
  const lineToAddr = $derived(() => {
    const m = new Map<number, number>();
    for (const [addr, ln] of addrToLine) {
      const prev = m.get(ln);
      if (prev === undefined || addr < prev) m.set(ln, addr);
    }
    return m;
  });

  const pcLine = $derived(addrToLine.get(pc) ?? -1);
  const irLine = $derived(irAddr >= 0 ? (addrToLine.get(irAddr) ?? -1) : -1);

  function isBreakpointLine(line1: number): boolean {
    const addr = lineToAddr().get(line1);
    if (addr === undefined) return false;
    return bps.has(addr);
  }

  function handleClick(line1: number): void {
    const addr = lineToAddr().get(line1);
    if (addr === undefined) return;
    onToggleBreakpoint?.(addr);
  }

  function handleKey(e: KeyboardEvent, line1: number): void {
    if (e.key === 'Enter' || e.key === ' ') {
      e.preventDefault();
      handleClick(line1);
    }
  }
</script>

<div class="src">
  <Etch>{title}</Etch>
  <div class="src-list">
    {#each lines as line, i (i)}
      {@const line1 = i + 1}
      {@const isPC = line1 === pcLine}
      {@const isIR = line1 === irLine}
      {@const isBP = isBreakpointLine(line1)}
      {@const hasAddr = lineToAddr().has(line1)}
      <div
        class="src-row"
        class:src-pc={isPC}
        class:src-ir={isIR}
        class:src-bp={isBP}
        class:src-clickable={hasAddr}
        onclick={() => handleClick(line1)}
        onkeydown={(e) => handleKey(e, line1)}
        role={hasAddr ? 'button' : undefined}
        tabindex={hasAddr ? -1 : undefined}
      >
        <span class="src-gutter">
          {#if isBP}
            <span class="src-bp-dot" aria-label="breakpoint">●</span>
          {:else if isPC}
            <span class="src-caret" aria-label="PC">▶</span>
          {/if}
        </span>
        <span class="src-ln">{line1.toString().padStart(3, ' ')}</span>
        <span class="src-text">{line.length === 0 ? ' ' : line}</span>
      </div>
    {/each}
  </div>
</div>

<style>
  .src {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
  }
  .src-list {
    max-height: 340px;
    overflow-y: auto;
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    font-size: 11px;
    margin-top: 6px;
  }
  .src-row {
    display: grid;
    grid-template-columns: 18px 36px 1fr;
    align-items: center;
    gap: 6px;
    padding: 2px 6px;
    border: 1px solid transparent;
    border-radius: 3px;
    color: var(--silk);
  }
  .src-row.src-clickable {
    cursor: pointer;
  }
  .src-row.src-clickable:hover {
    background: rgba(255, 255, 255, 0.04);
  }
  .src-gutter {
    text-align: center;
  }
  .src-caret {
    color: var(--accent);
    text-shadow: 0 0 6px var(--phosphor-glow);
  }
  :global(.chassis-paper) .src-caret {
    text-shadow: none;
  }
  .src-bp-dot {
    color: #ef4482;
    text-shadow: 0 0 4px #ef4482;
  }
  .src-ln {
    color: var(--silk-dim);
    text-align: right;
    letter-spacing: 0.06em;
  }
  .src-text {
    white-space: pre;
    color: var(--silk);
    letter-spacing: 0.04em;
  }
  .src-pc {
    background: rgba(244, 167, 40, 0.08);
    border-color: rgba(244, 167, 40, 0.35);
  }
  .src-ir {
    background: rgba(76, 122, 223, 0.12);
    border-color: rgba(76, 122, 223, 0.4);
  }
  .src-pc.src-ir {
    background: rgba(244, 167, 40, 0.14);
    border-color: var(--accent);
  }
</style>
