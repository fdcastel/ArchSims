<script lang="ts">
  import Etch from '../primitives/Etch.svelte';
  import { fmtHex2, fmtHex4 } from './format';
  import type { DisasmItem } from './types';

  interface Props {
    items: DisasmItem[];
    pc: number;
    irAddr?: number;
    addrDigits?: 2 | 4;
    title?: string;
    onToggleBreakpoint?: (addr: number) => void;
    breakpoints?: ReadonlySet<number>;
  }

  let {
    items,
    pc,
    irAddr = -1,
    addrDigits = 2,
    title = 'DISASSEMBLY',
    onToggleBreakpoint,
    breakpoints,
  }: Props = $props();

  const bps = $derived(breakpoints ?? new Set<number>());
  const fmtAddr = $derived<(a: number) => string>(
    addrDigits === 4 ? fmtHex4 : fmtHex2,
  );
</script>

<div class="disasm">
  <Etch>{title}</Etch>
  <div class="disasm-list">
    {#each items as item (item.addr)}
      {@const isPC = item.addr === pc}
      {@const isIR = item.addr === irAddr}
      {@const isBP = bps.has(item.addr)}
      <div
        class="disasm-row"
        class:disasm-ir={isIR}
        class:disasm-pc={isPC}
        class:disasm-bp={isBP}
        onclick={() => onToggleBreakpoint?.(item.addr)}
        onkeydown={(e) => {
          if (e.key === 'Enter' || e.key === ' ') onToggleBreakpoint?.(item.addr);
        }}
        role="button"
        tabindex="-1"
      >
        <span class="disasm-gutter">
          {#if isBP}
            <span class="disasm-bp-dot" aria-label="breakpoint">●</span>
          {:else if isPC}
            <span class="disasm-caret" aria-label="PC">▶</span>
          {/if}
        </span>
        <span class="disasm-addr">{fmtAddr(item.addr)}</span>
        {#if item.bytes && item.bytes.length > 0}
          <span class="disasm-bytes">
            {#each item.bytes as b, i (i)}
              <span class="disasm-byte">{fmtHex2(b)}</span>
            {/each}
          </span>
        {:else}
          <span class="disasm-bytes disasm-bytes-placeholder"></span>
        {/if}
        <span class="disasm-text">
          {#if item.label}
            <span class="disasm-label">:{item.label}</span>
          {/if}
          <span class="disasm-mnem">{item.text}</span>
        </span>
      </div>
    {/each}
  </div>
</div>

<style>
  .disasm {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
  }
  .disasm-list {
    display: flex;
    flex-direction: column;
    gap: 1px;
    font-size: 11px;
    margin-top: 6px;
    max-height: 340px;
    overflow-y: auto;
  }
  .disasm-row {
    display: grid;
    grid-template-columns: 18px 48px 84px 1fr;
    align-items: center;
    gap: 8px;
    padding: 3px 6px;
    border: 1px solid transparent;
    border-radius: 3px;
    cursor: pointer;
    background: transparent;
    font-family: inherit;
    color: inherit;
    text-align: left;
  }
  .disasm-row:hover {
    background: rgba(255, 255, 255, 0.04);
  }
  .disasm-gutter {
    text-align: center;
  }
  .disasm-caret {
    color: var(--accent);
    text-shadow: 0 0 6px var(--phosphor-glow);
  }
  .disasm-bp-dot {
    color: #ef4482;
    text-shadow: 0 0 4px #ef4482;
  }
  .disasm-addr {
    color: var(--silk-dim);
    letter-spacing: 0.08em;
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
  }
  .disasm-bytes {
    display: flex;
    gap: 3px;
  }
  .disasm-byte {
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    color: var(--silk-dim);
    font-size: 10px;
  }
  .disasm-text {
    display: flex;
    align-items: baseline;
    gap: 8px;
    color: var(--silk);
  }
  .disasm-label {
    color: #e7b3ff;
    font-weight: 600;
  }
  .disasm-mnem {
    letter-spacing: 0.06em;
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
  }
  .disasm-ir {
    background: rgba(76, 122, 223, 0.12);
    border-color: rgba(76, 122, 223, 0.4);
  }
  .disasm-pc .disasm-addr,
  .disasm-pc .disasm-mnem {
    color: var(--silk);
  }
  .disasm-pc {
    background: rgba(244, 167, 40, 0.08);
    border-color: rgba(244, 167, 40, 0.35);
  }
  .disasm-bp::before {
    content: '';
    position: absolute;
  }
</style>
