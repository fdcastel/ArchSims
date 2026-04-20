<script lang="ts">
  import Etch from '../primitives/Etch.svelte';
  import { formatByte, fmtHex1, fmtHex2 } from './format';
  import type { Base } from '../../stores/tweaks';

  interface Props {
    bytes: Uint8Array;
    base: Base;
    pc: number;
    irStart?: number;
    irSize?: number;
    lastRead?: number | null;
    lastWrite?: number | null;
    effAddr?: number | null;
    breakpoints?: ReadonlySet<number>;
    hoveredAddr?: number | null;
    onToggleBreakpoint?: (addr: number) => void;
    onHover?: (addr: number | null) => void;
    pageStart?: number;
    pageSize?: number;
    title?: string;
  }

  let {
    bytes,
    base,
    pc,
    irStart = -1,
    irSize = 1,
    lastRead = null,
    lastWrite = null,
    effAddr = null,
    breakpoints,
    hoveredAddr = null,
    onToggleBreakpoint,
    onHover,
    pageStart = 0,
    pageSize = 256,
    title,
  }: Props = $props();

  const bps = $derived(breakpoints ?? new Set<number>());

  const rows = $derived(() => {
    const out: number[][] = [];
    const end = Math.min(pageStart + pageSize, bytes.length);
    for (let row = pageStart; row < end; row += 16) {
      const cols: number[] = [];
      for (let c = 0; c < 16; c += 1) cols.push(row + c);
      out.push(cols);
    }
    return out;
  });

  const headerTitle = $derived(
    title ?? `MEMORY · ${pageSize === 256 ? '256 BYTES' : `${pageSize} BYTES`}`,
  );

  function classFor(addr: number): string {
    if (addr >= bytes.length) return 'mcell mcell-oob';
    const v = bytes[addr] ?? 0;
    const isPC = addr === pc;
    const isIrStart = irStart >= 0 && addr === irStart;
    const isIrSpan = irStart >= 0 && addr >= irStart && addr < irStart + irSize;
    const isRead = lastRead !== null && addr === lastRead;
    const isWrite = lastWrite !== null && addr === lastWrite;
    const isEff = effAddr !== null && addr === effAddr;
    const isHover = hoveredAddr === addr;
    const isBP = bps.has(addr);
    const classes = ['mcell'];
    if (v === 0) classes.push('mcell-zero');
    if (isIrSpan) classes.push('mcell-ir');
    if (isIrStart) classes.push('mcell-ir-start');
    if (isRead && !isWrite) classes.push('mcell-read');
    if (isWrite) classes.push('mcell-write');
    if (isEff) classes.push('mcell-eff');
    if (isPC) classes.push('mcell-pc');
    if (isHover) classes.push('mcell-hover');
    if (isBP) classes.push('mcell-bp');
    return classes.join(' ');
  }

  function rowLabel(addr: number): string {
    return (addr >> 4).toString(16).toUpperCase().padStart(2, '0');
  }
</script>

<div class="mem-wrap">
  <div class="mem-head">
    <Etch>{headerTitle}</Etch>
    <div class="mem-legend">
      <span class="lg lg-pc">PC</span>
      <span class="lg lg-ir">IR</span>
      <span class="lg lg-read">READ</span>
      <span class="lg lg-write">WRITE</span>
      <span class="lg lg-bp">BP</span>
    </div>
  </div>
  <div class="mem-grid">
    <div class="mrow mrow-header">
      <div class="mrow-label mrow-label-corner"></div>
      {#each Array.from({ length: 16 }, (_, c) => c) as c (c)}
        <div class="mhead">{fmtHex1(c)}</div>
      {/each}
    </div>
    {#each rows() as row (row[0])}
      <div class="mrow">
        <div class="mrow-label">{rowLabel(row[0] ?? 0)}<span class="mrow-label-zero">0</span></div>
        {#each row as addr (addr)}
          <div
            class={classFor(addr)}
            data-addr={addr}
            onclick={() => onToggleBreakpoint?.(addr)}
            onmouseenter={() => onHover?.(addr)}
            onmouseleave={() => onHover?.(null)}
            onkeydown={(e) => {
              if (e.key === 'Enter' || e.key === ' ') onToggleBreakpoint?.(addr);
            }}
            role="button"
            tabindex="-1"
            title={`addr ${fmtHex2(addr)} (${addr})  value ${fmtHex2(bytes[addr] ?? 0)} / ${bytes[addr] ?? 0}`}
          >
            <span class="mcell-val">{formatByte(bytes[addr] ?? 0, base)}</span>
          </div>
        {/each}
      </div>
    {/each}
  </div>
</div>

<style>
  .mem-wrap {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 12px;
  }
  .mem-head {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 10px;
  }
  .mem-legend {
    display: flex;
    gap: 6px;
    font-size: 9px;
    letter-spacing: 0.16em;
  }
  .lg {
    padding: 2px 5px;
    border-radius: 3px;
    color: var(--silk-dim);
    border: 1px solid rgba(255, 255, 255, 0.08);
  }
  .lg-pc {
    color: var(--accent);
    border-color: var(--accent);
  }
  .lg-ir {
    color: #9bbcff;
    border-color: #4c7adf;
  }
  .lg-read {
    color: #8dfbbf;
    border-color: rgba(93, 251, 155, 0.4);
  }
  .lg-write {
    color: #ffb4b4;
    border-color: rgba(239, 68, 68, 0.5);
  }
  .lg-bp {
    color: #ff8a9a;
    border-color: rgba(239, 68, 130, 0.4);
  }
  .mem-grid {
    background: var(--readout-bg);
    border: 1px solid var(--readout-edge);
    border-radius: 6px;
    padding: 8px;
    box-shadow: inset 0 0 14px rgba(0, 0, 0, 0.5);
  }
  .mrow {
    display: grid;
    grid-template-columns: 34px repeat(16, 1fr);
    gap: var(--grid-gap);
    margin-bottom: var(--grid-gap);
  }
  .mrow-header {
    margin-bottom: 4px;
  }
  .mrow-label,
  .mhead {
    font-size: 9px;
    letter-spacing: 0.1em;
    color: var(--silk-dim);
    display: flex;
    align-items: center;
    justify-content: center;
  }
  .mrow-label-zero {
    opacity: 0.4;
  }
  .mhead {
    padding-bottom: 2px;
  }
  .mcell {
    position: relative;
    min-height: 26px;
    display: flex;
    align-items: center;
    justify-content: center;
    background: transparent;
    color: var(--silk-dim);
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    font-size: 11px;
    border: 1px solid rgba(255, 255, 255, 0.03);
    border-radius: 3px;
    cursor: pointer;
    transition:
      background 120ms ease,
      border-color 120ms ease;
  }
  .mcell:not(.mcell-zero) {
    color: var(--silk);
  }
  .mcell-val {
    letter-spacing: 0.04em;
  }
  .mcell-hover {
    background: rgba(255, 255, 255, 0.08);
  }
  .mcell-ir {
    background: rgba(76, 122, 223, 0.12);
  }
  .mcell-ir-start {
    border-color: #4c7adf;
    box-shadow: 0 0 0 1px rgba(76, 122, 223, 0.3);
  }
  .mcell-pc {
    background: var(--phosphor-glow);
    color: var(--silk);
    border-color: var(--accent);
    box-shadow: 0 0 0 1px var(--accent);
  }
  .mcell-read {
    background: rgba(93, 251, 155, 0.16);
    color: #8dfbbf;
  }
  .mcell-write {
    background: rgba(239, 68, 68, 0.22);
    color: #ffb4b4;
    border-color: #ef4444;
  }
  .mcell-eff {
    outline: 1px dashed rgba(231, 179, 255, 0.5);
    outline-offset: -2px;
  }
  .mcell-bp::before {
    content: '';
    position: absolute;
    top: 2px;
    right: 2px;
    width: 6px;
    height: 6px;
    border-radius: 50%;
    background: #ef4482;
    box-shadow: 0 0 4px #ef4482;
  }
  .mcell-oob {
    visibility: hidden;
  }
</style>
