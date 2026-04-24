<script lang="ts">
  import Etch from '../primitives/Etch.svelte';
  import { CesarDisplayMemoryAddress } from '../../core/cesar';

  interface Props {
    /** Full Cesar memory image (Uint8Array of length 65536). */
    bytes: Uint8Array;
    /** Number of bytes in the display strip (default 36). */
    length?: number;
    /** Override start address (default 0xFFDC). */
    start?: number;
    /** Highlight the cell most recently written by the CPU, if in range. */
    lastWrite?: number | null;
    title?: string;
    /**
     * Reactivity token: caller bumps this on every CPU tick so the cell
     * derivation re-runs even when `bytes` is the same Uint8Array ref (the
     * core mutates in place). See P7-09 / BUG-9.
     */
    tick?: number;
  }

  let {
    bytes,
    length = 36,
    start = CesarDisplayMemoryAddress,
    lastWrite = null,
    title = 'DISPLAY',
    tick = 0,
  }: Props = $props();

  function renderChar(v: number): string {
    if (v === 0) return '·';
    if (v < 0x20 || v > 0x7e) return '·';
    return String.fromCharCode(v);
  }

  const cells = $derived.by<Array<{ addr: number; byte: number; ch: string }>>(() => {
    // The caller must pass a fresh `bytes` ref on each CPU tick (see
    // P7-09 / BUG-9); `tick` is accepted too as a belt-and-suspenders dep.
    const _tick = tick; // eslint-disable-line @typescript-eslint/no-unused-vars
    return Array.from({ length }, (_, i) => {
      const addr = start + i;
      const byte = bytes[addr] ?? 0;
      return { addr, byte, ch: renderChar(byte) };
    });
  });

  const highlightIdx = $derived(
    lastWrite !== null && lastWrite >= start && lastWrite < start + length
      ? lastWrite - start
      : -1,
  );
</script>

<div class="disp">
  <div class="disp-head">
    <Etch>{title}</Etch>
    <span class="disp-range">{start.toString(16).toUpperCase().padStart(4, '0')}..{(start + length - 1).toString(16).toUpperCase().padStart(4, '0')}</span>
  </div>
  <div class="disp-screen" role="presentation">
    {#each cells as cell, i (cell.addr)}
      <div class="disp-cell" class:just-written={i === highlightIdx}>
        <span class="disp-ch">{cell.ch}</span>
      </div>
    {/each}
  </div>
</div>

<style>
  .disp {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
  }
  .disp-head {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
  }
  .disp-range {
    font-size: 9px;
    letter-spacing: 0.18em;
    color: var(--silk-dim);
  }
  .disp-screen {
    margin-top: 8px;
    padding: 10px;
    background: var(--readout-bg);
    border: 1px solid var(--readout-edge);
    border-radius: 4px;
    display: grid;
    grid-template-columns: repeat(36, 1fr);
    gap: 2px;
    box-shadow: inset 0 0 16px rgba(0, 0, 0, 0.6);
  }
  :global(.chassis-paper) .disp-screen {
    box-shadow: inset 0 0 0 1px rgba(0, 0, 0, 0.1);
  }
  .disp-cell {
    display: flex;
    align-items: center;
    justify-content: center;
    min-height: 22px;
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    font-size: 13px;
    color: var(--accent);
    text-shadow: 0 0 4px var(--phosphor-glow);
    border-radius: 2px;
    background: rgba(255, 255, 255, 0.02);
    transition: background 180ms ease;
  }
  :global(.chassis-paper) .disp-cell {
    text-shadow: none;
    background: transparent;
  }
  .disp-cell.just-written {
    background: var(--phosphor-glow);
  }
  .disp-ch {
    letter-spacing: 0;
  }

  @media (prefers-reduced-motion: reduce) {
    .disp-cell {
      transition: none;
    }
  }
</style>
