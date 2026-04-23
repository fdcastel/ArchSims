<script lang="ts">
  import Etch from '../primitives/Etch.svelte';

  export type ShiftOp = 'shr' | 'shl' | 'ror' | 'rol';

  interface Props {
    ac: number;
    carry: boolean;
    op?: ShiftOp | null;
    title?: string;
  }

  let { ac, carry, op = null, title = 'SHIFT / ROTATE' }: Props = $props();

  const bits = $derived<number[]>(
    Array.from({ length: 8 }, (_, i) => ((ac >>> (7 - i)) & 1)),
  );

  const rightShift = $derived(op === 'shr' || op === 'ror');
  const rotate = $derived(op === 'ror' || op === 'rol');
</script>

<div class="sra">
  <div class="sra-head">
    <Etch>{title}</Etch>
    {#if op}
      <span class="sra-op">{op.toUpperCase()}</span>
    {:else}
      <span class="sra-op sra-op-idle">—</span>
    {/if}
  </div>

  <div class="sra-body" class:dir-right={rightShift} class:dir-left={!rightShift && op}>
    <div class="sra-carry sra-carry-left" class:sra-carry-on={carry}>
      <span class="sra-carry-k">C</span>
      <span class="sra-carry-v">{carry ? 1 : 0}</span>
    </div>

    <div class="sra-arrow sra-arrow-in-l" aria-hidden="true">
      {#if op === 'rol'}↩{:else if op === 'shl'}0{/if}
    </div>

    <div class="sra-strip" aria-label="accumulator bits (MSB left)">
      {#each bits as b, i (i)}
        <div class="sra-cell" class:on={b === 1}>
          <span class="sra-bit">{b}</span>
          <span class="sra-idx">{7 - i}</span>
        </div>
      {/each}
    </div>

    <div class="sra-arrow sra-arrow-in-r" aria-hidden="true">
      {#if op === 'ror'}↪{:else if op === 'shr'}0{/if}
    </div>

    <div class="sra-carry sra-carry-right" class:sra-carry-on={carry}>
      <span class="sra-carry-k">C</span>
      <span class="sra-carry-v">{carry ? 1 : 0}</span>
    </div>
  </div>

  <div class="sra-legend">
    {#if op === 'shr'}bit 0 → C, 0 → bit 7{/if}
    {#if op === 'shl'}bit 7 → C, 0 → bit 0{/if}
    {#if op === 'ror'}bit 0 → C → bit 7 (rotate through carry){/if}
    {#if op === 'rol'}bit 7 → C → bit 0 (rotate through carry){/if}
    {#if !op}waiting for SHR / SHL / ROR / ROL{/if}
  </div>
</div>

<style>
  .sra {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
    display: flex;
    flex-direction: column;
    gap: 8px;
  }
  .sra-head {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
  }
  .sra-op {
    font-size: 11px;
    letter-spacing: 0.22em;
    font-weight: 700;
    color: var(--accent);
    text-shadow: 0 0 6px var(--phosphor-glow);
  }
  :global(.chassis-paper) .sra-op {
    text-shadow: none;
  }
  .sra-op-idle {
    color: var(--silk-dim);
    text-shadow: none;
  }
  .sra-body {
    display: grid;
    grid-template-columns: auto 18px 1fr 18px auto;
    align-items: center;
    gap: 6px;
  }
  .sra-carry {
    display: flex;
    flex-direction: column;
    align-items: center;
    padding: 6px 8px;
    border: 1px solid var(--chassis-edge);
    border-radius: 4px;
    background: #1a1a1a;
    min-width: 28px;
    transition: all 140ms ease;
  }
  :global(.chassis-paper) .sra-carry {
    background: rgba(0, 0, 0, 0.04);
  }
  .sra-carry-k {
    font-size: 9px;
    letter-spacing: 0.22em;
    color: var(--silk-dim);
  }
  .sra-carry-v {
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    font-size: 14px;
    color: var(--silk-dim);
    margin-top: 2px;
  }
  .sra-carry-on {
    border-color: var(--accent);
    background: var(--phosphor-glow);
  }
  .sra-carry-on .sra-carry-v {
    color: var(--accent);
    text-shadow: 0 0 6px var(--phosphor-glow);
  }
  :global(.chassis-paper) .sra-carry-on .sra-carry-v {
    text-shadow: none;
  }

  .sra-arrow {
    height: 28px;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 14px;
    color: var(--silk-dim);
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
  }
  .sra-arrow-in-l {
    border-right: 1px dashed transparent;
  }
  .sra-arrow-in-r {
    border-left: 1px dashed transparent;
  }
  .dir-right .sra-arrow-in-l {
    color: var(--silk);
  }
  .dir-left .sra-arrow-in-r {
    color: var(--silk);
  }

  .sra-strip {
    display: grid;
    grid-template-columns: repeat(8, 1fr);
    gap: 3px;
  }
  .sra-cell {
    display: flex;
    flex-direction: column;
    align-items: center;
    padding: 4px 0 2px;
    background: #1a1a1a;
    border: 1px solid #2a2a2a;
    border-radius: 3px;
    color: var(--silk-dim);
    transition: all 140ms ease;
  }
  :global(.chassis-paper) .sra-cell {
    background: rgba(0, 0, 0, 0.04);
  }
  .sra-cell.on {
    background: var(--phosphor-glow);
    border-color: var(--accent);
    color: var(--accent);
  }
  .sra-bit {
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    font-size: 13px;
    font-weight: 700;
  }
  .sra-cell.on .sra-bit {
    text-shadow: 0 0 6px var(--phosphor-glow);
  }
  :global(.chassis-paper) .sra-cell.on .sra-bit {
    text-shadow: none;
  }
  .sra-idx {
    font-size: 8px;
    letter-spacing: 0.1em;
    color: var(--silk-dim);
    margin-top: 1px;
  }

  .sra-legend {
    font-size: 10px;
    letter-spacing: 0.08em;
    color: var(--silk-dim);
    text-align: center;
  }

  :global(.chassis.ann-off) .sra-legend {
    display: none;
  }

  @media (prefers-reduced-motion: reduce) {
    .sra-cell,
    .sra-carry {
      transition: none;
    }
  }
</style>
