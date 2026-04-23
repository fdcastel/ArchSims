<script lang="ts">
  import Etch from '../primitives/Etch.svelte';
  import Segmented from '../primitives/Segmented.svelte';

  interface Props {
    /** Full Cesar memory image (Uint8Array of length 65536). */
    bytes: Uint8Array;
    /** Current SP value (R6). */
    sp: number;
    /** Number of words to show above SP (towards higher addresses). */
    depth?: number;
    /** Show even when SP is 0 and nothing has been pushed. */
    showEmpty?: boolean;
    /** Accent for the top-of-stack highlight. */
    accent?: 'amber' | 'green' | 'ink';
    title?: string;
  }

  let {
    bytes,
    sp,
    depth = 8,
    showEmpty = false,
    accent = 'amber',
    title = 'STACK · R6',
  }: Props = $props();

  // SP points at the top-of-stack word (lower address = deeper).
  // Build a list of words from SP upwards (higher addresses) for visual "upward-growing" rendering.
  const items = $derived<Array<{ addr: number; word: number; isTop: boolean }>>(
    Array.from({ length: depth }, (_, i) => {
      const addr = (sp + i * 2) & 0xffff;
      const hi = bytes[addr] ?? 0;
      const lo = bytes[(addr + 1) & 0xffff] ?? 0;
      return { addr, word: ((hi << 8) | lo) & 0xffff, isTop: i === 0 };
    }),
  );

  const hidden = $derived(sp === 0 && !showEmpty);
</script>

{#if !hidden}
  <div class="stack">
    <div class="stack-head">
      <Etch>{title}</Etch>
      <span class="stack-sp">SP={sp.toString(16).toUpperCase().padStart(4, '0')}</span>
    </div>
    <div class="stack-list">
      {#each items as item, i (item.addr)}
        <div class="stack-row" class:stack-top={item.isTop}>
          <span class="stack-depth">{i === 0 ? '↑TOS' : `+${i}`}</span>
          <span class="stack-addr">{item.addr.toString(16).toUpperCase().padStart(4, '0')}</span>
          <Segmented
            text={item.word.toString(16).toUpperCase().padStart(4, '0')}
            size="sm"
            color={item.isTop ? accent : 'dim'}
          />
        </div>
      {/each}
    </div>
  </div>
{/if}

<style>
  .stack {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
  }
  .stack-head {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
  }
  .stack-sp {
    font-size: 9px;
    letter-spacing: 0.18em;
    color: var(--silk-dim);
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
  }
  .stack-list {
    margin-top: 6px;
    display: flex;
    flex-direction: column-reverse;
    gap: 1px;
  }
  .stack-row {
    display: grid;
    grid-template-columns: 48px 48px 1fr;
    align-items: baseline;
    padding: 3px 6px;
    background: rgba(0, 0, 0, 0.25);
    border: 1px solid transparent;
    border-radius: 3px;
  }
  :global(.chassis-paper) .stack-row {
    background: rgba(0, 0, 0, 0.04);
  }
  .stack-top {
    background: var(--phosphor-glow);
    border-color: var(--accent);
    box-shadow: inset 0 0 0 1px var(--accent);
  }
  .stack-depth {
    font-size: 9px;
    letter-spacing: 0.18em;
    color: var(--silk-dim);
    font-weight: 600;
  }
  .stack-top .stack-depth {
    color: var(--accent);
  }
  .stack-addr {
    font-size: 10px;
    letter-spacing: 0.1em;
    color: var(--silk-dim);
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
  }
</style>
