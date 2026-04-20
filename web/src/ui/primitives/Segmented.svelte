<script lang="ts">
  export type SegmentedSize = 'xl' | 'md' | 'sm';
  export type SegmentedTone = 'amber' | 'green' | 'ink' | 'dim';

  interface Props {
    text: string;
    size?: SegmentedSize;
    color?: SegmentedTone;
  }

  let { text, size = 'md', color = 'amber' }: Props = $props();

  const chars = $derived(text.split(''));
</script>

<span
  class="segmented"
  class:seg-xl={size === 'xl'}
  class:seg-md={size === 'md'}
  class:seg-sm={size === 'sm'}
  class:seg-amber={color === 'amber'}
  class:seg-green={color === 'green'}
  class:seg-ink={color === 'ink'}
  class:seg-dim={color === 'dim'}
>
  {#each chars as c, i (i)}
    <span class="seg-char" class:seg-blank={c === ' '}>{c}</span>
  {/each}
</span>

<style>
  .segmented {
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    font-variant-numeric: tabular-nums;
    color: var(--phosphor);
    text-shadow:
      0 0 8px var(--phosphor-glow),
      0 0 2px var(--phosphor-glow);
    letter-spacing: 0.08em;
    display: inline-flex;
    gap: 1px;
  }

  .seg-xl {
    font-size: 42px;
    font-weight: 600;
    letter-spacing: 0.12em;
  }
  .seg-md {
    font-size: 18px;
    font-weight: 500;
  }
  .seg-sm {
    font-size: 11px;
    font-weight: 500;
    letter-spacing: 0.14em;
  }

  .seg-ink {
    color: var(--phosphor);
    text-shadow: none;
  }
  .seg-dim {
    color: var(--silk-dim);
    text-shadow: none;
  }

  .seg-char.seg-blank {
    opacity: 0.2;
  }
</style>
