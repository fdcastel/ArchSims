<script lang="ts">
  import Segmented from '../primitives/Segmented.svelte';

  type Accent = 'amber' | 'green' | 'ink' | 'dim';

  interface Props {
    name: string;
    value: number;
    width?: 8 | 16;
    active?: boolean;
    accent?: Accent;
    hint?: string;
  }

  let {
    name,
    value,
    width = 8,
    active = false,
    accent = 'amber',
    hint = '',
  }: Props = $props();

  const mask = $derived(width === 16 ? 0xffff : 0xff);
  const masked = $derived(value & mask);
  const hex = $derived(
    masked.toString(16).toUpperCase().padStart(width === 16 ? 4 : 2, '0'),
  );
  const dec = $derived(String(masked).padStart(width === 16 ? 5 : 3, '0'));
  const bin = $derived(masked.toString(2).padStart(width, '0'));
</script>

<div class="reg-tile" class:reg-active={active}>
  <div class="reg-head">
    <span class="reg-name">{name}</span>
    {#if hint}
      <span class="reg-hint">{hint}</span>
    {/if}
  </div>
  <div class="reg-value">
    <Segmented text={hex} size="xl" color={accent} />
  </div>
  <div class="reg-sub-row">
    <div class="reg-sub">
      <span class="reg-sub-key">DEC</span>
      <Segmented text={dec} size="sm" color="dim" />
    </div>
    <div class="reg-sub">
      <span class="reg-sub-key">BIN</span>
      <Segmented text={bin} size="sm" color="dim" />
    </div>
  </div>
</div>

<style>
  .reg-tile {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
    box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.04);
    transition: border-color 160ms ease;
  }
  .reg-tile.reg-active {
    border-color: var(--accent);
    box-shadow:
      inset 0 1px 0 rgba(255, 255, 255, 0.06),
      0 0 0 1px rgba(244, 167, 40, 0.25);
  }
  .reg-head {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
    margin-bottom: 4px;
  }
  .reg-name {
    font-size: 11px;
    letter-spacing: 0.26em;
    color: var(--silk-etch);
    font-weight: 700;
  }
  .reg-hint {
    font-size: 9px;
    letter-spacing: 0.14em;
    color: var(--silk-dim);
    text-transform: uppercase;
  }
  .reg-value {
    padding: 4px 0 6px;
  }
  .reg-sub-row {
    display: flex;
    justify-content: space-between;
    gap: 8px;
  }
  .reg-sub {
    display: flex;
    align-items: baseline;
    gap: 6px;
  }
  .reg-sub-key {
    font-size: 9px;
    letter-spacing: 0.18em;
    color: var(--silk-dim);
  }

  @media (max-width: 640px) {
    .reg-tile {
      padding: 8px 8px;
    }
    .reg-sub-row {
      flex-direction: column;
      gap: 4px;
    }
  }
</style>
