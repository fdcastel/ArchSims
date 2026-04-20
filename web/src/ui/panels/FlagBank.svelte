<script lang="ts">
  import Etch from '../primitives/Etch.svelte';
  import Lamp from '../primitives/Lamp.svelte';
  import type { FlagSpec, LampColor } from './types';

  interface Props {
    flags: FlagSpec[];
    accent?: Exclude<LampColor, 'red'>;
    title?: string;
  }

  let { flags, accent = 'amber', title = 'FLAGS' }: Props = $props();
</script>

<div class="flag-bank">
  <Etch>{title}</Etch>
  <div class="flag-row">
    {#each flags as f (f.label)}
      <Lamp on={f.on} color={f.color ?? accent} label={f.label} sub={f.sub ?? ''} />
    {/each}
  </div>
</div>

<style>
  .flag-bank {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
  }
  .flag-row {
    display: flex;
    gap: 12px;
    margin-top: 6px;
    flex-wrap: wrap;
  }
</style>
