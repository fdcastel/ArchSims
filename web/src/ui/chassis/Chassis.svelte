<script lang="ts">
  import type { Snippet } from 'svelte';
  import type { Machine, TweaksStore } from '../../stores/tweaks';
  import Lamp from '../primitives/Lamp.svelte';
  import Scanlines from '../primitives/Scanlines.svelte';

  interface Props {
    machine: Machine;
    title: string;
    sub: string;
    running?: boolean;
    halted?: boolean;
    serial?: string;
    tweaks: TweaksStore;
    serviceOpen?: boolean;
    children: Snippet;
  }

  let {
    machine,
    title,
    sub,
    running = false,
    halted = false,
    serial = '',
    tweaks,
    serviceOpen = $bindable(false),
    children,
  }: Props = $props();

  const MACHINE_ORDER: Machine[] = ['neander', 'ahmes', 'ramses', 'cesar'];

  const accentLamp = $derived<'amber' | 'green' | 'ink'>(
    $tweaks.palette === 'green' ? 'green' : $tweaks.palette === 'paper' ? 'ink' : 'amber',
  );
</script>

<div
  class="chassis"
  class:chassis-amber={$tweaks.palette === 'amber'}
  class:chassis-green={$tweaks.palette === 'green'}
  class:chassis-paper={$tweaks.palette === 'paper'}
  class:density-comfortable={$tweaks.density === 'comfortable'}
  class:density-compact={$tweaks.density === 'compact'}
  class:ann-on={$tweaks.showAnnotations}
  class:ann-off={!$tweaks.showAnnotations}
>
  <Scanlines palette={$tweaks.palette} />

  <header class="chassis-head">
    <div class="chassis-head-left">
      <div class="logo-mark" aria-hidden="true">
        <span class="logo-diamond"></span>
        <span class="logo-diamond logo-diamond-2"></span>
      </div>
      <div class="chassis-titles">
        <div class="chassis-title">{title}</div>
        <div class="chassis-sub">{sub}</div>
      </div>
    </div>
    <div class="chassis-head-right">
      <div class="power">
        <Lamp on={true} color="red" label="PWR" />
        <Lamp on={running && !halted} color={accentLamp} label="RUN" />
        <Lamp on={halted} color="amber" label="HLT" />
      </div>
      {#if serial}
        <div class="serial">{serial}</div>
      {/if}
    </div>
  </header>

  <main class="chassis-main">
    {@render children()}
  </main>

  <footer class="chassis-foot">
    <span>MEMBRANE &middot; FRONT PANEL</span>
    <span class="chassis-foot-mid">
      {#each MACHINE_ORDER as m, i (m)}
        {#if i > 0}
          <span class="chain-sep"> &rarr; </span>
        {/if}
        {#if m === machine}
          <b>{m.toUpperCase()}</b>
        {:else}
          <a class="chain-link" href={`/${m}`}>{m.toUpperCase()}</a>
        {/if}
      {/each}
    </span>
    <button
      type="button"
      class="foot-service-btn"
      class:foot-service-on={serviceOpen}
      onclick={() => (serviceOpen = !serviceOpen)}
      title="Open service panel"
    >
      <span class="foot-service-icon" aria-hidden="true">⌘</span>
      <span>SERVICE PANEL</span>
    </button>
  </footer>
</div>

<style>
  .chassis-head {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 14px 20px;
    border: 1px solid var(--chassis-edge);
    border-radius: 8px;
    background:
      linear-gradient(180deg, rgba(255, 255, 255, 0.04), rgba(0, 0, 0, 0.1)),
      var(--chassis-2);
    box-shadow:
      inset 0 1px 0 rgba(255, 255, 255, 0.05),
      0 2px 0 var(--chassis-edge);
    margin-bottom: 16px;
  }
  :global(.chassis-paper) .chassis-head {
    box-shadow:
      inset 0 1px 0 rgba(255, 255, 255, 0.6),
      0 2px 0 var(--chassis-edge);
    background:
      linear-gradient(180deg, rgba(255, 255, 255, 0.4), rgba(0, 0, 0, 0.02)),
      var(--chassis-2);
  }

  .chassis-head-left {
    display: flex;
    align-items: center;
    gap: 16px;
  }
  .logo-mark {
    width: 36px;
    height: 36px;
    position: relative;
  }
  .logo-diamond {
    position: absolute;
    inset: 8px;
    border: 2px solid var(--accent);
    transform: rotate(45deg);
    border-radius: 2px;
  }
  .logo-diamond-2 {
    inset: 14px;
    border-color: var(--silk);
    opacity: 0.6;
  }

  .chassis-title {
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    font-weight: 700;
    letter-spacing: 0.35em;
    font-size: 22px;
    color: var(--silk);
  }
  .chassis-sub {
    font-size: 10px;
    letter-spacing: 0.25em;
    color: var(--silk-dim);
    margin-top: 2px;
  }
  .chassis-head-right {
    display: flex;
    align-items: center;
    gap: 20px;
  }
  .power {
    display: flex;
    gap: 16px;
  }
  .serial {
    font-size: 10px;
    letter-spacing: 0.2em;
    color: var(--silk-dim);
    padding: 4px 10px;
    border: 1px dashed var(--silk-dim);
    border-radius: 4px;
  }

  .chassis-main {
    display: block;
  }

  .chassis-foot {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-top: 18px;
    padding: 8px 16px;
    font-size: 10px;
    letter-spacing: 0.18em;
    color: var(--silk-dim);
    border-top: 1px dashed rgba(255, 255, 255, 0.08);
  }
  :global(.chassis-paper) .chassis-foot {
    border-top-color: rgba(0, 0, 0, 0.1);
  }
  .chassis-foot-mid b {
    color: var(--accent);
    letter-spacing: 0.24em;
  }
  .chain-link {
    color: inherit;
    text-decoration: none;
  }
  .chain-link:hover {
    color: var(--silk);
  }
  .chain-sep {
    opacity: 0.5;
  }

  .foot-service-btn {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    background: transparent;
    border: 1px dashed var(--silk-dim);
    color: var(--silk-dim);
    padding: 5px 12px;
    border-radius: 4px;
    font-family: inherit;
    font-size: 10px;
    letter-spacing: 0.22em;
    cursor: pointer;
    transition: all 140ms ease;
  }
  .foot-service-btn:hover,
  .foot-service-btn.foot-service-on {
    color: var(--silk);
    border-style: solid;
    border-color: var(--accent);
    background: rgba(255, 255, 255, 0.03);
  }
  :global(.chassis-paper) .foot-service-btn:hover,
  :global(.chassis-paper) .foot-service-btn.foot-service-on {
    background: rgba(0, 0, 0, 0.04);
  }
  .foot-service-icon {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 16px;
    height: 16px;
    border: 1px solid currentColor;
    border-radius: 3px;
    font-size: 11px;
  }
</style>
