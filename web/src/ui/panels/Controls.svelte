<script lang="ts">
  import Etch from '../primitives/Etch.svelte';
  import PanelButton from '../primitives/PanelButton.svelte';
  import Segmented from '../primitives/Segmented.svelte';
  import type { Counter, SpeedOption } from './types';

  interface Props {
    halted: boolean;
    running: boolean;
    speed: number;
    speeds?: SpeedOption[];
    counters?: Counter[];
    accent?: 'amber' | 'green' | 'ink';
    onStep: () => void;
    onRun: () => void;
    onBreak: () => void;
    onReset: () => void;
    onSpeed: (hz: number) => void;
  }

  const DEFAULT_SPEEDS: SpeedOption[] = [
    { label: '1 Hz', hz: 1 },
    { label: '10 Hz', hz: 10 },
    { label: '100 Hz', hz: 100 },
    { label: 'MAX', hz: 0 },
  ];

  let {
    halted,
    running,
    speed,
    speeds = DEFAULT_SPEEDS,
    counters = [],
    accent = 'amber',
    onStep,
    onRun,
    onBreak,
    onReset,
    onSpeed,
  }: Props = $props();

  function counterText(c: Counter): string {
    const s = String(c.value);
    return c.digits ? s.padStart(c.digits, '0') : s;
  }
</script>

<div class="controls">
  <Etch>CONTROL</Etch>
  <div class="ctrl-grid">
    <PanelButton
      label="STEP"
      sub="1 INSTR"
      onClick={onStep}
      disabled={halted || running}
    />
    {#if running}
      <PanelButton label="BREAK" sub="PAUSE" variant="red" onClick={onBreak} />
    {:else}
      <PanelButton
        label="RUN"
        sub="CONTINUOUS"
        variant="green"
        onClick={onRun}
        disabled={halted}
      />
    {/if}
    <PanelButton label="RESET" sub="PC \u2190 00" variant="amber" onClick={onReset} />
  </div>

  <div class="ctrl-speed">
    <Etch inline={true}>SPEED</Etch>
    <div class="ctrl-speed-row">
      {#each speeds as s (s.hz)}
        <button
          type="button"
          class="speed-btn"
          class:speed-on={speed === s.hz}
          onclick={() => onSpeed(s.hz)}
        >
          {s.label}
        </button>
      {/each}
    </div>
  </div>

  {#if counters.length > 0}
    <div class="ctrl-counters">
      {#each counters as c (c.label)}
        <div class="ctr">
          <span class="ctr-k">{c.label}</span>
          <Segmented text={counterText(c)} size="sm" color={c.color ?? accent} />
        </div>
      {/each}
    </div>
  {/if}
</div>

<style>
  .controls {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
  }
  .ctrl-grid {
    display: grid;
    grid-template-columns: 1fr 1fr 1fr;
    gap: 10px;
    margin-top: 6px;
  }
  .ctrl-speed {
    margin-top: 12px;
    display: flex;
    align-items: center;
    gap: 10px;
  }
  .ctrl-speed-row {
    display: flex;
    gap: 4px;
    flex: 1;
  }
  .speed-btn {
    flex: 1;
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--chassis-edge);
    color: var(--silk-dim);
    font-family: inherit;
    font-size: 10px;
    letter-spacing: 0.16em;
    padding: 4px 6px;
    border-radius: 3px;
    cursor: pointer;
  }
  .speed-btn:hover {
    color: var(--silk);
  }
  .speed-btn.speed-on {
    background: var(--accent);
    color: #0a0806;
    border-color: var(--accent);
  }
  .ctrl-counters {
    display: flex;
    justify-content: space-between;
    margin-top: 12px;
    gap: 8px;
    flex-wrap: wrap;
  }
  .ctr {
    display: flex;
    flex-direction: column;
    gap: 2px;
    align-items: flex-start;
  }
  .ctr-k {
    font-size: 9px;
    letter-spacing: 0.2em;
    color: var(--silk-dim);
  }
</style>
