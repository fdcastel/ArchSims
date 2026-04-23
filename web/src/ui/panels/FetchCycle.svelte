<script lang="ts">
  import { onDestroy } from 'svelte';
  import Etch from '../primitives/Etch.svelte';

  export type FetchPhase = 'idle' | 'fetch' | 'decode-src' | 'decode-tgt' | 'execute' | 'flags';

  interface Props {
    /** Number of steps executed so far. Changing this triggers a phase animation. */
    steps: number;
    /** Whether the panel should actually animate. When false, the widget shows 'idle' steady-state. */
    enabled?: boolean;
    /** Total ms for one full 5-phase sweep. */
    durationMs?: number;
    /** Whether to include "decode source" (off for Neander / single-operand machines). */
    hasSource?: boolean;
    title?: string;
  }

  let {
    steps,
    enabled = true,
    durationMs = 400,
    hasSource = true,
    title = 'FETCH CYCLE',
  }: Props = $props();

  const allPhases: Array<{ id: FetchPhase; label: string; sub: string; sourceOnly?: boolean }> = [
    { id: 'fetch', label: 'FETCH', sub: 'read opcode' },
    { id: 'decode-src', label: 'DEC SRC', sub: 'source operand', sourceOnly: true },
    { id: 'decode-tgt', label: 'DEC TGT', sub: 'target operand' },
    { id: 'execute', label: 'EXEC', sub: 'ALU / memory' },
    { id: 'flags', label: 'FLAGS', sub: 'N Z V C' },
  ];
  const phases = $derived(
    allPhases.filter((p) => (p.sourceOnly ? hasSource : true)),
  );

  let phase = $state<FetchPhase>('idle');
  let lastSteps = $state(steps);
  let timers: ReturnType<typeof setTimeout>[] = [];

  function clearTimers(): void {
    for (const t of timers) clearTimeout(t);
    timers = [];
  }

  function playSweep(): void {
    clearTimers();
    const dt = Math.floor(durationMs / (phases.length + 1));
    phases.forEach((p, i) => {
      timers.push(
        setTimeout(() => {
          phase = p.id;
        }, i * dt),
      );
    });
    timers.push(
      setTimeout(() => {
        phase = 'idle';
      }, phases.length * dt),
    );
  }

  $effect(() => {
    if (!enabled) {
      phase = 'idle';
      clearTimers();
      lastSteps = steps;
      return;
    }
    if (steps !== lastSteps) {
      lastSteps = steps;
      playSweep();
    }
  });

  onDestroy(clearTimers);
</script>

<div class="fc" class:fc-idle={phase === 'idle'} class:fc-disabled={!enabled}>
  <div class="fc-head">
    <Etch>{title}</Etch>
    <span class="fc-phase-label">{phase === 'idle' ? 'IDLE' : phase.toUpperCase()}</span>
  </div>
  <div class="fc-row">
    {#each phases as p, i (p.id)}
      <div class="fc-cell" class:active={phase === p.id}>
        <div class="fc-idx">{i + 1}</div>
        <div class="fc-label">{p.label}</div>
        <div class="fc-sub">{p.sub}</div>
      </div>
      {#if i < phases.length - 1}
        <div class="fc-sep" aria-hidden="true">›</div>
      {/if}
    {/each}
  </div>
</div>

<style>
  .fc {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
  }
  .fc.fc-disabled {
    opacity: 0.55;
  }
  .fc-head {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
  }
  .fc-phase-label {
    font-size: 11px;
    letter-spacing: 0.22em;
    font-weight: 700;
    color: var(--accent);
    text-shadow: 0 0 6px var(--phosphor-glow);
  }
  :global(.chassis-paper) .fc-phase-label {
    text-shadow: none;
  }
  .fc.fc-idle .fc-phase-label {
    color: var(--silk-dim);
    text-shadow: none;
  }

  .fc-row {
    display: flex;
    align-items: stretch;
    gap: 4px;
    margin-top: 8px;
  }
  .fc-cell {
    flex: 1;
    display: flex;
    flex-direction: column;
    gap: 2px;
    padding: 6px 4px;
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--chassis-edge);
    border-radius: 3px;
    transition: all 160ms ease;
    text-align: center;
  }
  :global(.chassis-paper) .fc-cell {
    background: rgba(0, 0, 0, 0.04);
  }
  .fc-cell.active {
    background: var(--phosphor-glow);
    border-color: var(--accent);
    box-shadow: inset 0 0 0 1px var(--accent);
  }
  .fc-idx {
    font-size: 8px;
    letter-spacing: 0.1em;
    color: var(--silk-dim);
  }
  .fc-label {
    font-size: 10px;
    letter-spacing: 0.16em;
    font-weight: 700;
    color: var(--silk);
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
  }
  .fc-cell.active .fc-label {
    color: var(--accent);
    text-shadow: 0 0 4px var(--phosphor-glow);
  }
  :global(.chassis-paper) .fc-cell.active .fc-label {
    text-shadow: none;
  }
  .fc-sub {
    font-size: 8px;
    letter-spacing: 0.1em;
    color: var(--silk-dim);
  }
  .fc-sep {
    display: flex;
    align-items: center;
    justify-content: center;
    color: var(--silk-dim);
    font-size: 14px;
  }

  @media (prefers-reduced-motion: reduce) {
    .fc-cell {
      transition: none;
    }
  }
</style>
