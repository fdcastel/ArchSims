<script lang="ts">
  import type { Snippet } from 'svelte';
  import type { TweaksStore } from '../../stores/tweaks';
  import type { MemFileKind } from '../../core/memfile';
  import { readMemFile, writeMemFile } from '../../core/memfile';

  export interface Diagnostics {
    steps: number;
    reads: number;
    writes: number;
    halted: boolean;
  }

  export interface SampleChoice {
    id: string;
    name: string;
    description?: string;
  }

  interface Props {
    open?: boolean;
    onClose?: () => void;
    tweaks: TweaksStore;
    diagnostics: Diagnostics;
    memKind: MemFileKind;
    memData: () => Uint8Array;
    onMemLoad: (bytes: Uint8Array) => void;
    onReset?: () => void;
    onLoadSample?: () => void;
    sampleLabel?: string;
    samples?: readonly SampleChoice[];
    currentSampleId?: string;
    onLoadSampleById?: (id: string) => void;
    extraSections?: Snippet;
  }

  let {
    open = $bindable(false),
    onClose,
    tweaks,
    diagnostics,
    memKind,
    memData,
    onMemLoad,
    onReset,
    onLoadSample,
    sampleLabel = 'SAMPLE',
    samples,
    currentSampleId,
    onLoadSampleById,
    extraSections,
  }: Props = $props();

  let pendingSampleId = $state<string | undefined>(currentSampleId);
  $effect(() => {
    if (currentSampleId !== undefined) pendingSampleId = currentSampleId;
  });

  let fileInput: HTMLInputElement | null = $state(null);
  let loadError = $state('');

  const close = () => {
    open = false;
    onClose?.();
  };

  function handleSave(): void {
    const bytes = memData();
    const file = writeMemFile(memKind, bytes);
    const blob = new Blob([file], { type: 'application/octet-stream' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `${memKind}-memory.mem`;
    document.body.appendChild(a);
    a.click();
    a.remove();
    URL.revokeObjectURL(url);
  }

  function handleFile(e: Event): void {
    const target = e.target as HTMLInputElement;
    const f = target.files?.[0];
    if (!f) return;
    const reader = new FileReader();
    reader.onload = () => {
      try {
        loadError = '';
        const buf = new Uint8Array(reader.result as ArrayBuffer);
        const bytes = readMemFile(memKind, buf);
        onMemLoad(bytes);
      } catch (err) {
        loadError = err instanceof Error ? err.message : String(err);
      }
    };
    reader.readAsArrayBuffer(f);
    target.value = '';
  }

  type SegOpt<V extends string> = { v: V; label: string; swatch?: boolean };

  const paletteOptions: SegOpt<'amber' | 'green' | 'paper'>[] = [
    { v: 'amber', label: 'Amber', swatch: true },
    { v: 'green', label: 'CRT Green', swatch: true },
    { v: 'paper', label: 'Paper', swatch: true },
  ];
  const baseOptions: SegOpt<'hex' | 'dec' | 'bin'>[] = [
    { v: 'hex', label: 'HEX' },
    { v: 'dec', label: 'DEC' },
    { v: 'bin', label: 'BIN' },
  ];
  const densityOptions: SegOpt<'comfortable' | 'compact'>[] = [
    { v: 'comfortable', label: 'Comfy' },
    { v: 'compact', label: 'Compact' },
  ];
  const annotationOptions: SegOpt<'on' | 'off'>[] = [
    { v: 'on', label: 'On' },
    { v: 'off', label: 'Off' },
  ];
</script>

<div class="sd-backdrop" class:sd-open={open} onclick={close} onkeydown={(e) => e.key === 'Escape' && close()} role="button" tabindex="-1" aria-label="Close service panel"></div>

<aside class="service-drawer" class:sd-open={open} aria-hidden={!open}>
  <div class="sd-head">
    <div class="sd-head-left">
      <span class="sd-head-icon" aria-hidden="true">⌁</span>
      <div>
        <div class="sd-head-title">SERVICE PANEL</div>
        <div class="sd-head-sub">Engineer access · configuration &amp; diagnostics</div>
      </div>
    </div>
    <button type="button" class="sd-close" onclick={close} aria-label="Close">×</button>
  </div>

  <div class="sd-section">
    <div class="sd-section-title">DISPLAY</div>

    <div class="sd-row">
      <div class="sd-row-meta">
        <div class="sd-row-label">Palette</div>
        <div class="sd-row-hint">Phosphor color of the readouts</div>
      </div>
      <div class="sd-row-ctrls">
        <div class="sd-seg">
          {#each paletteOptions as o (o.v)}
            <button type="button" class="sd-seg-btn" class:sd-on={$tweaks.palette === o.v} onclick={() => tweaks.patch({ palette: o.v })}>
              {#if o.swatch}<span class="sd-swatch swatch-{o.v}"></span>{/if}
              <span>{o.label}</span>
            </button>
          {/each}
        </div>
      </div>
    </div>

    <div class="sd-row">
      <div class="sd-row-meta">
        <div class="sd-row-label">Memory base</div>
        <div class="sd-row-hint">How bytes render in the grid</div>
      </div>
      <div class="sd-row-ctrls">
        <div class="sd-seg">
          {#each baseOptions as o (o.v)}
            <button type="button" class="sd-seg-btn" class:sd-on={$tweaks.base === o.v} onclick={() => tweaks.patch({ base: o.v })}>
              <span>{o.label}</span>
            </button>
          {/each}
        </div>
      </div>
    </div>

    <div class="sd-row">
      <div class="sd-row-meta">
        <div class="sd-row-label">Density</div>
        <div class="sd-row-hint">Panel spacing</div>
      </div>
      <div class="sd-row-ctrls">
        <div class="sd-seg">
          {#each densityOptions as o (o.v)}
            <button type="button" class="sd-seg-btn" class:sd-on={$tweaks.density === o.v} onclick={() => tweaks.patch({ density: o.v })}>
              <span>{o.label}</span>
            </button>
          {/each}
        </div>
      </div>
    </div>

    <div class="sd-row">
      <div class="sd-row-meta">
        <div class="sd-row-label">Annotations</div>
        <div class="sd-row-hint">Educational overlays &amp; labels</div>
      </div>
      <div class="sd-row-ctrls">
        <div class="sd-seg">
          {#each annotationOptions as o (o.v)}
            <button type="button" class="sd-seg-btn" class:sd-on={($tweaks.showAnnotations ? 'on' : 'off') === o.v} onclick={() => tweaks.patch({ showAnnotations: o.v === 'on' })}>
              <span>{o.label}</span>
            </button>
          {/each}
        </div>
      </div>
    </div>

    <div class="sd-row">
      <div class="sd-row-meta">
        <div class="sd-row-label">Fetch-cycle widget</div>
        <div class="sd-row-hint">Animate fetch / decode / execute / flags on each step</div>
      </div>
      <div class="sd-row-ctrls">
        <div class="sd-seg">
          {#each annotationOptions as o (o.v)}
            <button type="button" class="sd-seg-btn" class:sd-on={($tweaks.showFetchCycle ? 'on' : 'off') === o.v} onclick={() => tweaks.patch({ showFetchCycle: o.v === 'on' })}>
              <span>{o.label}</span>
            </button>
          {/each}
        </div>
      </div>
    </div>
  </div>

  <div class="sd-section">
    <div class="sd-section-title">MEMORY IMAGE</div>

    <div class="sd-row">
      <div class="sd-row-meta">
        <div class="sd-row-label">Save to file</div>
        <div class="sd-row-hint">Dumps the core image as a .mem file</div>
      </div>
      <div class="sd-row-ctrls">
        <button type="button" class="sd-btn" onclick={handleSave}>
          <span class="sd-btn-icon">↓</span> SAVE .MEM
        </button>
      </div>
    </div>

    <div class="sd-row">
      <div class="sd-row-meta">
        <div class="sd-row-label">Load from file</div>
        <div class="sd-row-hint">Restores a previously saved image</div>
      </div>
      <div class="sd-row-ctrls">
        <button type="button" class="sd-btn" onclick={() => fileInput?.click()}>
          <span class="sd-btn-icon">↑</span> LOAD .MEM
        </button>
        <input bind:this={fileInput} type="file" accept=".mem,application/octet-stream" onchange={handleFile} hidden />
      </div>
    </div>

    {#if loadError}
      <div class="sd-load-error" role="alert">{loadError}</div>
    {/if}

    {#if samples && samples.length > 0 && onLoadSampleById}
      <div class="sd-row sd-row-col">
        <div class="sd-row-meta">
          <div class="sd-row-label">Load sample</div>
          <div class="sd-row-hint">Bundled programs for this machine</div>
        </div>
        <div class="sd-sample-ctrls">
          <select
            class="sd-select"
            bind:value={pendingSampleId}
            aria-label="Sample program"
          >
            {#each samples as s (s.id)}
              <option value={s.id}>{s.name}</option>
            {/each}
          </select>
          <button
            type="button"
            class="sd-btn"
            onclick={() => pendingSampleId && onLoadSampleById(pendingSampleId)}
          >
            <span class="sd-btn-icon">↻</span> LOAD
          </button>
        </div>
        {#if pendingSampleId}
          {@const sel = samples.find((s) => s.id === pendingSampleId)}
          {#if sel?.description}
            <div class="sd-sample-desc">{sel.description}</div>
          {/if}
        {/if}
      </div>
    {:else if onLoadSample}
      <div class="sd-row">
        <div class="sd-row-meta">
          <div class="sd-row-label">Reload sample</div>
          <div class="sd-row-hint">Reset memory to the bundled sample</div>
        </div>
        <div class="sd-row-ctrls">
          <button type="button" class="sd-btn" onclick={onLoadSample}>
            <span class="sd-btn-icon">↻</span> {sampleLabel}
          </button>
        </div>
      </div>
    {/if}
  </div>

  <div class="sd-section">
    <div class="sd-section-title">DIAGNOSTICS</div>
    <div class="sd-diag-grid">
      <div class="sd-diag"><div class="sd-diag-k">STEPS</div><div class="sd-diag-v">{diagnostics.steps}</div></div>
      <div class="sd-diag"><div class="sd-diag-k">READS</div><div class="sd-diag-v">{diagnostics.reads}</div></div>
      <div class="sd-diag"><div class="sd-diag-k">WRITES</div><div class="sd-diag-v">{diagnostics.writes}</div></div>
      <div class="sd-diag"><div class="sd-diag-k">HALTED</div><div class="sd-diag-v">{diagnostics.halted ? 'YES' : 'NO'}</div></div>
    </div>
    {#if onReset}
      <button type="button" class="sd-btn sd-btn-danger" onclick={onReset}>
        <span class="sd-btn-icon">⏻</span> FULL RESET
      </button>
    {/if}
  </div>

  {#if extraSections}
    {@render extraSections()}
  {/if}

  <div class="sd-foot">
    <div>SERVICE ACCESS</div>
    <div>Settings persist locally.</div>
  </div>
</aside>

<style>
  .sd-backdrop {
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.55);
    opacity: 0;
    pointer-events: none;
    transition: opacity 220ms ease;
    z-index: 1180;
    border: none;
  }
  .sd-backdrop.sd-open {
    opacity: 1;
    pointer-events: auto;
  }
  :global(.chassis-paper) .sd-backdrop {
    background: rgba(40, 30, 10, 0.35);
  }

  .service-drawer {
    position: fixed;
    right: 0;
    top: 0;
    bottom: 0;
    width: min(420px, 92vw);
    background: linear-gradient(180deg, #1b1814, #141210);
    border-left: 1px solid var(--chassis-edge);
    box-shadow: -20px 0 60px rgba(0, 0, 0, 0.7);
    transform: translateX(100%);
    transition: transform 260ms cubic-bezier(0.4, 0, 0.2, 1);
    z-index: 1190;
    display: flex;
    flex-direction: column;
    color: var(--silk);
    overflow-y: auto;
  }
  :global(.chassis-paper) .service-drawer {
    background: linear-gradient(180deg, #faf4e4, #efe7d6);
    color: #2b2722;
  }
  .service-drawer.sd-open {
    transform: translateX(0);
  }

  .sd-head {
    padding: 18px 20px 14px;
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    border-bottom: 1px solid rgba(255, 255, 255, 0.06);
    background: linear-gradient(180deg, rgba(255, 255, 255, 0.03), transparent);
    flex-shrink: 0;
  }
  :global(.chassis-paper) .sd-head {
    border-bottom-color: rgba(0, 0, 0, 0.1);
    background: linear-gradient(180deg, rgba(255, 255, 255, 0.5), transparent);
  }
  .sd-head-left {
    display: flex;
    gap: 12px;
  }
  .sd-head-icon {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 34px;
    height: 34px;
    border: 1px solid var(--accent);
    border-radius: 4px;
    color: var(--accent);
    font-size: 18px;
    box-shadow: inset 0 0 8px var(--phosphor-glow);
  }
  .sd-head-title {
    font-size: 13px;
    letter-spacing: 0.3em;
    font-weight: 700;
    color: var(--silk);
  }
  .sd-head-sub {
    font-size: 10px;
    letter-spacing: 0.1em;
    color: var(--silk-dim);
    margin-top: 2px;
  }
  .sd-close {
    background: none;
    border: none;
    color: var(--silk-dim);
    font-size: 24px;
    cursor: pointer;
    line-height: 1;
    padding: 0 4px;
  }
  .sd-close:hover {
    color: var(--silk);
  }

  .sd-section {
    padding: 16px 20px;
    border-bottom: 1px solid rgba(255, 255, 255, 0.04);
  }
  :global(.chassis-paper) .sd-section {
    border-bottom-color: rgba(0, 0, 0, 0.06);
  }
  .sd-section-title {
    font-size: 10px;
    letter-spacing: 0.3em;
    color: var(--silk-etch);
    font-weight: 700;
    margin-bottom: 12px;
    padding-bottom: 4px;
    border-bottom: 1px dashed rgba(255, 255, 255, 0.06);
  }
  :global(.chassis-paper) .sd-section-title {
    border-bottom-color: rgba(0, 0, 0, 0.1);
  }

  .sd-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 12px;
    padding: 8px 0;
  }
  .sd-row-meta {
    flex: 1;
    min-width: 0;
  }
  .sd-row-label {
    font-size: 12px;
    color: var(--silk);
    font-weight: 500;
  }
  .sd-row-hint {
    font-size: 10px;
    color: var(--silk-dim);
    margin-top: 2px;
    letter-spacing: 0.04em;
  }
  .sd-row-ctrls {
    flex-shrink: 0;
  }

  .sd-seg {
    display: flex;
    gap: 2px;
    background: rgba(0, 0, 0, 0.4);
    border: 1px solid var(--chassis-edge);
    border-radius: 4px;
    padding: 2px;
  }
  :global(.chassis-paper) .sd-seg {
    background: rgba(0, 0, 0, 0.06);
  }
  .sd-seg-btn {
    display: inline-flex;
    align-items: center;
    gap: 5px;
    padding: 5px 10px;
    background: transparent;
    border: none;
    color: var(--silk-dim);
    font-size: 10px;
    letter-spacing: 0.14em;
    font-weight: 600;
    font-family: inherit;
    border-radius: 3px;
    cursor: pointer;
  }
  .sd-seg-btn:hover {
    color: var(--silk);
  }
  .sd-seg-btn.sd-on {
    background: var(--accent);
    color: #0a0806;
    box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.25);
  }
  :global(.chassis-paper) .sd-seg-btn.sd-on {
    color: #faf4e4;
  }
  .sd-swatch {
    display: inline-block;
    width: 10px;
    height: 10px;
    border-radius: 50%;
    border: 1px solid rgba(0, 0, 0, 0.5);
  }
  .swatch-amber {
    background: radial-gradient(circle at 35% 30%, #ffd27a, #f4a728 60%, #7a4a0a);
  }
  .swatch-green {
    background: radial-gradient(circle at 35% 30%, #c2ffde, #5dfb9b 60%, #14723d);
  }
  .swatch-paper {
    background: radial-gradient(circle at 35% 30%, #fffdf4, #d8cda9 70%, #9f9472);
    border-color: #7a7262;
  }

  .sd-btn {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    padding: 8px 14px;
    background: linear-gradient(180deg, #2a2520, #161311);
    border: 1px solid var(--chassis-edge);
    color: var(--silk);
    font-family: inherit;
    font-size: 11px;
    letter-spacing: 0.18em;
    font-weight: 600;
    border-radius: 4px;
    cursor: pointer;
    box-shadow:
      inset 0 1px 0 rgba(255, 255, 255, 0.05),
      0 1px 0 rgba(0, 0, 0, 0.5);
  }
  .sd-btn:hover {
    background: linear-gradient(180deg, #3a332c, #1e1a16);
  }
  .sd-btn:active {
    transform: translateY(1px);
    box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.03);
  }
  :global(.chassis-paper) .sd-btn {
    background: linear-gradient(180deg, #f0e6cf, #d6c9a7);
    color: #2b2722;
  }
  :global(.chassis-paper) .sd-btn:hover {
    background: linear-gradient(180deg, #faf0d8, #e6dbba);
  }
  .sd-btn-icon {
    font-size: 13px;
    color: var(--accent);
  }
  .sd-btn-danger {
    margin-top: 12px;
    width: 100%;
    justify-content: center;
  }
  .sd-btn-danger .sd-btn-icon {
    color: var(--red);
  }

  .sd-row-col {
    flex-direction: column;
    align-items: stretch;
    gap: 8px;
  }
  .sd-sample-ctrls {
    display: flex;
    gap: 8px;
    align-items: center;
  }
  .sd-select {
    flex: 1;
    min-width: 0;
    background: rgba(0, 0, 0, 0.4);
    border: 1px solid var(--chassis-edge);
    color: var(--silk);
    font-family: inherit;
    font-size: 11px;
    letter-spacing: 0.1em;
    padding: 7px 10px;
    border-radius: 4px;
    cursor: pointer;
  }
  :global(.chassis-paper) .sd-select {
    background: rgba(0, 0, 0, 0.06);
    color: #2b2722;
  }
  .sd-sample-desc {
    font-size: 10px;
    letter-spacing: 0.04em;
    color: var(--silk-dim);
    font-style: italic;
  }

  .sd-load-error {
    margin-top: 8px;
    padding: 6px 10px;
    font-size: 10px;
    letter-spacing: 0.08em;
    color: var(--red);
    background: rgba(239, 68, 68, 0.08);
    border: 1px solid rgba(239, 68, 68, 0.3);
    border-radius: 4px;
  }

  .sd-diag-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 8px;
    margin-bottom: 10px;
  }
  .sd-diag {
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--chassis-edge);
    border-radius: 4px;
    padding: 8px 10px;
  }
  :global(.chassis-paper) .sd-diag {
    background: rgba(0, 0, 0, 0.04);
  }
  .sd-diag-k {
    font-size: 9px;
    letter-spacing: 0.22em;
    color: var(--silk-dim);
  }
  .sd-diag-v {
    font-size: 18px;
    color: var(--accent);
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    text-shadow: 0 0 6px var(--phosphor-glow);
    margin-top: 2px;
  }
  :global(.chassis-paper) .sd-diag-v {
    text-shadow: none;
  }

  .sd-foot {
    margin-top: auto;
    padding: 14px 20px;
    font-size: 10px;
    letter-spacing: 0.18em;
    color: var(--silk-dim);
    border-top: 1px dashed rgba(255, 255, 255, 0.08);
    display: flex;
    justify-content: space-between;
    flex-shrink: 0;
  }
  :global(.chassis-paper) .sd-foot {
    border-top-color: rgba(0, 0, 0, 0.1);
  }
</style>
