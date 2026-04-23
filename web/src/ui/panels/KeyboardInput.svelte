<script lang="ts">
  import Etch from '../primitives/Etch.svelte';
  import { CesarKeyboardMemoryAddress } from '../../core/cesar';

  interface Props {
    /** Called with the byte value (0..255) that should be written to 0xFFDA. */
    onSubmit: (byte: number) => void;
    /** Override the target address shown in the label. */
    address?: number;
    title?: string;
  }

  let {
    onSubmit,
    address = CesarKeyboardMemoryAddress,
    title = 'KEYBOARD',
  }: Props = $props();

  let textValue = $state('');
  let hexValue = $state('');
  let recent = $state<string[]>([]);

  const addrText = address.toString(16).toUpperCase().padStart(4, '0');

  function submitBytes(bytes: number[]): void {
    if (bytes.length === 0) return;
    for (const b of bytes) {
      onSubmit(b & 0xff);
    }
    recent = [...bytes.map((b) => b.toString(16).toUpperCase().padStart(2, '0')), ...recent].slice(0, 6);
  }

  function handleTextSubmit(e: Event): void {
    e.preventDefault();
    if (textValue.length === 0) return;
    const bytes: number[] = [];
    for (let i = 0; i < textValue.length; i++) {
      bytes.push(textValue.charCodeAt(i) & 0xff);
    }
    submitBytes(bytes);
    textValue = '';
  }

  function handleHexSubmit(e: Event): void {
    e.preventDefault();
    const trimmed = hexValue.trim();
    if (!trimmed) return;
    const parsed = Number.parseInt(trimmed, 16);
    if (Number.isNaN(parsed) || parsed < 0 || parsed > 0xff) return;
    submitBytes([parsed]);
    hexValue = '';
  }
</script>

<div class="kbd">
  <div class="kbd-head">
    <Etch>{title}</Etch>
    <span class="kbd-target">&rarr; MEM[{addrText}]</span>
  </div>

  <form class="kbd-form" onsubmit={handleTextSubmit}>
    <label class="kbd-label" for="kbd-text">ASCII</label>
    <input
      id="kbd-text"
      type="text"
      class="kbd-input"
      bind:value={textValue}
      placeholder="type text, press enter"
      maxlength="16"
      autocomplete="off"
    />
    <button type="submit" class="kbd-btn" disabled={textValue.length === 0}>
      SEND
    </button>
  </form>

  <form class="kbd-form" onsubmit={handleHexSubmit}>
    <label class="kbd-label" for="kbd-hex">HEX</label>
    <input
      id="kbd-hex"
      type="text"
      class="kbd-input kbd-input-hex"
      bind:value={hexValue}
      placeholder="00..FF"
      maxlength="2"
      autocomplete="off"
    />
    <button type="submit" class="kbd-btn" disabled={hexValue.trim().length === 0}>
      SEND
    </button>
  </form>

  {#if recent.length > 0}
    <div class="kbd-recent">
      <span class="kbd-recent-k">RECENT</span>
      <span class="kbd-recent-v">
        {#each recent as r, i (i)}
          <span class="kbd-recent-byte">{r}</span>
        {/each}
      </span>
    </div>
  {/if}
</div>

<style>
  .kbd {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
    display: flex;
    flex-direction: column;
    gap: 8px;
  }
  .kbd-head {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
  }
  .kbd-target {
    font-size: 9px;
    letter-spacing: 0.16em;
    color: var(--silk-dim);
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
  }
  .kbd-form {
    display: grid;
    grid-template-columns: 48px 1fr auto;
    gap: 6px;
    align-items: center;
  }
  .kbd-label {
    font-size: 9px;
    letter-spacing: 0.2em;
    color: var(--silk-dim);
  }
  .kbd-input {
    background: var(--readout-bg);
    color: var(--accent);
    border: 1px solid var(--readout-edge);
    padding: 5px 8px;
    border-radius: 3px;
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    font-size: 11px;
    letter-spacing: 0.06em;
  }
  :global(.chassis-paper) .kbd-input {
    color: var(--accent);
  }
  .kbd-input:focus {
    outline: none;
    border-color: var(--accent);
    box-shadow: 0 0 0 1px var(--accent);
  }
  .kbd-input-hex {
    text-transform: uppercase;
    width: 4.5em;
  }
  .kbd-btn {
    background: linear-gradient(180deg, #2a2520, #161311);
    color: var(--silk);
    border: 1px solid var(--chassis-edge);
    border-radius: 3px;
    padding: 4px 10px;
    font-size: 9px;
    letter-spacing: 0.22em;
    font-weight: 700;
    cursor: pointer;
  }
  :global(.chassis-paper) .kbd-btn {
    background: linear-gradient(180deg, #f0e6cf, #d6c9a7);
    color: #2b2722;
  }
  .kbd-btn:hover:not(:disabled) {
    border-color: var(--accent);
    color: var(--accent);
  }
  .kbd-btn:disabled {
    opacity: 0.4;
    cursor: not-allowed;
  }
  .kbd-recent {
    display: flex;
    align-items: center;
    gap: 8px;
    font-size: 9px;
    letter-spacing: 0.16em;
    color: var(--silk-dim);
    padding-top: 4px;
    border-top: 1px dashed rgba(255, 255, 255, 0.05);
  }
  :global(.chassis-paper) .kbd-recent {
    border-top-color: rgba(0, 0, 0, 0.08);
  }
  .kbd-recent-v {
    display: flex;
    gap: 4px;
  }
  .kbd-recent-byte {
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    padding: 2px 5px;
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--chassis-edge);
    border-radius: 2px;
    color: var(--silk);
  }
  :global(.chassis-paper) .kbd-recent-byte {
    background: rgba(0, 0, 0, 0.05);
  }
</style>
