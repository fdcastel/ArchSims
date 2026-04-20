<script lang="ts">
  interface Props {
    on?: boolean;
    onChange?: (next: boolean) => void;
    label?: string;
    disabled?: boolean;
    compact?: boolean;
  }

  let { on = false, onChange, label = '', disabled = false, compact = false }: Props = $props();

  function click() {
    if (disabled) return;
    onChange?.(!on);
  }
</script>

<button
  type="button"
  class="toggle"
  class:toggle-on={on}
  class:toggle-compact={compact}
  onclick={click}
  disabled={disabled}
  aria-pressed={on}
>
  <span class="toggle-track">
    <span class="toggle-knob"></span>
  </span>
  {#if label}
    <span class="toggle-label">{label}</span>
  {/if}
</button>

<style>
  .toggle {
    display: inline-flex;
    align-items: center;
    gap: 8px;
    padding: 4px 6px;
    background: transparent;
    border: none;
    color: inherit;
    cursor: pointer;
    font-family: inherit;
  }

  .toggle:disabled {
    cursor: not-allowed;
    opacity: 0.5;
  }

  .toggle-track {
    position: relative;
    width: 36px;
    height: 16px;
    border-radius: 10px;
    background: #0a0806;
    border: 1px solid var(--chassis-edge);
    box-shadow:
      inset 0 1px 2px rgba(0, 0, 0, 0.6),
      inset 0 -1px 0 rgba(255, 255, 255, 0.03);
    transition: background 140ms ease;
  }

  .toggle-knob {
    position: absolute;
    top: 1px;
    left: 1px;
    width: 12px;
    height: 12px;
    border-radius: 50%;
    background: linear-gradient(180deg, #cfc6b6, #8a8372);
    box-shadow:
      0 1px 2px rgba(0, 0, 0, 0.6),
      inset 0 1px 0 rgba(255, 255, 255, 0.4);
    transition: transform 140ms ease;
  }

  .toggle-on .toggle-track {
    background: var(--accent);
    border-color: var(--accent);
    box-shadow:
      inset 0 0 8px var(--phosphor-glow),
      0 0 6px var(--phosphor-glow);
  }

  .toggle-on .toggle-knob {
    transform: translateX(20px);
    background: linear-gradient(180deg, #fffdf4, #d8cda9);
  }

  .toggle-compact .toggle-track {
    width: 26px;
    height: 12px;
  }
  .toggle-compact .toggle-knob {
    width: 8px;
    height: 8px;
  }
  .toggle-compact.toggle-on .toggle-knob {
    transform: translateX(14px);
  }

  .toggle-label {
    font-size: 11px;
    letter-spacing: 0.16em;
    color: var(--silk);
  }

  :global(.chassis-paper) .toggle-track {
    background: #d8cda9;
    border-color: #a89c80;
    box-shadow: inset 0 1px 2px rgba(0, 0, 0, 0.1);
  }

  :global(.chassis-paper) .toggle-knob {
    background: linear-gradient(180deg, #fff, #c9bd9e);
  }
</style>
