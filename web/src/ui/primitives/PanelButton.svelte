<script lang="ts">
  export type PanelButtonVariant = 'default' | 'green' | 'red' | 'amber';

  interface Props {
    label: string;
    sub?: string;
    onClick?: () => void;
    disabled?: boolean;
    variant?: PanelButtonVariant;
    held?: boolean;
  }

  let { label, sub = '', onClick, disabled = false, variant = 'default', held = false }: Props = $props();
</script>

<button
  type="button"
  class="panel-btn"
  class:panel-btn-default={variant === 'default'}
  class:panel-btn-green={variant === 'green'}
  class:panel-btn-red={variant === 'red'}
  class:panel-btn-amber={variant === 'amber'}
  class:held={held}
  onclick={onClick}
  disabled={disabled}
>
  <span class="panel-btn-inner">
    <span class="panel-btn-label">{label}</span>
    {#if sub}
      <span class="panel-btn-sub">{sub}</span>
    {/if}
  </span>
</button>

<style>
  .panel-btn {
    position: relative;
    padding: 12px 8px;
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    background: linear-gradient(180deg, #3a342c, #1a1713);
    box-shadow:
      inset 0 1px 0 rgba(255, 255, 255, 0.08),
      inset 0 -1px 0 rgba(0, 0, 0, 0.4),
      0 2px 0 rgba(0, 0, 0, 0.6);
    cursor: pointer;
    transition:
      transform 60ms ease,
      box-shadow 60ms ease;
    font-family: inherit;
    color: inherit;
  }

  .panel-btn:active:not(:disabled) {
    transform: translateY(1px);
    box-shadow:
      inset 0 1px 0 rgba(255, 255, 255, 0.04),
      0 1px 0 rgba(0, 0, 0, 0.5);
  }

  .panel-btn:disabled {
    opacity: 0.4;
    cursor: not-allowed;
  }

  .panel-btn-inner {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 2px;
  }

  .panel-btn-label {
    font-weight: 700;
    letter-spacing: 0.2em;
    font-size: 13px;
    color: var(--silk);
  }

  .panel-btn-sub {
    font-size: 9px;
    letter-spacing: 0.18em;
    color: var(--silk-dim);
  }

  .panel-btn-green {
    background: linear-gradient(180deg, #225c3a, #0f2d1c);
  }
  .panel-btn-green .panel-btn-label {
    color: #c5ffd9;
    text-shadow: 0 0 8px rgba(93, 251, 155, 0.4);
  }

  .panel-btn-red {
    background: linear-gradient(180deg, #6e2121, #300a0a);
  }
  .panel-btn-red .panel-btn-label {
    color: #ffd0d0;
    text-shadow: 0 0 8px rgba(239, 68, 68, 0.4);
  }

  .panel-btn-amber {
    background: linear-gradient(180deg, #6a4a1a, #2b1d08);
  }
  .panel-btn-amber .panel-btn-label {
    color: #ffe0a0;
    text-shadow: 0 0 8px var(--phosphor-glow);
  }

  :global(.chassis-paper) .panel-btn {
    background: linear-gradient(180deg, #f0e6cf, #c9bd9e);
  }
  :global(.chassis-paper) .panel-btn-label {
    color: #2b2722;
    text-shadow: none;
  }
  :global(.chassis-paper) .panel-btn-green {
    background: linear-gradient(180deg, #a4d8b0, #6ba87e);
  }
  :global(.chassis-paper) .panel-btn-red {
    background: linear-gradient(180deg, #e2a0a0, #b26464);
  }
  :global(.chassis-paper) .panel-btn-amber {
    background: linear-gradient(180deg, #efc88a, #b88b3a);
  }
</style>
