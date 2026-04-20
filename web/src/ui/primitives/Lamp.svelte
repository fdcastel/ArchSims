<script lang="ts">
  export type LampColor = 'amber' | 'green' | 'red' | 'ink';

  interface Props {
    on?: boolean;
    color?: LampColor;
    label?: string;
    sub?: string;
  }

  let { on = false, color = 'amber', label = '', sub = '' }: Props = $props();
</script>

<div class="lamp-wrap">
  <div
    class="lamp"
    class:lamp-on={on}
    class:lamp-amber={color === 'amber'}
    class:lamp-green={color === 'green'}
    class:lamp-red={color === 'red'}
    class:lamp-ink={color === 'ink'}
    aria-label={label || undefined}
    role={label ? 'img' : undefined}
  ></div>
  {#if label}
    <div class="lamp-label">{label}</div>
  {/if}
  {#if sub}
    <div class="lamp-sub">{sub}</div>
  {/if}
</div>

<style>
  .lamp-wrap {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 4px;
    min-width: 40px;
  }

  .lamp {
    width: 14px;
    height: 14px;
    border-radius: 50%;
    background: radial-gradient(circle at 35% 30%, #2a2620, #0a0806);
    border: 1px solid rgba(0, 0, 0, 0.8);
    box-shadow: inset 0 1px 1px rgba(255, 255, 255, 0.04);
    transition: all 120ms ease;
  }

  .lamp-on.lamp-amber,
  .lamp-on.lamp-ink {
    background: radial-gradient(circle at 35% 30%, #ffe0a0, var(--accent) 50%, #7a4a0a);
    box-shadow:
      0 0 10px var(--phosphor-glow),
      0 0 2px var(--accent),
      inset 0 1px 1px rgba(255, 255, 255, 0.4);
  }

  .lamp-on.lamp-green {
    background: radial-gradient(circle at 35% 30%, #e2ffef, #5dfb9b 50%, #14723d);
    box-shadow:
      0 0 10px rgba(93, 251, 155, 0.5),
      0 0 2px #5dfb9b,
      inset 0 1px 1px rgba(255, 255, 255, 0.4);
  }

  .lamp-on.lamp-red {
    background: radial-gradient(circle at 35% 30%, #ffd0d0, #ef4444 55%, #5c0f0f);
    box-shadow:
      0 0 10px rgba(239, 68, 68, 0.55),
      0 0 2px #ef4444,
      inset 0 1px 1px rgba(255, 255, 255, 0.4);
  }

  :global(.chassis-paper) .lamp {
    background: radial-gradient(circle at 35% 30%, #f5eeda, #c7bc9c);
    border-color: #8a7e61;
  }

  :global(.chassis-paper) .lamp-on.lamp-amber {
    background: radial-gradient(circle at 35% 30%, #ffd08a, #c15a19 60%, #6a2d07);
  }

  .lamp-label {
    font-size: 10px;
    letter-spacing: 0.18em;
    color: var(--silk);
    font-weight: 600;
  }

  .lamp-sub {
    font-size: 8px;
    letter-spacing: 0.12em;
    color: var(--silk-dim);
    text-transform: uppercase;
  }
</style>
