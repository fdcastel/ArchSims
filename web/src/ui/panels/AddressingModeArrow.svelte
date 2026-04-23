<script lang="ts">
  import { onMount, tick } from 'svelte';

  interface Props {
    /** Container whose bounds the SVG covers. Memory cells inside it must carry `data-addr`. */
    container: HTMLElement | null;
    /** Source memory address (operand byte); `null` hides the arrow. */
    from: number | null;
    /** Destination memory address (effective address). */
    to: number | null;
    /** Optional short label rendered near the arrowhead. */
    label?: string;
    /** Override stroke colour. Defaults to `var(--accent)` via CSS. */
    color?: string;
  }

  let {
    container,
    from,
    to,
    label = '',
    color = '',
  }: Props = $props();

  let svgEl: SVGSVGElement | null = $state(null);
  let pathD = $state('');
  let labelX = $state(0);
  let labelY = $state(0);
  let visible = $state(false);
  let resizeTick = $state(0);

  function cellCenter(addr: number): { x: number; y: number } | null {
    if (!container) return null;
    const cell = container.querySelector<HTMLElement>(`[data-addr="${addr}"]`);
    if (!cell) return null;
    const cb = container.getBoundingClientRect();
    const rc = cell.getBoundingClientRect();
    return {
      x: rc.left - cb.left + rc.width / 2,
      y: rc.top - cb.top + rc.height / 2,
    };
  }

  function recompute(): void {
    if (from === null || to === null || !container) {
      visible = false;
      return;
    }
    const src = cellCenter(from);
    const dst = cellCenter(to);
    if (!src || !dst) {
      visible = false;
      return;
    }
    // Build a curved path — more legible than a straight diagonal across the grid.
    const dx = dst.x - src.x;
    const dy = dst.y - src.y;
    const mx = (src.x + dst.x) / 2;
    const my = (src.y + dst.y) / 2;
    // Curve offset perpendicular to the line, scaled by distance.
    const len = Math.sqrt(dx * dx + dy * dy);
    const off = Math.min(40, len / 3);
    const nx = len === 0 ? 0 : -dy / len;
    const ny = len === 0 ? 0 : dx / len;
    const cx = mx + nx * off;
    const cy = my + ny * off;
    pathD = `M ${src.x} ${src.y} Q ${cx} ${cy} ${dst.x} ${dst.y}`;
    labelX = cx;
    labelY = cy;
    visible = true;
  }

  $effect(() => {
    void from;
    void to;
    void container;
    void resizeTick;
    void tick().then(() => recompute());
  });

  onMount(() => {
    const onResize = (): void => {
      resizeTick += 1;
    };
    window.addEventListener('resize', onResize);
    let ro: ResizeObserver | null = null;
    if (container && typeof ResizeObserver !== 'undefined') {
      ro = new ResizeObserver(() => (resizeTick += 1));
      ro.observe(container);
    }
    return () => {
      window.removeEventListener('resize', onResize);
      ro?.disconnect();
    };
  });
</script>

<svg
  bind:this={svgEl}
  class="amo"
  class:visible
  aria-hidden="true"
  xmlns="http://www.w3.org/2000/svg"
>
  <defs>
    <marker
      id="amo-head"
      viewBox="0 0 10 10"
      refX="9"
      refY="5"
      markerWidth="6"
      markerHeight="6"
      orient="auto-start-reverse"
    >
      <path d="M 0 0 L 10 5 L 0 10 z" fill="currentColor" />
    </marker>
  </defs>
  {#if visible && pathD}
    <path
      d={pathD}
      fill="none"
      stroke={color || 'currentColor'}
      stroke-width="2"
      stroke-dasharray="4 3"
      marker-end="url(#amo-head)"
      opacity="0.9"
    />
    {#if label}
      <g transform={`translate(${labelX}, ${labelY})`}>
        <rect x="-22" y="-10" width="44" height="16" rx="3" class="amo-label-bg" />
        <text x="0" y="1" text-anchor="middle" dominant-baseline="middle" class="amo-label">
          {label}
        </text>
      </g>
    {/if}
  {/if}
</svg>

<style>
  .amo {
    position: absolute;
    inset: 0;
    width: 100%;
    height: 100%;
    pointer-events: none;
    z-index: 3;
    color: var(--accent);
    opacity: 0;
    transition: opacity 180ms ease;
  }
  .amo.visible {
    opacity: 1;
  }

  .amo-label {
    fill: var(--silk);
    font-size: 9px;
    letter-spacing: 0.14em;
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
  }
  .amo-label-bg {
    fill: rgba(20, 18, 16, 0.85);
    stroke: currentColor;
    stroke-width: 1;
  }
  :global(.chassis-paper) .amo-label-bg {
    fill: rgba(255, 255, 255, 0.92);
  }

  @media (prefers-reduced-motion: reduce) {
    .amo {
      transition: none;
    }
  }
</style>
