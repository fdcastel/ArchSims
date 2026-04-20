<script lang="ts">
  import Etch from '../primitives/Etch.svelte';
  import Segmented from '../primitives/Segmented.svelte';
  import type { BitGroup, OperandRow } from './types';

  interface Props {
    addr: number;
    addrDigits?: number;
    groups: BitGroup[];
    operands: OperandRow[];
    accent?: 'amber' | 'green' | 'ink';
  }

  let {
    addr,
    addrDigits = 2,
    groups,
    operands,
    accent = 'amber',
  }: Props = $props();

  const addrText = $derived(
    addr.toString(16).toUpperCase().padStart(addrDigits, '0'),
  );

  const bit = (v: number, i: number): number => (v >> i) & 1;

  function bitsOf(value: number, width: number): number[] {
    const out: number[] = [];
    for (let i = width - 1; i >= 0; i -= 1) out.push(bit(value, i));
    return out;
  }
</script>

<div class="ir-decoder">
  <div class="ir-head">
    <Etch>INSTRUCTION REGISTER</Etch>
    <div class="ir-addr">
      @ <Segmented text={addrText} size="sm" color={accent} />
    </div>
  </div>

  <div class="ir-bits">
    {#each groups as g, i (g.label + i)}
      {#if i > 0}
        <div class="ir-sep"></div>
      {/if}
      <div class="ir-group ir-group-{g.color}">
        <div class="ir-group-label">{g.label}</div>
        <div class="ir-bit-row">
          {#each bitsOf(g.bits, g.width) as b, bi (bi)}
            <div class="ir-bit" class:on={b === 1}>{b}</div>
          {/each}
        </div>
        <div class="ir-group-sub">{g.subLabel || '—'}</div>
      </div>
    {/each}
  </div>

  <div class="ir-operand">
    {#each operands as row (row.key)}
      <div class="ir-operand-row">
        <span class="ir-operand-key">{row.key}</span>
        <Segmented text={row.value} size="md" color={row.color ?? accent} />
        {#if row.hint}
          <span class="ir-operand-bin">{row.hint}</span>
        {/if}
      </div>
    {/each}
  </div>
</div>

<style>
  .ir-decoder {
    background: var(--chassis-2);
    border: 1px solid var(--chassis-edge);
    border-radius: 6px;
    padding: 10px 12px;
    display: flex;
    flex-direction: column;
    gap: 10px;
  }
  .ir-head {
    display: flex;
    justify-content: space-between;
    align-items: baseline;
  }
  .ir-addr {
    font-size: 10px;
    color: var(--silk-dim);
    letter-spacing: 0.2em;
    display: flex;
    align-items: baseline;
    gap: 6px;
  }
  .ir-bits {
    display: flex;
    align-items: flex-end;
    gap: 6px;
  }
  .ir-group {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 4px;
    flex: 1;
  }
  .ir-group-label {
    font-size: 9px;
    letter-spacing: 0.32em;
    color: var(--silk-dim);
  }
  .ir-bit-row {
    display: flex;
    gap: 3px;
  }
  .ir-bit {
    width: 18px;
    height: 22px;
    display: flex;
    align-items: center;
    justify-content: center;
    background: #1a1a1a;
    border: 1px solid #2a2a2a;
    border-radius: 3px;
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    font-size: 11px;
    color: var(--silk-dim);
  }
  .ir-bit.on {
    color: var(--silk);
  }
  .ir-group-I .ir-bit {
    background: #1a1a22;
  }
  .ir-group-I .ir-bit.on {
    background: #1c2a48;
    border-color: #4c7adf;
    color: #9bbcff;
    text-shadow: 0 0 6px rgba(155, 188, 255, 0.35);
  }
  .ir-group-R .ir-bit.on {
    background: #23221a;
    border-color: #d6b24e;
    color: #f7d580;
    text-shadow: 0 0 6px rgba(247, 213, 128, 0.45);
  }
  .ir-group-M .ir-bit.on {
    background: #201a22;
    border-color: #b06bd6;
    color: #e7b3ff;
    text-shadow: 0 0 6px rgba(231, 179, 255, 0.45);
  }
  .ir-group-S .ir-bit.on {
    background: #1a2222;
    border-color: #4ec2b4;
    color: #a5f0e3;
    text-shadow: 0 0 6px rgba(165, 240, 227, 0.45);
  }
  .ir-group-T .ir-bit.on {
    background: #222018;
    border-color: #d6864e;
    color: #f7bc80;
    text-shadow: 0 0 6px rgba(247, 188, 128, 0.45);
  }
  .ir-group-sub {
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 0.14em;
    color: var(--silk);
  }
  .ir-sep {
    width: 1px;
    background: rgba(255, 255, 255, 0.06);
    align-self: stretch;
    margin-bottom: 18px;
  }
  .ir-operand {
    display: flex;
    flex-direction: column;
    gap: 6px;
  }
  .ir-operand-row {
    display: flex;
    align-items: baseline;
    justify-content: space-between;
    gap: 8px;
  }
  .ir-operand-key {
    font-size: 9px;
    letter-spacing: 0.2em;
    color: var(--silk-dim);
  }
  .ir-operand-bin {
    color: var(--silk-dim);
    font-size: 10px;
    letter-spacing: 0.12em;
    font-family: 'IBM Plex Mono', ui-monospace, Menlo, Consolas, monospace;
    text-align: right;
  }
</style>
