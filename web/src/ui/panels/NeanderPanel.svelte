<script lang="ts">
  import {
    createNeanderCpu,
    neanderDisassembleInstructions,
    NeanderInstruction,
    neanderReset,
    neanderStep,
    type NeanderCpu,
  } from '../../core/neander';
  import { SAMPLES_BY_MACHINE } from '../../samples';
  import type { Sample } from '../../samples/types';
  import { createCpuStore } from '../../stores/cpu';
  import { createTweaksStore } from '../../stores/tweaks';
  import Chassis from '../chassis/Chassis.svelte';
  import ServiceDrawer from '../chassis/ServiceDrawer.svelte';
  import Controls from './Controls.svelte';
  import Disassembly from './Disassembly.svelte';
  import FetchCycle from './FetchCycle.svelte';
  import FlagBank from './FlagBank.svelte';
  import IRDecoder from './IRDecoder.svelte';
  import MemoryGrid from './MemoryGrid.svelte';
  import RegisterTile from './RegisterTile.svelte';
  import { fmtBin8, fmtHex2 } from './format';
  import { createRunLoop } from './run-loop';
  import type { BitGroup, DisasmItem, FlagSpec, OperandRow } from './types';

  const SAMPLES = SAMPLES_BY_MACHINE.neander;
  let currentSample = $state<Sample>(SAMPLES[0] as Sample);

  function sampleBytes(): Uint8Array {
    return new Uint8Array(currentSample.bytes);
  }

  const tweaks = createTweaksStore('neander');
  const cpuStore = createCpuStore<NeanderCpu>({
    cpu: createNeanderCpu(),
    step: neanderStep,
    reset: neanderReset,
  });
  cpuStore.cpu.memory.data.set(sampleBytes());

  let running = $state(false);
  let serviceOpen = $state(false);
  let speed = $state(10);
  const breakpoints = $state(new Set<number>());
  let hoveredAddr: number | null = $state(null);
  let lastRead: number | null = $state(null);
  let lastWrite: number | null = $state(null);

  // Track last read/write by diffing memory read/write counters per step.
  let lastReadCount = $state(cpuStore.cpu.memory.readCount);
  let lastWriteCount = $state(cpuStore.cpu.memory.writeCount);

  const runLoop = createRunLoop({
    step: () => stepOne(),
    shouldBreak: () => {
      if (cpuStore.cpu.registers.flags.halted) return true;
      if (breakpoints.has(cpuStore.cpu.registers.programCounter)) return true;
      return false;
    },
  });

  function captureMemoryActivity(before: { r: number; w: number }): void {
    const mem = cpuStore.cpu.memory;
    const ir = cpuStore.cpu.registers.instructionRegister;
    // A write shows the STA target. A read (outside fetch) shows the operand fetched.
    if (mem.writeCount > before.w) {
      lastWrite = ir.operandAddress;
      lastRead = null;
    } else if (mem.readCount > before.r + instructionFetchReads(ir.opCode)) {
      lastRead = ir.operandAddress;
      lastWrite = null;
    } else {
      lastRead = null;
      lastWrite = null;
    }
    lastReadCount = mem.readCount;
    lastWriteCount = mem.writeCount;
  }

  function instructionFetchReads(opCode: number): number {
    // Fetch reads 1 byte (opcode) + 1 more for instructions taking an operand.
    switch (opCode) {
      case NeanderInstruction.Sta:
      case NeanderInstruction.Lda:
      case NeanderInstruction.Add:
      case NeanderInstruction.Or:
      case NeanderInstruction.And:
      case NeanderInstruction.Jmp:
      case NeanderInstruction.Jn:
      case NeanderInstruction.Jz:
        return 2;
      default:
        return 1;
    }
  }

  function stepOne(): void {
    const before = { r: cpuStore.cpu.memory.readCount, w: cpuStore.cpu.memory.writeCount };
    cpuStore.step();
    captureMemoryActivity(before);
  }

  function doStep(): void {
    if (running) return;
    stepOne();
  }

  function doRun(): void {
    running = true;
    runLoop.start(speed);
  }

  function doBreak(): void {
    running = false;
    runLoop.stop();
  }

  function doReset(): void {
    runLoop.stop();
    running = false;
    cpuStore.reset();
    cpuStore.cpu.memory.data.set(sampleBytes());
    lastRead = null;
    lastWrite = null;
    lastReadCount = cpuStore.cpu.memory.readCount;
    lastWriteCount = cpuStore.cpu.memory.writeCount;
  }

  function doFullReset(): void {
    runLoop.stop();
    running = false;
    cpuStore.reset();
    lastRead = null;
    lastWrite = null;
    lastReadCount = 0;
    lastWriteCount = 0;
  }

  function onSpeed(hz: number): void {
    speed = hz;
    if (running) {
      runLoop.stop();
      runLoop.start(hz);
    }
  }

  // After each tick the run-loop's shouldBreak may have halted us; reflect that.
  $effect(() => {
    void $cpuStore.tick;
    if (running && !runLoop.running) running = false;
  });

  function toggleBreakpoint(addr: number): void {
    if (breakpoints.has(addr)) breakpoints.delete(addr);
    else breakpoints.add(addr);
    // Trigger reactivity by reassigning (Sets aren't deep-tracked in runes).
    // eslint-disable-next-line @typescript-eslint/no-unused-expressions
    breakpoints.size;
  }

  // --- Derived data for panels ------------------------------------------------

  const cpu = $derived($cpuStore.cpu);
  // Re-alloc registers + flags on every tick so Svelte $derived propagates.
  // The core mutates cpu.registers in place; without a fresh ref, $derived
  // short-circuits on Object.is and downstream bindings (halted, flags, etc.)
  // stay stale after reset. See P8-02.
  const regs = $derived.by(() => {
    void $cpuStore.tick;
    const r = cpu.registers;
    return { ...r, flags: { ...r.flags }, instructionRegister: { ...r.instructionRegister } };
  });
  const ir = $derived(regs.instructionRegister);
  const irOp = $derived(ir.opCode);
  const irHigh = $derived((irOp & 0xf0) >>> 4);
  const irLow = $derived(irOp & 0x0f);

  const mnemonicFor = (op: number): string => {
    switch (op) {
      case NeanderInstruction.Nop: return 'NOP';
      case NeanderInstruction.Sta: return 'STA';
      case NeanderInstruction.Lda: return 'LDA';
      case NeanderInstruction.Add: return 'ADD';
      case NeanderInstruction.Or: return 'OR';
      case NeanderInstruction.And: return 'AND';
      case NeanderInstruction.Not: return 'NOT';
      case NeanderInstruction.Jmp: return 'JMP';
      case NeanderInstruction.Jn: return 'JN';
      case NeanderInstruction.Jz: return 'JZ';
      case NeanderInstruction.Hlt: return 'HLT';
      default: return '—';
    }
  };

  const isTwoByte = (op: number): boolean =>
    op === NeanderInstruction.Sta ||
    op === NeanderInstruction.Lda ||
    op === NeanderInstruction.Add ||
    op === NeanderInstruction.Or ||
    op === NeanderInstruction.And ||
    op === NeanderInstruction.Jmp ||
    op === NeanderInstruction.Jn ||
    op === NeanderInstruction.Jz;

  const irStart = $derived(
    // Last-fetched instruction begins one (or two) bytes behind PC.
    (regs.programCounter - (isTwoByte(irOp) ? 2 : 1) + 256) & 0xff,
  );
  const irSize = $derived(isTwoByte(irOp) ? 2 : 1);

  const groups = $derived<BitGroup[]>([
    {
      label: 'I I I I',
      bits: irHigh,
      width: 4,
      subLabel: mnemonicFor(irOp),
      color: 'I',
    },
    {
      label: '- - - -',
      bits: irLow,
      width: 4,
      subLabel: 'unused',
      color: 'M',
    },
  ]);

  const operandByte = $derived(ir.operandAddress);
  const operands = $derived<OperandRow[]>(
    isTwoByte(irOp)
      ? [
          { key: 'OPCODE', value: fmtHex2(irOp), hint: fmtBin8(irOp) },
          {
            key: 'OPERAND',
            value: fmtHex2(operandByte),
            hint: `→ MEM[${fmtHex2(operandByte)}]`,
          },
        ]
      : [{ key: 'OPCODE', value: fmtHex2(irOp), hint: fmtBin8(irOp) }],
  );

  const flags = $derived<FlagSpec[]>([
    { label: 'N', sub: 'neg', on: regs.flags.negative },
    { label: 'Z', sub: 'zero', on: regs.flags.zero },
    { label: 'HLT', sub: 'halt', on: regs.flags.halted, color: 'red' },
  ]);

  const disasm = $derived<DisasmItem[]>(() => {
    const items: DisasmItem[] = [];
    const bytes = cpu.memory.data;
    let addr = 0;
    while (addr < 256) {
      const slice: number[] = [];
      for (let i = addr; i < Math.min(addr + 2, 256); i++) slice.push(bytes[i] ?? 0);
      const [d] = neanderDisassembleInstructions(slice);
      if (!d || d.size === 0) break;
      items.push({
        addr,
        text: d.text,
        size: d.size,
        bytes: Array.from(bytes.slice(addr, addr + d.size)),
      });
      addr += d.size;
    }
    return items;
  });

  const accentAll = $derived<'amber' | 'green' | 'ink'>(
    $tweaks.palette === 'green' ? 'green' : $tweaks.palette === 'paper' ? 'ink' : 'amber',
  );

  const diagnostics = $derived({
    steps: $cpuStore.tick,
    reads: cpu.memory.readCount,
    writes: cpu.memory.writeCount,
    halted: regs.flags.halted,
  });

  function memData(): Uint8Array {
    return new Uint8Array(cpu.memory.data);
  }

  function onMemLoad(bytes: Uint8Array): void {
    cpu.memory.data.set(bytes);
    cpuStore.reset();
    cpu.memory.data.set(bytes);
    lastRead = null;
    lastWrite = null;
  }

  function onLoadSampleById(id: string): void {
    const found = SAMPLES.find((s) => s.id === id);
    if (!found) return;
    currentSample = found;
    doReset();
  }
</script>

<Chassis
  machine="neander"
  title="NEANDER"
  sub="8-BIT · ACCUMULATOR · 256 BYTES"
  running={running && !regs.flags.halted}
  halted={regs.flags.halted}
  serial="SN-0001 / AP-1971"
  {tweaks}
  bind:serviceOpen
>
  <div class="panel-grid">
    <section class="col col-left">
      <div class="tiles">
        <RegisterTile
          name="AC"
          value={regs.accumulator}
          active={true}
          accent={accentAll}
          hint="accumulator"
        />
        <RegisterTile name="PC" value={regs.programCounter} hint="program counter" />
      </div>

      <FlagBank {flags} accent={accentAll} title="FLAGS" />

      {#if $tweaks.showFetchCycle}
        <FetchCycle steps={$cpuStore.tick} hasSource={false} />
      {/if}

      <IRDecoder
        addr={irStart}
        {groups}
        operands={operands}
        accent={accentAll}
      />

      {#if $tweaks.showAnnotations}
        <div class="annot">
          <span class="annot-k">HINT</span>
          <span class="annot-v"
            >Low nibble is always 0 on Neander. <b>0x70</b> is used by
            <a href="/ahmes">Ahmes</a> for SUB.</span
          >
        </div>
      {/if}

      <Controls
        halted={regs.flags.halted}
        {running}
        {speed}
        accent={accentAll}
        onStep={doStep}
        onRun={doRun}
        onBreak={doBreak}
        onReset={doReset}
        {onSpeed}
        counters={[
          { label: 'STEPS', value: $cpuStore.tick, digits: 5 },
          { label: 'READS', value: cpu.memory.readCount, digits: 5, color: 'dim' },
          { label: 'WRITES', value: cpu.memory.writeCount, digits: 5, color: 'dim' },
        ]}
      />
    </section>

    <section class="col col-mid">
      <MemoryGrid
        bytes={cpu.memory.data}
        base={$tweaks.base}
        pc={regs.programCounter}
        irStart={irStart}
        irSize={irSize}
        lastRead={lastRead}
        lastWrite={lastWrite}
        effAddr={isTwoByte(irOp) ? ir.operandAddress : null}
        breakpoints={breakpoints}
        hoveredAddr={hoveredAddr}
        onToggleBreakpoint={toggleBreakpoint}
        onHover={(a) => (hoveredAddr = a)}
      />
    </section>

    <section class="col col-right">
      <Disassembly
        items={disasm()}
        pc={regs.programCounter}
        irAddr={irStart}
        addrDigits={2}
        breakpoints={breakpoints}
        onToggleBreakpoint={toggleBreakpoint}
      />
    </section>
  </div>
</Chassis>

<ServiceDrawer
  bind:open={serviceOpen}
  {tweaks}
  {diagnostics}
  memKind="neander"
  {memData}
  {onMemLoad}
  onReset={doFullReset}
  onLoadSample={doReset}
  sampleLabel="RELOAD SAMPLE"
  samples={SAMPLES}
  currentSampleId={currentSample.id}
  {onLoadSampleById}
/>

<style>
  .panel-grid {
    display: grid;
    grid-template-columns: minmax(0, 22rem) minmax(0, 1fr) minmax(0, 18rem);
    gap: 14px;
    align-items: start;
  }
  @media (max-width: 1024px) {
    .panel-grid {
      grid-template-columns: 1fr;
    }
  }
  .col {
    display: flex;
    flex-direction: column;
    gap: 12px;
    min-width: 0;
  }
  .tiles {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 10px;
  }
  .annot {
    font-size: 10px;
    letter-spacing: 0.08em;
    color: var(--silk-dim);
    padding: 8px 10px;
    background: rgba(255, 255, 255, 0.02);
    border-left: 2px solid var(--accent);
    border-radius: 3px;
  }
  :global(.chassis.ann-off) .annot {
    display: none;
  }
  .annot-k {
    color: var(--accent);
    margin-right: 6px;
    font-weight: 700;
  }
  .annot b {
    color: var(--silk);
  }
  .annot a {
    color: var(--accent);
  }
</style>
