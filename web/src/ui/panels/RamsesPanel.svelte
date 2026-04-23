<script lang="ts">
  import {
    createRamsesCpu,
    ramsesDisassembleInstruction,
    RamsesAddressMode,
    RamsesAddressModeMask,
    RamsesInstruction,
    RamsesInstructionMask,
    RamsesRegister,
    RamsesRegisterMask,
    ramsesReset,
    ramsesStep,
    type RamsesCpu,
  } from '../../core/ramses';
  import { SAMPLES_BY_MACHINE } from '../../samples';
  import type { Sample } from '../../samples/types';
  import { createCpuStore } from '../../stores/cpu';
  import { createTweaksStore } from '../../stores/tweaks';
  import Chassis from '../chassis/Chassis.svelte';
  import ServiceDrawer from '../chassis/ServiceDrawer.svelte';
  import AddressingModeArrow from './AddressingModeArrow.svelte';
  import Controls from './Controls.svelte';
  import Disassembly from './Disassembly.svelte';
  import FetchCycle from './FetchCycle.svelte';
  import FlagBank from './FlagBank.svelte';
  import IRDecoder from './IRDecoder.svelte';
  import MemoryGrid from './MemoryGrid.svelte';
  import RegisterTile from './RegisterTile.svelte';
  import SourceView from './SourceView.svelte';
  import { fmtBin8, fmtHex2 } from './format';
  import { createRunLoop } from './run-loop';
  import type { BitGroup, DisasmItem, FlagSpec, OperandRow } from './types';

  const SAMPLES = SAMPLES_BY_MACHINE.ramses;
  let currentSample = $state<Sample>(SAMPLES[0] as Sample);

  const addrToLabel = $derived.by<Map<number, string>>(() => {
    const m = new Map<number, string>();
    if (!currentSample.labels) return m;
    for (const [name, addr] of currentSample.labels) {
      if (!m.has(addr)) m.set(addr, name);
    }
    return m;
  });

  function sampleBytes(): Uint8Array {
    return new Uint8Array(currentSample.bytes);
  }

  const tweaks = createTweaksStore('ramses');
  const cpuStore = createCpuStore<RamsesCpu>({
    cpu: createRamsesCpu(),
    step: ramsesStep,
    reset: ramsesReset,
  });
  cpuStore.cpu.memory.data.set(sampleBytes());

  let running = $state(false);
  let serviceOpen = $state(false);
  let speed = $state(10);
  const breakpoints = $state(new Set<number>());
  let hoveredAddr: number | null = $state(null);
  let lastRead: number | null = $state(null);
  let lastWrite: number | null = $state(null);
  let memContainer: HTMLDivElement | null = $state(null);

  const runLoop = createRunLoop({
    step: () => stepOne(),
    shouldBreak: () => {
      if (cpuStore.cpu.registers.flags.halted) return true;
      if (breakpoints.has(cpuStore.cpu.registers.programCounter)) return true;
      return false;
    },
  });

  function stepOne(): void {
    const mem = cpuStore.cpu.memory;
    const before = { r: mem.readCount, w: mem.writeCount };
    cpuStore.step();
    const ir = cpuStore.cpu.registers.instructionRegister;
    const instruction = ir.opCode & RamsesInstructionMask;
    if (mem.writeCount > before.w) {
      lastWrite = ir.operandAddress;
      lastRead = null;
    } else if (mem.readCount > before.r + fetchReads(ir.opCode) && hasOperand(instruction)) {
      lastRead = ir.operandAddress;
      lastWrite = null;
    } else {
      lastRead = null;
      lastWrite = null;
    }
  }

  function hasOperand(instruction: number): boolean {
    return (
      instruction === RamsesInstruction.Str ||
      instruction === RamsesInstruction.Ldr ||
      instruction === RamsesInstruction.Add ||
      instruction === RamsesInstruction.Or ||
      instruction === RamsesInstruction.And ||
      instruction === RamsesInstruction.Sub ||
      instruction === RamsesInstruction.Jmp ||
      instruction === RamsesInstruction.Jn ||
      instruction === RamsesInstruction.Jz ||
      instruction === RamsesInstruction.Jc ||
      instruction === RamsesInstruction.Jsr
    );
  }

  function fetchReads(opCode: number): number {
    // Opcode byte is always fetched; instructions with an operand read 1 more;
    // indirect adds a third read for the pointer dereference.
    const instruction = opCode & RamsesInstructionMask;
    if (!hasOperand(instruction)) return 1;
    const mode = opCode & RamsesAddressModeMask;
    return mode === RamsesAddressMode.Indirect ? 3 : 2;
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
  }

  function doFullReset(): void {
    runLoop.stop();
    running = false;
    cpuStore.reset();
    lastRead = null;
    lastWrite = null;
  }

  function onSpeed(hz: number): void {
    speed = hz;
    if (running) {
      runLoop.stop();
      runLoop.start(hz);
    }
  }

  $effect(() => {
    void $cpuStore.tick;
    if (running && !runLoop.running) running = false;
  });

  function toggleBreakpoint(addr: number): void {
    if (breakpoints.has(addr)) breakpoints.delete(addr);
    else breakpoints.add(addr);
    // eslint-disable-next-line @typescript-eslint/no-unused-expressions
    breakpoints.size;
  }

  // --- Derived data ----------------------------------------------------------

  const cpu = $derived($cpuStore.cpu);
  const regs = $derived(cpu.registers);
  const ir = $derived(regs.instructionRegister);
  const irOp = $derived(ir.opCode);
  const irInstr = $derived(irOp & RamsesInstructionMask);
  const irReg = $derived(irOp & RamsesRegisterMask);
  const irMode = $derived(irOp & RamsesAddressModeMask);

  const instructionMnemonic: Record<number, string> = {
    [RamsesInstruction.Nop]: 'NOP',
    [RamsesInstruction.Str]: 'STR',
    [RamsesInstruction.Ldr]: 'LDR',
    [RamsesInstruction.Add]: 'ADD',
    [RamsesInstruction.Or]: 'OR',
    [RamsesInstruction.And]: 'AND',
    [RamsesInstruction.Not]: 'NOT',
    [RamsesInstruction.Sub]: 'SUB',
    [RamsesInstruction.Jmp]: 'JMP',
    [RamsesInstruction.Jn]: 'JN',
    [RamsesInstruction.Jz]: 'JZ',
    [RamsesInstruction.Jc]: 'JC',
    [RamsesInstruction.Jsr]: 'JSR',
    [RamsesInstruction.Neg]: 'NEG',
    [RamsesInstruction.Shr]: 'SHR',
    [RamsesInstruction.Hlt]: 'HLT',
  };
  const registerMnemonic: Record<number, string> = {
    [RamsesRegister.Ra]: 'RA',
    [RamsesRegister.Rb]: 'RB',
    [RamsesRegister.Rx]: 'RX',
    [RamsesRegister.Pc]: 'PC',
  };
  const modeMnemonic: Record<number, string> = {
    [RamsesAddressMode.Direct]: 'DIRECT',
    [RamsesAddressMode.Indirect]: 'INDIRECT',
    [RamsesAddressMode.Immediate]: 'IMMED.',
    [RamsesAddressMode.Indexed]: 'INDEXED',
  };

  const regUsed = (instr: number): boolean =>
    instr !== RamsesInstruction.Jmp &&
    instr !== RamsesInstruction.Jn &&
    instr !== RamsesInstruction.Jz &&
    instr !== RamsesInstruction.Jc &&
    instr !== RamsesInstruction.Jsr &&
    instr !== RamsesInstruction.Hlt &&
    instr !== RamsesInstruction.Nop;

  const irStart = $derived(
    (regs.programCounter - irSizeOf(irOp) + 256) & 0xff,
  );

  function irSizeOf(opCode: number): number {
    return fetchReads(opCode) >= 2 ? 2 : 1;
  }

  const irSize = $derived(irSizeOf(irOp));

  const groups = $derived<BitGroup[]>([
    {
      label: 'I I I I',
      bits: (irOp & 0xf0) >>> 4,
      width: 4,
      subLabel: instructionMnemonic[irInstr] ?? '???',
      color: 'I',
    },
    {
      label: 'R R',
      bits: (irOp & 0x0c) >>> 2,
      width: 2,
      subLabel: regUsed(irInstr) ? (registerMnemonic[irReg] ?? '—') : '—',
      color: 'R',
    },
    {
      label: 'M M',
      bits: irOp & 0x03,
      width: 2,
      subLabel: hasOperand(irInstr) ? (modeMnemonic[irMode] ?? '—') : '—',
      color: 'M',
    },
  ]);

  const operandByte = $derived(() => {
    // Raw byte at irStart+1, i.e. what the POC calls "operandByte".
    if (!hasOperand(irInstr) || irSize < 2) return 0;
    return cpu.memory.data[(irStart + 1) & 0xff] ?? 0;
  });

  const operands = $derived<OperandRow[]>(() => {
    const rows: OperandRow[] = [
      { key: 'OPCODE', value: fmtHex2(irOp), hint: fmtBin8(irOp) },
    ];
    if (hasOperand(irInstr)) {
      rows.push({
        key: 'OPERAND',
        value: fmtHex2(operandByte()),
        hint: fmtBin8(operandByte()),
      });
      rows.push({
        key: 'EFF. ADDR',
        value: fmtHex2(ir.operandAddress),
        hint: `→ MEM[${fmtHex2(ir.operandAddress)}]`,
      });
    }
    return rows;
  });

  const flags = $derived<FlagSpec[]>([
    { label: 'N', sub: 'neg', on: regs.flags.negative },
    { label: 'Z', sub: 'zero', on: regs.flags.zero },
    { label: 'C', sub: 'carry', on: regs.flags.carry },
    { label: 'HLT', sub: 'halt', on: regs.flags.halted, color: 'red' },
  ]);

  const disasm = $derived<DisasmItem[]>(() => {
    const items: DisasmItem[] = [];
    const bytes = cpu.memory.data;
    let addr = 0;
    while (addr < 256) {
      const slice: number[] = [];
      for (let i = addr; i < Math.min(addr + 2, 256); i++) slice.push(bytes[i] ?? 0);
      const { text, size } = ramsesDisassembleInstruction(slice);
      if (size === 0) break;
      items.push({
        addr,
        text,
        size,
        bytes: Array.from(bytes.slice(addr, addr + size)),
        label: addrToLabel.get(addr),
      });
      addr += size;
    }
    return items;
  });

  // --- Addressing-mode arrow -------------------------------------------------
  // When the user is NOT hovering, show the arrow for the live IR operand.
  // When hovering, show the arrow for the hovered byte's addressing behaviour.
  const arrowInfo = $derived(() => {
    if (!hasOperand(irInstr) || irSize < 2) return { from: null, to: null, label: '' };
    const operandAddr = (irStart + 1) & 0xff;
    return {
      from: operandAddr,
      to: ir.operandAddress,
      label: modeMnemonic[irMode] ?? '',
    };
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

  const activeReg = $derived<number>(regUsed(irInstr) ? irReg : -1);
</script>

<Chassis
  machine="ramses"
  title="RAMSES"
  sub="8-BIT · THREE REGS · 4 ADDRESSING MODES"
  running={running && !regs.flags.halted}
  halted={regs.flags.halted}
  serial="SN-0117 / AP-1985"
  {tweaks}
  bind:serviceOpen
>
  <div class="panel-grid">
    <section class="col col-left">
      <div class="tiles tiles-4">
        <RegisterTile
          name="RA"
          value={regs.ra}
          active={activeReg === RamsesRegister.Ra}
          accent={accentAll}
        />
        <RegisterTile
          name="RB"
          value={regs.rb}
          active={activeReg === RamsesRegister.Rb}
          accent={accentAll}
        />
        <RegisterTile
          name="RX"
          value={regs.rx}
          active={activeReg === RamsesRegister.Rx || irMode === RamsesAddressMode.Indexed}
          accent={accentAll}
          hint="index"
        />
        <RegisterTile
          name="PC"
          value={regs.programCounter}
          accent={accentAll}
          hint="program counter"
        />
      </div>

      <FlagBank {flags} accent={accentAll} title="FLAGS" />

      {#if $tweaks.showFetchCycle}
        <FetchCycle steps={$cpuStore.tick} hasSource={false} />
      {/if}

      <IRDecoder
        addr={irStart}
        {groups}
        operands={operands()}
        accent={accentAll}
      />

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
      <div class="mem-and-arrow" bind:this={memContainer}>
        <MemoryGrid
          bytes={cpu.memory.data}
          base={$tweaks.base}
          pc={regs.programCounter}
          irStart={irStart}
          irSize={irSize}
          lastRead={lastRead}
          lastWrite={lastWrite}
          effAddr={hasOperand(irInstr) ? ir.operandAddress : null}
          breakpoints={breakpoints}
          hoveredAddr={hoveredAddr}
          onToggleBreakpoint={toggleBreakpoint}
          onHover={(a) => (hoveredAddr = a)}
        />

        {#if $tweaks.showAnnotations}
          <AddressingModeArrow
            container={memContainer}
            from={arrowInfo().from}
            to={arrowInfo().to}
            label={arrowInfo().label}
          />
        {/if}
      </div>

      {#if $tweaks.showAnnotations && irInstr === RamsesInstruction.Jsr}
        <div class="jsr-note">
          <span class="jsr-k">JSR</span>
          Return byte is written <b>into</b> the called routine (no stack) — see
          the WRITE-highlighted cell.
        </div>
      {/if}
    </section>

    <section class="col col-right">
      <Disassembly
        items={disasm()}
        pc={regs.programCounter}
        irAddr={irStart}
        addrDigits={2}
        breakpoints={breakpoints}
        onToggleBreakpoint={toggleBreakpoint}
        title={`DISASSEMBLY · ${currentSample.name.toUpperCase()}`}
      />
      {#if currentSample.sourceText && currentSample.addrToLine}
        <SourceView
          source={currentSample.sourceText}
          addrToLine={currentSample.addrToLine as Map<number, number>}
          pc={regs.programCounter}
          irAddr={irStart}
          breakpoints={breakpoints}
          onToggleBreakpoint={toggleBreakpoint}
          title={`SOURCE · ${currentSample.name.toUpperCase()}`}
        />
      {/if}
    </section>
  </div>
</Chassis>

<ServiceDrawer
  bind:open={serviceOpen}
  {tweaks}
  {diagnostics}
  memKind="ramses"
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
    grid-template-columns: minmax(0, 22rem) minmax(0, 1fr) minmax(0, 20rem);
    gap: 14px;
    align-items: start;
  }
  :global(.chassis.frame-mobile) .panel-grid {
    grid-template-columns: 1fr;
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
  .tiles-4 {
    grid-template-columns: 1fr 1fr;
  }
  .mem-and-arrow {
    position: relative;
  }
  .jsr-note {
    font-size: 10px;
    letter-spacing: 0.08em;
    color: var(--silk-dim);
    padding: 8px 10px;
    background: rgba(255, 255, 255, 0.02);
    border-left: 2px solid var(--accent);
    border-radius: 3px;
  }
  .jsr-k {
    color: var(--accent);
    margin-right: 6px;
    font-weight: 700;
  }
  .jsr-note b {
    color: var(--silk);
  }
  :global(.chassis.ann-off) .jsr-note {
    display: none;
  }
</style>
