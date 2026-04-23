<script lang="ts">
  import {
    ahmesDisassembleInstructions,
    AhmesInstruction,
    ahmesReset,
    ahmesStep,
    createAhmesCpu,
    type AhmesCategory,
    type AhmesCpu,
  } from '../../core/ahmes';
  import { createCpuStore } from '../../stores/cpu';
  import { createTweaksStore } from '../../stores/tweaks';
  import Chassis from '../chassis/Chassis.svelte';
  import ServiceDrawer from '../chassis/ServiceDrawer.svelte';
  import Controls from './Controls.svelte';
  import Disassembly from './Disassembly.svelte';
  import FlagBank from './FlagBank.svelte';
  import IRDecoder from './IRDecoder.svelte';
  import MemoryGrid from './MemoryGrid.svelte';
  import RegisterTile from './RegisterTile.svelte';
  import ShiftRotateAnimation, { type ShiftOp } from './ShiftRotateAnimation.svelte';
  import { fmtBin8, fmtHex2 } from './format';
  import { createRunLoop } from './run-loop';
  import type { BitGroup, DisasmItem, FlagSpec, OperandRow } from './types';

  // Showcase the new Ahmes instructions: SUB, shifts/rotates, extended jumps.
  function sampleBytes(): Uint8Array {
    const b = new Uint8Array(256);
    // Compute 80 - 30 = 50, then SHR, then ROR-with-carry.
    b[0x00] = AhmesInstruction.Lda;
    b[0x01] = 0x80;
    b[0x02] = AhmesInstruction.Sub;
    b[0x03] = 0x81;
    b[0x04] = AhmesInstruction.Sta;
    b[0x05] = 0x82;
    b[0x06] = AhmesInstruction.Shr;
    b[0x07] = AhmesInstruction.Ror;
    b[0x08] = AhmesInstruction.Sta;
    b[0x09] = 0x83;
    b[0x0a] = AhmesInstruction.Hlt;
    b[0x80] = 0x50; // 80
    b[0x81] = 0x1e; // 30
    b[0x82] = 0x00;
    b[0x83] = 0x00;
    return b;
  }

  const tweaks = createTweaksStore('ahmes');
  const cpuStore = createCpuStore<AhmesCpu>({
    cpu: createAhmesCpu(),
    step: ahmesStep,
    reset: ahmesReset,
  });
  cpuStore.cpu.memory.data.set(sampleBytes());

  let running = $state(false);
  let serviceOpen = $state(false);
  let speed = $state(10);
  const breakpoints = $state(new Set<number>());
  let hoveredAddr: number | null = $state(null);
  let lastRead: number | null = $state(null);
  let lastWrite: number | null = $state(null);
  let lastShiftOp: ShiftOp | null = $state(null);

  const runLoop = createRunLoop({
    step: () => stepOne(),
    shouldBreak: () => {
      if (cpuStore.cpu.registers.flags.halted) return true;
      if (breakpoints.has(cpuStore.cpu.registers.programCounter)) return true;
      return false;
    },
  });

  function shiftOpFor(opCode: number): ShiftOp | null {
    switch (opCode) {
      case AhmesInstruction.Shr: return 'shr';
      case AhmesInstruction.Shl: return 'shl';
      case AhmesInstruction.Ror: return 'ror';
      case AhmesInstruction.Rol: return 'rol';
      default: return null;
    }
  }

  function fetchReads(opCode: number): number {
    // Neander-family pattern: opcode read + operand byte read for ops that take one.
    switch (opCode) {
      case AhmesInstruction.Sta:
      case AhmesInstruction.Lda:
      case AhmesInstruction.Add:
      case AhmesInstruction.Or:
      case AhmesInstruction.And:
      case AhmesInstruction.Sub:
      case AhmesInstruction.Jmp:
      case AhmesInstruction.Jn:
      case AhmesInstruction.Jp:
      case AhmesInstruction.Jv:
      case AhmesInstruction.Jnv:
      case AhmesInstruction.Jz:
      case AhmesInstruction.Jnz:
      case AhmesInstruction.Jc:
      case AhmesInstruction.Jnc:
      case AhmesInstruction.Jb:
      case AhmesInstruction.Jnb:
        return 2;
      default:
        return 1;
    }
  }

  function stepOne(): void {
    const mem = cpuStore.cpu.memory;
    const before = { r: mem.readCount, w: mem.writeCount };
    cpuStore.step();
    const ir = cpuStore.cpu.registers.instructionRegister;
    if (mem.writeCount > before.w) {
      lastWrite = ir.operandAddress;
      lastRead = null;
    } else if (mem.readCount > before.r + fetchReads(ir.opCode)) {
      lastRead = ir.operandAddress;
      lastWrite = null;
    } else {
      lastRead = null;
      lastWrite = null;
    }
    const so = shiftOpFor(ir.opCode);
    if (so) lastShiftOp = so;
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
    lastShiftOp = null;
  }

  function doFullReset(): void {
    runLoop.stop();
    running = false;
    cpuStore.reset();
    lastRead = null;
    lastWrite = null;
    lastShiftOp = null;
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
  const irHigh = $derived((irOp & 0xf0) >>> 4);
  const irLow = $derived(irOp & 0x0f);

  const isTwoByte = (op: number): boolean => fetchReads(op) === 2;

  const mnemonicFor = (op: number): string => {
    switch (op) {
      case AhmesInstruction.Nop: return 'NOP';
      case AhmesInstruction.Sta: return 'STA';
      case AhmesInstruction.Lda: return 'LDA';
      case AhmesInstruction.Add: return 'ADD';
      case AhmesInstruction.Or: return 'OR';
      case AhmesInstruction.And: return 'AND';
      case AhmesInstruction.Not: return 'NOT';
      case AhmesInstruction.Sub: return 'SUB';
      case AhmesInstruction.Jmp: return 'JMP';
      case AhmesInstruction.Jn: return 'JN';
      case AhmesInstruction.Jp: return 'JP';
      case AhmesInstruction.Jv: return 'JV';
      case AhmesInstruction.Jnv: return 'JNV';
      case AhmesInstruction.Jz: return 'JZ';
      case AhmesInstruction.Jnz: return 'JNZ';
      case AhmesInstruction.Jc: return 'JC';
      case AhmesInstruction.Jnc: return 'JNC';
      case AhmesInstruction.Jb: return 'JB';
      case AhmesInstruction.Jnb: return 'JNB';
      case AhmesInstruction.Shr: return 'SHR';
      case AhmesInstruction.Shl: return 'SHL';
      case AhmesInstruction.Ror: return 'ROR';
      case AhmesInstruction.Rol: return 'ROL';
      case AhmesInstruction.Hlt: return 'HLT';
      default: return '—';
    }
  };

  const irStart = $derived(
    (regs.programCounter - (isTwoByte(irOp) ? 2 : 1) + 256) & 0xff,
  );
  const irSize = $derived(isTwoByte(irOp) ? 2 : 1);

  // For jump-extensions + shifts, the low nibble carries the sub-opcode discriminator.
  const subLabelLow = $derived(() => {
    if (irOp >= 0x90 && irOp <= 0xbf) return 'sub-op';
    if (irOp >= 0xe0 && irOp <= 0xe3) return 'shift/rot';
    return '—';
  });

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
      subLabel: subLabelLow(),
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
    { label: 'V', sub: 'ovfl', on: regs.flags.overflow },
    { label: 'C', sub: 'carry', on: regs.flags.carry },
    { label: 'B', sub: 'borrow', on: regs.flags.borrow },
    { label: 'HLT', sub: 'halt', on: regs.flags.halted, color: 'red' },
  ]);

  // Categorize disassembly entries so the panel can color them per Ahmes manual §7.2.
  const categoryTag: Record<AhmesCategory, string> = {
    neander: 'N',
    arith: 'A',
    shift: 'S',
    'jump-ext': 'J',
    other: '—',
  };

  const disasm = $derived<DisasmItem[]>(() => {
    const items: DisasmItem[] = [];
    const bytes = cpu.memory.data;
    let addr = 0;
    while (addr < 256) {
      const slice: number[] = [];
      for (let i = addr; i < Math.min(addr + 2, 256); i++) slice.push(bytes[i] ?? 0);
      const [d] = ahmesDisassembleInstructions(slice);
      if (!d || d.size === 0) break;
      items.push({
        addr,
        text: d.text,
        size: d.size,
        bytes: Array.from(bytes.slice(addr, addr + d.size)),
        label: $tweaks.showAnnotations ? categoryTag[d.category] : undefined,
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
</script>

<Chassis
  machine="ahmes"
  title="AHMES"
  sub="8-BIT · AHMES EXTENSIONS · 24 OPCODES"
  running={running && !regs.flags.halted}
  halted={regs.flags.halted}
  serial="SN-0042 / AP-1973"
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

      <FlagBank {flags} accent={accentAll} title="FLAGS · N Z V C B" />

      <ShiftRotateAnimation
        ac={regs.accumulator}
        carry={regs.flags.carry}
        op={lastShiftOp}
      />

      <IRDecoder
        addr={irStart}
        {groups}
        operands={operands}
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
        title="DISASSEMBLY · N=classic A=arith S=shift J=jump-ext"
      />
    </section>
  </div>
</Chassis>

<ServiceDrawer
  bind:open={serviceOpen}
  {tweaks}
  {diagnostics}
  memKind="ahmes"
  {memData}
  {onMemLoad}
  onReset={doFullReset}
  onLoadSample={doReset}
  sampleLabel="RELOAD SUB+SHIFT DEMO"
/>

<style>
  .panel-grid {
    display: grid;
    grid-template-columns: minmax(0, 22rem) minmax(0, 1fr) minmax(0, 18rem);
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
</style>
