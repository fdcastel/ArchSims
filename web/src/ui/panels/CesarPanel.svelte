<script lang="ts">
  import {
    CesarAddressMode,
    CesarAddressModeMask,
    CesarDisplayMemoryAddress,
    CesarInstruction,
    CesarInstructionMask,
    CesarKeyboardMemoryAddress,
    CesarRegister,
    CesarRegisterMask,
    cesarDisassembleInstruction,
    cesarReset,
    cesarStep,
    createCesarCpu,
    type CesarCpu,
  } from '../../core/cesar';
  import { memoryWriteByte } from '../../core/memory';
  import { SAMPLES_BY_MACHINE } from '../../samples';
  import type { Sample } from '../../samples/types';
  import { createCpuStore } from '../../stores/cpu';
  import { createTweaksStore } from '../../stores/tweaks';
  import Chassis from '../chassis/Chassis.svelte';
  import ServiceDrawer from '../chassis/ServiceDrawer.svelte';
  import AddressingModeArrow from './AddressingModeArrow.svelte';
  import Controls from './Controls.svelte';
  import Disassembly from './Disassembly.svelte';
  import DisplayPanel from './DisplayPanel.svelte';
  import FetchCycle from './FetchCycle.svelte';
  import FlagBank from './FlagBank.svelte';
  import IRDecoder from './IRDecoder.svelte';
  import KeyboardInput from './KeyboardInput.svelte';
  import MemoryGrid from './MemoryGrid.svelte';
  import RegisterTile from './RegisterTile.svelte';
  import SourceView from './SourceView.svelte';
  import StackPanel from './StackPanel.svelte';
  import { fmtBin8, fmtHex2, fmtHex4 } from './format';
  import { createRunLoop } from './run-loop';
  import type { BitGroup, DisasmItem, FlagSpec, OperandRow } from './types';

  const SAMPLES = SAMPLES_BY_MACHINE.cesar;
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

  const tweaks = createTweaksStore('cesar');
  const cpuStore = createCpuStore<CesarCpu>({
    cpu: createCesarCpu(),
    step: cesarStep,
    reset: cesarReset,
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
  let manualPage: number | null = $state(null);
  let r6EverWritten = $state(false);

  const runLoop = createRunLoop({
    step: () => stepOne(),
    shouldBreak: () => {
      if (cpuStore.cpu.registers.flags.halted) return true;
      if (breakpoints.has(cpuStore.cpu.registers.r[7] ?? 0)) return true;
      return false;
    },
  });

  function stepOne(): void {
    const mem = cpuStore.cpu.memory;
    const before = { r: mem.readCount, w: mem.writeCount };
    const r6Before = cpuStore.cpu.registers.r[6] ?? 0;
    cpuStore.step();
    const ir = cpuStore.cpu.registers.instructionRegister;

    if (mem.writeCount > before.w) {
      // Best guess for last-write address: the target operand's memory addr, if any.
      const op = ir.targetOperand;
      lastWrite = op.kind === 'addr' ? op.address : null;
      lastRead = null;
    } else {
      lastWrite = null;
      // Best guess for a non-fetch read: source operand's address, if any.
      const so = ir.sourceOperand;
      lastRead = so.kind === 'addr' ? so.address : null;
    }

    const r6After = cpuStore.cpu.registers.r[6] ?? 0;
    if (r6After !== r6Before || r6After !== 0) r6EverWritten = true;
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
    r6EverWritten = false;
  }

  function doFullReset(): void {
    runLoop.stop();
    running = false;
    cpuStore.reset();
    lastRead = null;
    lastWrite = null;
    r6EverWritten = false;
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

  function onKeyboardSubmit(byte: number): void {
    memoryWriteByte(cpuStore.cpu.memory, CesarKeyboardMemoryAddress, byte);
    // Nudge the store so subscribers see the memory update.
    // (cpuStore.subscribe emits after step/reset; for manual writes we re-emit
    //  by doing a zero-step tick via any mutator.)
    // Easiest: use cpuStore's internal state via reset-less workaround — but
    // the cleanest path is to call step then undo. Instead, we leverage the
    // fact that reassigning lastWrite triggers reactivity since it's $state.
    lastWrite = CesarKeyboardMemoryAddress;
  }

  // --- Derived data ----------------------------------------------------------

  const cpu = $derived($cpuStore.cpu);
  // Re-alloc registers + flags + r-array on every tick so Svelte $derived
  // propagates. See P8-02 note in NeanderPanel.svelte.
  const regs = $derived.by(() => {
    void $cpuStore.tick;
    const rs = cpu.registers;
    return {
      ...rs,
      r: new Uint16Array(rs.r),
      flags: { ...rs.flags },
      instructionRegister: { ...rs.instructionRegister, data: [...rs.instructionRegister.data] },
    };
  });
  const r = $derived(regs.r);
  const ir = $derived(regs.instructionRegister);
  const firstOp = $derived(ir.data[0] ?? 0);
  const instruction = $derived(firstOp & CesarInstructionMask);
  const irSize = $derived(ir.data.length);
  const pc = $derived(r[7] ?? 0);
  const irStart = $derived((pc - irSize + 65536) & 0xffff);

  // Memory pager: follow PC unless the user manually chose a page.
  const autoPage = $derived((pc >>> 8) & 0xff);
  const currentPage = $derived(manualPage ?? autoPage);
  const pageStart = $derived(currentPage * 256);

  // IR decoder groups: high nibble = instruction, low nibble = sub-op / reg / mode.
  const mnemonicOf = (op: number): string => {
    if (instruction === CesarInstruction.Br) {
      // 0x3X family
      const lookup: Record<number, string> = {
        [CesarInstruction.Br]: 'BR',
        [CesarInstruction.Bne]: 'BNE',
        [CesarInstruction.Beq]: 'BEQ',
        [CesarInstruction.Bpl]: 'BPL',
        [CesarInstruction.Bmi]: 'BMI',
        [CesarInstruction.Bvc]: 'BVC',
        [CesarInstruction.Bvs]: 'BVS',
        [CesarInstruction.Bcc]: 'BCC',
        [CesarInstruction.Bcs]: 'BCS',
        [CesarInstruction.Bge]: 'BGE',
        [CesarInstruction.Blt]: 'BLT',
        [CesarInstruction.Bgt]: 'BGT',
        [CesarInstruction.Ble]: 'BLE',
        [CesarInstruction.Bhi]: 'BHI',
        [CesarInstruction.Bls]: 'BLS',
      };
      return lookup[op] ?? 'BR?';
    }
    if (instruction === CesarInstruction.Clr) {
      const lookup: Record<number, string> = {
        [CesarInstruction.Clr]: 'CLR',
        [CesarInstruction.Not]: 'NOT',
        [CesarInstruction.Inc]: 'INC',
        [CesarInstruction.Dec]: 'DEC',
        [CesarInstruction.Neg]: 'NEG',
        [CesarInstruction.Tst]: 'TST',
        [CesarInstruction.Ror]: 'ROR',
        [CesarInstruction.Rol]: 'ROL',
        [CesarInstruction.Asr]: 'ASR',
        [CesarInstruction.Asl]: 'ASL',
        [CesarInstruction.Adc]: 'ADC',
        [CesarInstruction.Sbc]: 'SBC',
      };
      return lookup[op] ?? 'CLR?';
    }
    const lookup: Record<number, string> = {
      [CesarInstruction.Nop]: 'NOP',
      [CesarInstruction.Ccc]: 'CCC',
      [CesarInstruction.Scc]: 'SCC',
      [CesarInstruction.Jmp]: 'JMP',
      [CesarInstruction.Sob]: 'SOB',
      [CesarInstruction.Jsr]: 'JSR',
      [CesarInstruction.Rts]: 'RTS',
      [CesarInstruction.Mov]: 'MOV',
      [CesarInstruction.Add]: 'ADD',
      [CesarInstruction.Sub]: 'SUB',
      [CesarInstruction.Cmp]: 'CMP',
      [CesarInstruction.And]: 'AND',
      [CesarInstruction.Or]: 'OR',
      [CesarInstruction.Hlt]: 'HLT',
    };
    return lookup[instruction] ?? '???';
  };

  const secondOp = $derived(ir.data[1] ?? 0);
  const sourceMode = $derived((firstOp << 2) & CesarAddressModeMask);
  const sourceReg = $derived((((firstOp & 0x01) << 2) | (secondOp >>> 6)) & CesarRegisterMask);
  const targetMode = $derived(secondOp & CesarAddressModeMask);
  const targetReg = $derived(secondOp & CesarRegisterMask);

  const modeName = (mode: number): string => {
    switch (mode) {
      case CesarAddressMode.Register: return 'REG';
      case CesarAddressMode.RegPostInc: return '(R)+';
      case CesarAddressMode.RegPreDec: return '-(R)';
      case CesarAddressMode.Indexed: return 'N(R)';
      case CesarAddressMode.RegisterIndirect: return '(R)';
      case CesarAddressMode.RegPostIncIndirect: return '((R)+)';
      case CesarAddressMode.RegPreDecIndirect: return '(-(R))';
      case CesarAddressMode.IndexedIndirect: return '(N(R))';
      default: return '?';
    }
  };

  const isTwoOperand = $derived(
    instruction === CesarInstruction.Mov ||
      instruction === CesarInstruction.Add ||
      instruction === CesarInstruction.Sub ||
      instruction === CesarInstruction.Cmp ||
      instruction === CesarInstruction.And ||
      instruction === CesarInstruction.Or,
  );

  const groups = $derived<BitGroup[]>(() => {
    const groupsArr: BitGroup[] = [
      {
        label: 'I I I I',
        bits: (firstOp & 0xf0) >>> 4,
        width: 4,
        subLabel: mnemonicOf(firstOp),
        color: 'I',
      },
    ];
    if (isTwoOperand) {
      groupsArr.push({
        label: 'S S S',
        bits: ((firstOp & 0x0f) << 2 | (secondOp >>> 6)) & 0x3f,
        width: 6,
        subLabel: `${modeName(sourceMode)} R${sourceReg}`,
        color: 'S',
      });
      groupsArr.push({
        label: 'T T T',
        bits: secondOp & 0x3f,
        width: 6,
        subLabel: `${modeName(targetMode)} R${targetReg}`,
        color: 'T',
      });
    } else if (irSize >= 2) {
      groupsArr.push({
        label: 'sub',
        bits: firstOp & 0x0f,
        width: 4,
        subLabel: 'sub-op',
        color: 'M',
      });
      groupsArr.push({
        label: 'T T T',
        bits: secondOp & 0x3f,
        width: 6,
        subLabel: `${modeName(targetMode)} R${targetReg}`,
        color: 'T',
      });
    } else {
      groupsArr.push({
        label: '- - - -',
        bits: firstOp & 0x0f,
        width: 4,
        subLabel: '—',
        color: 'M',
      });
    }
    return groupsArr;
  });

  const operands = $derived<OperandRow[]>(() => {
    const rows: OperandRow[] = [
      { key: 'OPCODE', value: fmtHex2(firstOp), hint: fmtBin8(firstOp) },
    ];
    if (irSize >= 2) {
      rows.push({ key: 'BYTE-2', value: fmtHex2(secondOp), hint: fmtBin8(secondOp) });
    }
    const src = ir.sourceOperand;
    if (src.kind === 'addr') {
      rows.push({ key: 'SRC EFF', value: fmtHex4(src.address), hint: `→ MEM[${fmtHex4(src.address)}]` });
    } else if (src.kind === 'reg') {
      rows.push({ key: 'SRC', value: `R${src.register}`, hint: `= ${fmtHex4(r[src.register] ?? 0)}` });
    }
    const tgt = ir.targetOperand;
    if (tgt.kind === 'addr') {
      rows.push({ key: 'TGT EFF', value: fmtHex4(tgt.address), hint: `→ MEM[${fmtHex4(tgt.address)}]` });
    } else if (tgt.kind === 'reg') {
      rows.push({ key: 'TGT', value: `R${tgt.register}`, hint: `= ${fmtHex4(r[tgt.register] ?? 0)}` });
    }
    return rows;
  });

  const flags = $derived<FlagSpec[]>([
    { label: 'N', sub: 'neg', on: regs.flags.negative },
    { label: 'Z', sub: 'zero', on: regs.flags.zero },
    { label: 'V', sub: 'ovfl', on: regs.flags.overflow },
    { label: 'C', sub: 'carry', on: regs.flags.carry },
    { label: 'HLT', sub: 'halt', on: regs.flags.halted, color: 'red' },
  ]);

  // Disassembly: walk bytes starting at page boundary to keep list size bounded.
  const disasm = $derived<DisasmItem[]>(() => {
    const items: DisasmItem[] = [];
    const bytes = cpu.memory.data;
    // Walk a window of 96 bytes starting at (irStart & ~0x0F) so the caret is visible.
    const windowStart = Math.max(0, (irStart - 16) & 0xfff0);
    let addr = windowStart;
    let safety = 0;
    while (addr < windowStart + 96 && addr < 65536 && safety < 96) {
      const slice: number[] = [];
      for (let i = addr; i < Math.min(addr + 6, 65536); i++) slice.push(bytes[i] ?? 0);
      const { text, size } = cesarDisassembleInstruction(slice);
      if (size === 0) break;
      items.push({
        addr,
        text,
        size,
        bytes: Array.from(bytes.slice(addr, addr + size)),
        label: addrToLabel.get(addr),
      });
      addr += size;
      safety += 1;
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

  // Active register for highlighting: SRC and TGT operand registers + any implicit R.
  const activeRegs = $derived(() => {
    const s = new Set<number>();
    const src = ir.sourceOperand;
    const tgt = ir.targetOperand;
    if (src.kind === 'reg') s.add(src.register);
    if (tgt.kind === 'reg') s.add(tgt.register);
    if (instruction === CesarInstruction.Jsr || instruction === CesarInstruction.Rts) {
      s.add(firstOp & CesarRegisterMask);
      s.add(6); // SP
    }
    if (instruction === CesarInstruction.Sob) {
      s.add(firstOp & CesarRegisterMask);
    }
    return s;
  });

  const arrowInfo = $derived(() => {
    const tgt = ir.targetOperand;
    if (tgt.kind !== 'addr' || irSize < 2) return { from: null as number | null, to: null as number | null, label: '' };
    const operandByteAddr = (irStart + 1) & 0xffff;
    return { from: operandByteAddr, to: tgt.address, label: modeName(targetMode) };
  });

  // SOB iteration counter: visible only while IR holds a SOB.
  const sobRemaining = $derived(() =>
    instruction === CesarInstruction.Sob ? (r[firstOp & CesarRegisterMask] ?? 0) : null,
  );

  const pageOptions = [0x00, 0x01, 0x02, 0x10, 0x20, 0x40, 0x80, 0xfe, 0xff];
</script>

<Chassis
  machine="cesar"
  title="CESAR"
  sub="16-BIT · 8 REGS · 8 ADDRESSING MODES · 64 KIB"
  running={running && !regs.flags.halted}
  halted={regs.flags.halted}
  serial="SN-0512 / AP-1991"
  {tweaks}
  bind:serviceOpen
>
  <div class="panel-grid">
    <section class="col col-left">
      <div class="tiles tiles-cesar">
        {#each [0, 1, 2, 3, 4, 5] as idx (idx)}
          <RegisterTile
            name={`R${idx}`}
            value={r[idx] ?? 0}
            width={16}
            active={activeRegs().has(idx)}
            accent={accentAll}
          />
        {/each}
        <RegisterTile
          name="R6"
          value={r[6] ?? 0}
          width={16}
          active={activeRegs().has(6)}
          accent={accentAll}
          hint="SP"
        />
        <RegisterTile
          name="R7"
          value={r[7] ?? 0}
          width={16}
          active={true}
          accent={accentAll}
          hint="PC"
        />
      </div>

      <FlagBank {flags} accent={accentAll} title="FLAGS · N Z V C" />

      {#if $tweaks.showFetchCycle}
        <FetchCycle steps={$cpuStore.tick} hasSource={isTwoOperand} />
      {/if}

      {#if sobRemaining() !== null && $tweaks.showAnnotations}
        <div class="sob-note">
          <span class="sob-k">SOB</span> R{firstOp & CesarRegisterMask} will decrement to
          <b>{((sobRemaining() ?? 0) - 1) & 0xffff}</b> then branch if non-zero.
        </div>
      {/if}

      <IRDecoder
        addr={irStart}
        addrDigits={4}
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
      <div class="mem-head-extra">
        <span class="mem-page-k">PAGE</span>
        <div class="mem-page-row">
          {#each pageOptions as p (p)}
            <button
              type="button"
              class="mem-page-btn"
              class:mem-page-on={currentPage === p && manualPage !== null}
              onclick={() => (manualPage = p)}
            >
              {p.toString(16).toUpperCase().padStart(2, '0')}xx
            </button>
          {/each}
          <button
            type="button"
            class="mem-page-btn"
            class:mem-page-on={manualPage === null}
            onclick={() => (manualPage = null)}
            title="Follow R7 (PC)"
          >
            ↦PC
          </button>
        </div>
      </div>
      <div class="mem-and-arrow" bind:this={memContainer}>
        <MemoryGrid
          bytes={cpu.memory.data}
          base={$tweaks.base}
          pc={pc}
          irStart={irStart >= pageStart && irStart < pageStart + 256 ? irStart : -1}
          irSize={irSize}
          lastRead={lastRead}
          lastWrite={lastWrite}
          effAddr={ir.targetOperand.kind === 'addr' ? ir.targetOperand.address : null}
          breakpoints={breakpoints}
          hoveredAddr={hoveredAddr}
          onToggleBreakpoint={toggleBreakpoint}
          onHover={(a) => (hoveredAddr = a)}
          pageStart={pageStart}
          pageSize={256}
          addrDigits={4}
          mmioRange={{ lo: CesarKeyboardMemoryAddress, hi: 0xffff }}
          title={`MEMORY · ${currentPage.toString(16).toUpperCase().padStart(2, '0')}xx`}
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

      <DisplayPanel
        bytes={cpu.memory.data}
        lastWrite={lastWrite}
      />

      <KeyboardInput onSubmit={onKeyboardSubmit} />
    </section>

    <section class="col col-right">
      <Disassembly
        items={disasm()}
        pc={pc}
        irAddr={irStart}
        addrDigits={4}
        breakpoints={breakpoints}
        onToggleBreakpoint={toggleBreakpoint}
        title={`DISASSEMBLY · ${currentSample.name.toUpperCase()}`}
      />

      <StackPanel
        bytes={cpu.memory.data}
        sp={r[6] ?? 0}
        depth={8}
        showEmpty={r6EverWritten}
        accent={accentAll}
      />

      {#if currentSample.sourceText && currentSample.addrToLine}
        <SourceView
          source={currentSample.sourceText}
          addrToLine={currentSample.addrToLine as Map<number, number>}
          pc={pc}
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
  memKind="cesar"
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
    grid-template-columns: minmax(0, 24rem) minmax(0, 1fr) minmax(0, 22rem);
    gap: 14px;
    align-items: start;
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
  .tiles-cesar {
    grid-template-columns: 1fr 1fr 1fr 1fr;
    gap: 6px;
  }
  @media (max-width: 1024px) {
    .panel-grid {
      grid-template-columns: 1fr;
    }
  }
  @media (max-width: 640px) {
    .tiles-cesar {
      grid-template-columns: 1fr 1fr;
    }
  }
  .mem-and-arrow {
    position: relative;
  }
  .mem-head-extra {
    display: flex;
    align-items: center;
    gap: 10px;
    font-size: 9px;
    letter-spacing: 0.2em;
    color: var(--silk-dim);
  }
  .mem-page-k {
    color: var(--silk-etch);
    font-weight: 700;
  }
  .mem-page-row {
    display: flex;
    gap: 4px;
    flex-wrap: wrap;
  }
  .mem-page-btn {
    background: rgba(0, 0, 0, 0.3);
    border: 1px solid var(--chassis-edge);
    color: var(--silk-dim);
    font-family: inherit;
    font-size: 9px;
    letter-spacing: 0.12em;
    padding: 3px 6px;
    border-radius: 3px;
    cursor: pointer;
  }
  .mem-page-btn:hover {
    color: var(--silk);
  }
  .mem-page-btn.mem-page-on {
    background: var(--accent);
    color: #0a0806;
    border-color: var(--accent);
  }
  .sob-note {
    font-size: 10px;
    letter-spacing: 0.08em;
    color: var(--silk-dim);
    padding: 8px 10px;
    background: rgba(255, 255, 255, 0.02);
    border-left: 2px solid var(--accent);
    border-radius: 3px;
  }
  .sob-k {
    color: var(--accent);
    margin-right: 6px;
    font-weight: 700;
  }
  .sob-note b {
    color: var(--silk);
  }
  :global(.chassis.ann-off) .sob-note {
    display: none;
  }
</style>
