<script lang="ts">
  import {
    Controls,
    Disassembly,
    FlagBank,
    IRDecoder,
    MemoryGrid,
    RegisterTile,
  } from './index';
  import type {
    BitGroup,
    DisasmItem,
    FlagSpec,
    OperandRow,
  } from './index';

  const bytes = new Uint8Array(256);
  bytes[0] = 0x20;
  bytes[1] = 0x80;
  bytes[2] = 0x30;
  bytes[3] = 0x81;
  bytes[4] = 0xf0;
  bytes[0x80] = 0x01;
  bytes[0x81] = 0x02;

  const flags: FlagSpec[] = [
    { label: 'N', sub: 'neg', on: false },
    { label: 'Z', sub: 'zero', on: true },
    { label: 'HLT', sub: 'halt', on: false, color: 'red' },
  ];

  const op = 0x30;
  const groups: BitGroup[] = [
    { label: 'I I I I', bits: (op & 0xf0) >> 4, width: 4, subLabel: 'ADD', color: 'I' },
    { label: '- - - -', bits: op & 0x0f, width: 4, subLabel: '—', color: 'M' },
  ];
  const operands: OperandRow[] = [
    { key: 'OPCODE', value: '30', hint: '00110000' },
    { key: 'OPERAND', value: '81', hint: '10000001' },
    { key: 'EFF. ADDR', value: '81', hint: '→ MEM[81]' },
  ];

  const disasmItems: DisasmItem[] = [
    { addr: 0x00, text: 'LDA 80', size: 2, bytes: [0x20, 0x80] },
    { addr: 0x02, text: 'ADD 81', size: 2, bytes: [0x30, 0x81] },
    { addr: 0x04, text: 'HLT', size: 1, bytes: [0xf0], label: 'end' },
  ];

  let speed = $state(10);
  let running = $state(false);
</script>

<div class="demo">
  <div class="row">
    <RegisterTile name="AC" value={0x03} hint="8-bit" active={true} />
    <RegisterTile name="PC" value={0x04} width={8} />
    <RegisterTile name="R7" value={0xabcd} width={16} hint="16-bit" />
  </div>

  <FlagBank {flags} />
  <IRDecoder addr={0x02} {groups} {operands} />
  <MemoryGrid {bytes} base="hex" pc={4} irStart={2} irSize={2} lastRead={0x81} effAddr={0x81} />
  <Disassembly items={disasmItems} pc={0x04} irAddr={0x02} />
  <Controls
    halted={false}
    {running}
    {speed}
    onStep={() => {}}
    onRun={() => (running = true)}
    onBreak={() => (running = false)}
    onReset={() => {}}
    onSpeed={(hz) => (speed = hz)}
    counters={[
      { label: 'STEPS', value: 42, digits: 5 },
      { label: 'READS', value: 128, digits: 5, color: 'dim' },
      { label: 'WRITES', value: 7, digits: 5, color: 'dim' },
    ]}
  />
</div>

<style>
  .demo {
    display: flex;
    flex-direction: column;
    gap: 14px;
  }
  .row {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 10px;
  }
</style>
