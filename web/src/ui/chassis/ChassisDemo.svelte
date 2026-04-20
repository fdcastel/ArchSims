<script lang="ts">
  import {
    createNeanderCpu,
    neanderReset,
    neanderStep,
    type NeanderCpu,
  } from '../../core/neander';
  import { createCpuStore } from '../../stores/cpu';
  import { createTweaksStore } from '../../stores/tweaks';
  import Chassis from './Chassis.svelte';
  import ServiceDrawer from './ServiceDrawer.svelte';

  const tweaks = createTweaksStore('neander');
  const cpuStore = createCpuStore<NeanderCpu>({
    cpu: createNeanderCpu(),
    step: neanderStep,
    reset: neanderReset,
  });

  let serviceOpen = $state(false);

  const diagnostics = $derived({
    steps: $cpuStore.tick,
    reads: 0,
    writes: 0,
    halted: $cpuStore.cpu.registers.flags.halted,
  });

  function memData(): Uint8Array {
    return new Uint8Array($cpuStore.cpu.memory.data);
  }

  function onMemLoad(bytes: Uint8Array): void {
    $cpuStore.cpu.memory.data.set(bytes);
    cpuStore.reset();
  }
</script>

<Chassis
  machine="neander"
  title="NEANDER"
  sub="8-bit accumulator"
  running={false}
  halted={$cpuStore.cpu.registers.flags.halted}
  serial="SN-0001"
  {tweaks}
  bind:serviceOpen
>
  <div class="demo-body">
    <p>tick: {$cpuStore.tick}</p>
    <button type="button" onclick={() => cpuStore.step()}>STEP</button>
    <button type="button" onclick={() => cpuStore.reset()}>RESET</button>
  </div>
</Chassis>

<ServiceDrawer
  bind:open={serviceOpen}
  {tweaks}
  {diagnostics}
  memKind="neander"
  {memData}
  {onMemLoad}
  onReset={() => cpuStore.reset()}
/>

<style>
  .demo-body {
    padding: 1rem;
    color: var(--silk);
  }
</style>
