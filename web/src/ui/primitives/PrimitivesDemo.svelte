<script lang="ts">
  import Etch from './Etch.svelte';
  import Lamp from './Lamp.svelte';
  import Segmented from './Segmented.svelte';
  import Toggle from './Toggle.svelte';
  import PanelButton from './PanelButton.svelte';

  let toggled = $state(false);
</script>

<div class="demo">
  <Etch>PRIMITIVES SMOKE</Etch>

  <section class="row">
    <Lamp on={true} color="red" label="PWR" />
    <Lamp on={true} color="amber" label="RUN" />
    <Lamp on={false} color="amber" label="HLT" />
    <Lamp on={true} color="green" label="OK" sub="ready" />
  </section>

  <section class="row">
    <Segmented text="PC  00" size="md" />
    <Segmented text="AC  00" size="md" color="dim" />
    <Segmented text="FF" size="xl" />
  </section>

  <section class="row">
    <PanelButton label="STEP" sub="advance" variant="default" />
    <PanelButton label="RUN" sub="go" variant="green" />
    <PanelButton label="HALT" sub="stop" variant="red" />
    <PanelButton label="RESET" sub="clear" variant="amber" />
  </section>

  <section class="row">
    <Toggle on={toggled} label="ANNOTATIONS" onChange={(v) => (toggled = v)} />
  </section>
</div>

<style>
  .demo {
    padding: 1.25rem;
    border: 1px solid var(--chassis-edge);
    background: var(--chassis-2);
    border-radius: 8px;
  }
  .row {
    display: flex;
    gap: 1rem;
    align-items: center;
    flex-wrap: wrap;
    padding: 0.5rem 0;
  }
</style>
