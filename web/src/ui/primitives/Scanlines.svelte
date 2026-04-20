<script lang="ts">
  type Palette = 'amber' | 'green' | 'paper';
  interface Props {
    palette?: Palette;
  }
  let { palette = 'amber' }: Props = $props();
</script>

<div
  class="scanlines"
  class:scanlines-amber={palette === 'amber'}
  class:scanlines-green={palette === 'green'}
  class:scanlines-paper={palette === 'paper'}
  aria-hidden="true"
></div>
