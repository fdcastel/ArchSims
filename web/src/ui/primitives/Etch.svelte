<script lang="ts">
  interface Props {
    inline?: boolean;
    children?: import('svelte').Snippet;
  }
  let { inline = false, children }: Props = $props();
</script>

<div class="etch" class:etch-inline={inline}>
  {@render children?.()}
</div>
