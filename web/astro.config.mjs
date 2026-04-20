import { defineConfig } from 'astro/config';
import svelte from '@astrojs/svelte';

// Static output for Cloudflare Pages.
// The @astrojs/cloudflare adapter is reserved for future SSR work; static builds
// drop straight into Pages without an adapter (see Phase 6).
export default defineConfig({
  output: 'static',
  integrations: [svelte()],
  vite: {
    resolve: {
      alias: {
        '@/core': '/src/core',
        '@/ui': '/src/ui',
        '@/assemblers': '/src/assemblers',
        '@/stores': '/src/stores',
        '@/samples': '/src/samples',
      },
    },
  },
});
