import { defineConfig } from 'vitest/config';
import { fileURLToPath } from 'node:url';
import { svelte } from '@sveltejs/vite-plugin-svelte';

const sharedAlias = {
  '@/core': fileURLToPath(new URL('./src/core', import.meta.url)),
  '@/ui': fileURLToPath(new URL('./src/ui', import.meta.url)),
  '@/assemblers': fileURLToPath(new URL('./src/assemblers', import.meta.url)),
  '@/stores': fileURLToPath(new URL('./src/stores', import.meta.url)),
  '@/samples': fileURLToPath(new URL('./src/samples', import.meta.url)),
};

export default defineConfig({
  resolve: { alias: sharedAlias },
  test: {
    passWithNoTests: true,
    projects: [
      // Core/CPU/store tests — plain Node, no DOM.
      {
        resolve: { alias: sharedAlias },
        test: {
          name: 'core',
          include: ['tests/**/*.test.ts'],
          environment: 'node',
          globals: false,
        },
      },
      // Primitive component tests — JSDOM + Svelte 5 runes via
      // @testing-library/svelte.
      {
        plugins: [svelte({ hot: false })],
        resolve: { alias: sharedAlias, conditions: ['browser'] },
        test: {
          name: 'components',
          include: ['tests-unit/**/*.test.ts'],
          environment: 'jsdom',
          setupFiles: ['./tests-unit/setup.ts'],
          globals: false,
        },
      },
    ],
  },
});
