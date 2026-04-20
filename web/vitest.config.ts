import { defineConfig } from 'vitest/config';
import { fileURLToPath } from 'node:url';

export default defineConfig({
  test: {
    include: ['tests/**/*.test.ts'],
    environment: 'node',
    globals: false,
    passWithNoTests: true,
  },
  resolve: {
    alias: {
      '@/core': fileURLToPath(new URL('./src/core', import.meta.url)),
      '@/ui': fileURLToPath(new URL('./src/ui', import.meta.url)),
      '@/assemblers': fileURLToPath(new URL('./src/assemblers', import.meta.url)),
      '@/stores': fileURLToPath(new URL('./src/stores', import.meta.url)),
      '@/samples': fileURLToPath(new URL('./src/samples', import.meta.url)),
    },
  },
});
