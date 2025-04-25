/// <reference types="vitest/config" />
import { defineConfig } from 'vite';
import { resolve } from 'path';

export default defineConfig({
  build: {
    lib: {
      // Could also be a dictionary or array of multiple entry points
      entry: resolve(__dirname, 'src/index.ts'),
      name: 'ArchSimsCore',
      // the proper extensions will be added
      fileName: 'archsims-core',
    },
    // Optional: Configure rollup options for more control
    // rollupOptions: { ... }
  },
  test: {
    // Vitest configuration options
    globals: true, // Use global APIs (describe, it, expect)
    environment: 'node', // Tests don't need browser APIs
    include: ['test/**/*.test.ts'], // Look for tests in the test folder
  },
});
