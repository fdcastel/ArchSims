import { expect, test } from '@playwright/test';

// P7-03 — regression guard for BUG-1. Every page at phone / tablet / desktop
// widths must render without creating a horizontal scrollbar, and without
// logging console errors or page errors.

const pages = ['/', '/neander', '/ahmes', '/ramses', '/cesar'] as const;
const viewports = [
  { name: 'phone', width: 375, height: 812 },
  { name: 'tablet', width: 768, height: 1024 },
  { name: 'desktop', width: 1440, height: 900 },
] as const;

for (const vp of viewports) {
  test.describe(`viewport ${vp.name} (${vp.width}×${vp.height})`, () => {
    test.use({ viewport: { width: vp.width, height: vp.height } });

    for (const url of pages) {
      test(`${url} — no horizontal overflow, no errors`, async ({ page }) => {
        const consoleErrors: string[] = [];
        const pageErrors: string[] = [];
        page.on('console', (m) => m.type() === 'error' && consoleErrors.push(m.text()));
        page.on('pageerror', (e) => pageErrors.push(`${e.name}: ${e.message}`));

        await page.goto(url, { waitUntil: 'networkidle' });

        const overflow = await page.evaluate(
          () => document.documentElement.scrollWidth > window.innerWidth + 1,
        );
        expect(overflow, 'page should not require horizontal scroll').toBe(false);
        expect(consoleErrors).toEqual([]);
        expect(pageErrors).toEqual([]);
      });
    }
  });
}
