import { expect, test, type Page } from '@playwright/test';

// P7-02 beachhead — one smoke spec per machine. Exercises the core front-panel
// interactions (STEP, RESET, RUN → HALT) so regressions in the Svelte chassis
// are caught before they ship. Real browser, real CPU, no mocks.

const machines = ['neander', 'ahmes', 'ramses', 'cesar'] as const;

async function openServicePanel(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
}

async function closeServicePanel(page: Page): Promise<void> {
  await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
}

for (const m of machines) {
  test.describe(`${m} front panel`, () => {
    test('STEP changes state; RESET restores initial', async ({ page }) => {
      await page.goto(`/${m}`);
      const before = await page.locator('body').innerText();

      const step = page.getByRole('button', { name: /STEP/ }).first();
      await step.click();
      await step.click();
      await step.click();
      const stepped = await page.locator('body').innerText();
      expect(stepped, 'body should change after STEP').not.toBe(before);

      await page.getByRole('button', { name: /RESET/ }).first().click();
      const reset = await page.locator('body').innerText();
      expect(reset, 'RESET should restore initial state').toBe(before);
    });

    test('loading the default sample and running MAX reaches HALT', async ({ page }) => {
      await page.goto(`/${m}`);
      await openServicePanel(page);
      await page.locator('select:visible').first().selectOption({ index: 0 });
      await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
      await closeServicePanel(page);

      await page.getByRole('button', { name: /^MAX$/ }).click();
      await page.getByRole('button', { name: /RUN CONTINUOUS/ }).click();
      await expect(page.locator('body')).toContainText(/halt/i, { timeout: 5000 });
    });
  });
}

test('index page shows all four machine cards', async ({ page }) => {
  await page.goto('/');
  for (const m of machines) {
    await expect(page.getByRole('link', { name: new RegExp(m, 'i') }).first()).toBeVisible();
  }
});
