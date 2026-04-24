import { expect, test, type Page } from '@playwright/test';

// P9-04 — FlagBank wiring: each lamp reflects its CPU flag, across machines.
// The `.lamp-on` class is the source-of-truth for "lit"; absent = dim.

async function loadAndRunToHalt(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
  await page.waitForTimeout(150);
  await page.locator('select:visible').first().selectOption({ index: 0 });
  await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
  await page.waitForTimeout(150);
  await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
  await page.waitForTimeout(150);
  await page.getByRole('button', { name: /^MAX$/ }).click();
  await page.getByRole('button', { name: /RUN\s+CONTINUOUS/ }).click();
  await expect(page.locator('body')).toContainText(/halt/i, { timeout: 5000 });
}

// Scope lamp lookups to the FlagBank (panel's flag-bank) — the chassis
// header has its own HLT lamp with the same label.
function flagLamp(page: Page, label: string) {
  return page.locator('.flag-bank .lamp-wrap').filter({ has: page.locator(`.lamp-label`, { hasText: new RegExp(`^${label}$`) }) });
}

test.describe('FlagBank — per machine', () => {
  test('Neander shows N / Z / HLT lamps; Z is lit at load, HLT after halt', async ({ page }) => {
    await page.goto('/neander');

    const n = flagLamp(page, 'N').locator('.lamp');
    const z = flagLamp(page, 'Z').locator('.lamp');
    const hlt = flagLamp(page, 'HLT').locator('.lamp');

    await expect(n, 'N dim at load').not.toHaveClass(/lamp-on/);
    await expect(z, 'Z lit at load (AC=0 → zero flag)').toHaveClass(/lamp-on/);
    await expect(hlt, 'HLT dim at load').not.toHaveClass(/lamp-on/);

    await loadAndRunToHalt(page);
    await expect(hlt, 'HLT lit after natural halt').toHaveClass(/lamp-on/);
  });

  test('Ahmes adds C / V flags and renders them dim at load', async ({ page }) => {
    await page.goto('/ahmes');

    for (const label of ['N', 'Z', 'V', 'C', 'HLT'] as const) {
      await expect(flagLamp(page, label), `${label} lamp present`).toHaveCount(1);
    }
    await expect(flagLamp(page, 'C').locator('.lamp'), 'C dim at load').not.toHaveClass(/lamp-on/);
  });

  test('Ramses shows N / Z / C / HLT flags', async ({ page }) => {
    await page.goto('/ramses');
    for (const label of ['N', 'Z', 'C', 'HLT'] as const) {
      await expect(flagLamp(page, label), `${label} lamp present`).toHaveCount(1);
    }
  });

  test('Cesar shows N / Z / V / C / HLT flags', async ({ page }) => {
    await page.goto('/cesar');
    for (const label of ['N', 'Z', 'V', 'C', 'HLT'] as const) {
      await expect(flagLamp(page, label), `${label} lamp present`).toHaveCount(1);
    }
  });

  test('HLT lamp clears when RESET follows a natural halt', async ({ page }) => {
    await page.goto('/neander');
    await loadAndRunToHalt(page);

    const hlt = flagLamp(page, 'HLT').locator('.lamp');
    await expect(hlt, 'HLT lit after halt').toHaveClass(/lamp-on/);

    await page.getByRole('button', { name: /RESET/ }).first().click();
    await page.waitForTimeout(150);
    await expect(hlt, 'HLT cleared after RESET').not.toHaveClass(/lamp-on/);
  });

  test('HLT lamp is red (color-coded danger)', async ({ page }) => {
    await page.goto('/neander');
    await loadAndRunToHalt(page);
    const hltLamp = flagLamp(page, 'HLT').locator('.lamp');
    await expect(hltLamp).toHaveClass(/lamp-red/);
  });
});
