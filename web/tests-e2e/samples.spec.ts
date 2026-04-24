import { expect, test, type Page } from '@playwright/test';

// P9-08 — Every sample in SAMPLES_BY_MACHINE loads, runs to HALT, and the
// panels surface the end-state. Detailed end-state assertions (specific
// accumulator / register values) live in Vitest core tests; here we assert
// the UI sees the sample, runs it, and reflects the halt.

async function openDrawer(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
  await page.waitForTimeout(150);
}
async function closeDrawer(page: Page): Promise<void> {
  await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
  await page.waitForTimeout(150);
}

async function listSamples(page: Page): Promise<Array<{ value: string; label: string }>> {
  await openDrawer(page);
  const opts = await page
    .locator('select:visible option')
    .evaluateAll((els) =>
      els.map((e) => ({
        value: e.getAttribute('value') ?? '',
        label: (e.textContent ?? '').trim(),
      })),
    );
  await closeDrawer(page);
  return opts;
}

async function loadSampleAndRun(page: Page, value: string): Promise<void> {
  await openDrawer(page);
  await page.locator('select:visible').first().selectOption(value);
  await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
  await page.waitForTimeout(150);
  await closeDrawer(page);
  await page.getByRole('button', { name: /^MAX$/ }).click();
  await page.getByRole('button', { name: /RUN\s+CONTINUOUS/ }).click();
}

const machines = ['neander', 'ahmes', 'ramses', 'cesar'] as const;

test.describe('Sample registry', () => {
  test('every machine registers at least one sample', async ({ page }) => {
    for (const m of machines) {
      await page.goto('/' + m);
      const samples = await listSamples(page);
      expect(samples.length, `${m} has ≥1 sample`).toBeGreaterThan(0);
      for (const s of samples) {
        expect(s.label, `${m} sample has non-empty label`).not.toBe('');
        expect(s.value, `${m} sample has non-empty id`).not.toBe('');
      }
    }
  });

  test('Cesar lists both BitShift and Hello samples', async ({ page }) => {
    await page.goto('/cesar');
    const samples = await listSamples(page);
    const labels = samples.map((s) => s.label.toLowerCase());
    expect(labels.some((l) => l.includes('bitshift')), 'BitShift.Cesar present').toBe(true);
    expect(labels.some((l) => l.includes('hello')), 'Hello.Cesar present').toBe(true);
  });

  for (const m of machines) {
    test(`${m}: every registered sample loads and runs to HALT`, async ({ page }) => {
      await page.goto('/' + m);
      const samples = await listSamples(page);
      expect(samples.length, `${m} has samples`).toBeGreaterThan(0);

      for (const s of samples) {
        await loadSampleAndRun(page, s.value);
        await expect(
          page.locator('body'),
          `${m} / ${s.label} reaches HALT`,
        ).toContainText(/halt/i, { timeout: 8000 });

        // Reset so the HALT lamp doesn't linger for the next sample.
        await page.getByRole('button', { name: /RESET/ }).first().click();
        await page.waitForTimeout(100);
      }
    });
  }

  test('loading a sample writes the sample bytes into memory (Neander)', async ({ page }) => {
    // Neander sum sample writes 0x02/0x04/0x08/0x10 at 0x80..0x83. Verify by
    // reading the .mem-grid cells at those addresses.
    await page.goto('/neander');
    await openDrawer(page);
    await page.locator('select:visible').first().selectOption({ index: 0 });
    await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
    await page.waitForTimeout(150);
    await closeDrawer(page);

    const cells = page.locator('.mem-grid .mcell');
    expect((await cells.nth(0x80).innerText()).trim(), 'MEM[0x80]=0x02').toBe('02');
    expect((await cells.nth(0x81).innerText()).trim(), 'MEM[0x81]=0x04').toBe('04');
    expect((await cells.nth(0x82).innerText()).trim(), 'MEM[0x82]=0x08').toBe('08');
    expect((await cells.nth(0x83).innerText()).trim(), 'MEM[0x83]=0x10').toBe('10');
  });

  test('Neander sum sample ends with AC=0x1E after RUN', async ({ page }) => {
    await page.goto('/neander');
    await loadSampleAndRun(page, 'neander-sum');
    await expect(page.locator('body')).toContainText(/halt/i, { timeout: 5000 });

    const acTile = page.locator('.reg-tile', { hasText: /^AC/ });
    await expect(acTile, 'AC = 0x1E after sum (02+04+08+10 = 30 = 0x1E)').toContainText('1E');
  });
});
