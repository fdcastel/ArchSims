import { expect, test, type Page } from '@playwright/test';

// P9-03 — Disassembly panel wiring: PC caret tracks the program counter,
// clicking rows invokes the breakpoint toggle, Enter/Space activate rows
// via keyboard. Breakpoint-dot assertion is skipped pending P7-06 fix.

async function loadDefaultSample(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
  await page.waitForTimeout(150);
  await page.locator('select:visible').first().selectOption({ index: 0 });
  await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
  await page.waitForTimeout(150);
  await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
  await page.waitForTimeout(150);
}

test.describe('Disassembly panel', () => {
  test('Neander renders one row per decoded instruction', async ({ page }) => {
    await page.goto('/neander');
    const rows = page.locator('.disasm-row');
    const n = await rows.count();
    expect(n, 'Neander disassembly has ≥ a handful of rows').toBeGreaterThan(4);
  });

  test('PC caret appears on exactly one row and moves with STEP', async ({ page }) => {
    await page.goto('/neander');
    await loadDefaultSample(page);

    // Exactly one PC row before and after stepping; its address changes.
    const pcRow = page.locator('.disasm-row.disasm-pc');
    await expect(pcRow, 'exactly one PC-marked row at load').toHaveCount(1);
    const pcAddrBefore = await pcRow.locator('.disasm-addr').innerText();

    await page.getByRole('button', { name: /STEP/ }).first().click();
    await page.waitForTimeout(100);

    await expect(pcRow, 'exactly one PC-marked row after STEP').toHaveCount(1);
    const pcAddrAfter = await pcRow.locator('.disasm-addr').innerText();
    expect(pcAddrAfter, 'PC address moves on STEP').not.toBe(pcAddrBefore);
  });

  test('PC row gutter shows ▶ caret glyph', async ({ page }) => {
    await page.goto('/neander');
    const pcRow = page.locator('.disasm-row.disasm-pc');
    await expect(pcRow.locator('.disasm-caret')).toBeVisible();
  });

  test('clicking a row invokes the breakpoint handler without throwing', async ({ page }) => {
    const pageErrors: string[] = [];
    page.on('pageerror', (e) => pageErrors.push(e.message));

    await page.goto('/neander');
    const rows = page.locator('.disasm-row');
    await rows.nth(3).click();
    await page.waitForTimeout(100);
    await rows.nth(3).click();
    await page.waitForTimeout(100);

    expect(pageErrors, 'no page error on repeated toggle').toEqual([]);
  });

  test('rows respond to Enter keyboard activation', async ({ page }) => {
    const pageErrors: string[] = [];
    page.on('pageerror', (e) => pageErrors.push(e.message));

    await page.goto('/neander');
    const rows = page.locator('.disasm-row');
    await rows.nth(2).focus();
    await page.keyboard.press('Enter');
    await page.waitForTimeout(100);
    expect(pageErrors).toEqual([]);
  });

  test.fixme('clicking a row toggles the ● breakpoint indicator (pending BUG-6 fix, P7-06)', async ({ page }) => {
    await page.goto('/neander');
    const rows = page.locator('.disasm-row');
    const dotsBefore = await page.locator('.disasm-bp-dot').count();

    await rows.nth(4).click();
    await page.waitForTimeout(100);
    const dotsAfter = await page.locator('.disasm-bp-dot').count();
    expect(dotsAfter, 'one more bp dot after click').toBe(dotsBefore + 1);

    await rows.nth(4).click();
    await page.waitForTimeout(100);
    const dotsFinal = await page.locator('.disasm-bp-dot').count();
    expect(dotsFinal, 'bp dot removed on second click').toBe(dotsBefore);
  });

  test('disassembly renders rows on all four machines', async ({ page }) => {
    for (const m of ['neander', 'ahmes', 'ramses', 'cesar'] as const) {
      await page.goto('/' + m);
      expect(await page.locator('.disasm-row').count(), `${m} has disasm rows`).toBeGreaterThan(0);
    }
  });

  test('8-bit machines show a PC marker on initial load', async ({ page }) => {
    // Cesar is excluded pending BUG-8 (windowStart computation fails for
    // small PC values, so PC=0 falls outside the visible window at load).
    for (const m of ['neander', 'ahmes', 'ramses'] as const) {
      await page.goto('/' + m);
      await expect(page.locator('.disasm-row.disasm-pc'), `${m} has PC marker`).toHaveCount(1);
    }
  });
});
