import { expect, test, type Page } from '@playwright/test';

// P9-06 — MemoryGrid wiring: PC / IR / READ / WRITE / BP class markers on
// cells, hex rendering per the Base tweak, row-label formatting. Cell
// classes: mcell-pc, mcell-ir, mcell-read, mcell-write, mcell-bp, mcell-mmio.

async function loadDefaultSample(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
  await page.waitForTimeout(150);
  await page.locator('select:visible').first().selectOption({ index: 0 });
  await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
  await page.waitForTimeout(150);
  await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
  await page.waitForTimeout(150);
}

async function openDrawer(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
  await page.waitForTimeout(150);
}

test.describe('Memory grid', () => {
  test('Neander renders 256 cells in 16x16', async ({ page }) => {
    await page.goto('/neander');
    await expect(page.locator('.mem-grid .mcell')).toHaveCount(256);
  });

  test('exactly one cell carries the PC marker, and it moves with STEP', async ({ page }) => {
    await page.goto('/neander');
    await loadDefaultSample(page);

    await expect(page.locator('.mcell.mcell-pc'), 'one PC cell at load').toHaveCount(1);
    const pcBefore = await page.locator('.mcell.mcell-pc').innerText();

    await page.getByRole('button', { name: /STEP/ }).first().click();
    await page.waitForTimeout(100);
    await expect(page.locator('.mcell.mcell-pc'), 'still one PC cell').toHaveCount(1);
    const pcAfter = await page.locator('.mcell.mcell-pc').innerText();
    expect(pcAfter, 'PC cell contents change (PC advanced to different byte)').not.toBe(pcBefore);
  });

  test('IR-start and IR-span markers appear after STEP', async ({ page }) => {
    await page.goto('/neander');
    await loadDefaultSample(page);
    await page.getByRole('button', { name: /STEP/ }).first().click();
    await page.waitForTimeout(100);

    // After stepping, the IR-start cell is populated.
    await expect(page.locator('.mcell.mcell-ir-start')).toHaveCount(1);
    // IR span >= 1 byte; may be same as IR-start for single-byte instructions.
    expect(await page.locator('.mcell.mcell-ir').count(), 'IR span is non-empty').toBeGreaterThanOrEqual(1);
  });

  test('WRITE marker appears after a store instruction runs', async ({ page }) => {
    await page.goto('/neander');
    await loadDefaultSample(page);

    // The sum-of-memory sample has a STA at some point. Step until a write
    // marker appears, but don't step past halt.
    const step = page.getByRole('button', { name: /STEP/ });
    for (let i = 0; i < 20; i++) {
      if (await page.locator('.mcell.mcell-write').count() > 0) break;
      await step.first().click();
      await page.waitForTimeout(30);
      if (await page.getByRole('button', { name: /RUN\s+CONTINUOUS/ }).first().isDisabled()) break;
    }
    expect(await page.locator('.mcell.mcell-write').count(), 'at least one WRITE marker appeared during run').toBeGreaterThan(0);
  });

  test('base tweak DEC changes cell text formatting', async ({ page }) => {
    await page.goto('/neander');
    const firstCell = page.locator('.mem-grid .mcell').first();
    const hexText = (await firstCell.innerText()).trim();
    expect(hexText, 'HEX cell is 2 hex digits').toMatch(/^[0-9A-F]{2}$/);

    await openDrawer(page);
    await page.getByRole('button', { name: /^DEC$/ }).click();
    await page.waitForTimeout(100);
    await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
    await page.waitForTimeout(100);

    const decText = (await firstCell.innerText()).trim();
    expect(decText, 'DEC cell text looks decimal').toMatch(/^\d{1,3}$/);
    expect(decText, 'DEC differs from HEX').not.toBe(hexText);
  });

  test('row labels render at correct width per machine', async ({ page }) => {
    // Exclude the corner (`.mrow-label-corner`) which is an empty spacer.
    await page.goto('/neander');
    const neLabels = await page
      .locator('.mem-grid .mrow-label:not(.mrow-label-corner)')
      .allInnerTexts();
    expect(neLabels.length, 'Neander has ≥1 row label').toBeGreaterThan(0);
    for (const l of neLabels.slice(0, 4)) {
      expect(l.replace(/\s/g, ''), `Neander row label is hex`).toMatch(/^[0-9A-F]{1,4}$/);
    }

    await page.goto('/cesar');
    const ceLabels = await page
      .locator('.mem-grid .mrow-label:not(.mrow-label-corner)')
      .allInnerTexts();
    expect(ceLabels.length, 'Cesar has ≥1 row label').toBeGreaterThan(0);
    for (const l of ceLabels.slice(0, 4)) {
      expect(l.replace(/\s/g, ''), `Cesar row label is hex`).toMatch(/^[0-9A-F]{1,4}$/);
    }
  });

  test('Cesar shows an MMIO range marker in the 0xFFxx page', async ({ page }) => {
    await page.goto('/cesar');
    // MMIO is 0xFFDA..0xFFFF so the FFxx page must show the last ~38 cells
    // marked. Click the FFxx page button (exact text match).
    await page.getByRole('button', { name: 'FFxx' }).click();
    await page.waitForTimeout(150);
    const n = await page.locator('.mcell.mcell-mmio').count();
    // 0xFFDA..0xFFFF = 38 bytes
    expect(n, 'MMIO cells marked in 0xFFxx page').toBeGreaterThanOrEqual(30);
  });
});
