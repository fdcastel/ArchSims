import { expect, test, type Page } from '@playwright/test';

// P9-02 — Register tiles render values correctly and react to CPU state.
// RegisterTile always shows HEX (prominent) + DEC + BIN side-by-side; the
// Memory-base tweak only affects the memory grid, not these tiles.

async function loadDefaultSample(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
  await page.waitForTimeout(150);
  await page.locator('select:visible').first().selectOption({ index: 0 });
  await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
  await page.waitForTimeout(150);
  await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
  await page.waitForTimeout(150);
}

test.describe('Register tiles', () => {
  test('Neander AC tile renders HEX + DEC + BIN for the same byte', async ({ page }) => {
    await page.goto('/neander');
    const acTile = page.locator('.reg-tile', { hasText: /^AC/ });

    // Initial AC = 0 → HEX "00", DEC "000", BIN "00000000"
    await expect(acTile).toContainText('00');
    await expect(acTile).toContainText('DEC');
    await expect(acTile).toContainText('000');
    await expect(acTile).toContainText('BIN');
    await expect(acTile).toContainText('00000000');
  });

  test('AC updates after stepping through a Lda+Hlt sequence', async ({ page }) => {
    await page.goto('/neander');
    await loadDefaultSample(page);

    const acTile = page.locator('.reg-tile', { hasText: /^AC/ });
    const pcTile = page.locator('.reg-tile', { hasText: /^PC/ });
    const acBefore = await acTile.innerText();
    const pcBefore = await pcTile.innerText();

    // Step enough times to see AC change on the sum-of-memory sample.
    const step = page.getByRole('button', { name: /STEP/ }).first();
    for (let i = 0; i < 6; i++) {
      await step.click();
      await page.waitForTimeout(50);
    }

    const acAfter = await acTile.innerText();
    const pcAfter = await pcTile.innerText();
    expect(acAfter, 'AC changes after stepping').not.toBe(acBefore);
    expect(pcAfter, 'PC changes after stepping').not.toBe(pcBefore);
  });

  test('Neander AC tile has the reg-active class (always active)', async ({ page }) => {
    await page.goto('/neander');
    const acTile = page.locator('.reg-tile', { hasText: /^AC/ });
    await expect(acTile).toHaveClass(/reg-active/);
  });

  test('Ramses active register indicator follows the IR opcode', async ({ page }) => {
    await page.goto('/ramses');
    await loadDefaultSample(page);

    // Step a few; at least one of RA/RB/RX should pick up the reg-active class
    // for at least one of the steps.
    const step = page.getByRole('button', { name: /STEP/ }).first();
    const activeCounts: number[] = [];
    for (let i = 0; i < 6; i++) {
      await step.click();
      await page.waitForTimeout(50);
      activeCounts.push(await page.locator('.reg-tile.reg-active').count());
    }
    // Should have seen at least one active register at some step (PC/IR isn't
    // a "reg-tile" on Ramses — only RA/RB/RX are).
    expect(activeCounts.some((c) => c > 0), 'at least one step highlights an active reg').toBe(true);
  });

  test('Cesar renders 8 register tiles (R0..R7) and they are 16-bit', async ({ page }) => {
    await page.goto('/cesar');
    const tiles = page.locator('.reg-tile');
    await expect(tiles).toHaveCount(8);

    // 16-bit means HEX is 4 chars ("0000") and BIN is 16 chars.
    const firstTile = tiles.first();
    await expect(firstTile).toContainText('R0');
    await expect(firstTile).toContainText('0000');
    // 16-bit BIN has 16 zero-chars — Segmented renders one per <span> so
    // innerText inserts whitespace; count digit characters instead.
    const text = await firstTile.innerText();
    const binRow = text.split('BIN')[1] ?? '';
    const zeros = (binRow.match(/0/g) ?? []).length;
    expect(zeros, 'Cesar R0 BIN row has ≥16 zero digits').toBeGreaterThanOrEqual(16);
  });

  test('R7 on Cesar (PC) updates with each STEP', async ({ page }) => {
    await page.goto('/cesar');
    await loadDefaultSample(page);

    const r7 = page.locator('.reg-tile', { hasText: /^R7/ });
    const before = await r7.innerText();
    await page.getByRole('button', { name: /STEP/ }).first().click();
    await page.waitForTimeout(100);
    const after = await r7.innerText();
    expect(after, 'R7 (PC) changes on STEP').not.toBe(before);
  });

  test('RESET returns register values to their initial state', async ({ page }) => {
    await page.goto('/neander');
    await loadDefaultSample(page);

    const acTile = page.locator('.reg-tile', { hasText: /^AC/ });
    const acInitial = await acTile.innerText();

    for (let i = 0; i < 4; i++) await page.getByRole('button', { name: /STEP/ }).first().click();
    await page.waitForTimeout(100);
    expect(await acTile.innerText(), 'AC changed before RESET').not.toBe(acInitial);

    await page.getByRole('button', { name: /RESET/ }).first().click();
    await page.waitForTimeout(100);
    expect(await acTile.innerText(), 'AC restored after RESET').toBe(acInitial);
  });
});
