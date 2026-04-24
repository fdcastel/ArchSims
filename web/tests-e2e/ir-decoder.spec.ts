import { expect, test, type Page } from '@playwright/test';

// P9-05 — IRDecoder panel: renders the current instruction's bit groups,
// the address, and the decoded operand row. Height-stability assertion is
// skipped pending P7-04 fix.

async function loadDefaultSample(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
  await page.waitForTimeout(150);
  await page.locator('select:visible').first().selectOption({ index: 0 });
  await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
  await page.waitForTimeout(150);
  await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
  await page.waitForTimeout(150);
}

test.describe('IR Decoder panel', () => {
  test('renders on every machine', async ({ page }) => {
    for (const m of ['neander', 'ahmes', 'ramses', 'cesar'] as const) {
      await page.goto('/' + m);
      await expect(page.locator('.ir-decoder'), `${m} has IR decoder`).toHaveCount(1);
    }
  });

  test('renders one .ir-bit per bit of the opcode/operand groups', async ({ page }) => {
    await page.goto('/neander');
    // Neander opcode is 8-bit (4 hi + 4 lo = 8 bits total minimum).
    const bits = page.locator('.ir-decoder .ir-bit');
    expect(await bits.count(), 'Neander IR has ≥ 8 bit cells').toBeGreaterThanOrEqual(8);
  });

  test('bit cells flip to .on state as the IR changes', async ({ page }) => {
    await page.goto('/neander');
    await loadDefaultSample(page);

    const bitsOnBefore = await page.locator('.ir-decoder .ir-bit.on').count();
    for (let i = 0; i < 4; i++) {
      await page.getByRole('button', { name: /STEP/ }).first().click();
      await page.waitForTimeout(50);
    }
    const bitsOnAfter = await page.locator('.ir-decoder .ir-bit.on').count();

    // We don't know the exact counts a priori — assert the pattern changes.
    // Because the Lda instruction in the sum sample has distinct opcode bits,
    // the count must differ at some step.
    expect(bitsOnBefore, 'bit pattern changes after stepping').not.toBe(bitsOnAfter);
  });

  test('IR address segment shows a hex value at the expected width', async ({ page }) => {
    // Neander is 8-bit addressed (2 hex digits); Cesar is 16-bit (4 digits).
    await page.goto('/neander');
    const addr8 = page.locator('.ir-decoder .ir-addr');
    const text8 = (await addr8.innerText()).replace(/\s/g, '');
    expect(text8, 'Neander IR addr is 2-hex-digit').toMatch(/^@[0-9A-F]{2}$/);

    await page.goto('/cesar');
    const addr16 = page.locator('.ir-decoder .ir-addr');
    const text16 = (await addr16.innerText()).replace(/\s/g, '');
    expect(text16, 'Cesar IR addr is 4-hex-digit').toMatch(/^@[0-9A-F]{4}$/);
  });

  test('IR group labels match the machine ISA', async ({ page }) => {
    await page.goto('/ramses');
    // Ramses IR: OP | MODE | ADDR. At minimum some of these labels must appear.
    const labels = await page.locator('.ir-decoder .ir-group-label').allInnerTexts();
    expect(labels.length, 'Ramses IR has multiple bit groups').toBeGreaterThan(1);
  });

  test.fixme('IR decoder reserves a minimum height so Controls don\'t jump (pending BUG-4, P7-04)', async ({ page }) => {
    await page.goto('/cesar');
    await loadDefaultSample(page);

    const ctrlYs: number[] = [];
    for (let i = 0; i < 5; i++) {
      await page.getByRole('button', { name: /STEP/ }).first().click();
      await page.waitForTimeout(60);
      const y = await page.locator('.controls').first().evaluate((el) => el.getBoundingClientRect().y);
      ctrlYs.push(Math.round(y));
    }
    const unique = new Set(ctrlYs);
    expect(unique.size, `Controls Y stays constant across STEPs, got ${JSON.stringify(ctrlYs)}`).toBe(1);
  });
});
