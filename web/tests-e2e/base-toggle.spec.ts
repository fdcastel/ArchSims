import { expect, test, type Page } from '@playwright/test';

// P8-03 regression guard — toggling Memory base HEX → DEC → BIN → HEX must
// return the rendered body text to its initial HEX state (with the drawer
// open in both snapshots so the drawer's own DOM is identical).

const machines = ['neander', 'ahmes', 'ramses', 'cesar'] as const;

async function openServicePanel(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
}

for (const m of machines) {
  test(`${m} base toggle HEX→DEC→BIN→HEX is idempotent`, async ({ page }) => {
    await page.goto(`/${m}`);
    await openServicePanel(page);

    const initial = await page.locator('body').innerText();

    await page.getByRole('button', { name: /^DEC$/ }).click();
    const afterDec = await page.locator('body').innerText();
    expect(afterDec, 'DEC changes the body').not.toBe(initial);

    await page.getByRole('button', { name: /^BIN$/ }).click();
    const afterBin = await page.locator('body').innerText();
    expect(afterBin, 'BIN changes the body').not.toBe(afterDec);

    await page.getByRole('button', { name: /^HEX$/ }).click();
    const afterHex = await page.locator('body').innerText();
    expect(afterHex, 'round-trip back to HEX matches initial').toBe(initial);
  });
}
