import { expect, test, type Page } from '@playwright/test';

// P9-09 — Chassis-level concerns: PWR / RUN / HLT status lamps, title/sub
// labels, serial badge, machine-chain footer with clickable siblings.

async function loadDefaultSample(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
  await page.waitForTimeout(150);
  await page.locator('select:visible').first().selectOption({ index: 0 });
  await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
  await page.waitForTimeout(150);
  await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
  await page.waitForTimeout(150);
}

// The chassis header has its own lamp row; scope lookups there so we don't
// pick up flag-bank lamps with the same labels.
function headerLamp(page: Page, label: string) {
  return page
    .locator('.chassis-head-right .lamp-wrap')
    .filter({ has: page.locator('.lamp-label', { hasText: new RegExp(`^${label}$`) }) });
}

test.describe('Chassis header + footer', () => {
  test('every machine page renders a chassis with title + sub', async ({ page }) => {
    const titles = [
      ['neander', 'NEANDER'],
      ['ahmes', 'AHMES'],
      ['ramses', 'RAMSES'],
      ['cesar', 'CESAR'],
    ] as const;
    for (const [slug, title] of titles) {
      await page.goto('/' + slug);
      await expect(page.locator('.chassis-title')).toContainText(title);
      await expect(page.locator('.chassis-sub')).toBeVisible();
    }
  });

  test('PWR lamp is always lit (red)', async ({ page }) => {
    await page.goto('/neander');
    const pwr = headerLamp(page, 'PWR').locator('.lamp');
    await expect(pwr, 'PWR is lit').toHaveClass(/lamp-on/);
    await expect(pwr, 'PWR is red').toHaveClass(/lamp-red/);
  });

  test('RUN lamp is dim at load; HLT lamp is dim at load', async ({ page }) => {
    await page.goto('/neander');
    await expect(headerLamp(page, 'RUN').locator('.lamp'), 'RUN dim at load').not.toHaveClass(/lamp-on/);
    await expect(headerLamp(page, 'HLT').locator('.lamp'), 'HLT dim at load').not.toHaveClass(/lamp-on/);
  });

  test('RUN lamp lights while the run-loop is active', async ({ page }) => {
    await page.goto('/neander');
    await loadDefaultSample(page);
    await page.getByRole('button', { name: /^1 Hz$/ }).click();
    await page.getByRole('button', { name: /RUN\s+CONTINUOUS/ }).click();

    await expect(headerLamp(page, 'RUN').locator('.lamp'), 'RUN lit during run').toHaveClass(/lamp-on/);

    await page.getByRole('button', { name: /^BREAK/ }).click();
    await expect(headerLamp(page, 'RUN').locator('.lamp'), 'RUN dim after BREAK').not.toHaveClass(/lamp-on/);
  });

  test('HLT lamp lights on halt, clears on RESET', async ({ page }) => {
    await page.goto('/neander');
    await loadDefaultSample(page);
    await page.getByRole('button', { name: /^MAX$/ }).click();
    await page.getByRole('button', { name: /RUN\s+CONTINUOUS/ }).click();

    await expect(headerLamp(page, 'HLT').locator('.lamp'), 'HLT lit on halt').toHaveClass(/lamp-on/);
    await expect(headerLamp(page, 'RUN').locator('.lamp'), 'RUN dim on halt').not.toHaveClass(/lamp-on/);

    await page.getByRole('button', { name: /RESET/ }).first().click();
    await page.waitForTimeout(100);
    await expect(headerLamp(page, 'HLT').locator('.lamp'), 'HLT cleared on RESET').not.toHaveClass(/lamp-on/);
  });

  test('serial badge shows the model string', async ({ page }) => {
    await page.goto('/neander');
    await expect(page.locator('.serial')).toContainText(/SN-\d+ \/ AP-\d+/);
  });

  test('chassis foot shows the machine chain with the current one bold', async ({ page }) => {
    await page.goto('/ramses');
    const chain = page.locator('.chassis-foot-mid');
    await expect(chain).toContainText('NEANDER');
    await expect(chain).toContainText('AHMES');
    await expect(chain).toContainText('RAMSES');
    await expect(chain).toContainText('CESAR');
    // The current one is wrapped in <b>, not <a>.
    await expect(chain.locator('b')).toHaveText('RAMSES');
    await expect(chain.locator('a'), 'three sibling links on RAMSES page').toHaveCount(3);
  });

  test('clicking a sibling link navigates to that machine', async ({ page }) => {
    await page.goto('/ramses');
    await page.locator('.chassis-foot-mid a', { hasText: 'CESAR' }).click();
    await expect(page).toHaveURL(/\/cesar\/?$/);
    await expect(page.locator('.chassis-title')).toContainText('CESAR');
  });

  test('SERVICE PANEL button reflects the drawer open state with foot-service-on', async ({ page }) => {
    await page.goto('/neander');
    const btn = page.getByRole('button', { name: /SERVICE PANEL/ }).first();
    const drawer = page.locator('aside.service-drawer');

    await expect(btn).not.toHaveClass(/foot-service-on/);
    await btn.click();
    await expect(drawer, 'drawer opens on click').toHaveClass(/sd-open/);
    await expect(btn, 'button shows active state when drawer open').toHaveClass(/foot-service-on/);
  });

  test('aria-hidden on drawer flips with open state', async ({ page }) => {
    await page.goto('/neander');
    const drawer = page.locator('aside.service-drawer');
    await expect(drawer, 'closed drawer hidden from a11y tree').toHaveAttribute('aria-hidden', 'true');

    await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
    await expect(drawer, 'open drawer exposed to a11y tree').toHaveAttribute('aria-hidden', 'false');
  });
});
