import { expect, test, type Page } from '@playwright/test';

// P9-01 — Service Drawer tweaks. Verifies each tweak fires the right side
// effect in the DOM and persists in localStorage. Runs on Neander since the
// drawer markup is identical across machines; Cesar-specific concerns
// (samples list) live in samples.spec.ts.

async function openDrawer(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
  await page.waitForTimeout(150);
}

test.describe('Service Drawer — tweaks wiring', () => {
  test('palette buttons swap the chassis palette class', async ({ page }) => {
    await page.goto('/neander');
    await openDrawer(page);
    const chassis = page.locator('.chassis').first();

    await page.getByRole('button', { name: /^Amber$/ }).click();
    await expect(chassis).toHaveClass(/chassis-amber/);
    await expect(chassis).not.toHaveClass(/chassis-green/);

    await page.getByRole('button', { name: /^CRT Green$/ }).click();
    await expect(chassis).toHaveClass(/chassis-green/);
    await expect(chassis).not.toHaveClass(/chassis-amber/);

    await page.getByRole('button', { name: /^Paper$/ }).click();
    await expect(chassis).toHaveClass(/chassis-paper/);
  });

  test('density buttons swap the density class', async ({ page }) => {
    await page.goto('/neander');
    await openDrawer(page);
    const chassis = page.locator('.chassis').first();

    await page.getByRole('button', { name: /^Comfy$/ }).click();
    await expect(chassis).toHaveClass(/density-comfortable/);

    await page.getByRole('button', { name: /^Compact$/ }).click();
    await expect(chassis).toHaveClass(/density-compact/);
    await expect(chassis).not.toHaveClass(/density-comfortable/);
  });

  test('annotations on/off toggle the chassis class', async ({ page }) => {
    await page.goto('/neander');
    await openDrawer(page);
    const chassis = page.locator('.chassis').first();

    // Annotations row has two "Off" buttons on the page (also fetch-cycle).
    // Target by walking from the "Annotations" label.
    const annotationsRow = page.locator('.sd-row', { hasText: 'Annotations' });
    await annotationsRow.getByRole('button', { name: /^Off$/ }).click();
    await expect(chassis).toHaveClass(/ann-off/);

    await annotationsRow.getByRole('button', { name: /^On$/ }).click();
    await expect(chassis).toHaveClass(/ann-on/);
  });

  test('fetch-cycle toggle mounts/unmounts the FetchCycle panel', async ({ page }) => {
    await page.goto('/neander');
    await openDrawer(page);

    const fetchRow = page.locator('.sd-row', { hasText: 'Fetch-cycle widget' });
    await fetchRow.getByRole('button', { name: /^On$/ }).click();
    await expect(page.locator('[class*="fetch-cycle"], [class*="fc-cell"]').first()).toBeVisible();

    await fetchRow.getByRole('button', { name: /^Off$/ }).click();
    await expect(page.locator('[class*="fetch-cycle"], [class*="fc-cell"]')).toHaveCount(0);
  });

  test('active option carries the sd-on class for visual feedback', async ({ page }) => {
    await page.goto('/neander');
    await openDrawer(page);

    const hexBtn = page.getByRole('button', { name: /^HEX$/ });
    const decBtn = page.getByRole('button', { name: /^DEC$/ });
    await expect(hexBtn, 'HEX is the default base').toHaveClass(/sd-on/);
    await expect(decBtn).not.toHaveClass(/sd-on/);

    await decBtn.click();
    await expect(decBtn).toHaveClass(/sd-on/);
    await expect(hexBtn).not.toHaveClass(/sd-on/);
  });

  test('tweaks persist to localStorage under the per-machine key', async ({ page }) => {
    await page.goto('/neander');
    await openDrawer(page);
    await page.getByRole('button', { name: /^Paper$/ }).click();
    await page.getByRole('button', { name: /^DEC$/ }).click();

    // Read localStorage directly and survive a reload.
    const stored = await page.evaluate(() => localStorage.getItem('neander.tweaks'));
    expect(stored, 'neander.tweaks key written').not.toBeNull();
    const parsed = JSON.parse(stored ?? '{}');
    expect(parsed.palette).toBe('paper');
    expect(parsed.base).toBe('dec');

    await page.reload({ waitUntil: 'networkidle' });
    await expect(page.locator('.chassis').first()).toHaveClass(/chassis-paper/);
  });

  test('each machine has its own isolated tweaks key', async ({ page }) => {
    await page.goto('/neander');
    await openDrawer(page);
    await page.getByRole('button', { name: /^Amber$/ }).click();

    await page.goto('/ahmes');
    await openDrawer(page);
    await page.getByRole('button', { name: /^Paper$/ }).click();

    const neander = await page.evaluate(() => JSON.parse(localStorage.getItem('neander.tweaks') ?? '{}'));
    const ahmes = await page.evaluate(() => JSON.parse(localStorage.getItem('ahmes.tweaks') ?? '{}'));
    expect(neander.palette).toBe('amber');
    expect(ahmes.palette).toBe('paper');
  });

  test('SAVE .MEM triggers a browser download', async ({ page }) => {
    await page.goto('/neander');
    await openDrawer(page);
    const downloadPromise = page.waitForEvent('download', { timeout: 3000 });
    await page.getByRole('button', { name: /SAVE \.MEM/ }).click();
    const dl = await downloadPromise;
    expect(dl.suggestedFilename()).toMatch(/\.mem$/);
  });

  test('FULL RESET button is present and clickable', async ({ page }) => {
    await page.goto('/neander');

    // Step a few times before opening the drawer — the drawer's backdrop
    // intercepts pointer events on the main chassis while open.
    const step = page.getByRole('button', { name: /STEP/ }).first();
    await step.click();
    await step.click();
    const bodyBefore = await page.locator('body').innerText();

    await openDrawer(page);
    await page.getByRole('button', { name: /FULL RESET/ }).click();
    await page.waitForTimeout(200);
    const bodyAfter = await page.locator('body').innerText();
    expect(bodyBefore, 'FULL RESET changes DOM').not.toBe(bodyAfter);
  });

  test('backdrop click closes the drawer', async ({ page }) => {
    await page.goto('/neander');
    const drawer = page.locator('aside.service-drawer');

    await openDrawer(page);
    await expect(drawer).toHaveClass(/sd-open/);

    await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
    await expect(drawer).not.toHaveClass(/sd-open/);
  });

  test.fixme('Escape key closes the drawer (keydown handler is on backdrop div, not autofocused)', async ({ page }) => {
    await page.goto('/neander');
    const drawer = page.locator('aside.service-drawer');
    await openDrawer(page);
    await expect(drawer).toHaveClass(/sd-open/);
    await page.keyboard.press('Escape');
    await expect(drawer).not.toHaveClass(/sd-open/);
  });
});
