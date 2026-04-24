import { expect, test, type Page } from '@playwright/test';

// P8-02 regression guard — verify the RUN / BREAK / HALT state machine on the
// Controls panel is consistent across machines. The button must be:
//   - "RUN" enabled         when idle (not running, not halted)
//   - "BREAK" (sub PAUSE)   while the run-loop is active
//   - "RUN" disabled        after halt (HLT lamp on, reset required)
//
// Originally filed as a bug because the Neander sum sample halts too fast at
// MAX speed to observe the BREAK state. This spec uses 1 Hz so the run-loop
// stays live long enough to inspect each phase.

const machines = ['neander', 'ahmes', 'ramses', 'cesar'] as const;

async function openServicePanel(page: Page): Promise<void> {
  await page.getByRole('button', { name: /SERVICE PANEL/ }).first().click();
}
async function closeServicePanel(page: Page): Promise<void> {
  await page.getByRole('button', { name: /Close service panel/i }).first().click({ force: true });
}

for (const m of machines) {
  test.describe(`${m} run/break/halt state machine`, () => {
    test('button shows RUN → BREAK → RUN-disabled across run lifecycle', async ({ page }) => {
      await page.goto(`/${m}`);

      const runBtn = page.getByRole('button', { name: /RUN\s+CONTINUOUS/ }).first();
      const breakBtn = page.getByRole('button', { name: /^BREAK/ }).first();

      await expect(runBtn, 'idle: RUN is visible').toBeVisible();
      await expect(runBtn, 'idle: RUN is enabled').toBeEnabled();
      await expect(breakBtn, 'idle: BREAK is not present').toHaveCount(0);

      // Load the default sample so the CPU has real instructions to execute.
      await openServicePanel(page);
      await page.locator('select:visible').first().selectOption({ index: 0 });
      await page.getByRole('button', { name: /^\s*↻?\s*LOAD\s*$/ }).first().click();
      await closeServicePanel(page);

      // 1 Hz keeps the run-loop alive long enough to observe BREAK.
      await page.getByRole('button', { name: /^1 Hz$/ }).click();
      await runBtn.click();

      await expect(breakBtn, 'running: BREAK replaces RUN').toBeVisible({ timeout: 2000 });
      await expect(page.getByRole('button', { name: /RUN\s+CONTINUOUS/ })).toHaveCount(0);

      // Bump to MAX so we reach HALT promptly, without waiting for 1 Hz ticks.
      await page.getByRole('button', { name: /^MAX$/ }).click();

      // After HALT the button flips back to RUN, but disabled.
      await expect(runBtn, 'halted: RUN visible again').toBeVisible({ timeout: 5000 });
      await expect(runBtn, 'halted: RUN is disabled').toBeDisabled();
      await expect(page.locator('body'), 'halted: HLT lamp/text present').toContainText(/halt/i);

      // RESET clears the halt; RUN re-enables.
      await page.getByRole('button', { name: /RESET/ }).first().click();
      await expect(runBtn, 'after reset: RUN re-enabled').toBeEnabled();
    });
  });
}
