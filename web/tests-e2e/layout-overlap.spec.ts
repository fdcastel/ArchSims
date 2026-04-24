import { expect, test } from '@playwright/test';

// P7-05 regression guard — register tiles and the memory grid must not
// overlap at desktop viewports. BUG-5 symptom: on Cesar the 16-bit BIN
// column blew past its grid track, pushing the tile over the middle
// column's memory grid. Fix landed in RegisterTile + per-panel grid
// track sizing (minmax(0, 1fr)).

function intersects(a: DOMRect, b: DOMRect): boolean {
  return !(a.right <= b.left || b.right <= a.left || a.bottom <= b.top || b.bottom <= a.top);
}

function intersectionArea(a: DOMRect, b: DOMRect): number {
  const x = Math.max(0, Math.min(a.right, b.right) - Math.max(a.left, b.left));
  const y = Math.max(0, Math.min(a.bottom, b.bottom) - Math.max(a.top, b.top));
  return x * y;
}

for (const m of ['neander', 'ahmes', 'ramses', 'cesar'] as const) {
  test(`${m}: register tiles and memory grid do not overlap at 1440px`, async ({ page }) => {
    await page.setViewportSize({ width: 1440, height: 900 });
    await page.goto(`/${m}`);

    const tiles = await page.locator('.reg-tile').all();
    expect(tiles.length, `${m} has register tiles`).toBeGreaterThan(0);
    const memGrid = page.locator('.mem-grid').first();
    await expect(memGrid).toBeVisible();

    const memBox = await memGrid.evaluate((el) => {
      const r = el.getBoundingClientRect();
      return { left: r.left, top: r.top, right: r.right, bottom: r.bottom } as DOMRect;
    });

    for (let i = 0; i < tiles.length; i++) {
      const tile = tiles[i];
      const tileBox = await tile.evaluate((el) => {
        const r = el.getBoundingClientRect();
        return { left: r.left, top: r.top, right: r.right, bottom: r.bottom } as DOMRect;
      });
      if (intersects(tileBox, memBox)) {
        const area = intersectionArea(tileBox, memBox);
        expect.fail(
          `${m} reg-tile #${i} intersects .mem-grid by ${area}px² — tile=${JSON.stringify(tileBox)} mem=${JSON.stringify(memBox)}`,
        );
      }
    }
  });
}
