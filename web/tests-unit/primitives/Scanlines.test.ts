import { describe, expect, it } from 'vitest';
import { render } from '@testing-library/svelte';
import Scanlines from '@/ui/primitives/Scanlines.svelte';

describe('Scanlines', () => {
  it('renders a scanlines div with aria-hidden', () => {
    const { container } = render(Scanlines, { props: {} });
    const el = container.querySelector('.scanlines');
    expect(el).not.toBeNull();
    expect(el).toHaveAttribute('aria-hidden', 'true');
  });

  it('defaults to the amber palette', () => {
    const { container } = render(Scanlines, { props: {} });
    expect(container.querySelector('.scanlines')).toHaveClass('scanlines-amber');
  });

  it('applies the palette variant class', () => {
    for (const [palette, cls] of [
      ['amber', 'scanlines-amber'],
      ['green', 'scanlines-green'],
      ['paper', 'scanlines-paper'],
    ] as const) {
      const { container, unmount } = render(Scanlines, { props: { palette } });
      expect(container.querySelector('.scanlines')).toHaveClass(cls);
      unmount();
    }
  });
});
