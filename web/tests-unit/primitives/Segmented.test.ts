import { describe, expect, it } from 'vitest';
import { render } from '@testing-library/svelte';
import Segmented from '@/ui/primitives/Segmented.svelte';

describe('Segmented', () => {
  it('renders one span per character of text', () => {
    const { container } = render(Segmented, { props: { text: 'FFFF' } });
    const chars = container.querySelectorAll('.seg-char');
    expect(chars).toHaveLength(4);
    expect(chars[0]?.textContent).toBe('F');
    expect(chars[3]?.textContent).toBe('F');
  });

  it('renders an empty span set for empty text', () => {
    const { container } = render(Segmented, { props: { text: '' } });
    expect(container.querySelectorAll('.seg-char')).toHaveLength(0);
  });

  it('marks whitespace chars with seg-blank', () => {
    const { container } = render(Segmented, { props: { text: 'A B' } });
    const chars = container.querySelectorAll('.seg-char');
    expect(chars).toHaveLength(3);
    expect(chars[0]).not.toHaveClass('seg-blank');
    expect(chars[1]).toHaveClass('seg-blank');
    expect(chars[2]).not.toHaveClass('seg-blank');
  });

  it('applies the size class per prop', () => {
    for (const [size, cls] of [
      ['xl', 'seg-xl'],
      ['md', 'seg-md'],
      ['sm', 'seg-sm'],
    ] as const) {
      const { container, unmount } = render(Segmented, {
        props: { text: '42', size },
      });
      expect(container.querySelector('.segmented')).toHaveClass(cls);
      unmount();
    }
  });

  it('applies the tone class per colour', () => {
    for (const [color, cls] of [
      ['amber', 'seg-amber'],
      ['green', 'seg-green'],
      ['ink', 'seg-ink'],
      ['dim', 'seg-dim'],
    ] as const) {
      const { container, unmount } = render(Segmented, {
        props: { text: '42', color },
      });
      expect(container.querySelector('.segmented')).toHaveClass(cls);
      unmount();
    }
  });

  it('defaults to md size + amber colour', () => {
    const { container } = render(Segmented, { props: { text: 'A' } });
    const seg = container.querySelector('.segmented');
    expect(seg).toHaveClass('seg-md');
    expect(seg).toHaveClass('seg-amber');
  });
});
