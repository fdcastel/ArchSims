import { describe, expect, it } from 'vitest';
import { render } from '@testing-library/svelte';
import Lamp from '@/ui/primitives/Lamp.svelte';

describe('Lamp', () => {
  it('renders dim by default', () => {
    const { container } = render(Lamp, { props: { label: 'PWR' } });
    const lamp = container.querySelector('.lamp');
    expect(lamp).not.toBeNull();
    expect(lamp).not.toHaveClass('lamp-on');
  });

  it('applies lamp-on when on=true', () => {
    const { container } = render(Lamp, { props: { on: true, label: 'RUN' } });
    expect(container.querySelector('.lamp')).toHaveClass('lamp-on');
  });

  it('applies the colour class for each accent', () => {
    for (const [color, cls] of [
      ['amber', 'lamp-amber'],
      ['green', 'lamp-green'],
      ['red', 'lamp-red'],
      ['ink', 'lamp-ink'],
    ] as const) {
      const { container, unmount } = render(Lamp, { props: { on: true, color, label: 'X' } });
      expect(container.querySelector('.lamp'), `${color} -> ${cls}`).toHaveClass(cls);
      unmount();
    }
  });

  it('renders the label and sub captions', () => {
    const { getByText } = render(Lamp, { props: { label: 'HLT', sub: 'halt' } });
    expect(getByText('HLT')).toBeInTheDocument();
    expect(getByText('halt')).toBeInTheDocument();
  });

  it('omits the label block when label is empty', () => {
    const { container } = render(Lamp, { props: {} });
    expect(container.querySelector('.lamp-label')).toBeNull();
    expect(container.querySelector('.lamp-sub')).toBeNull();
  });

  it('exposes label via aria-label on the lamp element', () => {
    const { container } = render(Lamp, { props: { label: 'HLT' } });
    const lamp = container.querySelector('.lamp');
    expect(lamp).toHaveAttribute('aria-label', 'HLT');
    expect(lamp).toHaveAttribute('role', 'img');
  });
});
