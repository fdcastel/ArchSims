import { describe, expect, it, vi } from 'vitest';
import { render } from '@testing-library/svelte';
import userEvent from '@testing-library/user-event';
import PanelButton from '@/ui/primitives/PanelButton.svelte';

describe('PanelButton', () => {
  it('renders the label and sub captions', () => {
    const { getByText } = render(PanelButton, {
      props: { label: 'STEP', sub: '1 INSTR' },
    });
    expect(getByText('STEP')).toBeInTheDocument();
    expect(getByText('1 INSTR')).toBeInTheDocument();
  });

  it('omits the sub span when sub is empty', () => {
    const { container } = render(PanelButton, { props: { label: 'RUN' } });
    expect(container.querySelector('.panel-btn-sub')).toBeNull();
  });

  it('calls onClick when clicked', async () => {
    const onClick = vi.fn();
    const { getByRole } = render(PanelButton, { props: { label: 'STEP', onClick } });
    await userEvent.click(getByRole('button'));
    expect(onClick).toHaveBeenCalledTimes(1);
  });

  it('does not fire onClick while disabled', async () => {
    const onClick = vi.fn();
    const { getByRole } = render(PanelButton, {
      props: { label: 'STEP', onClick, disabled: true },
    });
    const btn = getByRole('button');
    expect(btn).toBeDisabled();
    await userEvent.click(btn);
    expect(onClick).not.toHaveBeenCalled();
  });

  it('applies the colour class per variant', () => {
    for (const [variant, cls] of [
      ['default', 'panel-btn-default'],
      ['green', 'panel-btn-green'],
      ['red', 'panel-btn-red'],
      ['amber', 'panel-btn-amber'],
    ] as const) {
      const { container, unmount } = render(PanelButton, {
        props: { label: 'X', variant },
      });
      expect(container.querySelector('button')).toHaveClass(cls);
      unmount();
    }
  });

  it('exposes the held state as a class for sticky buttons', () => {
    const { container } = render(PanelButton, { props: { label: 'X', held: true } });
    expect(container.querySelector('button')).toHaveClass('held');
  });

  it('activates on Enter and Space via keyboard (native <button> behaviour)', async () => {
    const onClick = vi.fn();
    const { getByRole } = render(PanelButton, { props: { label: 'STEP', onClick } });
    getByRole('button').focus();
    await userEvent.keyboard('{Enter}');
    await userEvent.keyboard(' ');
    expect(onClick).toHaveBeenCalledTimes(2);
  });
});
