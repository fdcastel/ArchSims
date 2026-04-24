import { describe, expect, it, vi } from 'vitest';
import { render } from '@testing-library/svelte';
import userEvent from '@testing-library/user-event';
import Toggle from '@/ui/primitives/Toggle.svelte';

describe('Toggle', () => {
  it('renders in off state by default', () => {
    const { container } = render(Toggle, { props: {} });
    const btn = container.querySelector('.toggle');
    expect(btn).not.toHaveClass('toggle-on');
    expect(btn).toHaveAttribute('aria-pressed', 'false');
  });

  it('renders in on state when on=true', () => {
    const { container } = render(Toggle, { props: { on: true } });
    const btn = container.querySelector('.toggle');
    expect(btn).toHaveClass('toggle-on');
    expect(btn).toHaveAttribute('aria-pressed', 'true');
  });

  it('renders optional label', () => {
    const { getByText } = render(Toggle, { props: { label: 'SCAN' } });
    expect(getByText('SCAN')).toBeInTheDocument();
  });

  it('fires onChange with the next state on click', async () => {
    const onChange = vi.fn();
    const { getByRole } = render(Toggle, { props: { on: false, onChange } });
    await userEvent.click(getByRole('button'));
    expect(onChange).toHaveBeenCalledWith(true);
  });

  it('fires onChange(false) when clicked while on', async () => {
    const onChange = vi.fn();
    const { getByRole } = render(Toggle, { props: { on: true, onChange } });
    await userEvent.click(getByRole('button'));
    expect(onChange).toHaveBeenCalledWith(false);
  });

  it('ignores clicks while disabled', async () => {
    const onChange = vi.fn();
    const { getByRole } = render(Toggle, { props: { disabled: true, onChange } });
    await userEvent.click(getByRole('button'));
    expect(onChange).not.toHaveBeenCalled();
  });

  it('applies the compact class', () => {
    const { container } = render(Toggle, { props: { compact: true } });
    expect(container.querySelector('.toggle')).toHaveClass('toggle-compact');
  });
});
