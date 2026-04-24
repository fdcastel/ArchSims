import { describe, expect, it } from 'vitest';
import { render } from '@testing-library/svelte';
import { createRawSnippet } from 'svelte';
import Etch from '@/ui/primitives/Etch.svelte';

function textSnippet(text: string) {
  return createRawSnippet(() => ({ render: () => `<span>${text}</span>` }));
}

describe('Etch', () => {
  it('renders with .etch class by default', () => {
    const { container } = render(Etch, { props: {} });
    const el = container.querySelector('.etch');
    expect(el).not.toBeNull();
    expect(el).not.toHaveClass('etch-inline');
  });

  it('adds etch-inline when inline=true', () => {
    const { container } = render(Etch, { props: { inline: true } });
    expect(container.querySelector('.etch')).toHaveClass('etch-inline');
  });

  it('renders children passed as a snippet', () => {
    const { getByText } = render(Etch, {
      props: { children: textSnippet('CONTROL') },
    });
    expect(getByText('CONTROL')).toBeInTheDocument();
  });
});
