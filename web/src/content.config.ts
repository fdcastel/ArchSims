import { defineCollection } from 'astro:content';
import { glob } from 'astro/loaders';

const manuals = defineCollection({
  loader: glob({ pattern: '{Neander,Ahmes,Ramses,Cesar}.md', base: '../doc' }),
});

export const collections = { manuals };
