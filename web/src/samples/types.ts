export type SampleMachine = 'neander' | 'ahmes' | 'ramses' | 'cesar';

export interface Sample {
  id: string;
  name: string;
  description: string;
  machine: SampleMachine;
  sourceText?: string;
  bytes: Uint8Array;
  addrToLine?: ReadonlyMap<number, number>;
  labels?: ReadonlyMap<string, number>;
}
