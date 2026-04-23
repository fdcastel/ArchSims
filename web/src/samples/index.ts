import { AhmesSubShiftSample } from './ahmes-subshift';
import { CesarBitShiftSample } from './cesar-bitshift';
import { CesarHelloSample } from './cesar-hello';
import { NeanderSumSample } from './neander-sum';
import { RamsesBitShiftSample } from './ramses-bitshift';
import type { Sample, SampleMachine } from './types';

export type { Sample, SampleMachine } from './types';

export const SAMPLES_BY_MACHINE: Record<SampleMachine, readonly Sample[]> = {
  neander: [NeanderSumSample],
  ahmes: [AhmesSubShiftSample],
  ramses: [RamsesBitShiftSample],
  cesar: [CesarBitShiftSample, CesarHelloSample],
};

export function defaultSampleFor(machine: SampleMachine): Sample {
  const list = SAMPLES_BY_MACHINE[machine];
  if (list.length === 0) {
    throw new Error(`No samples registered for machine: ${machine}`);
  }
  return list[0] as Sample;
}
