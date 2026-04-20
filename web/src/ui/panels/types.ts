export type LampColor = 'amber' | 'green' | 'red' | 'ink';

export interface FlagSpec {
  label: string;
  sub?: string;
  on: boolean;
  color?: LampColor;
}

export type BitGroupColor = 'I' | 'R' | 'M' | 'S' | 'T';

export interface BitGroup {
  label: string;
  bits: number;
  width: number;
  subLabel: string;
  color: BitGroupColor;
}

export type OperandColor = 'amber' | 'green' | 'ink' | 'dim';

export interface OperandRow {
  key: string;
  value: string;
  hint?: string;
  color?: OperandColor;
}

export interface DisasmItem {
  addr: number;
  text: string;
  size: number;
  bytes?: number[];
  label?: string;
}

export interface Counter {
  label: string;
  value: number | string;
  color?: OperandColor;
  digits?: number;
}

export interface SpeedOption {
  label: string;
  hz: number;
}
