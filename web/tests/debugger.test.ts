import { describe, expect, it } from 'vitest';
import {
  createDebugger,
  debuggerClearBreakpoint,
  debuggerReset,
  debuggerRun,
  debuggerSetBreakpoint,
  debuggerStep,
} from '@/core/debugger';
import { NeanderInstruction, createNeanderCpu, neanderStep } from '@/core/neander';

const wireDebugger = (cpu = createNeanderCpu()) => {
  const debug = createDebugger(
    () => cpu.registers.programCounter,
    () => {
      neanderStep(cpu);
      return cpu.registers.flags.halted;
    },
  );
  return { cpu, debug };
};

describe('debugger', () => {
  it('detects "running forever" when the step ceiling is reached', () => {
    const { debug } = wireDebugger();
    debuggerRun(debug, 1000);
    expect(debug.lastStop).toBe('runningForever');
    expect(debug.instructionCount).toBe(1000);
    debuggerRun(debug, 500);
    expect(debug.lastStop).toBe('runningForever');
    expect(debug.instructionCount).toBe(1500);
  });

  it('reset clears counter, lastStop and breakpoints', () => {
    const { cpu, debug } = wireDebugger();
    cpu.memory.data[0] = NeanderInstruction.Hlt;
    debuggerSetBreakpoint(debug, 10);
    debuggerStep(debug);
    expect(debug.lastStop).toBe('halted');
    expect(debug.instructionCount).toBe(1);
    debuggerReset(debug);
    expect(debug.lastStop).toBe('none');
    expect(debug.instructionCount).toBe(0);
    expect(debug.breakpoints.size).toBe(0);
  });

  it('lastStop tracks halted, breakpoint, and clears between steps', () => {
    const { cpu, debug } = wireDebugger();
    debuggerStep(debug);
    expect(debug.lastStop).toBe('none');

    cpu.memory.data[1] = NeanderInstruction.Hlt;
    debuggerStep(debug);
    expect(debug.lastStop).toBe('halted');
    debuggerStep(debug);
    expect(debug.lastStop).toBe('none');

    debuggerSetBreakpoint(debug, 4);
    debuggerStep(debug);
    expect(debug.lastStop).toBe('breakpoint');
    debuggerStep(debug);
    expect(debug.lastStop).toBe('none');

    cpu.memory.data[123] = NeanderInstruction.Hlt;
    debuggerRun(debug, 1000);
    expect(debug.lastStop).toBe('halted');
    expect(debug.instructionCount).toBe(124);
  });

  it('breakpoint set/clear works as expected', () => {
    const { debug } = wireDebugger();
    debuggerSetBreakpoint(debug, 12);
    debuggerSetBreakpoint(debug, 50);
    debuggerRun(debug, 1000);
    expect(debug.lastStop).toBe('breakpoint');
    expect(debug.instructionCount).toBe(12);

    debuggerClearBreakpoint(debug, 12);
    debuggerRun(debug, 1000);
    expect(debug.lastStop).toBe('breakpoint');
    expect(debug.instructionCount).toBe(50);
  });
});
