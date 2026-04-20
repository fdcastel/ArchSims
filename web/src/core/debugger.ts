export type DebuggerStopReason = 'none' | 'halted' | 'breakpoint' | 'runningForever';

export interface Debugger {
  cpuGetProgramCounter: () => number;
  cpuStep: () => boolean; // returns true when the CPU is halted
  instructionCount: number;
  lastStop: DebuggerStopReason;
  breakpoints: Set<number>;
}

export function createDebugger(
  cpuGetProgramCounter: () => number,
  cpuStep: () => boolean,
): Debugger {
  return {
    cpuGetProgramCounter,
    cpuStep,
    instructionCount: 0,
    lastStop: 'none',
    breakpoints: new Set(),
  };
}

export function debuggerReset(debug: Debugger): void {
  debug.instructionCount = 0;
  debug.lastStop = 'none';
  debug.breakpoints = new Set();
}

export function debuggerStep(debug: Debugger): void {
  if (debug.cpuStep()) {
    debug.lastStop = 'halted';
  } else if (debug.breakpoints.has(debug.cpuGetProgramCounter())) {
    debug.lastStop = 'breakpoint';
  } else {
    debug.lastStop = 'none';
  }
  debug.instructionCount++;
}

export function debuggerRun(debug: Debugger, maximumInstructions: number): void {
  debug.lastStop = 'none';
  const limit = debug.instructionCount + maximumInstructions;
  while (debug.lastStop === 'none') {
    debuggerStep(debug);
    if (debug.instructionCount >= limit) {
      debug.lastStop = 'runningForever';
    }
  }
}

export function debuggerSetBreakpoint(debug: Debugger, address: number): void {
  debug.breakpoints.add(address);
}

export function debuggerClearBreakpoint(debug: Debugger, address: number): void {
  debug.breakpoints.delete(address);
}
