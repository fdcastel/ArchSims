// ts/archsims-core/src/debugger.ts

/**
 * Represents the reason why the debugger stopped execution.
 */
export enum DebuggerStopReason {
    None = 'None',
    Halted = 'Halted',
    Breakpoint = 'Breakpoint',
    RunningForever = 'RunningForever'
}

/**
 * Generic debugger for CPU simulators.
 * This closely follows the F# implementation structure.
 */
export interface Debugger {
    cpuGetProgramCounter: () => number;
    cpuStep: () => boolean;  // Should return true when the CPU is halted
    instructionCount: number;
    lastStop: DebuggerStopReason;
    breakpoints: Set<number>;
}

/**
 * Creates a new debugger instance for a CPU.
 * @param cpuGetProgramCounter Function that returns the CPU's current program counter value
 * @param cpuStep Function that executes one instruction and returns true if the CPU is halted
 * @returns A new debugger instance
 */
export function createDebugger(
    cpuGetProgramCounter: () => number,
    cpuStep: () => boolean
): Debugger {
    return {
        cpuGetProgramCounter,
        cpuStep,
        instructionCount: 0,
        lastStop: DebuggerStopReason.None,
        breakpoints: new Set<number>()
    };
}

/**
 * Resets the debugger's state without affecting the attached CPU.
 * @param dbg The debugger to reset
 */
export function debuggerReset(dbg: Debugger): void {
    dbg.instructionCount = 0;
    dbg.lastStop = DebuggerStopReason.None;
    dbg.breakpoints.clear();
}

/**
 * Executes a single step and updates debugger state.
 * @param dbg The debugger to step
 */
export function debuggerStep(dbg: Debugger): void {
    // Execute one CPU instruction and check if it's now halted
    const isHalted = dbg.cpuStep();
    
    // Determine reason for stopping (if any)
    if (isHalted) {
        dbg.lastStop = DebuggerStopReason.Halted;
    } else if (dbg.breakpoints.has(dbg.cpuGetProgramCounter())) {
        dbg.lastStop = DebuggerStopReason.Breakpoint;
    } else {
        dbg.lastStop = DebuggerStopReason.None;
    }
    
    dbg.instructionCount++;
}

/**
 * Runs the CPU until a breakpoint is hit, the CPU halts, or maximumInstructions is reached.
 * @param dbg The debugger to run
 * @param maximumInstructions Maximum number of instructions to execute before stopping with RunningForever
 */
export function debuggerRun(dbg: Debugger, maximumInstructions: number): void {
    dbg.lastStop = DebuggerStopReason.None;
    const limitInstructions = dbg.instructionCount + maximumInstructions;
    
    while (dbg.lastStop === DebuggerStopReason.None) {
        debuggerStep(dbg);
        if (dbg.instructionCount >= limitInstructions) {
            dbg.lastStop = DebuggerStopReason.RunningForever;
        }
    }
}

/**
 * Adds a breakpoint at the specified address.
 * @param dbg The debugger to modify
 * @param address The memory address where execution should break
 */
export function debuggerSetBreakpoint(dbg: Debugger, address: number): void {
    dbg.breakpoints.add(address);
}

/**
 * Removes a breakpoint at the specified address.
 * @param dbg The debugger to modify
 * @param address The memory address where the breakpoint should be removed
 */
export function debuggerClearBreakpoint(dbg: Debugger, address: number): void {
    dbg.breakpoints.delete(address);
}