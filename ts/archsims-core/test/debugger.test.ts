// ts/archsims-core/test/debugger.test.ts

import { describe, it, expect, beforeEach } from 'vitest';
import {
    Debugger,
    DebuggerStopReason,
    createDebugger,
    debuggerReset,
    debuggerStep,
    debuggerRun,
    debuggerSetBreakpoint,
    debuggerClearBreakpoint
} from '../src/debugger';
import {
    NeanderCpu,
    createNeanderCpu,
    neanderReset,
    neanderStep,
    NeanderInstruction
} from '../src/neander';

// Helper to assert debugger state matches expectations
interface DebuggerStateCheck {
    instructionCount?: number;
    lastStop?: DebuggerStopReason;
}

function assertDebuggerState(dbg: Debugger, checks: DebuggerStateCheck): void {
    if (checks.instructionCount !== undefined) {
        expect(dbg.instructionCount, 'Instruction count').toBe(checks.instructionCount);
    }
    if (checks.lastStop !== undefined) {
        expect(dbg.lastStop, 'Last stop reason').toBe(checks.lastStop);
    }
}

describe('Debugger', () => {
    let cpu: NeanderCpu;
    let dbg: Debugger;

    // Helper function to set up the debugger with a Neander CPU
    beforeEach(() => {
        cpu = createNeanderCpu();
        dbg = createDebugger(
            // Function to get the program counter
            () => cpu.registers.programCounter,
            // Function to step the CPU and return true if halted
            () => {
                neanderStep(cpu);
                return cpu.registers.flags.halted;
            }
        );
        neanderReset(cpu);
        debuggerReset(dbg);
    });

    it('should detect when running forever', () => {
        debuggerRun(dbg, 1000);
        assertDebuggerState(dbg, {
            instructionCount: 1000,
            lastStop: DebuggerStopReason.RunningForever
        });

        debuggerRun(dbg, 500);
        assertDebuggerState(dbg, {
            instructionCount: 1500,
            lastStop: DebuggerStopReason.RunningForever
        });
    });

    it('should reset to clean state', () => {
        // Set up CPU with HLT instruction
        cpu.memory.data[0] = NeanderInstruction.Hlt;
        
        // Add a breakpoint
        debuggerSetBreakpoint(dbg, 10);
        
        // Execute one step (should hit the HLT)
        debuggerStep(dbg);
        
        assertDebuggerState(dbg, {
            instructionCount: 1,
            lastStop: DebuggerStopReason.Halted
        });
        
        // Reset debugger
        debuggerReset(dbg);
        
        assertDebuggerState(dbg, {
            instructionCount: 0,
            lastStop: DebuggerStopReason.None
        });
        
        // Check that breakpoints are cleared
        expect(dbg.breakpoints.size).toBe(0);
        expect(dbg.breakpoints.has(10)).toBe(false);
    });

    // Split the test into smaller, more focused tests
    it('should detect halted state', () => {
        // Place HLT at PC=1
        cpu.memory.data[1] = NeanderInstruction.Hlt;
        
        // Initial step (should be None since memory starts with NOP)
        debuggerStep(dbg);
        assertDebuggerState(dbg, {
            lastStop: DebuggerStopReason.None
        });
        
        // Step to HLT instruction
        debuggerStep(dbg);
        assertDebuggerState(dbg, {
            lastStop: DebuggerStopReason.Halted
        });
    });
    
    it('should detect breakpoints', () => {
        // Test a simplified breakpoint scenario
        // Use a modified CPU step function for testing
        let mockedPC = 0;
        const testDbg = createDebugger(
            () => mockedPC,
            () => {
                mockedPC++;
                return false; // Never halted
            }
        );
        
        // Set a breakpoint at address 3
        debuggerSetBreakpoint(testDbg, 3);
        
        // Step until we hit the breakpoint
        debuggerStep(testDbg); // PC becomes 1
        assertDebuggerState(testDbg, {
            lastStop: DebuggerStopReason.None
        });
        
        debuggerStep(testDbg); // PC becomes 2
        assertDebuggerState(testDbg, {
            lastStop: DebuggerStopReason.None
        });
        
        debuggerStep(testDbg); // PC becomes 3, which is our breakpoint
        assertDebuggerState(testDbg, {
            lastStop: DebuggerStopReason.Breakpoint
        });
    });

    it('should stop execution at breakpoints', () => {
        debuggerSetBreakpoint(dbg, 12);
        debuggerSetBreakpoint(dbg, 50);
        
        // Run until the first breakpoint
        debuggerRun(dbg, 1000);
        assertDebuggerState(dbg, {
            instructionCount: 12,
            lastStop: DebuggerStopReason.Breakpoint
        });
        
        // Run until the second breakpoint
        debuggerRun(dbg, 1000);
        assertDebuggerState(dbg, {
            instructionCount: 50,
            lastStop: DebuggerStopReason.Breakpoint
        });

        // Continue running, should wrap around memory and hit the first breakpoint again
        debuggerRun(dbg, 1000);
        assertDebuggerState(dbg, {
            instructionCount: 256 + 12,
            lastStop: DebuggerStopReason.Breakpoint
        });

        // Run until the second breakpoint again
        debuggerRun(dbg, 1000);
        assertDebuggerState(dbg, {
            instructionCount: 256 + 50,
            lastStop: DebuggerStopReason.Breakpoint
        });
    });

    it('should allow clearing breakpoints', () => {
        // Set and then clear a breakpoint
        debuggerSetBreakpoint(dbg, 10);
        expect(dbg.breakpoints.has(10)).toBe(true);
        
        debuggerClearBreakpoint(dbg, 10);
        expect(dbg.breakpoints.has(10)).toBe(false);
        
        // Set another breakpoint, but run to a point before it
        debuggerSetBreakpoint(dbg, 30);
        debuggerRun(dbg, 20);
        
        // Since we ran for exactly 20 steps, we should be at position 20
        assertDebuggerState(dbg, {
            instructionCount: 20,
            lastStop: DebuggerStopReason.RunningForever
        });
    });
});