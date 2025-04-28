import React, {
  useState,
  useEffect,
  useCallback,
  useMemo,
  useRef,
  KeyboardEvent,
} from "react";
import {
  createNeanderCpu,
  neanderStep,
  neanderReset,
  type NeanderCpu,
  NeanderInstruction,
  type Memory,
} from "../core";

import { Toaster, toaster } from "@/components/ui/toaster";

import {
  Box,
  Button,
  Heading,
  SimpleGrid,
  Stat,
  VStack,
  HStack,
  Code,
  Input,
  Grid,
  GridItem,
} from "@chakra-ui/react";

import {
  createColumnHelper,
  flexRender,
  getCoreRowModel,
  useReactTable,
  getFilteredRowModel,
  type Table,
  type Row,
} from "@tanstack/react-table";

import { useVirtualizer, type VirtualItem } from "@tanstack/react-virtual";

// Memory data type for the table
type MemoryEntry = {
  address: number;
  value: number;
  instruction: string;
  isPC: boolean;
};

// Track active editing state
type ActiveCellState = {
  address: number;
  tableType: "program" | "data";
};

// Type for virtualizer
type Virtualizer = ReturnType<typeof useVirtualizer<HTMLDivElement, Element>>;

// Utility function to get instruction mnemonic from opcode
const getInstructionMnemonic = (opcode: number): string => {
  // Mask to get just the opcode part (higher nibble)
  const maskedOpcode = opcode & 0xf0;

  switch (maskedOpcode) {
    case NeanderInstruction.Nop:
      return "NOP";
    case NeanderInstruction.Sta:
      return "STA";
    case NeanderInstruction.Lda:
      return "LDA";
    case NeanderInstruction.Add:
      return "ADD";
    case NeanderInstruction.Or:
      return "OR";
    case NeanderInstruction.And:
      return "AND";
    case NeanderInstruction.Not:
      return "NOT";
    case NeanderInstruction.Jmp:
      return "JMP";
    case NeanderInstruction.Jn:
      return "JN";
    case NeanderInstruction.Jz:
      return "JZ";
    case NeanderInstruction.Hlt:
      return "HLT";
    default:
      return "???";
  }
};

const NeanderEmulator: React.FC = () => {
  const [cpu, setCpu] = useState<NeanderCpu | null>(null);

  // Non-counting memory access functions for UI operations
  // These functions directly access memory without incrementing read/write counters
  const memoryWriteByteWithoutCounting = useCallback(
    (memory: Memory, address: number, value: number): void => {
      if (address < 0 || address >= memory.data.length) {
        throw new Error(`Memory write out of bounds at address ${address}`);
      }
      // Direct access without incrementing the counter
      memory.data[address] = value & 0xFF;
    },
    []
  );

  // Run/Stop state management
  const [isRunning, setIsRunning] = useState<boolean>(false);
  const [instructionCount, setInstructionCount] = useState<number>(0);
  const runTimeoutRef = useRef<number | null>(null);
  
  // Breakpoint state
  const [breakpoints, setBreakpoints] = useState<Set<number>>(new Set());
  
  // File input ref for memory upload
  const fileInputRef = useRef<HTMLInputElement>(null);

  // Track the active cell (which table and address)
  const [activeCell, setActiveCell] = useState<ActiveCellState>({
    address: 0,
    tableType: "program",
  });

  // Track positions for each view independently
  const [programCursorPosition, setProgramCursorPosition] = useState<number>(0);
  const [dataCursorPosition, setDataCursorPosition] = useState<number>(128);

  // Track display mode (hex or decimal)
  const [isHexMode, setIsHexMode] = useState<boolean>(true);

  // Track the values being edited (one for each tableType)
  const [programEditValue, setProgramEditValue] = useState<string>("00");
  const [dataEditValue, setDataEditValue] = useState<string>("00");

  // Track the last PC value for auto-scrolling
  const [lastPC, setLastPC] = useState<number>(0);

  // Used to force UI refreshes when memory is updated
  const [memoryVersion, setMemoryVersion] = useState<number>(0);

  // Track if this is the first load to prevent repeated initialization
  const firstLoadRef = useRef<boolean>(true);
  
  // Track if we just focused a cell (to handle text selection only on first focus)
  const justFocusedRef = useRef<boolean>(false);

  // Refs for the table containers - needed for virtualization
  const programTableRef = useRef<HTMLDivElement>(null);
  const dataTableRef = useRef<HTMLDivElement>(null);

  // Refs for input elements - needed for focus management
  const programInputRef = useRef<HTMLInputElement>(null);
  const dataInputRef = useRef<HTMLInputElement>(null);

  // Store virtualizers in refs to avoid circular dependencies
  const programVirtualizerRef = useRef<Virtualizer | null>(null);
  const dataVirtualizerRef = useRef<Virtualizer | null>(null);

  // Initialize CPU
  useEffect(() => {
    const initialCpu = createNeanderCpu();
    setCpu(initialCpu);

    // Initialize edit values
    if (initialCpu) {
      setProgramEditValue(
        initialCpu.memory.data[0].toString(16).padStart(2, "0").toLowerCase()
      );
      setDataEditValue(
        initialCpu.memory.data[128].toString(16).padStart(2, "0").toLowerCase()
      );
    }
  }, []);

  // Function to force re-render
  const updateUI = useCallback(() => {
    // First update lastPC to ensure proper scrolling
    if (cpu) {
      setLastPC(cpu.registers.programCounter);
    }
    
    setCpu((prevCpu: NeanderCpu | null) => {
      if (prevCpu) {
        // Update edit values for active cells
        if (activeCell.tableType === "program") {
          setProgramEditValue(
            prevCpu.memory.data[activeCell.address]
              .toString(16)
              .padStart(2, "0")
              .toLowerCase()
          );
        } else {
          setDataEditValue(
            prevCpu.memory.data[activeCell.address]
              .toString(16)
              .padStart(2, "0")
              .toLowerCase()
          );
        }

        // Create a deep copy of the CPU state to ensure React detects changes to nested objects
        return {
          ...prevCpu,
          registers: {
            ...prevCpu.registers,
            flags: { ...prevCpu.registers.flags }
          }
        };
      }
      return null;
    });
    
    // Force a rerender by updating memory version
    setMemoryVersion(prev => prev + 1);
  }, [activeCell, cpu]);

  // Format a value based on the current display mode (hex or decimal)
  const formatValue = useCallback(
    (value: number, padWidth: number = 2): string => {
      if (isHexMode) {
        return `0x${value.toString(16).padStart(padWidth, "0").toUpperCase()}`;
      } else {
        return value.toString();
      }
    },
    [isHexMode]
  );

  // Handle toggling between hex and decimal display mode
  const toggleDisplayMode = useCallback(() => {
    setIsHexMode((prev) => !prev);
    toaster.create({
      title: `Display mode changed to ${!isHexMode ? "Hexadecimal" : "Decimal"}`,
      type: "info",
      duration: 1500,
    });
  }, [isHexMode]);

  // Handle stepping
  const handleStep = useCallback(() => {
    if (cpu) {
      try {
        neanderStep(cpu);
        // Increment instruction counter when stepping
        setInstructionCount(prev => prev + 1);
        updateUI();
      } catch (error) {
        // Use unknown type for better type safety
        const message =
          error instanceof Error ? error.message : "An unknown error occurred";

        toaster.create({
          title: "Execution Error",
          description: message,
          type: "error",
          duration: 3000,
        });
      }
    }
  }, [cpu, updateUI]);

  // Handle resetting
  const handleReset = useCallback(() => {
    if (cpu) {
      neanderReset(cpu);
      setLastPC(0); // Reset the last PC value
      setInstructionCount(0); // Reset instruction counter
      updateUI();

      toaster.create({
        title: "CPU Reset",
        type: "info",
        duration: 1500,
      });
    }
  }, [cpu, updateUI]);

  // Handle continuous execution (Run)
  const handleRun = useCallback(() => {
    if (!cpu) return;
    
    // Clear HALTED flag if set
    if (cpu.registers.flags.halted) {
      cpu.registers.flags.halted = false;
    }
    
    // Set running state
    setIsRunning(true);
    
    // Local counter for run execution limit (separate from displayed instruction counter)
    let runInstructionCount = 0;
    
    // Define recursive run function
    const runStep = () => {
      if (!cpu) return;
      
      try {
        // Execute one step
        neanderStep(cpu);
        
        // Increment instruction counter (for display)
        setInstructionCount(prev => prev + 1);
        
        // Increment run counter (for safety limit)
        runInstructionCount++;
        
        // Critical: Update PC immediately for UI
        setLastPC(cpu.registers.programCounter);
        setMemoryVersion(prev => prev + 1);
        
        // Also update CPU state in state for the UI
        setCpu({...cpu});
        
        // Check if HALTED flag was set during this step
        if (cpu.registers.flags.halted) {
          setIsRunning(false);
          return;
        }
        
        // Check if we hit a breakpoint - use current breakpoints state, not closure captured value
        if (breakpoints.has(cpu.registers.programCounter)) {
          // Clear timeout to ensure no more executions happen
          if (runTimeoutRef.current !== null) {
            window.clearTimeout(runTimeoutRef.current);
            runTimeoutRef.current = null;
          }
          setIsRunning(false);
          
          // Show toast notification for breakpoint hit
          toaster.create({
            title: "Breakpoint Hit",
            description: `Execution paused at ${formatValue(cpu.registers.programCounter)}`,
            type: "info",
            duration: 3000,
          });
          return;
        }
        
        // Safety check: stop if we've executed too many instructions in this run
        if (runInstructionCount > 1000) {
          // Clear timeout to ensure no more executions happen
          if (runTimeoutRef.current !== null) {
            window.clearTimeout(runTimeoutRef.current);
            runTimeoutRef.current = null;
          }
          setIsRunning(false);
          
          // Show warning toast
          toaster.create({
            title: "Execution Stopped",
            description: "Maximum instruction limit (1,000) reached. Possible infinite loop detected.",
            type: "warning",
            duration: 5000,
          });
          return;
        }
        
        // Schedule next step with small delay
        runTimeoutRef.current = window.setTimeout(() => {
          // Check if we should continue (checking current state, not closure state)
          if (runTimeoutRef.current === null) return;
          runStep();
        }, 10);
      } catch (error) {
        setIsRunning(false);
        
        // Show error toast
        const message = error instanceof Error ? error.message : "An unknown error occurred";
        toaster.create({
          title: "Execution Error",
          description: message,
          type: "error",
          duration: 3000,
        });
      }
    };
    
    // Start execution
    runStep();
  }, [cpu, breakpoints, formatValue]); // Add breakpoints to dependency array

  // Handle stopping execution
  const handleStop = useCallback(() => {
    if (runTimeoutRef.current !== null) {
      window.clearTimeout(runTimeoutRef.current);
      runTimeoutRef.current = null;
    }
    
    setIsRunning(false);
    
    // Ensure UI is updated with the current PC value
    updateUI();
    
    toaster.create({
      title: "Execution Stopped",
      type: "info",
      duration: 1500,
    });
  }, [updateUI]);

  // Move to next/previous memory cell
  const moveEditingCell = useCallback(
    (direction: "up" | "down") => {
      if (!cpu) return;

      // Get the current edit value based on active table type
      const currentValue =
        activeCell.tableType === "program" ? programEditValue : dataEditValue;

      try {
        // Parse the value based on current display mode
        let numValue: number;
        if (isHexMode) {
          // In hex mode, parse as hex
          numValue = parseInt(currentValue, 16);
        } else {
          // In decimal mode, parse as decimal
          numValue = parseInt(currentValue, 10);
        }

        // Validate it's a valid byte value (0-255)
        if (!isNaN(numValue) && numValue >= 0 && numValue <= 255) {
          // Use non-counting version for UI operations
          memoryWriteByteWithoutCounting(cpu.memory, activeCell.address, numValue);
          // Increment memory version to force UI refresh
          setMemoryVersion(prev => prev + 1);
        }
      } catch (error) {
        // Handle error silently - we'll continue with navigation anyway
        console.error("Error saving cell value:", error);
      }

      // Calculate new address
      let newAddress = activeCell.address;
      if (direction === "up") {
        newAddress = (newAddress - 1 + 256) % 256; // Wrap around
      } else {
        newAddress = (newAddress + 1) % 256; // Wrap around
      }

      // Update active cell
      setActiveCell({
        address: newAddress,
        tableType: activeCell.tableType,
      });

      // Update the appropriate cursor position
      if (activeCell.tableType === "program") {
        setProgramCursorPosition(newAddress);
      } else {
        setDataCursorPosition(newAddress);
      }

      // Get the actual value from memory for the new cell (don't copy the current edit value)
      const newCellValue = cpu.memory.data[newAddress]
        .toString(isHexMode ? 16 : 10)
        .padStart(isHexMode ? 2 : 1, "0")
        .toLowerCase();

      // Update edit values with the actual memory value
      if (activeCell.tableType === "program") {
        setProgramEditValue(newCellValue);
      } else {
        setDataEditValue(newCellValue);
      }

      // Ensure the view scrolls to the new cell
      const virtualizer =
        activeCell.tableType === "program"
          ? programVirtualizerRef.current
          : dataVirtualizerRef.current;

      if (virtualizer) {
        virtualizer.scrollToIndex(newAddress, { align: "center" });
      }

      // Set flag to indicate we should select text on focus
      justFocusedRef.current = true;
      
      // Focus and select text in the appropriate input
      setTimeout(() => {
        if (activeCell.tableType === "program" && programInputRef.current) {
          programInputRef.current.focus();
          // Only select text on first focus
          if (justFocusedRef.current) {
            programInputRef.current.select();
            justFocusedRef.current = false;
          }
        } else if (activeCell.tableType === "data" && dataInputRef.current) {
          dataInputRef.current.focus();
          // Only select text on first focus
          if (justFocusedRef.current) {
            dataInputRef.current.select();
            justFocusedRef.current = false;
          }
        }
      }, 0);
    },
    [activeCell, cpu, programEditValue, dataEditValue, setMemoryVersion, isHexMode]
  );

  // Switch between program and data views
  const switchTableView = useCallback(() => {
    if (!cpu) return;

    console.log("Switching table view!", { 
      from: activeCell.tableType,
      to: activeCell.tableType === "program" ? "data" : "program" 
    });

    // Get current edit value
    const currentValue =
      activeCell.tableType === "program" ? programEditValue : dataEditValue;

    try {
      // Parse the value based on current display mode
      let numValue: number;
      if (isHexMode) {
        // In hex mode, parse as hex
        numValue = parseInt(currentValue, 16);
      } else {
        // In decimal mode, parse as decimal
        numValue = parseInt(currentValue, 10);
      }

      // Validate it's a valid byte value (0-255)
      if (!isNaN(numValue) && numValue >= 0 && numValue <= 255) {
        // Use non-counting version for UI operations
        memoryWriteByteWithoutCounting(cpu.memory, activeCell.address, numValue);
        // Increment memory version to force UI refresh
        setMemoryVersion(prev => prev + 1);
      }
    } catch (error) {
      // Handle error silently - we'll continue with navigation anyway
      console.error("Error saving cell value:", error);
    }

    // Save current cursor position for current view type
    if (activeCell.tableType === "program") {
      setProgramCursorPosition(activeCell.address);
    } else {
      setDataCursorPosition(activeCell.address);
    }

    // Switch table type
    const newTableType =
      activeCell.tableType === "program" ? "data" : "program";

    // Get address from the other view's cursor position
    const newAddress = newTableType === "program" 
      ? programCursorPosition 
      : dataCursorPosition;

    // Update active cell
    setActiveCell({
      address: newAddress,
      tableType: newTableType,
    });

    // Get the actual value from memory for the new cell (don't copy the current edit value)
    const newCellValue = cpu.memory.data[newAddress]
      .toString(isHexMode ? 16 : 10)
      .padStart(isHexMode ? 2 : 1, "0")
      .toLowerCase();

    // Update the appropriate edit value with the actual memory value
    if (newTableType === "program") {
      setProgramEditValue(newCellValue);
    } else {
      setDataEditValue(newCellValue);
    }

    // Ensure the view scrolls to the new cell
    const virtualizer =
      newTableType === "program"
        ? programVirtualizerRef.current
        : dataVirtualizerRef.current;

    if (virtualizer) {
      virtualizer.scrollToIndex(newAddress, { align: "center" });
    }

    // Set flag to indicate we should select text on focus
    justFocusedRef.current = true;
    
    // Use a slightly longer delay to ensure React has updated the DOM
    setTimeout(() => {
      console.log("Focusing input for", newTableType);
      
      const inputRef = newTableType === "program" ? programInputRef : dataInputRef;
      
      if (inputRef.current) {
        // Try multiple times to get focus, in case of React rendering delays
        inputRef.current.focus();
        
        // Also try again after a small delay
        setTimeout(() => {
          if (inputRef.current) {
            inputRef.current.focus();
            if (justFocusedRef.current) {
              inputRef.current.select();
              justFocusedRef.current = false;
            }
          }
        }, 10);
      }
    }, 50);
  }, [activeCell, cpu, programEditValue, dataEditValue, setMemoryVersion, programCursorPosition, dataCursorPosition, isHexMode]);

  // Handle keyboard navigation
  const handleKeyNavigation = useCallback(
    (e: KeyboardEvent<HTMLInputElement>) => {
      if (!cpu) return;

      if (e.key === "ArrowUp") {
        e.preventDefault(); // Prevent default scrolling
        moveEditingCell("up");
      } else if (e.key === "ArrowDown" || e.key === "Enter") {
        e.preventDefault(); // Prevent default scrolling
        moveEditingCell("down");
      }
      // No more Escape key handler - always in edit mode
    },
    [cpu, moveEditingCell]
  );

  // Handle click on a cell to make it active
  const handleCellClick = useCallback(
    (address: number, tableType: "program" | "data") => {
      if (!cpu) return;

      // Get current edit value from the active cell
      const currentValue =
        activeCell.tableType === "program" ? programEditValue : dataEditValue;

      try {
        // Parse the value based on current display mode
        let numValue: number;
        if (isHexMode) {
          // In hex mode, parse as hex
          numValue = parseInt(currentValue, 16);
        } else {
          // In decimal mode, parse as decimal
          numValue = parseInt(currentValue, 10);
        }

        // Validate it's a valid byte value (0-255)
        if (!isNaN(numValue) && numValue >= 0 && numValue <= 255) {
          // Use non-counting version for UI operations
          memoryWriteByteWithoutCounting(cpu.memory, activeCell.address, numValue);
          // Increment memory version to force UI refresh
          setMemoryVersion(prev => prev + 1);
        }
      } catch (error) {
        // Handle error silently - we'll continue with navigation anyway
        console.error("Error saving cell value:", error);
      }

      // Only update if we're changing cells
      if (address !== activeCell.address || tableType !== activeCell.tableType) {
        // Update cursor position for the appropriate view
        if (tableType === "program") {
          setProgramCursorPosition(address);
        } else {
          setDataCursorPosition(address);
        }

        // Set the new active cell
        setActiveCell({
          address,
          tableType,
        });

        // Get the actual value from memory for the new cell (don't copy the current edit value)
        const newCellValue = cpu.memory.data[address]
          .toString(16)
          .padStart(2, "0")
          .toLowerCase();

        // Update the appropriate edit value with the actual memory value
        if (tableType === "program") {
          setProgramEditValue(newCellValue);
        } else {
          setDataEditValue(newCellValue);
        }

        // Set flag to indicate we should select text on focus
        justFocusedRef.current = true;
        
        // Focus the appropriate input
        setTimeout(() => {
          if (tableType === "program" && programInputRef.current) {
            programInputRef.current.focus();
            // Only select text on first focus
            if (justFocusedRef.current) {
              programInputRef.current.select();
              justFocusedRef.current = false;
            }
          } else if (tableType === "data" && dataInputRef.current) {
            dataInputRef.current.focus();
            // Only select text on first focus
            if (justFocusedRef.current) {
              dataInputRef.current.select();
              justFocusedRef.current = false;
            }
          }
        }, 0);
      }
    },
    [activeCell, cpu, programEditValue, dataEditValue, setMemoryVersion, isHexMode]
  );

  // Handle setting Program Counter to a specific address
  const handleSetProgramCounter = useCallback(
    (address: number) => {
      if (!cpu) return;
      
      // Update the Program Counter in the CPU
      cpu.registers.programCounter = address;
      
      // Update UI to reflect the change
      setLastPC(address);
      
      // Force a UI refresh
      setMemoryVersion(prev => prev + 1);
      
      toaster.create({
        title: `PC set to ${formatValue(address)}`,
        type: "info",
        duration: 1000,
      });
    },
    [cpu, formatValue]
  );
  
  // Toggle breakpoint at a specific address
  const toggleBreakpoint = useCallback(
    (address: number) => {
      setBreakpoints(prev => {
        const newBreakpoints = new Set(prev);
        if (newBreakpoints.has(address)) {
          newBreakpoints.delete(address);
          
          toaster.create({
            title: `Breakpoint removed at ${formatValue(address)}`,
            type: "info",
            duration: 1000,
          });
        } else {
          newBreakpoints.add(address);
          
          toaster.create({
            title: `Breakpoint set at ${formatValue(address)}`,
            type: "info",
            duration: 1000,
          });
        }
        return newBreakpoints;
      });
    },
    [formatValue]
  );

  // Handle memory download (save to JSON file)
  const handleMemoryDownload = useCallback(() => {
    if (!cpu) return;
    
    try {
      // Create a JSON representation of memory
      const memoryData = {
        memory: Array.from(cpu.memory.data)
      };
      
      // Convert to JSON string
      const jsonString = JSON.stringify(memoryData, null, 2);
      
      // Create a blob from the JSON string
      const blob = new Blob([jsonString], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      
      // Create a temporary link and trigger download
      const downloadLink = document.createElement('a');
      downloadLink.href = url;
      downloadLink.download = 'neander-memory.json';
      downloadLink.click();
      
      // Clean up
      URL.revokeObjectURL(url);
      
      toaster.create({
        title: "Memory Saved",
        description: "Memory data saved to neander-memory.json",
        type: "success",
        duration: 3000,
      });
    } catch (error) {
      const message = error instanceof Error ? error.message : "An unknown error occurred";
      toaster.create({
        title: "Memory Download Error",
        description: message,
        type: "error",
        duration: 3000,
      });
    }
  }, [cpu]);
  
  // Handle memory upload (load from JSON file)
  const handleMemoryUpload = useCallback(() => {
    // Trigger the hidden file input
    if (fileInputRef.current) {
      fileInputRef.current.click();
    }
  }, []);
  
  // Process the selected file for upload
  const handleFileChange = useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    if (!cpu || !event.target.files || event.target.files.length === 0) return;
    
    const file = event.target.files[0];
    const reader = new FileReader();
    
    reader.onload = (e) => {
      try {
        if (!e.target || typeof e.target.result !== 'string') {
          throw new Error("Failed to read file");
        }
        
        // Parse the JSON data
        const memoryData = JSON.parse(e.target.result);
        
        // Validate the file format
        if (!memoryData.memory || !Array.isArray(memoryData.memory) || memoryData.memory.length !== 256) {
          throw new Error("Invalid memory data format");
        }
        
        // Update CPU memory
        for (let i = 0; i < 256; i++) {
          const value = memoryData.memory[i];
          if (typeof value !== 'number' || value < 0 || value > 255 || !Number.isInteger(value)) {
            throw new Error(`Invalid memory value at address ${i}`);
          }
          // Use non-counting version for UI operations
          memoryWriteByteWithoutCounting(cpu.memory, i, value);
        }
        
        // Reset memory read/write counters
        cpu.memory.readCount = 0;
        cpu.memory.writeCount = 0;
        
        // Update UI
        updateUI();
        
        toaster.create({
          title: "Memory Loaded",
          description: `Loaded memory data from ${file.name}`,
          type: "success",
          duration: 3000,
        });
      } catch (error) {
        const message = error instanceof Error ? error.message : "Failed to load memory data";
        toaster.create({
          title: "Memory Upload Error",
          description: message,
          type: "error",
          duration: 3000,
        });
      } finally {
        // Reset the file input so the same file can be selected again
        if (event.target) {
          event.target.value = '';
        }
      }
    };
    
    reader.onerror = () => {
      toaster.create({
        title: "Memory Upload Error",
        description: "Failed to read the file",
        type: "error",
        duration: 3000,
      });
      
      // Reset the file input
      if (event.target) {
        event.target.value = '';
      }
    };
    
    // Read the file as text
    reader.readAsText(file);
  }, [cpu, updateUI]);

  // TanStack Table setup
  const data = useMemo(() => {
    if (!cpu) return [];

    return Array.from(cpu.memory.data).map((value, index) => ({
      address: index,
      value: value,
      instruction: getInstructionMnemonic(value),
      isPC: index === cpu.registers.programCounter,
    }));
  }, [cpu, memoryVersion]); // Add memoryVersion dependency to refresh on memory changes

  const columnHelper = createColumnHelper<MemoryEntry>();

  // Program view columns (with instruction opcodes)
  const programColumns = useMemo(
    () => [
      // Breakpoint column
      columnHelper.accessor("address", {
        header: "BP",
        cell: (info) => {
          const address = info.getValue();
          const hasBreakpoint = breakpoints.has(address);
          
          return (
            <span
              onClick={(e) => {
                e.stopPropagation(); // Prevent triggering cell click
                toggleBreakpoint(address);
              }}
              style={{
                cursor: "pointer",
                fontFamily: "monospace",
                fontSize: "14px",
                color: hasBreakpoint ? "red" : "#E2E8F0",
                fontWeight: "bold",
                width: "100%",
                display: "flex",
                justifyContent: "center",
              }}
              title={hasBreakpoint ? "Remove breakpoint" : "Set breakpoint"}
            >
              {hasBreakpoint ? "●" : "○"}
            </span>
          );
        },
        size: 30,
        id: "breakpoint" // Custom ID to distinguish from other address accessors
      }),
      columnHelper.accessor("isPC", {
        header: "PC",
        cell: (info) => {
          const address = info.row.original.address;
          const isCurrentPC = info.getValue();
          
          return (
            <span
              onClick={() => handleSetProgramCounter(address)}
              style={{
                cursor: "pointer",
                fontFamily: "monospace",
                fontSize: "14px",
                color: isCurrentPC ? "#3182CE" : "#E2E8F0",
                fontWeight: isCurrentPC ? "bold" : "normal",
                width: "100%",
                display: "flex",
                justifyContent: "center",
              }}
              title={`Set Program Counter to ${formatValue(address)}`}
            >
              {isCurrentPC ? "➤" : "○"}
            </span>
          );
        },
        size: 30,
      }),
      columnHelper.accessor("address", {
        header: "Addr",
        cell: (info) => formatValue(info.getValue(), 2),
        size: 60,
      }),
      columnHelper.accessor("value", {
        header: "Data",
        cell: (info) => {
          const address = info.row.original.address;
          const value = info.getValue();

          // Always show input for active address
          if (
            activeCell.tableType === "program" &&
            activeCell.address === address
          ) {
            return (
              <Input
                ref={programInputRef}
                size="xs"
                width="40px"
                value={programEditValue}
                autoFocus
                // Only select on initial render, not on every focus event
                onChange={(e) => setProgramEditValue(e.target.value)}
                onKeyDown={handleKeyNavigation}
              />
            );
          }

          // Otherwise show clickable value
          return (
            <span
              onClick={() => handleCellClick(address, "program")}
              style={{
                cursor: "pointer",
                padding: "2px 4px",
                border: "1px solid transparent",
                borderRadius: "2px",
                background:
                  address === cpu?.registers.programCounter
                    ? "#ebf8ff"
                    : "transparent",
              }}
            >
              {formatValue(value)}
            </span>
          );
        },
        size: 60,
      }),
      columnHelper.accessor("instruction", {
        header: "Op",
        cell: (info) => info.getValue(),
        size: 60,
      }),
    ],
    [activeCell, programEditValue, cpu, handleKeyNavigation, handleCellClick, formatValue, handleSetProgramCounter, breakpoints, toggleBreakpoint]
  );

  // Data view columns (without instruction opcodes)
  const dataColumns = useMemo(
    () => [
      columnHelper.accessor("address", {
        header: "Addr",
        cell: (info) => formatValue(info.getValue(), 2),
        size: 60,
      }),
      columnHelper.accessor("value", {
        header: "Data",
        cell: (info) => {
          const address = info.row.original.address;
          const value = info.getValue();

          // Always show input for active address
          if (
            activeCell.tableType === "data" &&
            activeCell.address === address
          ) {
            return (
              <Input
                ref={dataInputRef}
                size="xs"
                width="40px"
                value={dataEditValue}
                autoFocus
                // Only select on initial render, not on every focus event
                onChange={(e) => setDataEditValue(e.target.value)}
                onKeyDown={handleKeyNavigation}
              />
            );
          }

          // Otherwise show clickable value
          return (
            <span
              onClick={() => handleCellClick(address, "data")}
              style={{
                cursor: "pointer",
                padding: "2px 4px",
                border: "1px solid transparent",
                borderRadius: "2px",
                background:
                  address === cpu?.registers.programCounter
                    ? "#ebf8ff"
                    : "transparent",
                fontFamily: "monospace",
              }}
            >
              {formatValue(value)}
            </span>
          );
        },
        size: 60,
      }),
    ],
    [activeCell, dataEditValue, cpu, handleKeyNavigation, handleCellClick, formatValue]
  );

  // Configure tables
  const programTable = useReactTable({
    data,
    columns: programColumns,
    getCoreRowModel: getCoreRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
  });

  const dataTable = useReactTable({
    data,
    columns: dataColumns,
    getCoreRowModel: getCoreRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
  });

  // Set up virtualization for program table
  const programRows = programTable.getRowModel().rows;

  const programVirtualizer = useVirtualizer({
    count: programRows.length,
    getScrollElement: () => programTableRef.current,
    estimateSize: () => 35, // approximate row height
    overscan: 10, // number of items to render before/after the visible area
  });

  // Store virtualizer in ref for access from callbacks
  programVirtualizerRef.current = programVirtualizer;

  // Set up virtualization for data table
  const dataRows = dataTable.getRowModel().rows;

  const dataVirtualizer = useVirtualizer({
    count: dataRows.length,
    getScrollElement: () => dataTableRef.current,
    estimateSize: () => 35, // approximate row height
    overscan: 10, // number of items to render before/after the visible area
  });

  // Store virtualizer in ref for access from callbacks
  dataVirtualizerRef.current = dataVirtualizer;

  // Calculate virtualization metrics for program table
  const programVirtualRows = programVirtualizer.getVirtualItems();
  const programTotalSize = programVirtualizer.getTotalSize();
  const programPaddingTop =
    programVirtualRows.length > 0 ? programVirtualRows[0].start || 0 : 0;
  const programPaddingBottom =
    programVirtualRows.length > 0
      ? programTotalSize - (programVirtualRows[programVirtualRows.length - 1].end || 0)
      : 0;

  // Calculate virtualization metrics for data table
  const dataVirtualRows = dataVirtualizer.getVirtualItems();
  const dataTotalSize = dataVirtualizer.getTotalSize();
  const dataPaddingTop =
    dataVirtualRows.length > 0 ? dataVirtualRows[0].start || 0 : 0;
  const dataPaddingBottom =
    dataVirtualRows.length > 0
      ? dataTotalSize - (dataVirtualRows[dataVirtualRows.length - 1].end || 0)
      : 0;

  // Auto-scroll to PC when it changes (only in program view)
  useEffect(() => {
    if (cpu && programVirtualizerRef.current && lastPC !== undefined) {
      // Only auto-scroll if not actively editing
      if (
        activeCell.tableType !== "program" ||
        activeCell.address !== cpu.registers.programCounter
      ) {
        programVirtualizerRef.current.scrollToIndex(
          cpu.registers.programCounter,
          {
            align: "center",
            behavior: "smooth",
          }
        );
      }
    }
  }, [lastPC, cpu, activeCell]);

  // Initial scroll to address 128 for the data view (once on load)
  useEffect(() => {
    // Only run this initialization once
    if (dataVirtualizerRef.current && cpu && firstLoadRef.current) {
      firstLoadRef.current = false; // Mark initialization as done

      // Need to delay the initial scroll slightly to ensure the virtualization is ready
      setTimeout(() => {
        dataVirtualizerRef.current?.scrollToIndex(128, { align: "center" });

        // Set initial active cells
        setActiveCell({
          address: 0,
          tableType: "program",
        });

        // Set initial edit values
        setProgramEditValue(
          cpu.memory.data[0].toString(16).padStart(2, "0").toLowerCase()
        );
        setDataEditValue(
          cpu.memory.data[128].toString(16).padStart(2, "0").toLowerCase()
        );

        // Focus the program input
        setTimeout(() => {
          if (programInputRef.current) {
            programInputRef.current.focus();
            programInputRef.current.select();
          }
        }, 100);
      }, 100);
    }
  }, [cpu]);

  // Add keyboard shortcuts for global app control
  useEffect(() => {
    const handleKeyDown = (e: globalThis.KeyboardEvent) => {
      if (!cpu) return;
      
      // F10 = step (like VSCode)
      if (e.key === "F10") {
        e.preventDefault();
        handleStep();
        return;
      }
      
      // F5 = toggle run/stop
      if (e.key === "F5") {
        e.preventDefault();
        if (isRunning) {
          handleStop();
        } else if (!cpu.registers.flags.halted) {
          handleRun();
        }
        return;
      }

      // Ctrl+R = reset
      if (e.key === "r" && e.ctrlKey) {
        e.preventDefault();
        handleReset();
        return;
      }

      // Ctrl+D = switch between views (Data/Program)
      if (e.key === "d" && e.ctrlKey) {
        e.preventDefault();
        switchTableView();
        return;
      }
      
      // Ctrl+H = toggle hex/decimal display mode
      if (e.key === "h" && e.ctrlKey) {
        e.preventDefault();
        toggleDisplayMode();
        return;
      }
    };

    window.addEventListener("keydown", handleKeyDown);
    return () => window.removeEventListener("keydown", handleKeyDown);
  }, [handleStep, handleRun, handleStop, handleReset, switchTableView, toggleDisplayMode, cpu, isRunning]);

  if (!cpu) {
    return <Box>Loading Neander CPU...</Box>;
  }

  // Create a reusable table render function with fixed typing issues
  const renderMemoryTable = (
    tableRef: React.MutableRefObject<HTMLDivElement | null>,
    table: Table<MemoryEntry>,
    rows: Row<MemoryEntry>[],

    virtualRows: VirtualItem[],
    paddingTop: number,
    paddingBottom: number,
    columns: typeof programColumns | typeof dataColumns,
    title: string,
    currentAddress: number
  ) => (
    <Box>
      <HStack justify="space-between" mb={2}>
        <Heading size="sm">{title}</Heading>
        <Box fontFamily="monospace" fontSize="sm">
          Address: <Code>0x{currentAddress.toString(16).padStart(2, "0").toUpperCase()}</Code>
        </Box>
      </HStack>
      <Box
        ref={tableRef}
        borderWidth="1px"
        borderRadius="md"
        height="30em"
        overflow="auto"
        position="relative"
      >
        <table style={{ width: "100%", borderCollapse: "collapse" }}>
          <thead
            style={{
              position: "sticky",
              top: 0,
              background: "white",
              zIndex: 1,
            }}
          >
            {table.getHeaderGroups().map((headerGroup) => (
              <tr key={headerGroup.id}>
                {headerGroup.headers.map((header) => (
                  <th
                    key={header.id}
                    style={{
                      border: "1px solid #e2e8f0",
                      padding: "8px",
                      textAlign: header.id.includes("address")
                        ? "right"
                        : "left",
                      width: header.getSize(),
                    }}
                  >
                    {flexRender(
                      header.column.columnDef.header,
                      header.getContext()
                    )}
                  </th>
                ))}
              </tr>
            ))}
          </thead>
          <tbody>
            {paddingTop > 0 && (
              <tr>
                <td
                  style={{ height: `${paddingTop}px` }}
                  colSpan={columns.length}
                />
              </tr>
            )}
            {virtualRows.map((virtualRow) => {
              const row = rows[virtualRow.index];
              return (
                <tr
                  key={row.id}
                  style={{
                    backgroundColor: row.original.isPC
                      ? "#ebf8ff"
                      : "transparent",
                    height: `${virtualRow.size}px`,
                  }}
                >
                  {row.getVisibleCells().map((cell) => (
                    <td
                      key={cell.id}
                      style={{
                        border: "1px solid #e2e8f0",
                        padding: "6px",
                        textAlign: cell.column.id.includes("address")
                          ? "right"
                          : "left",
                        fontFamily: "monospace",
                      }}
                    >
                      {flexRender(
                        cell.column.columnDef.cell,
                        cell.getContext()
                      )}
                    </td>
                  ))}
                </tr>
              );
            })}
            {paddingBottom > 0 && (
              <tr>
                <td
                  style={{ height: `${paddingBottom}px` }}
                  colSpan={columns.length}
                />
              </tr>
            )}
          </tbody>
        </table>
      </Box>
    </Box>
  );

  // --- Render UI ---
  return (
    <>
      <Toaster />
      <VStack 
        gap={4} 
        align="stretch" 
        p={4}
        style={isRunning ? {
          background: "linear-gradient(45deg, rgba(76, 175, 80, 0.15) 0%, rgba(76, 175, 80, 0.05) 50%, rgba(76, 175, 80, 0.15) 100%)",
          backgroundSize: "400% 400%",
          animation: "gradientBg 3s ease infinite",
          borderWidth: "1px",
          borderStyle: "solid",
          borderColor: "rgba(76, 175, 80, 0.3)",
          borderRadius: "md",
        } : {}}
      >
        <Heading size="lg">Neander Emulator</Heading>

        {/* Controls */}
        <HStack gap={4}>
          <Button onClick={handleStep} colorPalette="blue" disabled={isRunning}>
            Step (F10)
          </Button>
          <Button 
            onClick={isRunning ? handleStop : handleRun} 
            colorPalette={isRunning ? "red" : "green"}
            disabled={!isRunning && cpu.registers.flags.halted}
          >
            {isRunning ? "Stop" : "Run"} (F5)
          </Button>
          <Button onClick={handleReset} colorPalette="orange" disabled={isRunning}>
            Reset (Ctrl+R)
          </Button>
          <Button onClick={toggleDisplayMode} colorPalette="purple" disabled={isRunning}>
            {isHexMode ? "Decimal" : "Hex"} (Ctrl+H)
          </Button>
          <Button onClick={handleMemoryDownload} colorPalette="teal" disabled={isRunning}>
            Save Memory
          </Button>
          <Button onClick={handleMemoryUpload} colorPalette="pink" disabled={isRunning}>
            Load Memory
          </Button>
          
          {/* Running status indicator */}
          {isRunning && (
            <Box 
              display="flex" 
              alignItems="center" 
              ml="auto" 
              px={3} 
              py={1} 
              borderRadius="md" 
              bg="green.100" 
              color="green.700"
              animation="pulse 1.5s infinite ease-in-out"
            >
              <Box 
                as="span" 
                w="10px" 
                h="10px" 
                borderRadius="50%" 
                bg="green.500" 
                mr={2} 
                animation="blink 1s infinite"
              />
              <Box as="span" fontWeight="bold">Program Running...</Box>
            </Box>
          )}
        </HStack>

        {/* Add animations to the global styles */}
        <Box as="style" dangerouslySetInnerHTML={{ __html: `

          @keyframes pulse {
            0% { opacity: 0.8; }
            50% { opacity: 1; }
            100% { opacity: 0.8; }
          }
          @keyframes blink {
            0% { opacity: 0.6; }
            50% { opacity: 1; }
            100% { opacity: 0.6; }
          }
          @keyframes gradientBg {
            0% { background-position: 0% 50%; }
            50% { background-position: 100% 50%; }
            100% { background-position: 0% 50%; }
          }
        `}} />

        {/* Instructions for keyboard navigation */}
        <Box fontSize="sm" color="gray.600">
          <strong>Keyboard shortcuts:</strong> F10 = Step, F5 = Run/Stop toggle, Ctrl+R = Reset, Arrow
          Up/Down or Enter = Navigate memory, Ctrl+D = Switch between views, Ctrl+H = Toggle Hex/Dec
        </Box>

        {/* Registers & Status */}
        <Stat.Root borderWidth="1px" borderRadius="lg" p={4}>
          <SimpleGrid columns={{ base: 1, md: 5 }} gap={4}>
            {/* Stat 1: PC */}
            <Box>
              <Stat.Label>PC (Program Counter)</Stat.Label>
              <Stat.ValueText fontFamily="monospace">
                {formatValue(cpu.registers.programCounter)}
              </Stat.ValueText>
            </Box>
            {/* Stat 2: AC */}
            <Box>
              <Stat.Label>AC (Accumulator)</Stat.Label>
              <Stat.ValueText fontFamily="monospace">
                {formatValue(cpu.registers.accumulator)}
              </Stat.ValueText>
            </Box>
            {/* Stat 3: Instruction Register */}
            <Box>
              <Stat.Label>IR (Instruction Register)</Stat.Label>
              <Stat.ValueText fontFamily="monospace">
                {formatValue(cpu.registers.instructionRegister.opCode)} 
                <Box as="span" fontSize="sm" color="gray.600" ml={2}>
                  {getInstructionMnemonic(cpu.registers.instructionRegister.opCode)}
                </Box>
              </Stat.ValueText>
            </Box>
            {/* Stat 4: Flags */}
            <Box>
              <Stat.Label>Status Flags</Stat.Label>
              <Stat.ValueText>
                <HStack>
                  <Code
                    colorPalette={cpu.registers.flags.negative ? "red" : "gray"}
                  >
                    N
                  </Code>
                  <Code
                    colorPalette={cpu.registers.flags.zero ? "green" : "gray"}
                  >
                    Z
                  </Code>
                  <Code
                    colorPalette={cpu.registers.flags.halted ? "yellow" : "gray"}
                  >
                    H
                  </Code>
                </HStack>
              </Stat.ValueText>
            </Box>
            {/* Stat 5: Memory Counters */}
            <Box>
              <Stat.Label>Memory Counters</Stat.Label>
              <Stat.ValueText fontFamily="monospace">
                <HStack gap={2} align="center">
                  <Box title="Memory reads">
                    <Box as="span" fontSize="sm" color="blue.500" mr={1}>R:</Box>
                    {cpu.memory.readCount}
                  </Box>
                  <Box title="Memory writes">
                    <Box as="span" fontSize="sm" color="green.500" mr={1}>W:</Box>
                    {cpu.memory.writeCount}
                  </Box>
                  <Box title="Instructions executed">
                    <Box as="span" fontSize="sm" color="purple.500" mr={1}>I:</Box>
                    {instructionCount}
                  </Box>
                </HStack>
              </Stat.ValueText>
            </Box>
          </SimpleGrid>
        </Stat.Root>

        {/* Memory Views - Side by Side */}
        <Grid templateColumns="repeat(2, 1fr)" gap={4}>
          {/* Left column: Instructions memory view */}
          <GridItem>
            {renderMemoryTable(
              programTableRef,
              programTable,
              programRows,
              programVirtualRows,
              programPaddingTop,
              programPaddingBottom,
              programColumns,
              "Instructions View",
              programCursorPosition
            )}
          </GridItem>

          {/* Right column: Data memory view */}
          <GridItem>
            {renderMemoryTable(
              dataTableRef,
              dataTable,
              dataRows,
              dataVirtualRows,
              dataPaddingTop,
              dataPaddingBottom,
              dataColumns,
              "Data View",
              dataCursorPosition
            )}
          </GridItem>
        </Grid>
      </VStack>
      {/* Hidden file input for memory upload */}
      <input
        type="file"
        ref={fileInputRef}
        style={{ display: 'none' }}
        accept=".json"
        onChange={handleFileChange}
      />
    </>
  );
};

export default NeanderEmulator;
