# Neander Emulator UI Specification

This document provides comprehensive instructions for implementing a Neander CPU emulator user interface. It serves as a "clean room" specification that can be used to reconstruct the UI without referencing the original source code.

## 1. Overview

The Neander Emulator is an educational tool for simulating a simple 8-bit CPU architecture. The UI provides:

- Visual representation of CPU state (registers, flags, memory)
- Interactive memory editing
- Program execution controls (step, run, stop)
- Breakpoint management
- Memory save/load functionality
- Visualization of CPU execution

## 2. Core Architecture

### 2.1. CPU Model

The emulator is built around the Neander CPU model with the following key components:

- **Memory**: 256 bytes (8-bit addressable)
- **Registers**:
  - Program Counter (PC): 8-bit
  - Accumulator (AC): 8-bit
  - Instruction Register (IR): 8-bit opcode + 8-bit operand
- **Flags**:
  - Negative (N): Set when bit 7 of AC is 1
  - Zero (Z): Set when AC is 0
  - Halted (H): Set when HLT instruction is executed

### 2.2. Instruction Set

| Opcode (Hex) | Mnemonic | Description                   | Operand    |
|--------------|----------|-------------------------------|------------|
| 0x00         | NOP      | No operation                  | None       |
| 0x10         | STA      | Store AC to memory            | Address    |
| 0x20         | LDA      | Load memory to AC             | Address    |
| 0x30         | ADD      | Add memory to AC              | Address    |
| 0x40         | OR       | Bitwise OR memory with AC     | Address    |
| 0x50         | AND      | Bitwise AND memory with AC    | Address    |
| 0x60         | NOT      | Bitwise NOT AC                | None       |
| 0x80         | JMP      | Jump to address               | Address    |
| 0x90         | JN       | Jump if negative              | Address    |
| 0xA0         | JZ       | Jump if zero                  | Address    |
| 0xF0         | HLT      | Halt execution                | None       |

## 3. Technology Stack

- **Frontend Framework**: React
- **UI Library**: Chakra UI v3
- **Data Grid**: TanStack Table & TanStack Virtual
- **Notifications**: Custom toast component

## 4. Component Structure

### 4.1. Main Components

```
NeanderEmulator
├── Control Panel (Step, Run/Stop, Reset, etc.)
├── Status Indicator
├── Register Display
├── Memory Views
│   ├── Program View (with instruction mnemonics)
│   └── Data View
└── Hidden File Input (for memory upload)
```

### 4.2. State Management

The component maintains the following state:

- CPU state (registers, flags, memory)
- UI state (running/stopped, active cell, view mode)
- Memory views state (cursor positions, edit values)
- Breakpoints set
- Display preferences (hex/decimal)

## 5. UI Implementation

### 5.1. Control Panel

Implement a horizontal control bar with these buttons:

- **Step**: Execute a single CPU instruction
  - Blue button
  - Keyboard shortcut: F10
  - Disabled while running

- **Run/Stop Toggle**:
  - Green "Run" button when stopped, Red "Stop" button when running
  - Changes state based on execution status
  - Keyboard shortcut: F5
  - Disabled when CPU is halted (in stopped state)

- **Reset**: Reset CPU to initial state
  - Orange button
  - Keyboard shortcut: Ctrl+R
  - Disabled while running

- **Display Mode**: Toggle between hexadecimal and decimal display
  - Purple button
  - Shows "Decimal" when in hex mode, "Hex" when in decimal mode
  - Keyboard shortcut: Ctrl+H
  - Disabled while running

- **Save Memory**: Save current memory to JSON file
  - Teal button
  - Disabled while running

- **Load Memory**: Load memory from JSON file
  - Pink button
  - Disabled while running

### 5.2. Status Indicator

Show a running status indicator when the CPU is executing:

- Appears only when program is running
- Positioned on the right side of control panel
- Green background with pulsing animation
- Contains a blinking green dot and "Program Running..." text

### 5.3. Register Display

Create a status panel to display CPU registers with these elements:

- **Program Counter (PC)**: Current instruction address
- **Accumulator (AC)**: Current accumulator value
- **Instruction Register (IR)**: Current opcode with mnemonic
- **Status Flags**: 
  - N: Negative flag (red when set)
  - Z: Zero flag (green when set)
  - H: Halted flag (yellow when set)
- **Memory Counters**:
  - Read count (blue)
  - Write count (green)
  - Instruction count (purple)

### 5.4. Memory Views

Create two side-by-side memory views:

#### 5.4.1. Program View (Instructions)

- Table with these columns:
  - **BP**: Breakpoint toggle (red circle when set)
  - **PC**: Program counter indicator (blue arrow for current position)
  - **Addr**: Memory address (hex/decimal based on mode)
  - **Data**: Memory value (editable)
  - **Op**: Instruction mnemonic

#### 5.4.2. Data View

- Table with these columns:
  - **Addr**: Memory address (hex/decimal based on mode)
  - **Data**: Memory value (editable)

### 5.5. Memory Navigation

Implement these memory navigation features:

- Click on any cell to make it active and editable
- Arrow up/down or Enter to navigate between cells
- Ctrl+D to switch between Program and Data views
- Automatic scrolling to follow PC during execution
- Initial views:
  - Program view starts at address 0
  - Data view starts at address 128

### 5.6. Animations

Implement these animations for better UX:

- **Run Status**: 
  - Green gradient background animation when running
  - Container border with subtle glow
  - Pulsing opacity on status indicator

- **Keyframe Animations**:
  ```css
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
  ```

## 6. Functional Requirements

### 6.1. CPU Operations

#### 6.1.1. Step Function

- Execute a single CPU instruction
- Update UI to reflect new CPU state
- Increment instruction counter
- Handle any errors with toast messages

#### 6.1.2. Run Function

- Execute instructions continuously with small delay (10ms)
- Update UI between steps
- Check for breakpoints after each step
- Stop execution when:
  - HLT instruction is encountered
  - A breakpoint is hit
  - 1,000 instructions have executed (prevent infinite loops)
  - An error occurs
- Show appropriate toast messages

#### 6.1.3. Reset Function

- Initialize CPU to default state
- Zero all registers and memory
- Reset all counters (instruction, read, write)
- Update UI to reflect reset state

### 6.2. Memory Management

#### 6.2.1. Memory Editing

- Allow direct editing of memory cells
- Parse hex or decimal input based on current display mode
- Validate input (0-255 range)
- Update memory without incrementing read/write counters

#### 6.2.2. Memory Save/Load

- **Save**: 
  - Export memory to JSON file with format: `{ "memory": [...256 byte values...] }`
  - Download as "neander-memory.json"
  
- **Load**:
  - Accept JSON file through hidden file input
  - Validate format and values
  - Update memory and reset counters
  - Provide error handling for invalid files

### 6.3. Breakpoint Management

- Allow toggling breakpoints at any memory address
- Visual indicator for active breakpoints (red circle)
- Pause execution when PC reaches a breakpoint
- Show toast notification when breakpoint is hit

### 6.4. Virtualization

- Implement table virtualization for performance
- Render only visible rows + overscan (10 rows)
- Maintain scroll position when switching views
- Follow PC during execution (auto-scroll)

## 7. User Interaction

### 7.1. Keyboard Shortcuts

| Key      | Function                      |
|----------|-------------------------------|
| F5       | Run/Stop toggle               |
| F10      | Step                          |
| Ctrl+R   | Reset CPU                     |
| Ctrl+D   | Toggle Program/Data view      |
| Ctrl+H   | Toggle Hex/Decimal display    |
| Up Arrow | Move to previous memory cell  |
| Down/Enter | Move to next memory cell    |

### 7.2. Mouse Interactions

- **Breakpoint Column**: Click to toggle breakpoint
- **PC Column**: Click to set Program Counter
- **Memory Cell**: Click to select and edit
- **Control Buttons**: Standard button interaction

## 8. Error Handling

- Show toast messages for execution errors
- Validate memory uploads
- Gracefully handle out-of-bounds memory access
- Show warning for potential infinite loops

## 9. Performance Considerations

### 9.1. Memory Access Optimization

- Use non-counting memory access for UI operations
  - Only CPU instructions should increment read/write counters
  - Direct memory access for UI to avoid counter increments

### 9.2. Rendering Optimization

- Use virtualization for memory tables
- Memoize expensive calculations with useMemo
- Optimize re-renders with useCallback
- Update only changed parts of the UI

## 10. Implementation Guide

### 10.1. Data Structures

- **CPU State**: NeanderCpu object with registers and memory
- **Breakpoints**: Set of memory addresses
- **Memory Entry**: Object with address, value, instruction, isPC properties

### 10.2. Key Utility Functions

- `memoryWriteByteWithoutCounting`: Write to memory without incrementing counters
- `formatValue`: Format values as hex or decimal based on current mode
- `getInstructionMnemonic`: Get mnemonic string from opcode
- `updateUI`: Force UI refresh and update edit values

### 10.3. Initialization Sequence

1. Create initial CPU state
2. Set default view positions and edit values
3. Set up virtualization
4. Set initial focus on program view

### 10.4. Component Update Flow

1. Execute CPU operation
2. Update CPU state
3. Update UI representation
4. Handle any side effects (scrolling, notifications)

## 11. Testing Recommendations

- Test CPU operations against expected behavior
- Verify memory read/write counter accuracy
- Test breakpoint functionality
- Verify memory save/load functionality
- Test keyboard shortcuts and navigation
- Verify UI updates correctly reflect CPU state

---

This specification provides all the necessary details to implement a Neander CPU emulator UI from scratch. The resulting implementation should provide an educational tool for understanding how a simple CPU operates, with interactive features that make learning computer architecture engaging and intuitive.