export interface RunLoopHooks {
  step: () => void;
  shouldBreak?: () => boolean;
  setInterval?: (cb: () => void, ms: number) => unknown;
  clearInterval?: (handle: unknown) => void;
  requestAnimationFrame?: (cb: (ts: number) => void) => unknown;
  cancelAnimationFrame?: (handle: unknown) => void;
  maxStepsPerFrame?: number;
}

export interface RunLoop {
  start(speed: number): void;
  stop(): void;
  readonly running: boolean;
}

export function createRunLoop(hooks: RunLoopHooks): RunLoop {
  const setInt = hooks.setInterval ?? globalThis.setInterval;
  const clrInt = hooks.clearInterval ?? globalThis.clearInterval;
  const raf = hooks.requestAnimationFrame ?? globalThis.requestAnimationFrame;
  const caf = hooks.cancelAnimationFrame ?? globalThis.cancelAnimationFrame;
  const maxStepsPerFrame = hooks.maxStepsPerFrame ?? 2000;

  let handle: unknown = null;
  let kind: 'interval' | 'raf' | null = null;

  function shouldStop(): boolean {
    return hooks.shouldBreak ? hooks.shouldBreak() : false;
  }

  function stop(): void {
    if (handle === null) return;
    if (kind === 'interval') clrInt(handle as never);
    else if (kind === 'raf') caf(handle as never);
    handle = null;
    kind = null;
  }

  function startRaf(): void {
    const tick = (): void => {
      if (handle === null) return;
      for (let i = 0; i < maxStepsPerFrame; i += 1) {
        if (shouldStop()) {
          stop();
          return;
        }
        hooks.step();
      }
      handle = raf(tick);
    };
    handle = raf(tick);
    kind = 'raf';
  }

  function startInterval(speed: number): void {
    const ms = Math.max(1, Math.floor(1000 / speed));
    handle = setInt(() => {
      if (shouldStop()) {
        stop();
        return;
      }
      hooks.step();
    }, ms);
    kind = 'interval';
  }

  function start(speed: number): void {
    stop();
    if (speed <= 0) startRaf();
    else startInterval(speed);
  }

  return {
    start,
    stop,
    get running() {
      return handle !== null;
    },
  };
}
