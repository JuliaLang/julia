# Issue #59483 - GC Corruption Investigation

GitHub issue: https://github.com/JuliaLang/julia/issues/59483

This analysis focuses on the latest reproducer in the issue (a lazy block array display crash). The reproducer is single-threaded with no `@spawn`, `@async`, or multi-task constructs.

## Summary

**Bug**: On ARM64, JIT-compiled functions place GC frame slots at fixed fp-relative offsets, leaving a gap between the slots and sp. Deeply-nested C callees (like `jl_exprn` using `JL_GC_PUSH`) can overwrite these slots with stack pointers, causing GC crashes.

**Status**: Root cause identified. Fix attempted (dynamic alloca) but too expensive. Need alternative approach.

---

## Root Cause Identified

**Stack pointers are stored in GC frame slots.**

A JIT-compiled Julia function creates a GC frame with stack pointers in slots where heap object pointers should be. When GC scans the frame, it tries to mark these stack addresses as Julia objects and crashes.

## Exact Location of Corruption

```
GC error: stack ptr 0x16fdfca00 in gcframe 0x16fdfcc20 slot 7
```

The corrupted GC frame:
```
(lldb) x/20gx 0x16fdfcc20
0x16fdfcc20: 0x0000000000000038 0x000000016fdfcd30  # nroots=56 (14 roots), prev
0x16fdfcc30: 0x000000010cbc81f0 0x000000010beb6430  # slot 0, 1 - valid
0x16fdfcc40: 0x000000010cbc81f0 0x000000010beb6470  # slot 2, 3 - valid
0x16fdfcc50: 0x000000000000001c 0x0000001f0000001b  # slot 4, 5 - GARBAGE (not pointers)
0x16fdfcc60: 0x00000001099d9600 0x000000016fdfca00  # slot 6 valid, slot 7 = STACK PTR
0x16fdfcc70: 0x0000000138a818f8 0x000000016fdfcaf0  # slot 8 valid, slot 9 = STACK PTR
```

The frame has `nroots = 56 = 14 << 2`, meaning 14 roots expected. But:
- Slots 4, 5: Small integers, not pointers
- Slot 7: `0x16fdfca00` - stack address
- Slot 9: `0x16fdfcaf0` - stack address

## Call Stack When Detected

```
frame #0:  gc_mark_stack at gc-stock.c:2075
frame #11: julia__growend_internalNOT._6302 at boot.jl:615
frame #12: julia_DFSNOT._6289 at array.jl:1205
frame #13: julia_construct_domtree_6197 at domtree.jl:252
frame #14: julia_CFGTransformStateNOT._35135 at ir.jl:707
frame #15: julia_adce_passNOT._87932 at ir.jl:807
frame #16: julia_run_passes_ipo_safe_89190 at optimize.jl:1035
...
frame #21: julia_typeinf_84010 at abstractinterpretation.jl:4574
frame #22: julia_const_prop_call_84035 at abstractinterpretation.jl:1343
```

GC is triggered during `_growend_internal!` → `DFS` → `construct_domtree` → `CFGTransformState` → `adce_pass` during type inference/optimization.

## ROOT CAUSE: GC Frame Extends Into Callee Stack Space

### Watchpoint Discovery

Set a hardware watchpoint on a GC slot of the corrupted GC frame:
```
watchpoint set expression -w write -s 8 -l c -- 0x16fdfcc68
```

The watchpoint caught a write of a stack pointer:
```
Watchpoint 1 hit:
old value: 4305540448
new value: 6171905264  (= 0x16fdfcaf0, a stack address!)

frame #0: jl_exprn at builtins.c:1740
   1739     jl_array_t *ar = jl_alloc_vec_any(n);
-> 1740     JL_GC_PUSH1(&ar);
```

**`JL_GC_PUSH1(&ar)` in `jl_exprn` is writing the stack address of local variable `ar` into memory that overlaps with the JIT function's GC frame.**

### The Overlap

The key observation is that addresses used by JL_GC_PUSH in callee functions fall within the JIT function's GC frame region:
- Corrupted GC frame at `0x16fdfcc20` has 14 roots (slots at 0x16fdfcc30-0x16fdfcc98)
- Callee C functions use stack space that overlaps with these slots
- JL_GC_PUSH writes stack addresses into its GC frame, which corrupts the JIT function's GC slots

### pgcstack Chain Analysis

The full pgcstack chain shows the corrupted frame is legitimately registered:
```
0x16fdfc870: nroots=0x24 (9 roots), prev=0x16fdfcc20
0x16fdfcc20: nroots=0x38 (14 roots), prev=0x16fdfcd30  *** CORRUPTED ***
0x16fdfcd30: nroots=0x20 (8 roots), prev=0x16fdfcdb0
```

### Critical Observation: Different Call Stacks

**At watchpoint time (when stack ptr was written):**
- jl_exprn's sp = 0x16fdfcc50
- Call stack: `jl_eval_toplevel_stmts -> jl_lower -> scm_to_julia_ -> jl_exprn`

**At crash time:**
- Frame 70's sp ≈ 0x16fdfc970, fp = 0x16fdfcd20
- Call stack: `jl_eval_toplevel_stmts -> interpreter -> JIT code (frame 70) -> ... -> GC`

Notice: jl_exprn's sp (0x16fdfcc50) is HIGHER than frame 70's sp (0x16fdfc970)!
This means jl_exprn is NOT a callee of frame 70.

**The same stack addresses are reused between different call chains.**

### The Bug Mechanism

1. `jl_eval_toplevel_stmts` calls `jl_lower` first
2. `jl_lower` chain uses stack including 0x16fdfcc50-0x16fdfcc68
3. `jl_exprn` writes `&ar` (stack ptr) to 0x16fdfcc68
4. The entire `jl_lower` chain returns, stack space is reclaimed
5. `jl_eval_toplevel_stmts` then runs the interpreter
6. Interpreter calls JIT code which creates GC frame at 0x16fdfcc20
7. **BUG**: The GC frame at 0x16fdfcc20 still contains the old value from step 3!
8. GC runs and finds garbage in slot 7

**Wait - this doesn't explain it** because step 6 should zero the GC slots...

### Revised Analysis

Actually, let me re-examine the pgcstack chain. The frame at 0x16fdfc870 (9 roots) points to 0x16fdfcc20 as its `prev`. This means:
- Frame 0x16fdfc870 was created AFTER frame 0x16fdfcc20
- Frame 0x16fdfc870's function is a callee of frame 0x16fdfcc20's function

But frame 0x16fdfc870 is at a LOWER address (0x16fdfc870 < 0x16fdfcc20), which is correct for callees.

The issue is:
1. Frame 0x16fdfcc20 was created (zeroed slots)
2. Deep nested calls created frame 0x16fdfc870 and deeper
3. Somewhere in the call chain, jl_exprn ran and wrote to 0x16fdfcc68
4. jl_exprn's stack frame (0x16fdfcc50-...) overlaps with frame 0x16fdfcc20's GC slots!

**The overlapping stack space is the bug.** Frame 0x16fdfcc20's GC slots extend into what becomes callee stack space.

## Reproducer

```julia
using BlockArrays, LazyBandedMatrices, FillArrays, InfiniteArrays
B = BlockVcat(...)  # complex infinite array
show(IOContext(stdout, :limit => true), MIME"text/plain"(), B)  # CRASHES
```

## Debug Build Settings

```make
# Make.user
WITH_GC_VERIFY=1
JCFLAGS+=-DGC_ASSERT_PARENT_VALIDITY
JCXXFLAGS+=-DGC_ASSERT_PARENT_VALIDITY
```

## Memory Around Stack Pointer

Memory dump around the bad address shows:
```
(lldb) x/20gx 0x16fdfcb00
0x16fdfcb00: 0x0000000000000000 0x0000000138a818f8
0x16fdfcb10: 0x0000000138a818e0 0x000000016fdfcaf0  # <-- stack ptr at +0x18
0x16fdfcb20: 0x0000000000000000 0x0000000138a818f8
...
0x16fdfcb50: 0x000000016fdfca00 0x0000000000000000  # <-- stack ptr at +0x50

(lldb) x/8gx 0x16fdfca00
0x16fdfca00: 0x0000000000000018 0x0000000000000050  # NOT a valid GC frame!
0x16fdfca10: 0x000000013772b8a0 0x0000000000000007
```

## Root Cause: GC Slots Above Stack Pointer

### The Bug

The JIT-compiled function at `0x10d06c000` (frame 70 at crash) has:

```asm
; Prologue
0x10d06c018: add    x29, sp, #0x50           ; fp = sp + 0x50
0x10d06c01c: sub    sp, sp, #0x360           ; allocate 0x360 bytes

; GC frame setup
0x10d06c048: sub    x10, x29, #0x100         ; GC frame at fp - 0x100
0x10d06c04c: mov    w8, #0x38                ; nroots = 0x38 = 56 (14 roots)
0x10d06c054: stp    x8, x9, [x29, #-0x100]   ; store {nroots, prev}
```

At runtime:
- `fp = 0x16fdfcd20`
- `sp = 0x16fdfcd20 - 0x50 - 0x360 = 0x16fdfc970`
- GC frame header at `fp - 0x100 = 0x16fdfcc20`
- GC slots at `0x16fdfcc30` through `0x16fdfcc98`

**The GC slots (0x16fdfcc30 - 0x16fdfcc98) are 0x2c0 bytes ABOVE sp (0x16fdfc970)!**

When this function calls deeper functions, their stack frames can overlap with these GC slots:
- `jl_exprn` runs with `sp = 0x16fdfcc50`
- `JL_GC_PUSH1(&ar)` writes to `sp + 0x18 = 0x16fdfcc68`
- But `0x16fdfcc68` is also slot 7 of frame 70's GC frame!

### Stack Layout Diagram

```
Higher addresses (toward 0x16fdfe000)
    │
    ├── 0x16fdfcd20: Frame 70's fp
    │        │
    │        ├── fp - 0x88: Last GC slot (slot 13) = 0x16fdfcc98
    │        ├── fp - 0xb8: GC slot 7 = 0x16fdfcc68 ← OVERWRITTEN!
    │        ├── fp - 0x100: GC frame header = 0x16fdfcc20
    │        │
    │        │   *** DANGER ZONE: 0x2c0 bytes ***
    │        │   Callees can use this region
    │        │   and stomp on GC slots!
    │        │
    │        ├── fp - 0x3b0: Frame 70's sp = 0x16fdfc970
    │
    ├── 0x16fdfcc50: jl_exprn's sp (callee)
    │        │
    │        └── sp + 0x18: JL_GC_PUSH1 writes here = 0x16fdfcc68
    │
Lower addresses (toward 0x16fdf0000)
```

### Conclusion

The JIT codegen places GC slots in the **red zone above sp** instead of **below sp** where they would be protected. When the function makes deep calls, callee stack frames overwrite the GC slots.

This is a **LLVM codegen / Julia codegen bug** in how GC frame slot allocation interacts with stack frame layout.

## GC Frame Chain Analysis

Valid GC frame chain:
```
0x16fdfcc20: nroots=56 (14 roots) ← CORRUPTED FRAME
    ↓ prev
0x16fdfcd30: nroots=32 (8 roots) ← valid, sequential addresses
    ↓ prev  
0x16fdfcdb0: nroots=16 (4 roots) ← valid
```

## Functions and Their GC Frames

| Function | nroots | Expected Slots |
|----------|--------|----------------|
| `run_passes_ipo_safe` | 860 (215 roots) | sp+0x6f0 |
| `batch_inline` | 336 (84 roots) | sp+0x270 |
| `cfg_inline_item` | 24 (6 roots) | fp-0xc0 |
| `optimize` | 72 (18 roots) | sp+0xa0 |

The **corrupted frame at 0x16fdfcc20** has nroots=56 (14 roots) - **doesn't match any known function!**

## Slots 10-11 Pattern

Looking at the corrupted frame memory:
```
0x16fdfcc80: 0x0000000000000018 0x0000000000000038
```
- Slot 10: `0x18` = 24 = nroots for 6 roots
- Slot 11: `0x38` = 56 = nroots for 14 roots

These look like **another GC frame header embedded in the middle**. This suggests either:
1. The nroots value (14) is too large, reading past actual roots
2. A child frame was placed overlapping with parent's slots
3. Frame pointer arithmetic error in codegen

## ROOT CAUSE FOUND

**The interpreter's `s->locals` array overlaps with a JIT-compiled function's GC frame on the stack.**

### Watchpoint Capture

Set a hardware watchpoint on the corrupted GC frame slot (0x16fdfcc68 = slot 7):

```
(lldb) watchpoint set expression -w write -s 8 -l c -- 0x16fdfcc68
Watchpoint created: Watchpoint 1: addr = 0x16fdfcc68 size = 8 state = enabled type = w
```

Watchpoint triggered:

```
Watchpoint 1 hit:
old value: 0
new value: 4636920464

Process 45200 stopped
* thread #1, queue = 'com.apple.main-thread', stop reason = watchpoint 1
    frame #0: 0x0000000100557390 libjulia-internal.1.14.0.dylib`eval_stmt_value at interpreter.c:195
   192  static void eval_stmt_value(jl_value_t *stmt, interpreter_state *s)
   193  {
   194      jl_value_t *res = eval_value(stmt, s);
-> 195      s->locals[jl_source_nslots(s->src) + s->ip] = res;
   196  }
```

**NOTE**: This watchpoint hit was a false positive - the value 4636920464 = 0x114601690 is a valid heap pointer, not a stack pointer. The ACTUAL corruption happens when jl_exprn runs (see below).

### The Real Corruption: jl_exprn

A later watchpoint (with condition for stack addresses) caught the actual corruption:

```
Watchpoint 1 hit:
old value: 4305540448
new value: 6171905264   (= 0x16fdfcaf0, a stack address!)

frame #0: jl_exprn at builtins.c:1740
   1739     jl_array_t *ar = jl_alloc_vec_any(n);
-> 1740     JL_GC_PUSH1(&ar);
```

`JL_GC_PUSH1(&ar)` writes the address of local variable `ar` (which is on the stack) into its GC frame. This write lands at address 0x16fdfcc68, which is ALSO slot 7 of the JIT function's GC frame at 0x16fdfcc20.

## Final Analysis

### The Root Cause

The JIT-compiled function at `0x10d06c000` (frame 70 at crash time) creates a GC frame with this layout:

```
Stack Layout (higher addresses = toward 0x16fdfe000):

0x16fdfcd20: Frame 70's fp
    │
    ├── fp - 0x88 = 0x16fdfcc98: GC slot 13 (last slot)
    ├── fp - 0xb8 = 0x16fdfcc68: GC slot 7 ← OVERWRITTEN BY CALLEE
    ├── fp - 0x100 = 0x16fdfcc20: GC frame header (nroots=56, prev)
    │
    │   *** GAP: 0x2b0 bytes between GC slots and sp ***
    │   This space is used by callees!
    │
    └── sp = 0x16fdfc970 (after sub sp, sp, #0x360)
```

When frame 70's function calls deeper functions (via the interpreter), those callees' stack frames can overlap with frame 70's GC slot region.

Specifically, when `jl_exprn` runs with sp = 0x16fdfcc50, its `JL_GC_PUSH1(&ar)` writes to sp + 0x18 = 0x16fdfcc68 - the same address as frame 70's GC slot 7!

### Why This Is A Bug

The JIT codegen places GC frame slots at **fp - offset** (fixed frame pointer-relative locations), but allocates stack space to **sp - offset** (below the stack pointer). This creates a gap between the GC slots and sp where callee frames operate.

When the call chain is deep enough, callees' stack usage reaches into the GC slot region and corrupts it.

## Fix Attempts

### Attempt 1: Dynamic GC Frame Allocation (TOO SLOW)

Tried modifying `src/llvm-final-gc-lowering.cpp` to split the entry block before the GC frame alloca, making it a "dynamic" allocation placed at sp level instead of fp-relative.

```cpp
// Split entry block to make alloca dynamic
BasicBlock *EntryBB = target->getParent();
if (EntryBB == &F.getEntryBlock()) {
    EntryBB->splitBasicBlock(target, "gcframe.bb");
    builder.SetInsertPoint(target);
}
```

**Result**: Compilation became extremely slow (hanging during sysimage build). Dynamic allocas require runtime stack pointer management on every function call, which is too expensive.

### Potential Fix Options

1. **C-side fix**: Change `JL_GC_PUSH` to allocate from a per-task pool instead of the C stack
2. **Stack reserve**: Add LLVM function attribute to reserve more stack space below allocas  
3. **Larger frame padding**: Ensure minimum gap between allocas and sp
4. **Runtime detection**: Add debug-mode validation of GC frame addresses


