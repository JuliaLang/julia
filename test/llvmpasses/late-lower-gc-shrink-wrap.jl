# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for GC frame shrink-wrapping in LateLowerGCFrame: the shadow-stack
# frame push/pop (and the memset that zero-inits it) should be sunk into the
# sub-CFG that actually needs GC roots, while the backing alloca stays in the
# entry block (static alloca, bracketed by lifetime.start/end).

# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# COM: Test 1: early-exit pattern. The fast path (x < 0) returns 0 without
# COM: rooting anything; the slow path holds a Ref{Any} live across two
# COM: calls. The alloca stays at entry but the push/memset/lifetime land
# COM: inside the slow-path block.
@noinline _step1(r) = (r[] = r[] + 1; r)
@noinline _step2(r) = (r[] = r[] * 2; r[])
function shrinkwrap_early_exit(x::Int)
    if x < 0
        return 0
    end
    r = Ref{Any}(x)
    _step1(r)
    return _step2(r)::Int
end

# CHECK-LABEL: @julia_shrinkwrap_early_exit
# CHECK: top:
# COM: alloca at entry (static); entry must not push, memset, or begin lifetime
# CHECK: %gcframe{{[0-9]*}} = alloca
# CHECK-NOT: store ptr %gcframe{{[0-9]*}}, ptr %pgcstack_arg
# CHECK-NOT: call void @llvm.lifetime.start
# CHECK-NOT: call void @llvm.memset{{.*}} %gcframe
# COM: the sunk block begins lifetime, memsets, and pushes
# CHECK: L{{[0-9]+}}:
# CHECK: call void @llvm.lifetime.start{{.*}} %gcframe
# CHECK: call void @llvm.memset{{.*}} %gcframe
# CHECK: store ptr %gcframe{{[0-9]*}}, ptr %pgcstack_arg
emit(shrinkwrap_early_exit, Int)

# COM: Test 2: loop preheader. Only the rare inner branch (i % 128 == 0)
# COM: needs roots across a function call. The alloca is at entry; the
# COM: push/memset land in the loop preheader (outside the body) and the
# COM: pop on the loop-exit edge — NOT per-iteration inside the loop.
@noinline _maybe_any(i::Int) = Base.inferencebarrier(i)
@noinline _combine(x, y::Int) = (x isa Int ? x ⊻ y : y)::Int
function shrinkwrap_loop(n::Int)
    acc = 0
    for i in 1:n
        if i % 128 == 0
            a = _maybe_any(i)
            acc += _combine(a, i)
        else
            acc += i
        end
    end
    return acc
end

# CHECK-LABEL: @julia_shrinkwrap_loop
# CHECK: top:
# COM: entry is the n==0 zero-trip guard; alloca here but no push/memset
# CHECK: %gcframe{{[0-9]*}} = alloca
# CHECK-NOT: store ptr %gcframe{{[0-9]*}}, ptr %pgcstack_arg
# CHECK-NOT: call void @llvm.memset{{.*}} %gcframe
# COM: push + memset + lifetime.start in the loop preheader
# CHECK: .preheader
# CHECK: call void @llvm.lifetime.start{{.*}} %gcframe
# CHECK: call void @llvm.memset{{.*}} %gcframe
# CHECK: store ptr %gcframe{{[0-9]*}}, ptr %pgcstack_arg
# COM: pop and lifetime.end on the loop-exit edge (not inside the loop body)
# CHECK: .loopexit
# CHECK: store ptr %frame.prev{{[0-9]*}}, ptr %pgcstack_arg
# CHECK: call void @llvm.lifetime.end{{.*}} %gcframe
emit(shrinkwrap_loop, Int)

# COM: Test 3: boundscheck pattern. N-dim indexing where the frame is only
# COM: needed in the cold throw path (building BoundsError). Widely
# COM: triggered across the stdlib.
function shrinkwrap_boundscheck(A::Array{Float64,5}, i::Int, j::Int, k::Int, l::Int, m::Int)
    return A[i, j, k, l, m]
end

# CHECK-LABEL: @julia_shrinkwrap_boundscheck
# CHECK: top:
# CHECK: %gcframe{{[0-9]*}} = alloca
# CHECK-NOT: store ptr %gcframe{{[0-9]*}}, ptr %pgcstack_arg
# COM: push appears in the throw path block; that block ends in unreachable
# CHECK: call void @llvm.lifetime.start{{.*}} %gcframe
# CHECK: store ptr %gcframe{{[0-9]*}}, ptr %pgcstack_arg
# CHECK: unreachable
emit(shrinkwrap_boundscheck, Array{Float64,5}, Int, Int, Int, Int, Int)

# COM: Test 4: regression guard. When the frame IS needed on the hot path
# COM: from entry, shrink-wrap must leave the push there. The alloca is
# COM: always at entry; the push should also be in entry here.
@noinline _identity_any(x)::Any = Base.inferencebarrier(x)
function shrinkwrap_hot_path(x::Int)
    a = _identity_any(x)
    b = _identity_any(x + 1)
    return (a isa Int ? a : 0) + (b isa Int ? b : 0)
end

# CHECK-LABEL: @julia_shrinkwrap_hot_path
# CHECK: top:
# CHECK: %gcframe{{[0-9]*}} = alloca
# CHECK: store ptr %gcframe{{[0-9]*}}, ptr %pgcstack_arg
emit(shrinkwrap_hot_path, Int)

# COM: Test 5: try/catch bailout. Any runtime EH call (ijl_enter_handler /
# COM: ijl_pop_handler[_noexcept]) must disable sinking for the whole
# COM: function — otherwise jl_eh_restore_state_noexcept's gcstack assertion
# COM: fires at normal try exit. Use @noinline Any-returning helpers so a
# COM: frame actually gets allocated across calls inside the try body.
@noinline _tc_step1(r) = (r[] = r[] + 1; r)
@noinline _tc_step2(r) = (r[] = r[] * 2; r[])
function shrinkwrap_try_catch(x::Int)
    try
        r = Ref{Any}(x)
        _tc_step1(r)
        return _tc_step2(r)::Int
    catch
        return -1
    end
end

# CHECK-LABEL: @julia_shrinkwrap_try_catch
# CHECK: top:
# CHECK: %gcframe{{[0-9]*}} = alloca
# COM: function has try/catch → push must stay in entry (no sinking),
# COM: alongside the memset and lifetime.start
# CHECK: call void @llvm.lifetime.start{{.*}} %gcframe
# CHECK: call void @llvm.memset{{.*}} %gcframe
# CHECK: store ptr %gcframe{{[0-9]*}}, ptr %pgcstack_arg
# COM: and the EH entry call is in this function (confirms we exercised the
# COM: bailout path rather than missing EH detection)
# CHECK: call {{.*}}@ijl_enter_handler
emit(shrinkwrap_try_catch, Int)
