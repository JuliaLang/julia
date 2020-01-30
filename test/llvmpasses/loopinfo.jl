# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s
# RUN: cat %t/module.ll | opt -load libjulia%shlibext -LowerSIMDLoop -S - | FileCheck %s -check-prefix=LOWER
# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s -check-prefix=FINAL

## Notes:
# This script uses the `emit` function (defined llvmpasses.jl) to emit either
# optimized or unoptimized LLVM IR. Each function is emitted individually and
# `llvm-link` is used to create a single module that can be passed to opt.
# The order in which files are emitted and linked is important since `lit` will
# process the test cases in order.
#
# There are three different test prefixes defined:
# - `CHECK`: Checks the result of codegen
# - `LOWER`: Checks the result of -LowerSIMDLoop
# - `FINAL`: Checks the result of running the entire pipeline
include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# CHECK-LABEL: @julia_simdf_
# LOWER-LABEL: @julia_simdf_
# FINAL-LABEL: @julia_simdf_
function simdf(X)
    acc = zero(eltype(X))
    @simd for x in X
        acc += x
# CHECK: call void @julia.loopinfo_marker(), {{.*}}, !julia.loopinfo [[LOOPINFO:![0-9]+]]
# LOWER-NOT: llvm.mem.parallel_loop_access
# LOWER: fadd fast double
# LOWER-NOT: call void @julia.loopinfo_marker()
# LOWER: br {{.*}}, !llvm.loop [[LOOPID:![0-9]+]]
# FINAL: fadd fast <{{[0-9]+}} x double>
    end
    acc
end

# CHECK-LABEL: @julia_simdf2_
# LOWER-LABEL: @julia_simdf2_
function simdf2(X)
    acc = zero(eltype(X))
    @simd ivdep for x in X
        acc += x
# CHECK: call void @julia.loopinfo_marker(), {{.*}}, !julia.loopinfo [[LOOPINFO2:![0-9]+]]
# LOWER: llvm.mem.parallel_loop_access
# LOWER-NOT: call void @julia.loopinfo_marker()
# LOWER: fadd fast double
# LOWER: br {{.*}}, !llvm.loop [[LOOPID2:![0-9]+]]
    end
    acc
end

@noinline iteration(i) = (@show(i); return nothing)

# CHECK-LABEL: @julia_loop_unroll
# LOWER-LABEL: @julia_loop_unroll
# FINAL-LABEL: @julia_loop_unroll
@eval function loop_unroll(N)
    for i in 1:N
        iteration(i)
        $(Expr(:loopinfo, (Symbol("llvm.loop.unroll.count"), 3)))
# CHECK: call void @julia.loopinfo_marker(), {{.*}}, !julia.loopinfo [[LOOPINFO3:![0-9]+]]
# LOWER-NOT: call void @julia.loopinfo_marker()
# LOWER: br {{.*}}, !llvm.loop [[LOOPID3:![0-9]+]]
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL-NOT: call void @julia_iteration
# FINAL: br
    end
end

# Example from a GPU kernel where we want to unroll the outer loop
# and the inner loop is a boundschecked single iteration loop.
# The `@show` is used to bloat the loop and `X-COUNT-10:` seems
# not to be working so we duplicate the checks. FIXME LLVM8
# CHECK-LABEL: @julia_loop_unroll2
# LOWER-LABEL: @julia_loop_unroll2
# FINAL-LABEL: @julia_loop_unroll2
@eval function loop_unroll2(J, I)
    for i in 1:10
        for j in J
            1 <= j <= I && continue
            @show (i,j)
            iteration(i)
        end
        $(Expr(:loopinfo, (Symbol("llvm.loop.unroll.full"),)))
# CHECK: call void @julia.loopinfo_marker(), {{.*}}, !julia.loopinfo [[LOOPINFO4:![0-9]+]]
# LOWER-NOT: call void @julia.loopinfo_marker()
# LOWER: br {{.*}}, !llvm.loop [[LOOPID4:![0-9]+]]
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL: call void @julia_iteration
# FINAL-NOT: call void @julia_iteration
    end
end

# FINAL-LABEL: @julia_notunroll
function notunroll(J, I)
    for i in 1:10
        for j in J
            1 <= j <= I && continue
            @show (i,j)
            iteration(i)
# FINAL: call void @julia_iteration
# FINAL-NOT: call void @julia_iteration
        end
    end
end

## Check all the MD nodes
# CHECK: [[LOOPINFO]] = !{!"julia.simdloop"}
# CHECK: [[LOOPINFO2]] = !{!"julia.simdloop", !"julia.ivdep"}
# CHECK: [[LOOPINFO3]] = !{[[LOOPUNROLL:![0-9]+]]}
# CHECK: [[LOOPUNROLL]] = !{!"llvm.loop.unroll.count", i64 3}
# CHECK: [[LOOPINFO4]] = !{[[LOOPUNROLL2:![0-9]+]]}
# CHECK: [[LOOPUNROLL2]] = !{!"llvm.loop.unroll.full"}
# LOWER: [[LOOPID]] = distinct !{[[LOOPID]]}
# LOWER: [[LOOPID2]] = distinct !{[[LOOPID2]]}
# LOWER: [[LOOPID3]] = distinct !{[[LOOPID3]], [[LOOPUNROLL:![0-9]+]]}
# LOWER: [[LOOPUNROLL]] = !{!"llvm.loop.unroll.count", i64 3}
# LOWER: [[LOOPID4]] = distinct !{[[LOOPID4]], [[LOOPUNROLL2:![0-9]+]]}
# LOWER: [[LOOPUNROLL2]] = !{!"llvm.loop.unroll.full"}

# Maintaining the order is important
emit(simdf, Vector{Float64})
emit(simdf2, Vector{Float64})
emit(loop_unroll, Int64)
emit(loop_unroll2, Int64, Int64)
emit(notunroll, Int64, Int64)
