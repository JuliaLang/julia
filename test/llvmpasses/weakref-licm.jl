# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no %s %t && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s -check-prefix=CODEGEN
# RUN: julia --startup-file=no %s %t -O && llvm-link -S %t/* -o %t/module.ll
# RUN: cat %t/module.ll | FileCheck %s -check-prefix=FINAL

## The GC may clear `WeakRef.value` at any safepoint, so the load has to be
## `monotonic` and must stay inside the loop, even though the only call in the
## loop is `:consistent` and `:effect_free` and therefore carries
## `memory(argmem: read)`. See https://github.com/JuliaLang/julia/issues/62613
##
## - `CODEGEN`: the field is loaded with `monotonic` ordering
## - `FINAL`: the optimization pipeline does not hoist that load out of the loop

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

@noinline add_weakref_value(x, y) = x + y
@noinline make_weakref(n) = WeakRef(Base.RefValue(n))

# CODEGEN-LABEL: @julia_weakref_licm
# CODEGEN: load atomic ptr addrspace(10), ptr addrspace(11) %{{[0-9]+}} monotonic

# FINAL-LABEL: @julia_weakref_licm
# COM: an `unordered` load would be hoisted into the entry block, ahead of this branch
# FINAL-NOT: load atomic ptr, ptr %{{[0-9]+}}
# FINAL: br label %[[LOOP:[a-zA-Z0-9_.]+]]
# FINAL: [[LOOP]]:
# FINAL: load atomic ptr, ptr %{{[0-9]+}} monotonic
# FINAL: br i1 {{.*}} label %[[LOOP]]
function weakref_licm(n)
    w = make_weakref(n)
    z = 0
    for i in 1:1000
        y = w.value
        if y isa Base.RefValue{Int}
            z = add_weakref_value(z, y[])
        end
    end
    return z
end
emit(weakref_licm, Int)
