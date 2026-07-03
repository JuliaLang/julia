# This file is a part of Julia. License is MIT: https://julialang.org/license

# RUN: julia --startup-file=no -O2 %s %t -O && llvm-link -S %t/* | FileCheck %s

include(joinpath("..", "testhelpers", "llvmpasses.jl"))

# Test that @cancel_check generates the expected cancellation lowering IR:
# - A jl_setjmp call with returns_twice attribute
# - A reset_ctx_ptr getelementptr
# - Atomic store of ucontext buffer to reset_ctx
# - Atomic store of null to reset_ctx before return (since ucontext is stack-allocated)

# CHECK-LABEL: @julia_test_cancel_check
# CHECK: %cancel_ucontext = alloca
# CHECK: %reset_ctx_ptr = getelementptr
# CHECK: store atomic ptr {{.*}}, ptr %reset_ctx_ptr release
# CHECK: call i32 @{{.*}}setjmp{{.*}}(ptr {{.*}}) #[[ATTR:[0-9]+]]
# CHECK: store atomic ptr null, ptr %reset_ctx_ptr release
# CHECK: ret
# CHECK: attributes #[[ATTR]] = {{{.*}}returns_twice{{.*}}}
function test_cancel_check()
    Base.@cancel_check
    return 1
end

emit(test_cancel_check)
