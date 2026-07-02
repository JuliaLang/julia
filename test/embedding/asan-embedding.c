// This file is a part of Julia. License is MIT: https://julialang.org/license

// Test for Julia's ASan-compatibility workarounds.
//
// This program embeds a *stock* (non-sanitizer) build of Julia inside a host that
// IS built with AddressSanitizer (see the check-asan target in the Makefile). This
// configuration is deliberately NOT covered by the ASan CI job, which rebuilds all
// of Julia with ASan and therefore never mixes instrumented and uninstrumented code.
//
// It exercises the runtime workarounds gated on `jl_running_under_sanitizer()`,
// which detects ASan at runtime (via dlsym(RTLD_DEFAULT, "__asan_init")) or honors
// JULIA_ASAN_COMPAT=0/1:
//   - jl_dlopen bypasses ASan's dlopen interposition ($ORIGIN/RUNPATH resolution),
//   - RTLD_DEEPBIND is avoided, and
//   - jl_longjmp bypasses ASan's siglongjmp interceptor.
// Running under ASan's allocator bookkeeping (alloc_dealloc_mismatch=1) also
// guards against `operator new`/`delete` calls crossing the libjulia-internal /
// libjulia-codegen boundary: libjulia-internal links libstdc++ statically, so its
// allocations bypass ASan's interceptors while deallocations in other libraries
// do not.
//
// Exit code 0 and "ALL TESTS PASSED" on stderr == success. A crash, an ASan
// report, or a non-zero exit == failure.

#include <julia.h>
#include <stdio.h>

JULIA_DEFINE_FAST_TLS

static const char *PROGRAM =
"using LinearAlgebra\n"
"let failures = 0\n"
"    function check(f, label)\n"
"        try\n"
"            f()\n"
"            println(stderr, \"  ok    \", label)\n"
"        catch e\n"
"            print(stderr, \"  FAIL  \", label, \": \")\n"
"            showerror(stderr, e)\n"
"            println(stderr)\n"
"            failures += 1\n"
"        end\n"
"    end\n"
"    # Exception unwinding throws and jl_longjmp's back to the handler ->\n"
"    # exercises the siglongjmp interceptor bypass.\n"
"    check(\"exceptions (jl_longjmp / siglongjmp bypass)\") do\n"
"        threw = false\n"
"        try; error(\"boom\"); catch; threw = true; end\n"
"        @assert threw\n"
"    end\n"
"    # Same, but across a task switch.\n"
"    check(\"tasks + exceptions\") do\n"
"        t = @task (try; error(\"x\"); catch; 42; end)\n"
"        schedule(t); @assert fetch(t) == 42\n"
"    end\n"
"    # Float64 matmul loads OpenBLAS via libblastrampoline -> exercises Julia's\n"
"    # jl_dlopen ($ORIGIN/RUNPATH resolution for libgfortran etc.) and then\n"
"    # allocates/frees across the library boundary. (libblastrampoline's own\n"
"    # DEEPBIND use is controlled separately via LBT_USE_RTLD_DEEPBIND, which\n"
"    # the Makefile sets to 0.)\n"
"    check(\"dlopen + RUNPATH via BLAS\") do\n"
"        A = rand(64, 64); B = rand(64, 64); C = A * B\n"
"        @assert size(C) == (64, 64)\n"
"        @assert isfinite(sum(C))\n"
"    end\n"
"    if failures == 0\n"
"        println(stderr, \"ALL TESTS PASSED\")\n"
"    else\n"
"        error(string(failures, \" test(s) failed\"))\n"
"    end\n"
"end\n";

int main(void)
{
    jl_init();
    jl_eval_string(PROGRAM);
    int rc = 0;
    if (jl_exception_occurred()) {
        fprintf(stderr, "asan-embedding: FAILED (uncaught error; see messages above)\n");
        rc = 1;
    }
    jl_atexit_hook(rc);
    return rc;
}
