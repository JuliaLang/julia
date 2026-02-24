# This file is a part of Julia. License is MIT: https://julialang.org/license

using Core: CodeInstance, MethodInstance
using Test

struct TestOwner end
const owner = TestOwner()

function compile_no_deps(f, argtypes)
    @nospecialize
    mi = Base.method_instance(f, argtypes)
    source, _ = only(code_typed(f, argtypes))
    ci = CodeInstance(
        mi, owner, source.rettype, #=exctype=#Any, #=inferred_const=#nothing,
        #=inferred=#nothing, #=const_flags=#Int32(0), source.min_world,
        #=max_world=#typemax(UInt), #=effects=#UInt32(0),
        #=analysis_results=#nothing, source.debuginfo, source.edges
    )
    # Insert the CI into the global cache (necessary before adding to JIT)
    ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), mi, ci)
    ccall(:jl_add_codeinst_to_jit, Cvoid, (Any, Any), ci, source)
    ci
end

function check_edges_not_compiled(ci::CodeInstance, target)
    @nospecialize
    for e in ci.edges
        e isa CodeInstance || continue
        e.def isa MethodInstance || continue
        e.def.def isa Method || continue
        if e.def.def.sig <: Tuple{typeof(target), Vararg}
            e.invoke == Ptr{Nothing}(0) || return false
            e.specptr == Ptr{Nothing}(0) || return false
        end
    end
    true
end

# Test fptr1 -> tojlinvoke trampoline
module M1
    @noinline foo(xs...) = xs[2]
    bar(x) = 2*foo(x, x, x, x, x, x)
end
ci = compile_no_deps(M1.bar, (Int,))
@test check_edges_not_compiled(ci, M1.foo)
@test invoke(M1.bar, ci, 100) == 200

# Test specsig -> tojlinvoke trampoline
module M2
    @noinline foo(x) = x+100
    bar(x) = 2*foo(x)
end
ci = compile_no_deps(M2.bar, (Int,))
@test check_edges_not_compiled(ci, M2.foo)
@test invoke(M2.bar, ci, 5) == 210

# Each `eval` must compile (because of the ccall) a top-level thunk.  The
# CodeInstance for this thunk becomes garbage-collectable after being invoked,
# but before returning, because of wait().  If the invoke must return for the
# CodeInstance address to be unregistered from the JIT, this will crash.  Credit
# to @vtjnash for this example.
function test_gc_codeinst()
    for i=1:10000
        @async eval(:(ccall(:sqrt, Float64, (Float64,), $i); wait()))
        i % 100 == 0 && GC.gc()
    end
    true
end
@test test_gc_codeinst()
sleep(5)  # Avoids problems where we don't respond to Distributed.jl fast enough
