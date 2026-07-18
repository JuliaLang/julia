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
    ccall(:jl_add_codeinsts_to_jit, Cvoid, (Any, Any), Any[ci], Any[source])
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

# Compilation batches must stay closed under invoke edges whose CodeInstance is
# globally cached but whose source is only visible to another interpreter:
# `return_types` infers `bar` (and its edge `foo`) into the global cache without
# compiling anything, so the subsequent `precompile` batch sees `foo` as a
# sourceless cached edge. It must compile `foo` alongside `bar` rather than
# linking the call as a permanently-boxing `tojlinvoke` trampoline.
module M3
    @noinline foo(x) = x+1
    bar(x) = foo(x)
end
Base.return_types(M3.bar, (Int,))
let mi = Base.method_instance(M3.foo, (Int,))
    ci = mi.cache
    # Precondition for the scenario: inference cached foo's CodeInstance without
    # compiling it. If this fails, the setup no longer produces a sourceless
    # cached edge and the test needs a new way to construct one.
    @test ci isa CodeInstance
    @test ci.invoke == Ptr{Nothing}(0)
    @test precompile(M3.bar, (Int,))
    @test ci.invoke != Ptr{Nothing}(0)
    @test ci.specptr != Ptr{Nothing}(0)
end

# When runtime dispatch caches compiled code onto an exact-signature
# MethodInstance by copying it from the widened compileable MethodInstance
# (`copy_to_mi_cache`), a specsig specptr must not be adopted: it is ABI'd to
# the widened signature, and the copy would advertise
# JL_CI_FLAGS_INVOKE_MATCHES_SPECPTR without JL_CI_FLAGS_SPECPTR_SPECIALIZED,
# tripping the flag-consistency assert in `JuliaOJIT::linkCISymbol` when a
# batch later links a call target to it. Only the boxed-ABI invoke wrapper may
# be copied.
@noinline copyspecsig(@nospecialize(x)) = x === nothing ? 0 : 1
let m = only(methods(copyspecsig))
    # the exact (non-normalized) specialization runtime dispatch would mint
    mi = ccall(:jl_specializations_get_linfo, Ref{MethodInstance},
               (Any, Any, Any), m, Tuple{typeof(copyspecsig), Int}, Core.svec())
    args = Any[1]
    # TRIGGER_FOREIGN forces the copy onto `mi` even for matching sparams
    @test ccall(:jl_invoke, Any, (Any, Ptr{Any}, UInt32, Any),
                copyspecsig, args, length(args), mi) === 1
    known_invokes = Ptr{Cvoid}[
        unsafe_load(cglobal(:jl_fptr_args_addr, Ptr{Cvoid})),
        unsafe_load(cglobal(:jl_fptr_const_return_addr, Ptr{Cvoid})),
        unsafe_load(cglobal(:jl_fptr_sparam_addr, Ptr{Cvoid})),
        unsafe_load(cglobal(:jl_fptr_interpret_call_addr, Ptr{Cvoid})),
        unsafe_load(cglobal(:jl_fptr_wait_for_compiled_addr, Ptr{Cvoid})),
    ]
    for spec in Base.specializations(m)
        ci = isdefined(spec, :cache, :acquire) ? (@atomic :acquire spec.cache) : nothing
        while ci isa CodeInstance
            flags = @atomic :acquire ci.flags
            invoke = @atomic :acquire ci.invoke
            specptr = @atomic :acquire ci.specptr
            if !iszero(flags & 0x02) && invoke != C_NULL && specptr != C_NULL
                # INVOKE_MATCHES_SPECPTR requires SPECPTR_SPECIALIZED to agree
                # with the invoke pointer's api
                @test (invoke ∉ known_invokes) == !iszero(flags & 0x01)
            end
            ci = isdefined(ci, :next, :acquire) ? (@atomic :acquire ci.next) : nothing
        end
    end
end

# External symbol renames must keep JITLink's external symbol map consistent.
@testset "JITLink external symbol rename" begin
    jitlink_rename_resolve(chunks, i, x) =
        jitlink_rename_resolve(Base.tail(chunks), i,
            map(tuple, x, i[1] === Colon() ? (1, (), 1) : (1, 1, ())))
    jitlink_rename_resolve(::Tuple{}, i, x) = x
    function jitlink_rename_reproducer(i)
        x = jitlink_rename_resolve(Base.inferencebarrier(true) ? (1,) :
            map(+, Tuple([]), (1, 1)), i, ((), (), ()))
        y = Base.inferencebarrier(true) ? :a : :b
        if y === :a; elseif y === :b
            jitlink_rename_reproducer(x[3])
        else
            0[x[2]]
        end
    end
    @test precompile(jitlink_rename_reproducer, (Tuple{Colon},))
end

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
