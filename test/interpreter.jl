# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

# interpreted but inferred/optimized top-level expressions with vars
let code = """
           while true
               try
                   this_is_undefined_29213
                   ed = 0
                   break
               finally
                   break
               end
           end
           print(42)
           """
    @test read(`$(Base.julia_cmd()) --startup-file=no --compile=min -e $code`, String) == "42"
end

let code = "Threads.atomic_add!(Threads.Atomic{Int}(40), 2)"
    @test read(`$(Base.julia_cmd()) --startup-file=no --compile=min -E $code`, String) == "40\n"
end

let p = Pipe(),
    c = pipeline(`$(Base.julia_cmd()) --startup-file=no --compile=min -E 'error()'`, stderr=p)
    proc = run(c, wait=false)
    # skip anything printed before the error, such as an objcache warning (#62800)
    while !eof(p) && !occursin("ERROR:", readline(p)) end
    @test readline(p) == "Stacktrace:"
    wait(proc)
    close(p)
end

# Test generated function behavior in interpreter
@test success(pipeline(`$(Base.julia_cmd()) --compile=min -E 'include("staged.jl")'`; stderr))

# Test contextual execution mechanism in interpreter (#54360)
let compiler_contextual_test = escape_string(joinpath(@__DIR__,"../Compiler/test/contextual.jl"))
    @test success(pipeline(`$(Base.julia_cmd()) --compile=min -E "include(\"$compiler_contextual_test\")"`; stderr))
end

let code = """
    f(args...) = args
    f(1)
    mi = first(Base.method_instances(f, Tuple{Int}, Base.get_world_counter()))
    ci = ccall(:jl_new_codeinst, Ref{Core.CodeInstance},
        (Any, Any, Any, Any, Any, Any, Int32, UInt, UInt, UInt32, Any, Any, Any),
        mi, nothing, Any, Any, nothing, nothing,
        Int32(0), UInt(1), typemax(UInt), UInt32(0), nothing, nothing, Core.svec())
    @assert invoke(f, ci, 42) === (42,)
    print("ok")
    """
    @test read(`$(Base.julia_cmd()) --startup-file=no --compile=min -e $code`, String) == "ok"
end

# An interpreted frame clears the SSA values used only in one basic block when it leaves that
# block, rather than keeping them alive until the frame returns.
module InterpreterBlockLocals
using Test
Base.Experimental.@compiler_options compile=min

track(w::Vector{WeakRef}, x) = (push!(w, WeakRef(x)); x)
make(n) = [Ref(i) for i in 1:n]
dead(w) = all(r -> r.value === nothing, w)
pair(a, b) = (sum(x -> x[], a), b)
@noinline id(x) = x

# a value that is never used dies when its block is left
function unused(w, c)
    track(w, make(3))
    c && id(nothing)
    GC.gc(true)
    return dead(w)
end

# a value used later in its block survives a collection before that use
same_block(w) = pair(track(w, make(3)), (GC.gc(true); 1))

# and one used in another block survives the blocks in between
function across_blocks(w, c)
    a = 0
    x = pair(track(w, make(3)), c ? (GC.gc(true); id(1)) : (a = 2))
    GC.gc(true)
    return x, a
end

# the temporaries of each iteration die at the end of the iteration, while the iterator,
# which every iteration uses, lives on
function loop(w)
    s = 0
    for x in track(w, make(3))
        s += length(track(w, make(x[])))
        GC.gc(true)
    end
    GC.gc(true)
    return s, dead(w[2:end])
end

# a value cleared at the end of an iteration is computed again before the next uses it
function revisit(n)
    s = 0
    for i in 1:n
        GC.gc(false)
        s += length(make(i % 64 + 1))
        GC.gc(false)
    end
    return s
end

# values that live across a try, and the catch and finally blocks that run after a throw in
# the middle of a block
function across_catch(w)
    return pair(track(w, make(3)), try
        GC.gc(true)
        x = pair(make(2), (GC.gc(true); error("thrown")))
    catch
        GC.gc(true)
        1
    end)
end
function with_finally(w, n)
    s = 0
    for i in 1:n
        try
            s += pair(track(w, make(i)), (GC.gc(true); i))[1]
            iseven(i) && error("even")
        catch
            s += length(make(i))
        finally
            GC.gc(true)
            s += sum(x -> x[], make(2))
        end
    end
    GC.gc(true)
    return s, dead(w)
end

# a preserved value lives until the end of the preserve, across blocks
function preserved(w, n)
    x = track(w, make(n))
    s = 0
    GC.@preserve x begin
        x = nothing
        for i in 1:n
            GC.gc(true)
            s += i
        end
        alive = w[1].value !== nothing
    end
    return s, alive
end

@testset "interpreted SSA values die at the end of their block" begin
    @test unused(WeakRef[], true)
    @test unused(WeakRef[], false)
    @test same_block(WeakRef[]) == (6, 1)
    @test across_blocks(WeakRef[], true) == ((6, 1), 0)
    @test across_blocks(WeakRef[], false) == ((6, 2), 2)
    @test loop(WeakRef[]) == (6, true)
    @test revisit(200) == sum(i -> i % 64 + 1, 1:200)
    @test across_catch(WeakRef[]) == (6, 1)
    w = WeakRef[]
    @test with_finally(w, 5) == (35 + 2 + 4 + 5 * 3, true)
    @test length(w) == 5
    @test preserved(WeakRef[], 4) == (10, true)
end
end

# The same for optimized code, with phis and a preserved SSA value, which the interpreter runs
# as an opaque closure that is not compiled
module InterpreterBlockLocalsOptimized
using Test
@noinline track(w, x) = (push!(w, WeakRef(x)); x)
@noinline make(n) = [Ref(i) for i in 1:n]
@noinline gc() = GC.gc(true)
function f(w, v, n)
    s = 0
    for i in 1:n
        s += length(track(w, make(i)))
        gc()
    end
    t = iseven(n) ? n + 1 : 2n
    y = track(v, make(n))
    GC.@preserve y begin
        for i in 1:n
            gc()
            s += i
        end
        alive = v[1].value !== nothing
    end
    gc()
    return s, t, alive, all(r -> r.value === nothing, w)
end
@testset "optimized code" begin
    ir = only(Base.code_ircode(f, (Vector{WeakRef}, Vector{WeakRef}, Int)))[1]
    @test any(s -> Meta.isexpr(s, :gc_preserve_begin) && s.args[1] isa Core.SSAValue, ir.stmts.stmt)
    @test any(s -> s isa Core.PhiNode, ir.stmts.stmt)
    ir.argtypes[1] = Tuple{}
    oc = Core.OpaqueClosure(ir; do_compile=false)
    @test oc(WeakRef[], WeakRef[], 4) == (20, 5, true, true)
    @test oc(WeakRef[], WeakRef[], 3) == (12, 6, true, true)
end
end
