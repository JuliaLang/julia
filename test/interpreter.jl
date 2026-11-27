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

# An interpreted frame lets an SSA value die once no statement can use it any more, as
# compiled code does, rather than keeping it alive until the frame returns.
module InterpreterLifetimes
using Test
Base.Experimental.@compiler_options compile=min

track(w::Vector{WeakRef}, x) = (push!(w, WeakRef(x)); x)
make(n) = [Ref(i) for i in 1:n]

function unused(w)
    track(w, make(3))
    GC.gc(true)
    return w[1].value === nothing
end

# the iterator is used at every iteration, so it lives until the loop is done
function loop(w)
    s = 0
    for x in track(w, make(3))
        GC.gc(true)
        s += x[]
    end
    GC.gc(true)
    return s, w[1].value === nothing
end

# the first argument lives across the exception
pair(a, b) = (sum(x -> x[], a), b)
function across_catch(w)
    return pair(track(w, make(3)), try
        GC.gc(true)
        error("thrown")
    catch
        GC.gc(true)
        1
    end)
end

# a value that dies during an iteration is left out by one collection; the next, after the
# loop has gone back to before the value is computed again, must not find it either
function revisit(n)
    s = 0
    for i in 1:n
        GC.gc(false)
        s += length(make(i % 64 + 1))
        GC.gc(false)
    end
    return s
end

# MMTk's concurrent marking still scans every value of an interpreted frame as live
const dead_values_die = !occursin("CONCURRENTIMMIX", GC.gc_active_impl())

@testset "interpreted SSA values die after their last use" begin
    @test unused(WeakRef[]) skip=!dead_values_die
    s, died = loop(WeakRef[])
    @test s == 6
    @test died skip=!dead_values_die
    @test across_catch(WeakRef[]) == (6, 1)
    @test revisit(200) == sum(i -> i % 64 + 1, 1:200)
end
end
