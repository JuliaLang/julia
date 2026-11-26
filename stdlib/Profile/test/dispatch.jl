# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Profile: Dispatch

struct DispatchBox
    x::Any
end
@noinline dispatch_unbox(b) = b.x + 1
function dispatch_loop(bs)
    t = 0
    for b in bs
        t += dispatch_unbox(b)::Int
    end
    return t
end
dispatch_splat(f, args) = f(args...)
mutable struct DispatchGarbage
    x::Int
end
dispatch_finalize(g::DispatchGarbage) = (g.x = 0; nothing)
@noinline dispatch_deep(n, b) = n == 0 ? dispatch_unbox(b)::Int : dispatch_deep(n - 1, b) + 1

@testset "dispatch profiler" begin
    Dispatch.clear()
    @test_logs (:warn, r"^There were no samples collected\.") Dispatch.print(IOBuffer())

    bs = [DispatchBox(i) for i in 1:1000]
    dispatch_loop(bs)
    line = first(methods(dispatch_unbox)).line
    @test Dispatch.@profile(sample_rate = 1, dispatch_loop(bs)) == dispatch_loop(bs)
    results = Dispatch.fetch()
    @test results.sample_rate == 1
    at_unbox = filter(results.dispatches) do d
        !isempty(d.stacktrace) && d.stacktrace[1].func === :dispatch_unbox
    end
    # every `b.x + 1` is a dispatch, found in the call-site cache once it is warm
    @test length(at_unbox) == 1000
    @test all(d -> d.stacktrace[1].line == line, at_unbox)
    @test all(d -> d.callee.def.name === :+ && d.callee.specTypes == Tuple{typeof(+), Int, Int}, at_unbox)
    @test all(d -> d.path === :callsite, at_unbox)

    report = sprint(Dispatch.print, results)
    @test occursin(r"1000 +\d+\.\d% +dispatch_unbox\(b::DispatchBox\) at \S*dispatch\.jl:", report)
    @test occursin("+(::$Int, ::$Int)", report)
    # files are shown with the package they belong to
    Dispatch.@profile sample_rate = 1 sprint(show, MIME("text/plain"), Any[1, 2.0])
    @test occursin(r" at @Base/\S+\.jl:\d+", sprint(Dispatch.print, Dispatch.fetch()))
    Dispatch.clear()
    Dispatch.@profile(sample_rate = 1, dispatch_loop(bs))
    results = Dispatch.fetch()

    # the callers of a call site
    report = sprint(io -> Dispatch.print(IOContext(io, :displaysize => (50, 200)), results; callers = 2))
    @test occursin(r"called from\n +1000  dispatch_loop at \S*dispatch\.jl:\d+ ← ", report)
    @test !occursin("called from", sprint(Dispatch.print, results))

    # the backtraces as a tree, ending in the callee
    tree = sprint(io -> Dispatch.print(IOContext(io, :displaysize => (50, 200)), results; format = :tree))
    @test occursin(r"1000 +\S*dispatch\.jl:\d+ +dispatch_loop\(", tree)
    @test occursin(r"1000 +\S*dispatch\.jl:\d+ +dispatch_unbox\(b::DispatchBox\)\n", tree)
    @test occursin(Regex("1000 +\\S+:\\d+ +→ \\+\\(::$Int, ::$Int\\)\n"), tree)
    @test occursin("Total samples: $(length(results.dispatches))", tree)
    @test_throws ArgumentError Dispatch.print(IOBuffer(), results; format = :nope)

    # stopped: nothing more is recorded, and the samples stay until cleared
    dispatch_loop(bs)
    @test length(Dispatch.fetch().dispatches) == length(results.dispatches)
    Dispatch.clear()
    @test isempty(Dispatch.fetch().dispatches)

    # a limited backtrace still has the call site
    Dispatch.@profile sample_rate = 1 maxdepth = 1 dispatch_loop(bs)
    results = Dispatch.fetch()
    @test count(d -> length(d.stacktrace) >= 1 && d.stacktrace[1].func === :dispatch_unbox, results.dispatches) == 1000
    @test maximum(d -> length(d.stacktrace), results.dispatches) <= 4 # one address, with its inlined frames
    Dispatch.clear()

    # sampling
    Dispatch.@profile sample_rate = 0.1 for _ in 1:20
        dispatch_loop(bs)
    end
    results = Dispatch.fetch()
    @test results.sample_rate == 0.1
    @test 1000 < length(results.dispatches) < 3000
    @test occursin("dispatches at sample_rate = 0.1", sprint(Dispatch.print, results))

    # samples of different rates in one buffer have no common rate
    Dispatch.@profile sample_rate = 1 dispatch_loop(bs)
    @test Dispatch.fetch().sample_rate === nothing
    Dispatch.clear()

    # clearing while recording keeps the rate of what is recorded next
    Dispatch.start(sample_rate = 0.5)
    dispatch_loop(bs)
    Dispatch.clear()
    dispatch_loop(bs)
    Dispatch.stop()
    @test Dispatch.fetch().sample_rate == 0.5
    Dispatch.@profile sample_rate = 1 dispatch_loop(bs)
    @test Dispatch.fetch().sample_rate === nothing
    Dispatch.clear()
    @test Dispatch.fetch().sample_rate === nothing && isempty(Dispatch.fetch().dispatches)

    # a backtrace too long for a block of the arena, followed by short ones
    deep = dispatch_deep(20_000, bs[1])
    Dispatch.@profile sample_rate = 1 begin
        @test dispatch_deep(20_000, bs[1]) == deep
        dispatch_loop(bs)
    end
    results = Dispatch.fetch()
    @test maximum(d -> length(d.stacktrace), results.dispatches) > 20_000
    @test count(d -> !isempty(d.stacktrace) && d.stacktrace[1].func === :dispatch_unbox, results.dispatches) == 1001
    Dispatch.clear()

    # a finalizer run by the GC is marked as one
    # the objects escape, so that their finalizers are not run early by the compiler
    dispatch_garbage() = length(Any[finalizer(dispatch_finalize, DispatchGarbage(i)) for i in 1:100])
    dispatch_garbage(); GC.gc()
    Dispatch.@profile sample_rate = 1 (dispatch_garbage(); GC.gc(); GC.gc())
    report = sprint(io -> Dispatch.print(IOContext(io, :displaysize => (50, 200)), Dispatch.fetch(); limit = 100))
    @test occursin(r"\[finalizer[\],].*\n +\d+  dispatch_finalize\(::DispatchGarbage\)", report)
    Dispatch.clear()

    # a call made by the runtime for a splat is attributed to the Julia frame
    dispatch_splat(+, Any[1, 2])
    Dispatch.@profile sample_rate = 1 for _ in 1:100
        dispatch_splat(+, Any[1, 2])
    end
    report = sprint(Dispatch.print, Dispatch.fetch())
    @test occursin(r"dispatch_splat\(.* at \S*dispatch\.jl:\d+ \[splat\]", report)
    @test occursin("interpreted code under ", report) # the loop at top level
    Dispatch.clear()
end
