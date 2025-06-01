# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests for /base/stacktraces.jl

using Serialization, Base.StackTraces

let
    @noinline child() = stacktrace()
    @noinline parent() = child()
    @noinline grandparent() = parent()
    line_numbers = @__LINE__() .- [3, 2, 1]
    stack = grandparent()

    # Basic tests.
    @assert length(stack) >= 3 "Compiler has unexpectedly inlined functions"

    @test [:child, :parent, :grandparent] == [f.func for f in stack[1:3]]
    for (line, frame) in zip(line_numbers, stack[1:3])
        @test [Symbol(@__FILE__), line] == [frame.file, frame.line]
    end
    @test [false, false, false] == [f.from_c for f in stack[1:3]]

    # Test remove_frames!
    stack = StackTraces.remove_frames!(grandparent(), :parent)
    @test stack[1] == StackFrame(:grandparent, @__FILE__, line_numbers[3])

    stack = StackTraces.remove_frames!(grandparent(), [:child, :something_nonexistent])
    @test stack[1:2] == [
        StackFrame(:parent, @__FILE__, line_numbers[2]),
        StackFrame(:grandparent, @__FILE__, line_numbers[3])
    ]

    b = PipeBuffer()
    frame = stack[1]
    serialize(b, frame)
    frame2 = deserialize(b)
    @test frame !== frame2
    @test frame == frame2
    @test frame.linfo !== nothing
    @test frame2.linfo === nothing
end

# Test from_c
let (default, with_c, without_c) = (stacktrace(), stacktrace(true), stacktrace(false))
    @test default == without_c
    @test length(with_c) > length(without_c)
    @test !isempty(filter(frame -> frame.from_c, with_c))
    @test isempty(filter(frame -> frame.from_c, without_c))
end

@test StackTraces.lookup(C_NULL) == [StackTraces.UNKNOWN] == StackTraces.lookup(C_NULL + 1) == StackTraces.lookup(C_NULL - 1)

let ct = current_task()
    # After a task switch, there should be nothing in catch_backtrace
    yieldto(@task yieldto(ct))
    @test catch_backtrace() == StackFrame[]

    @noinline bad_function() = throw(UndefVarError(:nonexistent))
    @noinline function try_stacktrace()
        try
            bad_function()
        catch
            return stacktrace()
        end
    end
    @noinline function try_catch()
        try
            bad_function()
        catch
            return stacktrace(catch_backtrace())
        end
    end
    line_numbers = @__LINE__() .- [15, 10, 5]

    # Test try...catch with stacktrace
    @test try_stacktrace()[1] == StackFrame(:try_stacktrace, @__FILE__, line_numbers[2])

    # Test try...catch with catch_backtrace
    @test try_catch()[1:2] == [
        StackFrame(:bad_function, @__FILE__, line_numbers[1]),
        StackFrame(:try_catch, @__FILE__, line_numbers[3])
    ]
end

module inlined_test
using Test
@inline g(x) = (x == 3 && throw("a"); x)
@inline h(x) = (x == 3 && g(x); x)
f(x) = (y = h(x); y)
trace = (try; f(3); catch; stacktrace(catch_backtrace()); end)[1:3]
can_inline = Bool(Base.JLOptions().can_inline)
for (frame, func, inlined) in zip(trace, [g,h,f], (can_inline, can_inline, false))
    @test frame.func === typeof(func).name.singletonname
    # broken until #50082 can be addressed
    mi = isa(frame.linfo, Core.CodeInstance) ? frame.linfo.def : frame.linfo
    @test mi.def.module === which(func, (Any,)).module broken=inlined
    @test mi.def === which(func, (Any,)) broken=inlined
    @test mi.specTypes === Tuple{typeof(func), Int} broken=inlined
    # line
    @test frame.file === Symbol(@__FILE__)
    @test !frame.from_c
    @test frame.inlined === inlined
end
end

let src = Meta.lower(Main, quote let x = 1 end end).args[1]::Core.CodeInfo
    li = ccall(:jl_method_instance_for_thunk, Ref{Core.MethodInstance}, (Any, Any), src, @__MODULE__)
    sf = StackFrame(:a, :b, 3, li, false, false, 0)
    repr = string(sf)
    @test repr == "Toplevel MethodInstance thunk at b:3"
end
let li = only(methods(fieldtype)).unspecialized,
    sf = StackFrame(:a, :b, 3, li, false, false, 0),
    repr = string(sf)
    @test repr == "fieldtype(::Vararg{Any}) at b:3"
end

let ctestptr = cglobal((:ctest, "libccalltest")),
    ctest = StackTraces.lookup(ctestptr)

    @test length(ctest) == 1
    @test ctest[1].func === :ctest
    @test ctest[1].linfo === nothing
    @test ctest[1].from_c
    @test ctest[1].pointer === UInt64(ctestptr)
end

# issue #19655
let st = stacktrace(empty!(backtrace()))
    # not in a `catch`, so should return an empty StackTrace
    @test isempty(st)
    @test isa(st, StackTrace)
end

module StackTracesTestMod
    unfiltered_stacktrace() = stacktrace()
    filtered_stacktrace() = StackTraces.remove_frames!(stacktrace(), StackTracesTestMod)
end

# Test that `removes_frames!` can correctly remove frames from within the module
trace = StackTracesTestMod.unfiltered_stacktrace()
@test occursin("unfiltered_stacktrace", string(trace))

trace = StackTracesTestMod.filtered_stacktrace()
@test !occursin("filtered_stacktrace", string(trace))

let bt, topline = @__LINE__
try
    let x = 1
        y = 2x
        z = 2z-1
    end
catch
    bt = stacktrace(catch_backtrace())
end
@test bt[1].line == topline+4
end

# Accidental incorrect phi block computation in interpreter
global global_false_bool = false
let bt, topline = @__LINE__
    try
        let
            global read_write_global_bt_test, global_false_bool
            if global_false_bool
            end
            (read_write_global_bt_test, (read_write_global_bt_test=2;))
        end
    catch
        bt = stacktrace(catch_backtrace())
    end
    @test bt[1].line == topline+6
end

# issue #28990
let bt
try
    eval(Expr(:toplevel, LineNumberNode(42, :foo), :(error("blah"))))
catch
    bt = stacktrace(catch_backtrace())
end
@test bt[2].line == 42
@test bt[2].file === :foo
end

@noinline f33065(x; b=1.0, a="") = error()
@noinline f33065(x, y; b=1.0, a="", c...) = error()
let bt
    try
        f33065(0.0f0)
    catch
        bt = stacktrace(catch_backtrace())
    end
    @test any(s->startswith(string(s), "f33065(x::Float32; b::Float64, a::String)"), bt)
    try
        f33065(0.0f0, b=:x)
    catch
        bt = stacktrace(catch_backtrace())
    end
    @test any(s->startswith(string(s), "f33065(x::Float32; b::Symbol, a::String)"), bt)
    try
        f33065(0.0f0, 0.0f0, z=0)
    catch
        bt = stacktrace(catch_backtrace())
    end
    @test any(s->startswith(string(s), "f33065(x::Float32, y::Float32; b::Float64, a::String, c::"), bt)
end

struct F49231{a,b,c,d,e,f,g} end
(::F49231)(a,b,c) = error("oops")

@testset "type_depth_limit" begin
    tdl = Base.type_depth_limit

    str = repr(typeof(view([1, 2, 3], 1:2)))
    @test tdl(str, 0, maxdepth = 1) == "SubArray{…}"
    @test tdl(str, 0, maxdepth = 2) == "SubArray{$Int, 1, Vector{…}, Tuple{…}, true}"
    @test tdl(str, 0, maxdepth = 3) == "SubArray{$Int, 1, Vector{$Int}, Tuple{UnitRange{…}}, true}"
    @test tdl(str, 0, maxdepth = 4) == "SubArray{$Int, 1, Vector{$Int}, Tuple{UnitRange{$Int}}, true}"
    @test tdl(str, 3) == "SubArray{…}"
    @test tdl(str, 44) == "SubArray{…}"
    @test tdl(str, 45) == "SubArray{$Int, 1, Vector{…}, Tuple{…}, true}"
    @test tdl(str, 59) == "SubArray{$Int, 1, Vector{…}, Tuple{…}, true}"
    @test tdl(str, 60) == "SubArray{$Int, 1, Vector{$Int}, Tuple{UnitRange{…}}, true}"
    @test tdl(str, 100) == "SubArray{$Int, 1, Vector{$Int}, Tuple{UnitRange{$Int}}, true}"

    str = repr(Vector{V} where V<:AbstractVector{T} where T<:Real)
    @test tdl(str, 0, maxdepth = 1) == "Vector{…} where {…}"
    @test tdl(str, 0, maxdepth = 2) == "Vector{V} where {T<:Real, V<:AbstractVector{…}}"
    @test tdl(str, 0, maxdepth = 3) == "Vector{V} where {T<:Real, V<:AbstractVector{T}}"
    @test tdl(str, 20) == "Vector{…} where {…}"
    @test tdl(str, 46) == "Vector{…} where {…}"
    @test tdl(str, 47) == "Vector{V} where {T<:Real, V<:AbstractVector{T}}"

    str = "F49231{Vector,Val{('}','}')},Vector{Vector{Vector{Vector}}},Tuple{Int,Int,Int,Int,Int,Int,Int},Int,Int,Int}"
    @test tdl(str, 105) == "F49231{Vector,Val{('}','}')},Vector{Vector{Vector{…}}},Tuple{Int,Int,Int,Int,Int,Int,Int},Int,Int,Int}"
    @test tdl(str, 85) == "F49231{Vector,Val{…},Vector{…},Tuple{…},Int,Int,Int}"

    # Stacktrace
    a = UInt8(81):UInt8(160)
    b = view(a, 1:64)
    c = reshape(b, (8, 8))
    d = reinterpret(reshape, Float64, c)
    sqrteach(a) = [sqrt(x) for x in a]
    st = try
        sqrteach(d)
    catch e
        stacktrace(catch_backtrace())
    end
    str = sprint(Base.show_backtrace, st, context = (:limit=>true, :stacktrace_types_limited => Ref(false), :color=>true, :displaysize=>(50,105)))
    @test contains(str, "[5] \e[0m\e[1mcollect_to!\e[22m\e[0m\e[1m(\e[22m\e[90mdest\e[39m::\e[0mVector\e[90m{…}\e[39m, \e[90mitr\e[39m::\e[0mBase.Generator\e[90m{…}\e[39m, \e[90moffs\e[39m::\e[0m$Int, \e[90mst\e[39m::\e[0mTuple\e[90m{…}\e[39m\e[0m\e[1m)\e[22m\n\e[90m")

    st = try
        F49231{Vector,Val{'}'},Vector{Vector{Vector{Vector}}},Tuple{Int,Int,Int,Int,Int,Int,Int},Int,Int,Int}()(1,2,3)
    catch e
        stacktrace(catch_backtrace())
    end
    str = sprint(Base.show_backtrace, st, context = (:limit=>true, :stacktrace_types_limited => Ref(false), :color=>true, :displaysize=>(50,132)))
    @test contains(str, "[2] \e[0m\e[1m(::$F49231{Vector, Val{…}, Vector{…}, NTuple{…}, $Int, $Int, $Int})\e[22m\e[0m\e[1m(\e[22m\e[90ma\e[39m::\e[0m$Int, \e[90mb\e[39m::\e[0m$Int, \e[90mc\e[39m::\e[0m$Int\e[0m\e[1m)\e[22m\n\e[90m")
end

@testset "Base.StackTraces docstrings" begin
    @test isempty(Docs.undocumented_names(StackTraces))
end
