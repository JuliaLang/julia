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
@inline g(x) = (y = throw("a"); y) # the inliner does not insert the proper markers when inlining a single expression
@inline h(x) = (y = g(x); y)       # this test could be extended to check for that if we switch to linear representation
f(x) = (y = h(x); y)
trace = (try; f(3); catch; stacktrace(catch_backtrace()); end)[1:3]
can_inline = Bool(Base.JLOptions().can_inline)
for (frame, func, inlined) in zip(trace, [g,h,f], (can_inline, can_inline, false))
    @test frame.func === typeof(func).name.mt.name
    #@test get(frame.linfo).def === which(func, (Any,)).func
    #@test get(frame.linfo).specTypes === Tuple{typeof(func), Int}
    # line
    @test frame.file === Symbol(@__FILE__)
    @test !frame.from_c
    @test frame.inlined === inlined
end
end

let src = Meta.lower(Main, quote let x = 1 end end).args[1]::Core.CodeInfo,
    li = ccall(:jl_new_method_instance_uninit, Ref{Core.MethodInstance}, ()),
    sf

    li.uninferred = src
    li.specTypes = Tuple{}
    li.def = @__MODULE__
    sf = StackFrame(:a, :b, 3, li, false, false, 0)
    repr = string(sf)
    @test repr == "Toplevel MethodInstance thunk at b:3"
end
let li = typeof(fieldtype).name.mt.cache.func::Core.MethodInstance,
    sf = StackFrame(:a, :b, 3, li, false, false, 0),
    repr = string(sf)
    @test repr == "fieldtype(...) at b:3"
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
    @test any(s->startswith(string(s), "f33065(::Float32; b::Float64, a::String)"), bt)
    try
        f33065(0.0f0, b=:x)
    catch
        bt = stacktrace(catch_backtrace())
    end
    @test any(s->startswith(string(s), "f33065(::Float32; b::Symbol, a::String)"), bt)
    try
        f33065(0.0f0, 0.0f0, z=0)
    catch
        bt = stacktrace(catch_backtrace())
    end
    @test any(s->startswith(string(s), "f33065(::Float32, ::Float32; b::Float64, a::String, c::"), bt)
end

# Test hidden frames
function bt_not_hidden_frame()
    backtrace()
end
Base.@hide_in_stacktrace function bt_hidden_frame()
    bt_not_hidden_frame()
end

let bt = bt_hidden_frame()
    st = stacktrace(bt, true)
    hidden_frame = st[findfirst(s->s.func == :bt_hidden_frame, st)]
    @test StackTraces.is_hidden_frame(hidden_frame)
    not_hidden_frame = st[findfirst(s->s.func == :bt_not_hidden_frame, st)]
    @test !StackTraces.is_hidden_frame(not_hidden_frame)
end
