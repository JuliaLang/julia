# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "stacktraces" begin

let
    @noinline child() = stacktrace()
    @noinline parent() = child()
    @noinline grandparent() = parent()
    line_numbers = @__LINE__ - [3, 2, 1]
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
    @test !isnull(frame.linfo)
    @test isnull(frame2.linfo)
end

let
    # Test from_c
    default, with_c, without_c = stacktrace(), stacktrace(true), stacktrace(false)
    @test default == without_c
    @test length(with_c) > length(without_c)
    @test !isempty(filter(frame -> frame.from_c, with_c))
    @test isempty(filter(frame -> frame.from_c, without_c))
end

@test StackTraces.lookup(C_NULL) == [StackTraces.UNKNOWN]

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
            return catch_stacktrace()
        end
    end
    line_numbers = @__LINE__ .- [15, 10, 5]

    # Test try...catch with stacktrace
    @test try_stacktrace()[1] == StackFrame(:try_stacktrace, @__FILE__, line_numbers[2])

    # Test try...catch with catch_stacktrace
    @test try_catch()[1:2] == [
        StackFrame(:bad_function, @__FILE__, line_numbers[1]),
        StackFrame(:try_catch, @__FILE__, line_numbers[3])
    ]
end

module inlined_test
using Base.Test
@inline g(x) = (y = throw("a"); y) # the inliner does not insert the proper markers when inlining a single expression
@inline h(x) = (y = g(x); y)       # this test could be extended to check for that if we switch to linear representation
f(x) = (y = h(x); y)
trace = (try; f(3); catch; catch_stacktrace(); end)[1:3]
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
end
