# This file is a part of Julia. License is MIT: http://julialang.org/license

# Tests for /base/stacktraces.jl

let
    @noinline child() = stacktrace()
    @noinline parent() = child()
    @noinline grandparent() = parent()
    line_numbers = @__LINE__ - [3, 2, 1]

    # Basic tests.
    stack = grandparent()
    @test [:child, :parent, :grandparent] == [f.func for f in stack[1:3]]
    for (line, frame) in zip(line_numbers, stack[1:3])
        @test [Symbol(@__FILE__), line] in
            ([frame.file, frame.line], [frame.inlined_file, frame.inlined_line])
    end
    @test [false, false, false] == [f.from_c for f in stack[1:3]]

    # Test from_c
    default, with_c, without_c = stacktrace(), stacktrace(true), stacktrace(false)
    @test default == without_c
    @test length(with_c) > length(without_c)
    @test !isempty(filter(frame -> frame.from_c, with_c))
    @test isempty(filter(frame -> frame.from_c, without_c))

    # Test remove_frames!
    stack = StackTraces.remove_frames!(grandparent(), :parent)
    @test stack[1] == StackFrame(
        :grandparent, @__FILE__, line_numbers[3], Symbol(""), -1, false, 0
    )

    stack = StackTraces.remove_frames!(grandparent(), [:child, :something_nonexistent])
    @test stack[1:2] == [
        StackFrame(:parent, @__FILE__, line_numbers[2], Symbol(""), -1, false, 0),
        StackFrame(:grandparent, @__FILE__, line_numbers[3], Symbol(""), -1, false, 0)
    ]
end

let
    # No errors should mean nothing in catch_backtrace
    @test catch_backtrace() == StackFrame[]

    @noinline bad_function() = nonexistent_var + 1
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
    line_numbers = @__LINE__ .- [5, 10, 15]

    # Test try...catch with stacktrace
    @test try_stacktrace()[1] == StackFrame(
        :try_stacktrace, @__FILE__, line_numbers[2], Symbol(""), -1, false, 0
    )

    # Test try...catch with catch_stacktrace
    @test try_catch()[1:2] == [
        StackFrame(:bad_function, @__FILE__, line_numbers[1], Symbol(""), -1, false, 0),
        StackFrame(:try_catch, @__FILE__, line_numbers[3], Symbol(""), -1, false, 0)
    ]
end
