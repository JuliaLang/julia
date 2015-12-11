# This file is a part of Julia. License is MIT: http://julialang.org/license

# Tests for /base/stacktraces.jl

# Some tests don't currently work for Appveyor 32-bit Windows
const APPVEYOR_WIN32 = (
    OS_NAME == :Windows && WORD_SIZE == 32 && get(ENV, "APPVEYOR", "False") == "True"
)

if !APPVEYOR_WIN32
    let
        @noinline child() = stacktrace()
        @noinline parent() = child()
        @noinline grandparent() = parent()
        line_numbers = @__LINE__ - [3, 2, 1]

        # Basic tests.
        stack = grandparent()
        @assert length(stack) >= 3 "Compiler has unexpectedly inlined functions"
        @test [:child, :parent, :grandparent] == [f.func for f in stack[1:3]]
        for (line, frame) in zip(line_numbers, stack[1:3])
            @test [Symbol(@__FILE__), line] in
                ([frame.file, frame.line], [frame.inlined_file, frame.inlined_line])
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
    end
end

let
    # Test from_c
    default, with_c, without_c = stacktrace(), stacktrace(true), stacktrace(false)
    @test default == without_c
    @test length(with_c) > length(without_c)
    @test !isempty(filter(frame -> frame.from_c, with_c))
    @test isempty(filter(frame -> frame.from_c, without_c))
end

@test StackTraces.lookup(C_NULL) == StackTraces.UNKNOWN

let
    # No errors should mean nothing in catch_backtrace
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
