# This file is a part of Julia. License is MIT: http://julialang.org/license

# Tests for /base/stacktraces.jl

using Base.StackTraces
using Base.StackTraces: StackFrame, StackTrace, remove_frames!

@noinline child() = stacktrace()
@noinline parent() = child()
@noinline grandparent() = parent()
line_numbers = [@__LINE__, @__LINE__, @__LINE__] - [3, 2, 1]

@noinline bad_function() = nonexistent_var
@noinline function good_function()
    try
        bad_function()
    catch
        return catch_stacktrace()
    end
end
e_line_numbers = [@__LINE__, @__LINE__] - [8, 5]

format_stack = [
    StackFrame(:frame1, "path/file.1", 10, "path/file.inline", 0, false, 0),
    StackFrame(:frame2, "path/file.2", 20, Symbol(""), -1, false, 0)
]

@testset "basic" begin
    stack = grandparent()
    @test [:child, :parent, :grandparent] == [f.func for f in stack[1:3]]
    for (line, frame) in zip(line_numbers, stack[1:3])
        @test in(
            [Symbol(@__FILE__), line],
            ([frame.file, frame.line], [frame.inlined_file, frame.inlined_line])
        )
    end
    @test [false, false, false] == [f.from_c for f in stack[1:3]]
end

@testset "from_c" begin
    default, with_c, without_c = stacktrace(), stacktrace(true), stacktrace(false)
    @test default == without_c
    @test length(with_c) > length(without_c)
    @test !isempty(filter(frame -> frame.from_c, with_c))
    @test isempty(filter(frame -> frame.from_c, without_c))
end

@testset "remove_frames!" begin
    stack = remove_frames!(grandparent(), :parent)
    @test stack[1] == StackFrame(
        :grandparent, @__FILE__, line_numbers[3], Symbol(""), -1, false, 0
    )

    stack = remove_frames!(grandparent(), [:child, :something_nonexistent])
    @test stack[1:2] == [
        StackFrame(:parent, @__FILE__, line_numbers[2], Symbol(""), -1, false, 0),
        StackFrame(:grandparent, @__FILE__, line_numbers[3], Symbol(""), -1, false, 0)
    ]
end

@testset "try...catch" begin
    stack = good_function()
    @test stack[1:2] == [
        StackFrame(:bad_function, @__FILE__, e_line_numbers[1], Symbol(""), -1, false, 0),
        StackFrame(:good_function, @__FILE__, e_line_numbers[2], Symbol(""), -1, false, 0)
    ]
end

@testset "formatting" begin
    @testset "frame" begin
        @test format_stackframe(format_stack[1]) ==
            "[inlined code from file.1:10] frame1 at file.inline:0"
        @test format_stackframe(format_stack[1]; full_path=true) ==
            "[inlined code from path/file.1:10] frame1 at path/file.inline:0"

        @test format_stackframe(format_stack[2]) == "frame2 at file.2:20"
        @test format_stackframe(format_stack[2]; full_path=true) ==
            "frame2 at path/file.2:20"
    end

    @testset "stack" begin
        @test format_stacktrace(format_stack, ", ") ==
            "[inlined code from file.1:10] frame1 at file.inline:0, frame2 at file.2:20"
        @test format_stacktrace(format_stack, ", "; full_path=true) ==
            string(
                "[inlined code from path/file.1:10] ",
                "frame1 at path/file.inline:0, frame2 at path/file.2:20"
            )

        @test format_stacktrace(format_stack, ", ", "Stack: ") ==
            string(
                "Stack: [inlined code from file.1:10] ",
                "frame1 at file.inline:0, frame2 at file.2:20"
            )
        @test format_stacktrace(format_stack, ", ", "Stack: "; full_path=true) ==
            string(
                "Stack: [inlined code from path/file.1:10] ",
                "frame1 at path/file.inline:0, frame2 at path/file.2:20"
            )

        @test format_stacktrace(format_stack, ", ", "{", "}") ==
            string(
                "{[inlined code from file.1:10] ",
                "frame1 at file.inline:0, frame2 at file.2:20}"
            )
        @test format_stacktrace(format_stack, ", ", "{", "}", full_path=true) ==
            string(
                "{[inlined code from path/file.1:10] ",
                "frame1 at path/file.inline:0, frame2 at path/file.2:20}"
            )
    end

    @testset "empty" begin
        @test format_stacktrace(StackTrace(), ", ") == ""
        @test format_stacktrace(StackTrace(), ", ", "Stack: ") == ""
        @test format_stacktrace(StackTrace(), ", ", "{", "}") == ""
    end
end

@testset "output" begin
    io = IOBuffer()
    show_stacktrace(io, format_stack)
    @test takebuf_string(io) ==
        """
        StackTrace with 2 StackFrames:
          [inlined code from file.1:10] frame1 at file.inline:0
          frame2 at file.2:20
        """
    show_stacktrace()   # Improves code coverage.
end
