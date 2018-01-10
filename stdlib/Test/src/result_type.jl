
"""
    Result

All tests produce a result object. This object may or may not be
stored, depending on whether the test is part of a test set.
"""
abstract type Result end

"""
    Pass

The test condition was true, i.e. the expression evaluated to true or
the correct exception was thrown.
"""
struct Pass <: Result
    test_type::Symbol
    orig_expr
    data
    value
end
function Base.show(io::IO, t::Pass)
    print_with_color(:green, io, "Test Passed"; bold = true)
    if !(t.orig_expr === nothing)
        print(io, "\n  Expression: ", t.orig_expr)
    end
    if t.test_type == :test_throws
        # The correct type of exception was thrown
        print(io, "\n      Thrown: ", typeof(t.value))
    elseif t.test_type == :test && t.data !== nothing
        # The test was an expression, so display the term-by-term
        # evaluated version as well
        print(io, "\n   Evaluated: ", t.data)
    end
end

"""
    Fail

The test condition was false, i.e. the expression evaluated to false or
the correct exception was not thrown.
"""
mutable struct Fail <: Result
    test_type::Symbol
    orig_expr
    data
    value
    source::LineNumberNode
end
function Base.show(io::IO, t::Fail)
    print_with_color(Base.error_color(), io, "Test Failed"; bold = true)
    print(io, " at ")
    print_with_color(:default, io, t.source.file, ":", t.source.line, "\n"; bold = true)
    print(io, "  Expression: ", t.orig_expr)
    if t.test_type == :test_throws_wrong
        # An exception was thrown, but it was of the wrong type
        print(io, "\n    Expected: ", t.data)
        print(io, "\n      Thrown: ", isa(t.data, Type) ? typeof(t.value) : t.value)
    elseif t.test_type == :test_throws_nothing
        # An exception was expected, but no exception was thrown
        print(io, "\n    Expected: ", t.data)
        print(io, "\n  No exception thrown")
    elseif t.test_type == :test && t.data !== nothing
        # The test was an expression, so display the term-by-term
        # evaluated version as well
        print(io, "\n   Evaluated: ", t.data)
    end
end

"""
    Error

The test condition couldn't be evaluated due to an exception, or
it evaluated to something other than a [`Bool`](@ref).
In the case of `@test_broken` it is used to indicate that an
unexpected `Pass` `Result` occurred.
"""
mutable struct Error <: Result
    test_type::Symbol
    orig_expr
    value
    backtrace
    source::LineNumberNode
end
function Base.show(io::IO, t::Error)
    if t.test_type == :test_interrupted
        print_with_color(Base.error_color(), io, "Interrupted")
        return
    end
    print_with_color(Base.error_color(), io, "Error During Test"; bold = true)
    print(io, " at ")
    print_with_color(:default, io, t.source.file, ":", t.source.line, "\n"; bold = true)
    if t.test_type == :test_nonbool
        println(io, "  Expression evaluated to non-Boolean")
        println(io, "  Expression: ", t.orig_expr)
        print(  io, "       Value: ", t.value)
    elseif t.test_type == :test_error
        println(io, "  Test threw an exception of type ", typeof(t.value))
        println(io, "  Expression: ", t.orig_expr)
        # Capture error message and indent to match
        errmsg = sprint(showerror, t.value, scrub_backtrace(t.backtrace))
        print(io, join(map(line->string("  ",line),
                            split(errmsg, "\n")), "\n"))
    elseif t.test_type == :test_unbroken
        # A test that was expected to fail did not
        println(io, " Unexpected Pass")
        println(io, " Expression: ", t.orig_expr)
        println(io, " Got correct result, please change to @test if no longer broken.")
    elseif t.test_type == :nontest_error
        # we had an error outside of a @test
        println(io, "  Got an exception of type $(typeof(t.value)) outside of a @test")
        # Capture error message and indent to match
        errmsg = sprint(showerror, t.value, t.backtrace)
        print(io, join(map(line->string("  ",line),
                            split(errmsg, "\n")), "\n"))
    end
end

"""
    Broken

The test condition is the expected (failed) result of a broken test,
or was explicitly skipped with `@test_skip`.
"""
mutable struct Broken <: Result
    test_type::Symbol
    orig_expr
end
function Base.show(io::IO, t::Broken)
    print_with_color(Base.warn_color(), io, "Test Broken\n"; bold = true)
    if t.test_type == :skipped && !(t.orig_expr === nothing)
        println(io, "  Skipped: ", t.orig_expr)
    elseif !(t.orig_expr === nothing)
        println(io, "Expression: ", t.orig_expr)
    end
end

#-----------------------------------------------------------------------

abstract type ExecutionResult end
