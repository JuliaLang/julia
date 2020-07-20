# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Simple unit testing functionality:

* `@test`
* `@test_throws`

All tests belong to a *test set*. There is a default, task-level
test set that throws on the first failure. Users can choose to wrap
their tests in (possibly nested) test sets that will store results
and summarize them at the end of the test set with `@testset`.
"""
module Test

export @test, @test_throws, @test_broken, @test_skip,
    @test_warn, @test_nowarn,
    @test_logs, @test_deprecated

export @testset
# Legacy approximate testing functions, yet to be included
export @inferred
export detect_ambiguities, detect_unbound_args
export GenericString, GenericSet, GenericDict, GenericArray, GenericOrder
export TestSetException

import Distributed: myid

using Random
using Random: AbstractRNG, default_rng
using InteractiveUtils: gen_call_with_extracted_types
using Core.Compiler: typesubtract

const DISPLAY_FAILED = (
    :isequal,
    :isapprox,
    :≈,
    :occursin,
    :startswith,
    :endswith,
    :isempty,
)

#-----------------------------------------------------------------------

# Backtrace utility functions
function ip_has_file_and_func(ip, file, funcs)
    return any(fr -> (string(fr.file) == file && fr.func in funcs), StackTraces.lookup(ip))
end

function scrub_backtrace(bt)
    do_test_ind = findfirst(ip -> ip_has_file_and_func(ip, @__FILE__, (:do_test, :do_test_throws)), bt)
    if do_test_ind !== nothing && length(bt) > do_test_ind
        bt = bt[do_test_ind + 1:end]
    end
    name_ind = findfirst(ip -> ip_has_file_and_func(ip, @__FILE__, (Symbol("macro expansion"),)), bt)
    if name_ind !== nothing && length(bt) != 0
        bt = bt[1:name_ind]
    end
    return bt
end

function scrub_exc_stack(stack)
    return Any[ (x[1], scrub_backtrace(x[2])) for x in stack ]
end

# define most of the test infrastructure without type specialization
@nospecialize

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
    printstyled(io, "Test Passed"; bold = true, color=:green)
    if !(t.orig_expr === nothing)
        print(io, "\n  Expression: ", t.orig_expr)
    end
    if t.test_type === :test_throws
        # The correct type of exception was thrown
        print(io, "\n      Thrown: ", typeof(t.value))
    elseif t.test_type === :test && t.data !== nothing
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
    printstyled(io, "Test Failed"; bold=true, color=Base.error_color())
    print(io, " at ")
    printstyled(io, something(t.source.file, :none), ":", t.source.line, "\n"; bold=true, color=:default)
    print(io, "  Expression: ", t.orig_expr)
    if t.test_type === :test_throws_wrong
        # An exception was thrown, but it was of the wrong type
        print(io, "\n    Expected: ", t.data)
        print(io, "\n      Thrown: ", isa(t.data, Type) ? typeof(t.value) : t.value)
    elseif t.test_type === :test_throws_nothing
        # An exception was expected, but no exception was thrown
        print(io, "\n    Expected: ", t.data)
        print(io, "\n  No exception thrown")
    elseif t.test_type === :test && t.data !== nothing
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

    function Error(test_type, orig_expr, value, bt, source)
        if test_type === :test_error
            bt = scrub_exc_stack(bt)
        end
        if test_type === :test_error || test_type === :nontest_error
            bt_str = sprint(Base.show_exception_stack, bt; context=stdout)
        else
            bt_str = ""
        end
        new(test_type,
            orig_expr,
            sprint(show, value, context = :limit => true),
            bt_str,
            source)
    end
end
function Base.show(io::IO, t::Error)
    if t.test_type === :test_interrupted
        printstyled(io, "Interrupted", color=Base.error_color())
        return
    end
    printstyled(io, "Error During Test"; bold=true, color=Base.error_color())
    print(io, " at ")
    printstyled(io, something(t.source.file, :none), ":", t.source.line, "\n"; bold=true, color=:default)
    if t.test_type === :test_nonbool
        println(io, "  Expression evaluated to non-Boolean")
        println(io, "  Expression: ", t.orig_expr)
        print(  io, "       Value: ", t.value)
    elseif t.test_type === :test_error
        println(io, "  Test threw exception")
        println(io, "  Expression: ", t.orig_expr)
        # Capture error message and indent to match
        join(io, ("  " * line for line in split(t.backtrace, "\n")), "\n")
    elseif t.test_type === :test_unbroken
        # A test that was expected to fail did not
        println(io, " Unexpected Pass")
        println(io, " Expression: ", t.orig_expr)
        println(io, " Got correct result, please change to @test if no longer broken.")
    elseif t.test_type === :nontest_error
        # we had an error outside of a @test
        println(io, "  Got exception outside of a @test")
        # Capture error message and indent to match
        join(io, ("  " * line for line in split(t.backtrace, "\n")), "\n")
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
    printstyled(io, "Test Broken\n"; bold=true, color=Base.warn_color())
    if t.test_type === :skipped && !(t.orig_expr === nothing)
        print(io, "  Skipped: ", t.orig_expr)
    elseif !(t.orig_expr === nothing)
        print(io, "  Expression: ", t.orig_expr)
    end
end

#-----------------------------------------------------------------------

abstract type ExecutionResult end

struct Returned <: ExecutionResult
    value
    data
    source::LineNumberNode
end

struct Threw <: ExecutionResult
    exception
    backtrace
    source::LineNumberNode
end

function eval_test(evaluated::Expr, quoted::Expr, source::LineNumberNode, negate::Bool=false)
    res = true
    i = 1
    evaled_args = evaluated.args
    quoted_args = quoted.args
    n = length(evaled_args)
    kw_suffix = ""
    if evaluated.head === :comparison
        args = evaled_args
        while i < n
            a, op, b = args[i], args[i+1], args[i+2]
            if res
                res = op(a, b) === true  # Keep `res` type stable
            end
            quoted_args[i] = a
            quoted_args[i+2] = b
            i += 2
        end

    elseif evaluated.head === :call
        op = evaled_args[1]
        kwargs = evaled_args[2].args  # Keyword arguments from `Expr(:parameters, ...)`
        args = evaled_args[3:n]

        res = op(args...; kwargs...) === true

        # Create "Evaluated" expression which looks like the original call but has all of
        # the arguments evaluated
        func_sym = quoted_args[1]
        if isempty(kwargs)
            quoted = Expr(:call, func_sym, args...)
        elseif func_sym === :≈ && !res
            quoted = Expr(:call, func_sym, args...)
            kw_suffix = " ($(join(["$k=$v" for (k, v) in kwargs], ", ")))"
        else
            kwargs_expr = Expr(:parameters, [Expr(:kw, k, v) for (k, v) in kwargs]...)
            quoted = Expr(:call, func_sym, kwargs_expr, args...)
        end
    else
        throw(ArgumentError("Unhandled expression type: $(evaluated.head)"))
    end

    if negate
        res = !res
        quoted = Expr(:call, :!, quoted)
    end

    Returned(res,
             # stringify arguments in case of failure, for easy remote printing
             res ? quoted : sprint(io->print(IOContext(io, :limit => true), quoted))*kw_suffix,
             source)
end

const comparison_prec = Base.operator_precedence(:(==))

"""
    test_expr!(ex, kws...)

Preprocess test expressions of function calls with trailing keyword arguments
so that e.g. `@test a ≈ b atol=ε` means `@test ≈(a, b, atol=ε)`.
"""
test_expr!(m, ex) = ex

function test_expr!(m, ex, kws...)
    ex isa Expr && ex.head === :call || @goto fail
    for kw in kws
        kw isa Expr && kw.head === :(=) || @goto fail
        kw.head = :kw
        push!(ex.args, kw)
    end
    return ex
@label fail
    error("invalid test macro call: $m $ex $(join(kws," "))")
end

# @test - check if the expression evaluates to true
"""
    @test ex
    @test f(args...) key=val ...

Tests that the expression `ex` evaluates to `true`.
Returns a `Pass` `Result` if it does, a `Fail` `Result` if it is
`false`, and an `Error` `Result` if it could not be evaluated.

# Examples
```jldoctest
julia> @test true
Test Passed

julia> @test [1, 2] + [2, 1] == [3, 3]
Test Passed
```

The `@test f(args...) key=val...` form is equivalent to writing
`@test f(args..., key=val...)` which can be useful when the expression
is a call using infix syntax such as approximate comparisons:

```jldoctest
julia> @test π ≈ 3.14 atol=0.01
Test Passed
```

This is equivalent to the uglier test `@test ≈(π, 3.14, atol=0.01)`.
It is an error to supply more than one expression unless the first
is a call expression and the rest are assignments (`k=v`).
"""
macro test(ex, kws...)
    test_expr!("@test", ex, kws...)
    orig_ex = Expr(:inert, ex)
    result = get_test_result(ex, __source__)
    :(do_test($result, $orig_ex))
end

"""
    @test_broken ex
    @test_broken f(args...) key=val ...

Indicates a test that should pass but currently consistently fails.
Tests that the expression `ex` evaluates to `false` or causes an
exception. Returns a `Broken` `Result` if it does, or an `Error` `Result`
if the expression evaluates to `true`.

The `@test_broken f(args...) key=val...` form works as for the `@test` macro.

# Examples
```jldoctest
julia> @test_broken 1 == 2
Test Broken
  Expression: 1 == 2

julia> @test_broken 1 == 2 atol=0.1
Test Broken
  Expression: ==(1, 2, atol = 0.1)
```
"""
macro test_broken(ex, kws...)
    test_expr!("@test_broken", ex, kws...)
    orig_ex = Expr(:inert, ex)
    result = get_test_result(ex, __source__)
    # code to call do_test with execution result and original expr
    :(do_broken_test($result, $orig_ex))
end

"""
    @test_skip ex
    @test_skip f(args...) key=val ...

Marks a test that should not be executed but should be included in test
summary reporting as `Broken`. This can be useful for tests that intermittently
fail, or tests of not-yet-implemented functionality.

The `@test_skip f(args...) key=val...` form works as for the `@test` macro.

# Examples
```jldoctest
julia> @test_skip 1 == 2
Test Broken
  Skipped: 1 == 2

julia> @test_skip 1 == 2 atol=0.1
Test Broken
  Skipped: ==(1, 2, atol = 0.1)
```
"""
macro test_skip(ex, kws...)
    test_expr!("@test_skip", ex, kws...)
    orig_ex = Expr(:inert, ex)
    testres = :(Broken(:skipped, $orig_ex))
    :(record(get_testset(), $testres))
end

# An internal function, called by the code generated by the @test
# macro to get results of the test expression.
# In the special case of a comparison, e.g. x == 5, generate code to
# evaluate each term in the comparison individually so the results
# can be displayed nicely.
function get_test_result(ex, source)
    negate = QuoteNode(false)
    orig_ex = ex
    # Evaluate `not` wrapped functions separately for pretty-printing failures
    if isa(ex, Expr) && ex.head === :call && length(ex.args) == 2 && ex.args[1] === :!
        negate = QuoteNode(true)
        ex = ex.args[2]
    end
    # Normalize non-dot comparison operator calls to :comparison expressions
    is_splat = x -> isa(x, Expr) && x.head === :...
    if isa(ex, Expr) && ex.head === :call && length(ex.args) == 3 &&
        first(string(ex.args[1])) != '.' && !is_splat(ex.args[2]) && !is_splat(ex.args[3]) &&
        (ex.args[1] === :(==) || Base.operator_precedence(ex.args[1]) == comparison_prec)
        ex = Expr(:comparison, ex.args[2], ex.args[1], ex.args[3])
    end
    if isa(ex, Expr) && ex.head === :comparison
        # pass all terms of the comparison to `eval_comparison`, as an Expr
        escaped_terms = [esc(arg) for arg in ex.args]
        quoted_terms = [QuoteNode(arg) for arg in ex.args]
        testret = :(eval_test(
            Expr(:comparison, $(escaped_terms...)),
            Expr(:comparison, $(quoted_terms...)),
            $(QuoteNode(source)),
            $negate,
        ))
    elseif isa(ex, Expr) && ex.head === :call && ex.args[1] in DISPLAY_FAILED
        escaped_func = esc(ex.args[1])
        quoted_func = QuoteNode(ex.args[1])

        escaped_args = []
        escaped_kwargs = []

        # Keywords that occur before `;`. Note that the keywords are being revised into
        # a form we can splat.
        for a in ex.args[2:end]
            if isa(a, Expr) && a.head === :kw
                push!(escaped_kwargs, Expr(:call, :(=>), QuoteNode(a.args[1]), esc(a.args[2])))
            end
        end

        # Keywords that occur after ';'
        parameters_expr = ex.args[2]
        if isa(parameters_expr, Expr) && parameters_expr.head === :parameters
            for a in parameters_expr.args
                if isa(a, Expr) && a.head === :kw
                    push!(escaped_kwargs, Expr(:call, :(=>), QuoteNode(a.args[1]), esc(a.args[2])))
                elseif isa(a, Expr) && a.head === :...
                    push!(escaped_kwargs, Expr(:..., esc(a.args[1])))
                end
            end
        end

        # Positional arguments
        for a in ex.args[2:end]
            isa(a, Expr) && a.head in (:kw, :parameters) && continue

            if isa(a, Expr) && a.head === :...
                push!(escaped_args, Expr(:..., esc(a.args[1])))
            else
                push!(escaped_args, esc(a))
            end
        end

        testret = :(eval_test(
            Expr(:call, $escaped_func, Expr(:parameters, $(escaped_kwargs...)), $(escaped_args...)),
            Expr(:call, $quoted_func),
            $(QuoteNode(source)),
            $negate,
        ))
    else
        testret = :(Returned($(esc(orig_ex)), nothing, $(QuoteNode(source))))
    end
    result = quote
        try
            $testret
        catch _e
            _e isa InterruptException && rethrow()
            Threw(_e, Base.catch_stack(), $(QuoteNode(source)))
        end
    end
    Base.remove_linenums!(result)
    result
end

# An internal function, called by the code generated by the @test
# macro to actually perform the evaluation and manage the result.
function do_test(result::ExecutionResult, orig_expr)
    # get_testset() returns the most recently added test set
    # We then call record() with this test set and the test result
    if isa(result, Returned)
        # expr, in the case of a comparison, will contain the
        # comparison with evaluated values of each term spliced in.
        # For anything else, just contains the test expression.
        # value is the evaluated value of the whole test expression.
        # Ideally it is true, but it may be false or non-Boolean.
        value = result.value
        testres = if isa(value, Bool)
            # a true value Passes
            value ? Pass(:test, nothing, nothing, value) :
                    Fail(:test, orig_expr, result.data, value, result.source)
        else
            # If the result is non-Boolean, this counts as an Error
            Error(:test_nonbool, orig_expr, value, nothing, result.source)
        end
    else
        # The predicate couldn't be evaluated without throwing an
        # exception, so that is an Error and not a Fail
        @assert isa(result, Threw)
        testres = Error(:test_error, orig_expr, result.exception, result.backtrace, result.source)
    end
    record(get_testset(), testres)
end

function do_broken_test(result::ExecutionResult, orig_expr)
    testres = Broken(:test, orig_expr)
    # Assume the test is broken and only change if the result is true
    if isa(result, Returned)
        value = result.value
        if isa(value, Bool) && value
            testres = Error(:test_unbroken, orig_expr, value, nothing, result.source)
        end
    end
    record(get_testset(), testres)
end

#-----------------------------------------------------------------------

"""
    @test_throws exception expr

Tests that the expression `expr` throws `exception`.
The exception may specify either a type,
or a value (which will be tested for equality by comparing fields).
Note that `@test_throws` does not support a trailing keyword form.

# Examples
```jldoctest
julia> @test_throws BoundsError [1, 2, 3][4]
Test Passed
      Thrown: BoundsError

julia> @test_throws DimensionMismatch [1, 2, 3] + [1, 2]
Test Passed
      Thrown: DimensionMismatch
```
"""
macro test_throws(extype, ex)
    orig_ex = Expr(:inert, ex)
    result = quote
        try
            Returned($(esc(ex)), nothing, $(QuoteNode(__source__)))
        catch _e
            if $(esc(extype)) != InterruptException && _e isa InterruptException
                rethrow()
            end
            Threw(_e, nothing, $(QuoteNode(__source__)))
        end
    end
    Base.remove_linenums!(result)
    :(do_test_throws($result, $orig_ex, $(esc(extype))))
end

# An internal function, called by the code generated by @test_throws
# to evaluate and catch the thrown exception - if it exists
function do_test_throws(result::ExecutionResult, orig_expr, extype)
    if isa(result, Threw)
        # Check that the right type of exception was thrown
        success = false
        exc = result.exception
        if isa(extype, Type)
            success = isa(exc, extype)
        else
            if isa(exc, typeof(extype))
                success = true
                for fld in 1:nfields(extype)
                    if !isequal(getfield(extype, fld), getfield(exc, fld))
                        success = false
                        break
                    end
                end
            end
        end
        if success
            testres = Pass(:test_throws, nothing, nothing, exc)
        else
            testres = Fail(:test_throws_wrong, orig_expr, extype, exc, result.source)
        end
    else
        testres = Fail(:test_throws_nothing, orig_expr, extype, nothing, result.source)
    end
    record(get_testset(), testres)
end

#-----------------------------------------------------------------------
# Test for log messages

# Test for warning messages (deprecated)

contains_warn(output, s::AbstractString) = occursin(s, output)
contains_warn(output, s::Regex) = occursin(s, output)
contains_warn(output, s::Function) = s(output)
contains_warn(output, S::Union{AbstractArray,Tuple}) = all(s -> contains_warn(output, s), S)

"""
    @test_warn msg expr

Test whether evaluating `expr` results in [`stderr`](@ref) output that contains
the `msg` string or matches the `msg` regular expression.  If `msg` is
a boolean function, tests whether `msg(output)` returns `true`.  If `msg` is a
tuple or array, checks that the error output contains/matches each item in `msg`.
Returns the result of evaluating `expr`.

See also [`@test_nowarn`](@ref) to check for the absence of error output.

Note: Warnings generated by `@warn` cannot be tested with this macro. Use
`@test_logs` instead.
"""
macro test_warn(msg, expr)
    quote
        let fname = tempname()
            try
                ret = open(fname, "w") do f
                    redirect_stderr(f) do
                        $(esc(expr))
                    end
                end
                @test contains_warn(read(fname, String), $(esc(msg)))
                ret
            finally
                rm(fname, force=true)
            end
        end
    end
end

"""
    @test_nowarn expr

Test whether evaluating `expr` results in empty [`stderr`](@ref) output
(no warnings or other messages).  Returns the result of evaluating `expr`.

Note: The absence of warnings generated by `@warn` cannot be tested
with this macro. Use `@test_logs expr` instead.
"""
macro test_nowarn(expr)
    quote
        @test_warn r"^(?!.)"s $(esc(expr))
    end
end

#-----------------------------------------------------------------------

# The AbstractTestSet interface is defined by two methods:
# record(AbstractTestSet, Result)
#   Called by do_test after a test is evaluated
# finish(AbstractTestSet)
#   Called after the test set has been popped from the test set stack
abstract type AbstractTestSet end

"""
    record(ts::AbstractTestSet, res::Result)

Record a result to a testset. This function is called by the `@testset`
infrastructure each time a contained `@test` macro completes, and is given the
test result (which could be an `Error`). This will also be called with an `Error`
if an exception is thrown inside the test block but outside of a `@test` context.
"""
function record end

"""
    finish(ts::AbstractTestSet)

Do any final processing necessary for the given testset. This is called by the
`@testset` infrastructure after a test block executes. One common use for this
function is to record the testset to the parent's results list, using
`get_testset`.
"""
function finish end

"""
    TestSetException

Thrown when a test set finishes and not all tests passed.
"""
struct TestSetException <: Exception
    pass::Int
    fail::Int
    error::Int
    broken::Int
    errors_and_fails::Vector{Union{Fail, Error}}
end

function Base.show(io::IO, ex::TestSetException)
    print(io, "Some tests did not pass: ")
    print(io, ex.pass,  " passed, ")
    print(io, ex.fail,  " failed, ")
    print(io, ex.error, " errored, ")
    print(io, ex.broken, " broken.")
end

function Base.showerror(io::IO, ex::TestSetException, bt; backtrace=true)
    printstyled(io, string(ex), color=Base.error_color())
end

#-----------------------------------------------------------------------

"""
    FallbackTestSet

A simple fallback test set that throws immediately on a failure.
"""
struct FallbackTestSet <: AbstractTestSet end
fallback_testset = FallbackTestSet()

struct FallbackTestSetException <: Exception
    msg::AbstractString
end

function Base.showerror(io::IO, ex::FallbackTestSetException, bt; backtrace=true)
    printstyled(io, ex.msg, color=Base.error_color())
end

# Records nothing, and throws an error immediately whenever a Fail or
# Error occurs. Takes no action in the event of a Pass or Broken result
record(ts::FallbackTestSet, t::Union{Pass,Broken}) = t
function record(ts::FallbackTestSet, t::Union{Fail,Error})
    println(t)
    throw(FallbackTestSetException("There was an error during testing"))
end
# We don't need to do anything as we don't record anything
finish(ts::FallbackTestSet) = ts

#-----------------------------------------------------------------------

"""
    DefaultTestSet

If using the DefaultTestSet, the test results will be recorded. If there
are any `Fail`s or `Error`s, an exception will be thrown only at the end,
along with a summary of the test results.
"""
mutable struct DefaultTestSet <: AbstractTestSet
    description::AbstractString
    results::Vector
    n_passed::Int
    anynonpass::Bool
end
DefaultTestSet(desc) = DefaultTestSet(desc, [], 0, false)

# For a broken result, simply store the result
record(ts::DefaultTestSet, t::Broken) = (push!(ts.results, t); t)
# For a passed result, do not store the result since it uses a lot of memory
record(ts::DefaultTestSet, t::Pass) = (ts.n_passed += 1; t)

# For the other result types, immediately print the error message
# but do not terminate. Print a backtrace.
function record(ts::DefaultTestSet, t::Union{Fail, Error})
    if myid() == 1
        printstyled(ts.description, ": ", color=:white)
        # don't print for interrupted tests
        if !(t isa Error) || t.test_type !== :test_interrupted
            print(t)
            if !isa(t, Error) # if not gets printed in the show method
                Base.show_backtrace(stdout, scrub_backtrace(backtrace()))
            end
            println()
        end
    end
    push!(ts.results, t)
    isa(t, Error) || backtrace()
    return t
end

# When a DefaultTestSet finishes, it records itself to its parent
# testset, if there is one. This allows for recursive printing of
# the results at the end of the tests
record(ts::DefaultTestSet, t::AbstractTestSet) = push!(ts.results, t)

@specialize

function print_test_errors(ts::DefaultTestSet)
    for t in ts.results
        if (isa(t, Error) || isa(t, Fail)) && myid() == 1
            println("Error in testset $(ts.description):")
            Base.show(stdout,t)
            println()
        elseif isa(t, DefaultTestSet)
            print_test_errors(t)
        end
    end
end

function print_test_results(ts::DefaultTestSet, depth_pad=0)
    # Calculate the overall number for each type so each of
    # the test result types are aligned
    passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = get_test_counts(ts)
    total_pass   = passes + c_passes
    total_fail   = fails  + c_fails
    total_error  = errors + c_errors
    total_broken = broken + c_broken
    dig_pass   = total_pass   > 0 ? ndigits(total_pass)   : 0
    dig_fail   = total_fail   > 0 ? ndigits(total_fail)   : 0
    dig_error  = total_error  > 0 ? ndigits(total_error)  : 0
    dig_broken = total_broken > 0 ? ndigits(total_broken) : 0
    total = total_pass + total_fail + total_error + total_broken
    dig_total = total > 0 ? ndigits(total) : 0
    # For each category, take max of digits and header width if there are
    # tests of that type
    pass_width   = dig_pass   > 0 ? max(length("Pass"),   dig_pass)   : 0
    fail_width   = dig_fail   > 0 ? max(length("Fail"),   dig_fail)   : 0
    error_width  = dig_error  > 0 ? max(length("Error"),  dig_error)  : 0
    broken_width = dig_broken > 0 ? max(length("Broken"), dig_broken) : 0
    total_width  = dig_total  > 0 ? max(length("Total"),  dig_total)  : 0
    # Calculate the alignment of the test result counts by
    # recursively walking the tree of test sets
    align = max(get_alignment(ts, 0), length("Test Summary:"))
    # Print the outer test set header once
    pad = total == 0 ? "" : " "
    printstyled(rpad("Test Summary:", align, " "), " |", pad; bold=true, color=:white)
    if pass_width > 0
        printstyled(lpad("Pass", pass_width, " "), "  "; bold=true, color=:green)
    end
    if fail_width > 0
        printstyled(lpad("Fail", fail_width, " "), "  "; bold=true, color=Base.error_color())
    end
    if error_width > 0
        printstyled(lpad("Error", error_width, " "), "  "; bold=true, color=Base.error_color())
    end
    if broken_width > 0
        printstyled(lpad("Broken", broken_width, " "), "  "; bold=true, color=Base.warn_color())
    end
    if total_width > 0
        printstyled(lpad("Total", total_width, " "); bold=true, color=Base.info_color())
    end
    println()
    # Recursively print a summary at every level
    print_counts(ts, depth_pad, align, pass_width, fail_width, error_width, broken_width, total_width)
end


const TESTSET_PRINT_ENABLE = Ref(true)

# Called at the end of a @testset, behaviour depends on whether
# this is a child of another testset, or the "root" testset
function finish(ts::DefaultTestSet)
    # If we are a nested test set, do not print a full summary
    # now - let the parent test set do the printing
    if get_testset_depth() != 0
        # Attach this test set to the parent test set
        parent_ts = get_testset()
        record(parent_ts, ts)
        return ts
    end
    passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = get_test_counts(ts)
    total_pass   = passes + c_passes
    total_fail   = fails  + c_fails
    total_error  = errors + c_errors
    total_broken = broken + c_broken
    total = total_pass + total_fail + total_error + total_broken

    if TESTSET_PRINT_ENABLE[]
        print_test_results(ts)
    end

    # Finally throw an error as we are the outermost test set
    if total != total_pass + total_broken
        # Get all the error/failures and bring them along for the ride
        efs = filter_errors(ts)
        throw(TestSetException(total_pass,total_fail,total_error, total_broken, efs))
    end

    # return the testset so it is returned from the @testset macro
    ts
end

# Recursive function that finds the column that the result counts
# can begin at by taking into account the width of the descriptions
# and the amount of indentation. If a test set had no failures, and
# no failures in child test sets, there is no need to include those
# in calculating the alignment
function get_alignment(ts::DefaultTestSet, depth::Int)
    # The minimum width at this depth is
    ts_width = 2*depth + length(ts.description)
    # If all passing, no need to look at children
    !ts.anynonpass && return ts_width
    # Return the maximum of this width and the minimum width
    # for all children (if they exist)
    isempty(ts.results) && return ts_width
    child_widths = map(t->get_alignment(t, depth+1), ts.results)
    return max(ts_width, maximum(child_widths))
end
get_alignment(ts, depth::Int) = 0

# Recursive function that fetches backtraces for any and all errors
# or failures the testset and its children encountered
function filter_errors(ts::DefaultTestSet)
    efs = []
    for t in ts.results
        if isa(t, DefaultTestSet)
            append!(efs, filter_errors(t))
        elseif isa(t, Union{Fail, Error})
            append!(efs, [t])
        end
    end
    efs
end

# Recursive function that counts the number of test results of each
# type directly in the testset, and totals across the child testsets
function get_test_counts(ts::DefaultTestSet)
    passes, fails, errors, broken = ts.n_passed, 0, 0, 0
    c_passes, c_fails, c_errors, c_broken = 0, 0, 0, 0
    for t in ts.results
        isa(t, Fail)   && (fails  += 1)
        isa(t, Error)  && (errors += 1)
        isa(t, Broken) && (broken += 1)
        if isa(t, DefaultTestSet)
            np, nf, ne, nb, ncp, ncf, nce , ncb = get_test_counts(t)
            c_passes += np + ncp
            c_fails  += nf + ncf
            c_errors += ne + nce
            c_broken += nb + ncb
        end
    end
    ts.anynonpass = (fails + errors + c_fails + c_errors > 0)
    return passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken
end

# Recursive function that prints out the results at each level of
# the tree of test sets
function print_counts(ts::DefaultTestSet, depth, align,
                      pass_width, fail_width, error_width, broken_width, total_width)
    # Count results by each type at this level, and recursively
    # through any child test sets
    passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken = get_test_counts(ts)
    subtotal = passes + fails + errors + broken + c_passes + c_fails + c_errors + c_broken
    # Print test set header, with an alignment that ensures all
    # the test results appear above each other
    print(rpad(string("  "^depth, ts.description), align, " "), " | ")

    np = passes + c_passes
    if np > 0
        printstyled(lpad(string(np), pass_width, " "), "  ", color=:green)
    elseif pass_width > 0
        # No passes at this level, but some at another level
        print(lpad(" ", pass_width), "  ")
    end

    nf = fails + c_fails
    if nf > 0
        printstyled(lpad(string(nf), fail_width, " "), "  ", color=Base.error_color())
    elseif fail_width > 0
        # No fails at this level, but some at another level
        print(lpad(" ", fail_width), "  ")
    end

    ne = errors + c_errors
    if ne > 0
        printstyled(lpad(string(ne), error_width, " "), "  ", color=Base.error_color())
    elseif error_width > 0
        # No errors at this level, but some at another level
        print(lpad(" ", error_width), "  ")
    end

    nb = broken + c_broken
    if nb > 0
        printstyled(lpad(string(nb), broken_width, " "), "  ", color=Base.warn_color())
    elseif broken_width > 0
        # None broken at this level, but some at another level
        print(lpad(" ", broken_width), "  ")
    end

    if np == 0 && nf == 0 && ne == 0 && nb == 0
        printstyled("No tests", color=Base.info_color())
    else
        printstyled(lpad(string(subtotal), total_width, " "), color=Base.info_color())
    end
    println()

    # Only print results at lower levels if we had failures
    if np + nb != subtotal
        for t in ts.results
            if isa(t, DefaultTestSet)
                print_counts(t, depth + 1, align,
                    pass_width, fail_width, error_width, broken_width, total_width)
            end
        end
    end
end

#-----------------------------------------------------------------------

function _check_testset(testsettype, testsetname)
    if !(testsettype isa Type && testsettype <: AbstractTestSet)
        error("Expected `$testsetname` to be an AbstractTestSet, it is a ",
              typeof(testsettype), ". ",
              typeof(testsettype) == String ?
                  """
                  To use `$testsetname` as a testset name, interpolate it into a string, e.g:
                      @testset "\$$testsetname" begin
                          ...
                      end"""
             :
                  ""
            )
    end
end

"""
    @testset [CustomTestSet] [option=val  ...] ["description"] begin ... end
    @testset [CustomTestSet] [option=val  ...] ["description \$v"] for v in (...) ... end
    @testset [CustomTestSet] [option=val  ...] ["description \$v, \$w"] for v in (...), w in (...) ... end

Starts a new test set, or multiple test sets if a `for` loop is provided.

If no custom testset type is given it defaults to creating a `DefaultTestSet`.
`DefaultTestSet` records all the results and, if there are any `Fail`s or
`Error`s, throws an exception at the end of the top-level (non-nested) test set,
along with a summary of the test results.

Any custom testset type (subtype of `AbstractTestSet`) can be given and it will
also be used for any nested `@testset` invocations. The given options are only
applied to the test set where they are given. The default test set type does
not take any options.

The description string accepts interpolation from the loop indices.
If no description is provided, one is constructed based on the variables.

By default the `@testset` macro will return the testset object itself, though
this behavior can be customized in other testset types. If a `for` loop is used
then the macro collects and returns a list of the return values of the `finish`
method, which by default will return a list of the testset objects used in
each iteration.

Before the execution of the body of a `@testset`, there is an implicit
call to `Random.seed!(seed)` where `seed` is the current seed of the global RNG.
Moreover, after the execution of the body, the state of the global RNG is
restored to what it was before the `@testset`. This is meant to ease
reproducibility in case of failure, and to allow seamless
re-arrangements of `@testset`s regardless of their side-effect on the
global RNG state.

# Examples
```jldoctest
julia> @testset "trigonometric identities" begin
           θ = 2/3*π
           @test sin(-θ) ≈ -sin(θ)
           @test cos(-θ) ≈ cos(θ)
           @test sin(2θ) ≈ 2*sin(θ)*cos(θ)
           @test cos(2θ) ≈ cos(θ)^2 - sin(θ)^2
       end;
Test Summary:            | Pass  Total
trigonometric identities |    4      4
```
"""
macro testset(args...)
    isempty(args) && error("No arguments to @testset")

    tests = args[end]

    # Determine if a single block or for-loop style
    if !isa(tests,Expr) || (tests.head !== :for && tests.head !== :block)
        error("Expected begin/end block or for loop as argument to @testset")
    end

    if tests.head === :for
        return testset_forloop(args, tests, __source__)
    else
        return testset_beginend(args, tests, __source__)
    end
end

"""
Generate the code for a `@testset` with a `begin`/`end` argument
"""
function testset_beginend(args, tests, source)
    desc, testsettype, options = parse_testset_args(args[1:end-1])
    if desc === nothing
        desc = "test set"
    end
    # If we're at the top level we'll default to DefaultTestSet. Otherwise
    # default to the type of the parent testset
    if testsettype === nothing
        testsettype = :(get_testset_depth() == 0 ? DefaultTestSet : typeof(get_testset()))
    end

    # Generate a block of code that initializes a new testset, adds
    # it to the task local storage, evaluates the test(s), before
    # finally removing the testset and giving it a chance to take
    # action (such as reporting the results)
    ex = quote
        _check_testset($testsettype, $(QuoteNode(testsettype.args[1])))
        local ret
        local ts = $(testsettype)($desc; $options...)
        push_testset(ts)
        # we reproduce the logic of guardseed, but this function
        # cannot be used as it changes slightly the semantic of @testset,
        # by wrapping the body in a function
        local RNG = default_rng()
        local oldrng = copy(RNG)
        try
            # RNG is re-seeded with its own seed to ease reproduce a failed test
            Random.seed!(RNG.seed)
            let
                $(esc(tests))
            end
        catch err
            err isa InterruptException && rethrow()
            # something in the test block threw an error. Count that as an
            # error in this test set
            record(ts, Error(:nontest_error, Expr(:tuple), err, Base.catch_stack(), $(QuoteNode(source))))
        finally
            copy!(RNG, oldrng)
            pop_testset()
            ret = finish(ts)
        end
        ret
    end
    # preserve outer location if possible
    if tests isa Expr && tests.head === :block && !isempty(tests.args) && tests.args[1] isa LineNumberNode
        ex = Expr(:block, tests.args[1], ex)
    end
    return ex
end


"""
Generate the code for a `@testset` with a `for` loop argument
"""
function testset_forloop(args, testloop, source)
    # Pull out the loop variables. We might need them for generating the
    # description and we'll definitely need them for generating the
    # comprehension expression at the end
    loopvars = Expr[]
    if testloop.args[1].head === :(=)
        push!(loopvars, testloop.args[1])
    elseif testloop.args[1].head === :block
        for loopvar in testloop.args[1].args
            push!(loopvars, loopvar)
        end
    else
        error("Unexpected argument to @testset")
    end

    desc, testsettype, options = parse_testset_args(args[1:end-1])

    if desc === nothing
        # No description provided. Generate from the loop variable names
        v = loopvars[1].args[1]
        desc = Expr(:string, "$v = ", esc(v)) # first variable
        for l = loopvars[2:end]
            v = l.args[1]
            push!(desc.args, ", $v = ")
            push!(desc.args, esc(v))
        end
    end

    if testsettype === nothing
        testsettype = :(get_testset_depth() == 0 ? DefaultTestSet : typeof(get_testset()))
    end

    # Uses a similar block as for `@testset`, except that it is
    # wrapped in the outer loop provided by the user
    tests = testloop.args[2]
    blk = quote
        _check_testset($testsettype, $(QuoteNode(testsettype.args[1])))
        # Trick to handle `break` and `continue` in the test code before
        # they can be handled properly by `finally` lowering.
        if !first_iteration
            pop_testset()
            push!(arr, finish(ts))
            # it's 1000 times faster to copy from tmprng rather than calling Random.seed!
            copy!(RNG, tmprng)

        end
        ts = $(testsettype)($desc; $options...)
        push_testset(ts)
        first_iteration = false
        try
            $(esc(tests))
        catch err
            err isa InterruptException && rethrow()
            # Something in the test block threw an error. Count that as an
            # error in this test set
            record(ts, Error(:nontest_error, Expr(:tuple), err, Base.catch_stack(), $(QuoteNode(source))))
        end
    end
    quote
        local arr = Vector{Any}()
        local first_iteration = true
        local ts
        local RNG = default_rng()
        local oldrng = copy(RNG)
        Random.seed!(RNG.seed)
        local tmprng = copy(RNG)
        try
            let
                $(Expr(:for, Expr(:block, [esc(v) for v in loopvars]...), blk))
            end
        finally
            # Handle `return` in test body
            if !first_iteration
                pop_testset()
                push!(arr, finish(ts))
            end
            copy!(RNG, oldrng)
        end
        arr
    end
end

"""
Parse the arguments to the `@testset` macro to pull out the description,
Testset Type, and options. Generally this should be called with all the macro
arguments except the last one, which is the test expression itself.
"""
function parse_testset_args(args)
    desc = nothing
    testsettype = nothing
    options = :(Dict{Symbol, Any}())
    for arg in args
        # a standalone symbol is assumed to be the test set we should use
        if isa(arg, Symbol)
            testsettype = esc(arg)
        # a string is the description
        elseif isa(arg, AbstractString) || (isa(arg, Expr) && arg.head === :string)
            desc = esc(arg)
        # an assignment is an option
        elseif isa(arg, Expr) && arg.head === :(=)
            # we're building up a Dict literal here
            key = Expr(:quote, arg.args[1])
            push!(options.args, Expr(:call, :(=>), key, arg.args[2]))
        else
            error("Unexpected argument $arg to @testset")
        end
    end

    (desc, testsettype, options)
end

#-----------------------------------------------------------------------
# Various helper methods for test sets

"""
    get_testset()

Retrieve the active test set from the task's local storage. If no
test set is active, use the fallback default test set.
"""
function get_testset()
    testsets = get(task_local_storage(), :__BASETESTNEXT__, AbstractTestSet[])
    return isempty(testsets) ? fallback_testset : testsets[end]
end

"""
    push_testset(ts::AbstractTestSet)

Adds the test set to the task_local_storage.
"""
function push_testset(ts::AbstractTestSet)
    testsets = get(task_local_storage(), :__BASETESTNEXT__, AbstractTestSet[])
    push!(testsets, ts)
    setindex!(task_local_storage(), testsets, :__BASETESTNEXT__)
end

"""
    pop_testset()

Pops the last test set added to the task_local_storage. If there are no
active test sets, returns the fallback default test set.
"""
function pop_testset()
    testsets = get(task_local_storage(), :__BASETESTNEXT__, AbstractTestSet[])
    ret = isempty(testsets) ? fallback_testset : pop!(testsets)
    setindex!(task_local_storage(), testsets, :__BASETESTNEXT__)
    return ret
end

"""
    get_testset_depth()

Returns the number of active test sets, not including the default test set
"""
function get_testset_depth()
    testsets = get(task_local_storage(), :__BASETESTNEXT__, AbstractTestSet[])
    return length(testsets)
end

_args_and_call(args...; kwargs...) = (args[1:end-1], kwargs, args[end](args[1:end-1]...; kwargs...))
_materialize_broadcasted(f, args...) = Broadcast.materialize(Broadcast.broadcasted(f, args...))

"""
    @inferred [AllowedType] f(x)

Tests that the call expression `f(x)` returns a value of the same type inferred by the
compiler. It is useful to check for type stability.

`f(x)` can be any call expression. Returns the result of `f(x)` if the types match, and an
`Error` `Result` if it finds different types.

Optionally, `AllowedType` relaxes the test, by making it pass when either the type of `f(x)`
matches the inferred type modulo `AllowedType`, or when the return type is a subtype of
`AllowedType`. This is useful when testing type stability of functions returning a small
union such as `Union{Nothing, T}` or `Union{Missing, T}`.

```jldoctest; setup = :(using InteractiveUtils), filter = r"begin\\n(.|\\n)*end"
julia> f(a) = a > 1 ? 1 : 1.0
f (generic function with 1 method)

julia> typeof(f(2))
Int64

julia> @code_warntype f(2)
Variables
  #self#::Core.Const(f, false)
  a::Int64

Body::UNION{FLOAT64, INT64}
1 ─ %1 = (a > 1)::Bool
└──      goto #3 if not %1
2 ─      return 1
3 ─      return 1.0

julia> @inferred f(2)
ERROR: return type Int64 does not match inferred return type Union{Float64, Int64}
[...]

julia> @inferred max(1, 2)
2

julia> g(a) = a < 10 ? missing : 1.0
g (generic function with 1 method)

julia> @inferred g(20)
ERROR: return type Float64 does not match inferred return type Union{Missing, Float64}
[...]

julia> @inferred Missing g(20)
1.0

julia> h(a) = a < 10 ? missing : f(a)
h (generic function with 1 method)

julia> @inferred Missing h(20)
ERROR: return type Int64 does not match inferred return type Union{Missing, Float64, Int64}
[...]
```
"""
macro inferred(ex)
    _inferred(ex, __module__)
end
macro inferred(allow, ex)
    _inferred(ex, __module__, allow)
end
function _inferred(ex, mod, allow = :(Union{}))
    if Meta.isexpr(ex, :ref)
        ex = Expr(:call, :getindex, ex.args...)
    end
    Meta.isexpr(ex, :call)|| error("@inferred requires a call expression")
    farg = ex.args[1]
    if isa(farg, Symbol) && first(string(farg)) == '.'
        farg = Symbol(string(farg)[2:end])
        ex = Expr(:call, GlobalRef(Test, :_materialize_broadcasted),
            farg, ex.args[2:end]...)
    end
    Base.remove_linenums!(quote
        let
            allow = $(esc(allow))
            allow isa Type || throw(ArgumentError("@inferred requires a type as second argument"))
            $(if any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex.args)
                # Has keywords
                args = gensym()
                kwargs = gensym()
                quote
                    $(esc(args)), $(esc(kwargs)), result = $(esc(Expr(:call, _args_and_call, ex.args[2:end]..., ex.args[1])))
                    inftypes = $(gen_call_with_extracted_types(mod, Base.return_types, :($(ex.args[1])($(args)...; $(kwargs)...))))
                end
            else
                # No keywords
                quote
                    args = ($([esc(ex.args[i]) for i = 2:length(ex.args)]...),)
                    result = $(esc(ex.args[1]))(args...)
                    inftypes = Base.return_types($(esc(ex.args[1])), Base.typesof(args...))
                end
            end)
            @assert length(inftypes) == 1
            rettype = result isa Type ? Type{result} : typeof(result)
            rettype <: allow || rettype == typesubtract(inftypes[1], allow) || error("return type $rettype does not match inferred return type $(inftypes[1])")
            result
        end
    end)
end

"""
    detect_ambiguities(mod1, mod2...; imported=false, recursive=false, ambiguous_bottom=false)

Returns a vector of `(Method,Method)` pairs of ambiguous methods
defined in the specified modules.
Use `imported=true` if you wish to also test functions that were
imported into these modules from elsewhere.
Use `recursive=true` to test in all submodules.

`ambiguous_bottom` controls whether ambiguities triggered only by
`Union{}` type parameters are included; in most cases you probably
want to set this to `false`. See [`Base.isambiguous`](@ref).
"""
function detect_ambiguities(mods...;
                            imported::Bool = false,
                            recursive::Bool = false,
                            ambiguous_bottom::Bool = false)
    function sortdefs(m1::Method, m2::Method)
        ord12 = m1.file < m2.file
        if !ord12 && (m1.file == m2.file)
            ord12 = m1.line < m2.line
        end
        return ord12 ? (m1, m2) : (m2, m1)
    end
    ambs = Set{Tuple{Method,Method}}()
    for mod in mods
        for n in names(mod, all = true, imported = imported)
            Base.isdeprecated(mod, n) && continue
            if !isdefined(mod, n)
                println("Skipping ", mod, '.', n)  # typically stale exports
                continue
            end
            f = Base.unwrap_unionall(getfield(mod, n))
            if recursive && isa(f, Module) && f !== mod && parentmodule(f) === mod && nameof(f) === n
                subambs = detect_ambiguities(f,
                    imported=imported, recursive=recursive, ambiguous_bottom=ambiguous_bottom)
                union!(ambs, subambs)
            elseif isa(f, DataType) && isdefined(f.name, :mt) && f.name.mt !== Symbol.name.mt
                mt = Base.MethodList(f.name.mt)
                for m in mt
                    ambig = Int32[0]
                    for m2 in Base._methods_by_ftype(m.sig, -1, typemax(UInt), true, UInt[typemin(UInt)], UInt[typemax(UInt)], ambig)
                        ambig[1] == 0 && break
                        m2 = m2[3]
                        if Base.isambiguous(m, m2, ambiguous_bottom=ambiguous_bottom)
                            push!(ambs, sortdefs(m, m2))
                        end
                    end
                end
            end
        end
    end
    function is_in_mods(m::Module)
        while true
            m in mods && return true
            recursive || return false
            p = parentmodule(m)
            p === m && return false
            m = p
        end
    end
    let mt = Base.MethodList(Symbol.name.mt)
        for m in mt
            if is_in_mods(m.module)
                ambig = Int32[0]
                for m2 in Base._methods_by_ftype(m.sig, -1, typemax(UInt), true, UInt[typemin(UInt)], UInt[typemax(UInt)], ambig)
                    ambig[1] == 0 && break
                    m2 = m2[3]
                    if Base.isambiguous(m, m2, ambiguous_bottom=ambiguous_bottom)
                        push!(ambs, sortdefs(m, m2))
                    end
                end
            end
        end
    end
    return collect(ambs)
end

"""
    detect_unbound_args(mod1, mod2...; imported=false, recursive=false)

Returns a vector of `Method`s which may have unbound type parameters.
Use `imported=true` if you wish to also test functions that were
imported into these modules from elsewhere.
Use `recursive=true` to test in all submodules.
"""
function detect_unbound_args(mods...;
                             imported::Bool = false,
                             recursive::Bool = false)
    ambs = Set{Method}()
    for mod in mods
        for n in names(mod, all = true, imported = imported)
            Base.isdeprecated(mod, n) && continue
            if !isdefined(mod, n)
                println("Skipping ", mod, '.', n)  # typically stale exports
                continue
            end
            f = Base.unwrap_unionall(getfield(mod, n))
            if recursive && isa(f, Module) && f !== mod && parentmodule(f) === mod && nameof(f) === n
                subambs = detect_unbound_args(f, imported=imported, recursive=recursive)
                union!(ambs, subambs)
            elseif isa(f, DataType) && isdefined(f.name, :mt)
                mt = Base.MethodList(f.name.mt)
                for m in mt
                    if has_unbound_vars(m.sig)
                        tuple_sig = Base.unwrap_unionall(m.sig)::DataType
                        if Base.isvatuple(tuple_sig)
                            params = tuple_sig.parameters[1:(end - 1)]
                            tuple_sig = Base.rewrap_unionall(Tuple{params...}, m.sig)
                            mf = ccall(:jl_gf_invoke_lookup, Any, (Any, UInt), tuple_sig, typemax(UInt))
                            if mf !== nothing && mf !== m && mf.sig <: tuple_sig
                                continue
                            end
                        end
                        push!(ambs, m)
                    end
                end
            end
        end
    end
    return collect(ambs)
end

# find if var will be constrained to have a definite value
# in any concrete leaftype subtype of typ
function constrains_param(var::TypeVar, @nospecialize(typ), covariant::Bool)
    typ === var && return true
    while typ isa UnionAll
        covariant && constrains_param(var, typ.var.ub, covariant) && return true
        # typ.var.lb doesn't constrain var
        typ = typ.body
    end
    if typ isa Union
        # for unions, verify that both options would constrain var
        ba = constrains_param(var, typ.a, covariant)
        bb = constrains_param(var, typ.b, covariant)
        (ba && bb) && return true
    elseif typ isa DataType
        # return true if any param constrains var
        fc = length(typ.parameters)
        if fc > 0
            if typ.name === Tuple.name
                # vararg tuple needs special handling
                for i in 1:(fc - 1)
                    p = typ.parameters[i]
                    constrains_param(var, p, covariant) && return true
                end
                lastp = typ.parameters[fc]
                vararg = Base.unwrap_unionall(lastp)
                if vararg isa DataType && vararg.name === Base._va_typename
                    N = vararg.parameters[2]
                    constrains_param(var, N, covariant) && return true
                    # T = vararg.parameters[1] doesn't constrain var
                else
                    constrains_param(var, lastp, covariant) && return true
                end
            else
                for i in 1:fc
                    p = typ.parameters[i]
                    constrains_param(var, p, false) && return true
                end
            end
        end
    end
    return false
end

function has_unbound_vars(@nospecialize sig)
    while sig isa UnionAll
        var = sig.var
        sig = sig.body
        if !constrains_param(var, sig, true)
            return true
        end
    end
    return false
end


"""
The `GenericString` can be used to test generic string APIs that program to
the `AbstractString` interface, in order to ensure that functions can work
with string types besides the standard `String` type.
"""
struct GenericString <: AbstractString
    string::AbstractString
end
Base.ncodeunits(s::GenericString) = ncodeunits(s.string)
Base.codeunit(s::GenericString) = codeunit(s.string)
Base.codeunit(s::GenericString, i::Integer) = codeunit(s.string, i)
Base.isvalid(s::GenericString, i::Integer) = isvalid(s.string, i)
Base.iterate(s::GenericString, i::Integer=1) = iterate(s.string, i)
Base.reverse(s::GenericString) = GenericString(reverse(s.string))
Base.reverse(s::SubString{GenericString}) =
    GenericString(typeof(s.string)(reverse(String(s))))

"""
The `GenericSet` can be used to test generic set APIs that program to
the `AbstractSet` interface, in order to ensure that functions can work
with set types besides the standard `Set` and `BitSet` types.
"""
struct GenericSet{T} <: AbstractSet{T}
    s::AbstractSet{T}
end

"""
The `GenericDict` can be used to test generic dict APIs that program to
the `AbstractDict` interface, in order to ensure that functions can work
with associative types besides the standard `Dict` type.
"""
struct GenericDict{K,V} <: AbstractDict{K,V}
    s::AbstractDict{K,V}
end

for G in (GenericSet, GenericDict)
    @eval begin
        Base.iterate(s::$G, state...) = iterate(s.s, state...)
    end
    for f in (:isempty, :length)
        @eval begin
            Base.$f(s::$G) = $f(s.s)
        end
    end
end

Base.get(s::GenericDict, x, y) = get(s.s, x, y)

"""
The `GenericArray` can be used to test generic array APIs that program to
the `AbstractArray` interface, in order to ensure that functions can work
with array types besides the standard `Array` type.
"""
struct GenericArray{T,N} <: AbstractArray{T,N}
    a::Array{T,N}
end

GenericArray{T}(args...) where {T} = GenericArray(Array{T}(args...))
GenericArray{T,N}(args...) where {T,N} = GenericArray(Array{T,N}(args...))

"""
The `GenericOrder` can be used to test APIs for their support
of generic ordered types.
"""
struct GenericOrder{T}
    val::T
end
Base.isless(x::GenericOrder, y::GenericOrder) = isless(x.val,y.val)

Base.keys(a::GenericArray) = keys(a.a)
Base.axes(a::GenericArray) = axes(a.a)
Base.length(a::GenericArray) = length(a.a)
Base.size(a::GenericArray) = size(a.a)
Base.IndexStyle(::Type{<:GenericArray}) = IndexLinear()
Base.getindex(a::GenericArray, i::Int) = a.a[i]
Base.setindex!(a::GenericArray, x, i::Int) = a.a[i] = x

Base.similar(A::GenericArray, s::Integer...) = GenericArray(similar(A.a, s...))

"`guardseed(f)` runs the function `f()` and then restores the
state of the global RNG as it was before."
function guardseed(f::Function, r::AbstractRNG=default_rng())
    old = copy(r)
    try
        f()
    finally
        copy!(r, old)
    end
end

"`guardseed(f, seed)` is equivalent to running `Random.seed!(seed); f()` and
then restoring the state of the global RNG as it was before."
guardseed(f::Function, seed::Union{Vector{UInt32},Integer}) = guardseed() do
    Random.seed!(seed)
    f()
end

function _check_bitarray_consistency(B::BitArray{N}) where N
    n = length(B)
    if N ≠ 1
        all(d ≥ 0 for d in B.dims) || (@warn("Negative d in dims: $(B.dims)"); return false)
        prod(B.dims) ≠ n && (@warn("Inconsistent dims/len: prod(dims)=$(prod(B.dims)) len=$n"); return false)
    end
    Bc = B.chunks
    nc = length(Bc)
    nc == Base.num_bit_chunks(n) || (@warn("Incorrect chunks length for length $n: expected=$(Base.num_bit_chunks(n)) actual=$nc"); return false)
    n == 0 && return true
    Bc[end] & Base._msk_end(n) == Bc[end] || (@warn("Nonzero bits in chunk after `BitArray` end"); return false)
    return true
end

# 0.7 deprecations

begin
    approx_full(x::AbstractArray) = x
    approx_full(x::Number) = x
    approx_full(x) = full(x)

    function test_approx_eq(va, vb, Eps, astr, bstr)
        va = approx_full(va)
        vb = approx_full(vb)
        la, lb = length(LinearIndices(va)), length(LinearIndices(vb))
        if la != lb
            error("lengths of ", astr, " and ", bstr, " do not match: ",
                "\n  ", astr, " (length $la) = ", va,
                "\n  ", bstr, " (length $lb) = ", vb)
        end
        diff = real(zero(eltype(va)))
        for (xa, xb) = zip(va, vb)
            if isfinite(xa) && isfinite(xb)
                diff = max(diff, abs(xa-xb))
            elseif !isequal(xa,xb)
                error("mismatch of non-finite elements: ",
                    "\n  ", astr, " = ", va,
                    "\n  ", bstr, " = ", vb)
            end
        end

        if !isnan(Eps) && !(diff <= Eps)
            sdiff = string("|", astr, " - ", bstr, "| <= ", Eps)
            error("assertion failed: ", sdiff,
                "\n  ", astr, " = ", va,
                "\n  ", bstr, " = ", vb,
                "\n  difference = ", diff, " > ", Eps)
        end
    end

    array_eps(a::AbstractArray{Complex{T}}) where {T} = eps(float(maximum(x->(isfinite(x) ? abs(x) : T(NaN)), a)))
    array_eps(a) = eps(float(maximum(x->(isfinite(x) ? abs(x) : oftype(x,NaN)), a)))

    test_approx_eq(va, vb, astr, bstr) =
        test_approx_eq(va, vb, 1E4*length(LinearIndices(va))*max(array_eps(va), array_eps(vb)), astr, bstr)
end

include("logging.jl")

end # module
