# This file is a part of Julia. License is MIT: http://julialang.org/license

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

export @test, @test_throws, @test_broken, @test_skip
export @testset
# Legacy approximate testing functions, yet to be included
export @test_approx_eq, @test_approx_eq_eps, @inferred
export detect_ambiguities
export GenericString

#-----------------------------------------------------------------------

"""
    Result

All tests produce a result object. This object may or may not be
stored, depending on whether the test is part of a test set.
"""
abstract Result

"""
    Pass

The test condition was true, i.e. the expression evaluated to true or
the correct exception was thrown.
"""
immutable Pass <: Result
    test_type::Symbol
    orig_expr
    data
    value
end
function Base.show(io::IO, t::Pass)
    print_with_color(:green, io, "Test Passed\n")
    print(io, "  Expression: ", t.orig_expr)
    if t.test_type == :test_throws
        # The correct type of exception was thrown
        print(io, "\n      Thrown: ", typeof(t.value))
    elseif t.test_type == :test && isa(t.data,Expr) && t.data.head == :comparison
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
type Fail <: Result
    test_type::Symbol
    orig_expr
    data
    value
end
function Base.show(io::IO, t::Fail)
    print_with_color(:red, io, "Test Failed\n")
    print(io, "  Expression: ", t.orig_expr)
    if t.test_type == :test_throws_wrong
        # An exception was thrown, but it was of the wrong type
        print(io, "\n    Expected: ", t.data)
        print(io, "\n      Thrown: ", typeof(t.value))
    elseif t.test_type == :test_throws_nothing
        # An exception was expected, but no exception was thrown
        print(io, "\n    Expected: ", t.data)
        print(io, "\n  No exception thrown")
    elseif t.test_type == :test && isa(t.data,Expr) && t.data.head == :comparison
        # The test was an expression, so display the term-by-term
        # evaluated version as well
        print(io, "\n   Evaluated: ", t.data)
    end
end

"""
    Error

The test condition couldn't be evaluated due to an exception, or
it evaluated to something other than a `Bool`.
In the case of `@test_broken` it is used to indicate that an
unexpected `Pass` `Result` occurred.
"""
type Error <: Result
    test_type::Symbol
    orig_expr
    value
    backtrace
end
function Base.show(io::IO, t::Error)
    print_with_color(:red, io, "Error During Test\n")
    if t.test_type == :test_nonbool
        println(io, "  Expression evaluated to non-Boolean")
        println(io, "  Expression: ", t.orig_expr)
        print(  io, "       Value: ", t.value)
    elseif t.test_type == :test_error
        println(io, "  Test threw an exception of type ", typeof(t.value))
        println(io, "  Expression: ", t.orig_expr)
        # Capture error message and indent to match
        errmsg = sprint(showerror, t.value, t.backtrace)
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
type Broken <: Result
    test_type::Symbol
    orig_expr
end
function Base.show(io::IO, t::Broken)
    print_with_color(:yellow, io, "Test Broken\n")
    if t.test_type == :skipped
        println(io, "  Skipped: ", t.orig_expr)
    else
        println(io, "Expression: ", t.orig_expr)
    end
end

#-----------------------------------------------------------------------

abstract ExecutionResult

immutable Returned <: ExecutionResult
    value
    data
end

immutable Threw <: ExecutionResult
    exception
    backtrace
end

function eval_comparison(ex::Expr)
    res = true
    i = 1
    a = ex.args
    n = length(a)
    while i < n
        res = a[i+1](a[i], a[i+2])
        if !isa(res,Bool) || !res
            break
        end
        i += 2
    end
    Returned(res, ex)
end

const comparison_prec = Base.operator_precedence(:(==))

# @test - check if the expression evaluates to true
"""
    @test ex

Tests that the expression `ex` evaluates to `true`.
Returns a `Pass` `Result` if it does, a `Fail` `Result` if it is
`false`, and an `Error` `Result` if it could not be evaluated.
"""
macro test(ex)
    orig_ex = Expr(:inert, ex)
    result = get_test_result(ex)
    :(do_test($result, $orig_ex))
end

"""
    @test_broken ex

Indicates a test that should pass but currently consistently fails.
Tests that the expression `ex` evaluates to `false` or causes an
exception. Returns a `Broken` `Result` if it does, or an `Error` `Result`
if the expression evaluates to `true`.
"""
macro test_broken(ex)
    orig_ex = Expr(:inert, ex)
    result = get_test_result(ex)
    # code to call do_test with execution result and original expr
    :(do_broken_test($result, $orig_ex))
end

"""
    @test_skip ex

Marks a test that should not be executed but should be included in test
summary reporting as `Broken`. This can be useful for tests that intermittently
fail, or tests of not-yet-implemented functionality.
"""
macro test_skip(ex)
    orig_ex = Expr(:inert, ex)
    testres = :(Broken(:skipped, $orig_ex))
    :(record(get_testset(), $testres))
end

# An internal function, called by the code generated by the @test
# macro to get results of the test expression.
# In the special case of a comparison, e.g. x == 5, generate code to
# evaluate each term in the comparison individually so the results
# can be displayed nicely.
function get_test_result(ex)
    orig_ex = Expr(:inert, ex)
    # Normalize comparison operator calls to :comparison expressions
    if isa(ex, Expr) && ex.head == :call && length(ex.args)==3 &&
        (ex.args[1] === :(==) || Base.operator_precedence(ex.args[1]) == comparison_prec)
        testret = :(eval_comparison(Expr(:comparison,
                                         $(esc(ex.args[2])), $(esc(ex.args[1])), $(esc(ex.args[3])))))
    elseif isa(ex, Expr) && ex.head == :comparison
        # pass all terms of the comparison to `eval_comparison`, as an Expr
        terms = ex.args
        for i = 1:length(terms)
            terms[i] = esc(terms[i])
        end
        testret = :(eval_comparison(Expr(:comparison, $(terms...))))
    else
        testret = :(Returned($(esc(ex)), nothing))
    end
    result = quote
        try
            $testret
        catch _e
            Threw(_e, catch_backtrace())
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
                    Fail(:test, orig_expr, result.data, value)
        else
            # If the result is non-Boolean, this counts as an Error
            Error(:test_nonbool, orig_expr, value, nothing)
        end
    else
        # The predicate couldn't be evaluated without throwing an
        # exception, so that is an Error and not a Fail
        @assert isa(result, Threw)
        testres = Error(:test_error, orig_expr, result.exception, result.backtrace)
    end
    record(get_testset(), testres)
end

function do_broken_test(result::ExecutionResult, orig_expr)
    testres = Broken(:test, orig_expr)
    # Assume the test is broken and only change if the result is true
    if isa(result, Returned)
        value = result.value
        if isa(value, Bool) && value
            testres = Error(:test_unbroken, orig_expr, value, nothing)
        end
    end
    record(get_testset(), testres)
end

#-----------------------------------------------------------------------

"""
    @test_throws extype ex

Tests that the expression `ex` throws an exception of type `extype`.
"""
macro test_throws(extype, ex)
    orig_ex = Expr(:inert, ex)
    result = quote
        try
            Returned($(esc(ex)), nothing)
        catch _e
            Threw(_e, nothing)
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
        if isa(result.exception, extype)
            testres = Pass(:test_throws, nothing, nothing, result.exception)
        else
            testres = Fail(:test_throws_wrong, orig_expr, extype, result.exception)
        end
    else
        testres = Fail(:test_throws_nothing, orig_expr, extype, nothing)
    end
    record(get_testset(), testres)
end

#-----------------------------------------------------------------------

# The AbstractTestSet interface is defined by two methods:
# record(AbstractTestSet, Result)
#   Called by do_test after a test is evaluated
# finish(AbstractTestSet)
#   Called after the test set has been popped from the test set stack
abstract AbstractTestSet

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
type TestSetException <: Exception
    pass::Int
    fail::Int
    error::Int
    broken::Int
    errors_and_fails
end

function Base.show(io::IO, ex::TestSetException)
    print(io, "Some tests did not pass: ")
    print(io, ex.pass,  " passed, ")
    print(io, ex.fail,  " failed, ")
    print(io, ex.error, " errored, ")
    print(io, ex.broken, " broken.")
end

#-----------------------------------------------------------------------

"""
    FallbackTestSet

A simple fallback test set that throws immediately on a failure.
"""
immutable FallbackTestSet <: AbstractTestSet
end
fallback_testset = FallbackTestSet()

# Records nothing, and throws an error immediately whenever a Fail or
# Error occurs. Takes no action in the event of a Pass or Broken result
record(ts::FallbackTestSet, t::Union{Pass,Broken}) = t
function record(ts::FallbackTestSet, t::Union{Fail,Error})
    println(t)
    error("There was an error during testing")
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
type DefaultTestSet <: AbstractTestSet
    description::AbstractString
    results::Vector
    anynonpass::Bool
end
DefaultTestSet(desc) = DefaultTestSet(desc, [], false)

# For a passing or broken result, simply store the result
record(ts::DefaultTestSet, t::Union{Pass,Broken}) = (push!(ts.results, t); t)

# For the other result types, immediately print the error message
# but do not terminate. Print a backtrace.
function record(ts::DefaultTestSet, t::Union{Fail, Error})
    if myid() == 1
        print_with_color(:white, ts.description, ": ")
        print(t)
        # don't print the backtrace for Errors because it gets printed in the show
        # method
        isa(t, Error) || Base.show_backtrace(STDOUT, backtrace())
        println()
    end
    push!(ts.results, t)
    t, isa(t, Error) || backtrace()
end

# When a DefaultTestSet finishes, it records itself to its parent
# testset, if there is one. This allows for recursive printing of
# the results at the end of the tests
record(ts::DefaultTestSet, t::AbstractTestSet) = push!(ts.results, t)

function print_test_errors(ts::DefaultTestSet)
    for t in ts.results
        if (isa(t, Error) || isa(t, Fail)) && myid() == 1
            println("Error in testset $(ts.description):")
            Base.show(STDOUT,t)
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
    print_with_color(:white, rpad("Test Summary:",align," "), " | ")
    if pass_width > 0
        print_with_color(:green, lpad("Pass",pass_width," "), "  ")
    end
    if fail_width > 0
        print_with_color(:red, lpad("Fail",fail_width," "), "  ")
    end
    if error_width > 0
        print_with_color(:red, lpad("Error",error_width," "), "  ")
    end
    if broken_width > 0
        print_with_color(:yellow, lpad("Broken",broken_width," "), "  ")
    end
    if total_width > 0
        print_with_color(:blue, lpad("Total",total_width, " "))
    end
    println()
    # Recursively print a summary at every level
    print_counts(ts, depth_pad, align, pass_width, fail_width, error_width, broken_width, total_width)
end

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
    passes, fails, errors, broken = 0, 0, 0, 0
    c_passes, c_fails, c_errors, c_broken = 0, 0, 0, 0
    for t in ts.results
        isa(t, Pass)   && (passes += 1)
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
    print(rpad(string(lpad("  ",depth), ts.description), align, " "), " | ")

    np = passes + c_passes
    if np > 0
        print_with_color(:green, lpad(string(np), pass_width, " "), "  ")
    elseif pass_width > 0
        # No passes at this level, but some at another level
        print(lpad(" ", pass_width), "  ")
    end

    nf = fails + c_fails
    if nf > 0
        print_with_color(:red, lpad(string(nf), fail_width, " "), "  ")
    elseif fail_width > 0
        # No fails at this level, but some at another level
        print(lpad(" ", fail_width), "  ")
    end

    ne = errors + c_errors
    if ne > 0
        print_with_color(:red, lpad(string(ne), error_width, " "), "  ")
    elseif error_width > 0
        # No errors at this level, but some at another level
        print(lpad(" ", error_width), "  ")
    end

    nb = broken + c_broken
    if nb > 0
        print_with_color(:yellow, lpad(string(nb), broken_width, " "), "  ")
    elseif broken_width > 0
        # None broken at this level, but some at another level
        print(lpad(" ", broken_width), "  ")
    end

    if np == 0 && nf == 0 && ne == 0 && nb == 0
        print_with_color(:blue, "No tests")
    else
        print_with_color(:blue, lpad(string(subtotal), total_width, " "))
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

"""
    @testset [CustomTestSet] [option=val  ...] ["description"] begin ... end
    @testset [CustomTestSet] [option=val  ...] ["description \$v"] for v in (...) ... end
    @testset [CustomTestSet] [option=val  ...] ["description \$v, \$w"] for v in (...), w in (...) ... end

Starts a new test set, or multiple test sets if a `for` loop is provided.

If no custom testset type is given it defaults to creating a `DefaultTestSet`.
`DefaultTestSet` records all the results and, and if there are any `Fail`s or
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
"""
macro testset(args...)
    isempty(args) && error("No arguments to @testset")

    tests = args[end]

    # Determine if a single block or for-loop style
    if !isa(tests,Expr) || (tests.head != :for && tests.head != :block)
        error("Expected begin/end block or for loop as argument to @testset")
    end

    if tests.head == :for
        return testset_forloop(args, tests)
    else
        return testset_beginend(args, tests)
    end
end

"""
Generate the code for a `@testset` with a `begin`/`end` argument
"""
function testset_beginend(args, tests)
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
    quote
        ts = $(testsettype)($desc; $options...)
        push_testset(ts)
        try
            $(esc(tests))
        catch err
            # something in the test block threw an error. Count that as an
            # error in this test set
            record(ts, Error(:nontest_error, :(), err, catch_backtrace()))
        end
        pop_testset()
        finish(ts)
    end
end


"""
Generate the code for a `@testset` with a `for` loop argument
"""
function testset_forloop(args, testloop)
    # Pull out the loop variables. We might need them for generating the
    # description and we'll definitely need them for generating the
    # comprehension expression at the end
    loopvars = Expr[]
    if testloop.args[1].head == :(=)
        push!(loopvars, testloop.args[1])
    elseif testloop.args[1].head == :block
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
        # Trick to handle `break` and `continue` in the test code before
        # they can be handled properly by `finally` lowering.
        if !first_iteration
            pop_testset()
            push!(arr, finish(ts))
        end
        first_iteration = false
        ts = $(testsettype)($desc; $options...)
        push_testset(ts)
        try
            $(esc(tests))
        catch err
            # Something in the test block threw an error. Count that as an
            # error in this test set
            record(ts, Error(:nontest_error, :(), err, catch_backtrace()))
        end
    end
    quote
        arr = Array{Any,1}(0)
        first_iteration = true
        local ts
        try
            $(Expr(:for, Expr(:block, [esc(v) for v in loopvars]...), blk))
        finally
            # Handle `return` in test body
            if !first_iteration
                pop_testset()
                push!(arr, finish(ts))
            end
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
        elseif isa(arg, AbstractString) || (isa(arg, Expr) && arg.head == :string)
            desc = esc(arg)
        # an assignment is an option
        elseif isa(arg, Expr) && arg.head == :(=)
            # we're building up a Dict literal here
            key = Expr(:quote, arg.args[1])
            push!(options.args, Expr(:(=>), key, arg.args[2]))
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

Returns the number of active test sets, not including the defaut test set
"""
function get_testset_depth()
    testsets = get(task_local_storage(), :__BASETESTNEXT__, AbstractTestSet[])
    return length(testsets)
end

#-----------------------------------------------------------------------
# Legacy approximate testing functions, yet to be included

approx_full(x::AbstractArray) = x
approx_full(x::Number) = x
approx_full(x) = full(x)

function test_approx_eq(va, vb, Eps, astr, bstr)
    va = approx_full(va)
    vb = approx_full(vb)
    la, lb = length(linearindices(va)), length(linearindices(vb))
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

array_eps{T}(a::AbstractArray{Complex{T}}) = eps(float(maximum(x->(isfinite(x) ? abs(x) : T(NaN)), a)))
array_eps(a) = eps(float(maximum(x->(isfinite(x) ? abs(x) : oftype(x,NaN)), a)))

test_approx_eq(va, vb, astr, bstr) =
    test_approx_eq(va, vb, 1E4*length(linearindices(va))*max(array_eps(va), array_eps(vb)), astr, bstr)

"""
    @test_approx_eq_eps(a, b, tol)

Test two floating point numbers `a` and `b` for equality taking into account
a margin of tolerance given by `tol`.
"""
macro test_approx_eq_eps(a, b, c)
    :(test_approx_eq($(esc(a)), $(esc(b)), $(esc(c)), $(string(a)), $(string(b))))
end

"""
    @test_approx_eq(a, b)

Test two floating point numbers `a` and `b` for equality taking into account
small numerical errors.
"""
macro test_approx_eq(a, b)
    :(test_approx_eq($(esc(a)), $(esc(b)), $(string(a)), $(string(b))))
end

_args_and_call(args...; kwargs...) = (args[1:end-1], kwargs, args[end](args[1:end-1]...; kwargs...))
"""
    @inferred f(x)

Tests that the call expression `f(x)` returns a value of the same type
inferred by the compiler. It is useful to check for type stability.

`f(x)` can be any call expression.
Returns the result of `f(x)` if the types match,
and an `Error` `Result` if it finds different types.

```jldoctest
julia> using Base.Test

julia> f(a,b,c) = b > 1 ? 1 : 1.0
f (generic function with 1 method)

julia> typeof(f(1,2,3))
Int64

julia> @code_warntype f(1,2,3)
...
Body:
  begin
      unless (Base.slt_int)(1,b::Int64)::Bool goto 3
      return 1
      3:
      return 1.0
  end::UNION{FLOAT64,INT64}

julia> @inferred f(1,2,3)
ERROR: return type Int64 does not match inferred return type Union{Float64,Int64}
 in error(::String) at ./error.jl:21
 ...

julia> @inferred max(1,2)
2
```
"""
macro inferred(ex)
    if Meta.isexpr(ex, :ref)
        ex = Expr(:call, :getindex, ex.args...)
    end
    Meta.isexpr(ex, :call)|| error("@inferred requires a call expression")

    Base.remove_linenums!(quote
        $(if any(a->(Meta.isexpr(a, :kw) || Meta.isexpr(a, :parameters)), ex.args)
            # Has keywords
            args = gensym()
            kwargs = gensym()
            quote
                $(esc(args)), $(esc(kwargs)), result = $(esc(Expr(:call, _args_and_call, ex.args[2:end]..., ex.args[1])))
                inftypes = $(Base.gen_call_with_extracted_types(Base.return_types, :($(ex.args[1])($(args)...; $(kwargs)...))))
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
        rettype = isa(result, Type) ? Type{result} : typeof(result)
        rettype == inftypes[1] || error("return type $rettype does not match inferred return type $(inftypes[1])")
        result
    end)
end

# Test approximate equality of vectors or columns of matrices modulo floating
# point roundoff and phase (sign) differences.
#
# This function is designed to test for equality between vectors of floating point
# numbers when the vectors are defined only up to a global phase or sign, such as
# normalized eigenvectors or singular vectors. The global phase is usually
# defined consistently, but may occasionally change due to small differences in
# floating point rounding noise or rounding modes, or through the use of
# different conventions in different algorithms. As a result, most tests checking
# such vectors have to detect and discard such overall phase differences.
#
# Inputs:
#     a, b:: StridedVecOrMat to be compared
#     err :: Default: m^3*(eps(S)+eps(T)), where m is the number of rows
#
# Raises an error if any columnwise vector norm exceeds err. Otherwise, returns
# nothing.
function test_approx_eq_modphase{S<:Real,T<:Real}(
        a::StridedVecOrMat{S}, b::StridedVecOrMat{T}, err=nothing)
    @test indices(a,1) == indices(b,1) && indices(a,2) == indices(b,2)
    m = length(indices(a,1))
    err === nothing && (err=m^3*(eps(S)+eps(T)))
    for i in indices(a,2)
        v1, v2 = a[:, i], b[:, i]
        @test_approx_eq_eps min(abs(norm(v1-v2)), abs(norm(v1+v2))) 0.0 err
    end
end

"""
    detect_ambiguities(mod1, mod2...; imported=false)

Returns a vector of `(Method,Method)` pairs of ambiguous methods
defined in the specified modules. Use `imported=true` if you wish to
also test functions that were imported into these modules from
elsewhere.
"""
function detect_ambiguities(mods...; imported::Bool=false)
    function sortdefs(m1, m2)
        ord12 = m1.file < m2.file
        if !ord12 && (m1.file == m2.file)
            ord12 = m1.line < m2.line
        end
        ord12 ? (m1, m2) : (m2, m1)
    end
    ambs = Set{Tuple{Method,Method}}()
    for mod in mods
        for n in names(mod, true, imported)
            Base.isdeprecated(mod, n) && continue
            if !isdefined(mod, n)
                println("Skipping ", mod, '.', n)  # typically stale exports
                continue
            end
            f = getfield(mod, n)
            if isa(f, DataType) && isdefined(f.name, :mt)
                mt = Base.MethodList(f.name.mt)
                for m in mt
                    if m.ambig !== nothing
                        for m2 in m.ambig
                            if Base.isambiguous(m, m2)
                                push!(ambs, sortdefs(m, m2))
                            end
                        end
                    end
                end
            end
        end
    end
    return collect(ambs)
end

"""
The `GenericString` can be used to test generic string APIs that program to
the `AbstractString` interface, in order to ensure that functions can work
with string types besides the standard `String` type.
"""
immutable GenericString <: AbstractString
    string::AbstractString
end
Base.convert(::Type{GenericString}, s::AbstractString) = GenericString(s)
Base.endof(s::GenericString) = endof(s.string)
Base.next(s::GenericString, i::Int) = next(s.string, i)

end # module
