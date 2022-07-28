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
export @inferred
export detect_ambiguities, detect_unbound_args
export GenericString, GenericSet, GenericDict, GenericArray, GenericOrder
export TestSetException
export TestLogger, LogRecord

using Random
using Random: AbstractRNG, default_rng
using InteractiveUtils: gen_call_with_extracted_types
using Base: typesplit
using Serialization: Serialization

const DISPLAY_FAILED = (
    :isequal,
    :isapprox,
    :≈,
    :occursin,
    :startswith,
    :endswith,
    :isempty,
    :contains
)

const FAIL_FAST = Ref{Bool}(false)

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
    return Any[ (x[1], scrub_backtrace(x[2]::Vector{Union{Ptr{Nothing},Base.InterpreterIP}})) for x in stack ]
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
    source::Union{Nothing,LineNumberNode}
    message_only::Bool
    function Pass(test_type::Symbol, orig_expr, data, thrown, source::Union{Nothing,LineNumberNode}=nothing, message_only::Bool=false)
        return new(test_type, orig_expr, data, thrown, source, message_only)
    end
end

function Base.show(io::IO, t::Pass)
    printstyled(io, "Test Passed"; bold = true, color=:green)
    if t.test_type === :test_throws
        # The correct type of exception was thrown
        if t.message_only
            print(io, "\n     Message: ", t.value)
        else
            print(io, "\n      Thrown: ", typeof(t.value))
        end
    end
end

"""
    Fail

The test condition was false, i.e. the expression evaluated to false or
the correct exception was not thrown.
"""
struct Fail <: Result
    test_type::Symbol
    orig_expr::String
    data::Union{Nothing, String}
    value::String
    context::Union{Nothing, String}
    source::LineNumberNode
    message_only::Bool
    function Fail(test_type::Symbol, orig_expr, data, value, context, source::LineNumberNode, message_only::Bool)
        return new(test_type,
            string(orig_expr),
            data === nothing ? nothing : string(data),
            string(isa(data, Type) ? typeof(value) : value),
            context,
            source,
            message_only)
    end
end

# Deprecated fallback constructor without `context` argument (added in Julia 1.9). Remove in Julia 2.0.
Fail(test_type::Symbol, orig_expr, data, value, source::LineNumberNode, message_only::Bool=false) =
    Fail(test_type, orig_expr, data, value, nothing, source, message_only)

function Base.show(io::IO, t::Fail)
    printstyled(io, "Test Failed"; bold=true, color=Base.error_color())
    print(io, " at ")
    printstyled(io, something(t.source.file, :none), ":", t.source.line, "\n"; bold=true, color=:default)
    print(io, "  Expression: ", t.orig_expr)
    value, data = t.value, t.data
    if t.test_type === :test_throws_wrong
        # An exception was thrown, but it was of the wrong type
        if t.message_only
            print(io, "\n    Expected: ", data)
            print(io, "\n     Message: ", value)
        else
            print(io, "\n    Expected: ", data)
            print(io, "\n      Thrown: ", value)
        end
    elseif t.test_type === :test_throws_nothing
        # An exception was expected, but no exception was thrown
        print(io, "\n    Expected: ", data)
        print(io, "\n  No exception thrown")
    elseif t.test_type === :test
        if data !== nothing
            # The test was an expression, so display the term-by-term
            # evaluated version as well
            print(io, "\n   Evaluated: ", data)
        end
        if t.context !== nothing
            print(io, "\n     Context: ", t.context)
        end
    end
end

"""
    Error

The test condition couldn't be evaluated due to an exception, or
it evaluated to something other than a [`Bool`](@ref).
In the case of `@test_broken` it is used to indicate that an
unexpected `Pass` `Result` occurred.
"""
struct Error <: Result
    test_type::Symbol
    orig_expr::String
    value::String
    backtrace::String
    source::LineNumberNode

    function Error(test_type::Symbol, orig_expr, value, bt, source::LineNumberNode)
        if test_type === :test_error
            bt = scrub_exc_stack(bt)
        end
        if test_type === :test_error || test_type === :nontest_error
            bt_str = try # try the latest world for this, since we might have eval'd new code for show
                    Base.invokelatest(sprint, Base.show_exception_stack, bt; context=stdout)
                catch ex
                    "#=ERROR showing exception stack=# " *
                        try
                            sprint(Base.showerror, ex, catch_backtrace(); context=stdout)
                        catch
                            "of type " * string(typeof(ex))
                        end
                end
        else
            bt_str = ""
        end
        value = try # try the latest world for this, since we might have eval'd new code for show
                Base.invokelatest(sprint, show, value, context = :limit => true)
            catch ex
                "#=ERROR showing error of type " * string(typeof(value)) * "=# " *
                    try
                        sprint(Base.showerror, ex, catch_backtrace(); context=stdout)
                    catch
                        "of type " * string(typeof(ex))
                    end
            end
        return new(test_type,
            string(orig_expr),
            value,
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
struct Broken <: Result
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

# Types that appear in TestSetException.errors_and_fails we convert eagerly into strings
# other types we convert lazily
function Serialization.serialize(s::Serialization.AbstractSerializer, t::Pass)
    Serialization.serialize_type(s, typeof(t))
    Serialization.serialize(s, t.test_type)
    Serialization.serialize(s, t.orig_expr === nothing ? nothing : string(t.orig_expr))
    Serialization.serialize(s, t.data === nothing ? nothing : string(t.data))
    Serialization.serialize(s, string(t.value))
    Serialization.serialize(s, t.source === nothing ? nothing : t.source)
    Serialization.serialize(s, t.message_only)
    nothing
end

function Serialization.serialize(s::Serialization.AbstractSerializer, t::Broken)
    Serialization.serialize_type(s, typeof(t))
    Serialization.serialize(s, t.test_type)
    Serialization.serialize(s, t.orig_expr === nothing ? nothing : string(t.orig_expr))
    nothing
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
    backtrace::Union{Nothing,Vector{Any}}
    source::LineNumberNode
end

function eval_test(evaluated::Expr, quoted::Expr, source::LineNumberNode, negate::Bool=false)
    evaled_args = evaluated.args
    quoted_args = quoted.args
    n = length(evaled_args)
    kw_suffix = ""
    if evaluated.head === :comparison
        args = evaled_args
        res = true
        i = 1
        while i < n
            a, op, b = args[i], args[i+1], args[i+2]
            if res
                res = op(a, b)
            end
            quoted_args[i] = a
            quoted_args[i+2] = b
            i += 2
        end

    elseif evaluated.head === :call
        op = evaled_args[1]
        kwargs = (evaled_args[2]::Expr).args  # Keyword arguments from `Expr(:parameters, ...)`
        args = evaled_args[3:n]

        res = op(args...; kwargs...)

        # Create "Evaluated" expression which looks like the original call but has all of
        # the arguments evaluated
        func_sym = quoted_args[1]::Union{Symbol,Expr}
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
             res === true ? quoted : sprint(print, quoted, context=(:limit => true)) * kw_suffix,
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
    @test ex broken=true
    @test ex skip=true

Test that the expression `ex` evaluates to `true`.
If executed inside a `@testset`, return a `Pass` `Result` if it does, a `Fail` `Result` if it is
`false`, and an `Error` `Result` if it could not be evaluated.
If executed outside a `@testset`, throw an exception instead of returning `Fail` or `Error`.

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

You can use any key for the `key=val` arguments, except for `broken` and `skip`,
which have special meanings in the context of `@test`:

* `broken=cond` indicates a test that should pass but currently consistently
  fails when `cond==true`.  Tests that the expression `ex` evaluates to `false`
  or causes an exception.  Returns a `Broken` `Result` if it does, or an `Error`
  `Result` if the expression evaluates to `true`.  Regular `@test ex` is
  evaluated when `cond==false`.
* `skip=cond` marks a test that should not be executed but should be included in
  test summary reporting as `Broken`, when `cond==true`.  This can be useful for
  tests that intermittently fail, or tests of not-yet-implemented functionality.
  Regular `@test ex` is evaluated when `cond==false`.

# Examples

```jldoctest
julia> @test 2 + 2 ≈ 6 atol=1 broken=true
Test Broken
  Expression: ≈(2 + 2, 6, atol = 1)

julia> @test 2 + 2 ≈ 5 atol=1 broken=false
Test Passed

julia> @test 2 + 2 == 5 skip=true
Test Broken
  Skipped: 2 + 2 == 5

julia> @test 2 + 2 == 4 skip=false
Test Passed
```

!!! compat "Julia 1.7"
     The `broken` and `skip` keyword arguments require at least Julia 1.7.
"""
macro test(ex, kws...)
    # Collect the broken/skip keywords and remove them from the rest of keywords
    broken = [kw.args[2] for kw in kws if kw.args[1] === :broken]
    skip = [kw.args[2] for kw in kws if kw.args[1] === :skip]
    kws = filter(kw -> kw.args[1] ∉ (:skip, :broken), kws)
    # Validation of broken/skip keywords
    for (kw, name) in ((broken, :broken), (skip, :skip))
        if length(kw) > 1
            error("invalid test macro call: cannot set $(name) keyword multiple times")
        end
    end
    if length(skip) > 0 && length(broken) > 0
        error("invalid test macro call: cannot set both skip and broken keywords")
    end

    # Build the test expression
    test_expr!("@test", ex, kws...)
    orig_ex = Expr(:inert, ex)

    result = get_test_result(ex, __source__)

    return quote
        if $(length(skip) > 0 && esc(skip[1]))
            record(get_testset(), Broken(:skipped, $orig_ex))
        else
            let _do = $(length(broken) > 0 && esc(broken[1])) ? do_broken_test : do_test
                _do($result, $orig_ex)
            end
        end
    end
end

"""
    @test_broken ex
    @test_broken f(args...) key=val ...

Indicates a test that should pass but currently consistently fails.
Tests that the expression `ex` evaluates to `false` or causes an
exception. Returns a `Broken` `Result` if it does, or an `Error` `Result`
if the expression evaluates to `true`.  This is equivalent to
[`@test ex broken=true`](@ref @test).

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
fail, or tests of not-yet-implemented functionality.  This is equivalent to
[`@test ex skip=true`](@ref @test).

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

    # Mark <: and >: as :comparison expressions
    elseif isa(ex, Expr) && length(ex.args) == 2 &&
        !is_splat(ex.args[1]) && !is_splat(ex.args[2]) &&
        Base.operator_precedence(ex.head) == comparison_prec
        ex = Expr(:comparison, ex.args[1], ex.head, ex.args[2])
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
                elseif isa(a, Expr) && a.head === :.
                    push!(escaped_kwargs, Expr(:call, :(=>), QuoteNode(a.args[2].value), esc(Expr(:., a.args[1], QuoteNode(a.args[2].value)))))
                elseif isa(a, Symbol)
                    push!(escaped_kwargs, Expr(:call, :(=>), QuoteNode(a), esc(a)))
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
            Threw(_e, Base.current_exceptions(), $(QuoteNode(source)))
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
            value ? Pass(:test, orig_expr, result.data, value, result.source) :
                    Fail(:test, orig_expr, result.data, value, nothing, result.source, false)
        else
            # If the result is non-Boolean, this counts as an Error
            Error(:test_nonbool, orig_expr, value, nothing, result.source)
        end
    else
        # The predicate couldn't be evaluated without throwing an
        # exception, so that is an Error and not a Fail
        @assert isa(result, Threw)
        testres = Error(:test_error, orig_expr, result.exception, result.backtrace::Vector{Any}, result.source)
    end
    isa(testres, Pass) || trigger_test_failure_break(result)
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
a string, regular expression, or list of strings occurring in the displayed error message,
a matching function,
or a value (which will be tested for equality by comparing fields).
Note that `@test_throws` does not support a trailing keyword form.

!!! compat "Julia 1.8"
    The ability to specify anything other than a type or a value as `exception` requires Julia v1.8 or later.

# Examples
```jldoctest
julia> @test_throws BoundsError [1, 2, 3][4]
Test Passed
      Thrown: BoundsError

julia> @test_throws DimensionMismatch [1, 2, 3] + [1, 2]
Test Passed
      Thrown: DimensionMismatch

julia> @test_throws "Try sqrt(Complex" sqrt(-1)
Test Passed
     Message: "DomainError with -1.0:\\nsqrt will only return a complex result if called with a complex argument. Try sqrt(Complex(x))."
```

In the final example, instead of matching a single string it could alternatively have been performed with:

- `["Try", "Complex"]` (a list of strings)
- `r"Try sqrt\\([Cc]omplex"` (a regular expression)
- `str -> occursin("complex", str)` (a matching function)
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

const MACROEXPAND_LIKE = Symbol.(("@macroexpand", "@macroexpand1", "macroexpand"))

# An internal function, called by the code generated by @test_throws
# to evaluate and catch the thrown exception - if it exists
function do_test_throws(result::ExecutionResult, orig_expr, extype)
    if isa(result, Threw)
        # Check that the right type of exception was thrown
        success = false
        message_only = false
        exc = result.exception
        # NB: Throwing LoadError from macroexpands is deprecated, but in order to limit
        # the breakage in package tests we add extra logic here.
        from_macroexpand =
            orig_expr isa Expr &&
            orig_expr.head in (:call, :macrocall) &&
            orig_expr.args[1] in MACROEXPAND_LIKE
        if isa(extype, Type)
            success =
                if from_macroexpand && extype == LoadError && exc isa Exception
                    Base.depwarn("macroexpand no longer throws a LoadError so `@test_throws LoadError ...` is deprecated and passed without checking the error type!", :do_test_throws)
                    true
                else
                    isa(exc, extype)
                end
        elseif isa(extype, Exception) || !isa(exc, Exception)
            if extype isa LoadError && !(exc isa LoadError) && typeof(extype.error) == typeof(exc)
                extype = extype.error # deprecated
            end
            if isa(exc, typeof(extype))
                success = true
                for fld in 1:nfields(extype)
                    if !isequal(getfield(extype, fld), getfield(exc, fld))
                        success = false
                        break
                    end
                end
            end
        else
            message_only = true
            exc = sprint(showerror, exc)
            success = contains_warn(exc, extype)
            exc = repr(exc)
            if isa(extype, AbstractString)
                extype = repr(extype)
            elseif isa(extype, Function)
                extype = "< match function >"
            end
        end
        if success
            testres = Pass(:test_throws, orig_expr, extype, exc, result.source, message_only)
        else
            testres = Fail(:test_throws_wrong, orig_expr, extype, exc, nothing, result.source, message_only)
        end
    else
        testres = Fail(:test_throws_nothing, orig_expr, extype, nothing, nothing, result.source, false)
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
[`@test_logs`](@ref) instead.
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
with this macro. Use [`@test_logs`](@ref) instead.
"""
macro test_nowarn(expr)
    quote
        # Duplicate some code from `@test_warn` to allow printing the content of
        # `stderr` again to `stderr` here while suppressing it for `@test_warn`.
        # If that shouldn't be used, it would be possible to just use
        #     @test_warn isempty $(esc(expr))
        # here.
        let fname = tempname()
            try
                ret = open(fname, "w") do f
                    redirect_stderr(f) do
                        $(esc(expr))
                    end
                end
                stderr_content = read(fname, String)
                print(stderr, stderr_content) # this is helpful for debugging
                @test isempty(stderr_content)
                ret
            finally
                rm(fname, force=true)
            end
        end
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
`@testset` infrastructure after a test block executes.

Custom `AbstractTestSet` subtypes should call `record` on their parent (if there
is one) to add themselves to the tree of test results. This might be implemented
as:

```julia
if get_testset_depth() != 0
    # Attach this test set to the parent test set
    parent_ts = get_testset()
    record(parent_ts, self)
    return self
end
```
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
    msg::String
end

function Base.showerror(io::IO, ex::FallbackTestSetException, bt; backtrace=true)
    printstyled(io, ex.msg, color=Base.error_color())
end

# Records nothing, and throws an error immediately whenever a Fail or
# Error occurs. Takes no action in the event of a Pass or Broken result
record(ts::FallbackTestSet, t::Union{Pass, Broken}) = t
function record(ts::FallbackTestSet, t::Union{Fail, Error})
    println(t)
    throw(FallbackTestSetException("There was an error during testing"))
end
# We don't need to do anything as we don't record anything
finish(ts::FallbackTestSet) = ts

#-----------------------------------------------------------------------

"""
    ContextTestSet

Passes test failures through to the parent test set, while adding information
about a context object that is being tested.
"""
struct ContextTestSet <: AbstractTestSet
    parent_ts::AbstractTestSet
    context_name::Union{Symbol, Expr}
    context::Any
end

function ContextTestSet(name::Union{Symbol, Expr}, @nospecialize(context))
    if (name isa Expr) && (name.head != :tuple)
        error("Invalid syntax: $(name)")
    end
    return ContextTestSet(get_testset(), name, context)
end
record(c::ContextTestSet, t) = record(c.parent_ts, t)
function record(c::ContextTestSet, t::Fail)
    context = string(c.context_name, " = ", c.context)
    context = t.context === nothing ? context : string(t.context, "\n              ", context)
    record(c.parent_ts, Fail(t.test_type, t.orig_expr, t.data, t.value, context, t.source, t.message_only))
end

#-----------------------------------------------------------------------

"""
    DefaultTestSet

If using the DefaultTestSet, the test results will be recorded. If there
are any `Fail`s or `Error`s, an exception will be thrown only at the end,
along with a summary of the test results.
"""
mutable struct DefaultTestSet <: AbstractTestSet
    description::String
    results::Vector{Any}
    n_passed::Int
    anynonpass::Bool
    verbose::Bool
    showtiming::Bool
    time_start::Float64
    time_end::Union{Float64,Nothing}
    failfast::Bool
end
function DefaultTestSet(desc::AbstractString; verbose::Bool = false, showtiming::Bool = true, failfast::Union{Nothing,Bool} = nothing)
    if isnothing(failfast)
        # pass failfast state into child testsets
        parent_ts = get_testset()
        if parent_ts isa DefaultTestSet
            failfast = parent_ts.failfast
        else
            failfast = false
        end
    end
    return DefaultTestSet(String(desc)::String, [], 0, false, verbose, showtiming, time(), nothing, failfast)
end

struct FailFastError <: Exception end

# For a broken result, simply store the result
record(ts::DefaultTestSet, t::Broken) = (push!(ts.results, t); t)
# For a passed result, do not store the result since it uses a lot of memory
record(ts::DefaultTestSet, t::Pass) = (ts.n_passed += 1; t)

# For the other result types, immediately print the error message
# but do not terminate. Print a backtrace.
function record(ts::DefaultTestSet, t::Union{Fail, Error})
    if TESTSET_PRINT_ENABLE[]
        print(ts.description, ": ")
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
    (FAIL_FAST[] || ts.failfast) && throw(FailFastError())
    return t
end

# When a DefaultTestSet finishes, it records itself to its parent
# testset, if there is one. This allows for recursive printing of
# the results at the end of the tests
record(ts::DefaultTestSet, t::AbstractTestSet) = push!(ts.results, t)

@specialize

function print_test_errors(ts::DefaultTestSet)
    for t in ts.results
        if isa(t, Error) || isa(t, Fail)
            println("Error in testset $(ts.description):")
            show(t)
            println()
        elseif isa(t, DefaultTestSet)
            print_test_errors(t)
        end
    end
end

function print_test_results(ts::DefaultTestSet, depth_pad=0)
    # Calculate the overall number for each type so each of
    # the test result types are aligned
    passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken, duration = get_test_counts(ts)
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
    duration_width = max(length("Time"), length(duration))
    # Calculate the alignment of the test result counts by
    # recursively walking the tree of test sets
    align = max(get_alignment(ts, 0), length("Test Summary:"))
    # Print the outer test set header once
    pad = total == 0 ? "" : " "
    printstyled(rpad("Test Summary:", align, " "), " |", pad; bold=true)
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
        printstyled(lpad("Total", total_width, " "), "  "; bold=true, color=Base.info_color())
    end
    if ts.showtiming
        printstyled(lpad("Time", duration_width, " "); bold=true)
    end
    println()
    # Recursively print a summary at every level
    print_counts(ts, depth_pad, align, pass_width, fail_width, error_width, broken_width, total_width, duration_width, ts.showtiming)
end


const TESTSET_PRINT_ENABLE = Ref(true)

# Called at the end of a @testset, behaviour depends on whether
# this is a child of another testset, or the "root" testset
function finish(ts::DefaultTestSet)
    ts.time_end = time()
    # If we are a nested test set, do not print a full summary
    # now - let the parent test set do the printing
    if get_testset_depth() != 0
        # Attach this test set to the parent test set
        parent_ts = get_testset()
        record(parent_ts, ts)
        return ts
    end
    passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken, duration = get_test_counts(ts)
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
        throw(TestSetException(total_pass, total_fail, total_error, total_broken, efs))
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
    # If not verbose and all passing, no need to look at children
    !ts.verbose && !ts.anynonpass && return ts_width
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
            np, nf, ne, nb, ncp, ncf, nce , ncb, duration = get_test_counts(t)
            c_passes += np + ncp
            c_fails  += nf + ncf
            c_errors += ne + nce
            c_broken += nb + ncb
        end
    end
    ts.anynonpass = (fails + errors + c_fails + c_errors > 0)
    duration = if isnothing(ts.time_end)
        ""
    else
        dur_s = ts.time_end - ts.time_start
        if dur_s < 60
            string(round(dur_s, digits = 1), "s")
        else
            m, s = divrem(dur_s, 60)
            s = lpad(string(round(s, digits = 1)), 4, "0")
            string(round(Int, m), "m", s, "s")
        end
    end
    return passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken, duration
end

# Recursive function that prints out the results at each level of
# the tree of test sets
function print_counts(ts::DefaultTestSet, depth, align,
                      pass_width, fail_width, error_width, broken_width, total_width, duration_width, showtiming)
    # Count results by each type at this level, and recursively
    # through any child test sets
    passes, fails, errors, broken, c_passes, c_fails, c_errors, c_broken, duration = get_test_counts(ts)
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
        printstyled(lpad("None", total_width, " "), "  ", color=Base.info_color())
    else
        printstyled(lpad(string(subtotal), total_width, " "), "  ", color=Base.info_color())
    end

    if showtiming
        printstyled(lpad(string(duration), duration_width, " "))
    end
    println()

    # Only print results at lower levels if we had failures or if the user
    # wants.
    if (np + nb != subtotal) || (ts.verbose)
        for t in ts.results
            if isa(t, DefaultTestSet)
                print_counts(t, depth + 1, align,
                    pass_width, fail_width, error_width, broken_width, total_width, duration_width, ts.showtiming)
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
    @testset [CustomTestSet] [option=val  ...] ["description \$v, \$w"] foo()

# With begin/end or function call

When @testset is used, with begin/end or a single function call, the macro
starts a new test set in which to evaulate the given expression.

If no custom testset type is given it defaults to creating a `DefaultTestSet`.
`DefaultTestSet` records all the results and, if there are any `Fail`s or
`Error`s, throws an exception at the end of the top-level (non-nested) test set,
along with a summary of the test results.

Any custom testset type (subtype of `AbstractTestSet`) can be given and it will
also be used for any nested `@testset` invocations. The given options are only
applied to the test set where they are given. The default test set type
accepts three boolean options:
- `verbose`: if `true`, the result summary of the nested testsets is shown even
when they all pass (the default is `false`).
- `showtiming`: if `true`, the duration of each displayed testset is shown
(the default is `true`).
- `failfast`: if `true`, any test failure or error will cause the testset and any
child testsets to return immediately (the default is `false`). This can also be set
globally via the env var `JULIA_TEST_FAILFAST`.

!!! compat "Julia 1.9"
    `failfast` requires at least Julia 1.9.

The description string accepts interpolation from the loop indices.
If no description is provided, one is constructed based on the variables.
If a function call is provided, its name will be used. Explicit description strings override this behavior.

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

## Examples
```jldoctest; filter = r"trigonometric identities |    4      4  [0-9\\.]+s"
julia> @testset "trigonometric identities" begin
           θ = 2/3*π
           @test sin(-θ) ≈ -sin(θ)
           @test cos(-θ) ≈ cos(θ)
           @test sin(2θ) ≈ 2*sin(θ)*cos(θ)
           @test cos(2θ) ≈ cos(θ)^2 - sin(θ)^2
       end;
Test Summary:            | Pass  Total  Time
trigonometric identities |    4      4  0.2s
```

# `@testset for`

When `@testset for` is used, the macro starts a new test for each iteration of
the provided loop. The semantics of each test set are otherwise identical to that
of that `begin/end` case (as if used for each loop iteration).

# `@testset let`

When `@testset let` is used, the macro starts a *transparent* test set with
the given object added as a context object to any failing test contained
therein. This is useful when performing a set of related tests on one larger
object and it is desirable to print this larger object when any of the
individual tests fail. Transparent test sets do not introduce additional levels
of nesting in the test set hierarchy and are passed through directly to the
parent test set (with the context object appended to any failing tests.)

    !!! compat "Julia 1.9"
    `@testset let` requires at least Julia 1.9.

## Examples
```jldoctest
julia> @testset let logi = log(im)
           @test imag(logi) == π/2
           @test !iszero(real(logi))
       end
Test Failed at none:3
  Expression: !(iszero(real(logi)))
     Context: logi = 0.0 + 1.5707963267948966im
ERROR: There was an error during testing
```
"""
macro testset(args...)
    isempty(args) && error("No arguments to @testset")

    tests = args[end]

    # Determine if a single block or for-loop style
    if !isa(tests,Expr) || (tests.head !== :for && tests.head !== :block && tests.head !== :call && tests.head !== :let)

        error("Expected function call, begin/end block or for loop as argument to @testset")
    end

    FAIL_FAST[] = something(tryparse(Bool, get(ENV, "JULIA_TEST_FAILFAST", "false")), false)

    if tests.head === :for
        return testset_forloop(args, tests, __source__)
    elseif tests.head === :let
        return testset_context(args, tests, __source__)
    else
        return testset_beginend_call(args, tests, __source__)
    end
end

trigger_test_failure_break(@nospecialize(err)) =
    ccall(:jl_test_failure_breakpoint, Cvoid, (Any,), err)

"""
Generate the code for an `@testset` with a `let` argument.
"""
function testset_context(args, tests, source)
    desc, testsettype, options = parse_testset_args(args[1:end-1])
    if desc !== nothing || testsettype !== nothing
        # Reserve this syntax if we ever want to allow this, but for now,
        # just do the transparent context test set.
        error("@testset with a `let` argument cannot be customized")
    end

    assgn = tests.args[1]
    if !isa(assgn, Expr) || assgn.head !== :(=)
        error("`@testset let` must have exactly one assignment")
    end
    assignee = assgn.args[1]

    tests.args[2] = quote
        $push_testset($(ContextTestSet)($(QuoteNode(assignee)), $assignee; $options...))
        try
            $(tests.args[2])
        finally
            $pop_testset()
        end
    end

    return esc(tests)
end

"""
Generate the code for a `@testset` with a function call or `begin`/`end` argument
"""
function testset_beginend_call(args, tests, source)
    desc, testsettype, options = parse_testset_args(args[1:end-1])
    if desc === nothing
        if tests.head === :call
            desc = string(tests.args[1]) # use the function name as test name
        else
            desc = "test set"
        end
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
        local oldseed = Random.GLOBAL_SEED
        try
            # RNG is re-seeded with its own seed to ease reproduce a failed test
            Random.seed!(Random.GLOBAL_SEED)
            let
                $(esc(tests))
            end
        catch err
            err isa InterruptException && rethrow()
            # something in the test block threw an error. Count that as an
            # error in this test set
            trigger_test_failure_break(err)
            if err isa FailFastError
                get_testset_depth() > 1 ? rethrow() : failfast_print()
            else
                record(ts, Error(:nontest_error, Expr(:tuple), err, Base.current_exceptions(), $(QuoteNode(source))))
            end
        finally
            copy!(RNG, oldrng)
            Random.set_global_seed!(oldseed)
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

function failfast_print()
    printstyled("\nFail-fast enabled:"; color = Base.error_color(), bold=true)
    printstyled(" Fail or Error occured\n\n"; color = Base.error_color())
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
            finish_errored = true
            push!(arr, finish(ts))
            finish_errored = false

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
            trigger_test_failure_break(err)
            if !isa(err, FailFastError)
                record(ts, Error(:nontest_error, Expr(:tuple), err, Base.current_exceptions(), $(QuoteNode(source))))
            end
        end
    end
    quote
        local arr = Vector{Any}()
        local first_iteration = true
        local ts
        local finish_errored = false
        local RNG = default_rng()
        local oldrng = copy(RNG)
        local oldseed = Random.GLOBAL_SEED
        Random.seed!(Random.GLOBAL_SEED)
        local tmprng = copy(RNG)
        try
            let
                $(Expr(:for, Expr(:block, [esc(v) for v in loopvars]...), blk))
            end
        finally
            # Handle `return` in test body
            if !first_iteration && !finish_errored
                pop_testset()
                push!(arr, finish(ts))
            end
            copy!(RNG, oldrng)
            Random.set_global_seed!(oldseed)
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
            push!(options.args, Expr(:call, :(=>), key, esc(arg.args[2])))
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

Adds the test set to the `task_local_storage`.
"""
function push_testset(ts::AbstractTestSet)
    testsets = get(task_local_storage(), :__BASETESTNEXT__, AbstractTestSet[])
    push!(testsets, ts)
    setindex!(task_local_storage(), testsets, :__BASETESTNEXT__)
end

"""
    pop_testset()

Pops the last test set added to the `task_local_storage`. If there are no
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
MethodInstance for f(::Int64)
  from f(a)
     @ Main none:1
Arguments
  #self#::Core.Const(f)
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
    if isa(farg, Symbol) && farg !== :.. && first(string(farg)) == '.'
        farg = Symbol(string(farg)[2:end])
        ex = Expr(:call, GlobalRef(Test, :_materialize_broadcasted),
            farg, ex.args[2:end]...)
    end
    Base.remove_linenums!(let ex = ex;
        quote
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
                rettype <: allow || rettype == typesplit(inftypes[1], allow) || error("return type $rettype does not match inferred return type $(inftypes[1])")
                result
            end
        end
    end)
end

function is_in_mods(m::Module, recursive::Bool, mods)
    while true
        m in mods && return true
        recursive || return false
        p = parentmodule(m)
        p === m && return false
        m = p
    end
end

"""
    detect_ambiguities(mod1, mod2...; recursive=false,
                                      ambiguous_bottom=false,
                                      allowed_undefineds=nothing)

Returns a vector of `(Method,Method)` pairs of ambiguous methods
defined in the specified modules.
Use `recursive=true` to test in all submodules.

`ambiguous_bottom` controls whether ambiguities triggered only by
`Union{}` type parameters are included; in most cases you probably
want to set this to `false`. See [`Base.isambiguous`](@ref).

See [`Test.detect_unbound_args`](@ref) for an explanation of
`allowed_undefineds`.

!!! compat "Julia 1.8"
    `allowed_undefineds` requires at least Julia 1.8.
"""
function detect_ambiguities(mods::Module...;
                            recursive::Bool = false,
                            ambiguous_bottom::Bool = false,
                            allowed_undefineds = nothing)
    @nospecialize
    ambs = Set{Tuple{Method,Method}}()
    mods = collect(mods)::Vector{Module}
    function sortdefs(m1::Method, m2::Method)
        ord12 = cmp(m1.file, m2.file)
        if ord12 == 0
            ord12 = cmp(m1.line, m2.line)
        end
        return ord12 <= 0 ? (m1, m2) : (m2, m1)
    end
    function examine(mt::Core.MethodTable)
        for m in Base.MethodList(mt)
            m.sig == Tuple && continue # ignore Builtins
            is_in_mods(m.module, recursive, mods) || continue
            world = Base.get_world_counter()
            ambig = Ref{Int32}(0)
            ms = Base._methods_by_ftype(m.sig, nothing, -1, world, true, Ref(typemin(UInt)), Ref(typemax(UInt)), ambig)::Vector
            ambig[] == 0 && continue
            for match2 in ms
                match2 = match2::Core.MethodMatch
                m2 = match2.method
                 if !(m === m2 || Base.morespecific(m2.sig, m.sig))
                    if Base.isambiguous(m, m2; ambiguous_bottom)
                        push!(ambs, sortdefs(m, m2))
                    end
                end
            end
        end
    end
    work = Base.loaded_modules_array()
    filter!(mod -> mod === parentmodule(mod), work) # some items in loaded_modules_array are not top modules (really just Base)
    while !isempty(work)
        mod = pop!(work)
        for n in names(mod, all = true)
            Base.isdeprecated(mod, n) && continue
            if !isdefined(mod, n)
                if is_in_mods(mod, recursive, mods)
                    if allowed_undefineds === nothing || GlobalRef(mod, n) ∉ allowed_undefineds
                        println("Skipping ", mod, '.', n)  # typically stale exports
                    end
                end
                continue
            end
            f = Base.unwrap_unionall(getfield(mod, n))
            if isa(f, Module) && f !== mod && parentmodule(f) === mod && nameof(f) === n
                push!(work, f)
            elseif isa(f, DataType) && isdefined(f.name, :mt) && f.name.module === mod && f.name.name === n && f.name.mt !== Symbol.name.mt && f.name.mt !== DataType.name.mt
                examine(f.name.mt)
            end
        end
    end
    examine(Symbol.name.mt)
    examine(DataType.name.mt)
    return collect(ambs)
end

"""
    detect_unbound_args(mod1, mod2...; recursive=false, allowed_undefineds=nothing)

Returns a vector of `Method`s which may have unbound type parameters.
Use `recursive=true` to test in all submodules.

By default, any undefined symbols trigger a warning. This warning can
be suppressed by supplying a collection of `GlobalRef`s for which
the warning can be skipped. For example, setting

```
allow_undefineds = Set([GlobalRef(Base, :active_repl),
                        GlobalRef(Base, :active_repl_backend)])
```

would suppress warnings about `Base.active_repl` and
`Base.active_repl_backend`.

!!! compat "Julia 1.8"
    `allowed_undefineds` requires at least Julia 1.8.
"""
function detect_unbound_args(mods...;
                             recursive::Bool = false,
                             allowed_undefineds=nothing)
    @nospecialize mods
    ambs = Set{Method}()
    mods = collect(mods)::Vector{Module}
    function examine(mt::Core.MethodTable)
        for m in Base.MethodList(mt)
            is_in_mods(m.module, recursive, mods) || continue
            has_unbound_vars(m.sig) || continue
            tuple_sig = Base.unwrap_unionall(m.sig)::DataType
            if Base.isvatuple(tuple_sig)
                params = tuple_sig.parameters[1:(end - 1)]
                tuple_sig = Base.rewrap_unionall(Tuple{params...}, m.sig)
                world = Base.get_world_counter()
                mf = ccall(:jl_gf_invoke_lookup, Any, (Any, Any, UInt), tuple_sig, nothing, world)
                if mf !== nothing && mf !== m && mf.sig <: tuple_sig
                    continue
                end
            end
            push!(ambs, m)
        end
    end
    work = Base.loaded_modules_array()
    filter!(mod -> mod === parentmodule(mod), work) # some items in loaded_modules_array are not top modules (really just Base)
    while !isempty(work)
        mod = pop!(work)
        for n in names(mod, all = true)
            Base.isdeprecated(mod, n) && continue
            if !isdefined(mod, n)
                if is_in_mods(mod, recursive, mods)
                    if allowed_undefineds === nothing || GlobalRef(mod, n) ∉ allowed_undefineds
                        println("Skipping ", mod, '.', n)  # typically stale exports
                    end
                end
                continue
            end
            f = Base.unwrap_unionall(getfield(mod, n))
            if isa(f, Module) && f !== mod && parentmodule(f) === mod && nameof(f) === n
                push!(work, f)
            elseif isa(f, DataType) && isdefined(f.name, :mt) && f.name.module === mod && f.name.name === n && f.name.mt !== Symbol.name.mt && f.name.mt !== DataType.name.mt
                examine(f.name.mt)
            end
        end
    end
    examine(Symbol.name.mt)
    examine(DataType.name.mt)
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
                if vararg isa Core.TypeofVararg && isdefined(vararg, :N)
                    constrains_param(var, vararg.N, covariant) && return true
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
Base.ncodeunits(s::GenericString) = ncodeunits(s.string)::Int
Base.codeunit(s::GenericString) = codeunit(s.string)::Type{<:Union{UInt8, UInt16, UInt32}}
Base.codeunit(s::GenericString, i::Integer) = codeunit(s.string, i)::Union{UInt8, UInt16, UInt32}
Base.isvalid(s::GenericString, i::Integer) = isvalid(s.string, i)::Bool
Base.iterate(s::GenericString, i::Integer=1) = iterate(s.string, i)::Union{Nothing,Tuple{AbstractChar,Int}}
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
guardseed(f::Function, seed::Union{Vector{UInt64},Vector{UInt32},Integer,NTuple{4,UInt64}}) = guardseed() do
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
