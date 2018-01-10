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
    print_with_color(Base.error_color(), io, string(ex))
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
call to `srand(seed)` where `seed` is the current seed of the global RNG.
Moreover, after the execution of the body, the state of the global RNG is
restored to what it was before the `@testset`. This is meant to ease
reproducibility in case of failure, and to allow seamless
re-arrangements of `@testset`s regardless of their side-effect on the
global RNG state.
"""
macro testset(args...)
    isempty(args) && error("No arguments to @testset")

    tests = args[end]

    # Determine if a single block or for-loop style
    if !isa(tests,Expr) || (tests.head != :for && tests.head != :block)
        error("Expected begin/end block or for loop as argument to @testset")
    end

    if tests.head == :for
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
        ts = $(testsettype)($desc; $options...)
        # this empty loop is here to force the block to be compiled,
        # which is needed for backtrace scrubbing to work correctly.
        while false; end
        push_testset(ts)
        # we reproduce the logic of guardsrand, but this function
        # cannot be used as it changes slightly the semantic of @testset,
        # by wrapping the body in a function
        oldrng = copy(Base.GLOBAL_RNG)
        try
            # GLOBAL_RNG is re-seeded with its own seed to ease reproduce a failed test
            srand(Base.GLOBAL_RNG.seed)
            $(esc(tests))
        catch err
            # something in the test block threw an error. Count that as an
            # error in this test set
            record(ts, Error(:nontest_error, :(), err, catch_backtrace(), $(QuoteNode(source))))
        finally
            copy!(Base.GLOBAL_RNG, oldrng)
        end
        pop_testset()
        finish(ts)
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
        _check_testset($testsettype, $(QuoteNode(testsettype.args[1])))
        # Trick to handle `break` and `continue` in the test code before
        # they can be handled properly by `finally` lowering.
        if !first_iteration
            pop_testset()
            push!(arr, finish(ts))
            # it's 1000 times faster to copy from tmprng rather than calling srand
            copy!(Base.GLOBAL_RNG, tmprng)

        end
        ts = $(testsettype)($desc; $options...)
        push_testset(ts)
        first_iteration = false
        try
            $(esc(tests))
        catch err
            # Something in the test block threw an error. Count that as an
            # error in this test set
            record(ts, Error(:nontest_error, :(), err, catch_backtrace(), $(QuoteNode(source))))
        end
    end
    quote
        arr = Vector{Any}()
        local first_iteration = true
        local ts
        local oldrng = copy(Base.GLOBAL_RNG)
        srand(Base.GLOBAL_RNG.seed)
        local tmprng = copy(Base.GLOBAL_RNG)
        try
            $(Expr(:for, Expr(:block, [esc(v) for v in loopvars]...), blk))
        finally
            # Handle `return` in test body
            if !first_iteration
                pop_testset()
                push!(arr, finish(ts))
            end
            copy!(Base.GLOBAL_RNG, oldrng)
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

Returns the number of active test sets, not including the defaut test set
"""
function get_testset_depth()
    testsets = get(task_local_storage(), :__BASETESTNEXT__, AbstractTestSet[])
    return length(testsets)
end
