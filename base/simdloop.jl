# This file is a part of Julia. License is MIT: https://julialang.org/license

# Support for @simd for

module SimdLoop

export @simd, simd_outer_range, simd_inner_length, simd_index

# Error thrown from ill-formed uses of @simd
struct SimdError <: Exception
    msg::AbstractString
end

# Parse iteration space expression
#       symbol '=' range
#       symbol 'in' range
function parse_iteration_space(x)
    (isa(x, Expr) && (x.head === :(=) || x.head === :in)) || throw(SimdError("= or in expected"))
    length(x.args) == 2 || throw(SimdError("simd range syntax is wrong"))
    isa(x.args[1], Symbol) || throw(SimdError("simd loop index must be a symbol"))
    x.args # symbol, range
end

# reject invalid control flow statements in @simd loop body
function check_body!(x::Expr)
    if x.head === :break || x.head === :continue
        throw(SimdError("$(x.head) is not allowed inside a @simd loop body"))
    elseif x.head === :macrocall && x.args[1] === Symbol("@goto")
        throw(SimdError("$(x.args[1]) is not allowed inside a @simd loop body"))
    end
    for arg in x.args
        check_body!(arg)
    end
    return true
end
check_body!(x::QuoteNode) = check_body!(x.value)
check_body!(x) = true

# @simd splits a for loop into two loops: an outer scalar loop and
# an inner loop marked with :loopinfo. The simd_... functions define
# the splitting.
# Custom iterators that do not support random access cannot support
# vectorization. In order to be compatible with `@simd` annotated loops,
#they should override `simd_inner_length(v::MyIter, j) = 1`,
#`simd_outer_range(v::MyIter) = v`, and `simd_index(v::MyIter, j, i) = j`.

# Get range for outer loop.
simd_outer_range(r) = 0:0

# Get trip count for inner loop.
@inline simd_inner_length(r, j) = Base.length(r)

# Construct user-level element from original range, outer loop index j, and inner loop index i.
@inline simd_index(r, j, i) = (@inbounds ret = r[i+firstindex(r)]; ret)

# Compile Expr x in context of @simd.
function compile(x, ivdep)
    (isa(x, Expr) && x.head === :for) || throw(SimdError("for loop expected"))
    length(x.args) == 2 || throw(SimdError("1D for loop expected"))
    check_body!(x)

    var,range = parse_iteration_space(x.args[1])
    r = gensym("r") # Range value
    j = gensym("i") # Iteration variable for outer loop
    n = gensym("n") # Trip count for inner loop
    i = gensym("i") # Trip index for inner loop
    quote
        # Evaluate range value once, to enhance type and data flow analysis by optimizers.
        let $r = $range
            for $j in Base.simd_outer_range($r)
                let $n = Base.simd_inner_length($r,$j)
                    if zero($n) < $n
                        # Lower loop in way that seems to work best for LLVM 3.3 vectorizer.
                        let $i = zero($n)
                            while $i < $n
                                local $var = Base.simd_index($r,$j,$i)
                                $(x.args[2])        # Body of loop
                                $i += 1
                                $(Expr(:loopinfo, Symbol("julia.simdloop"), ivdep))  # Mark loop as SIMD loop
                            end
                        end
                    end
                end
            end
        end
        nothing
    end
end

"""
    @simd

Annotate a `for` loop to allow the compiler to take extra liberties to allow loop re-ordering

!!! warning
    This feature is experimental and could change or disappear in future versions of Julia.
    Incorrect use of the `@simd` macro may cause unexpected results.

The object iterated over in a `@simd for` loop should be a one-dimensional range.
By using `@simd`, you are asserting several properties of the loop:

* It is safe to execute iterations in arbitrary or overlapping order, with special consideration for reduction variables.
* Floating-point operations on reduction variables can be reordered, possibly causing different results than without `@simd`.

In many cases, Julia is able to automatically vectorize inner for loops without the use of `@simd`.
Using `@simd` gives the compiler a little extra leeway to make it possible in more situations. In
either case, your inner loop should have the following properties to allow vectorization:

* The loop must be an innermost loop
* The loop body must be straight-line code. Therefore, [`@inbounds`](@ref) is
    currently needed for all array accesses. The compiler can sometimes turn
    short `&&`, `||`, and `?:` expressions into straight-line code if it is safe
    to evaluate all operands unconditionally. Consider using the [`ifelse`](@ref)
    function instead of `?:` in the loop if it is safe to do so.
* Accesses must have a stride pattern and cannot be "gathers" (random-index
    reads) or "scatters" (random-index writes).
* The stride should be unit stride.

!!! note
    The `@simd` does not assert by default that the loop is completely free of loop-carried
    memory dependencies, which is an assumption that can easily be violated in generic code.
    If you are writing non-generic code, you can use `@simd ivdep for ... end` to also assert that:

* There exists no loop-carried memory dependencies
* No iteration ever waits on a previous iteration to make forward progress.
"""
macro simd(forloop)
    esc(compile(forloop, nothing))
end

macro simd(ivdep, forloop)
    if ivdep === :ivdep
        esc(compile(forloop, Symbol("julia.ivdep")))
    else
        throw(SimdError("Only ivdep is valid as the first argument to @simd"))
    end
end

end # module SimdLoop
