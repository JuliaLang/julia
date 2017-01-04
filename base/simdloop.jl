# This file is a part of Julia. License is MIT: http://julialang.org/license

# Support for @simd for

module SimdLoop

export @simd, simd_outer_range, simd_inner_length, simd_index

# Error thrown from ill-formed uses of @simd
type SimdError <: Exception
    msg::String
end

# Parse iteration space expression
#       symbol '=' range
#       symbol 'in' range
function parse_iteration_space(x)
    (isa(x, Expr) && (x.head == :(=) || x.head == :in)) || throw(SimdError("= or in expected"))
    length(x.args) == 2 || throw(SimdError("simd range syntax is wrong"))
    isa(x.args[1], Symbol) || throw(SimdError("simd loop index must be a symbol"))
    x.args # symbol, range
end

# reject invalid control flow statements in @simd loop body
function check_body!(x::Expr)
    if x.head === :break || x.head == :continue
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
# an inner loop marked with :simdloop. The simd_... functions define
# the splitting.

# Get range for outer loop.
simd_outer_range(r) = 0:0

# Get trip count for inner loop.
@inline simd_inner_length(r,j::Int) = length(r)

# Construct user-level element from original range, outer loop index j, and inner loop index i.
@inline simd_index(r,j::Int,i) = (@inbounds ret = r[i+1]; ret)

# Compile Expr x in context of @simd.
function compile(x)
    (isa(x, Expr) && x.head == :for) || throw(SimdError("for loop expected"))
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
                                $(Expr(:simdloop))  # Mark loop as SIMD loop
                            end
                        end
                        # Set index to last value just like a regular for loop would
                        $var = last($r)
                    end
                end
            end
        end
        nothing
    end
end

macro simd(forloop)
    esc(compile(forloop))
end

end # module SimdLoop
