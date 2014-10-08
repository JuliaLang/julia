# Support for @simd for

module SimdLoop

export @simd

# Error thrown from ill-formed uses of @simd
type SimdError <: Exception
    msg::ASCIIString
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
    elseif x.head === :macrocall && x.args[1] === symbol("@goto")
        throw(SimdError("$(x.args[1]) is not allowed inside a @simd loop body"))
    end
    for arg in x.args
        check_body!(arg)
    end
    return true
end
check_body!(x::QuoteNode) = check_body!(x.value)
check_body!(x) = true

# Compile Expr x in context of @simd.
function compile(x)
    (isa(x, Expr) && x.head == :for) || throw(SimdError("for loop expected"))
    length(x.args) == 2 || throw(SimdError("1D for loop expected"))
    check_body!(x)

    var,range = parse_iteration_space(x.args[1])
    r = gensym("r") # Range value
    n = gensym("n") # Trip count
    i = gensym("i") # Trip index
    quote
        # Evaluate range value once, to enhance type and data flow analysis by optimizers.
        let $r = $range, $n = length($r)
            if zero($n) < $n
                # Lower loop in way that seems to work best for LLVM 3.3 vectorizer.
                let $i = zero($n)
                    while $i < $n
                        local $var = first($r) + $i*step($r)
                        $(x.args[2])        # Body of loop
                        $i += 1
                        $(Expr(:simdloop))  # Mark loop as SIMD loop
                    end
                end
                # Set index to last value just like a regular for loop would
                $var = last($r)
            end
        end
        nothing
    end
end

macro simd(forloop)
    esc(compile(forloop))
end

end # module SimdLoop
