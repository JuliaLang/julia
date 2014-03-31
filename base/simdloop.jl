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
    Meta.isexpr(x, [:(=), :in]) || throw(SimdError("= or in expected"))
    length(x.args) == 2 || throw(SimdError("simd range syntax is wrong"))
    isa(x.args[1], Symbol) || throw(SimdError("simd loop index must be a symbol"))
    x.args # symbol, range
end

# Compile Expr x in context of @simd.
function compile(x)
    Meta.isexpr(x, :for) || throw(SimdError("for loop expected"))
    length(x.args) == 2 || throw(SimdError("1D for loop expected"))

    var,range = parse_iteration_space(x.args[1])
    r = gensym("r") # Range value
    n = gensym("n") # Trip count
    s = gensym("s") # Step
    i = gensym("i") # Index variable
    quote
        # Evaluate range value once, to enhance type and data flow analysis by optimizers.
        let $r = $range, $n = length($r)
            if zero($n) < $n
                let $var = first($r)
                    # LLVM vectorizer needs to compute a trip count, so make it obvious.
                    local $s = step($r)
                    local $i = zero($n)
                    while $i < $n
                        $(x.args[2])
                        $var += $s
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
