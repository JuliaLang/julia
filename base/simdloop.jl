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
    svar = Expr(:call, :symbol, string(var))
    n = gensym("n") # Trip count
    s = gensym("s") # Step
    i = gensym("i") # Index variable
    # LLVM vectorizer needs to compute a trip count, so make it obvious.
    quote
        let $var = first($range)
            local $n = length($range)
            local $s = step($range)
            local $i = zero($n)
            while $i < $n
                $(x.args[2])
                $var += $s
                $i += 1
                $(Expr(:simdloop))  # Mark loop as SIMD loop
            end
        end
        isdefined($svar) && ($var = last($range))
        nothing
    end
end

macro simd(forloop)
    esc(compile(forloop))
end

end # module SimdLoop
