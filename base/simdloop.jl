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
function parse_iteration_space( x )
    if !Meta.isexpr(x,[:(=),:in])
        throw( SimdError("= or in expected"))
    elseif length(x.args)!=2
        throw( SimdError("simd range syntax is wrong"))
    elseif !isa(x.args[1],Symbol)
        throw( SimdError("simd loop index must be a symbol"))
    else
        x.args # symbol, range
    end
end

# Compile Expr x in context of @simd.
function compile(x)
    if !Meta.isexpr(x,:for)
        throw(SimdError("for loop expected"))
    elseif length(x.args)!=2
        throw(SimdError("1D for loop expected"))
    else
        var,range = parse_iteration_space(x.args[1])
        r = gensym() # Range
        n = gensym() # Trip count
        s = gensym() # Step
        i = gensym() # Index variable
        # LLVM vectorizer needs to compute a trip count, so make it obivious.
        quote
            local $r = $range
            local $n = length($r)
            local $s = step($r)
            local $var = first($r)
            local $i = zero($n)
            while $i < $n
                $(x.args[2])
                $var += $s
                $i += 1
                $(Expr(:simdloop))  # Mark loop as SIMD loop
            end
        end
    end
end

macro simd(forloop)
    esc(compile(forloop))
end

end # simdloop
