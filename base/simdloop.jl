# Support for @simd for

module SimdLoop

export @simd

# Error thrown from ill-formed uses of @simd
type SimdError <: Exception
    msg::ASCIIString
end

# Parse colon expression low:high, returning (low,high)
function parse_range( x::Expr )
    if x.head!=:(:)
        throw( SimdError("range must use : syntax"))
    elseif length(x.args)!=2
        throw( SimdError("wrong number of args in range"));
    else
        (x.args[1],x.args[2])
    end
end

# Parse iteration space expression
#       symbol '=' range
#       symbol 'in' range
function parse_iteration_space( x::Expr )
    if x.head!=:(=) && x.head!=:(in)
        throw( SimdError("= or in expected"))
    elseif length(x.args)!=2
        throw( SimdError("simd syntax error"))
    else
        sym = (x.args[1])::Symbol
        (low,high)=parse_range(x.args[2])
        return (sym,low,high)
    end
end

# Compile Expr x in context of @simd.
function compile(x::Expr)
    h = x.head
    if h != :for
        throw(SimdError("for loop expected"))
    elseif length(x.args)!=2
        throw(SimdError("1D for loop expected"))
    else
        (var,low,high) = parse_iteration_space(x.args[1])
        tmp = gensym()
        body = x.args[2]
        loop = quote
            local $tmp = $high+1
            local $var = $low
            while $var < $tmp
                $body
                $var = $var+1
                $(Expr(:simdloop))  # Mark loop as SIMD loop
            end
        end
        return loop
    end
end

macro simd(forloop)
    esc(compile(forloop))
end

end # simdloop
