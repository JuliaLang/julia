# This file is a part of Julia. License is MIT: https://julialang.org/license

# NOTE: This is in a separate file from tuple.jl so that it can be conditionally included
# in sysimg.jl after the necessary machinery for `@generated` is defined. Furthermore,
# this code must not be loaded into Core.Compiler.

if nameof(@__MODULE__) === :Base

(::Type{T})(x::Tuple) where {T<:Tuple} = convert(T, x)  # still use `convert` for tuples

# resolve ambiguity between preceding and following methods
All16{E,N}(x::Tuple) where {E,N} = convert(All16{E,N}, x)

function (T::All16{E,N})(itr) where {E,N}
    len = N+16
    elts = collect(E, Iterators.take(itr,len))
    if length(elts) != len
        _totuple_err(T)
    end
    (elts...,)
end

@generated function (::Type{T})(itr)::T where {T<:Tuple}
    tuple_expr = Expr(:tuple)
    if isvatuple(T)
        t = unwrap_unionall(T)
        n = length(t.parameters) - 1
    else
        n = fieldcount(T)
    end
    for i in 1:n
        push!(tuple_expr.args, quote
            if done(itr, state)
                _totuple_err(T)
            else
                item, state = next(itr, state)
                convert(fieldtype(T, $i), item)
            end
        end)
    end
    if isvatuple(T)
        V = rewrap_unionall(unwrap_unionall(t.parameters[n + 1]), T)
        U = unwrapva(V)
        if n == 0 # then avoid creating a redundant iterator
            return :((collect($U, itr)...,))
        end
        push!(tuple_expr.args, Expr(:..., :(collect($U, Iterators.rest(itr, state)))))
    end
    return quote
        state = start(itr)
        $tuple_expr
    end
end

function _totuple_err(@nospecialize T)
    @_noinline_meta
    throw(ArgumentError("too few elements for tuple type $T"))
end

end # nameof(@__MODULE__) === :Base
