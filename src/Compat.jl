module Compat

using Base.Meta

if VERSION < v"0.4.0-dev+1419"
    export UInt, UInt8, UInt16, UInt32, UInt64, UInt128
    const UInt = Uint
    const UInt8 = Uint8
    const UInt16 = Uint16
    const UInt32 = Uint32
    const UInt64 = Uint64
    const UInt128 = Uint128
end

if VERSION < v"0.4.0-dev+1387"
    typealias AbstractString String
    export AbstractString
end

if VERSION < v"0.4.0-dev+412"
    eval(Base, :(const IPAddr = IpAddr))
end

if VERSION < v"0.4.0-dev+2197"
    Base.IPv4(ipstr::AbstractString) = Base.parseipv4(ipstr)
    Base.IPv6(ipstr::AbstractString) = Base.parseipv6(ipstr)
end

if VERSION < v"0.4.0-dev+2200"
    function Base.isless{T<:Base.IPAddr}(a::T, b::T)
        return isless(a.host, b.host)
    end
end

if VERSION < v"0.4.0-dev+980"
    macro Dict(pairs...)
        esc(Expr(:dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:typed_dict, :(Any=>Any), pairs...))
    end

    Base.Dict(kv) = dict_with_eltype(kv, eltype(kv))
    dict_with_eltype{K,V}(kv, ::Type{(K,V)}) = Dict{K,V}(kv)
    dict_with_eltype(kv, t) = Dict{Any,Any}(kv)
else
    macro Dict(pairs...)
        esc(Expr(:call, :Dict, pairs...))
    end
    macro AnyDict(pairs...)
        esc(Expr(:call, :(Base.AnyDict), pairs...))
    end
end

import Base: round, ceil, floor, trunc
if VERSION < v"0.4.0-dev+1827"
    for (fnew,fold) in ((:round, :iround), (:ceil, :iceil), (:floor, :ifloor), (:trunc, :itrunc))
        @eval begin
            ($fnew){T<:Integer}(::Type{T}, x::Integer) = convert(T, x)  # ambiguity resolution with digits/base version, not all old methods defined
            ($fnew){T<:Integer}(::Type{T}, x) = ($fold)(T, x)
            ($fnew){T<:Integer}(::Type{T}, x::Rational) = convert(T, ($fold)(x)) # no e.g. iround(::Type{T}, x::Rational) is defined in 0.3
        end
    end
end

if VERSION < v"0.4.0-dev+1884"
    randexp(rng::MersenneTwister) = Base.Random.randmtzig_exprnd(rng)
    randexp() = Base.Random.randmtzig_exprnd()
    export randexp
end

if VERSION < v"0.4.0-dev+2014"
    sizehint! = Base.sizehint
    export sizehint!
end

function rewrite_dict(ex)
    length(ex.args) == 1 && return ex

    f = ex.args[1]
    if isexpr(f, :curly)
        newex = Expr(:typed_dict, :($(f.args[2])=>$(f.args[3])))
    else
        newex = Expr(:dict)
    end

    for i = 2:length(ex.args)
        pair = ex.args[i]
        !isexpr(pair, :(=>)) && return ex
        push!(newex.args, pair)
    end
    newex
end

# rewrite Julia 0.4-style split or rsplit (str, splitter; kws...)
# into 0.2/0.3-style positional arguments
function rewrite_split(ex, f)
    limit = nothing
    keep = nothing
    for i in 4:length(ex.args)
        if isexpr(ex.args[i], :kw)
            kw = ex.args[i].args
            if kw[1] == :limit
                limit = kw[2]
            elseif kw[1] == :keep
                keep = kw[2]
            end
        end
    end
    if limit == nothing
        if keep == nothing
            return Expr(:call, f, ex.args[2], ex.args[3])
        else
            return Expr(:call, f, ex.args[2], ex.args[3], keep)
        end
    else
        if keep == nothing
            return Expr(:call, f, ex.args[2], ex.args[3], limit)
        else
            return Expr(:call, f, ex.args[2], ex.args[3], limit, keep)
        end
    end
end

if VERSION < v"0.4.0-dev+707"
    macro inline(ex)
        esc(ex)
    end
end

if VERSION < v"0.4.0-dev+2056"
    macro noinline(ex)
        esc(ex)
    end
end

function _compat(ex::Expr)
    if ex.head == :call
        f = ex.args[1]
        if VERSION < v"0.4.0-dev+980" && (f == :Dict || (isexpr(f, :curly) && length(f.args) == 3 && f.args[1] == :Dict))
            ex = rewrite_dict(ex)
        elseif VERSION < v"0.4.0-dev+129" && (f == :split || f == :rsplit) && length(ex.args) >= 4 && isexpr(ex.args[4], :kw)
            ex = rewrite_split(ex, f)
        end
    end
    return Expr(ex.head, map(_compat, ex.args)...)
end
_compat(ex) = ex

macro compat(ex)
    esc(_compat(ex))
end

export @compat, @inline, @noinline

end # module
