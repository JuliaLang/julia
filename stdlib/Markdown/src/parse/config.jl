# This file is a part of Julia. License is MIT: https://julialang.org/license

const InnerConfig = Dict{Char, Vector{Function}}

mutable struct Config
    breaking::Vector{Function}
    regular::Vector{Function}
    inner::InnerConfig
end

Config() = Config(Function[], Function[], InnerConfig())

const META = IdDict{Function, Dict{Symbol, Any}}()

getset(coll, key, default) = coll[key] = get(coll, key, default)

meta(f) = getset(META, f, Dict{Symbol, Any}())

breaking!(f) = meta(f)[:breaking] = true
breaking(f) = get(meta(f), :breaking, false)

triggers!(f, ts) = meta(f)[:triggers] = Set{Char}(ts)
triggers(f) = get(meta(f), :triggers, Set{Char}())

# Macros

isexpr(x::Expr, ts...) = x.head in ts
isexpr(x::T, ts...) where {T} = T in ts

macro breaking(ex)
    isexpr(ex, :->) || error("invalid @breaking form, use ->")
    b, def = ex.args
    if b
        quote
            f = $(esc(def))
            breaking!(f)
            f
        end
    else
        esc(def)
    end
end

macro trigger(ex)
    isexpr(ex, :->) || error("invalid @triggers form, use ->")
    ts, def = ex.args
    quote
        f = $(esc(def))
        triggers!(f, $ts)
        f
    end
end

# Construction

function config(parsers::Function...)
    c = Config()
    for parser in parsers
        ts = triggers(parser)
        if breaking(parser)
            push!(c.breaking, parser)
        elseif !isempty(ts)
            for t in ts
                push!(getset(c.inner, t, Function[]), parser)
            end
        else
            push!(c.regular, parser)
        end
    end
    return c
end

# Flavour definitions

const flavors = Dict{Symbol, Config}()

macro flavor(name, features)
    quote
        const $(esc(name)) = config($(map(esc,features.args)...))
        flavors[$(Expr(:quote, name))] = $(esc(name))
    end
end
