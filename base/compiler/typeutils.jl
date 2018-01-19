# This file is a part of Julia. License is MIT: https://julialang.org/license

const _TYPE_NAME = Type.body.name

isType(@nospecialize t) = isa(t, DataType) && (t::DataType).name === _TYPE_NAME

# true if Type is inlineable as constant (is a singleton)
function isconstType(@nospecialize t)
    isType(t) || return false
    p1 = t.parameters[1]
    # typeof(Bottom) is special since even though it is as leaftype,
    # at runtime, it might be Type{Union{}} instead, so don't attempt inference of it
    p1 === typeof(Union{}) && return false
    p1 === Union{} && return true
    isleaftype(p1) && return true
    return false
end

iskindtype(@nospecialize t) = (t === DataType || t === UnionAll || t === Union || t === typeof(Bottom))

argtypes_to_type(argtypes::Array{Any,1}) = Tuple{anymap(widenconst, argtypes)...}

isknownlength(t::DataType) = !isvatuple(t) || (length(t.parameters) > 0 && isa(unwrap_unionall(t.parameters[end]).parameters[2],Int))

function type_depth(@nospecialize(t))
    if t === Bottom
        return 0
    elseif isa(t, Union)
        return max(type_depth(t.a), type_depth(t.b)) + 1
    elseif isa(t, DataType)
        return (t::DataType).depth
    elseif isa(t, UnionAll)
        if t.var.ub === Any && t.var.lb === Bottom
            return type_depth(t.body)
        end
        return max(type_depth(t.var.ub) + 1, type_depth(t.var.lb) + 1, type_depth(t.body))
    end
    return 0
end

# try to find `type` somewhere in `comparison` type
# at a minimum nesting depth of `mindepth`
function is_derived_type(@nospecialize(t), @nospecialize(c), mindepth::Int)
    if mindepth > 0
        mindepth -= 1
    end
    if t === c
        return mindepth == 0
    end
    if isa(c, TypeVar)
        # see if it is replacing a TypeVar upper bound with something simpler
        return is_derived_type(t, c.ub, mindepth)
    elseif isa(c, Union)
        # see if it is one of the elements of the union
        return is_derived_type(t, c.a, mindepth + 1) || is_derived_type(t, c.b, mindepth + 1)
    elseif isa(c, UnionAll)
        # see if it is derived from the body
        return is_derived_type(t, c.body, mindepth)
    elseif isa(c, DataType)
        if isa(t, DataType)
            # see if it is one of the supertypes of a parameter
            super = supertype(c)
            while super !== Any
                t === super && return true
                super = supertype(super)
            end
        end
        # see if it was extracted from a type parameter
        cP = c.parameters
        for p in cP
            is_derived_type(t, p, mindepth) && return true
        end
        if isleaftype(c) && isbits(c)
            # see if it was extracted from a fieldtype
            # however, only look through types that can be inlined
            # to ensure monotonicity of derivation
            # since we know that for immutable types,
            # the field types must have been constructed prior to the type,
            # it cannot have a reference cycle in the type graph
            cF = c.types
            for f in cF
                is_derived_type(t, f, mindepth) && return true
            end
        end
    end
    return false
end

function is_derived_type_from_any(@nospecialize(t), sources::SimpleVector, mindepth::Int)
    for s in sources
        is_derived_type(t, s, mindepth) && return true
    end
    return false
end

function valid_tparam(@nospecialize(x))
    if isa(x,Tuple)
        for t in x
            !valid_tparam(t) && return false
        end
        return true
    end
    return isa(x,Int) || isa(x,Symbol) || isa(x,Bool) || (!isa(x,Type) && isbits(x))
end

has_free_typevars(@nospecialize(t)) = ccall(:jl_has_free_typevars, Cint, (Any,), t)!=0

@pure function type_typeof(@nospecialize(v))
    if isa(v, Type)
        return Type{v}
    end
    return typeof(v)
end

# return an upper-bound on type `a` with type `b` removed
# such that `return <: a` && `Union{return, b} == Union{a, b}`
function typesubtract(@nospecialize(a), @nospecialize(b))
    if a <: b
        return Bottom
    end
    if isa(a, Union)
        return Union{typesubtract(a.a, b),
                     typesubtract(a.b, b)}
    end
    return a # TODO: improve this bound?
end

function tuple_tail_elem(@nospecialize(init), ct)
    return Vararg{widenconst(foldl((a, b) -> tmerge(a, unwrapva(b)), init, ct))}
end

# t[n:end]
function tupleparam_tail(t::SimpleVector, n)
    lt = length(t)
    if n > lt
        va = t[lt]
        if isvarargtype(va)
            # assumes that we should never see Vararg{T, x}, where x is a constant (should be guaranteed by construction)
            return Tuple{va}
        end
        return Tuple{}
    end
    return Tuple{t[n:lt]...}
end

# take a Tuple where one or more parameters are Unions
# and return an array such that those Unions are removed
# and `Union{return...} == ty`
function switchtupleunion(@nospecialize(ty))
    tparams = (unwrap_unionall(ty)::DataType).parameters
    return _switchtupleunion(Any[tparams...], length(tparams), [], ty)
end

function _switchtupleunion(t::Vector{Any}, i::Int, tunion::Vector{Any}, @nospecialize(origt))
    if i == 0
        tpl = rewrap_unionall(Tuple{t...}, origt)
        push!(tunion, tpl)
    else
        ti = t[i]
        if isa(ti, Union)
            for ty in uniontypes(ti::Union)
                t[i] = ty
                _switchtupleunion(t, i - 1, tunion, origt)
            end
            t[i] = ti
        else
            _switchtupleunion(t, i - 1, tunion, origt)
        end
    end
    return tunion
end
