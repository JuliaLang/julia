# This file is a part of Julia. License is MIT: https://julialang.org/license

#####################
# lattice utilities #
#####################

function rewrap(@nospecialize(t), @nospecialize(u))
    isa(t, Const) && return t
    isa(t, Conditional) && return t
    return rewrap_unionall(t, u)
end

const _TYPE_NAME = Type.body.name
isType(@nospecialize t) = isa(t, DataType) && (t::DataType).name === _TYPE_NAME

# true if Type{T} is inlineable as constant T
# requires that T is a singleton, s.t. T == S implies T === S
isconstType(@nospecialize t) = isType(t) && issingletontype(t.parameters[1])

# test whether T is a singleton type, s.t. T == S implies T === S
function issingletontype(@nospecialize t)
    # typeof(Bottom) is special since even though it is a leaftype,
    # at runtime, it might be Type{Union{}} instead, so don't attempt inference of it
    t === typeof(Union{}) && return false
    t === Union{} && return true
    isa(t, TypeVar) && return false # TypeVars are identified by address, not equality
    iskindtype(typeof(t)) || return true # non-types are always compared by egal in the type system
    isconcretetype(t) && return true # these are also interned and pointer comparable
    if isa(t, DataType) && t.name !== Tuple.name && !isvarargtype(t) # invariant DataTypes
        return all(p -> issingletontype(p), t.parameters)
    end
    return false
end

iskindtype(@nospecialize t) = (t === DataType || t === UnionAll || t === Union || t === typeof(Bottom))
isconcretedispatch(@nospecialize t) = isconcretetype(t) && !iskindtype(t)

# equivalent to isdispatchtuple(Tuple{v}) || v == Union{}
# and is thus perhaps most similar to the old (pre-1.0) `isleaftype` query
function isdispatchelem(@nospecialize v)
    return (v === Bottom) || (v === typeof(Bottom)) ||
        isconcretedispatch(v) || (isType(v) && !has_free_typevars(v))
end

argtypes_to_type(argtypes::Array{Any,1}) = Tuple{anymap(widenconst, argtypes)...}

function isknownlength(t::DataType)
    isvatuple(t) || return true
    return length(t.parameters) > 0 && isa(unwrap_unionall(t.parameters[end]).parameters[2], Int)
end

# test if non-Type, non-TypeVar `x` can be used to parameterize a type
function valid_tparam(@nospecialize(x))
    if isa(x, Tuple)
        for t in x
            isa(t, Symbol) || isbitstype(typeof(t)) || return false
        end
        return true
    end
    return isa(x, Symbol) || isbitstype(typeof(x))
end

has_free_typevars(@nospecialize(t)) = ccall(:jl_has_free_typevars, Cint, (Any,), t) != 0

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

function tvar_extent(@nospecialize t)
    while t isa TypeVar
        t = t.ub
    end
    return t
end

_typename(@nospecialize a) = Union{}
_typename(a::TypeVar) = Core.TypeName
function _typename(a::Union)
    ta = _typename(a.a)
    tb = _typename(a.b)
    ta === tb && return ta # same type-name
    (ta === Union{} || tb === Union{}) && return Union{} # threw an error
    (ta isa Const && tb isa Const) && return Union{} # will throw an error (different type-names)
    return Core.TypeName # uncertain result
end
_typename(union::UnionAll) = _typename(union.body)
_typename(a::DataType) = Const(a.name)

function tuple_tail_elem(@nospecialize(init), ct)
    return Vararg{widenconst(foldl((a, b) -> tmerge(a, tvar_extent(unwrapva(b))), init, ct))}
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
