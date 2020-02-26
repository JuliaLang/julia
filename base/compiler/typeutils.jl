# This file is a part of Julia. License is MIT: https://julialang.org/license

#####################
# lattice utilities #
#####################

function rewrap(@nospecialize(t), @nospecialize(u))
    if isa(t, TypeVar) || isa(t, Type)
        return rewrap_unionall(t, u)
    end
    return t
end

isType(@nospecialize t) = isa(t, DataType) && t.name === _TYPE_NAME

# true if Type{T} is inlineable as constant T
# requires that T is a singleton, s.t. T == S implies T === S
isconstType(@nospecialize t) = isType(t) && hasuniquerep(t.parameters[1])

# test whether type T has a unique representation, s.t. T == S implies T === S
function hasuniquerep(@nospecialize t)
    # typeof(Bottom) is special since even though it is a leaftype,
    # at runtime, it might be Type{Union{}} instead, so don't attempt inference of it
    t === typeof(Union{}) && return false
    t === Union{} && return true
    isa(t, TypeVar) && return false # TypeVars are identified by address, not equality
    iskindtype(typeof(t)) || return true # non-types are always compared by egal in the type system
    isconcretetype(t) && return true # these are also interned and pointer comparable
    if isa(t, DataType) && t.name !== Tuple.name && !isvarargtype(t) # invariant DataTypes
        return _all(hasuniquerep, t.parameters)
    end
    return false
end

function has_nontrivial_const_info(@nospecialize t)
    isa(t, PartialStruct) && return true
    return isa(t, Const) && !isdefined(typeof(t.val), :instance) && !(isa(t.val, Type) && hasuniquerep(t.val))
end

# Subtyping currently intentionally answers certain queries incorrectly for kind types. For
# some of these queries, this check can be used to somewhat protect against making incorrect
# decisions based on incorrect subtyping. Note that this check, itself, is broken for
# certain combinations of `a` and `b` where one/both isa/are `Union`/`UnionAll` type(s)s.
isnotbrokensubtype(@nospecialize(a), @nospecialize(b)) = (!iskindtype(b) || !isType(a) || hasuniquerep(a.parameters[1]))

argtypes_to_type(argtypes::Array{Any,1}) = Tuple{anymap(widenconst, argtypes)...}

function isknownlength(t::DataType)
    isvatuple(t) || return true
    return length(t.parameters) > 0 && isa(unwrap_unionall(t.parameters[end]).parameters[2], Int)
end

# test if non-Type, non-TypeVar `x` can be used to parameterize a type
function valid_tparam(@nospecialize(x))
    if isa(x, Tuple)
        for t in x
            isa(t, Symbol) || isbits(t) || return false
        end
        return true
    end
    return isa(x, Symbol) || isbits(x)
end

# return an upper-bound on type `a` with type `b` removed
# such that `return <: a` && `Union{return, b} == Union{a, b}`
function typesubtract(@nospecialize(a), @nospecialize(b))
    if a <: b && isnotbrokensubtype(a, b)
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

function tuple_tail_elem(@nospecialize(init), ct::Vector{Any})
    t = init
    for x in ct
        # FIXME: this is broken: it violates subtyping relations and creates invalid types with free typevars
        t = tmerge(t, tvar_extent(unwrapva(x)))
    end
    return Vararg{widenconst(t)}
end

function countunionsplit(atypes::Union{SimpleVector,Vector{Any}})
    nu = 1
    for ti in atypes
        if isa(ti, Union)
            nu, ovf = Core.Intrinsics.checked_smul_int(nu, unionlen(ti::Union))
            if ovf
                return typemax(Int)
            end
        end
    end
    return nu
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

# unioncomplexity estimates the number of calls to `tmerge` to obtain the given type by
# counting the Union instances, taking also into account those hidden in a Tuple or UnionAll
function unioncomplexity(u::Union)
    inner = max(unioncomplexity(u.a), unioncomplexity(u.b))
    return inner == 0 ? 0 : 1 + inner
end
function unioncomplexity(t::DataType)
    t.name === Tuple.name || return 0
    c = 1
    for ti in t.parameters
        ci = unioncomplexity(ti)
        if ci > c
            c = ci
        end
    end
    return c
end
unioncomplexity(u::UnionAll) = max(unioncomplexity(u.body), unioncomplexity(u.var.ub))
unioncomplexity(@nospecialize(x)) = 0

function improvable_via_constant_propagation(@nospecialize(t))
    if isconcretetype(t) && t <: Tuple
        for p in t.parameters
            p === DataType && return true
        end
    end
    return false
end
