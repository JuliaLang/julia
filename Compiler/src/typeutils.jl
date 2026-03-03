# This file is a part of Julia. License is MIT: https://julialang.org/license

#####################
# lattice utilities #
#####################

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
        return all(hasuniquerep, t.parameters)
    end
    return false
end

"""
    isTypeDataType(@nospecialize t)::Bool

For a type `t` test whether ∀S s.t. `isa(S, rewrap_unionall(Type{t}, ...))`,
we have `isa(S, DataType)`. In particular, if a statement is typed as `Type{t}`
(potentially wrapped in some `UnionAll`), then we are guaranteed that this statement
will be a `DataType` at runtime (and not e.g. a `Union` or `UnionAll` typeequal to it).
"""
function isTypeDataType(@nospecialize t)
    isa(t, DataType) || return false
    isType(t) && return false
    # Could be Union{} at runtime
    t === Core.TypeofBottom && return false
    # Return true if `t` is not covariant
    return t.name !== Tuple.name
end

has_extended_info(@nospecialize x) = (!isa(x, Type) && !isvarargtype(x)) || isType(x)

# Subtyping currently intentionally answers certain queries incorrectly for kind types. For
# some of these queries, this check can be used to somewhat protect against making incorrect
# decisions based on incorrect subtyping. Note that this check, itself, is broken for
# certain combinations of `a` and `b` where one/both isa/are `Union`/`UnionAll` type(s)s.
isnotbrokensubtype(@nospecialize(a), @nospecialize(b)) = (!iskindtype(b) || !isType(a) || hasuniquerep(a.parameters[1]) || b <: a)

function argtypes_to_type(argtypes::Vector{Any})
    argtypes = anymap(@nospecialize(a) -> isvarargtype(a) ? a : widenconst(a), argtypes)
    filter!(@nospecialize(x) -> !isvarargtype(x) || valid_as_lattice(unwrapva(x), true), argtypes)
    all(@nospecialize(x) -> isvarargtype(x) || valid_as_lattice(x, true), argtypes) || return Bottom
    return Tuple{argtypes...}
end

function isknownlength(t::DataType)
    isvatuple(t) || return true
    va = t.parameters[end]
    return isdefined(va, :N) && va.N isa Int
end

has_concrete_subtype(d::DataType) = d.flags & 0x0020 == 0x0020 # n.b. often computed only after setting the type and layout fields

# determine whether x is a valid lattice element
# For example, Type{v} is not valid if v is a value
# Accepts TypeVars and has_free_typevar also, since it assumes the user will rewrap it correctly
# If astag is true, then also requires that it be a possible type tag for a valid object
function valid_as_lattice(@nospecialize(x), astag::Bool=false)
    x === Bottom && false
    x isa TypeVar && return valid_as_lattice(x.ub, astag)
    x isa UnionAll && (x = unwrap_unionall(x))
    if x isa Union
        # the Union constructor ensures this (and we'll recheck after
        # operations that might remove the Union itself)
        return true
    end
    if x isa DataType
        if isType(x)
            p = x.parameters[1]
            p isa Type || p isa TypeVar || return false
        elseif astag && isstructtype(x)
            datatype_fieldtypes(x) # force computation of has_concrete_subtype to be updated now
            return has_concrete_subtype(x)
        end
        return true
    end
    return false
end

function valid_typeof_tparam(@nospecialize(t))
    if t === Symbol || t === Module || isbitstype(t)
        return true
    end
    isconcretetype(t) || return false
    if t <: NamedTuple
        t = t.parameters[2]::DataType
    end
    if t <: Tuple
        for p in t.parameters
            valid_typeof_tparam(p) || return false
        end
        return true
    end
    return false
end

# test if non-Type, non-TypeVar `x` can be used to parameterize a type
valid_tparam(@nospecialize(x)) = valid_typeof_tparam(typeof(x))

function compatible_vatuple(a::DataType, b::DataType)
    vaa = a.parameters[end]
    vab = b.parameters[end]
    if !(isvarargtype(vaa) && isvarargtype(vab))
        return isvarargtype(vaa) == isvarargtype(vab)
    end
    isdefined(vaa, :N) || return !isdefined(vab, :N)
    isdefined(vab, :N) || return false
    return vaa.N === vab.N
end

# return an upper-bound on type `a` with type `b` removed
# and also any contents that are not valid type tags on any objects
# such that `return <: a` && `Union{return, b} == Union{a, b}`
function typesubtract(@nospecialize(a), @nospecialize(b), max_union_splitting::Int)
    if a <: b && isnotbrokensubtype(a, b)
        return Bottom
    end
    ua = unwrap_unionall(a)
    if isa(ua, Union)
        uua = typesubtract(rewrap_unionall(ua.a, a), b, max_union_splitting)
        uub = typesubtract(rewrap_unionall(ua.b, a), b, max_union_splitting)
        return Union{valid_as_lattice(uua, true) ? uua : Union{},
                     valid_as_lattice(uub, true) ? uub : Union{}}
    elseif a isa DataType
        ub = unwrap_unionall(b)
        if ub isa DataType
            if a.name === ub.name === Tuple.name && length(a.parameters) == length(ub.parameters)
                if 1 < unionsplitcost(JLTypeLattice(), a.parameters) <= max_union_splitting
                    ta = switchtupleunion(a)
                    return typesubtract(Union{ta...}, b, 0)
                elseif b isa DataType
                    if !compatible_vatuple(a, b)
                        return a
                    end
                    # if exactly one element is not bottom after calling typesubtract
                    # then the result is all of the elements as normal except that one
                    notbottom = fill(false, length(a.parameters))
                    for i = 1:length(notbottom)
                        ap = unwrapva(a.parameters[i])
                        bp = unwrapva(b.parameters[i])
                        notbottom[i] = !(ap <: bp && isnotbrokensubtype(ap, bp))
                    end
                    let i = findfirst(notbottom)
                        if i !== nothing && findnext(notbottom, i + 1) === nothing
                            ta = collect(a.parameters)
                            ap = a.parameters[i]
                            bp = b.parameters[i]
                            (isvarargtype(ap) || isvarargtype(bp)) && return a
                            ta[i] = typesubtract(ap, bp, min(2, max_union_splitting))
                            ta[i] === Union{} && return Union{}
                            return Tuple{ta...}
                        end
                    end
                end
            end
        end
    end
    return a # TODO: improve this bound?
end

_typename(@nospecialize a) = Union{}
_typename(::TypeVar) = Core.TypeName
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

function tuple_tail_elem(𝕃::AbstractLattice, @nospecialize(init), ct::Vector{Any})
    t = init
    for x in ct
        # FIXME: this is broken: it violates subtyping relations and creates invalid types with free typevars
        t = tmerge(𝕃, t, unwraptv(unwrapva(x)))
    end
    return Vararg{widenconst(t)}
end

# Given `fargs` from `ArgInfo` and optionally `argtypes`, compute alias groups
# for argument positions. Returns `nothing` if `fargs === nothing`, otherwise a
# `Vector{Int}` where `groups[i]` is the index of the leader for position `i`.
# `groups[i] == i` means leader (or non-aliased); `groups[i] < i` means follower.
#
# Aliasing is detected from two sources:
# 1. IR identity: same `SlotNumber`/`SSAValue` in `fargs`
# 2. `MustAlias` in `argtypes`: same `(slot, ssadef, fldidx)` means same value
function compute_alias_groups(
        na::Int,
        fargs::Union{Nothing,Vector{Any}},
        argtypes::Union{Nothing,Vector{Any}}
    )
    groups = Vector{Int}(undef, na)
    for i = 1:na
        groups[i] = i
    end
    fargs !== nothing && merge_fargs_alias_groups!(groups, fargs)
    argtypes !== nothing && merge_mustalias_groups!(groups, argtypes)
    return groups
end

# Detect aliasing from IR identity: same `SlotNumber`/`SSAValue` in `fargs`.
function merge_fargs_alias_groups!(groups::Vector{Int}, fargs::Vector{Any})
    for i = 1:length(groups)
        arg_i = fargs[i]
        if arg_i isa SlotNumber || arg_i isa SSAValue
            for j in 1:i-1
                if fargs[j] === arg_i
                    groups[i] = groups[j]
                    break
                end
            end
        end
    end
    return groups
end

# Detect additional aliasing from `MustAlias` lattice elements in `argtypes`:
# two positions with the same `(slot, ssadef, fldidx)` must refer to the same value.
function merge_mustalias_groups!(groups::Vector{Int}, argtypes::Vector{Any})
    for i = 1:length(groups)
        groups[i] == i || continue # already a follower
        ti = argtypes[i]
        ti isa MustAlias || continue
        for j in 1:i-1
            groups[j] == j || continue # only match against leaders
            tj = argtypes[j]
            tj isa MustAlias || continue
            if ti.slot == tj.slot && ti.ssadef == tj.ssadef && ti.fldidx == tj.fldidx
                # Merge: make j the leader for i, and re-point any
                # existing followers of i to j as well
                for k in i:length(groups)
                    if groups[k] == i
                        groups[k] = j
                    end
                end
                break
            end
        end
    end
    return groups
end

# Gives a cost function over the effort to switch a tuple-union representation
# as a cartesian product, relative to the size of the original representation.
# Thus, we count the longest element as being roughly invariant to being inside
# or outside of the Tuple/Union nesting, though somewhat more expensive to be
# outside than inside because the representation is larger (because and it
# informs the callee whether any splitting is possible).
function unionsplitcost(𝕃::AbstractLattice, argtypes::Union{SimpleVector,Vector{Any}};
                        fargs::Union{Nothing,Vector{Any}}=nothing)
    na = length(argtypes)
    groups = compute_alias_groups(na, fargs, argtypes isa Vector{Any} ? argtypes : nothing)
    nu = 1
    max = 2
    for i in 1:na
        # skip followers: their type is constrained by their leader
        if groups !== nothing && groups[i] != i
            continue
        end
        ti = argtypes[i]
        if has_extended_unionsplit(𝕃) && !isvarargtype(ti)
            ti = widenconst(ti)
        end
        if isa(ti, Union)
            nti = unionlen(ti)
            if nti > max
                max, nti = nti, max
            end
            nu, ovf = Core.Intrinsics.checked_smul_int(nu, nti)
            ovf && return typemax(Int)
        end
    end
    return nu
end

# take a Tuple where one or more parameters are Unions
# and return an array such that those Unions are removed
# and `Union{return...} == ty`
function switchtupleunion(@nospecialize(ty))
    tparams = (unwrap_unionall(ty)::DataType).parameters
    return _switchtupleunion(JLTypeLattice(), Any[tparams...], length(tparams), [], ty)
end

function switchtupleunion(𝕃::AbstractLattice, argtypes::Vector{Any};
                          fargs::Union{Nothing,Vector{Any}}=nothing)
    na = length(argtypes)
    groups = compute_alias_groups(na, fargs, argtypes)
    return _switchtupleunion(𝕃, argtypes, na, [], nothing, groups)
end

function _switchtupleunion(𝕃::AbstractLattice, t::Vector{Any}, i::Int, tunion::Vector{Any},
                           @nospecialize(origt), groups::Union{Nothing,Vector{Int}}=nothing)
    if i == 0
        if origt === nothing
            push!(tunion, copy(t))
        else
            tpl = rewrap_unionall(Tuple{t...}, origt)
            push!(tunion, tpl)
        end
        return tunion
    end

    if groups !== nothing && groups[i] != i
        # If this position is a follower (aliased to an earlier position),
        # its type is already set by the leader — just recurse without iterating.
        _switchtupleunion(𝕃, t, i - 1, tunion, origt, groups)
    else
        origti = ti = t[i]
        followers = Int[]
        if groups !== nothing
            for j in 1:length(t)
                if groups[j] == i # Collect follower indices for this leader
                    push!(followers, j)
                end
            end
        end
        # TODO Generalize this to allow callsite union-splitting of MustAlias
        if isa(ti, Union)
            origtypes = Any[t[j] for j in followers]
            for ty in uniontypes(ti)
                t[i] = ty
                for j in followers
                    t[j] = ty
                end
                _switchtupleunion(𝕃, t, i - 1, tunion, origt, groups)
            end
            t[i] = origti
            for (k, j) in enumerate(followers)
                t[j] = origtypes[k]
            end
        elseif (has_extended_unionsplit(𝕃) && !isa(ti, Const) && !isvarargtype(ti) &&
            (wty = widenconst(ti); isa(wty, Union)))
            origtypes = Any[t[j] for j in followers]
            for ty in uniontypes(wty)
                t[i] = ty
                for j in followers
                    t[j] = ty
                end
                _switchtupleunion(𝕃, t, i - 1, tunion, origt, groups)
            end
            t[i] = origti
            for (k, j) in enumerate(followers)
                t[j] = origtypes[k]
            end
        else
            _switchtupleunion(𝕃, t, i - 1, tunion, origt, groups)
        end
    end
    return tunion
end

# unioncomplexity estimates the number of calls to `tmerge` to obtain the given type by
# counting the Union instances, taking also into account those hidden in a Tuple or UnionAll
unioncomplexity(@nospecialize x) = _unioncomplexity(x)::Int
function _unioncomplexity(@nospecialize x)
    if isa(x, DataType)
        x.name === Tuple.name || return 0
        c = 0
        for ti in x.parameters
            c = max(c, unioncomplexity(ti))
        end
        return c
    elseif isa(x, Union)
        return unioncomplexity(x.a) + unioncomplexity(x.b) + 1
    elseif isa(x, UnionAll)
        return max(unioncomplexity(x.body), unioncomplexity(x.var.ub))
    elseif isa(x, TypeofVararg)
        return isdefined(x, :T) ? unioncomplexity(x.T) + 1 : 1
    else
        return 0
    end
end

function unionall_depth(@nospecialize ua) # aka subtype_env_size
    depth = 0
    while ua isa UnionAll
        depth += 1
        ua = ua.body
    end
    return depth
end

function unwraptv_ub(@nospecialize t)
    while isa(t, TypeVar)
        t = t.ub
    end
    return t
end
function unwraptv_lb(@nospecialize t)
    while isa(t, TypeVar)
        t = t.lb
    end
    return t
end
const unwraptv = unwraptv_ub

"""
    is_identity_free_argtype(argtype)::Bool

Return `true` if the `argtype` object is identity free in the sense that this type or any
reachable through its fields has non-content-based identity (see `Base.isidentityfree`).
This query is specifically designed for `adjust_effects`, enabling it to refine the
`:consistent` effect property tainted by mutable allocation(s) within the analyzed call
graph when the return value type is `is_identity_free_argtype`, ensuring that the allocated
mutable objects are never returned.
"""
is_identity_free_argtype(@nospecialize ty) = is_identity_free_type(widenconst(ignorelimited(ty)))
is_identity_free_type(@nospecialize ty) = isidentityfree(ty)

"""
    is_immutable_argtype(argtype)::Bool

Return `true` if the `argtype` object is known to be immutable.
This query is specifically designed for `getfield_effects` and `isdefined_effects`, allowing
them to prove `:consistent`-cy of `getfield` / `isdefined` calls when applied to immutable
objects. Otherwise, we need to additionally prove that the non-immutable object is not a
global object to prove the `:consistent`-cy.
"""
is_immutable_argtype(@nospecialize argtype) = is_immutable_type(widenconst(ignorelimited(argtype)))
is_immutable_type(@nospecialize ty) = _is_immutable_type(unwrap_unionall(ty))
function _is_immutable_type(@nospecialize ty)
    if isa(ty, Union)
        return _is_immutable_type(ty.a) && _is_immutable_type(ty.b)
    end
    return !isabstracttype(ty) && !ismutabletype(ty)
end

"""
    is_mutation_free_argtype(argtype)::Bool

Return `true` if `argtype` object is mutation free in the sense that no mutable memory
is reachable from this type (either in the type itself) or through any fields
(see `Base.ismutationfree`).
This query is specifically written for analyzing the `:inaccessiblememonly` effect property
and is supposed to improve the analysis accuracy by not tainting the `:inaccessiblememonly`
property when there is access to mutation-free global object.
"""
is_mutation_free_argtype(@nospecialize(argtype)) =
    is_mutation_free_type(widenconst(ignorelimited(argtype)))
is_mutation_free_type(@nospecialize ty) = ismutationfree(ty)
