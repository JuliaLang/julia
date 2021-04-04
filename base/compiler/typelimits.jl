# This file is a part of Julia. License is MIT: https://julialang.org/license

#########################
# limitation parameters #
#########################

const MAX_TYPEUNION_COMPLEXITY = 3
const MAX_TYPEUNION_LENGTH = 3
const MAX_INLINE_CONST_SIZE = 256

#########################
# limitation heuristics #
#########################

# limit the complexity of type `t` to be simpler than the comparison type `compare`
# no new values may be introduced, so the parameter `source` encodes the set of all values already present
# the outermost tuple type is permitted to have up to `allowed_tuplelen` parameters
function limit_type_size(@nospecialize(t), @nospecialize(compare), @nospecialize(source), allowed_tupledepth::Int, allowed_tuplelen::Int)
    source = svec(unwrap_unionall(compare), unwrap_unionall(source))
    source[1] === source[2] && (source = svec(source[1]))
    type_more_complex(t, compare, source, 1, allowed_tupledepth, allowed_tuplelen) || return t
    r = _limit_type_size(t, compare, source, 1, allowed_tuplelen)
    #@assert t <: r # this may fail if t contains a typevar in invariant and multiple times
        # in covariant position and r looses the occurrence in invariant position (see #36407)
    if !(t <: r) # ideally, this should never happen
        # widen to minimum complexity to obtain a valid result
        r = _limit_type_size(t, Any, source, 1, allowed_tuplelen)
        t <: r || (r = Any) # final escape hatch
    end
    #@assert r === _limit_type_size(r, t, source) # this monotonicity constraint is slightly stronger than actually required,
      # since we only actually need to demonstrate that repeated application would reaches a fixed point,
      #not that it is already at the fixed point
    return r
end

# try to find `type` somewhere in `comparison` type
# at a minimum nesting depth of `mindepth`
function is_derived_type(@nospecialize(t), @nospecialize(c), mindepth::Int)
    if t === c
        return mindepth <= 1
    end
    if isa(c, Union)
        # see if it is one of the elements of the union
        return is_derived_type(t, c.a, mindepth) || is_derived_type(t, c.b, mindepth)
    elseif isa(c, UnionAll)
        # see if it is derived from the body
        # also handle the var here, since this construct bounds the mindepth to the smallest possible value
        return is_derived_type(t, c.var.ub, mindepth) || is_derived_type(t, c.body, mindepth)
    elseif isa(c, Core.TypeofVararg)
        return is_derived_type(t, unwrapva(c), mindepth)
    elseif isa(c, DataType)
        if mindepth > 0
            mindepth -= 1
        end
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
    end
    return false
end

function is_derived_type_from_any(@nospecialize(t), sources::SimpleVector, mindepth::Int)
    for s in sources
        is_derived_type(t, s, mindepth) && return true
    end
    return false
end

# The goal of this function is to return a type of greater "size" and less "complexity" than
# both `t` or `c` over the lattice defined by `sources`, `depth`, and `allowed_tuplelen`.
function _limit_type_size(@nospecialize(t), @nospecialize(c), sources::SimpleVector, depth::Int, allowed_tuplelen::Int)
    if t === c
        return t # quick egal test
    elseif t === Union{}
        return t # easy case
    elseif isa(t, DataType) && isempty(t.parameters)
        return t # fast path: unparameterized are always simple
    else
        ut = unwrap_unionall(t)
        if isa(ut, DataType) && isa(c, Type) && c !== Union{} && c <: t
            # TODO: need to check that the UnionAll bounds on t are limited enough too
            return t # t is already wider than the comparison in the type lattice
        elseif is_derived_type_from_any(ut, sources, depth)
            return t # t isn't something new
        end
    end
    # peel off (and ignore) wrappers - they contribute no useful information, so we don't need to consider their size
    # first attempt to turn `c` into a type that contributes meaningful information
    # by peeling off meaningless non-matching wrappers of comparison one at a time
    # then unwrap `t`
    if isa(c, TypeVar)
        if isa(t, TypeVar) && t.ub === c.ub && (t.lb === Union{} || t.lb === c.lb)
            return t # it's ok to change the name, or widen `lb` to Union{}, so we can handle this immediately here
        end
        return _limit_type_size(t, c.ub, sources, depth, allowed_tuplelen)
    end
    if isa(c, UnionAll)
        return _limit_type_size(t, c.body, sources, depth, allowed_tuplelen)
    end
    if isa(t, UnionAll)
        tbody = _limit_type_size(t.body, c, sources, depth, allowed_tuplelen)
        tbody === t.body && return t
        return UnionAll(t.var, tbody)
    elseif isa(t, TypeVar)
        # don't have a matching TypeVar in comparison, so we keep just the upper bound
        return _limit_type_size(t.ub, c, sources, depth, allowed_tuplelen)
    elseif isa(t, Union)
        if isa(c, Union)
            a = _limit_type_size(t.a, c.a, sources, depth, allowed_tuplelen)
            b = _limit_type_size(t.b, c.b, sources, depth, allowed_tuplelen)
            return Union{a, b}
        end
    elseif isa(t, Core.TypeofVararg)
        isa(c, Core.TypeofVararg) || return Vararg
        VaT = _limit_type_size(unwrapva(t), unwrapva(c), sources, depth + 1, 0)
        if isdefined(t, :N) && (isa(t.N, TypeVar) || (isdefined(c, :N) && t.N === c.N))
            return Vararg{VaT, t.N}
        end
        return Vararg{VaT}
    elseif isa(t, DataType)
        if isa(c, DataType)
            tP = t.parameters
            cP = c.parameters
            if t.name === c.name && !isempty(cP)
                if t.name === Tuple.name
                    # for covariant datatypes (Tuple),
                    # apply type-size limit element-wise
                    ltP = length(tP)
                    lcP = length(cP)
                    np = min(ltP, max(lcP, allowed_tuplelen))
                    Q = Any[ tP[i] for i in 1:np ]
                    if ltP > np
                        # combine tp[np:end] into tP[np] using Vararg
                        Q[np] = tuple_tail_elem(Bottom, Any[ tP[i] for i in np:ltP ])
                    end
                    for i = 1:np
                        # now apply limit element-wise to Q
                        # padding out the comparison as needed to allowed_tuplelen elements
                        if i <= lcP
                            cPi = cP[i]
                        elseif isvarargtype(cP[lcP])
                            cPi = cP[lcP]
                        else
                            cPi = Any
                        end
                        Q[i] = _limit_type_size(Q[i], cPi, sources, depth + 1, 0)
                    end
                    return Tuple{Q...}
                end
            end
        elseif isa(c, Core.TypeofVararg)
            # Tuple{Vararg{T}} --> Tuple{T} is OK
            return _limit_type_size(t, c.T, sources, depth, 0)
        end
        if isType(t) # allow taking typeof as Type{...}, but ensure it doesn't start nesting
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, DataType) && !isType(tt)
                is_derived_type_from_any(tt, sources, depth) && return t
            end
        end
        if allowed_tuplelen < 1 && t.name === Tuple.name
            return Any
        end
        widert = t.name.wrapper
        if !(t <: widert)
            # This can happen when a typevar has bounds too wide for its context, e.g.
            # `Complex{T} where T` is not a subtype of `Complex`. In that case widen even
            # faster to something safe to ensure the result is a supertype of the input.
            return Any
        end
        return widert
    end
    return Any
end

function type_more_complex(@nospecialize(t), @nospecialize(c), sources::SimpleVector, depth::Int, tupledepth::Int, allowed_tuplelen::Int)
    # detect cases where the comparison is trivial
    if t === c
        return false
    elseif t === Union{}
        return false # Bottom is as simple as they come
    elseif isa(t, DataType) && isempty(t.parameters)
        return false # fastpath: unparameterized types are always finite
    elseif tupledepth > 0 && isa(unwrap_unionall(t), DataType) && isa(c, Type) && c !== Union{} && c <: t
        # TODO: need to check that the UnionAll bounds on t are limited enough too
        return false # t is already wider than the comparison in the type lattice
    elseif tupledepth > 0 && is_derived_type_from_any(unwrap_unionall(t), sources, depth)
        return false # t isn't something new
    end
    # peel off wrappers
    if isa(c, UnionAll)
        # allow wrapping type with fewer UnionAlls than comparison if in a covariant context
        if !isa(t, UnionAll) && tupledepth == 0
            return true
        end
        t = unwrap_unionall(t)
        c = unwrap_unionall(c)
    end
    # rules for various comparison types
    if isa(c, TypeVar)
        tupledepth = 1 # allow replacing a TypeVar with a concrete value (since we know the UnionAll must be in covariant position)
        if isa(t, TypeVar)
            return !(t.lb === Union{} || t.lb === c.lb) || # simplify lb towards Union{}
                   type_more_complex(t.ub, c.ub, sources, depth + 1, tupledepth, 0)
        end
        c.lb === Union{} || return true
        return type_more_complex(t, c.ub, sources, depth, tupledepth, 0)
    elseif isa(c, Union)
        if isa(t, Union)
            return type_more_complex(t.a, c.a, sources, depth, tupledepth, allowed_tuplelen) ||
                   type_more_complex(t.b, c.b, sources, depth, tupledepth, allowed_tuplelen)
        end
        return type_more_complex(t, c.a, sources, depth, tupledepth, allowed_tuplelen) &&
               type_more_complex(t, c.b, sources, depth, tupledepth, allowed_tuplelen)
    elseif isa(t, Int) && isa(c, Int)
        return t !== 1 && !(0 <= t < c) # alternatively, could use !(abs(t) <= abs(c) || abs(t) < n) for some n
    end
    # base case for data types
    if isa(t, DataType)
        tP = t.parameters
        if isa(c, DataType) && t.name === c.name
            cP = c.parameters
            length(cP) < length(tP) && return true
            length(cP) > length(tP) && !isvarargtype(tP[end]) && depth == 1 && return false
            ntail = length(cP) - length(tP) # assume parameters were dropped from the tuple head
            # allow creating variation within a nested tuple, but only so deep
            if t.name === Tuple.name && tupledepth > 0
                tupledepth -= 1
            elseif !isvarargtype(t)
                tupledepth = 0
            end
            isgenerator = (t.name.name === :Generator && t.name.module === _topmod(t.name.module))
            for i = 1:length(tP)
                tPi = tP[i]
                cPi = cP[i + ntail]
                if isgenerator
                    let tPi = unwrap_unionall(tPi),
                        cPi = unwrap_unionall(cPi)
                        if isa(tPi, DataType) && isa(cPi, DataType) &&
                                !tPi.abstract && !cPi.abstract &&
                                sym_isless(cPi.name.name, tPi.name.name)
                            # allow collect on (anonymous) Generators to nest, provided that their functions are appropriately ordered
                            # TODO: is there a better way?
                            continue
                        end
                    end
                end
                type_more_complex(tPi, cPi, sources, depth + 1, tupledepth, 0) && return true
            end
            return false
        elseif isvarargtype(c)
            return type_more_complex(t, unwrapva(c), sources, depth, tupledepth, 0)
        end
        if isType(t) # allow taking typeof any source type anywhere as Type{...}, as long as it isn't nesting Type{Type{...}}
            tt = unwrap_unionall(t.parameters[1])
            if isa(tt, DataType) && !isType(tt)
                is_derived_type_from_any(tt, sources, depth) || return true
                return false
            end
        end
    end
    return true
end

union_count_abstract(x::Union) = union_count_abstract(x.a) + union_count_abstract(x.b)
union_count_abstract(@nospecialize(x)) = !isdispatchelem(x)

function issimpleenoughtype(@nospecialize t)
    t = ignorelimited(t)
    return unionlen(t) + union_count_abstract(t) <= MAX_TYPEUNION_LENGTH &&
           unioncomplexity(t) <= MAX_TYPEUNION_COMPLEXITY
end

# pick a wider type that contains both typea and typeb,
# with some limits on how "large" it can get,
# but without losing too much precision in common cases
# and also trying to be mostly associative and commutative
function tmerge(@nospecialize(typea), @nospecialize(typeb))
    typea === Union{} && return typeb
    typeb === Union{} && return typea
    suba = typea ⊑ typeb
    suba && issimpleenoughtype(typeb) && return typeb
    subb = typeb ⊑ typea
    suba && subb && return typea
    subb && issimpleenoughtype(typea) && return typea

    # type-lattice for LimitedAccuracy wrapper
    # the merge create a slightly narrower type than needed, but we can't
    # represent the precise intersection of causes and don't attempt to
    # enumerate some of these cases where we could
    if isa(typea, LimitedAccuracy) && isa(typeb, LimitedAccuracy)
        if typea.causes ⊆ typeb.causes
            causes = typeb.causes
        elseif typeb.causes ⊆ typea.causes
            causes = typea.causes
        else
            causes = union!(copy(typea.causes), typeb.causes)
        end
        return LimitedAccuracy(tmerge(typea.typ, typeb.typ), causes)
    elseif isa(typea, LimitedAccuracy)
        return LimitedAccuracy(tmerge(typea.typ, typeb), typea.causes)
    elseif isa(typeb, LimitedAccuracy)
        return LimitedAccuracy(tmerge(typea, typeb.typ), typeb.causes)
    end
    # type-lattice for MaybeUndef wrapper
    if isa(typea, MaybeUndef) || isa(typeb, MaybeUndef)
        return MaybeUndef(tmerge(
            isa(typea, MaybeUndef) ? typea.typ : typea,
            isa(typeb, MaybeUndef) ? typeb.typ : typeb))
    end
    # type-lattice for Conditional wrapper
    if isa(typea, Conditional) && isa(typeb, Const)
        if typeb.val === true
            typeb = Conditional(typea.var, Any, Union{})
        elseif typeb.val === false
            typeb = Conditional(typea.var, Union{}, Any)
        end
    end
    if isa(typeb, Conditional) && isa(typea, Const)
        if typea.val === true
            typea = Conditional(typeb.var, Any, Union{})
        elseif typea.val === false
            typea = Conditional(typeb.var, Union{}, Any)
        end
    end
    if isa(typea, Conditional) && isa(typeb, Conditional)
        if is_same_conditionals(typea, typeb)
            vtype = tmerge(typea.vtype, typeb.vtype)
            elsetype = tmerge(typea.elsetype, typeb.elsetype)
            if vtype != elsetype
                return Conditional(typea.var, vtype, elsetype)
            end
        end
        val = maybe_extract_const_bool(typea)
        if val isa Bool && val === maybe_extract_const_bool(typeb)
            return Const(val)
        end
        return Bool
    end
    # type-lattice for InterConditional wrapper, InterConditional will never be merged with Conditional
    if isa(typea, InterConditional) && isa(typeb, Const)
        if typeb.val === true
            typeb = InterConditional(typea.slot, Any, Union{})
        elseif typeb.val === false
            typeb = InterConditional(typea.slot, Union{}, Any)
        end
    end
    if isa(typeb, InterConditional) && isa(typea, Const)
        if typea.val === true
            typea = InterConditional(typeb.slot, Any, Union{})
        elseif typea.val === false
            typea = InterConditional(typeb.slot, Union{}, Any)
        end
    end
    if isa(typea, InterConditional) && isa(typeb, InterConditional)
        if is_same_conditionals(typea, typeb)
            vtype = tmerge(typea.vtype, typeb.vtype)
            elsetype = tmerge(typea.elsetype, typeb.elsetype)
            if vtype != elsetype
                return InterConditional(typea.slot, vtype, elsetype)
            end
        end
        val = maybe_extract_const_bool(typea)
        if val isa Bool && val === maybe_extract_const_bool(typeb)
            return Const(val)
        end
        return Bool
    end
    # type-lattice for Const and PartialStruct wrappers
    if (isa(typea, PartialStruct) || isa(typea, Const)) &&
       (isa(typeb, PartialStruct) || isa(typeb, Const)) &&
        widenconst(typea) === widenconst(typeb)

       typea_nfields = nfields_tfunc(typea)
       typeb_nfields = nfields_tfunc(typeb)
       if !isa(typea_nfields, Const) || !isa(typeb_nfields, Const) || typea_nfields.val !== typeb_nfields.val
            return widenconst(typea)
       end

       type_nfields = typea_nfields.val::Int
       fields = Vector{Any}(undef, type_nfields)
       anyconst = false
       for i = 1:type_nfields
            fields[i] = tmerge(getfield_tfunc(typea, Const(i)),
                               getfield_tfunc(typeb, Const(i)))
            anyconst |= has_nontrivial_const_info(fields[i])
       end
       return anyconst ? PartialStruct(widenconst(typea), fields) :
            widenconst(typea)
    end
    if isa(typea, PartialOpaque) && isa(typeb, PartialOpaque) && widenconst(typea) == widenconst(typeb)
        if !(typea.source === typeb.source &&
             typea.isva === typeb.isva &&
             typea.parent === typeb.parent)
            return widenconst(typea)
        end
        return PartialOpaque(typea.typ, tmerge(typea.env, typeb.env),
            typea.isva, typea.parent, typea.source)
    end
    # no special type-inference lattice, join the types
    typea, typeb = widenconst(typea), widenconst(typeb)
    if !isa(typea, Type) || !isa(typeb, Type)
        # XXX: this should never happen
        return Any
    end
    typea == typeb && return typea
    # it's always ok to form a Union of two concrete types
    if (isconcretetype(typea) || isType(typea)) && (isconcretetype(typeb) || isType(typeb))
        return Union{typea, typeb}
    end
    # collect the list of types from past tmerge calls returning Union
    # and then reduce over that list
    types = Any[]
    _uniontypes(typea, types)
    _uniontypes(typeb, types)
    typenames = Vector{Core.TypeName}(undef, length(types))
    for i in 1:length(types)
        # check that we will be able to analyze (and simplify) everything
        # bail if everything isn't a well-formed DataType
        ti = types[i]
        uw = unwrap_unionall(ti)
        (uw isa DataType && ti <: uw.name.wrapper) || return Any
        typenames[i] = uw.name
    end
    u = Union{types...}
    if issimpleenoughtype(u)
        return u
    end
    # see if any of the union elements have the same TypeName
    # in which case, simplify this tmerge by replacing it with
    # the widest possible version of itself (the wrapper)
    for i in 1:length(types)
        ti = types[i]
        for j in (i + 1):length(types)
            if typenames[i] === typenames[j]
                tj = types[j]
                if ti <: tj
                    types[i] = Union{}
                    typenames[i] = Any.name
                    break
                elseif tj <: ti
                    types[j] = Union{}
                    typenames[j] = Any.name
                else
                    if typenames[i] === Tuple.name
                        # try to widen Tuple slower: make a single non-concrete Tuple containing both
                        # converge the Tuple element-wise if they are the same length
                        # see 4ee2b41552a6bc95465c12ca66146d69b354317b, be59686f7613a2ccfd63491c7b354d0b16a95c05,
                        widen = tuplemerge(unwrap_unionall(ti)::DataType, unwrap_unionall(tj)::DataType)
                        widen = rewrap_unionall(rewrap_unionall(widen, ti), tj)
                    else
                        wr = typenames[i].wrapper
                        uw = unwrap_unionall(wr)::DataType
                        ui = unwrap_unionall(ti)::DataType
                        uj = unwrap_unionall(tj)::DataType
                        merged = wr
                        for k = 1:length(uw.parameters)
                            ui_k = ui.parameters[k]
                            if ui_k === uj.parameters[k] && !has_free_typevars(ui_k)
                                merged = merged{ui_k}
                            else
                                merged = merged{uw.parameters[k]}
                            end
                        end
                        widen = rewrap_unionall(merged, wr)
                    end
                    types[i] = Union{}
                    typenames[i] = Any.name
                    types[j] = widen
                    break
                end
            end
        end
    end
    u = Union{types...}
    # don't let type unions get too big, if the above didn't reduce it enough
    if issimpleenoughtype(u)
        return u
    end
    # don't let the slow widening of Tuple cause the whole type to grow too fast
    for i in 1:length(types)
        if typenames[i] === Tuple.name
            widen = unwrap_unionall(types[i])
            if isa(widen, DataType) && !isvatuple(widen)
                widen = NTuple{length(widen.parameters), Any}
            else
                widen = Tuple
            end
            types[i] = widen
            u = Union{types...}
            if issimpleenoughtype(u)
                return u
            end
            break
        end
    end
    # finally, just return the widest possible type
    return Any
end

# the inverse of switchtupleunion, with limits on max element union size
function tuplemerge(a::DataType, b::DataType)
    @assert a.name === b.name === Tuple.name "assertion failure"
    ap, bp = a.parameters, b.parameters
    lar = length(ap)::Int
    lbr = length(bp)::Int
    va = lar > 0 && isvarargtype(ap[lar])
    vb = lbr > 0 && isvarargtype(bp[lbr])
    if lar == lbr && !va && !vb
        lt = lar
        vt = false
    else
        lt = 0 # or min(lar - va, lbr - vb)
        vt = true
    end
    # combine the common elements
    p = Vector{Any}(undef, lt + vt)
    for i = 1:lt
        ui = Union{ap[i], bp[i]}
        p[i] = issimpleenoughtype(ui) ? ui : Any
    end
    # merge the remaining tail into a single, simple Tuple{Vararg{T}} (#22120)
    if vt
        tail = Union{}
        for loop_b = (false, true)
            for i = (lt + 1):(loop_b ? lbr : lar)
                ti = unwrapva(loop_b ? bp[i] : ap[i])
                while ti isa TypeVar
                    ti = ti.ub
                end
                # compare (ti <-> tail), (wrapper ti <-> tail), (ti <-> wrapper tail), then (wrapper ti <-> wrapper tail)
                # until we find the first element that contains the other in the pair
                # TODO: this result would be more stable (and more associative and more commutative)
                #   if we either joined all of the element wrappers first into a wide-tail, then picked between that or an exact tail,
                #   or (equivalently?) iteratively took super-types until reaching a common wrapper
                #   e.g. consider the results of `tuplemerge(Tuple{Complex}, Tuple{Number, Int})` and of
                #   `tuplemerge(Tuple{Int}, Tuple{String}, Tuple{Int, String})`
                if !(ti <: tail)
                    if tail <: ti
                        tail = ti # widen to ti
                    else
                        uw = unwrap_unionall(tail)
                        if uw isa DataType && tail <: uw.name.wrapper
                            # widen tail to wrapper(tail)
                            tail = uw.name.wrapper
                            if !(ti <: tail)
                                #assert !(tail <: ti)
                                uw = unwrap_unionall(ti)
                                if uw isa DataType && ti <: uw.name.wrapper
                                    # widen ti to wrapper(ti)
                                    ti = uw.name.wrapper
                                    #assert !(ti <: tail)
                                    if tail <: ti
                                        tail = ti
                                    else
                                        tail = Any # couldn't find common super-type
                                    end
                                else
                                    tail = Any # couldn't analyze type
                                end
                            end
                        else
                            tail = Any # couldn't analyze type
                        end
                    end
                end
                tail === Any && return Tuple # short-circuit loop
            end
        end
        @assert !(tail === Union{})
        p[lt + 1] = Vararg{tail}
    end
    return Tuple{p...}
end

# compute typeintersect over the extended inference lattice
# where v is in the extended lattice, and t is a Type
function tmeet(@nospecialize(v), @nospecialize(t))
    if isa(v, Const)
        if !has_free_typevars(t) && !isa(v.val, t)
            return Bottom
        end
        return v
    elseif isa(v, PartialStruct)
        has_free_typevars(t) && return v
        widev = widenconst(v)
        if widev <: t
            return v
        end
        ti = typeintersect(widev, t)
        if ti === Bottom
            return Bottom
        end
        @assert widev <: Tuple
        new_fields = Vector{Any}(undef, length(v.fields))
        for i = 1:length(new_fields)
            if isa(v.fields[i], Core.TypeofVararg)
                new_fields[i] = v.fields[i]
            else
                new_fields[i] = tmeet(v.fields[i], widenconst(getfield_tfunc(t, Const(i))))
                if new_fields[i] === Bottom
                    return Bottom
                end
            end
        end
        return tuple_tfunc(new_fields)
    elseif isa(v, Conditional)
        if !(Bool <: t)
            return Bottom
        end
        return v
    end
    return typeintersect(widenconst(v), t)
end
