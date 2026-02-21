# Standalone copy of all Base methods reachable from `@show typeof(obj)`,
# with every custom function renamed using a `Base_` prefix to avoid
# colliding with Base's own definitions.
#
# Infrastructure primitives (print, println, write, IOContext, IOBuffer,
# sprint, string, nameof, parentmodule, etc.) are used directly from Base.

using Base: unwrap_unionall, rewrap_unionall, has_free_typevars, has_typevar,
            isdeprecated, isconst, isdefinedglobal, unsorted_names, getglobal,
            uniontypes, unionlen, inferencebarrier, is_valid_identifier,
            isidentifier, _isoperator, is_id_start_char, escape_raw_string,
            isvarargtype, isvatuple, unwrapva, is_root_module, IdSet,
            tls_world_age, lookup_binding_partition, binding_kind,
            is_some_explicit_imported, partition_restriction,
            is_defined_const_binding, ImmutableDict, SimpleVector,
            nonmissingtype, nonnothingtype, printstyled, takestring!

# ── constants ────────────────────────────────────────────────────────────
const Base_undef_ref_str = "#undef"
const Base_quoted_syms = Set{Symbol}([:(:), :(::), :(:=), :(=), :(==), :(===), :(=>)])

# ── show_circular ────────────────────────────────────────────────────────
Base_show_circular(io::IO, @nospecialize(x)) = false
function Base_show_circular(io::IOContext, @nospecialize(x))
    d = 1
    for (k, v) in io.dict
        if k === :SHOWN_SET
            if v === x
                printstyled(io, "#= circular reference @-$d =#"; color = :yellow)
                return true
            end
            d += 1
        end
    end
    return false
end

# ── show(io, ::Core.TypeofBottom) ────────────────────────────────────────
Base_show(io::IO, ::Core.TypeofBottom) = print(io, "Union{}")

# ── print_without_params ────────────────────────────────────────────────
function Base_print_without_params(@nospecialize(x))
    b = unwrap_unionall(x)
    return isa(b, DataType) && b.name.wrapper === x
end

# ── io_has_tvar_name ────────────────────────────────────────────────────
function Base_io_has_tvar_name(io::IOContext, name::Symbol, @nospecialize(x))
    for (key, val) in io.dict
        if key === :unionall_env && val isa TypeVar && val.name === name && has_typevar(x, val)
            return true
        end
    end
    return false
end
Base_io_has_tvar_name(io::IO, name::Symbol, @nospecialize(x)) = false

# ── modulesof! ──────────────────────────────────────────────────────────
Base_modulesof!(s::Set{Module}, x::TypeVar) = Base_modulesof!(s, x.ub)
function Base_modulesof!(s::Set{Module}, @nospecialize(x::Type))
    x = unwrap_unionall(x)
    if x isa DataType
        push!(s, parentmodule(x))
    elseif x isa Union
        Base_modulesof!(s, x.a)
        Base_modulesof!(s, x.b)
    end
    s
end

# ── makeproper ──────────────────────────────────────────────────────────
function Base_makeproper(io::IO, @nospecialize(x::Type))
    if io isa IOContext
        for (key, val) in io.dict
            if key === :unionall_env && val isa TypeVar
                x = UnionAll(val, x)
            end
        end
    end
    has_free_typevars(x) && return Any
    return x
end

# ── alias candidate cache ──────────────────────────────────────────────
# Iterating unsorted_names(mod) + isdefinedglobal / isconst / getglobal
# for every call to Base_make_typealias is O(|module|) and dominates
# the cost for large modules like Base (~130 μs for 1215 names).
# Cache the small set of candidates (~30 for Base) on first access.
const _typealias_candidates = Dict{Module, Vector{Tuple{Symbol, Any}}}()

function _get_typealias_candidates(mod::Module)
    return get!(_typealias_candidates, mod) do
        candidates = Tuple{Symbol, Any}[]
        for name in unsorted_names(mod)
            if isdefinedglobal(mod, name) && isconst(mod, name)
                alias = getglobal(mod, name)
                if alias isa Type && !has_free_typevars(alias) && !Base_print_without_params(alias)
                    push!(candidates, (name, alias))
                end
            end
        end
        candidates
    end
end

# ── make_typealias ──────────────────────────────────────────────────────
function Base_make_typealias(@nospecialize(x::Type))
    Any === x && return nothing
    x <: Tuple && return nothing
    mods = Base_modulesof!(Set{Module}(), x)
    replace!(mods, Core => Base)
    aliases = Tuple{GlobalRef,SimpleVector}[]
    xenv = UnionAll[]
    for p in uniontypes(unwrap_unionall(x))
        p isa UnionAll && push!(xenv, p)
    end
    x isa UnionAll && push!(xenv, x)
    # For a DataType, `applied === x` can only hold when the alias
    # shares the same TypeName, so we can skip the expensive `x <: alias`
    # subtyping check for every unrelated type constant in the module.
    ux = unwrap_unionall(x)
    x_tn = ux isa DataType ? ux.name : nothing
    for mod in mods
        for (name, alias) in _get_typealias_candidates(mod)
            if !isdeprecated(mod, name)
                if x_tn !== nothing
                    ua = unwrap_unionall(alias)
                    (ua isa DataType && ua.name === x_tn) || continue
                end
                x <: alias || continue
                if alias isa UnionAll
                    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), x, alias)::SimpleVector
                    env = env::SimpleVector
                    applied = try
                            alias{env...}
                        catch ex
                            ex isa TypeError || rethrow()
                            continue
                        end
                    for p in xenv
                        applied = rewrap_unionall(applied, p)
                    end
                    has_free_typevars(applied) && continue
                    applied === x || continue
                elseif alias === x
                    env = Core.svec()
                else
                    continue
                end
                push!(aliases, (GlobalRef(mod, name), env))
            end
        end
    end
    if length(aliases) == 1
        return aliases[1]
    end
end

# ── isgensym ────────────────────────────────────────────────────────────
Base_isgensym(s::Symbol) = '#' in string(s)

# ── show_can_elide ──────────────────────────────────────────────────────
function Base_show_can_elide(p::TypeVar, wheres::Vector, elide::Int, env::SimpleVector, skip::Int)
    elide == 0 && return false
    wheres[elide] === p || return false
    for i = (elide + 1):length(wheres)
        v = wheres[i]::TypeVar
        has_typevar(v.lb, p) && return false
        has_typevar(v.ub, p) && return false
    end
    for i = eachindex(env)
        i == skip && continue
        has_typevar(env[i], p) && return false
    end
    return true
end

# ── show_typeparams ─────────────────────────────────────────────────────
function Base_show_typeparams(io::IO, env::SimpleVector, orig::SimpleVector, wheres::Vector)
    n = length(env)
    elide = length(wheres)
    function egal_var(p::TypeVar, @nospecialize o)
        return o isa TypeVar &&
            ccall(:jl_types_egal, Cint, (Any, Any), p.ub, o.ub) != 0 &&
            ccall(:jl_types_egal, Cint, (Any, Any), p.lb, o.lb) != 0
    end
    for i = n:-1:1
        p = env[i]
        if p isa TypeVar
            if i == n && egal_var(p, orig[i]) && Base_show_can_elide(p, wheres, elide, env, i)
                n -= 1
                elide -= 1
            elseif p.lb === Union{} && Base_isgensym(p.name) && Base_show_can_elide(p, wheres, elide, env, i)
                elide -= 1
            elseif p.ub === Any && Base_isgensym(p.name) && Base_show_can_elide(p, wheres, elide, env, i)
                elide -= 1
            end
        end
    end
    if n > 0
        print(io, "{")
        for i = 1:n
            p = env[i]
            if p isa TypeVar
                if p.lb === Union{} && something(findfirst(@nospecialize(w) -> w === p, wheres), 0) > elide
                    print(io, "<:")
                    Base_show(io, p.ub)
                elseif p.ub === Any && something(findfirst(@nospecialize(w) -> w === p, wheres), 0) > elide
                    print(io, ">:")
                    Base_show(io, p.lb)
                else
                    Base_show(io, p)
                end
            else
                Base_show(io, p)
            end
            i < n && print(io, ", ")
        end
        print(io, "}")
    end
    resize!(wheres, elide)
    nothing
end

# ── isvisible ───────────────────────────────────────────────────────────
function Base_isvisible(sym::Symbol, parent::Module, from::Module)
    isdeprecated(parent, sym) && return false
    isdefinedglobal(from, sym) || return false
    isdefinedglobal(parent, sym) || return false
    parent_binding = convert(Core.Binding, GlobalRef(parent, sym))
    from_binding = convert(Core.Binding, GlobalRef(from, sym))
    while true
        from_binding === parent_binding && return true
        partition = lookup_binding_partition(tls_world_age(), from_binding)
        is_some_explicit_imported(binding_kind(partition)) || break
        from_binding = partition_restriction(partition)::Core.Binding
    end
    parent_partition = lookup_binding_partition(tls_world_age(), parent_binding)
    from_partition = lookup_binding_partition(tls_world_age(), from_binding)
    if is_defined_const_binding(binding_kind(parent_partition)) && is_defined_const_binding(binding_kind(from_partition))
        return parent_partition.restriction === from_partition.restriction
    end
    return false
end

# ── show_typealias (4-arg) ──────────────────────────────────────────────
function Base_show_typealias(io::IO, name::GlobalRef, @nospecialize(x::Type), env::SimpleVector, wheres::Vector)
    if !(get(io, :compact, false)::Bool)
        from = get(io, :module, Main)
        if (from === nothing || !Base_isvisible(name.name, name.mod, from))
            Base_show(io, name.mod)
            print(io, ".")
        end
    end
    print(io, name.name)
    isempty(env) && return
    io = IOContext(io)
    for p in wheres
        io = IOContext(io, :unionall_env => p)
    end
    orig = getfield(name.mod, name.name)
    vars = TypeVar[]
    while orig isa UnionAll
        push!(vars, orig.var)
        orig = orig.body
    end
    Base_show_typeparams(io, env, Core.svec(vars...), wheres)
    nothing
end

# ── make_wheres ─────────────────────────────────────────────────────────
function Base_make_wheres(io::IO, env::SimpleVector, @nospecialize(x::Type))
    seen = IdSet()
    wheres = TypeVar[]
    if io isa IOContext
        for (key, val) in io.dict
            if key === :unionall_env && val isa TypeVar && has_typevar(x, val)
                push!(seen, val)
            end
        end
    end
    while x isa UnionAll
        if !(x.var in seen)
            push!(seen, x.var)
            push!(wheres, x.var)
        end
        x = x.body
    end
    for i = length(env):-1:1
        p = env[i]
        if p isa TypeVar && !(p in seen)
            push!(seen, p)
            pushfirst!(wheres, p)
        end
    end
    return wheres
end

# ── show_wheres ─────────────────────────────────────────────────────────
function Base_show_wheres(io::IO, wheres::Vector{TypeVar})
    isempty(wheres) && return
    io = IOContext(io)
    n = length(wheres)
    for i = 1:n
        p = wheres[i]
        print(io, n == 1 ? " where " : i == 1 ? " where {" : ", ")
        Base_show(io, p)
        io = IOContext(io, :unionall_env => p)
    end
    n > 1 && print(io, "}")
    nothing
end

# ── show_typealias (1-arg) ──────────────────────────────────────────────
function Base_show_typealias(io::IO, @nospecialize(x::Type))
    properx = Base_makeproper(io, x)
    alias = Base_make_typealias(properx)
    alias === nothing && return false
    wheres = Base_make_wheres(io, alias[2], x)
    Base_show_typealias(io, alias[1], x, alias[2], wheres)
    Base_show_wheres(io, wheres)
    return true
end

# ── make_typealiases ────────────────────────────────────────────────────
function Base_make_typealiases(@nospecialize(x::Type))
    aliases = SimpleVector[]
    Any === x && return aliases, Union{}
    x <: Tuple && return aliases, Union{}
    mods = Base_modulesof!(Set{Module}(), x)
    replace!(mods, Core => Base)
    vars = Dict{Symbol,TypeVar}()
    xenv = UnionAll[]
    each = Any[]
    for p in uniontypes(unwrap_unionall(x))
        p isa UnionAll && push!(xenv, p)
        push!(each, rewrap_unionall(p, x))
    end
    x isa UnionAll && push!(xenv, x)
    for mod in mods
        for name in unsorted_names(mod)
            if isdefinedglobal(mod, name) && !isdeprecated(mod, name) && isconst(mod, name)
                alias = getglobal(mod, name)
                if alias isa Type && !has_free_typevars(alias) && !Base_print_without_params(alias) && !(alias <: Tuple)
                    (ti, env) = ccall(:jl_type_intersection_with_env, Any, (Any, Any), x, alias)::SimpleVector
                    ti === Union{} && continue
                    mod2 = Base_modulesof!(Set{Module}(), alias)
                    mod in mod2 || (mod === Base && Core in mod2) || continue
                    env = env::SimpleVector
                    applied = alias
                    if !isempty(env)
                        applied = try
                                alias{env...}
                            catch ex
                                ex isa TypeError || rethrow()
                                continue
                            end
                    end
                    ul = unionlen(applied)
                    for p in xenv
                        applied = rewrap_unionall(applied, p)
                    end
                    has_free_typevars(applied) && continue
                    applied <: x || continue
                    Base_print_without_params(x) && (env = Core.svec())
                    for typ in each
                        if typ <: applied
                            push!(aliases, Core.svec(GlobalRef(mod, name), env, applied, (ul, -length(env))))
                            break
                        end
                    end
                end
            end
        end
    end
    if isempty(aliases)
        return aliases, Union{}
    end
    sort!(aliases, by = x -> x[4]::Tuple{Int,Int}, rev = true)
    let applied = Union{}
        applied1 = Union{}
        keep = SimpleVector[]
        prev = (0, 0)
        for alias in aliases
            alias4 = alias[4]::Tuple{Int,Int}
            if alias4[1] < 2
                if !(alias[3] <: applied)
                    applied1 = Union{applied1, alias[3]}
                    push!(keep, alias)
                end
            elseif alias4 == prev || !(alias[3] <: applied)
                applied = applied1 = Union{applied1, alias[3]}
                push!(keep, alias)
                prev = alias4
            end
        end
        return keep, applied1
    end
end

# ── show_unionaliases ───────────────────────────────────────────────────
function Base_show_unionaliases(io::IO, x::Union)
    properx = Base_makeproper(io, x)
    aliases, applied = Base_make_typealiases(properx)
    isempty(aliases) && return false
    first = true
    tvar = false
    for typ in uniontypes(x)
        if isa(typ, TypeVar)
            tvar = true
            continue
        elseif rewrap_unionall(typ, properx) <: applied
            continue
        end
        print(io, first ? "Union{" : ", ")
        first = false
        Base_show(io, typ)
    end
    if first && !tvar && length(aliases) == 1
        alias = aliases[1]
        env = alias[2]::SimpleVector
        wheres = Base_make_wheres(io, env, x)
        Base_show_typealias(io, alias[1], x, env, wheres)
        Base_show_wheres(io, wheres)
    else
        for alias in aliases
            print(io, first ? "Union{" : ", ")
            first = false
            env = alias[2]::SimpleVector
            wheres = Base_make_wheres(io, env, x)
            Base_show_typealias(io, alias[1], x, env, wheres)
            Base_show_wheres(io, wheres)
        end
        if tvar
            for typ in uniontypes(x)
                if isa(typ, TypeVar)
                    print(io, ", ")
                    Base_show(io, typ)
                end
            end
        end
        print(io, "}")
    end
    return true
end

# ── is_global_function ──────────────────────────────────────────────────
function Base_is_global_function(tn::Core.TypeName, globname::Union{Symbol,Nothing})
    if globname !== nothing && isconcretetype(tn.wrapper) && tn !== DataType.name
        globname_str = string(globname::Symbol)
        if '#' ∉ globname_str && '@' ∉ globname_str && isdefined(tn, :module) &&
                isdefinedglobal(tn.module, globname) && isconst(tn.module, globname) &&
                isa(getglobal(tn.module, globname), tn.wrapper)
            return true
        end
    end
    return false
end

# ── check_world_bounded ─────────────────────────────────────────────────
function Base_check_world_bounded(tn::Core.TypeName)
    bnd = ccall(:jl_get_module_binding, Ref{Core.Binding}, (Any, Any, Cint), tn.module, tn.name, true)
    isdefined(bnd, :partitions) || return nothing
    partition = @atomic bnd.partitions
    while true
        if is_defined_const_binding(binding_kind(partition))
            cval = partition_restriction(partition)
            if isa(cval, Type) && cval <: tn.wrapper
                max_world = @atomic partition.max_world
                max_world == typemax(UInt) && return nothing
                return Int(partition.min_world):Int(max_world)
            end
        end
        isdefined(partition, :next) || return nothing
        partition = @atomic partition.next
    end
end

# ── show_sym ────────────────────────────────────────────────────────────
function Base_show_sym(io::IO, sym::Symbol; allow_macroname=false)
    if is_valid_identifier(sym)
        print(io, sym)
    elseif allow_macroname && (sym_str = string(sym); startswith(sym_str, '@'))
        print(io, '@')
        Base_show_sym(io, Symbol(sym_str[2:end]))
    else
        print(io, "var\"", escape_raw_string(string(sym)), '"')
    end
end

# ── show_type_name ──────────────────────────────────────────────────────
function Base_show_type_name(io::IO, tn::Core.TypeName)
    if tn === UnionAll.name
        return print(io, "UnionAll")
    end
    globname = tn.singletonname
    globfunc = Base_is_global_function(tn, globname)
    sym = (globfunc ? globname : tn.name)::Symbol
    globfunc && print(io, "typeof(")
    quo = false
    world = Base_check_world_bounded(tn)
    world !== nothing && print(io, "@world(")
    if !(get(io, :compact, false)::Bool)
        from = get(io, :module, Main)
        if isdefined(tn, :module) && (from === nothing || !Base_isvisible(sym, tn.module, from::Module))
            Base_show(io, tn.module)
            print(io, ".")
            if globfunc && !is_id_start_char(first(string(sym)))
                print(io, ':')
                if sym in Base_quoted_syms
                    print(io, '(')
                    quo = true
                end
            end
        end
    end
    Base_show_sym(io, sym)
    world !== nothing && print(io, ", ", world, ")")
    quo      && print(io, ")")
    globfunc && print(io, ")")
    nothing
end

# ── maybe_kws_nt ────────────────────────────────────────────────────────
function Base_maybe_kws_nt(x::DataType)
    x.name === Base.typename(Base.Pairs) || return nothing
    length(x.parameters) == 4 || return nothing
    x.parameters[1] === Symbol || return nothing
    p4 = x.parameters[4]
    if (isa(p4, DataType) && p4.name === Base.typename(NamedTuple) && length(p4.parameters) == 2)
        syms, types = p4.parameters
        types isa DataType || return nothing
        x.parameters[2] === eltype(p4) || return nothing
        isa(syms, Tuple) || return nothing
        x.parameters[3] === Nothing || return nothing
        return p4
    end
    return nothing
end

# ── show_at_namedtuple ──────────────────────────────────────────────────
function Base_show_at_namedtuple(io::IO, syms::Tuple, types::DataType)
    first = true
    for i in eachindex(syms)
        if !first
            print(io, ", ")
        end
        Base_show_sym(io, syms[i])
        typ = types.parameters[i]
        if typ !== Any
            print(io, "::")
            Base_show(io, typ)
        end
        first = false
    end
end

# ── show_datatype ───────────────────────────────────────────────────────
function Base_show_datatype(io::IO, x::DataType, wheres::Vector{TypeVar}=TypeVar[])
    parameters = x.parameters::SimpleVector
    istuple = x.name === Tuple.name
    isnamedtuple = x.name === Base.typename(NamedTuple)
    kwsnt = Base_maybe_kws_nt(x)
    n = length(parameters)

    if istuple
        if n == 0
            print(io, "Tuple{}")
            return
        end
        max_n = 3
        taillen = 1
        pn = parameters[n]
        fulln = n
        vakind = :none
        vaN = 0
        if pn isa Core.TypeofVararg
            if isdefined(pn, :N)
                vaN = pn.N
                if vaN isa Int
                    taillen = vaN
                    fulln += taillen - 1
                    vakind = :fixed
                else
                    vakind = :bound
                end
            else
                vakind = :unbound
            end
            pn = unwrapva(pn)
        end
        if !(pn isa TypeVar || pn isa Type)
            taillen = 0
        elseif vakind === :none || vakind === :fixed
            for i in (n-1):-1:1
                if parameters[i] === pn
                    taillen += 1
                else
                    break
                end
            end
        end
        if (vakind == :bound && n == 1 == taillen) || (vakind === :fixed && taillen == fulln > max_n) ||
           (vakind === :none && taillen == fulln > max_n)
            print(io, "NTuple{")
            vakind === :bound ? Base_show(io, vaN) : print(io, fulln)
            print(io, ", ")
            Base_show(io, pn)
            print(io, "}")
        else
            print(io, "Tuple{")
            headlen = (taillen > max_n ? fulln - taillen : fulln)
            for i = 1:headlen
                i > 1 && print(io, ", ")
                Base_show(io, vakind === :fixed && i >= n ? pn : parameters[i])
            end
            if headlen < fulln
                headlen > 0 && print(io, ", ")
                print(io, "Vararg{")
                Base_show(io, pn)
                print(io, ", ", fulln - headlen, "}")
            end
            print(io, "}")
        end
        return
    elseif isnamedtuple
        syms, types = parameters
        if syms isa Tuple && types isa DataType && length(types.parameters) == length(syms) && !isvatuple(types)
            print(io, "@NamedTuple{")
            Base_show_at_namedtuple(io, syms, types)
            print(io, "}")
            return
        end
    elseif get(io, :backtrace, false)::Bool && kwsnt !== nothing
        print(io, "@Kwargs{")
        Base_show_at_namedtuple(io, kwsnt.parameters[1]::Tuple, kwsnt.parameters[2]::DataType)
        print(io, "}")
        return
    end

    Base_show_type_name(io, x.name)
    Base_show_typeparams(io, parameters, (unwrap_unionall(x.name.wrapper)::DataType).parameters, wheres)
end

# ── show_delim_array (AbstractArray/SimpleVector) ───────────────────────
function Base_show_delim_array(io::IO, itr::Union{AbstractArray,SimpleVector}, op, delim, cl,
                          delim_one, i1=first(LinearIndices(itr)), l=last(LinearIndices(itr)))
    print(io, op)
    if !Base_show_circular(io, itr)
        recur_io = IOContext(io, :SHOWN_SET => itr)
        first = true
        i = i1
        if l >= i1
            while true
                if !isassigned(itr, i)
                    print(io, Base_undef_ref_str)
                else
                    x = itr[i]
                    Base_show(recur_io, x)
                end
                if i == l
                    delim_one && first && print(io, delim)
                    break
                end
                i += 1
                first = false
                print(io, delim)
                print(io, ' ')
            end
        end
    end
    print(io, cl)
end

# ── show_delim_array (generic iterable) ─────────────────────────────────
function Base_show_delim_array(io::IO, itr, op, delim, cl, delim_one, i1=1, n=typemax(Int))
    print(io, op)
    if !Base_show_circular(io, itr)
        recur_io = IOContext(io, :SHOWN_SET => itr)
        y = iterate(itr)
        first = true
        i0 = i1-1
        while i1 > 1 && y !== nothing
            y = iterate(itr, y[2])
            i1 -= 1
        end
        if y !== nothing
            typeinfo = get(io, :typeinfo, Any)
            while true
                x = y[1]
                y = iterate(itr, y[2])
                Base_show(IOContext(recur_io, :typeinfo => itr isa typeinfo <: Tuple ?
                                             fieldtype(typeinfo, i1+i0) :
                                             typeinfo),
                     x)
                i1 += 1
                if y === nothing || i1 > n
                    delim_one && first && print(io, delim)
                    break
                end
                first = false
                print(io, delim)
                print(io, ' ')
            end
        end
    end
    print(io, cl)
end

# ── show_unquoted (minimal needed for TypeVar name display) ─────────────
Base_show_unquoted(io::IO, ex)              = Base_show_unquoted(io, ex, 0, 0)
Base_show_unquoted(io::IO, ex, indent::Int) = Base_show_unquoted(io, ex, indent, 0)
Base_show_unquoted(io::IO, ex, ::Int, ::Int) = Base_show(io, ex)
Base_show_unquoted(io::IO, sym::Symbol, ::Int, ::Int) = Base_show_sym(io, sym, allow_macroname=false)

# ── show(io, Module) ────────────────────────────────────────────────────
function Base_show(io::IO, m::Module)
    if is_root_module(m)
        print(io, nameof(m))
    else
        Base_print_fullname(io, m)
    end
end

function Base_print_fullname(io::IO, m::Module)
    mp = parentmodule(m)
    if m === Main || m === Base || m === Core || mp === m
        Base_show_sym(io, nameof(m))
    else
        Base_print_fullname(io, mp)
        print(io, '.')
        Base_show_sym(io, nameof(m))
    end
end

# ── show(io, TypeName) ──────────────────────────────────────────────────
function Base_show(io::IO, tn::Core.TypeName)
    print(io, "typename(")
    Base_show_type_name(io, tn)
    print(io, ")")
end

# ── show(io, TypeVar) ───────────────────────────────────────────────────
function Base_show(io::IO, tv::TypeVar)
    in_env = (:unionall_env => tv) in io
    function show_bound(io::IO, @nospecialize(b))
        parens = isa(b, UnionAll) && !Base_print_without_params(b)
        parens && print(io, "(")
        Base_show(io, b)
        parens && print(io, ")")
    end
    lb, ub = tv.lb, tv.ub
    if !in_env && lb !== Union{}
        if ub === Any
            Base_show_unquoted(io, tv.name)
            print(io, ">:")
            show_bound(io, lb)
        else
            show_bound(io, lb)
            print(io, "<:")
            Base_show_unquoted(io, tv.name)
        end
    else
        Base_show_unquoted(io, tv.name)
    end
    if !in_env && ub !== Any
        print(io, "<:")
        show_bound(io, ub)
    end
    nothing
end

# ── show(io, Vararg) ────────────────────────────────────────────────────
function Base_show(io::IO, vm::Core.TypeofVararg)
    print(io, "Vararg")
    if isdefined(vm, :T)
        print(io, "{")
        Base_show(io, vm.T)
        if isdefined(vm, :N)
            print(io, ", ")
            Base_show(io, vm.N)
        end
        print(io, "}")
    end
end

# ── type node budget (prevents exponential blowup on nested types) ──────
#
# The budget limits the total number of type nodes expanded during
# printing.  This is an **opt-in** mechanism: callers set the IOContext
# property `:type_budget => Ref{Int}` (a countdown) to activate it.
# When this property is absent, the budget check is a no-op and types
# are printed in full (preserving default behavior of `repr`, `print`,
# `string`, etc.).
#
# To opt in, callers create an IOContext before calling `show`:
#
#     io = IOContext(io, :type_budget => Ref(80))
#     show(io, some_type)
#
# The budget system is complementary to `type_depth_limit`:
#   - Budget: limits work *during* printing (performance guard against
#     exponential blowup on deeply nested types)
#   - `type_depth_limit`: post-processing that truncates an already-
#     generated string to fit the display width (cosmetic)

"""
    _base_type_budget_exceeded!(io::IO) -> Bool

Check whether the type node budget has been exceeded.

Returns `false` immediately if `:type_budget` is not present in `io`
(i.e., the caller did not opt in to budget-limited printing).

When the budget is active, decrements the countdown and returns `true`
once it drops below zero, signalling the caller to print `…` instead of
expanding the type further.
"""
function _base_type_budget_exceeded!(io::IO)
    budget = get(io, :type_budget, nothing)
    budget === nothing && return false
    budget::Base.RefValue{Int}
    budget[] -= 1
    return budget[] < 0
end

# ── show(io, Type) / _show_type ─────────────────────────────────────────
Base_show(io::IO, @nospecialize(x::Type)) = Base_show_type(io, inferencebarrier(x))
function Base_show_type(io::IO, @nospecialize(x::Type))
    if _base_type_budget_exceeded!(io)
        print(io, "…")
        return
    end
    if Base_print_without_params(x)
        Base_show_type_name(io, (unwrap_unionall(x)::DataType).name)
        return
    elseif get(io, :compact, true)::Bool && Base_show_typealias(io, x)
        return
    elseif x isa DataType
        Base_show_datatype(io, x)
        return
    elseif x isa Union
        if get(io, :compact, true)::Bool && Base_show_unionaliases(io, x)
            return
        end
        print(io, "Union")
        Base_show_delim_array(io, uniontypes(x), '{', ',', '}', false)
        return
    end

    x = x::UnionAll
    wheres = TypeVar[]
    let io = IOContext(io)
        while x isa UnionAll
            var = x.var
            if var.name === :_ || Base_io_has_tvar_name(io, var.name, x)
                counter = 1
                while true
                    newname = Symbol(var.name, counter)
                    if !Base_io_has_tvar_name(io, newname, x)
                        var = TypeVar(newname, var.lb, var.ub)
                        x = x{var}
                        break
                    end
                    counter += 1
                end
            else
                x = x.body
            end
            push!(wheres, var)
            io = IOContext(io, :unionall_env => var)
        end
        if x isa DataType
            Base_show_datatype(io, x, wheres)
        else
            Base_show(io, x)
        end
    end
    Base_show_wheres(io, wheres)
end

# ── repr ────────────────────────────────────────────────────────────────
"""
    Base_repr(x; context=nothing)

Return a string representation of `x` using `Base_show`.

By default, types are printed in full without any truncation.  To enable
budget-limited type printing, pass an `IOContext` with the `:type_budget`
property via `context`:

```julia
context = IOContext(stdout, :type_budget => Ref(80))
Base_repr(some_type; context=context)
```

# IOContext property for type node budgeting

 - `:type_budget :: Ref{Int}` — Countdown of remaining type nodes to
   expand.  Initialize to `Ref(n)` where `n` is the maximum number of
   nodes.  Each node visited decrements the countdown; when it reaches
   zero, remaining nodes are replaced with `…`.
"""
function Base_repr(x; context=nothing)
    sprint(Base_show, x; context=context)
end

# ── sprint (re-export from Base for convenience) ────────────────────────
const Base_sprint = sprint

# ── println ─────────────────────────────────────────────────────────────
Base_println(io::IO, xs...) = print(io, xs..., "\n")

# ── @show macro ─────────────────────────────────────────────────────────
macro Base_show(exs...)
    blk = Expr(:block)
    for ex in exs
        push!(blk.args, :(println($(sprint(Base.show_unquoted, ex) * " = "),
                                  Base_repr(begin local value = $(esc(ex)) end))))
    end
    isempty(exs) || push!(blk.args, :value)
    return blk
end

# ── ANSIIterator / ANSIDelimiter (needed by type_depth_limit) ────────────
const Base_ansi_regex = r"(?s)(?:\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~]))|."

struct Base_ANSIDelimiter
    del::SubString{String}
end
Base.ncodeunits(c::Base_ANSIDelimiter) = ncodeunits(c.del)
Base.textwidth(::Base_ANSIDelimiter) = 0

struct Base_ANSIIterator{S}
    captures::Base.RegexMatchIterator{S}
end
Base_ANSIIterator(s::AbstractString) = Base_ANSIIterator(eachmatch(Base_ansi_regex, s))

Base.IteratorSize(::Type{<:Base_ANSIIterator}) = Base.SizeUnknown()
Base.eltype(::Type{<:Base_ANSIIterator}) = Pair{Int, Union{Char, Base_ANSIDelimiter}}
function Base.iterate(I::Base_ANSIIterator, (i, m_st)=(1, iterate(I.captures)))
    m_st === nothing && return nothing
    m, (j, new_m_st) = m_st
    c = lastindex(m.match) == 1 ? only(m.match) : Base_ANSIDelimiter(m.match)
    return (i => c, (j, iterate(I.captures, (j, new_m_st))))
end

"""
    Base_type_depth_limit(str::String, n::Int; maxdepth=nothing) -> String

Limit the nesting depth of `{ }` brackets in `str` until the string's
`textwidth` is at most `n`, replacing elided content with `…`.

If `maxdepth` is given, truncate at exactly that depth regardless of width.

This is a **post-processing** step that operates on an already-generated
string.  It is complementary to the type node budget system
(`_base_type_budget_exceeded!`), which limits work *during* printing to
prevent exponential blowup.  `type_depth_limit` handles width-fitting
with proper ANSI escape code and quoted-string awareness.

# Examples
```
julia> str = repr(typeof(view([1, 2, 3], 1:2)))
"SubArray{Int64, 1, Vector{Int64}, Tuple{UnitRange{Int64}}, true}"

julia> Base.type_depth_limit(str, 0, maxdepth = 1)
"SubArray{…}"

julia> Base.type_depth_limit(str, 45)
"SubArray{Int64, 1, Vector{…}, Tuple{…}, true}"
```
"""
function Base_type_depth_limit(str::String, n::Int; maxdepth = nothing)
    depth = 0
    width_at = Int[]
    depths = zeros(Int16, lastindex(str))
    levelcount = Int[]
    strwid = 0
    st_0, st_backslash, st_squote, st_dquote = 0, 1, 2, 4
    state = Ref(st_0)
    stateis(s) = (state[] & s) != 0
    quoted() = stateis(st_squote) || stateis(st_dquote)
    enter(s) = (state[] |= s)
    leave(s) = (state[] &= ~s)
    for (i, c) in Base_ANSIIterator(str)
        if c isa Base_ANSIDelimiter
            depths[i] = depth
            continue
        end

        if c == '\\' && quoted()
            enter(st_backslash)
        elseif c == '\''
            if stateis(st_backslash) || stateis(st_dquote)
            elseif stateis(st_squote)
                leave(st_squote)
            else
                enter(st_squote)
            end
        elseif c == '"'
            if stateis(st_backslash) || stateis(st_squote)
            elseif stateis(st_dquote)
                leave(st_dquote)
            else
                enter(st_dquote)
            end
        end
        if c == '}' && !quoted()
            depth -= 1
        end

        wid = textwidth(c)
        strwid += wid
        if depth > 0
            width_at[depth] += wid
        end
        depths[i] = depth

        if c == '{' && !quoted()
            depth += 1
            if depth > length(width_at)
                push!(width_at, 0)
                push!(levelcount, 0)
            end
            levelcount[depth] += 1
        end
        if c != '\\' && stateis(st_backslash)
            leave(st_backslash)
        end
    end
    if maxdepth === nothing
        limit_at = length(width_at) + 1
        while strwid > n
            limit_at -= 1
            limit_at <= 1 && break
            strwid = strwid - width_at[limit_at] + levelcount[limit_at]
            if limit_at < length(width_at)
                strwid -= levelcount[limit_at+1]
            end
        end
    else
        limit_at = maxdepth
    end
    output = IOBuffer()
    prev = 0
    for (i, c) in Base_ANSIIterator(str)
        di = depths[i]
        if di < limit_at
            if c isa Base_ANSIDelimiter
                write(output, c.del)
            else
                write(output, c)
            end
        end
        if di > prev && di == limit_at
            write(output, "…")
        end
        prev = di
    end
    return Base.unsafe_takestring!(output)
end

# ── Fallback for non-Type objects: delegate to Base.show ────────────────
# This ensures Base_show works for Int, String, etc. which appear as
# type parameters.
Base_show(io::IO, @nospecialize(x)) = Base.show(io, x)
