# This file is a part of Julia. License is MIT: https://julialang.org/license

# Support for extensible enum types declared with the `enum` keyword.
#
# Enum types are primitive-like types carrying a runtime member table on
# their TypeName. They have no common supertype (the declared supertype is
# user-chosen), so the generic entry points below test for enum-ness with
# `isenumtype` instead of dispatching on a type.

"""
    isenumtype(T) -> Bool

Whether `T` is an enum type declared with the [`enum`](@ref) keyword.

!!! compat "Julia 1.14"
    This function requires Julia 1.14 or later.
"""
isenumtype(@nospecialize t) = isa(t, DataType) && isdefined(t.name, :enumtab)

# the (concrete primitive integer) storage type of the enum type `t`
enumstoragetype(@nospecialize(t::DataType)) = ccall(:jl_enum_storagetype, Any, (Any,), t)

# whether the enum type `t` is open (extensible)
isopenenum(@nospecialize(t::DataType)) = ccall(:jl_enum_isopen, Cint, (Any,), t) != 0

function instances(@nospecialize(t::Type))
    isenumtype(t) || throw(MethodError(instances, (t,)))
    tab = Core._enum_members(t)
    n = (length(tab) - 3) ÷ 5
    # member instances are at table slots 3 + 5*(i-1) + 3
    return ntuple(i -> tab[5 * i + 1], n)
end

# the (name::Symbol, owning module::Module, isexplicit::Bool) of the member
# with the same bits as `x`, or nothing if the bit pattern has no registered
# member
function enum_member_info(@nospecialize(x))
    m = ccall(:jl_enum_lookup_value, Any, (Any,), x)
    m === nothing && return nothing
    m = m::Core.SimpleVector
    return (m[1]::Symbol, m[2]::Module, m[4]::Bool)
end

# whether inline data of type `t` can contain enum-typed bits
function type_contains_enum(@nospecialize t)
    isenumtype(t) && return true
    isa(t, DataType) || return false
    for ft in fieldtypes(t)
        type_contains_enum(ft) && return true
    end
    return false
end

# The member (mod, name) of the enum type `t`, registered in the member table
# (without a constant binding) if not present: deserialization uses this to
# resolve members whose declaring package is not loaded. A later registration
# by the package unifies with the entry created here.
enum_resolve_member(t::DataType, mod::Module, name::Symbol, hint::UInt64, isexplicit::Bool) =
    ccall(:jl_enum_resolve_member, Any, (Any, Any, Any, UInt64, Cint),
          t, mod, name, hint, isexplicit)

function show_enum_value(io::IO, @nospecialize(x))
    info = enum_member_info(x)
    t = typeof(x)
    if info === nothing
        # a bit pattern that does not correspond to a registered member
        print(io, "reinterpret(")
        show(io, t)
        print(io, ", ")
        show(io, reinterpret(enumstoragetype(t)::Type, x))
        print(io, ")")
    else
        sym, def = info[1], info[2]
        if !(get(io, :compact, false)::Bool)
            from = get(io, :module, Main)
            if from === nothing || !isvisible(sym, def, from)
                show(io, def)
                print(io, ".")
            end
        end
        print(io, sym)
    end
    return nothing
end

function show_enum_value_plain(io::IO, @nospecialize(x))
    show_enum_value(io, x)
    print(io, "::")
    show(IOContext(io, :compact => true), typeof(x))
    print(io, " = ")
    show(io, reinterpret(enumstoragetype(typeof(x))::Type, x))
    return nothing
end

# `hash` for enum values uses the member's identity hash stored in the member
# table, which is derived from the owning module and member name. This keeps
# hashes independent of the member's (rebasable) bit pattern, so they are
# stable across sessions and hash containers keyed by enum values survive
# serialization into package images.
function hash_enum_value(@nospecialize(x), h::UInt)
    m = ccall(:jl_enum_lookup_value, Any, (Any,), x)
    if m === nothing
        # a bit pattern with no registered member
        return hash(objectid(x), h)
    end
    return hash((m::Core.SimpleVector)[5]::UInt64, h)
end
