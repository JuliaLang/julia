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
    n = (length(tab) - 3) ÷ 4
    # member instances are at table slots 3 + 4*(i-1) + 3
    return ntuple(i -> tab[4 * i + 2], n)
end

# the (name::Symbol, owning module::Module) of the member with the same bits
# as `x`, or nothing if the bit pattern has no registered member
function enum_member_info(@nospecialize(x))
    m = ccall(:jl_enum_lookup_value, Any, (Any,), x)
    m === nothing && return nothing
    m = m::Core.SimpleVector
    return (m[1]::Symbol, m[2]::Module)
end

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
        sym, def = info
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
