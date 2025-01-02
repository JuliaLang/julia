# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Meta
include("irutils.jl")

# In this test, we will manually construct a CodeInstance that specializes the `myplus`
# method on a constant for the second argument and test various, interfaces surrounding
# CodeInstances with ABI overrides.
myplus(x::Int, y::Int) = x + y

struct SecondArgConstOverride
    arg2::Int
end

world = Base.tls_world_age()
mi = Base.specialize_method(only(Base._methods_by_ftype(Tuple{typeof(myplus), Int, Int}, -1, world)))
interp = Compiler.NativeInterpreter(world)
ci = Compiler.typeinf_ext(interp, mi, Compiler.SOURCE_MODE_FORCE_SOURCE)

function is_known_call(@nospecialize(x), @nospecialize(func), src::Core.CodeInfo)
    isexpr(x, :call) || return false
    ft = Compiler.argextype(x.args[1], src, Compiler.VarState[])
    return Compiler.singleton_type(ft) === func
end

# Construct a CodeInstance with an ABI override
new_ci = let new_source = copy(Base.uncompressed_ir(ci))
    ## Sanity check
    @assert length(new_source.code) == 2
    add = new_source.code[1]
    @assert is_known_call(add, Core.Intrinsics.add_int, new_source) && add.args[3] == Core.Argument(3)

    ## Replace x + y by x + 1
    add.args[3] = 1

    ## Remove the argument
    resize!(new_source.slotnames, 2)
    resize!(new_source.slotflags, 2)

    # Construct the CodeInstance
    new_ci = Core.CodeInstance(Core.ABIOverride(Tuple{typeof(myplus), Int}, mi),
        SecondArgConstOverride(1), ci.rettype, ci.exctype, nothing, new_source,
        Int32(0), ci.min_world, ci.max_world, ci.ipo_purity_bits, nothing, ci.relocatability, ci.debuginfo, ci.edges)

    # Poke the CI into the global cache
    ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), mi, new_ci)

    new_ci
end

@test contains(repr(new_ci), "ABI Overridden")
@test invoke(myplus, new_ci, 10) == 11
