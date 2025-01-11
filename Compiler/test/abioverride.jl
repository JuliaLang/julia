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

function is_known_call(@nospecialize(x), @nospecialize(func), src::Core.CodeInfo)
    isexpr(x, :call) || return false
    ft = Compiler.argextype(x.args[1], src, Compiler.VarState[])
    return Compiler.singleton_type(ft) === func
end


# Construct a CodeInstance with an ABI override
let world = Base.tls_world_age()
    # Get some inferred source code to give to the compiler
    # Do not look at a CodeInstance here, since those fields are only valid to
    # use while attached to a cache, and are thus invalid to make copies of
    # (since you'd have to have made the copy to insert into the cache before
    # making the original CodeInstance to copy from, which is obviously
    # rather temporally-challenged)
    new_source = only(code_typed(myplus, (Int, Int)))[1]
    mi = new_source.parent
    ## Sanity check
    @assert length(new_source.code) == 2
    add = new_source.code[1]
    @assert is_known_call(add, Core.Intrinsics.add_int, new_source) && add.args[3] == Core.Argument(3)

    ## Replace x + y by x + 1
    add.args[3] = 1

    ## Remove the argument
    resize!(new_source.slotnames, 2)
    resize!(new_source.slotflags, 2)

    # Construct the CodeInstance from the modified CodeInfo data
    global new_ci = Core.CodeInstance(Core.ABIOverride(Tuple{typeof(myplus), Int}, mi),
        #=owner=#SecondArgConstOverride(1), new_source.rettype, Any#=new_source.exctype is missing=#,
        #=inferred_const=#nothing, #=code=#nothing, #=const_flags=#Int32(0),
        new_source.min_world, new_source.max_world, #=new_source.ipo_purity_bits is missing=#UInt32(0),
        #=analysis_results=#nothing, new_source.debuginfo, new_source.edges)

    # Poke the CI into the global cache
    # This isn't necessary, but does conveniently give it the mandatory permanent GC-root before calling `invoke`
    ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), mi, new_ci)

    # Poke the source code into the JIT for it
    ccall(:jl_add_codeinst_to_jit, Cvoid, (Any, Any), new_ci, new_source)
end

@test contains(repr(new_ci), "ABI Overridden")
@test invoke(myplus, new_ci, 10) == 11
