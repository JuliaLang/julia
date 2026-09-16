# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.Meta
include("irutils.jl")

# In this test, we will manually construct a CodeInstance that specializes the `myplus`
# method on a constant for the second argument and test various interfaces surrounding
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
    new_source.nargs = 2

    # Construct the CodeInstance from the modified CodeInfo data
    global new_ci = Core.CodeInstance(Core.ABIOverride(Tuple{typeof(myplus), Int}, mi),
        #=owner=#SecondArgConstOverride(1), new_source.rettype, Any#=new_source.exctype is missing=#,
        #=inferred_const=#nothing, #=code=#nothing, #=const_flags=#Int32(0),
        new_source.min_world, typemax(UInt), #=new_source.ipo_purity_bits is missing=#UInt32(0),
        #=analysis_results=#nothing, new_source.debuginfo, new_source.edges)

    # Poke the CI into the global cache
    # This isn't necessary, but does conveniently give it the mandatory permanent GC-root before calling `invoke`
    ccall(:jl_mi_cache_insert, Cvoid, (Any, Any), mi, new_ci)

    # Poke the source code into the JIT for it
    ccall(:jl_add_codeinsts_to_jit, Cvoid, (Any, Any), Any[new_ci], Any[new_source])
end

@test contains(repr(new_ci), "ABI Overridden")
@test invoke(myplus, new_ci, 10) == 11

# Test that fallback paths that run when a CodeInstance has no compiled code available
# do not silently substitute the plain method for a CodeInstance whose semantics
# (foreign `owner`) or calling convention (`ABIOverride`) differ from it.

# Permanent GC-roots for manually-constructed CodeInstances handed to the JIT
const ci_roots = Any[]

# Construct a CodeInstance for `mi` (with an ABI override if `abi` is given), rooted
# permanently; `src` provides the metadata, and is only handed to the JIT if `add_to_jit`
# is set — otherwise the CodeInstance carries no code and cannot be compiled later.
function make_codeinst(mi::Core.MethodInstance, src::Core.CodeInfo;
                       abi=nothing, owner=nothing, add_to_jit::Bool=false)
    def = abi === nothing ? mi : Core.ABIOverride(abi, mi)
    ci = Core.CodeInstance(def, owner, src.rettype, Any,
        #=inferred_const=#nothing, #=code=#nothing, #=const_flags=#Int32(0),
        src.min_world, typemax(UInt), #=ipo_purity_bits=#UInt32(0),
        #=analysis_results=#nothing, src.debuginfo, src.edges)
    push!(ci_roots, ci)
    if add_to_jit
        ccall(:jl_add_codeinsts_to_jit, Cvoid, (Any, Any), Any[ci], Any[src])
    end
    return ci
end

mysum(x::Int, y::Int) = x + y

let src = only(code_typed(mysum, (Int, Int)))[1]
    mi = src.parent
    # An override CodeInstance with no code attached: compilation is impossible, so
    # calling it must error rather than silently running plain `mysum`
    abici = make_codeinst(mi, src; abi=Tuple{typeof(mysum), Int, Int})

    # builtin `invoke` fallback (`jl_f_invoke`)
    @test_throws ErrorException invoke(mysum, abici, 1, 2)

    # interpreter fallback (`do_invoke`): run an `Expr(:invoke, ci, ...)` statement
    # through the toplevel interpreter
    thk = Meta.lower(@__MODULE__, :($mysum(1, 2))).args[1]::Core.CodeInfo
    callidx = findfirst(x -> isexpr(x, :call), thk.code)
    @assert callidx !== nothing
    thk.code[callidx] = Expr(:invoke, abici, mysum, 1, 2)
    @test_throws ErrorException ccall(:jl_eval_thunk, Any, (Any, Any, Cint), @__MODULE__, thk, 0)
end

# Same for the `tojlinvoke` trampoline emitted when compiled code targets a
# CodeInstance that has no code available at link time
@noinline mycallee(x::Int, y::Int) = x * y
mycaller(x::Int, y::Int) = mycallee(x, y)

let caller_src = only(code_typed(mycaller, (Int, Int)))[1]
    caller_mi = caller_src.parent
    invokeidx = findfirst(x -> isexpr(x, :invoke), caller_src.code)
    @assert invokeidx !== nothing

    callee_src = only(code_typed(mycallee, (Int, Int)))[1]
    callee_ci = make_codeinst(callee_src.parent, callee_src;
                              abi=Tuple{typeof(mycallee), Int, Int})

    # Retarget the caller's `:invoke` at the codeless override CodeInstance and compile it
    caller_src.code[invokeidx].args[1] = callee_ci
    caller_ci = make_codeinst(caller_mi, caller_src;
                              owner=SecondArgConstOverride(2), add_to_jit=true)
    @test_throws ErrorException invoke(mycaller, caller_ci, 1, 2)
end

# Test that codegen's self-recursion shortcut is not applied when an `:invoke` targets a
# *different* CodeInstance of the method currently being compiled
# (`@noinline` keeps the recursion from being partially inlined, so the typed IR contains
# exactly one recursive `:invoke` and one `mul_int`)
@noinline myfact(n::Int) = n <= 1 ? 1 : n * myfact(n - 1)

function retarget_recursion!(src::Core.CodeInfo, @nospecialize(target))
    invokeidx = findfirst(x -> isexpr(x, :invoke), src.code)
    @assert invokeidx !== nothing
    src.code[invokeidx].args[1] = target
    return src
end

let mi = only(code_typed(myfact, (Int,)))[1].parent
    # Variant A: replace the multiply with an add, recursing into plain `myfact`,
    # i.e. a(n) = n <= 1 ? 1 : n + myfact(n - 1)
    src_a = only(code_typed(myfact, (Int,)))[1]
    mulidx = findfirst(x -> is_known_call(x, Core.Intrinsics.mul_int, src_a), src_a.code)
    @assert mulidx !== nothing
    src_a.code[mulidx].args[1] = Core.Intrinsics.add_int
    ci_a = make_codeinst(mi, src_a; owner=SecondArgConstOverride(3), add_to_jit=true)
    # a(3) = 3 + myfact(2) = 3 + 2
    @test invoke(myfact, ci_a, 3) == 5

    # Variant B: keep the multiply, but recurse into variant A,
    # i.e. b(n) = n <= 1 ? 1 : n * a(n - 1)
    src_b = retarget_recursion!(only(code_typed(myfact, (Int,)))[1], ci_a)
    ci_b = make_codeinst(mi, src_b; owner=SecondArgConstOverride(4), add_to_jit=true)
    # b(3) = 3 * a(2) = 3 * (2 + myfact(1)) = 9. If codegen's self-recursion shortcut
    # incorrectly reused the function being compiled for this `:invoke`, this would
    # recurse into variant B itself and return factorial(3) == 6
    @test invoke(myfact, ci_b, 3) == 9

    # A genuinely self-recursive variant is still handled by the shortcut:
    # c(n) = n <= 1 ? 1 : n + c(n - 1), recursing into its own CodeInstance
    src_c = only(code_typed(myfact, (Int,)))[1]
    mulidx = findfirst(x -> is_known_call(x, Core.Intrinsics.mul_int, src_c), src_c.code)
    @assert mulidx !== nothing
    src_c.code[mulidx].args[1] = Core.Intrinsics.add_int
    ci_c = make_codeinst(mi, src_c; owner=SecondArgConstOverride(5))
    retarget_recursion!(src_c, ci_c)
    ccall(:jl_add_codeinsts_to_jit, Cvoid, (Any, Any), Any[ci_c], Any[src_c])
    # c(3) = 3 + c(2) = 3 + 2 + c(1)
    @test invoke(myfact, ci_c, 3) == 6
end

# Same for the `needsparams` path in codegen's `emit_invoke`, taken when the target
# method instance has unresolved static parameters (here `T` in a specialization on
# the abstract `Vector`), which previously always emitted a plain `jl_invoke`
@noinline myparam(x::Vector{T}) where T = length(x)
mycaller2(x::Vector) = myparam(x)

let caller_src = only(code_typed(mycaller2, (Vector,)))[1]
    caller_mi = caller_src.parent
    callidx = findfirst(x -> isexpr(x, :call), caller_src.code)
    @assert callidx !== nothing

    callee_src = only(code_typed(myparam, (Vector,)))[1]
    @assert any(x -> x isa Core.SimpleVector, callee_src.parent.sparam_vals) # i.e. `needsparams`
    callee_ci = make_codeinst(callee_src.parent, callee_src;
                              abi=Tuple{typeof(myparam), Vector})

    # Retarget the caller's call at the codeless override CodeInstance and compile it
    call = caller_src.code[callidx]
    caller_src.code[callidx] = Expr(:invoke, callee_ci, call.args...)
    caller_ci = make_codeinst(caller_mi, caller_src;
                              owner=SecondArgConstOverride(6), add_to_jit=true)
    @test_throws ErrorException invoke(mycaller2, caller_ci, [1, 2, 3])
end
