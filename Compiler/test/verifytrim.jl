# This file is a part of Julia. License is MIT: https://julialang.org/license

include("setup_Compiler.jl")

# revise: Core.include(Compiler.TrimVerifier, joinpath(@__DIR__, "../src/verifytrim.jl"))

using Test
using .Compiler: typeinf_ext_toplevel, TrimVerifier, TRIM_SAFE, TRIM_UNSAFE
using .TrimVerifier: get_verify_typeinf_trim, verify_print_error, CallMissing, CCallableMissing

function sprint(f, args...)
    return Base.sprint((io, f, args...) -> f(IOContext{IO}(io), args...), f, args...)
end

let infos = Any[]
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)
    @test isempty(parents)
end

struct ScopedTrimEnv
    value::Int
end

const scoped_trim_env = Base.ScopedValues.ScopedValue(ScopedTrimEnv(1))

# Scope updates must keep the abstract-key HAMT traversal statically resolvable.
function scoped_trim_read()
    return Base.ScopedValues.with(scoped_trim_env => ScopedTrimEnv(2)) do
        scoped_trim_env[].value
    end
end

let infos = typeinf_ext_toplevel(
    Any[Base.method_instance(scoped_trim_read, ())],
    [Base.get_world_counter()],
    TRIM_UNSAFE,
)[1]
    errors, _ = get_verify_typeinf_trim(infos)
    @test scoped_trim_read() == 2
    @test isempty(errors)
end

finalizer(@nospecialize(f), @nospecialize(o)) = Core.finalizer(f, o)

let infos = typeinf_ext_toplevel(Any[Core.svec(Nothing, Tuple{typeof(finalizer), typeof(identity), Any})], [Base.get_world_counter()], TRIM_UNSAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    @test !isempty(errors) # unresolvable finalizer

    # the only error should be a CallMissing error for the Core.finalizer builtin
    (warn, desc) = only(errors)
    @test !warn
    @test desc isa CallMissing
    @test occursin("finalizer", desc.desc)
    repr = sprint(verify_print_error, desc, parents, warn)
    # New format uses multiline for unstable types
    @test occursin(r"^unresolved finalizer registered from statement (Core\.)?finalizer\("s, repr)
    @test occursin(r"f::Any"s, repr)
    @test occursin(r"o::Any"s, repr)
    @test occursin(r"::Nothing\n\nStacktrace:"s, repr)
    @test occursin(r"\[1\] finalizer\(f::Any, o::Any\)"s, repr)
end

# test that basic `cfunction` generation is allowed, when the dispatch target can be resolved
make_cfunction() = @cfunction(+, Float64, (Int64,Int64))
let infos = typeinf_ext_toplevel(Any[Core.svec(Ptr{Cvoid}, Tuple{typeof(make_cfunction)})], [Base.get_world_counter()], TRIM_UNSAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)
end

# use TRIM_UNSAFE to bypass verifier inside typeinf_ext_toplevel
make_cfunction_bad(@nospecialize(f::Any)) = @cfunction($f, Float64, (Int64,Int64))::Base.CFunction
let infos = typeinf_ext_toplevel(Any[Core.svec(Base.CFunction, Tuple{typeof(make_cfunction_bad), Any})], [Base.get_world_counter()], TRIM_UNSAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    @test !isempty(errors) # missing cfunction

    (is_warning, desc) = only(errors)
    @test !is_warning
    @test desc isa CallMissing
    @test occursin("cfunction", desc.desc)
    repr = sprint(verify_print_error, desc, parents, is_warning)
    @test occursin(r"^unresolved cfunction from statement"s, repr)
    @test occursin(r"::Base.CFunction\n\nStacktrace:"s, repr)
    @test occursin(r"\[1\] make_cfunction_bad\(f::Any\)"s, repr)
    resize!(infos, 1)
    @test infos[1] isa Core.SimpleVector && infos[1][1] isa Type && infos[1][2] isa Type
    errors, parents = get_verify_typeinf_trim(infos)
    (warn, desc) = only(errors)
    @test !warn
    @test desc isa CCallableMissing
    @test desc.rt == Base.CFunction
    @test desc.sig == Tuple{typeof(make_cfunction_bad), Any}
    @test occursin("unresolved ccallable", desc.desc)
    repr = sprint(verify_print_error, desc, parents, warn)
    @test repr == "unresolved ccallable for Tuple{$(typeof(make_cfunction_bad)), Any} => Base.CFunction\n\n"
end

let infos = typeinf_ext_toplevel(Any[Core.svec(Base.SecretBuffer, Tuple{Type{Base.SecretBuffer}})], [Base.get_world_counter()], TRIM_UNSAFE)[1]
    @test length(infos) > 4
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)

    resize!(infos, 1)
    @test infos[1] isa Core.SimpleVector && infos[1][1] isa Type && infos[1][2] isa Type
    errors, parents = get_verify_typeinf_trim(infos)
    (warn, desc) = only(errors)
    @test !warn
    @test desc isa CCallableMissing
    @test desc.rt == Base.SecretBuffer
    @test desc.sig == Tuple{Type{Base.SecretBuffer}}
    @test occursin("unresolved ccallable", desc.desc)
    repr = sprint(verify_print_error, desc, parents, warn)
    @test repr == "unresolved ccallable for Tuple{Type{Base.SecretBuffer}} => Base.SecretBuffer\n\n"
end

let infos = typeinf_ext_toplevel(Any[Core.svec(Float64, Tuple{typeof(+), Int32, Int64})], [Base.get_world_counter()], TRIM_UNSAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    (warn, desc) = only(errors)
    @test !warn
    @test desc isa CCallableMissing
    @test desc.rt == Int64
    @test desc.sig == Tuple{typeof(+), Int32, Int64}
    @test occursin("ccallable declared return type", desc.desc)
    repr = sprint(verify_print_error, desc, parents, warn)
    @test repr == "ccallable declared return type does not match inference for Tuple{typeof(+), Int32, Int64} => Int64\n\n"
end

let infos = typeinf_ext_toplevel(Any[Core.svec(Int64, Tuple{typeof(ifelse), Bool, Int64, UInt64})], [Base.get_world_counter()], TRIM_UNSAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    (warn, desc) = only(errors)
    @test warn  # this is a warning since Union{Int64, UInt64} <: Int64 is false but not an error
    @test desc isa CCallableMissing
    @test occursin("ccallable declared return type", desc.desc)
    repr = sprint(verify_print_error, desc, parents, warn)
    @test repr == "ccallable declared return type does not match inference for Tuple{typeof(ifelse), Bool, Int64, UInt64} => Union{Int64, UInt64}\n\n"
end

let infos = typeinf_ext_toplevel(Any[Core.svec(Union{Int64,UInt64}, Tuple{typeof(ifelse), Bool, Int64, UInt64})], [Base.get_world_counter()], TRIM_SAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)
    infos = typeinf_ext_toplevel(Any[Core.svec(Real, Tuple{typeof(ifelse), Bool, Int64, UInt64})], [Base.get_world_counter()], TRIM_SAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)
end


mi = Base.method_instance(sum, (Vector{Union{Int64,Float64, Float32,UInt32}},))
let infos = typeinf_ext_toplevel(Any[mi], [Base.get_world_counter()], TRIM_UNSAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    @test !isempty(errors)
end

# A `ccall`/`cglobal` whose library is given by a runtime value loads that library on first
# use by calling back into `Libdl.dlopen(lib)`, which must be checked by the verifier (and
# anticipated by `collectinvokes!`). A library type whose `dlopen` method is itself fully
# static keeps the entire cone resolvable, so this site must verify with no errors at all.
struct StaticTrimLib end
const static_trim_lib = StaticTrimLib()
const static_trim_libname = "libjulia-internal"
Base.Libc.Libdl.dlopen(::StaticTrimLib) =
    ccall(:jl_load_dynamic_library, Ptr{Cvoid}, (Ptr{UInt8}, UInt32, Cint),
          static_trim_libname, Base.Libc.Libdl.RTLD_LAZY, Cint(0))
trim_ccall_static_lib() = ccall((:jl_getpid, static_trim_lib), Int32, ())

let infos = typeinf_ext_toplevel(Any[Core.svec(Int32, Tuple{typeof(trim_ccall_static_lib)})],
                                 [Base.get_world_counter()], TRIM_UNSAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)
    # the `dlopen(::StaticTrimLib)` call the runtime makes was enqueued and compiled
    @test any(infos) do item
        item isa Core.CodeInstance || return false
        mi = item.def
        mi isa Core.MethodInstance &&
            mi.specTypes === Tuple{typeof(Base.Libc.Libdl.dlopen), StaticTrimLib}
    end
end

# The same site, but with the library in a non-constant global so that its type is not
# known at the call site: the runtime `dlopen` call cannot be resolved and must be rejected.
global loose_trim_lib = static_trim_lib
trim_ccall_loose_lib() = ccall((:jl_getpid, loose_trim_lib), Int32, ())

let infos = typeinf_ext_toplevel(Any[Core.svec(Int32, Tuple{typeof(trim_ccall_loose_lib)})],
                                 [Base.get_world_counter()], TRIM_UNSAFE)[1]
    errors, parents = get_verify_typeinf_trim(infos)
    (warn, desc) = only(errors)
    @test !warn
    @test desc isa CallMissing
    @test desc.desc == "unresolved dlopen for ccall / cglobal"
    repr = sprint(verify_print_error, desc, parents, warn)
    @test occursin("unresolved dlopen for ccall / cglobal", repr)
end
