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

# use TRIM_UNSAFE to bypass verifier inside typeinf_ext_toplevel
let infos = typeinf_ext_toplevel(Any[Core.svec(Base.SecretBuffer, Tuple{Type{Base.SecretBuffer}})], [Base.get_world_counter()], TRIM_UNSAFE)
    @test_broken length(infos) > 4
    errors, parents = get_verify_typeinf_trim(infos)
    @test_broken isempty(errors) # missing cfunction
    #resize!(infos, 4)
    #errors, = get_verify_typeinf_trim(infos)

    desc = only(errors)
    @test desc.first
    desc = desc.second
    @test desc isa CallMissing
    @test occursin("finalizer", desc.desc)
    repr = sprint(verify_print_error, desc, parents)
    @test occursin(
        r"""^unresolved finalizer registered from statement \(Core.finalizer\)\(Base.final_shred!, %new\(\)::Base.SecretBuffer\)::Nothing
            Stacktrace:
             \[1\] finalizer\(f::typeof\(Base.final_shred!\), o::Base.SecretBuffer\)
               @ Base gcutils.jl:(\d+) \[inlined\]
             \[2\] Base.SecretBuffer\(; sizehint::Int\d\d\)
               @ Base secretbuffer.jl:(\d+) \[inlined\]
             \[3\] Base.SecretBuffer\(\)
               @ Base secretbuffer.jl:(\d+)

            $""", repr)

    resize!(infos, 1)
    @test infos[1] isa Core.SimpleVector && infos[1][1] isa Type && infos[1][2] isa Type
    errors, parents = get_verify_typeinf_trim(infos)
    desc = only(errors)
    @test !desc.first
    desc = desc.second
    @test desc isa CCallableMissing
    @test desc.rt == Base.SecretBuffer
    @test desc.sig == Tuple{Type{Base.SecretBuffer}}
    @test occursin("unresolved ccallable", desc.desc)
    repr = sprint(verify_print_error, desc, parents)
    @test repr == "unresolved ccallable for Tuple{Type{Base.SecretBuffer}} => Base.SecretBuffer\n\n"
end

let infos = typeinf_ext_toplevel(Any[Core.svec(Float64, Tuple{typeof(+), Int32, Int64})], [Base.get_world_counter()], TRIM_UNSAFE)
    errors, parents = get_verify_typeinf_trim(infos)
    desc = only(errors)
    @test !desc.first
    desc = desc.second
    @test desc isa CCallableMissing
    @test desc.rt == Int64
    @test desc.sig == Tuple{typeof(+), Int32, Int64}
    @test occursin("ccallable declared return type", desc.desc)
    repr = sprint(verify_print_error, desc, parents)
    @test repr == "ccallable declared return type does not match inference for Tuple{typeof(+), Int32, Int64} => Int64\n\n"
end

let infos = typeinf_ext_toplevel(Any[Core.svec(Int64, Tuple{typeof(ifelse), Bool, Int64, UInt64})], [Base.get_world_counter()], TRIM_UNSAFE)
    errors, parents = get_verify_typeinf_trim(infos)
    desc = only(errors)
    @test desc.first
    desc = desc.second
    @test desc isa CCallableMissing
    @test occursin("ccallable declared return type", desc.desc)
    repr = sprint(verify_print_error, desc, parents)
    @test repr == "ccallable declared return type does not match inference for Tuple{typeof(ifelse), Bool, Int64, UInt64} => Union{Int64, UInt64}\n\n"
end

let infos = typeinf_ext_toplevel(Any[Core.svec(Union{Int64,UInt64}, Tuple{typeof(ifelse), Bool, Int64, UInt64})], [Base.get_world_counter()], TRIM_SAFE)
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)
    infos = typeinf_ext_toplevel(Any[Core.svec(Real, Tuple{typeof(ifelse), Bool, Int64, UInt64})], [Base.get_world_counter()], TRIM_SAFE)
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)
end
