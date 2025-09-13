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

finalizer(@nospecialize(f), @nospecialize(o)) = Core.finalizer(f, o)

let infos = typeinf_ext_toplevel(Any[Core.svec(Nothing, Tuple{typeof(finalizer), typeof(identity), Any})], [Base.get_world_counter()], TRIM_UNSAFE)
    errors, parents = get_verify_typeinf_trim(infos)
    @test !isempty(errors) # unresolvable finalizer

    # the only error should be a CallMissing error for the Core.finalizer builtin
    (warn, desc) = only(errors)
    @test !warn
    @test desc isa CallMissing
    @test occursin("finalizer", desc.desc)
    repr = sprint(verify_print_error, desc, parents)
    @test occursin(
        r"""^unresolved finalizer registered from statement \(Core.finalizer\)\(f::Any, o::Any\)::Nothing
            Stacktrace:
             \[1\] finalizer\(f::Any, o::Any\)""", repr)
end

# test that basic `cfunction` generation is allowed, when the dispatch target can be resolved
make_cfunction() = @cfunction(+, Float64, (Int64,Int64))
let infos = typeinf_ext_toplevel(Any[Core.svec(Ptr{Cvoid}, Tuple{typeof(make_cfunction)})], [Base.get_world_counter()], TRIM_UNSAFE)
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)
end

# use TRIM_UNSAFE to bypass verifier inside typeinf_ext_toplevel
make_cfunction_bad(@nospecialize(f::Any)) = @cfunction($f, Float64, (Int64,Int64))::Base.CFunction
let infos = typeinf_ext_toplevel(Any[Core.svec(Base.CFunction, Tuple{typeof(make_cfunction_bad), Any})], [Base.get_world_counter()], TRIM_UNSAFE)
    errors, parents = get_verify_typeinf_trim(infos)
    @test !isempty(errors) # missing cfunction

    (is_warning, desc) = only(errors)
    @test !is_warning
    @test desc isa CallMissing
    @test occursin("cfunction", desc.desc)
    repr = sprint(verify_print_error, desc, parents)
    @test occursin(r"""^unresolved cfunction from statement \$\(Expr\(:cfunction, Base.CFunction, :\(f::Any\), Float64, :\(svec\(Int64, Int64\)::Core.SimpleVector\), :\(:ccall\)\)\)::Base.CFunction
            Stacktrace:
             \[1\] make_cfunction_bad\(f::Any\)""", repr)
    resize!(infos, 1)
    @test infos[1] isa Core.SimpleVector && infos[1][1] isa Type && infos[1][2] isa Type
    errors, parents = get_verify_typeinf_trim(infos)
    desc = only(errors)
    @test !desc.first
    desc = desc.second
    @test desc isa CCallableMissing
    @test desc.rt == Base.CFunction
    @test desc.sig == Tuple{typeof(make_cfunction_bad), Any}
    @test occursin("unresolved ccallable", desc.desc)
    repr = sprint(verify_print_error, desc, parents)
    @test repr == "unresolved ccallable for Tuple{$(typeof(make_cfunction_bad)), Any} => Base.CFunction\n\n"
end

let infos = typeinf_ext_toplevel(Any[Core.svec(Base.SecretBuffer, Tuple{Type{Base.SecretBuffer}})], [Base.get_world_counter()], TRIM_UNSAFE)
    @test length(infos) > 4
    errors, parents = get_verify_typeinf_trim(infos)
    @test isempty(errors)

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
