#!/usr/bin/env -S julia --project=@scriptdir

module Main2
using OpenBLAS_jll
using LinearAlgebra
using OrdinaryDiffEq
#using PrecompileTools

#OpenBLAS_jll.__init__()
#LinearAlgebra.libblastrampoline_jll.__init__()
#LinearAlgebra.__init__()

f(u,p,t) = 1.01*u
const u0=1/2
const tspan = (0.0,1.0)
const prob = ODEProblem(f,u0,tspan)

Base.@ccallable function main() :: Cvoid
    sol = solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8)
    for i in eachindex(sol)
        ccall(:printf, Int32, (Ptr{UInt8},Float64...), "value %lf \n", sol[i])
    end
#    take_heap_snapshot()
    return nothing
end
#=
@setup_workload begin
    # Putting some things in `@setup_workload` instead of `@compile_workload` can reduce the size of the
    # precompile file and potentially make loading faster.
    N=10
    A = rand(N, N); b = rand(N)

    @compile_workload begin
        sol = solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8)
        for i in eachindex(sol)
            ccall(:printf, Int32, (Ptr{UInt8},Float64...), "value %lf \n", sol[i])
        end
    end
end

precompile(main, ())
precompile(Base._str_sizehint, (String,))
precompile(Base._str_sizehint, (UInt32,))
precompile(print, (Base.GenericIOBuffer{Memory{UInt8}}, String))
precompile(print, (Base.GenericIOBuffer{Memory{UInt8}}, UInt32))
precompile(join , (Base.GenericIOBuffer{Memory{UInt8}}, Array{Base.SubString{String}, 1}, String))
precompile(join , (Base.GenericIOBuffer{Memory{UInt8}}, Array{String, 1}, Char))
precompile(Base.showerror_nostdio, (Core.MissingCodeError, String))
precompile(Base.VersionNumber, (UInt32, UInt32, UInt32, Tuple{}, Tuple{}))
precompile(! ,(Bool,))
=#
end
