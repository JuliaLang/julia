module Main2
using OrdinaryDiffEq
using OpenBLAS_jll
using LinearAlgebra
using PrecompileTools
using OrdinaryDiffEq, ModelingToolkit


@parameters σ ρ β
@variables t x(t) y(t) z(t)
const D = Differential(t)

const eqs = [D(D(x)) ~ σ * (y - x),
    D(y) ~ x * (ρ - z) - y,
    D(z) ~ x * y - β * z]

@named sys = ODESystem(eqs)
sys = structural_simplify(sys)

const u0 = [D(x) => 2.0,
    x => 1.0,
    y => 0.0,
    z => 0.0]

const p = [σ => 28.0,
    ρ => 10.0,
    β => 8 / 3]

const tspan = (0.0, 100.0)
const prob = ODEProblem(sys, u0, tspan, p, jac = true)


Base.@ccallable function main() :: Cint
    sol = solve(prob,Tsit5())
    for i in 1:15
        ccall(:printf, Int32, (Ptr{UInt8},Float64...), "value %lf \n", sol[i][1])
    end
    take_heap_snapshot()
    return 0
end

@setup_workload begin
    # Putting some things in `@setup_workload` instead of `@compile_workload` can reduce the size of the
    # precompile file and potentially make loading faster.
    N=10
    A = rand(N, N); b = rand(N)

    @compile_workload begin
        sol = solve(prob,Tsit5())
        for i in 1:15
            ccall(:printf, Int32, (Ptr{UInt8},Float64...), "value %lf \n", sol[i][1])
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
# precompile()
end