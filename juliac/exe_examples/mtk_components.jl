module Main2

using OrdinaryDiffEq
using OpenBLAS_jll
using LinearAlgebra
using PrecompileTools
using OrdinaryDiffEq, ModelingToolkit
using ModelingToolkitStandardLibrary.Electrical, ModelingToolkit, OrdinaryDiffEq
using ModelingToolkitStandardLibrary.Blocks: Step,
    Constant, Sine, Cosine, ExpSine, Ramp,
    Square, Triangular
using ModelingToolkitStandardLibrary.Blocks: square, triangular
using OrdinaryDiffEq: ReturnCode.Success
using SciMLBase
using SparseDiffTools

@parameters t
@named source = Sine(offset = 0, amplitude = 1.0, frequency = 1e3, start_time = 0.5, phase = 0)
@named voltage = Voltage()
@named R1 = Resistor(R = 1e3)
@named R2 = Resistor(R = 1e3)
@named ground = Ground()

const connections = [connect(source.output, voltage.V)
    connect(voltage.p, R1.p)
    connect(R1.n, R2.p)
    connect(R2.n, voltage.n, ground.g)]

@named model = ODESystem(connections, t,
        systems = [R1, R2, source, voltage, ground])
sys = structural_simplify(model)
const prob = ODEProblem(sys, Pair[R2.i => 0.0], (0, 2.0))

# and then if that works...
# this will probably fail since it goes through RuntimeGeneratedFunctions

Base.@ccallable function main() :: Cvoid
    sol = solve(prob, Rodas5P())
    for i in eachindex(sol)
        ccall(:printf, Int32, (Ptr{UInt8},Float64...), "value %lf \n", sol[i][1])
    end
    return nothing
end

@setup_workload begin
    # Putting some things in `@setup_workload` instead of `@compile_workload` can reduce the size of the
    # precompile file and potentially make loading faster.
    N=10
    A = rand(N, N); b = rand(N)

    @compile_workload begin
        sol = solve(prob, Rodas5P())
        for i in eachindex(sol)
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
end
