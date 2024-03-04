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

function take_heap_snapshot()
    flags = Base.open_flags(
        read = true,
        write = true,
        create = true,
        truncate = true,
        append = false,
    )
    nodes = IOStream("<file lala.nodes>")
    ccall(:ios_file, Ptr{Cvoid}, (Ptr{UInt8}, Cstring, Cint, Cint, Cint, Cint),
        nodes.ios, "lala.nodes", flags.read, flags.write, flags.create, flags.truncate)
    edges = IOStream("<file lala.edges>")
    ccall(:ios_file, Ptr{Cvoid}, (Ptr{UInt8}, Cstring, Cint, Cint, Cint, Cint),
        edges.ios, "lala.edges", flags.read, flags.write, flags.create, flags.truncate)
    strings = IOStream("<file lala.strings>")
    ccall(:ios_file, Ptr{Cvoid},(Ptr{UInt8}, Cstring, Cint, Cint, Cint, Cint),
        strings.ios, "lala.strings", flags.read, flags.write, flags.create, flags.truncate)
    json = IOStream("<file lala.metadata.json>")
    ccall(:ios_file, Ptr{Cvoid}, (Ptr{UInt8}, Cstring, Cint, Cint, Cint, Cint),
        json.ios, "lala.metadata.json", flags.read, flags.write, flags.create, flags.truncate)
    ccall(:jl_gc_take_heap_snapshot,
        Cvoid,
        (Ptr{Cvoid},Ptr{Cvoid},Ptr{Cvoid},Ptr{Cvoid}, Cchar),
        nodes.handle, edges.handle, strings.handle, json.handle,
        Cchar(false))
    ccall(:ios_close, Cint, (Ptr{Cvoid},), nodes.ios)
    ccall(:ios_close, Cint, (Ptr{Cvoid},), edges.ios)
    ccall(:ios_close, Cint, (Ptr{Cvoid},), strings.ios)
    ccall(:ios_close, Cint, (Ptr{Cvoid},), json.ios)
    return nothing
end

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
    Sys.__init__()
    OpenBLAS_jll.__init__()
    LinearAlgebra.libblastrampoline_jll.__init__()
    LinearAlgebra.__init__()
    # println("Hello, world!")
    task = current_task()
    task.rngState0 = 0x5156087469e170ab
    task.rngState1 = 0x7431eaead385992c
    task.rngState2 = 0x503e1d32781c2608
    task.rngState3 = 0x3a77f7189200c20b
    task.rngState4 = 0x5502376d099035ae
    sol = solve(prob, Rodas5P())
    for i in eachindex(sol)
        ccall(:printf, Int32, (Ptr{UInt8},Float64...), "value %lf \n", sol[i][1])
    end
    # take_heap_snapshot()
    return nothing
end

@setup_workload begin
    # Putting some things in `@setup_workload` instead of `@compile_workload` can reduce the size of the
    # precompile file and potentially make loading faster.
    N=10
    A = rand(N, N); b = rand(N)

    @compile_workload begin
        Sys.__init__()
        OpenBLAS_jll.__init__()
        LinearAlgebra.libblastrampoline_jll.__init__()
        LinearAlgebra.__init__()
        sol = solve(prob, Rodas5P())
        for i in eachindex(sol)
            ccall(:printf, Int32, (Ptr{UInt8},Float64...), "value %lf \n", sol[i][1])
        end
    end
end

precompile(main, ())
precompile(Base._str_sizehint, (String,))
precompile(Base._str_sizehint, (UInt32,))
precompile(print, (Base.GenericIOBuffer{Array{UInt8, 1}}, String))
precompile(print, (Base.GenericIOBuffer{Array{UInt8, 1}}, UInt32))
precompile(join , (Base.GenericIOBuffer{Array{UInt8, 1}}, Array{Base.SubString{String}, 1}, String))
precompile(join , (Base.GenericIOBuffer{Array{UInt8, 1}}, Array{String, 1}, Char))
precompile(Base.showerror_nostdio, (Core.MissingCodeError, String))
precompile(Base.VersionNumber, (UInt32, UInt32, UInt32, Tuple{}, Tuple{}))
# precompile(print, (Base.GenericIOBuffer{Core.Array{UInt8, 1}}, String) )
precompile(! ,(Bool,))
precompile(SparseDiffTools._get_t, (DataType, Core.Array{Float64, 1}, Core.Array{Core.Array{Tuple{Float64}, 1}, 1}))
# SparseDiffTools.var"#_get_t"()(DataType, Core.Array{Float64, 1}, Core.Array{Core.Array{Tuple{Float64}, 1}, 1})
end