#!/usr/bin/env -S julia --project=@scriptdir

module Main2
using OrdinaryDiffEq
using OpenBLAS_jll
using LinearAlgebra
using PrecompileTools
using OrdinaryDiffEq, ModelingToolkit

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
        Sys.__init__()
        OpenBLAS_jll.__init__()
        LinearAlgebra.libblastrampoline_jll.__init__()
        LinearAlgebra.__init__()
        sol = solve(prob,Tsit5())
        for i in 1:15
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
precompile(! ,(Bool,))
# precompile()
end