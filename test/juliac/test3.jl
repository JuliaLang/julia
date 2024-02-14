#!/usr/bin/env -S julia --project=@scriptdir

module Main2
using OrdinaryDiffEq
using OpenBLAS_jll
using LinearAlgebra
using PrecompileTools

function take_heap_snapshot()
    flags = Base.open_flags(
        read = true,
        write = true,
        create = true,
        truncate = true,
        append = false,
    )
    fname = "lala.heapsnapshot"
    s = IOStream("<file lala.heapsnapshot>")
    ccall(:ios_file, Ptr{Cvoid},
                      (Ptr{UInt8}, Cstring, Cint, Cint, Cint, Cint),
                      s.ios, fname, flags.read, flags.write, flags.create, flags.truncate)
    ccall(:jl_gc_take_heap_snapshot, Cvoid, (Ptr{Cvoid}, Cchar), s.handle, Cchar(false))
    ccall(:ios_close, Cint, (Ptr{Cvoid},), s.ios)
    return nothing
end

f(u,p,t) = 1.01*u
const u0=1/2
const tspan = (0.0,1.0)
const prob = ODEProblem(f,u0,tspan)

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
    sol = solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8)
    for i in eachindex(sol)
        ccall(:printf, Int32, (Ptr{UInt8},Float64...), "value %lf \n", sol[i])
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
        sol = solve(prob,Tsit5(),reltol=1e-8,abstol=1e-8)
        for i in eachindex(sol)
            ccall(:printf, Int32, (Ptr{UInt8},Float64...), "value %lf \n", sol[i])
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