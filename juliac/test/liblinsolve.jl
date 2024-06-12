module Lib

using OpenBLAS_jll, PrecompileTools, SparseArrays, RecursiveFactorization, StrideArraysCore, Static, LinearAlgebra, Logging

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

Base.@ccallable function linsolve10(A::Ptr{Float64}, b::Ptr{Float64})::Ptr{Float64}
    OpenBLAS_jll.__init__()
    LinearAlgebra.libblastrampoline_jll.__init__()
    LinearAlgebra.__init__()
    # SparseArrays.CHOLMOD.__init__()
    N = 10
    piv = Ref{NTuple{N, Int}}()
    D = static(N)
    GC.@preserve piv begin
        F = RecursiveFactorization.lu!(PtrArray(A, (D, D)),
            PtrArray(Base.unsafe_convert(Ptr{Int}, piv), (D,)))
        ldiv!(F, PtrArray(b, (D,)))
    end
    # take_heap_snapshot()
    return b
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
            # SparseArrays.CHOLMOD.__init__()
            GC.@preserve A b begin
                linsolve10(pointer(A), pointer(b))
            end
        end
    end
    precompile(Core.current_scope, ())
    precompile(! ,(Bool,))
    precompile(Base._str_sizehint, (String,))
    precompile(Base._str_sizehint, (UInt32,))
    precompile(print, (Base.GenericIOBuffer{Array{UInt8, 1}}, String))
    precompile(print, (Base.GenericIOBuffer{Array{UInt8, 1}}, UInt32))
    precompile(join , (Base.GenericIOBuffer{Array{UInt8, 1}}, Array{Base.SubString{String}, 1}, String))
    precompile(join , (Base.GenericIOBuffer{Array{UInt8, 1}}, Array{String, 1}, Char))
    precompile(Base.showerror_nostdio, (Core.MissingCodeError, String))
    precompile(Base.VersionNumber, (UInt32, UInt32, UInt32, Tuple{}, Tuple{}))
    precompile(linsolve10, (Ptr{Float64}, Ptr{Float64}))
    precompile(Base.CoreLogging.shouldlog, (Base.CoreLogging.SimpleLogger, Base.CoreLogging.LogLevel, Module, Symbol, Symbol))
end
