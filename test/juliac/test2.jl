#!/usr/bin/env -S julia --project=@scriptdir

module Main2

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

Base.@ccallable function main() :: Cvoid
    # println("Hello, world!")
    task = current_task()
    task.rngState0 = 0x5156087469e170ab
    task.rngState1 = 0x7431eaead385992c
    task.rngState2 = 0x503e1d32781c2608
    task.rngState3 = 0x3a77f7189200c20b
    task.rngState4 = 0x5502376d099035ae
    a = rand(10)
    b = sum(a)
    ccall(:printf, Int32, (Ptr{UInt8},Float64...), "hello_world %lf", b)
    take_heap_snapshot()
    return nothing
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