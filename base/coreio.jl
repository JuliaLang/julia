# This file is a part of Julia. License is MIT: https://julialang.org/license

print(xs...)   = print(stdout::IO, xs...)
println(xs...) = println(stdout::IO, xs...)
println(io::IO) = print(io, '\n')

function show end
function repr end

struct DevNull <: IO end
const devnull = DevNull()
write(::DevNull, ::UInt8) = 1
unsafe_write(::DevNull, ::Ptr{UInt8}, n::UInt)::Int = n
close(::DevNull) = nothing
wait_close(::DevNull) = wait()

let CoreIO = Union{Core.CoreSTDOUT, Core.CoreSTDERR}
    global write(io::CoreIO, x::UInt8) = Core.write(io, x)
    global unsafe_write(io::CoreIO, x::Ptr{UInt8}, nb::UInt) = Core.unsafe_write(io, x, nb)

    CoreIO = Union{CoreIO, DevNull}
    global read(::CoreIO, ::Type{UInt8}) = throw(EOFError())
    global isopen(::CoreIO) = true
    global isreadable(::CoreIO) = false
    global iswritable(::CoreIO) = true
    global flush(::CoreIO) = nothing
    global eof(::CoreIO) = true
    global wait_readnb(::CoreIO, nb::Int) = nothing
end

stdin = devnull
stdout = Core.stdout
stderr = Core.stderr
