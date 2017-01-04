# This file is a part of Julia. License is MIT: http://julialang.org/license

show(x) = show(STDOUT::IO, x)
print(xs...)   = print(STDOUT::IO, xs...)
println(xs...) = println(STDOUT::IO, xs...)
println(io::IO) = print(io, '\n')

immutable DevNullStream <: IO end
const DevNull = DevNullStream()
isreadable(::DevNullStream) = false
iswritable(::DevNullStream) = true
isopen(::DevNullStream) = true
read(::DevNullStream, ::Type{UInt8}) = throw(EOFError())
write(::DevNullStream, ::UInt8) = 1
close(::DevNullStream) = nothing
flush(::DevNullStream) = nothing
wait_connected(::DevNullStream) = nothing
wait_readnb(::DevNullStream) = wait()
wait_readbyte(::DevNullStream) = wait()
wait_close(::DevNullStream) = wait()
eof(::DevNullStream) = true

let CoreIO = Union{Core.CoreSTDOUT, Core.CoreSTDERR}
    global write, unsafe_write
    write(io::CoreIO, x::UInt8) = Core.write(io, x)
    unsafe_write(io::CoreIO, x::Ptr{UInt8}, nb::UInt) = Core.unsafe_write(io, x, nb)
end

STDIN = DevNull
STDOUT = Core.STDOUT
STDERR = Core.STDERR
