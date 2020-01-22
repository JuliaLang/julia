# This file is a part of Julia. License is MIT: https://julialang.org/license

print(xs...)   = print(stdout::IO, xs...)
println(xs...) = println(stdout::IO, xs...)
println(io::IO) = print(io, '\n')

function show end
function repr end

struct DevNull <: IO end
const devnull = DevNull()
isreadable(::DevNull) = false
iswritable(::DevNull) = true
isopen(::DevNull) = true
read(::DevNull, ::Type{UInt8}) = throw(EOFError())
write(::DevNull, ::UInt8) = 1
unsafe_write(::DevNull, ::Ptr{UInt8}, n::UInt)::Int = n
close(::DevNull) = nothing
flush(::DevNull) = nothing
wait_readnb(::DevNull) = wait()
wait_close(::DevNull) = wait()
eof(::DevNull) = true

let CoreIO = Union{Core.CoreSTDOUT, Core.CoreSTDERR}
    global write, unsafe_write
    write(io::CoreIO, x::UInt8) = Core.write(io, x)
    unsafe_write(io::CoreIO, x::Ptr{UInt8}, nb::UInt) = Core.unsafe_write(io, x, nb)
end

stdin = devnull
stdout = Core.stdout
stderr = Core.stderr
