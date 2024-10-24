# This file is a part of Julia. License is MIT: https://julialang.org/license

print(xs...)   = print(stdout, xs...)
println(xs...) = print(stdout, xs..., '\n')  # 2-3 fewer allocations, this way, not calling println 
println(io::IO) = print(io, "\n")  # One less allocation than with '\n', though ok above

function show end
function repr end

struct DevNull <: IO end
const devnull = DevNull()
write(::DevNull, ::UInt8) = 1
unsafe_write(::DevNull, ::Ptr{UInt8}, n::UInt)::Int = n
closewrite(::DevNull) = nothing
close(::DevNull) = nothing
wait_close(::DevNull) = wait()
bytesavailable(io::DevNull) = 0

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

stdin::IO = devnull
stdout::IO = Core.stdout
stderr::IO = Core.stderr
