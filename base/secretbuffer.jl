"""
    Base.SecretBuffer()

An IOBuffer-like object where the contents will be securely wiped when garbage collected. However, it is
considered best practice to wipe the buffer using `Base.shred!(::SecretBuffer)` as soon as the
secure data are no longer required. Avoid initializing with converting to strings as they are
unable to be explicitly zeroed through mutation; when initializing with existing data the `SecretBuffer!`
function is recommended to securely zero the passed argument.

# Examples
```jldoctest
julia> s = Base.SecretBuffer()
SecretBuffer("*******")

julia> write(s, 's', 'e', 'c', 'r', 'e', 't')
6

julia> seek(s, 0); Char(read(s, UInt8))
's': ASCII/Unicode U+0073 (category Ll: Letter, lowercase)

julia> Base.shred!(s)
SecretBuffer("*******")

julia> eof(s)
true
```
"""
mutable struct SecretBuffer <: IO
    data::Vector{UInt8}
    size::Int
    ptr::Int

    function SecretBuffer(; sizehint=128)
        s = new(Vector{UInt8}(undef, sizehint), 0, 1)
        finalizer(final_shred!, s)
        return s
    end
end

convert(::Type{SecretBuffer}, s::AbstractString) = SecretBuffer(String(s))
SecretBuffer(str::AbstractString) = SecretBuffer(String(str))
function SecretBuffer(str::String)
    buf = unsafe_wrap(Vector{UInt8}, str)
    s = SecretBuffer(sizehint=length(buf))
    for c in buf
        write(s, c)
    end
    seek(s, 0)
    s
end

"""
    SecretBuffer!(data::Union{Cstring, Ptr{UInt8}, Vector{UInt8}})

Initialize a new `SecretBuffer` with `data` and securely zero the original source argument.
"""
SecretBuffer!(s::Cstring) = SecretBuffer!(convert(Ptr{UInt8}, s))
function SecretBuffer!(p::Ptr{UInt8})
    len = ccall(:strlen, Cint, (Ptr{UInt8},), p)
    s = SecretBuffer(sizehint=len)
    for i in 1:len
        write(s, unsafe_load(p, i))
    end
    seek(s, 0)
    unsafe_securezero!(p, len)
    s
end
function SecretBuffer!(d::Vector{UInt8})
    s = SecretBuffer(sizehint=length(d))
    for i in 1:len
        write(s, unsafe_load(p, i))
    end
    seek(s, 0)
    securezero!(d)
    s
end

show(io::IO, s::SecretBuffer) = print(io, "SecretBuffer(\"*******\")")

hash(s::SecretBuffer, h::UInt) = hash(SecretBuffer, hash((s.data, s.size, s.ptr), h))
==(s1::SecretBuffer, s2::SecretBuffer) = (view(s1.data, 1:s1.size), s1.ptr) == (view(s2.data, 1:s2.size), s2.ptr)

function write(io::SecretBuffer, b::UInt8)
    if io.ptr > length(io.data)
        # We need to resize! the array: do this manually to ensure no copies are left behind
        newdata = Vector{UInt8}(undef, (io.size+16)*2)
        copyto!(newdata, io.data)
        securezero!(io.data)
        io.data = newdata
    end
    io.size == io.ptr-1 && (io.size += 1)
    io.data[io.ptr] = b
    io.ptr += 1
    return 1
end

function write(io::IO, s::SecretBuffer)
    nb = 0
    for i in 1:s.size
        nb += write(io, s.data[i])
    end
    return nb
end

function unsafe_convert(::Type{Ptr{UInt8}}, s::SecretBuffer)
    # Add a hidden nul byte just past the end of the valid region
    p = s.ptr
    s.ptr = s.size + 1
    write(s, '\0')
    s.ptr = p
    s.size -= 1
    return unsafe_convert(Ptr{UInt8}, s.data)
end

seek(io::SecretBuffer, n::Integer) = (io.ptr = max(min(n+1, io.size+1), 1))
bytesavailable(io::SecretBuffer) = io.size - io.ptr + 1
position(io::SecretBuffer) = io.ptr-1
eof(io::SecretBuffer) = io.ptr > io.size
isempty(io::SecretBuffer) = io.size == 0
function peek(io::SecretBuffer)
    eof(io) && throw(EOFError())
    return io.data[io.ptr]
end
function read(io::SecretBuffer, ::Type{UInt8})
    eof(io) && throw(EOFError())
    byte = io.data[io.ptr]
    io.ptr += 1
    return byte
end

function final_shred!(s::SecretBuffer)
    if !isshredded(s)
        shred!(s)
    end
end

function shred!(s::SecretBuffer)
    securezero!(s.data)
    s.ptr = 1
    s.size = 0
    return s
end

isshredded(s::SecretBuffer) = all(iszero, s.data)

function shred!(f::Function, x)
    try
        f(x)
    finally
        shred!(x)
    end
    x
end
