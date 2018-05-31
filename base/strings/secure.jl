"""
    SecureString(string::AbstractString)

A string where the contents will be securely wiped when garbage collected. However, it is
considered best practise to wipe the string using `shred!(::SecureString)` as soon as the
secure data is no longer required. Note that when the parameter is of type `Vector{UInt8}`
then the memory of the passed in parameter will also be securely wiped.

# Examples
```jldoctest
julia> str = "abc"::String
"abc"

julia> s = SecureString(str)
"abc"

julia> shred!(s)
"\0\0\0"

julia> str
"abc"
```
"""
mutable struct SecureString <: IO
    data::Vector{UInt8}
    size::Int
    ptr::Int

    function SecureString(; sizehint=128)
        s = new(Vector{UInt8}(undef, sizehint), 0, 1)
        finalizer(final_shred!, s)
        return s
    end
end

function SecureString!(p::Ptr{UInt8})
    # Copy into our own Vector and zero the source
    len = ccall(:strlen, Cint, (Ptr{UInt8},), p)
    s = SecureString(sizehint=len)
    for i in 1:len
        write(s, unsafe_load(p, i))
    end
    seek(s, 0)
    s
end

convert(::Type{SecureString}, s::AbstractString) = SecureString(String(s))
SecureString(str::AbstractString) = SecureString(String(str))
function SecureString(str::String)
    buf = unsafe_wrap(Vector{UInt8}, str)
    s = SecureString(sizehint=length(buf))
    for c in buf
        write(s, c)
    end
    seek(s, 0)
    s
end

show(io::IO, s::SecureString) = print(io, "SecureString(\"*******\")")

hash(s::SecureString, h::UInt) = hash(SecureString, hash((s.data, s.size, s.ptr), h))
==(s1::SecureString, s2::SecureString) = (view(s1.data, 1:s1.size), s1.ptr) == (view(s2.data, 1:s2.size), s2.ptr)

function write(io::SecureString, b::UInt8)
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

function write(io::IO, s::SecureString)
    nb = 0
    for i in 1:s.size
        nb += write(io, s.data[i])
    end
    return nb
end

function unsafe_convert(::Type{Ptr{UInt8}}, s::SecureString)
    # Add a hidden nul byte just past the end of the valid region
    p = s.ptr
    s.ptr = s.size + 1
    write(s, '\0')
    s.ptr = p
    s.size -= 1
    return unsafe_convert(Ptr{UInt8}, s.data)
end

seek(io::SecureString, n::Integer) = (io.ptr = max(min(n+1, io.size+1), 1))
bytesavailable(io::SecureString) = io.size - io.ptr + 1
position(io::SecureString) = io.ptr-1
eof(io::SecureString) = io.ptr > io.size
isempty(io::SecureString) = io.size == 0
function peek(io::SecureString)
    eof(io) && throw(EOFError())
    return io.data[io.ptr]
end
function read(io::SecureString, ::Type{UInt8})
    eof(io) && throw(EOFError())
    byte = io.data[io.ptr]
    io.ptr += 1
    return byte
end

function final_shred!(s::SecureString)
    if !isshredded(s)
        shred!(s)
    end
end

function shred!(s::SecureString)
    securezero!(s.data)
    s.ptr = 1
    s.size = 0
    return s
end

isshredded(s::SecureString) = all(iszero, s.data)

function shred!(f::Function, x)
    try
        f(x)
    finally
        shred!(x)
    end
    x
end
