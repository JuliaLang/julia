# Bare-bones mmapped-array constructor
# This is needed for fancy stuff, such as MAP_ANONYMOUS.
function mmap_array{T,N}(::Type{T}, dims::NTuple{N,Int}, prot::Int, flags::Int, fd::Integer, offset::FileOffset)
    const pagesize::Int = 4096
    offset_page::FileOffset = ifloor(offset/pagesize)*pagesize
    len::Int = prod(dims)*sizeof(T) + offset - offset_page
    p = ccall(:mmap, Ptr{Void}, (Ptr{Void}, Int, Int, Int, Int, FileOffset), C_NULL, len, prot, flags, fd, offset_page)
    if convert(Int,p) < 1
        println("Memory mapping failed")
        error(strerror())
    end
    A = pointer_to_array(convert(Ptr{T},p), dims)
    finalizer(A,x->munmap(p,len))
    return A
end

# More user-friendly mmapped-array constructor
function mmap_array{T,N}(::Type{T}, dims::NTuple{N,Int}, s::IOStream, offset::FileOffset)
    const PROT_READ::Int = 1
    const PROT_WRITE::Int = 2
    const MAP_SHARED::Int = 1
    const F_GETFL::Int = 3
    # Get the stream's mode
    mode = ccall(:fcntl,Int,(Int,Int),fd(s),F_GETFL)
    mode = mode & 3
    if mode == 0
        prot = PROT_READ
    elseif mode == 1
        prot = PROT_WRITE
    else
        prot = PROT_READ | PROT_WRITE
    end
    flags = MAP_SHARED
    A = mmap_array(T, dims, prot, flags, fd(s), offset)
    return A
end
mmap_array{T,N}(::Type{T}, dims::NTuple{N,Int}, s::IOStream) = mmap_array(T, dims, s, position(s))

function munmap(p::Ptr,len::Int)
    ret = ccall(:munmap,Int,(Ptr{Void},Int),p,len)
    if ret != 0
        error(strerror())
    end
end
