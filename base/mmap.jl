# Low-level routines
# These are needed for things like MAP_ANONYMOUS
function mmap(len::Int, prot::Int, flags::Int, fd::Int, offset::FileOffset)
    const pagesize::Int = 4096
    # Check that none of the computations will overflow
    if len > typemax(Int)-pagesize
        error("Cannot map such a large buffer")
    end
    # Set the offset to a page boundary
    offset_page::FileOffset = ifloor(offset/pagesize)*pagesize
    len_page::Int = (offset-offset_page) + len
    # Mmap the file
    p = ccall(:mmap, Ptr{Void}, (Ptr{Void}, Int, Int, Int, Int, FileOffset), C_NULL, len_page, prot, flags, fd, offset_page)
    if convert(Int,p) < 1
        error("Memory mapping failed", strerror())
    end
    # Also return a pointer that compensates for any adjustment in the offset
    return p, int(offset-offset_page)
end

# Before mapping, grow the file to sufficient size
# (Required if you're going to write to a new memory-mapped file)
#
# Note: a few mappable streams do not support lseek. When Julia
# supports structures in ccall, switch to fstat.
function mmap_grow(len::Int, prot::Int, flags::Int, fd::Int, offset::FileOffset)
    const SEEK_SET::Int = 0
    const SEEK_CUR::Int = 1
    const SEEK_END::Int = 2
    # Save current file position so we can restore it later
    cpos = ccall(:lseek, FileOffset, (Int, FileOffset, Int), fd, 0, SEEK_CUR)
    if cpos < 0
        error(strerror())
    end
    filelen = ccall(:lseek, FileOffset, (Int, FileOffset, Int), fd, 0, SEEK_END)
    if filelen < 0
        error(strerror())
    end
    if (filelen < offset + len)
        n = ccall(:pwrite, Int, (Int, Ptr{Void}, Int, FileOffset), fd, int8([0]), 1, offset + len - 1)
        if (n < 1)
            error(strerror())
        end
    end
    cpos = ccall(:lseek, FileOffset, (Int, FileOffset, Int), fd, cpos, SEEK_SET)
    return mmap(len, prot, flags, fd, offset)
end

function munmap(p::Ptr,len::Int)
    ret = ccall(:munmap,Int,(Ptr{Void},Int),p,len)
    if ret != 0
        error(strerror())
    end
end

const MS_ASYNC = 1
const MS_INVALIDATE = 2
const MS_SYNC = 4
function msync(p::Ptr, len::Int, flags::Int)
    ret = ccall(:msync, Int, (Ptr{Void}, Int, Int), p, len, flags)
    if ret != 0
        error(strerror())
    end
end    

# Higher-level functions
# Determine a stream's read/write mode, and return prot & flags
# appropriate for mmap
function mmap_stream_settings(s::IOStream)
    const PROT_READ::Int = 1
    const PROT_WRITE::Int = 2
    const MAP_SHARED::Int = 1
    const F_GETFL::Int = 3
    mode = ccall(:fcntl,Int,(Int,Int),fd(s),F_GETFL)
    mode = mode & 3
    if mode == 0
        prot = PROT_READ
    elseif mode == 1
        prot = PROT_WRITE
    else
        prot = PROT_READ | PROT_WRITE
    end
    if prot & PROT_READ == 0
        error("For mmap, you need read permissions on the file (choose r+)")
    end
    flags = MAP_SHARED
    return prot, flags, (prot & PROT_WRITE) > 0
end

# Mmapped-array constructor
function mmap_array{T,N}(::Type{T}, dims::NTuple{N,Int}, s::IOStream, offset::FileOffset)
    prot, flags, iswrite = mmap_stream_settings(s)
    len = prod(dims)*sizeof(T)
    if iswrite
        pmap, delta = mmap_grow(len, prot, flags, fd(s), offset)
    else
        pmap, delta = mmap(len, prot, flags, fd(s), offset)
    end
    A = pointer_to_array(pointer(T, uint(pmap)+delta), dims)
    finalizer(A,x->munmap(pmap,len+delta))
    return A
end
mmap_array{T,N}(::Type{T}, dims::NTuple{N,Int}, s::IOStream) = mmap_array(T, dims, s, position(s))
msync{T}(A::Array{T}, flags::Int) = msync(pointer(A), prod(size(A))*sizeof(T), flags)
msync{T}(A::Array{T}) = msync(A,MS_SYNC)
