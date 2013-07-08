### Generic interface ###

# Arrays
mmap_array{T,N}(::Type{T}, dims::NTuple{N,Int}, s::IO) = mmap_array(T, dims, s, position(s))

msync{T}(A::Array{T}) = msync(pointer(A), length(A)*sizeof(T))

# BitArrays
mmap_bitarray{N}(::Type{Bool}, dims::NTuple{N,Int}, s::IOStream, offset::FileOffset) =
    mmap_bitarray(dims, s, offset)
mmap_bitarray{N}(::Type{Bool}, dims::NTuple{N,Int}, s::IOStream) = mmap_bitarray(dims, s, position(s))
mmap_bitarray{N}(dims::NTuple{N,Int}, s::IOStream) = mmap_bitarray(dims, s, position(s))

msync(B::BitArray) = msync(pointer(B.chunks), length(B.chunks)*sizeof(Uint64))

### UNIX implementation ###

@unix_only begin
# Low-level routines
# These are needed for things like MAP_ANONYMOUS
function mmap(len::Integer, prot::Integer, flags::Integer, fd::Integer, offset::FileOffset)
    const pagesize::Int = ccall(:jl_getpagesize, Clong, ())
    # Check that none of the computations will overflow
    if len > typemax(Int)-pagesize
        error("Cannot map such a large buffer")
    end
    # Set the offset to a page boundary
    offset_page::FileOffset = ifloor(offset/pagesize)*pagesize
    len_page::Int = (offset-offset_page) + len
    # Mmap the file
    p = ccall(:mmap, Ptr{Void}, (Ptr{Void}, Csize_t, Cint, Cint, Cint, FileOffset), C_NULL, len_page, prot, flags, fd, offset_page)
    if int(p) == -1
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
function mmap_grow(len::Integer, prot::Integer, flags::Integer, fd::Integer, offset::FileOffset)
    const SEEK_SET::Cint = 0
    const SEEK_CUR::Cint = 1
    const SEEK_END::Cint = 2
    # Save current file position so we can restore it later
    cpos = ccall(:lseek, FileOffset, (Cint, FileOffset, Cint), fd, 0, SEEK_CUR)
    if cpos < 0
        error(strerror())
    end
    filelen = ccall(:lseek, FileOffset, (Cint, FileOffset, Cint), fd, 0, SEEK_END)
    if filelen < 0
        error(strerror())
    end
    if (filelen < offset + len)
        n = ccall(:pwrite, Cssize_t, (Cint, Ptr{Void}, Uint, FileOffset), fd, int8([0]), 1, offset + len - 1)
        if (n < 1)
            error(strerror())
        end
    end
    cpos = ccall(:lseek, FileOffset, (Cint, FileOffset, Cint), fd, cpos, SEEK_SET)
    return mmap(len, prot, flags, fd, offset)
end

function munmap(p::Ptr,len::Integer)
    ret = ccall(:munmap,Cint,(Ptr{Void},Int),p,len)
    if ret != 0
        error(strerror())
    end
end

const MS_ASYNC = 1
const MS_INVALIDATE = 2
const MS_SYNC = 4
function msync(p::Ptr, len::Integer, flags::Integer)
    ret = ccall(:msync, Cint, (Ptr{Void}, Csize_t, Cint), p, len, flags)
    if ret != 0
        error(strerror())
    end
end
msync(p::Ptr, len::Integer) = msync(p, len, MS_SYNC)

# Higher-level functions
# Determine a stream's read/write mode, and return prot & flags
# appropriate for mmap
# We could use isreadonly here, but it's worth checking that it's readable too
function mmap_stream_settings(s::IO)
    const PROT_READ::Cint = 1
    const PROT_WRITE::Cint = 2
    const MAP_SHARED::Cint = 1
    const F_GETFL::Cint = 3
    mode = ccall(:fcntl,Cint,(Cint,Cint),fd(s),F_GETFL)
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
function mmap_array{T,N}(::Type{T}, dims::NTuple{N,Int}, s::IO, offset::FileOffset)
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

# Mmapped-bitarray constructor
function mmap_bitarray{N}(dims::NTuple{N,Int}, s::IOStream, offset::FileOffset)
    prot, flags, iswrite = mmap_stream_settings(s)
    if length(dims) == 0
        dims = 0
    end
    n = prod(dims)
    nc = num_bit_chunks(n)
    chunks = mmap_array(Uint64, (nc,), s, offset)
    if iswrite
        chunks[end] &= @_msk_end n
    else
        if chunks[end] != chunks[end] & @_msk_end n
            error("The given file does not contain a valid BitArray of size ", join(dims, 'x'), " (open with r+ to override)")
        end
    end
    B = BitArray{N}(ntuple(N,i->0)...)
    B.chunks = chunks
    B.len = n
    if N != 1
        B.dims = Int[i for i in dims]
    end
    finalizer(B, x->munmap(pointer(B.chunks), length(B.chunks)*sizeof(Uint64)))
    return B
end
end


### Windows implementation ###
@windows_only begin
# Mmapped-array constructor
function mmap_array{T,N}(::Type{T}, dims::NTuple{N,Int}, s::IO, offset::FileOffset)
    shandle = _get_osfhandle(RawFD(fd(s)))
    ro = isreadonly(shandle)
    flprotect = ro ? 0x02 : 0x04
    szarray = convert(Csize_t, prod(dims))*sizeof(T)
    szfile = szarray + convert(Csize_t, offset)
    mmaphandle = ccall(:CreateFileMappingA, stdcall, Ptr{Void}, (Ptr{Void}, Ptr{Void}, Cint, Cint, Cint, Ptr{Void}), shandle.handle, C_NULL, flprotect, szfile>>32, szfile&0xffffffff, C_NULL)
    if mmaphandle == C_NULL
        error("Could not create file mapping")
    end
    access = ro ? 4 : 2
    viewhandle = ccall(:MapViewOfFile, stdcall, Ptr{Void}, (Ptr{Void}, Cint, Cint, Cint, Csize_t), mmaphandle, access, offset>>32, offset&0xffffffff, szarray)
    if viewhandle == C_NULL
        error("Could not create mapping view")
    end
    A = pointer_to_array(pointer(T, viewhandle), dims)
    finalizer(A, x->munmap(viewhandle))
    return A
end

function munmap(viewhandle::Ptr)
    status = bool(ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Void},), viewhandle))
    if !status
        error("Could not unmap view")
    end
end

function msync(p::Ptr, len::Integer)
    status = bool(ccall(:FlushViewOfFile, stdcall, Cint, (Ptr{Void}, Csize_t), p, len))
    if !status
        error("Could not msync")
    end
end

end
