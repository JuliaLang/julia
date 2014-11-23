### Generic interface ###

# Arrays
mmap_array{T,N}(::Type{T}, dims::NTuple{N,Integer}, s::IO) = mmap_array(T, dims, s, position(s))

msync{T}(A::Array{T}) = msync(pointer(A), length(A)*sizeof(T))

# BitArrays
mmap_bitarray{N}(::Type{Bool}, dims::NTuple{N,Integer}, s::IOStream, offset::FileOffset) =
    mmap_bitarray(dims, s, offset)
mmap_bitarray{N}(::Type{Bool}, dims::NTuple{N,Integer}, s::IOStream) = mmap_bitarray(dims, s, position(s))
mmap_bitarray{N}(dims::NTuple{N,Integer}, s::IOStream) = mmap_bitarray(dims, s, position(s))

msync(B::BitArray) = msync(pointer(B.chunks), length(B.chunks)*sizeof(UInt64))

### UNIX implementation ###

@unix_only begin
# Low-level routines
# These are needed for things like MAP_ANONYMOUS
function mmap(len::Integer, prot::Integer, flags::Integer, fd, offset::Integer)
    const pagesize::Int = ccall(:jl_getpagesize, Clong, ())
    # Check that none of the computations will overflow
    if len < 0
        error("requested size is negative")
    end
    if len > typemax(Int)-pagesize
        error("requested size is too large")
    end
    # Set the offset to a page boundary
    offset_page::FileOffset = floor(Integer,offset/pagesize)*pagesize
    len_page::Int = (offset-offset_page) + len
    # Mmap the file
    p = ccall(:jl_mmap, Ptr{Void}, (Ptr{Void}, Csize_t, Cint, Cint, Cint, FileOffset), C_NULL, len_page, prot, flags, fd, offset_page)
    systemerror("memory mapping failed", reinterpret(Int,p) == -1)
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
    cpos = ccall(:jl_lseek, FileOffset, (Cint, FileOffset, Cint), fd, 0, SEEK_CUR)
    systemerror("lseek", cpos < 0)
    filelen = ccall(:jl_lseek, FileOffset, (Cint, FileOffset, Cint), fd, 0, SEEK_END)
    systemerror("lseek", filelen < 0)
    if (filelen < offset + len)
        systemerror("pwrite", ccall(:jl_pwrite, Cssize_t, (Cint, Ptr{Void}, UInt, FileOffset), fd, int8([0]), 1, offset + len - 1) < 1)
    end
    cpos = ccall(:jl_lseek, FileOffset, (Cint, FileOffset, Cint), fd, cpos, SEEK_SET)
    systemerror("lseek", cpos < 0)
    return mmap(len, prot, flags, fd, offset)
end

function munmap(p::Ptr,len::Integer)
    systemerror("munmap", ccall(:munmap,Cint,(Ptr{Void},Int),p,len) != 0)
end

const MS_ASYNC = 1
const MS_INVALIDATE = 2
const MS_SYNC = 4
function msync(p::Ptr, len::Integer, flags::Integer)
    systemerror("msync", ccall(:msync, Cint, (Ptr{Void}, Csize_t, Cint), p, len, flags) != 0)
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
    systemerror("fcntl F_GETFL", mode == -1)
    mode = mode & 3
    if mode == 0
        prot = PROT_READ
    elseif mode == 1
        prot = PROT_WRITE
    else
        prot = PROT_READ | PROT_WRITE
    end
    if prot & PROT_READ == 0
        error("mmap requires read permissions on the file (choose r+)")
    end
    flags = MAP_SHARED
    return prot, flags, (prot & PROT_WRITE) > 0
end

# Mmapped-array constructor
function mmap_array{T,N}(::Type{T}, dims::NTuple{N,Integer}, s::IO, offset::FileOffset; grow::Bool=true)
    prot, flags, iswrite = mmap_stream_settings(s)
    len = prod(dims)*sizeof(T)
    if len > typemax(Int)
        error("file is too large to memory-map on this platform")
    end
    if iswrite && grow
        pmap, delta = mmap_grow(len, prot, flags, fd(s), offset)
    else
        pmap, delta = mmap(len, prot, flags, fd(s), offset)
    end
    A = pointer_to_array(convert(Ptr{T}, uint(pmap)+delta), dims)
    finalizer(A,x->munmap(pmap,len+delta))
    return A
end

end

### Windows implementation ###

@windows_only type SharedMemSpec
    name :: AbstractString
    readonly :: Bool
    create :: Bool
end

@windows_only begin
# Mmapped-array constructor
function mmap_array{T,N}(::Type{T}, dims::NTuple{N,Integer}, s::Union(IO,SharedMemSpec), offset::FileOffset)
    if isa(s,IO)
        hdl = _get_osfhandle(RawFD(fd(s))).handle
        if int(hdl) == -1
            error("could not get handle for file to map: $(FormatMessage())")
        end
        name = C_NULL
        ro = isreadonly(s)
        create = true
    else
        # shared memory
        hdl = -1
        name = utf16(s.name)
        ro = s.readonly
        create = s.create
    end
    len = prod(dims)*sizeof(T)
    const granularity::Int = ccall(:jl_getallocationgranularity, Clong, ())
    if len < 0
        error("requested size is negative")
    end
    if len > typemax(Int)-granularity
        error("file is too large to memory-map on this platform")
    end
    # Set the offset to a page boundary
    offset_page::FileOffset = div(offset, granularity)*granularity
    szfile = convert(Csize_t, len + offset)
    szarray = szfile - convert(Csize_t, offset_page)
    access = ro ? 4 : 2
    if create
        flprotect = ro ? 0x02 : 0x04
        mmaphandle = ccall(:CreateFileMappingW, stdcall, Ptr{Void}, (Cptrdiff_t, Ptr{Void}, Cint, Cint, Cint, Ptr{UInt16}),
            hdl, C_NULL, flprotect, szfile>>32, szfile&typemax(UInt32), name)
    else
        mmaphandle = ccall(:OpenFileMappingW, stdcall, Ptr{Void}, (Cint, Cint, Ptr{UInt16}),
            access, true, name)
    end
    if mmaphandle == C_NULL
        error("could not create file mapping: $(FormatMessage())")
    end
    viewhandle = ccall(:MapViewOfFile, stdcall, Ptr{Void}, (Ptr{Void}, Cint, Cint, Cint, Csize_t),
        mmaphandle, access, offset_page>>32, offset_page&typemax(UInt32), szarray)
    if viewhandle == C_NULL
        error("could not create mapping view: $(FormatMessage())")
    end
    A = pointer_to_array(convert(Ptr{T}, viewhandle+offset-offset_page), dims)
    finalizer(A, x->munmap(viewhandle, mmaphandle))
    return A
end

function munmap(viewhandle::Ptr, mmaphandle::Ptr)
    status = bool(ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Void},), viewhandle))
    status |= bool(ccall(:CloseHandle, stdcall, Cint, (Ptr{Void},), mmaphandle))
    if !status
        error("could not unmap view: $(FormatMessage())")
    end
end

function msync(p::Ptr, len::Integer)
    status = bool(ccall(:FlushViewOfFile, stdcall, Cint, (Ptr{Void}, Csize_t), p, len))
    if !status
        error("could not msync: $(FormatMessage())")
    end
end

end

# Mmapped-bitarray constructor
function mmap_bitarray{N}(dims::NTuple{N,Integer}, s::IOStream, offset::FileOffset)
    iswrite = !isreadonly(s)
    n = 1
    for d in dims
        if d < 0
            error("invalid dimension size")
        end
        n *= d
    end
    nc = num_bit_chunks(n)
    if nc > typemax(Int)
        error("file is too large to memory-map on this platform")
    end
    chunks = mmap_array(UInt64, (nc,), s, offset)
    if iswrite
        chunks[end] &= @_msk_end n
    else
        if chunks[end] != chunks[end] & @_msk_end n
            error("the given file does not contain a valid BitArray of size ", join(dims, 'x'), " (open with \"r+\" mode to override)")
        end
    end
    B = BitArray{N}(ntuple(N,i->0)...)
    B.chunks = chunks
    B.len = n
    if N != 1
        B.dims = dims
    end
    return B
end
