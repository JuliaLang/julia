# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Low level module for mmap (memory mapping of files).
"""
module Mmap

import Base: OS_HANDLE, INVALID_OS_HANDLE
using Base.Sys: PAGESIZE

export mmap

# platform-specific mmap utilities
if Sys.isunix()

const PROT_READ     = Cint(1)
const PROT_WRITE    = Cint(2)
const MAP_SHARED    = Cint(1)
const MAP_PRIVATE   = Cint(2)
const MAP_ANONYMOUS = Cint(Sys.isbsd() ? 0x1000 : 0x20)
const F_GETFL       = Cint(3)

mutable struct Virtual <: IO
    name::String
    ios::Union{Nothing, IOStream}
    readonly::Bool
    create::Bool
    maxsize::Int
end

gethandle(io::Virtual) = fd(io.ios)
gethandle(io::IO) = fd(io)

Base.isopen(io::Virtual) = io.ios != INVALID_OS_HANDLE

# Determine a stream's read/write mode, and return prot & flags appropriate for mmap
function settings(s::RawFD, shared::Bool)
    flags = shared ? MAP_SHARED : MAP_PRIVATE
    if s == INVALID_OS_HANDLE
        flags |= MAP_ANONYMOUS
        prot = PROT_READ | PROT_WRITE
    else
        mode = ccall(:fcntl, Cint, (RawFD, Cint, Cint...), s, F_GETFL)
        systemerror("fcntl F_GETFL", mode == -1)
        mode = mode & 3
        prot = (mode == 0) ? PROT_READ : ((mode == 1) ? PROT_WRITE : (PROT_READ | PROT_WRITE))
        if prot & PROT_READ == 0
            throw(ArgumentError("mmap requires read permissions on the file (open with \"r+\" mode to override)"))
        end
    end
    return prot, flags, (prot & PROT_WRITE) == 0
end

# Before mapping, grow the file to sufficient size
# Note: a few mappable streams do not support lseek. When Julia
# supports structures in ccall, switch to fstat.
function grow!(io::IO, offset::Integer, len::Integer)
    pos = position(io)
    filelen = filesize(io)
    # If non-regular file skip trying to grow since we know that will fail the ftruncate syscall
    filelen == 0 && !isfile(io) && return
    if filelen < offset + len
        failure = ccall(:jl_ftruncate, Cint, (Cint, Int64), fd(io), offset+len)
        Base.systemerror(:ftruncate, failure != 0)
    end
    seek(io, pos)
    return
end

elseif Sys.iswindows()

const DWORD = Culong

const PAGE_READONLY          = DWORD(0x02)
const PAGE_READWRITE         = DWORD(0x04)
const PAGE_WRITECOPY         = DWORD(0x08)
const PAGE_EXECUTE_READ      = DWORD(0x20)
const PAGE_EXECUTE_READWRITE = DWORD(0x40)
const PAGE_EXECUTE_WRITECOPY = DWORD(0x80)
const FILE_MAP_COPY          = DWORD(0x01)
const FILE_MAP_WRITE         = DWORD(0x02)
const FILE_MAP_READ          = DWORD(0x04)
const FILE_MAP_EXECUTE       = DWORD(0x20)

mutable struct Virtual <: IO
    name::String
    handle::Ptr{Cvoid}
    readonly::Bool
    create::Bool
    maxsize::Int
end

gethandle(::Virtual) = INVALID_OS_HANDLE

function gethandle(io::IO)
    handle = Libc._get_osfhandle(RawFD(fd(io)))
    Base.windowserror(:mmap, handle == INVALID_OS_HANDLE)
    return handle
end

Base.isopen(io::Virtual) = io.handle != C_NULL

else
    error("mmap not defined for this OS")
end # os-test

function Base.open(::Type{Virtual}, name::AbstractString, size::Integer, mode::AbstractString)
    mode == "r"  ? open(Virtual, name, size; readonly = true,  create = false) :
    mode == "r+" ? open(Virtual, name, size; readonly = false, create = false) :
    mode == "w+" ? open(Virtual, name, size; readonly = false, create = true)  :
    throw(ArgumentError("invalid virtual open mode: $mode; must be \"r\", \"r+\", or \"w+\""))
end

Base.filesize(io::Virtual) = io.maxsize
Base.position(io::Virtual) = 0
Base.isfile(io::Virtual) = !isempty(io.name)
Base.isreadonly(io::Virtual) = io.readonly
Base.isreadable(io::Virtual) = true
Base.iswritable(io::Virtual) = !io.readonly

isanonymous(io::Virtual) = isempty(io.name)
isanonymous(::IO) = false

if Sys.isunix()

function Base.open(::Type{Virtual}, name::AbstractString, size::Integer;
    readonly :: Bool = true,
    create   :: Bool = false
)
    isempty(name) && !create &&
        throw(ArgumentError("Anonymous virtual files must have `create = true`."))
    size > 0 ||
        throw(ArgumentError("Size of virtual files must be greater than 0."))

    oflag = (readonly ? JL_O_RDONLY : JL_O_RDWR)
    if create
        oflag |= JL_O_CREAT
    end
    mode = S_IRUSR | S_IWUSR # Owner read/write
    fd_mem = shm_open(name, oflag, mode)
    systemerror(:shm_open, fd_mem < 0)

    io = Virtual(name, C_NULL, readonly, create, size)
    io.ios = fdio(fd_mem, true)

    if create
        # Set the size of the shared memory object
        # On OSX, ftruncate must be used to set size of segment, just lseek does not work.
        # And only at creation time.
        status = ccall(:jl_ftruncate, Cint, (Cint, Int64), fd_mem, size)
        systemerror(:ftruncate, status != 0)
    end

    return io
end

function Base.close(io::Virtual)
    if io.ios !== nothing
        rc = shm_unlink(io.name) # are both needed?
        systemerror("Error unlinking shmem segment " * io.name, rc != 0)
        close(io.ios)
        io.ios = nothing
    end
    return nothing
end

function shm_open(name, oflags, permissions)
    # On macOS, `shm_open()` is a variadic function, so to properly match
    # calling ABI, we must declare our arguments as variadic as well.
    @static if Sys.isapple()
        return ccall(:shm_open, Cint, (Cstring, Cint, Base.Cmode_t...), name, oflags, permissions)
    else
        return ccall(:shm_open, Cint, (Cstring, Cint, Base.Cmode_t), name, oflags, permissions)
    end
end

shm_unlink(name) = ccall(:shm_unlink, Cint, (Cstring,), name)

else # Sys.iswindows()
    
const ACTUAL_PAGESIZE = 4096

mutable struct SECURITY_ATTRIBUTES
    nLength::DWORD
    lpSecurityDescriptor::Ptr{Cvoid}
    bInheritHandle::Bool

    SECURITY_ATTRIBUTES(inherit_handle::Bool) = new(sizeof(SECURITY_ATTRIBUTES), C_NULL, inherit_handle)
end

split_size(size::Integer, i) =
    i == 1 ? size >> 32 :
    i == 2 ? size & typemax(UInt32) :
    throw(ArgumentError("invalid index $i for split_size; must be 1 or 2"))

function Base.open(::Type{Virtual}, name::AbstractString, size::Integer;
    readonly :: Bool = true,
    create   :: Bool = false
)
    isempty(name) && !create &&
        throw(ArgumentError("Anonymous virtual files must have `create = true`."))
    size > 0 ||
        throw(ArgumentError("Size of virtual files must be greater than 0."))

    io = Virtual(name, C_NULL, readonly, create, ((size - 1) ÷ ACTUAL_PAGESIZE + 1) * ACTUAL_PAGESIZE)
    if create
        io.handle = ccall(:CreateFileMappingW, stdcall, Ptr{Cvoid}, (OS_HANDLE, Ptr{Cvoid}, DWORD, DWORD, DWORD, Cwstring),
            INVALID_OS_HANDLE,                         # Backed by system paging file
            Ref(SECURITY_ATTRIBUTES(true)),            # Default security, can be inherited
            readonly ? PAGE_READONLY : PAGE_READWRITE, # Requested access mode
            split_size(size, 1), split_size(size, 2),  # High-order and low-order bits of size
            name                                       # Object name
        )
        Base.windowserror(:CreateFileMappingW, io.handle == C_NULL)
    else
        io.handle = ccall(:OpenFileMappingW, stdcall, Ptr{Cvoid}, (DWORD, Cint, Cwstring),
            readonly ? FILE_MAP_READ : FILE_MAP_WRITE, # Requested access mode
            true,                                      # Can be inherited
            name                                       # Object name
        )
        Base.windowserror(:OpenFileMappingW, io.handle == C_NULL)
    end

    return io
end

function Base.close(io::Virtual)
    if io.handle != C_NULL
        status = ccall(:CloseHandle, stdcall, Cint, (Ptr{Cvoid},), io.handle) != 0
        Base.windowserror(:CloseHandle, status == 0)
        io.handle = C_NULL
    end
    return nothing
end

end


# core implementation of mmap

"""
    mmap(io::Union{IOStream,AbstractString,Mmap.AnonymousMmap}[, type::Type{Array{T,N}}, dims, offset]; grow::Bool=true, shared::Bool=true)
    mmap(type::Type{Array{T,N}}, dims)

Create an `Array` whose values are linked to a file, using memory-mapping. This provides a
convenient way of working with data too large to fit in the computer's memory.

The type is an `Array{T,N}` with a bits-type element of `T` and dimension `N` that
determines how the bytes of the array are interpreted. Note that the file must be stored in
binary format, and no format conversions are possible (this is a limitation of operating
systems, not Julia).

`dims` is a tuple or single [`Integer`](@ref) specifying the size or length of the array.

The file is passed via the stream argument, either as an open [`IOStream`](@ref) or filename string.
When you initialize the stream, use `"r"` for a "read-only" array, and `"w+"` to create a
new array used to write values to disk.

If no `type` argument is specified, the default is `Vector{UInt8}`.

Optionally, you can specify an offset (in bytes) if, for example, you want to skip over a
header in the file. The default value for the offset is the current stream position for an
`IOStream`.

The `grow` keyword argument specifies whether the disk file should be grown to accommodate
the requested size of array (if the total file size is < requested array size). Write
privileges are required to grow the file.

The `shared` keyword argument specifies whether the resulting `Array` and changes made to it
will be visible to other processes mapping the same file.

For example, the following code

```julia
# Create a file for mmapping
# (you could alternatively use mmap to do this step, too)
using Mmap
A = rand(1:20, 5, 30)
s = open("/tmp/mmap.bin", "w+")
# We'll write the dimensions of the array as the first two Ints in the file
write(s, size(A,1))
write(s, size(A,2))
# Now write the data
write(s, A)
close(s)

# Test by reading it back in
s = open("/tmp/mmap.bin")   # default is read-only
m = read(s, Int)
n = read(s, Int)
A2 = mmap(s, Matrix{Int}, (m,n))
```

creates a `m`-by-`n` `Matrix{Int}`, linked to the file associated with stream `s`.

A more portable file would need to encode the word size -- 32 bit or 64 bit -- and endianness
information in the header. In practice, consider encoding binary data using standard formats
like HDF5 (which can be used with memory-mapping).
"""
function mmap(io::IO,
              ::Type{Array{T,N}}=Vector{UInt8},
              dims::NTuple{N,Integer}=(div(filesize(io)-position(io),Base.aligned_sizeof(T)),),
              offset::Integer=position(io); grow::Bool=true, shared::Bool=true) where {T,N}
    # check inputs
    isopen(io) || throw(ArgumentError("$io must be open to mmap"))
    isbitstype(T)  || throw(ArgumentError("unable to mmap $T; must satisfy isbitstype(T) == true"))

    len = Base.aligned_sizeof(T)
    for l in dims
        len, overflow = Base.Checked.mul_with_overflow(promote(len, l)...)
        overflow && throw(ArgumentError("requested size prod($((len, dims...))) too large, would overflow typeof(size(T)) == $(typeof(len))"))
    end
    len >= 0 || throw(ArgumentError("requested size must be ≥ 0, got $len"))
    len == 0 && return Array{T}(undef, dims)
    len < typemax(Int) - PAGESIZE || throw(ArgumentError("requested size must be < $(typemax(Int)-PAGESIZE), got $len"))

    offset >= 0 || throw(ArgumentError("requested offset must be ≥ 0, got $offset"))

    # shift `offset` to start of page boundary
    offset_page::Int64 = div(offset, PAGESIZE) * PAGESIZE
    # add (offset - offset_page) to `len` to get total length of memory-mapped region
    mmaplen = (offset - offset_page) + len

    file_desc = gethandle(io)
    szfile = convert(Csize_t, len + offset)
    requestedSizeLarger = false
    # platform-specific mmapping
    @static if Sys.isunix()
        prot, flags, readonly = settings(file_desc, shared)
        if requestedSizeLarger
            if readonly
                throw(ArgumentError("unable to increase file size to $szfile due to read-only permissions"))
            elseif !grow
                throw(ArgumentError("requested size $szfile larger than file size $(filesize(io)), but requested not to grow"))
            elseif isanonymous(io)
                throw(ArgumentError("unable to increase size of anonymous memory beyond $(filesize(io))"))
            else
                grow!(io, offset, len)
            end
        end
        # mmap the file
        ptr = ccall(:jl_mmap, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t, Cint, Cint, RawFD, Int64),
            C_NULL, mmaplen, prot, flags, file_desc, offset_page)
        systemerror("memory mapping failed", reinterpret(Int, ptr) == -1)
    else
        readonly = isreadonly(io)
        if requestedSizeLarger
            if readonly
                throw(ArgumentError("unable to increase file size to $szfile due to read-only permissions"))
            elseif !grow
                throw(ArgumentError("requested size $szfile larger than file size $(filesize(io)), but requested not to grow"))
            elseif io isa Virtual
                throw(ArgumentError("unable to increase size of virtual file beyond $(filesize(io))"))
            end
        end
        if !(io isa Virtual)
            handle = ccall(:CreateFileMappingW, stdcall, Ptr{Cvoid}, (OS_HANDLE, Ptr{Cvoid}, DWORD, DWORD, DWORD, Cwstring),
                file_desc,                                    # File to map
                Ref(SECURITY_ATTRIBUTES(true)),               # Default security, can be inherited
                readonly ? PAGE_READONLY : PAGE_READWRITE,    # Requested access mode
                split_size(szfile, 1), split_size(szfile, 2), # High-order and low-order bits of size
                ""                                            # Object name
            )
            Base.windowserror(:CreateFileMappingW, handle == C_NULL)
        else
            handle = io.handle
        end
        ptr = ccall(:MapViewOfFile, stdcall, Ptr{Cvoid}, (Ptr{Cvoid}, DWORD, DWORD, DWORD, Csize_t),
            handle,                                                 # Handle to mapping object
            readonly ? FILE_MAP_READ : FILE_MAP_WRITE,              # Requested access mode
            split_size(offset_page, 1), split_size(offset_page, 2), # High-order and low-order bits of offset
            mmaplen                                                 # Number of bytes to map
        )
        Base.windowserror(:MapViewOfFile, ptr == C_NULL)
        if !(io isa Virtual)
            status = ccall(:CloseHandle, stdcall, Cint, (Ptr{Cvoid},), handle) != 0
            Base.windowserror(:CloseHandle, status == 0)
        end
    end # os-test

    # convert mmapped region to Julia Array at `ptr + (offset - offset_page)` since file was mapped at offset_page
    A = unsafe_wrap(Array, convert(Ptr{T}, UInt(ptr) + UInt(offset - offset_page)), dims)
    finalizer(A.ref.mem) do x
        @static if Sys.isunix()
            status = ccall(:munmap, Cint, (Ptr{Cvoid}, Int), ptr, mmaplen)
            systemerror(:munmap, status != 0)     
        else
            status = ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Cvoid},), ptr)
            Base.windowserror(:UnmapViewOfFile, status == 0)
        end
    end
    return A
end

mmap(file::AbstractString,
     ::Type{T}=Vector{UInt8},
     dims::NTuple{N,Integer}=(div(filesize(file),Base.aligned_sizeof(eltype(T))),),
     offset::Integer=Int64(0); grow::Bool=true, shared::Bool=true) where {T<:Array,N} =
    open(io->mmap(io, T, dims, offset; grow, shared), file, isfile(file) ? "r" : "w+")::Array{eltype(T),N}

# using a length argument instead of dims
mmap(io::IO, ::Type{T}, len::Integer, offset::Integer=position(io); grow::Bool=true, shared::Bool=true) where {T<:Array} =
    mmap(io, T, (len,), offset; grow=grow, shared=shared)
mmap(file::AbstractString, ::Type{T}, len::Integer, offset::Integer=Int64(0); grow::Bool=true, shared::Bool=true) where {T<:Array} =
    open(io->mmap(io, T, (len,), offset; grow, shared), file, isfile(file) ? "r" : "w+")::Vector{eltype(T)}
    # On Windows, 

# constructors for non-file-backed, unnamed (anonymous) mmaps
mmap(::Type{T}, dims::NTuple{N,Integer}; shared::Bool=true) where {T <: Array, N} =
    open(io -> mmap(io, T, dims, Int64(0); shared), Virtual, "", prod(dims) * Base.aligned_sizeof(eltype(T)), "w+")
mmap(::Type{T}, i::Integer...; shared::Bool=true) where {T <: Array} = mmap(T, convert(Tuple{Vararg{Int}}, i); shared)

"""
    mmap(io, BitArray, [dims, offset])

Create a [`BitArray`](@ref) whose values are linked to a file, using memory-mapping; it has the same
purpose, works in the same way, and has the same arguments, as [`mmap`](@ref mmap), but
the byte representation is different.

# Examples
```jldoctest
julia> using Mmap

julia> io = open("mmap.bin", "w+");

julia> B = mmap(io, BitArray, (25,30000));

julia> B[3, 4000] = true;

julia> Mmap.sync!(B);

julia> close(io);

julia> io = open("mmap.bin", "r+");

julia> C = mmap(io, BitArray, (25,30000));

julia> C[3, 4000]
true

julia> C[2, 4000]
false

julia> close(io)

julia> rm("mmap.bin")
```
This creates a 25-by-30000 `BitArray`, linked to the file associated with stream `io`.
"""
function mmap(io::IOStream, ::Type{<:BitArray}, dims::NTuple{N,Integer},
              offset::Int64=position(io); grow::Bool=true, shared::Bool=true) where N
    n = prod(dims)
    nc = Base.num_bit_chunks(n)
    chunks = mmap(io, Vector{UInt64}, (nc,), offset; grow=grow, shared=shared)
    if !isreadonly(io)
        chunks[end] &= Base._msk_end(n)
    else
        if chunks[end] != chunks[end] & Base._msk_end(n)
            throw(ArgumentError("the given file does not contain a valid BitArray of size $(join(dims, 'x')) (open with \"r+\" mode to override)"))
        end
    end
    B = BitArray{N}(undef, ntuple(i->0,Val(N))...)
    B.chunks = chunks
    B.len = n
    if N != 1
        B.dims = dims
    end
    return B
end

mmap(file::AbstractString, ::Type{T}, dims::NTuple{N,Integer}, offset::Integer=Int64(0);grow::Bool=true, shared::Bool=true) where {T<:BitArray,N} =
    open(io->mmap(io, T, dims, offset; grow=grow, shared=shared), file, isfile(file) ? "r" : "w+")::BitArray{N}

# using a length argument instead of dims
mmap(io::IO, ::Type{T}, len::Integer, offset::Integer=position(io); grow::Bool=true, shared::Bool=true) where {T<:BitArray} =
    mmap(io, T, (len,), offset; grow=grow, shared=shared)
mmap(file::AbstractString, ::Type{T}, len::Integer, offset::Integer=Int64(0); grow::Bool=true, shared::Bool=true) where {T<:BitArray} =
    open(io->mmap(io, T, (len,), offset; grow=grow, shared=shared), file, isfile(file) ? "r" : "w+")::BitVector

# constructors for non-file-backed (anonymous) mmaps
mmap(::Type{T}, dims::NTuple{N,Integer}; shared::Bool=true) where {T<:BitArray,N} = mmap(Anonymous(), T, dims, Int64(0); shared=shared)
mmap(::Type{T}, i::Integer...; shared::Bool=true) where {T<:BitArray} = mmap(Anonymous(), T, convert(Tuple{Vararg{Int}},i), Int64(0); shared=shared)

# msync flags for unix
const MS_ASYNC = 1
const MS_INVALIDATE = 2
const MS_SYNC = 4

"""
    Mmap.sync!(array)

Forces synchronization between the in-memory version of a memory-mapped `Array` or
[`BitArray`](@ref) and the on-disk version.
"""
function sync!(m::Array, flags::Integer=MS_SYNC)
    ptr = pointer(m)
    offset = rem(UInt(ptr), PAGESIZE)
    ptr = ptr - offset
    mmaplen = sizeof(m) + offset
    GC.@preserve m @static if Sys.isunix()
        systemerror("msync",
                    ccall(:msync, Cint, (Ptr{Cvoid}, Csize_t, Cint), ptr, mmaplen, flags) != 0)
    else
        Base.windowserror(:FlushViewOfFile,
            ccall(:FlushViewOfFile, stdcall, Cint, (Ptr{Cvoid}, Csize_t), ptr, mmaplen) == 0)
    end
end
sync!(B::BitArray, flags::Integer=MS_SYNC) = sync!(B.chunks, flags)

@static if Sys.isunix()
const MADV_NORMAL = 0
const MADV_RANDOM = 1
const MADV_SEQUENTIAL = 2
const MADV_WILLNEED = 3
const MADV_DONTNEED = 4
if Sys.islinux()
    const MADV_FREE = 8
    const MADV_REMOVE = 9
    const MADV_DONTFORK = 10
    const MADV_DOFORK = 11
    const MADV_MERGEABLE = 12
    const MADV_UNMERGEABLE = 13
    const MADV_HUGEPAGE = 14
    const MADV_NOHUGEPAGE = 15
    const MADV_DONTDUMP = 16
    const MADV_DODUMP = 17
    const MADV_WIPEONFORK = 18
    const MADV_KEEPONFORK = 19
    const MADV_COLD = 20
    const MADV_PAGEOUT = 21
    const MADV_HWPOISON = 100
    const MADV_SOFT_OFFLINE = 101
elseif Sys.isapple()
    const MADV_FREE = 5
elseif Sys.isfreebsd() || Sys.isdragonfly()
    const MADV_FREE = 5
    const MADV_NOSYNC = 6
    const MADV_AUTOSYNC = 7
    const MADV_NOCORE = 8
    const MADV_CORE = 9
    if Sys.isfreebsd()
        const MADV_PROTECT = 10
    else
        const MADV_INVAL = 10
        const MADV_SETMAP = 11
    end
elseif Sys.isopenbsd() || Sys.isnetbsd()
    const MADV_SPACEAVAIL = 5
    const MADV_FREE = 6
end

"""
    Mmap.madvise!(array, flag::Integer = Mmap.MADV_NORMAL)

Advises the kernel on the intended usage of the memory-mapped `array`, with the intent
`flag` being one of the available `MADV_*` constants.
"""
function madvise!(m::Array, flag::Integer=MADV_NORMAL)
    ptr = pointer(m)
    offset = rem(UInt(ptr), PAGESIZE)
    ptr = ptr - offset
    mmaplen = sizeof(m) + offset
    GC.@preserve m begin
        systemerror("madvise",
                    ccall(:madvise, Cint, (Ptr{Cvoid}, Csize_t, Cint), ptr, mmaplen, flag) != 0)
    end
end
madvise!(B::BitArray, flag::Integer=MADV_NORMAL) = madvise!(B.chunks, flag)
end # Sys.isunix()

end # module
