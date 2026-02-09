# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Low level module for mmap (memory mapping of files).
"""
module Mmap

import Base: OS_HANDLE, INVALID_OS_HANDLE, IOError
import Base.Filesystem: JL_O_CREAT, JL_O_RDONLY, JL_O_RDWR, JL_O_EXCL, S_IRUSR, S_IWUSR
using Base.Sys: PAGESIZE

export mmap

"""
    Mmap.SharedMemory

A `IO`-like object representing a shared memory region. It is used by `mmap` to support memory-mapping of shared memory.

This type should not be constructed directly; instead, use `open(SharedMemory, name, size; readonly, create)`, which opens
a shared memory region and returns the corresponding `SharedMemory` object.

If `name` is empty, the region will be anonymous. If a name is provided and `create` is `true`, the attempt to open will
fail if a shared memory region with the same name already exists.

Shared memory size cannot be grown after creation, though smaller regions can be mmapped within it.
"""
SharedMemory

mutable struct SharedMemory <: IO
    name::String
    handle::OS_HANDLE
    readonly::Bool
    create::Bool
    size::UInt

    function SharedMemory(name, handle, readonly, create, size)
        io = new(name, handle, readonly, create, size)
        finalizer(close, io)
        return io
    end
end

# platform-specific mmap utilities
if Sys.isunix()

const PROT_READ     = Cint(1)
const PROT_WRITE    = Cint(2)
const MAP_SHARED    = Cint(1)
const MAP_PRIVATE   = Cint(2)
const MAP_ANONYMOUS = Cint(Sys.isbsd() ? 0x1000 : 0x20)
const F_GETFL       = Cint(3)

gethandle(io::IO) = fd(io)

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

function preallocate(fd::OS_HANDLE, size::Integer)
    @static if Sys.isapple()
        # MacOS does not support preallocation of shared memory as a syscall, and will
        # allow the memory to grow on demand until it exceeds limits and the process is
        # killed. And when that happens, the resources may be left open. To avoid this,
        # we first use `os_proc_available_memory()` to determine if there is sufficient
        # memory available. Then we temporarily `mmap` the region and use iterative
        # `mlock` to force MacOS to commit the memory, to match the semantics on other
        # platforms.
        avail = ccall(:os_proc_available_memory, Csize_t, ())
        if size > avail
            systemerror("preallocate", Libc.ENOMEM)
        end

        # Temporarily mmap the shared memory region
        ptr = ccall(:jl_mmap, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t, Cint, Cint, OS_HANDLE, Int64),
                    C_NULL, size, PROT_READ | PROT_WRITE, MAP_SHARED, fd, Int64(0))
        systemerror(:mmap, reinterpret(Int, ptr) == -1)
        try
            # Lock and immediately unlock each page to force backing page allocation.
            # munlock frees the physical frame so the next iteration can reuse it.
            for page_offset in 0:Int(PAGESIZE):(size - 1)
                chunk = min(Int(PAGESIZE), size - page_offset)
                status = ccall(:mlock, Cint, (Ptr{Cvoid}, Csize_t), ptr + page_offset, chunk)
                systemerror(:mlock, status == -1)
                ccall(:munlock, Cint, (Ptr{Cvoid}, Csize_t), ptr + page_offset, chunk)
            end
        finally
            ccall(:munmap, Cint, (Ptr{Cvoid}, Csize_t), ptr, size)
        end
    else
        status = ccall(:posix_fallocate, Cint, (OS_HANDLE, Int, Int), fd, 0, size) # does not set `errno`
        status != 0 && systemerror(:posix_fallocate, status)
    end
end

function Base.open(::Type{SharedMemory}, name::AbstractString, size::Integer;
    readonly :: Bool = true,
    create   :: Bool = false
)
    validate_sharedmemory_args(name, size, create)
    io = SharedMemory(name, INVALID_OS_HANDLE, readonly, create, size)
    if !isempty(name)
        oflag = (readonly ? JL_O_RDONLY : JL_O_RDWR)
        if create
            oflag |= JL_O_CREAT | JL_O_EXCL
        end
        mode = S_IRUSR | S_IWUSR # Owner read/write
        try
            io.handle = RawFD(shm_open(name, oflag, mode))
            systemerror(:shm_open, io.handle == INVALID_OS_HANDLE)
        catch e
            if e isa SystemError
                throw(IOError("Failed to $(create ? "create" : "open") shared memory", e.errnum))
            else
                rethrow()
            end
        end
        try
            if create
                # Set the size of the shared memory object
                # On OSX, ftruncate must be used to set size of segment, just lseek does not work.
                # And only at creation time.
                status = ccall(:jl_ftruncate, Cint, (OS_HANDLE, Int64), io.handle, size)
                systemerror(:ftruncate, status != 0)

                # Reserve space for the shared memory object
                # Failing to do this here may later result in SIGBUS fault if reading/writing beyond available memory
                preallocate(io.handle, size)
            end
        catch e
            close(io)
            if e isa SystemError
                throw(IOError("Failed to allocate shared memory", e.errnum))
            else
                rethrow()
            end
        end
    else
        # Anonymous mapping doesn't require a shared-memory file descriptor.
    end

    return io
end

function Base.close(io::SharedMemory)
    if io.handle != INVALID_OS_HANDLE
        try
            if io.create
                status = shm_unlink(io.name)
                systemerror(:shm_unlink, status != 0)
            end
        finally
            status = ccall(:close, Cint, (OS_HANDLE,), io.handle)
            io.handle = INVALID_OS_HANDLE
            systemerror(:close, status != 0)
        end
    end
    return nothing
end

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
# Note: a few mappable streams do not support lseek. When Julia supports structures in ccall, switch to fstat.
function grow!(io::IO, offset::Integer, len::Integer)
    pos = position(io)
    filelen = filesize(io)
    # Don't attempt to grow non-regular file, will fail
    filelen == 0 && !isfile(io) && return
    if filelen < offset + len
        status = ccall(:jl_ftruncate, Cint, (Cint, Int64), fd(io), offset + len)
        Base.systemerror(:ftruncate, status != 0)
    end
    seek(io, pos)
    return nothing
end

elseif Sys.iswindows()

# Windows API constants
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

const ERROR_ALREADY_EXISTS   = DWORD(0xB7)

const NULL_OS_HANDLE         = reinterpret(OS_HANDLE, 0)

function gethandle(io::IO)
    handle = Libc._get_osfhandle(RawFD(fd(io)))
    Base.windowserror(:mmap, handle == INVALID_OS_HANDLE)
    return handle
end

mutable struct SECURITY_ATTRIBUTES
    nLength::DWORD
    lpSecurityDescriptor::Ptr{Cvoid}
    bInheritHandle::Bool

    SECURITY_ATTRIBUTES(inherit_handle::Bool) = new(sizeof(SECURITY_ATTRIBUTES), C_NULL, inherit_handle)
end
default_security_attrs() = Ref(SECURITY_ATTRIBUTES(true))

# Split 64-bit size into high and low 32-bit parts for Windows API
split_high_bits(size::Integer) = UInt32(size >> 32)
split_low_bits(size::Integer) = UInt32(size & typemax(UInt32))

function Base.open(::Type{SharedMemory}, name::AbstractString, size::Integer;
    readonly :: Bool = true,
    create   :: Bool = false
)
    validate_sharedmemory_args(name, size, create)
    io = SharedMemory(name, INVALID_OS_HANDLE, readonly, create, size)
    if create
        try
            page_prot = readonly ? PAGE_READONLY : PAGE_READWRITE
            handle = ccall(:CreateFileMappingW, stdcall, OS_HANDLE, (OS_HANDLE, Ref{SECURITY_ATTRIBUTES}, DWORD, DWORD, DWORD, Cwstring),
                INVALID_OS_HANDLE,                                 # Backed by system paging file
                default_security_attrs(),                          # Default security, can be inherited
                page_prot,                                         # Requested access mode
                split_high_bits(io.size), split_low_bits(io.size), # High-order and low-order bits of size
                name                                               # Object name
            )
            lasterr = ccall(:GetLastError, stdcall, DWORD, ())
            if lasterr == ERROR_ALREADY_EXISTS
                # Windows indicates if it already exists this way, but returns a valid handle anyway
                status = ccall(:CloseHandle, stdcall, Cint, (OS_HANDLE,), handle)
                Base.windowserror(:CloseHandle, status == 0)
                Base.windowserror(:CreateFileMappingW, lasterr)
            else
                Base.windowserror(:CreateFileMappingW,  handle == NULL_OS_HANDLE)
            end
            io.handle = handle
        catch e
            if e isa SystemError
                throw(IOError("Failed to create and allocate shared memory", e.errnum))
            else
                rethrow()
            end
        end
    else
        try
            map_access = readonly ? FILE_MAP_READ : FILE_MAP_WRITE
            handle = ccall(:OpenFileMappingW, stdcall, OS_HANDLE, (DWORD, Cint, Cwstring),
                map_access, # Requested access mode
                true,       # Can be inherited
                name        # Object name
            )
            Base.windowserror(:OpenFileMappingW, handle == NULL_OS_HANDLE)
            io.handle = handle
        catch e
            if e isa SystemError
                throw(IOError("Failed to open shared memory", e.errnum))
            else
                rethrow()
            end
        end
    end
    return io
end

function Base.close(io::SharedMemory)
    if io.handle != INVALID_OS_HANDLE
        status = ccall(:CloseHandle, stdcall, Cint, (OS_HANDLE,), io.handle)
        io.handle = INVALID_OS_HANDLE
        Base.windowserror(:CloseHandle, status == 0)
    end
    return nothing
end

else
    error("mmap not defined for this OS")
end # os-test

# SharedMemory cannot be grown after creation
grow!(::SharedMemory, ::Integer, ::Integer) =
    throw(ArgumentError("resizing of SharedMemory is not supported"))

function Base.open(::Type{SharedMemory}, name::AbstractString, size::Integer, mode::AbstractString)
    mode == "r"  ? open(SharedMemory, name, size; readonly = true,  create = false) :
    mode == "r+" ? open(SharedMemory, name, size; readonly = false, create = false) :
    mode == "w+" ? open(SharedMemory, name, size; readonly = false, create = true)  :
    throw(ArgumentError("invalid SharedMemory open mode: $mode; must be \"r\", \"r+\", or \"w+\""))
end

Base.filesize(io::SharedMemory) = io.size
Base.position(io::SharedMemory) = 0
Base.isfile(io::SharedMemory) = false
Base.isreadonly(io::SharedMemory) = io.readonly
Base.isreadable(io::SharedMemory) = true
Base.iswritable(io::SharedMemory) = !io.readonly
Base.isopen(io::SharedMemory) = io.handle != INVALID_OS_HANDLE || io.name == ""

gethandle(io::SharedMemory) = io.handle

function validate_sharedmemory_args(name::AbstractString, size::Integer, create::Bool)
    isempty(name) && !create &&
        throw(ArgumentError("Anonymous SharedMemory files must have `create = true`."))
    create && size == 0 &&
        throw(ArgumentError("Size of SharedMemory files must be greater than 0."))
end

function check_can_grow(io::IO, szfile::Csize_t, readonly::Bool, grow::Bool)
    requestedSizeLarger = szfile > filesize(io)
    if requestedSizeLarger
        readonly && throw(ArgumentError("unable to increase file size to $szfile due to read-only permissions"))
        !grow && throw(ArgumentError("requested size $szfile larger than file size $(filesize(io)), but requested not to grow"))
        io isa SharedMemory && throw(ArgumentError("unable to increase size of SharedMemory beyond $(filesize(io))"))
        return true
    end
    return false
end

# core implementation of mmap

"""
    mmap(io::Union{IOStream,AbstractString,Mmap.SharedMemory}[, type::Type{Array{T,N}}, dims, offset]; grow::Bool=true, shared::Bool=true)
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

    # platform-specific mmapping
    handle = INVALID_OS_HANDLE
    try
        @static if Sys.isunix()
            prot, flags, readonly = settings(file_desc, shared)
            check_can_grow(io, szfile, readonly, grow) && grow!(io, offset, len)
            ptr = ccall(:jl_mmap, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t, Cint, Cint, OS_HANDLE, Int64),
                C_NULL, mmaplen, prot, flags, file_desc, offset_page)
            systemerror(:mmap, reinterpret(Int, ptr) == -1)
        else
            readonly = isreadonly(io)
            check_can_grow(io, szfile, readonly, grow) # Cannot grow on Windows, error if requested size is larger than file size

            if !(io isa SharedMemory)
                page_prot = readonly ? PAGE_READONLY : PAGE_READWRITE
                handle = ccall(:CreateFileMappingW, stdcall, OS_HANDLE, (OS_HANDLE, Ref{SECURITY_ATTRIBUTES}, DWORD, DWORD, DWORD, Cwstring),
                    file_desc,                                       # File to map
                    default_security_attrs(),                        # Default security, can be inherited
                    page_prot,                                       # Requested access mode
                    split_high_bits(szfile), split_low_bits(szfile), # High-order and low-order bits of size
                    ""                                               # Object name
                )
                Base.windowserror(:CreateFileMappingW, handle == NULL_OS_HANDLE)
            else
                handle = io.handle
            end

            map_access = readonly ? FILE_MAP_READ : FILE_MAP_WRITE
            ptr = ccall(:MapViewOfFile, stdcall, Ptr{Cvoid}, (OS_HANDLE, DWORD, DWORD, DWORD, Csize_t),
                handle,                                                    # Handle to mapping object
                map_access,                                                # Requested access mode
                split_high_bits(offset_page), split_low_bits(offset_page), # High-order and low-order bits of offset
                mmaplen                                                    # Number of bytes to map
            )
            Base.windowserror(:MapViewOfFile, ptr == C_NULL)
        end # os-test

        # convert mmapped region to Julia Array at `ptr + (offset - offset_page)` since file was mapped at offset_page
        A = unsafe_wrap(Array, convert(Ptr{T}, UInt(ptr) + UInt(offset - offset_page)), dims)
        finalizer(A.ref.mem) do _
            @static if Sys.isunix()
                ustatus = ccall(:munmap, Cint, (Ptr{Cvoid}, Int), ptr, mmaplen)
                systemerror(:munmap, ustatus != 0)
            else
                ustatus = ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Cvoid},), ptr)
                Base.windowserror(:UnmapViewOfFile, ustatus == 0)
            end
        end
        return A
    catch e
        if e isa SystemError
            throw(IOError("Failed to create memory map", e.errnum))
        else
            rethrow()
        end
    finally
        @static if Sys.iswindows()
            if !(io isa SharedMemory) && handle != INVALID_OS_HANDLE
                status = ccall(:CloseHandle, stdcall, Cint, (OS_HANDLE,), handle) != 0
                Base.windowserror(:CloseHandle, status == 0)
            end
        end
    end
end

mmap(file::AbstractString,
     ::Type{T}=Vector{UInt8},
     dims::NTuple{N,Integer}=(div(filesize(file),Base.aligned_sizeof(eltype(T))),),
     offset::Integer=Int64(0); grow::Bool=true, shared::Bool=true) where {T<:Array,N} =
    open(io->mmap(io, T, dims, offset; grow, shared), file, isfile(file) ? "r" : "w+")::Array{eltype(T),N}

# using a length argument instead of dims
mmap(io::IO, ::Type{T}, len::Integer, offset::Integer=position(io); grow::Bool=true, shared::Bool=true) where {T<:Array} =
    mmap(io, T, (len,), offset; grow, shared)
mmap(file::AbstractString, ::Type{T}, len::Integer, offset::Integer=Int64(0); grow::Bool=true, shared::Bool=true) where {T<:Array} =
    open(io->mmap(io, T, (len,), offset; grow, shared), file, isfile(file) ? "r" : "w+")::Vector{eltype(T)}

# constructors for non-file-backed, unnamed (anonymous) mmaps
mmap(::Type{T}, dims::NTuple{N,Integer}; shared::Bool=true) where {T <: Array, N} =
    open(io -> mmap(io, T, dims, Int64(0); shared), SharedMemory, "", prod(dims) * Base.aligned_sizeof(eltype(T)), "w+")
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
    open(io->mmap(io, T, dims, offset; grow, shared), file, isfile(file) ? "r" : "w+")::BitArray{N}

# using a length argument instead of dims
mmap(io::IO, ::Type{T}, len::Integer, offset::Integer=position(io); grow::Bool=true, shared::Bool=true) where {T<:BitArray} =
    mmap(io, T, (len,), offset; grow, shared)
mmap(file::AbstractString, ::Type{T}, len::Integer, offset::Integer=Int64(0); grow::Bool=true, shared::Bool=true) where {T<:BitArray} =
    open(io->mmap(io, T, (len,), offset; grow, shared), file, isfile(file) ? "r" : "w+")::BitVector

# constructors for non-file-backed (anonymous) mmaps
mmap(::Type{T}, dims::NTuple{N,Integer}; shared::Bool=true) where {T<:BitArray,N} =
    open(io -> mmap(io, T, dims, Int64(0); shared), SharedMemory, "", prod(dims) * sizeof(T), "w+")
mmap(::Type{T}, i::Integer...; shared::Bool=true) where {T<:BitArray} = mmap(T, convert(Tuple{Vararg{Int}}, i); shared)

# msync flags for unix
const MS_ASYNC = 1
const MS_INVALIDATE = 2
const MS_SYNC = 4

# Helper for page-aligned pointer calculations
function page_aligned_region(ptr::Ptr, len::Integer)
    offset = rem(UInt(ptr), PAGESIZE)
    aligned_ptr = ptr - offset
    aligned_len = len + offset
    return aligned_ptr, aligned_len
end

"""
    Mmap.sync!(array)

Forces synchronization between the in-memory version of a memory-mapped `Array` or
[`BitArray`](@ref) and the on-disk version.
"""
function sync!(m::Array, flags::Integer=MS_SYNC)
    ptr, len = page_aligned_region(pointer(m), sizeof(m))
    GC.@preserve m @static if Sys.isunix()
        systemerror("msync",
            ccall(:msync, Cint, (Ptr{Cvoid}, Csize_t, Cint), ptr, len, flags) != 0)
    else
        Base.windowserror(:FlushViewOfFile,
            ccall(:FlushViewOfFile, stdcall, Cint, (Ptr{Cvoid}, Csize_t), ptr, len) == 0)
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
    ptr, len = page_aligned_region(pointer(m), sizeof(m))
    GC.@preserve m begin
        systemerror("madvise",
            ccall(:madvise, Cint, (Ptr{Cvoid}, Csize_t, Cint), ptr, len, flag) != 0)
    end
end
madvise!(B::BitArray, flag::Integer=MADV_NORMAL) = madvise!(B.chunks, flag)
end # Sys.isunix()

end # module
