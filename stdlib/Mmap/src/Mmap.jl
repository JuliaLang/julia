# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Low level module for mmap (memory mapping of files).
"""
module Mmap

import Base: OS_HANDLE, INVALID_OS_HANDLE

const PAGESIZE = Int(Sys.isunix() ? ccall(:jl_getpagesize, Clong, ()) : ccall(:jl_getallocationgranularity, Clong, ()))

# for mmaps not backed by files
mutable struct Anonymous <: IO
    name::AbstractString
    readonly::Bool
    create::Bool
end

"""
    Mmap.Anonymous(name::AbstractString="", readonly::Bool=false, create::Bool=true)

Create an `IO`-like object for creating zeroed-out mmapped-memory that is not tied to a file
for use in [`Mmap.mmap`](@ref Mmap.mmap). Used by `SharedArray` for creating shared memory arrays.

# Examples
```jldoctest
julia> using Mmap

julia> anon = Mmap.Anonymous();

julia> isreadable(anon)
true

julia> iswritable(anon)
true

julia> isopen(anon)
true
```
"""
Anonymous() = Anonymous("",false,true)

Base.isopen(::Anonymous) = true
Base.isreadable(::Anonymous) = true
Base.iswritable(a::Anonymous) = !a.readonly

# const used for zeroed, anonymous memory
gethandle(io::Anonymous) = INVALID_OS_HANDLE

# platform-specific mmap utilities
if Sys.isunix()

const PROT_READ     = Cint(1)
const PROT_WRITE    = Cint(2)
const MAP_SHARED    = Cint(1)
const MAP_PRIVATE   = Cint(2)
const MAP_ANONYMOUS = Cint(Sys.isbsd() ? 0x1000 : 0x20)
const F_GETFL       = Cint(3)

gethandle(io::IO) = RawFD(fd(io))

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
    return prot, flags, (prot & PROT_WRITE) > 0
end

# Before mapping, grow the file to sufficient size
# Note: a few mappable streams do not support lseek. When Julia
# supports structures in ccall, switch to fstat.
grow!(::Anonymous,o::Integer,l::Integer) = return
function grow!(io::IO, offset::Integer, len::Integer)
    pos = position(io)
    filelen = filesize(io)
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

function gethandle(io::IO)
    handle = Libc._get_osfhandle(RawFD(fd(io)))
    Base.windowserror(:mmap, handle == INVALID_OS_HANDLE)
    return handle
end

settings(sh::Anonymous) = sh.name, sh.readonly, sh.create
settings(io::IO) = Ptr{Cwchar_t}(0), isreadonly(io), true

else
    error("mmap not defined for this OS")
end # os-test

# core implementation of mmap

"""
    Mmap.mmap(io::Union{IOStream,AbstractString,Mmap.AnonymousMmap}[, type::Type{Array{T,N}}, dims, offset]; grow::Bool=true, shared::Bool=true)
    Mmap.mmap(type::Type{Array{T,N}}, dims)

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
A2 = Mmap.mmap(s, Matrix{Int}, (m,n))
```

creates a `m`-by-`n` `Matrix{Int}`, linked to the file associated with stream `s`.

A more portable file would need to encode the word size -- 32 bit or 64 bit -- and endianness
information in the header. In practice, consider encoding binary data using standard formats
like HDF5 (which can be used with memory-mapping).
"""
function mmap(io::IO,
              ::Type{Array{T,N}}=Vector{UInt8},
              dims::NTuple{N,Integer}=(div(filesize(io)-position(io),sizeof(T)),),
              offset::Integer=position(io); grow::Bool=true, shared::Bool=true) where {T,N}
    # check inputs
    isopen(io) || throw(ArgumentError("$io must be open to mmap"))
    isbitstype(T)  || throw(ArgumentError("unable to mmap $T; must satisfy isbitstype(T) == true"))

    len = prod(dims) * sizeof(T)
    len >= 0 || throw(ArgumentError("requested size must be ≥ 0, got $len"))
    len == 0 && return Array{T}(undef, ntuple(x->0,Val(N)))
    len < typemax(Int) - PAGESIZE || throw(ArgumentError("requested size must be < $(typemax(Int)-PAGESIZE), got $len"))

    offset >= 0 || throw(ArgumentError("requested offset must be ≥ 0, got $offset"))

    # shift `offset` to start of page boundary
    offset_page::Int64 = div(offset, PAGESIZE) * PAGESIZE
    # add (offset - offset_page) to `len` to get total length of memory-mapped region
    mmaplen = (offset - offset_page) + len

    file_desc = gethandle(io)
    # platform-specific mmapping
    @static if Sys.isunix()
        prot, flags, iswrite = settings(file_desc, shared)
        iswrite && grow && grow!(io, offset, len)
        # mmap the file
        ptr = ccall(:jl_mmap, Ptr{Cvoid}, (Ptr{Cvoid}, Csize_t, Cint, Cint, RawFD, Int64),
            C_NULL, mmaplen, prot, flags, file_desc, offset_page)
        systemerror("memory mapping failed", reinterpret(Int, ptr) == -1)
    else
        name, readonly, create = settings(io)
        szfile = convert(Csize_t, len + offset)
        readonly && szfile > filesize(io) && throw(ArgumentError("unable to increase file size to $szfile due to read-only permissions"))
        handle = create ? ccall(:CreateFileMappingW, stdcall, Ptr{Cvoid}, (OS_HANDLE, Ptr{Cvoid}, DWORD, DWORD, DWORD, Cwstring),
                                file_desc, C_NULL, readonly ? PAGE_READONLY : PAGE_READWRITE, szfile >> 32, szfile & typemax(UInt32), name) :
                          ccall(:OpenFileMappingW, stdcall, Ptr{Cvoid}, (DWORD, Cint, Cwstring),
                                readonly ? FILE_MAP_READ : FILE_MAP_WRITE, true, name)
        Base.windowserror(:mmap, handle == C_NULL)
        ptr = ccall(:MapViewOfFile, stdcall, Ptr{Cvoid}, (Ptr{Cvoid}, DWORD, DWORD, DWORD, Csize_t),
                    handle, readonly ? FILE_MAP_READ : FILE_MAP_WRITE, offset_page >> 32, offset_page & typemax(UInt32), (offset - offset_page) + len)
        Base.windowserror(:mmap, ptr == C_NULL)
    end # os-test
    # convert mmapped region to Julia Array at `ptr + (offset - offset_page)` since file was mapped at offset_page
    A = unsafe_wrap(Array, convert(Ptr{T}, UInt(ptr) + UInt(offset - offset_page)), dims)
    finalizer(A) do x
        @static if Sys.isunix()
            systemerror("munmap",  ccall(:munmap, Cint, (Ptr{Cvoid}, Int), ptr, mmaplen) != 0)
        else
            status = ccall(:UnmapViewOfFile, stdcall, Cint, (Ptr{Cvoid},), ptr)!=0
            status |= ccall(:CloseHandle, stdcall, Cint, (Ptr{Cvoid},), handle)!=0
            Base.windowserror(:UnmapViewOfFile, status == 0)
        end
    end
    return A
end

mmap(file::AbstractString,
     ::Type{T}=Vector{UInt8},
     dims::NTuple{N,Integer}=(div(filesize(file),sizeof(eltype(T))),),
     offset::Integer=Int64(0); grow::Bool=true, shared::Bool=true) where {T<:Array,N} =
    open(io->mmap(io, T, dims, offset; grow=grow, shared=shared), file, isfile(file) ? "r" : "w+")::Array{eltype(T),N}

# using a length argument instead of dims
mmap(io::IO, ::Type{T}, len::Integer, offset::Integer=position(io); grow::Bool=true, shared::Bool=true) where {T<:Array} =
    mmap(io, T, (len,), offset; grow=grow, shared=shared)
mmap(file::AbstractString, ::Type{T}, len::Integer, offset::Integer=Int64(0); grow::Bool=true, shared::Bool=true) where {T<:Array} =
    open(io->mmap(io, T, (len,), offset; grow=grow, shared=shared), file, isfile(file) ? "r" : "w+")::Vector{eltype(T)}

# constructors for non-file-backed (anonymous) mmaps
mmap(::Type{T}, dims::NTuple{N,Integer}; shared::Bool=true) where {T<:Array,N} = mmap(Anonymous(), T, dims, Int64(0); shared=shared)
mmap(::Type{T}, i::Integer...; shared::Bool=true) where {T<:Array} = mmap(Anonymous(), T, convert(Tuple{Vararg{Int}},i), Int64(0); shared=shared)

"""
    Mmap.mmap(io, BitArray, [dims, offset])

Create a [`BitArray`](@ref) whose values are linked to a file, using memory-mapping; it has the same
purpose, works in the same way, and has the same arguments, as [`mmap`](@ref Mmap.mmap), but
the byte representation is different.

# Examples
```jldoctest
julia> using Mmap

julia> io = open("mmap.bin", "w+");

julia> B = Mmap.mmap(io, BitArray, (25,30000));

julia> B[3, 4000] = true;

julia> Mmap.sync!(B);

julia> close(io);

julia> io = open("mmap.bin", "r+");

julia> C = Mmap.mmap(io, BitArray, (25,30000));

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
function sync!(m::Array{T}, flags::Integer=MS_SYNC) where T
    offset = rem(UInt(pointer(m)), PAGESIZE)
    ptr = pointer(m) - offset
    GC.@preserve m @static if Sys.isunix()
        systemerror("msync",
                    ccall(:msync, Cint, (Ptr{Cvoid}, Csize_t, Cint), ptr, length(m) * sizeof(T), flags) != 0)
    else
        Base.windowserror(:FlushViewOfFile,
            ccall(:FlushViewOfFile, stdcall, Cint, (Ptr{Cvoid}, Csize_t), ptr, length(m)) == 0)
    end
end
sync!(B::BitArray, flags::Integer=MS_SYNC) = sync!(B.chunks, flags)

end # module
