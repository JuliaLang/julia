# This file is a part of Julia. License is MIT: https://julialang.org/license

# Generic IO stubs -- all subtypes should implement these (if meaningful)

"""
    EOFError()

No more data was available to read from a file or stream.
"""
struct EOFError <: Exception end

"""
    SystemError(prefix::AbstractString, [errno::Int32])

A system call failed with an error code (in the `errno` global variable).
"""
struct SystemError <: Exception
    prefix::AbstractString
    errnum::Int32
    extrainfo
    SystemError(p::AbstractString, e::Integer, extrainfo) = new(p, e, extrainfo)
    SystemError(p::AbstractString, e::Integer) = new(p, e, nothing)
    SystemError(p::AbstractString) = new(p, Libc.errno())
end

lock(::IO) = nothing
unlock(::IO) = nothing
reseteof(x::IO) = nothing

const SZ_UNBUFFERED_IO = 65536
buffer_writes(x::IO, bufsize=SZ_UNBUFFERED_IO) = x

"""
    isopen(object) -> Bool

Determine whether an object - such as a stream or timer
-- is not yet closed. Once an object is closed, it will never produce a new event.
However, since a closed stream may still have data to read in its buffer,
use [`eof`](@ref) to check for the ability to read data.
Use the `FileWatching` package to be notified when a stream might be writable or readable.

# Examples
```jldoctest
julia> io = open("my_file.txt", "w+");

julia> isopen(io)
true

julia> close(io)

julia> isopen(io)
false
```
"""
function isopen end

"""
    close(stream)

Close an I/O stream. Performs a [`flush`](@ref) first.
"""
function close end
function flush end
function wait_readnb end
function wait_close end
function bytesavailable end

"""
    readavailable(stream)

Read all available data on the stream, blocking the task only if no data is available. The
result is a `Vector{UInt8}`.
"""
function readavailable end

"""
    isreadable(io) -> Bool

Return `true` if the specified IO object is readable (if that can be determined).

# Examples
```jldoctest
julia> open("myfile.txt", "w") do io
           print(io, "Hello world!");
           isreadable(io)
       end
false

julia> open("myfile.txt", "r") do io
           isreadable(io)
       end
true

julia> rm("myfile.txt")
```
"""
function isreadable end

"""
    iswritable(io) -> Bool

Return `true` if the specified IO object is writable (if that can be determined).

# Examples
```jldoctest
julia> open("myfile.txt", "w") do io
           print(io, "Hello world!");
           iswritable(io)
       end
true

julia> open("myfile.txt", "r") do io
           iswritable(io)
       end
false

julia> rm("myfile.txt")
```
"""
function iswritable end
function copy end
function eof end

"""
    read(io::IO, T)

Read a single value of type `T` from `io`, in canonical binary representation.

Note that Julia does not convert the endianness for you. Use [`ntoh`](@ref) or
[`ltoh`](@ref) for this purpose.

    read(io::IO, String)

Read the entirety of `io`, as a `String`.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> read(io, Char)
'J': ASCII/Unicode U+004A (category Lu: Letter, uppercase)

julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> read(io, String)
"JuliaLang is a GitHub organization"
```
"""
read(stream, t)

"""
    write(io::IO, x)
    write(filename::AbstractString, x)

Write the canonical binary representation of a value to the given I/O stream or file.
Return the number of bytes written into the stream. See also [`print`](@ref) to
write a text representation (with an encoding that may depend upon `io`).

The endianness of the written value depends on the endianness of the host system.
Convert to/from a fixed endianness when writing/reading (e.g. using  [`htol`](@ref) and
[`ltoh`](@ref)) to get results that are consistent across platforms.

You can write multiple values with the same `write` call. i.e. the following are equivalent:

    write(io, x, y...)
    write(io, x) + write(io, y...)

# Examples
Consistent serialization:
```jldoctest
julia> fname = tempname(); # random temporary filename

julia> open(fname,"w") do f
           # Make sure we write 64bit integer in little-endian byte order
           write(f,htol(Int64(42)))
       end
8

julia> open(fname,"r") do f
           # Convert back to host byte order and host integer type
           Int(ltoh(read(f,Int64)))
       end
42
```

Merging write calls:
```jldoctest
julia> io = IOBuffer();

julia> write(io, "JuliaLang is a GitHub organization.", " It has many members.")
56

julia> String(take!(io))
"JuliaLang is a GitHub organization. It has many members."

julia> write(io, "Sometimes those members") + write(io, " write documentation.")
44

julia> String(take!(io))
"Sometimes those members write documentation."
```
User-defined plain-data types without `write` methods can be written when wrapped in a `Ref`:
```jldoctest
julia> struct MyStruct; x::Float64; end

julia> io = IOBuffer()
IOBuffer(data=UInt8[...], readable=true, writable=true, seekable=true, append=false, size=0, maxsize=Inf, ptr=1, mark=-1)

julia> write(io, Ref(MyStruct(42.0)))
8

julia> seekstart(io); read!(io, Ref(MyStruct(NaN)))
Base.RefValue{MyStruct}(MyStruct(42.0))
```
"""
function write end

read(s::IO, ::Type{UInt8}) = error(typeof(s)," does not support byte I/O")
write(s::IO, x::UInt8) = error(typeof(s)," does not support byte I/O")

"""
    unsafe_write(io::IO, ref, nbytes::UInt)

Copy `nbytes` from `ref` (converted to a pointer) into the `IO` object.

It is recommended that subtypes `T<:IO` override the following method signature
to provide more efficient implementations:
`unsafe_write(s::T, p::Ptr{UInt8}, n::UInt)`
"""
function unsafe_write(s::IO, p::Ptr{UInt8}, n::UInt)
    written::Int = 0
    for i = 1:n
        written += write(s, unsafe_load(p, i))
    end
    return written
end

"""
    unsafe_read(io::IO, ref, nbytes::UInt)

Copy `nbytes` from the `IO` stream object into `ref` (converted to a pointer).

It is recommended that subtypes `T<:IO` override the following method signature
to provide more efficient implementations:
`unsafe_read(s::T, p::Ptr{UInt8}, n::UInt)`
"""
function unsafe_read(s::IO, p::Ptr{UInt8}, n::UInt)
    for i = 1:n
        unsafe_store!(p, read(s, UInt8)::UInt8, i)
    end
    nothing
end

function peek(s::IO, ::Type{T}) where T
    mark(s)
    try read(s, T)
    finally
        reset(s)
    end
end

peek(s) = peek(s, UInt8)

# Generic `open` methods

"""
    open_flags(; keywords...) -> NamedTuple

Compute the `read`, `write`, `create`, `truncate`, `append` flag value for
a given set of keyword arguments to [`open`](@ref) a [`NamedTuple`](@ref).
"""
function open_flags(;
    read     :: Union{Bool,Nothing} = nothing,
    write    :: Union{Bool,Nothing} = nothing,
    create   :: Union{Bool,Nothing} = nothing,
    truncate :: Union{Bool,Nothing} = nothing,
    append   :: Union{Bool,Nothing} = nothing,
)
    if write === true && read !== true && append !== true
        create   === nothing && (create   = true)
        truncate === nothing && (truncate = true)
    end

    if truncate === true || append === true
        write  === nothing && (write  = true)
        create === nothing && (create = true)
    end

    write    === nothing && (write    = false)
    read     === nothing && (read     = !write)
    create   === nothing && (create   = false)
    truncate === nothing && (truncate = false)
    append   === nothing && (append   = false)

    return (
        read = read,
        write = write,
        create = create,
        truncate = truncate,
        append = append,
    )
end

"""
    open(f::Function, args...; kwargs....)

Apply the function `f` to the result of `open(args...; kwargs...)` and close the resulting file
descriptor upon completion.

# Examples
```jldoctest
julia> open("myfile.txt", "w") do io
           write(io, "Hello world!")
       end;

julia> open(f->read(f, String), "myfile.txt")
"Hello world!"

julia> rm("myfile.txt")
```
"""
function open(f::Function, args...; kwargs...)
    io = open(args...; kwargs...)
    try
        f(io)
    finally
        close(io)
    end
end

# Generic wrappers around other IO objects
abstract type AbstractPipe <: IO end
function pipe_reader end
function pipe_writer end

write(io::AbstractPipe, byte::UInt8) = write(pipe_writer(io), byte)
unsafe_write(io::AbstractPipe, p::Ptr{UInt8}, nb::UInt) = unsafe_write(pipe_writer(io), p, nb)
buffer_writes(io::AbstractPipe, args...) = buffer_writes(pipe_writer(io), args...)
flush(io::AbstractPipe) = flush(pipe_writer(io))

read(io::AbstractPipe, byte::Type{UInt8}) = read(pipe_reader(io), byte)
unsafe_read(io::AbstractPipe, p::Ptr{UInt8}, nb::UInt) = unsafe_read(pipe_reader(io), p, nb)
read(io::AbstractPipe) = read(pipe_reader(io))
readuntil(io::AbstractPipe, arg::UInt8; kw...) = readuntil(pipe_reader(io), arg; kw...)
readuntil(io::AbstractPipe, arg::AbstractChar; kw...) = readuntil(pipe_reader(io), arg; kw...)
readuntil(io::AbstractPipe, arg::AbstractString; kw...) = readuntil(pipe_reader(io), arg; kw...)
readuntil(io::AbstractPipe, arg::AbstractVector; kw...) = readuntil(pipe_reader(io), arg; kw...)
readuntil_vector!(io::AbstractPipe, target::AbstractVector, keep::Bool, out) = readuntil_vector!(pipe_reader(io), target, keep, out)
readbytes!(io::AbstractPipe, target::AbstractVector{UInt8}, n=length(target)) = readbytes!(pipe_reader(io), target, n)

for f in (
        # peek/mark interface
        :mark, :unmark, :reset, :ismarked,
        # Simple reader functions
        :readavailable, :isreadable)
    @eval $(f)(io::AbstractPipe) = $(f)(pipe_reader(io))
end
peek(io::AbstractPipe, ::Type{T}) where {T} = peek(pipe_reader(io), T)

iswritable(io::AbstractPipe) = iswritable(pipe_writer(io))
isopen(io::AbstractPipe) = isopen(pipe_writer(io)) || isopen(pipe_reader(io))
close(io::AbstractPipe) = (close(pipe_writer(io)); close(pipe_reader(io)))
wait_readnb(io::AbstractPipe, nb::Int) = wait_readnb(pipe_reader(io), nb)
wait_close(io::AbstractPipe) = (wait_close(pipe_writer(io)); wait_close(pipe_reader(io)))

"""
    bytesavailable(io)

Return the number of bytes available for reading before a read from this stream or buffer will block.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> bytesavailable(io)
34
```
"""
bytesavailable(io::AbstractPipe) = bytesavailable(pipe_reader(io))

"""
    eof(stream) -> Bool

Test whether an I/O stream is at end-of-file. If the stream is not yet exhausted, this
function will block to wait for more data if necessary, and then return `false`. Therefore
it is always safe to read one byte after seeing `eof` return `false`. `eof` will return
`false` as long as buffered data is still available, even if the remote end of a connection
is closed.
"""
eof(io::AbstractPipe) = eof(pipe_reader(io))
reseteof(io::AbstractPipe) = reseteof(pipe_reader(io))


# Exception-safe wrappers (io = open(); try f(io) finally close(io))

write(filename::AbstractString, a1, args...) = open(io->write(io, a1, args...), filename, "w")

"""
    read(filename::AbstractString, args...)

Open a file and read its contents. `args` is passed to `read`: this is equivalent to
`open(io->read(io, args...), filename)`.

    read(filename::AbstractString, String)

Read the entire contents of a file as a string.
"""
read(filename::AbstractString, args...) = open(io->read(io, args...), filename)

read(filename::AbstractString, ::Type{T}) where {T} = open(io->read(io, T), filename)

"""
    read!(stream::IO, array::AbstractArray)
    read!(filename::AbstractString, array::AbstractArray)

Read binary data from an I/O stream or file, filling in `array`.
"""
function read! end

read!(filename::AbstractString, a) = open(io->read!(io, a), filename)

"""
    readuntil(stream::IO, delim; keep::Bool = false)
    readuntil(filename::AbstractString, delim; keep::Bool = false)

Read a string from an I/O stream or a file, up to the given delimiter.
The delimiter can be a `UInt8`, `AbstractChar`, string, or vector.
Keyword argument `keep` controls whether the delimiter is included in the result.
The text is assumed to be encoded in UTF-8.

# Examples
```jldoctest
julia> open("my_file.txt", "w") do io
           write(io, "JuliaLang is a GitHub organization.\\nIt has many members.\\n");
       end
57

julia> readuntil("my_file.txt", 'L')
"Julia"

julia> readuntil("my_file.txt", '.', keep = true)
"JuliaLang is a GitHub organization."

julia> rm("my_file.txt")
```
"""
readuntil(filename::AbstractString, args...; kw...) = open(io->readuntil(io, args...; kw...), filename)

"""
    readline(io::IO=stdin; keep::Bool=false)
    readline(filename::AbstractString; keep::Bool=false)

Read a single line of text from the given I/O stream or file (defaults to `stdin`).
When reading from a file, the text is assumed to be encoded in UTF-8. Lines in the
input end with `'\\n'` or `"\\r\\n"` or the end of an input stream. When `keep` is
false (as it is by default), these trailing newline characters are removed from the
line before it is returned. When `keep` is true, they are returned as part of the
line.

# Examples
```jldoctest
julia> open("my_file.txt", "w") do io
           write(io, "JuliaLang is a GitHub organization.\\nIt has many members.\\n");
       end
57

julia> readline("my_file.txt")
"JuliaLang is a GitHub organization."

julia> readline("my_file.txt", keep=true)
"JuliaLang is a GitHub organization.\\n"

julia> rm("my_file.txt")
```
"""
function readline(filename::AbstractString; keep::Bool=false)
    open(filename) do f
        readline(f, keep=keep)
    end
end

function readline(s::IO=stdin; keep::Bool=false)::String
    line = readuntil(s, 0x0a, keep=true)
    i = length(line)
    if keep || i == 0 || line[i] != 0x0a
        return String(line)
    elseif i < 2 || line[i-1] != 0x0d
        return String(resize!(line,i-1))
    else
        return String(resize!(line,i-2))
    end
end

"""
    readlines(io::IO=stdin; keep::Bool=false)
    readlines(filename::AbstractString; keep::Bool=false)

Read all lines of an I/O stream or a file as a vector of strings. Behavior is
equivalent to saving the result of reading [`readline`](@ref) repeatedly with the same
arguments and saving the resulting lines as a vector of strings.

# Examples
```jldoctest
julia> open("my_file.txt", "w") do io
           write(io, "JuliaLang is a GitHub organization.\\nIt has many members.\\n");
       end
57

julia> readlines("my_file.txt")
2-element Array{String,1}:
 "JuliaLang is a GitHub organization."
 "It has many members."

julia> readlines("my_file.txt", keep=true)
2-element Array{String,1}:
 "JuliaLang is a GitHub organization.\\n"
 "It has many members.\\n"

julia> rm("my_file.txt")
```
"""
function readlines(filename::AbstractString; kw...)
    open(filename) do f
        readlines(f; kw...)
    end
end
readlines(s=stdin; kw...) = collect(eachline(s; kw...))

## byte-order mark, ntoh & hton ##

let a = UInt32[0x01020304]
    endian_bom = GC.@preserve a unsafe_load(convert(Ptr{UInt8}, pointer(a)))
    global ntoh, hton, ltoh, htol
    if endian_bom == 0x01
        ntoh(x) = x
        hton(x) = x
        ltoh(x) = bswap(x)
        htol(x) = bswap(x)
        const global ENDIAN_BOM = 0x01020304
    elseif endian_bom == 0x04
        ntoh(x) = bswap(x)
        hton(x) = bswap(x)
        ltoh(x) = x
        htol(x) = x
        const global ENDIAN_BOM = 0x04030201
    else
        error("seriously? what is this machine?")
    end
end

"""
    ENDIAN_BOM

The 32-bit byte-order-mark indicates the native byte order of the host machine.
Little-endian machines will contain the value `0x04030201`. Big-endian machines will contain
the value `0x01020304`.
"""
ENDIAN_BOM

"""
    ntoh(x)

Convert the endianness of a value from Network byte order (big-endian) to that used by the Host.
"""
ntoh(x)

"""
    hton(x)

Convert the endianness of a value from that used by the Host to Network byte order (big-endian).
"""
hton(x)

"""
    ltoh(x)

Convert the endianness of a value from Little-endian to that used by the Host.
"""
ltoh(x)

"""
    htol(x)

Convert the endianness of a value from that used by the Host to Little-endian.
"""
htol(x)


"""
    isreadonly(io) -> Bool

Determine whether a stream is read-only.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> isreadonly(io)
true

julia> io = IOBuffer();

julia> isreadonly(io)
false
```
"""
isreadonly(s) = isreadable(s) && !iswritable(s)

## binary I/O ##

write(io::IO, x) = throw(MethodError(write, (io, x)))
function write(io::IO, x1, xs...)
    written::Int = write(io, x1)
    for x in xs
        written += write(io, x)
    end
    return written
end

@noinline unsafe_write(s::IO, p::Ref{T}, n::Integer) where {T} =
    unsafe_write(s, unsafe_convert(Ref{T}, p)::Ptr, n) # mark noinline to ensure ref is gc-rooted somewhere (by the caller)
unsafe_write(s::IO, p::Ptr, n::Integer) = unsafe_write(s, convert(Ptr{UInt8}, p), convert(UInt, n))
write(s::IO, x::Ref{T}) where {T} = unsafe_write(s, x, Core.sizeof(T))
write(s::IO, x::Int8) = write(s, reinterpret(UInt8, x))
function write(s::IO, x::Union{Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128,Float16,Float32,Float64})
    return write(s, Ref(x))
end

write(s::IO, x::Bool) = write(s, UInt8(x))
write(to::IO, p::Ptr) = write(to, convert(UInt, p))

function write(s::IO, A::AbstractArray)
    if !isbitstype(eltype(A))
        error("`write` is not supported on non-isbits arrays")
    end
    nb = 0
    for a in A
        nb += write(s, a)
    end
    return nb
end

function write(s::IO, a::Array)
    if isbitstype(eltype(a))
        return GC.@preserve a unsafe_write(s, pointer(a), sizeof(a))
    else
        error("`write` is not supported on non-isbits arrays")
    end
end

function write(s::IO, a::SubArray{T,N,<:Array}) where {T,N}
    if !isbitstype(T) || !isa(a, StridedArray)
        return invoke(write, Tuple{IO, AbstractArray}, s, a)
    end
    elsz = elsize(a)
    colsz = size(a,1) * elsz
    GC.@preserve a if stride(a,1) != 1
        for idxs in CartesianIndices(size(a))
            unsafe_write(s, pointer(a, idxs), elsz)
        end
        return elsz * length(a)
    elseif N <= 1
        return unsafe_write(s, pointer(a, 1), colsz)
    else
        for colstart in CartesianIndices((1, size(a)[2:end]...))
            unsafe_write(s, pointer(a, colstart), colsz)
        end
        return colsz * trailingsize(a,2)
    end
end

function write(io::IO, c::Char)
    u = bswap(reinterpret(UInt32, c))
    n = 1
    while true
        write(io, u % UInt8)
        (u >>= 8) == 0 && return n
        n += 1
    end
end
# write(io, ::AbstractChar) is not defined: implementations
# must provide their own encoding-specific method.

function write(io::IO, s::Symbol)
    pname = unsafe_convert(Ptr{UInt8}, s)
    return unsafe_write(io, pname, Int(ccall(:strlen, Csize_t, (Cstring,), pname)))
end

function write(to::IO, from::IO)
    n = 0
    while !eof(from)
        n += write(to, readavailable(from))
    end
    return n
end

@noinline unsafe_read(s::IO, p::Ref{T}, n::Integer) where {T} = unsafe_read(s, unsafe_convert(Ref{T}, p)::Ptr, n) # mark noinline to ensure ref is gc-rooted somewhere (by the caller)
unsafe_read(s::IO, p::Ptr, n::Integer) = unsafe_read(s, convert(Ptr{UInt8}, p), convert(UInt, n))
read!(s::IO, x::Ref{T}) where {T} = (unsafe_read(s, x, Core.sizeof(T)); x)

read(s::IO, ::Type{Int8}) = reinterpret(Int8, read(s, UInt8))
function read(s::IO, T::Union{Type{Int16},Type{UInt16},Type{Int32},Type{UInt32},Type{Int64},Type{UInt64},Type{Int128},Type{UInt128},Type{Float16},Type{Float32},Type{Float64}})
    return read!(s, Ref{T}(0))[]::T
end

read(s::IO, ::Type{Bool}) = (read(s, UInt8) != 0)
read(s::IO, ::Type{Ptr{T}}) where {T} = convert(Ptr{T}, read(s, UInt))

function read!(s::IO, a::Array{UInt8})
    GC.@preserve a unsafe_read(s, pointer(a), sizeof(a))
    return a
end

function read!(s::IO, a::AbstractArray{T}) where T
    if isbitstype(T) && (a isa Array || a isa FastContiguousSubArray{T,<:Any,<:Array{T}})
        GC.@preserve a unsafe_read(s, pointer(a), sizeof(a))
    else
        for i in eachindex(a)
            a[i] = read(s, T)
        end
    end
    return a
end

function read(io::IO, ::Type{Char})
    b0 = read(io, UInt8)
    l = 8(4-leading_ones(b0))
    c = UInt32(b0) << 24
    if l < 24
        s = 16
        while s ≥ l && !eof(io)
            peek(io) & 0xc0 == 0x80 || break
            b = read(io, UInt8)
            c |= UInt32(b) << s
            s -= 8
        end
    end
    return reinterpret(Char, c)
end
# read(io, T) is not defined for other AbstractChar: implementations
# must provide their own encoding-specific method.

# readuntil_string is useful below since it has
# an optimized method for s::IOStream
readuntil_string(s::IO, delim::UInt8, keep::Bool) = String(readuntil(s, delim, keep=keep))

function readuntil(s::IO, delim::AbstractChar; keep::Bool=false)
    if delim ≤ '\x7f'
        return readuntil_string(s, delim % UInt8, keep)
    end
    out = IOBuffer()
    while !eof(s)
        c = read(s, Char)
        if c == delim
            keep && write(out, c)
            break
        end
        write(out, c)
    end
    return String(take!(out))
end

function readuntil(s::IO, delim::T; keep::Bool=false) where T
    out = (T === UInt8 ? StringVector(0) : Vector{T}())
    while !eof(s)
        c = read(s, T)
        if c == delim
            keep && push!(out, c)
            break
        end
        push!(out, c)
    end
    return out
end

# requires that indices for target are the integer unit range from firstindex to lastindex
# returns whether the delimiter was matched
# uses the Knuth–Morris–Pratt_algorithm, with the first and second cache entries unrolled
# For longer targets, the cache improves the big-O efficiency of scanning of sequences
# with repeated patterns
# Each cache entry tells us which index we should start the search at.
# We assume this is unlikely, so we only lazy-initialize as much of the cache as we need to use
# When we allocate the cache, we initialize it to 0 (and offset by the first index afterwards).
# Suppose target is:
#    Index:  1245689
#    Value: "aδcaδcx"
# We would set the cache to
#    0 0 0 1 2 3 4 0
# So after if we mismatch after the second aδc sequence,
# we can immediately jump back to index 5 (4 + 1).
function readuntil_vector!(io::IO, target::AbstractVector{T}, keep::Bool, out) where {T}
    first = firstindex(target)
    last = lastindex(target)
    len = last - first + 1
    if len < 1
        return true
    end
    pos = 0 # array-offset
    max_pos = 1 # array-offset in cache
    local cache # will be lazy initialized when needed
    output! = (isa(out, IO) ? write : push!)
    while !eof(io)
        c = read(io, T)
        # Backtrack until the next target character matches what was found
        while true
            c1 = target[pos + first]
            if c == c1
                pos += 1
                break
            elseif pos == 0
                break
            elseif pos == 1
                if !keep
                    output!(out, target[first])
                end
                pos = 0
            else
                # grow cache to contain up to `pos`
                if !@isdefined(cache)
                    cache = zeros(Int, len)
                end
                while max_pos < pos
                    ci = target[max_pos + first]
                    b = max_pos
                    max_pos += 1
                    while b != 0
                        b = cache[b]
                        cb = target[b + first]
                        if ci == cb
                            cache[max_pos] = b + 1
                            break
                        end
                    end
                end
                # read new position from cache
                pos1 = cache[pos]
                if !keep
                    # and add the removed prefix from the target to the output
                    # if not always keeping the match
                    for b in 1:(pos - pos1)
                        output!(out, target[b - 1 + first])
                    end
                end
                pos = pos1
            end
        end
        if keep || pos == 0
            output!(out, c)
        end
        pos == len && return true
    end
    if !keep
        # failed early without finishing the match,
        # add the partial match to the output
        # if not always keeping the match
        for b in 1:pos
            output!(out, target[b - 1 + first])
        end
    end
    return false
end

function readuntil(io::IO, target::AbstractString; keep::Bool=false)
    # small-string target optimizations
    isempty(target) && return ""
    c, rest = Iterators.peel(target)
    if isempty(rest) && c <= '\x7f'
        return readuntil_string(io, c % UInt8, keep)
    end
    # convert String to a utf8-byte-iterator
    if !(target isa String) && !(target isa SubString{String})
        target = String(target)
    end
    target = codeunits(target)::AbstractVector
    return String(readuntil(io, target, keep=keep))
end

function readuntil(io::IO, target::AbstractVector{T}; keep::Bool=false) where T
    out = (T === UInt8 ? StringVector(0) : Vector{T}())
    readuntil_vector!(io, target, keep, out)
    return out
end

"""
    readchomp(x)

Read the entirety of `x` as a string and remove a single trailing newline
if there is one. Equivalent to `chomp(read(x, String))`.

# Examples
```jldoctest
julia> open("my_file.txt", "w") do io
           write(io, "JuliaLang is a GitHub organization.\\nIt has many members.\\n");
       end;

julia> readchomp("my_file.txt")
"JuliaLang is a GitHub organization.\\nIt has many members."

julia> rm("my_file.txt");
```
"""
readchomp(x) = chomp(read(x, String))

# read up to nb bytes into nb, returning # bytes read

"""
    readbytes!(stream::IO, b::AbstractVector{UInt8}, nb=length(b))

Read at most `nb` bytes from `stream` into `b`, returning the number of bytes read.
The size of `b` will be increased if needed (i.e. if `nb` is greater than `length(b)`
and enough bytes could be read), but it will never be decreased.
"""
function readbytes!(s::IO, b::AbstractArray{UInt8}, nb=length(b))
    require_one_based_indexing(b)
    olb = lb = length(b)
    nr = 0
    while nr < nb && !eof(s)
        a = read(s, UInt8)
        nr += 1
        if nr > lb
            lb = nr * 2
            resize!(b, lb)
        end
        b[nr] = a
    end
    if lb > olb
        resize!(b, nr) # shrink to just contain input data if was resized
    end
    return nr
end

"""
    read(s::IO, nb=typemax(Int))

Read at most `nb` bytes from `s`, returning a `Vector{UInt8}` of the bytes read.
"""
function read(s::IO, nb::Integer = typemax(Int))
    # Let readbytes! grow the array progressively by default
    # instead of taking of risk of over-allocating
    b = Vector{UInt8}(undef, nb == typemax(Int) ? 1024 : nb)
    nr = readbytes!(s, b, nb)
    return resize!(b, nr)
end

read(s::IO, ::Type{String}) = String(read(s))
read(s::IO, T::Type) = error("The IO stream does not support reading objects of type $T.")

## high-level iterator interfaces ##

struct EachLine{IOT <: IO}
    stream::IOT
    ondone::Function
    keep::Bool
    EachLine(stream::IO=stdin; ondone::Function=()->nothing, keep::Bool=false) =
        new{typeof(stream)}(stream, ondone, keep)
end

"""
    eachline(io::IO=stdin; keep::Bool=false)
    eachline(filename::AbstractString; keep::Bool=false)

Create an iterable `EachLine` object that will yield each line from an I/O stream
or a file. Iteration calls [`readline`](@ref) on the stream argument repeatedly with
`keep` passed through, determining whether trailing end-of-line characters are
retained. When called with a file name, the file is opened once at the beginning of
iteration and closed at the end. If iteration is interrupted, the file will be
closed when the `EachLine` object is garbage collected.

# Examples
```jldoctest
julia> open("my_file.txt", "w") do io
           write(io, "JuliaLang is a GitHub organization.\\n It has many members.\\n");
       end;

julia> for line in eachline("my_file.txt")
           print(line)
       end
JuliaLang is a GitHub organization. It has many members.

julia> rm("my_file.txt");
```
"""
function eachline(stream::IO=stdin; keep::Bool=false)
    EachLine(stream, keep=keep)::EachLine
end

function eachline(filename::AbstractString; keep::Bool=false)
    s = open(filename)
    EachLine(s, ondone=()->close(s), keep=keep)::EachLine
end

function iterate(itr::EachLine, state=nothing)
    eof(itr.stream) && return (itr.ondone(); nothing)
    (readline(itr.stream, keep=itr.keep), nothing)
end

eltype(::Type{<:EachLine}) = String

IteratorSize(::Type{<:EachLine}) = SizeUnknown()

# IOStream Marking
# Note that these functions expect that io.mark exists for
# the concrete IO type. This may not be true for IO types
# not in base.

"""
    mark(s)

Add a mark at the current position of stream `s`. Return the marked position.

See also [`unmark`](@ref), [`reset`](@ref), [`ismarked`](@ref).
"""
function mark(io::IO)
    io.mark = position(io)
end

"""
    unmark(s)

Remove a mark from stream `s`. Return `true` if the stream was marked, `false` otherwise.

See also [`mark`](@ref), [`reset`](@ref), [`ismarked`](@ref).
"""
function unmark(io::IO)
    !ismarked(io) && return false
    io.mark = -1
    return true
end

"""
    reset(s)

Reset a stream `s` to a previously marked position, and remove the mark. Return the
previously marked position. Throw an error if the stream is not marked.

See also [`mark`](@ref), [`unmark`](@ref), [`ismarked`](@ref).
"""
function reset(io::T) where T<:IO
    ismarked(io) || throw(ArgumentError("$T not marked"))
    m = io.mark
    seek(io, m)
    io.mark = -1 # must be after seek, or seek may fail
    return m
end

"""
    ismarked(s)

Return `true` if stream `s` is marked.

See also [`mark`](@ref), [`unmark`](@ref), [`reset`](@ref).
"""
ismarked(io::IO) = io.mark >= 0

# Make sure all IO streams support flush, even if only as a no-op,
# to make it easier to write generic I/O code.

"""
    flush(stream)

Commit all currently buffered writes to the given stream.
"""
flush(io::IO) = nothing

"""
    skipchars(predicate, io::IO; linecomment=nothing)

Advance the stream `io` such that the next-read character will be the first remaining for
which `predicate` returns `false`. If the keyword argument `linecomment` is specified, all
characters from that character until the start of the next line are ignored.

# Examples
```jldoctest
julia> buf = IOBuffer("    text")
IOBuffer(data=UInt8[...], readable=true, writable=false, seekable=true, append=false, size=8, maxsize=Inf, ptr=1, mark=-1)

julia> skipchars(isspace, buf)
IOBuffer(data=UInt8[...], readable=true, writable=false, seekable=true, append=false, size=8, maxsize=Inf, ptr=5, mark=-1)

julia> String(readavailable(buf))
"text"
```
"""
function skipchars(predicate, io::IO; linecomment=nothing)
    while !eof(io)
        c = read(io, Char)
        if c === linecomment
            readline(io)
        elseif !predicate(c)
            skip(io, -ncodeunits(c))
            break
        end
    end
    return io
end

"""
    countlines(io::IO; eol::AbstractChar = '\\n')

Read `io` until the end of the stream/file and count the number of lines. To specify a file
pass the filename as the first argument. EOL markers other than `'\\n'` are supported by
passing them as the second argument.  The last non-empty line of `io` is counted even if it does not
end with the EOL, matching the length returned by [`eachline`](@ref) and [`readlines`](@ref).

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization.\\n");

julia> countlines(io)
1

julia> io = IOBuffer("JuliaLang is a GitHub organization.");

julia> countlines(io)
1

julia> countlines(io, eol = '.')
0
```
"""
function countlines(io::IO; eol::AbstractChar='\n')
    isascii(eol) || throw(ArgumentError("only ASCII line terminators are supported"))
    aeol = UInt8(eol)
    a = Vector{UInt8}(undef, 8192)
    nl = nb = 0
    while !eof(io)
        nb = readbytes!(io, a)
        @simd for i=1:nb
            @inbounds nl += a[i] == aeol
        end
    end
    if nb > 0 && a[nb] != aeol
        nl += 1 # final line is not terminated with eol
    end
    nl
end

countlines(f::AbstractString; eol::AbstractChar = '\n') = open(io->countlines(io, eol = eol), f)::Int
