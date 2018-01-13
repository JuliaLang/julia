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

Determine whether an object - such as a stream, timer, or [`mmap`](@ref Mmap.mmap)
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
function wait_connected end
function wait_readnb end
function wait_readbyte end
function wait_close end
function nb_available end

"""
    readavailable(stream)

Read all available data on the stream, blocking the task only if no data is available. The
result is a `Vector{UInt8,1}`.
"""
function readavailable end

"""
    isreadable(io) -> Bool

Return `true` if the specified IO object is readable (if that can be determined).

# Examples
```jldoctest
julia> open("myfile.txt", "w") do io
           write(io, "Hello world!");
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
           write(io, "Hello world!");
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

    read(io::IO, String)

Read the entirety of `io`, as a `String`.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> read(io, Char)
'J': ASCII/Unicode U+004a (category Lu: Letter, uppercase)

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
Return the number of bytes written into the stream.

You can write multiple values with the same `write` call. i.e. the following are equivalent:

    write(io, x, y...)
    write(io, x) + write(io, y...)

# Examples
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
    local written::Int = 0
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
readuntil(io::AbstractPipe, arg::UInt8) = readuntil(pipe_reader(io), arg)
readuntil(io::AbstractPipe, arg::Char) = readuntil(pipe_reader(io), arg)
readuntil_indexable(io::AbstractPipe, target#=::Indexable{T}=#, out) = readuntil_indexable(pipe_reader(io), target, out)

readavailable(io::AbstractPipe) = readavailable(pipe_reader(io))

isreadable(io::AbstractPipe) = isreadable(pipe_reader(io))
iswritable(io::AbstractPipe) = iswritable(pipe_writer(io))
isopen(io::AbstractPipe) = isopen(pipe_writer(io)) || isopen(pipe_reader(io))
close(io::AbstractPipe) = (close(pipe_writer(io)); close(pipe_reader(io)))
wait_readnb(io::AbstractPipe, nb::Int) = wait_readnb(pipe_reader(io), nb)
wait_readbyte(io::AbstractPipe, byte::UInt8) = wait_readbyte(pipe_reader(io), byte)
wait_close(io::AbstractPipe) = (wait_close(pipe_writer(io)); wait_close(pipe_reader(io)))

"""
    nb_available(io)

Return the number of bytes available for reading before a read from this stream or buffer will block.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization");

julia> nb_available(io)
34
```
"""
nb_available(io::AbstractPipe) = nb_available(pipe_reader(io))

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

"""
    read!(stream::IO, array::Union{Array, BitArray})
    read!(filename::AbstractString, array::Union{Array, BitArray})

Read binary data from an I/O stream or file, filling in `array`.
"""
function read! end

read!(filename::AbstractString, a) = open(io->read!(io, a), filename)

"""
    readuntil(stream::IO, delim)
    readuntil(filename::AbstractString, delim)

Read a string from an I/O stream or a file, up to and including the given delimiter byte.
The text is assumed to be encoded in UTF-8.

# Examples
```jldoctest
julia> open("my_file.txt", "w") do io
           write(io, "JuliaLang is a GitHub organization.\\nIt has many members.\\n");
       end
57

julia> readuntil("my_file.txt", 'L')
"JuliaL"

julia> readuntil("my_file.txt", '.')
"JuliaLang is a GitHub organization."

julia> rm("my_file.txt")
```
"""
readuntil(filename::AbstractString, args...) = open(io->readuntil(io, args...), filename)

"""
    readline(io::IO=STDIN; chomp::Bool=true)
    readline(filename::AbstractString; chomp::Bool=true)

Read a single line of text from the given I/O stream or file (defaults to `STDIN`).
When reading from a file, the text is assumed to be encoded in UTF-8. Lines in the
input end with `'\\n'` or `"\\r\\n"` or the end of an input stream. When `chomp` is
true (as it is by default), these trailing newline characters are removed from the
line before it is returned. When `chomp` is false, they are returned as part of the
line.

# Examples
```jldoctest
julia> open("my_file.txt", "w") do io
           write(io, "JuliaLang is a GitHub organization.\\nIt has many members.\\n");
       end
57

julia> readline("my_file.txt")
"JuliaLang is a GitHub organization."

julia> readline("my_file.txt", chomp=false)
"JuliaLang is a GitHub organization.\\n"

julia> rm("my_file.txt")
```
"""
function readline(filename::AbstractString; chomp::Bool=true)
    open(filename) do f
        readline(f, chomp=chomp)
    end
end

function readline(s::IO=STDIN; chomp::Bool=true)
    line = readuntil(s, 0x0a)
    i = length(line)
    if !chomp || i == 0 || line[i] != 0x0a
        return String(line)
    elseif i < 2 || line[i-1] != 0x0d
        return String(resize!(line,i-1))
    else
        return String(resize!(line,i-2))
    end
end

"""
    readlines(io::IO=STDIN; chomp::Bool=true)
    readlines(filename::AbstractString; chomp::Bool=true)

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

julia> readlines("my_file.txt", chomp=false)
2-element Array{String,1}:
 "JuliaLang is a GitHub organization.\\n"
 "It has many members.\\n"

julia> rm("my_file.txt")
```
"""
function readlines(filename::AbstractString; chomp::Bool=true)
    open(filename) do f
        readlines(f, chomp=chomp)
    end
end
readlines(s=STDIN; chomp::Bool=true) = collect(eachline(s, chomp=chomp))

## byte-order mark, ntoh & hton ##

let a = UInt32[0x01020304]
    endian_bom = @gc_preserve a unsafe_load(convert(Ptr{UInt8}, pointer(a)))
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

Converts the endianness of a value from Network byte order (big-endian) to that used by the Host.
"""
ntoh(x)

"""
    hton(x)

Converts the endianness of a value from that used by the Host to Network byte order (big-endian).
"""
hton(x)

"""
    ltoh(x)

Converts the endianness of a value from Little-endian to that used by the Host.
"""
ltoh(x)

"""
    htol(x)

Converts the endianness of a value from that used by the Host to Little-endian.
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
    local written::Int = 0
    written += write(io, x1)
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
    if !isbits(eltype(A))
        depwarn("Calling `write` on non-isbits arrays is deprecated. Use a loop or `serialize` instead.", :write)
    end
    nb = 0
    for a in A
        nb += write(s, a)
    end
    return nb
end

function write(s::IO, a::Array)
    if isbits(eltype(a))
        return @gc_preserve a unsafe_write(s, pointer(a), sizeof(a))
    else
        depwarn("Calling `write` on non-isbits arrays is deprecated. Use a loop or `serialize` instead.", :write)
        nb = 0
        for b in a
            nb += write(s, b)
        end
        return nb
    end
end

function write(s::IO, a::SubArray{T,N,<:Array}) where {T,N}
    if !isbits(T)
        return invoke(write, Tuple{IO, AbstractArray}, s, a)
    end
    elsz = sizeof(T)
    colsz = size(a,1) * elsz
    @gc_preserve a if stride(a,1) != 1
        for idxs in CartesianIndices(size(a))
            unsafe_write(s, pointer(a, idxs.I), elsz)
        end
        return elsz * length(a)
    elseif N <= 1
        return unsafe_write(s, pointer(a, 1), colsz)
    else
        for idxs in CartesianIndices((1, size(a)[2:end]...))
            unsafe_write(s, pointer(a, idxs.I), colsz)
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
    @gc_preserve a unsafe_read(s, pointer(a), sizeof(a))
    return a
end

function read!(s::IO, a::Array{T}) where T
    if isbits(T)
        @gc_preserve a unsafe_read(s, pointer(a), sizeof(a))
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

# readuntil_string is useful below since it has
# an optimized method for s::IOStream
readuntil_string(s::IO, delim::UInt8) = String(readuntil(s, delim))

function readuntil(s::IO, delim::Char)
    if delim ≤ '\x7f'
        return readuntil_string(s, delim % UInt8)
    end
    out = IOBuffer()
    while !eof(s)
        c = read(s, Char)
        write(out, c)
        if c == delim
            break
        end
    end
    return String(take!(out))
end

function readuntil(s::IO, delim::T) where T
    out = (T === UInt8 ? StringVector(0) : Vector{T}())
    while !eof(s)
        c = read(s, T)
        push!(out, c)
        if c == delim
            break
        end
    end
    return out
end

# requires that indices for target are small ordered integers bounded by start and endof
function readuntil_indexable(io::IO, target#=::Indexable{T}=#, out)
    T = eltype(target)
    first = start(target)
    if done(target, first)
        return
    end
    len = endof(target)
    local cache # will be lazy initialized when needed
    second = next(target, first)[2]
    max_pos = second
    pos = first
    while !eof(io)
        c = read(io, T)
        # Backtrack until the next target character matches what was found
        if out isa IO
            write(out, c)
        else
            push!(out, c)
        end
        while true
            c1, pos1 = next(target, pos)
            if c == c1
                pos = pos1
                break
            elseif pos == first
                break
            elseif pos == second
                pos = first
            else
                # grow cache to contain up to `pos`
                if !@isdefined(cache)
                    cache = zeros(Int, len)
                end
                while max_pos < pos
                    b = cache[max_pos] + first
                    cb, b1 = next(target, b)
                    ci, max_pos1 = next(target, max_pos)
                    if ci == cb
                        cache[max_pos1] = b1 - first
                    end
                    max_pos = max_pos1
                end
                pos = cache[pos] + first
            end
        end
        done(target, pos) && break
    end
end

function readuntil(io::IO, target::AbstractString)
    # small-string target optimizations
    i = start(target)
    done(target, i) && return ""
    c, i = next(target, start(target))
    if done(target, i) && c <= '\x7f'
        return readuntil_string(io, c % UInt8)
    end
    # decide how we can index target
    if target isa String
        # convert String to a utf8-byte-iterator
        target = codeunits(target)
    #elseif applicable(codeunit, target)
    #   TODO: a more general version of above optimization
    #         would be to permit accessing any string via codeunit
    #   target = CodeUnitVector(target)
    elseif !(target isa SubString{String})
        # type with unknown indexing behavior: convert to array
        target = collect(target)
    end
    out = (eltype(target) === UInt8 ? StringVector(0) : IOBuffer())
    readuntil_indexable(io, target, out)
    out = isa(out, IO) ? take!(out) : out
    return String(out)
end

function readuntil(io::IO, target::AbstractVector{T}) where T
    out = (T === UInt8 ? StringVector(0) : Vector{T}())
    readuntil_indexable(io, target, out)
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
    b = Vector{UInt8}(uninitialized, nb == typemax(Int) ? 1024 : nb)
    nr = readbytes!(s, b, nb)
    return resize!(b, nr)
end

read(s::IO, ::Type{String}) = String(read(s))
read(s::IO, T::Type) = error("The IO stream does not support reading objects of type $T.")

## high-level iterator interfaces ##

mutable struct EachLine
    stream::IO
    ondone::Function
    chomp::Bool

    EachLine(stream::IO=STDIN; ondone::Function=()->nothing, chomp::Bool=true) =
        new(stream, ondone, chomp)
end

"""
    eachline(io::IO=STDIN; chomp::Bool=true)
    eachline(filename::AbstractString; chomp::Bool=true)

Create an iterable `EachLine` object that will yield each line from an I/O stream
or a file. Iteration calls [`readline`](@ref) on the stream argument repeatedly with
`chomp` passed through, determining whether trailing end-of-line characters are
removed. When called with a file name, the file is opened once at the beginning of
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
eachline(stream::IO=STDIN; chomp::Bool=true) = EachLine(stream, chomp=chomp)::EachLine

function eachline(filename::AbstractString; chomp::Bool=true)
    s = open(filename)
    EachLine(s, ondone=()->close(s), chomp=chomp)::EachLine
end

start(itr::EachLine) = nothing
function done(itr::EachLine, ::Nothing)
    eof(itr.stream) || return false
    itr.ondone()
    true
end
next(itr::EachLine, ::Nothing) = (readline(itr.stream, chomp=itr.chomp), nothing)

eltype(::Type{EachLine}) = String

IteratorSize(::Type{EachLine}) = SizeUnknown()

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
    ismarked(io) || throw(ArgumentError("$(T) not marked"))
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
    skipchars(io::IO, predicate; linecomment=nothing)

Advance the stream `io` such that the next-read character will be the first remaining for
which `predicate` returns `false`. If the keyword argument `linecomment` is specified, all
characters from that character until the start of the next line are ignored.

# Examples
```jldoctest
julia> buf = IOBuffer("    text")
IOBuffer(data=UInt8[...], readable=true, writable=false, seekable=true, append=false, size=8, maxsize=Inf, ptr=1, mark=-1)

julia> skipchars(buf, isspace)
IOBuffer(data=UInt8[...], readable=true, writable=false, seekable=true, append=false, size=8, maxsize=Inf, ptr=5, mark=-1)

julia> String(readavailable(buf))
"text"
```
"""
function skipchars(io::IO, pred; linecomment=nothing)
    while !eof(io)
        c = read(io, Char)
        if c === linecomment
            readline(io)
        elseif !pred(c)
            skip(io, -codelen(c))
            break
        end
    end
    return io
end

"""
    countlines(io::IO, eol::Char='\\n')

Read `io` until the end of the stream/file and count the number of lines. To specify a file
pass the filename as the first argument. EOL markers other than `'\\n'` are supported by
passing them as the second argument.

# Examples
```jldoctest
julia> io = IOBuffer("JuliaLang is a GitHub organization.\n");

julia> countlines(io)
1

julia> io = IOBuffer("JuliaLang is a GitHub organization.");

julia> countlines(io)
0

julia> countlines(io, '.')
1
```
"""
function countlines(io::IO, eol::Char='\n')
    isascii(eol) || throw(ArgumentError("only ASCII line terminators are supported"))
    aeol = UInt8(eol)
    a = Vector{UInt8}(uninitialized, 8192)
    nl = 0
    while !eof(io)
        nb = readbytes!(io, a)
        @simd for i=1:nb
            @inbounds nl += a[i] == aeol
        end
    end
    nl
end

countlines(f::AbstractString, eol::Char='\n') = open(io->countlines(io,eol), f)::Int
