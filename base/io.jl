abstract IO

const sizeof_off_t = int(ccall(:jl_sizeof_off_t, Int32, ()))
const sizeof_ios_t = int(ccall(:jl_sizeof_ios_t, Int32, ()))
const sizeof_fd_set = int(ccall(:jl_sizeof_fd_set, Int32, ()))

if sizeof_off_t == 4
    typealias FileOffset Int32
else
    typealias FileOffset Int64
end

type IOStream <: IO
    # NOTE: for some reason the order of these field is significant!?
    ios::Array{Uint8,1}
    name::String

    IOStream(name::String, buf::Array{Uint8,1}) = new(buf, name)

    # TODO: delay adding finalizer, e.g. for memio with a small buffer, or
    # in the case where we takebuf it.
    function IOStream(name::String, finalize::Bool)
        x = new(zeros(Uint8,sizeof_ios_t), name)
        if finalize
            finalizer(x, close)
        end
        return x
    end
    IOStream(name::String) = IOStream(name, true)
end

# "own" means the descriptor will be closed with the IOStream
function fdio(name::String, fd::Integer, own::Bool)
    s = IOStream(name)
    ccall(:ios_fd, Void, (Ptr{Uint8}, Int, Int32, Int32),
          s.ios, fd, 0, own);
    return s
end
fdio(name::String, fd::Integer) = fdio(name, fd, false)
fdio(fd::Integer, own::Bool) = fdio(strcat("<fd ",fd,">"), fd, own)
fdio(fd::Integer) = fdio(fd, false)

make_stdin_stream() = fdio("<stdin>", ccall(:jl_stdin, Int32, ()))
make_stderr_stream() = fdio("<stderr>", ccall(:jl_stderr, Int32, ()))
make_stdout_stream() = IOStream("<stdout>", ccall(:jl_stdout_stream, Any, ()))

show(s::IOStream) = print("IOStream(",s.name,")")

fd(s::IOStream) = ccall(:jl_ios_fd, Int, (Ptr{Void},), s.ios)
close(s::IOStream) = ccall(:ios_close, Void, (Ptr{Void},), s.ios)

function open(fname::String, rd::Bool, wr::Bool, cr::Bool, tr::Bool, ff::Bool)
    s = IOStream(strcat("<file ",fname,">"))
    if ccall(:ios_file, Ptr{Void},
             (Ptr{Uint8}, Ptr{Uint8}, Int32, Int32, Int32, Int32),
             s.ios, fname, rd, wr, cr, tr) == C_NULL
        error("could not open file ", fname)
    end
    if ff && ccall(:ios_seek_end, FileOffset, (Ptr{Void},), s.ios) != 0
        error("error seeking to end of file ", fname)
    end
    return s
end
open(fname::String) = open(fname, true, false, false, false, false)

function open(fname::String, mode::String)
    mode == "r"  ? open(fname, true,  false, false, false, false) :
    mode == "r+" ? open(fname, true,  true , false, false, false) :
    mode == "w"  ? open(fname, false, true , true , true , false) :
    mode == "w+" ? open(fname, true,  true , true , true , false) :
    mode == "a"  ? open(fname, false, true , true , false, true ) :
    mode == "a+" ? open(fname, true,  true , true , false, true ) :
    error("invalid open mode: ", mode)
end

function memio(x::Integer, finalize::Bool)
    s = IOStream("<memio>", finalize)
    ccall(:jl_ios_mem, Ptr{Void}, (Ptr{Uint8}, Uint), s.ios, x)
    return s
end
memio(x::Integer) = memio(x, true)
memio() = memio(0, true)

convert(T::Type{Ptr}, s::IOStream) = convert(T, s.ios)

current_output_stream() = ccall(:jl_current_output_stream_obj, IOStream, ())

set_current_output_stream(s::IOStream) =
    ccall(:jl_set_current_output_stream_obj, Void, (Any,), s)

function with_output_stream(s::IOStream, f::Function, args...)
    try
        set_current_output_stream(s)
        f(args...)
    catch e
        throw(e)
    end
end

# custom version for print_to_*
function _jl_with_output_stream(s::IOStream, f::Function, args...)
    try
        set_current_output_stream(s)
        f(args...)
    catch e
        # only add finalizer if takebuf doesn't happen
        finalizer(s, close)
        throw(e)
    end
end

takebuf_array(s::IOStream) =
    ccall(:jl_takebuf_array, Any, (Ptr{Void},), s.ios)::Array{Uint8,1}

takebuf_string(s::IOStream) =
    ccall(:jl_takebuf_string, ByteString, (Ptr{Void},), s.ios)

function print_to_array(size::Integer, f::Function, args...)
    s = memio(size, false)
    _jl_with_output_stream(s, f, args...)
    takebuf_array(s)
end

function print_to_string(size::Integer, f::Function, args...)
    s = memio(size, false)
    _jl_with_output_stream(s, f, args...)
    takebuf_string(s)
end

print_to_array(f::Function, args...) = print_to_array(0, f, args...)
print_to_string(f::Function, args...) = print_to_string(0, f, args...)

nthbyte(x::Integer, n::Integer) = (n > sizeof(x) ? uint8(0) : uint8((x>>>((n-1)<<3))))

write(x) = write(current_output_stream(), x)
write(s, x::Uint8) = error(typeof(s)," does not support byte I/O")

function write(s, x::Integer)
    for n = 1:sizeof(x)
        write(s, nthbyte(x, n))
    end
end

write(s, x::Bool)    = write(s, uint8(x))
write(s, x::Float32) = write(s, boxsi32(unbox32(x)))
write(s, x::Float64) = write(s, boxsi64(unbox64(x)))

function write(s, a::AbstractArray)
    for i = 1:numel(a)
        write(s, a[i])
    end
end

read(s, x::Type{Uint8}) = error(typeof(s)," does not support byte I/O")

function read{T <: Integer}(s, ::Type{T})
    x = zero(T)
    for n = 1:sizeof(x)
        x |= (convert(T,read(s,Uint8))<<((n-1)<<3))
    end
    return x
end

read(s, ::Type{Bool})    = (read(s,Uint8)!=0)
read(s, ::Type{Float32}) = boxf32(unbox32(read(s,Int32)))
read(s, ::Type{Float64}) = boxf64(unbox64(read(s,Int64)))

read{T}(s, t::Type{T}, d1::Int, dims::Int...) =
    read(s, t, tuple(d1,dims...))
read{T}(s, t::Type{T}, d1::Integer, dims::Integer...) =
    read(s, t, map(int,tuple(d1,dims...)))

read{T}(s, ::Type{T}, dims::Dims) = read(s, Array(T, dims))

function read{T}(s, a::Array{T})
    for i = 1:numel(a)
        a[i] = read(s, T)
    end
    return a
end

## low-level calls ##

write(s::IOStream, b::Uint8) = ccall(:ios_putc, Int32, (Int32, Ptr{Void}), b, s.ios)
write(s::IOStream, c::Char) = ccall(:ios_pututf8, Int32, (Ptr{Void}, Char), s.ios, c)

function write{T}(s::IOStream, a::Array{T})
    if isa(T,BitsKind)
        ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint),
              s.ios, a, numel(a)*sizeof(T))
    else
        invoke(write, (Any, Array), s, a)
    end
end

function write(s::IOStream, p::Ptr, nb::Integer)
    ccall(:ios_write, Uint, (Ptr{Void}, Ptr{Void}, Uint), s.ios, p, nb)
end

function write{T,N}(s::IOStream, a::SubArray{T,N,Array})
    if !isa(T,BitsKind) || stride(a,1)!=1
        return invoke(write, (Any, AbstractArray), s, a)
    end
    colsz = size(a,1)*sizeof(T)
    if N==1
        write(s, pointer(a, 1), colsz)
    else
        cartesian_map((idxs...)->write(s, pointer(a, idxs), colsz),
                      tuple(1, size(a)[2:]...))
    end
end

# num bytes available without blocking
nb_available(s::IOStream) = ccall(:jl_nb_available, Int32, (Ptr{Void},), s.ios)

function read(s::IOStream, ::Type{Uint8})
    b = ccall(:ios_getc, Int32, (Ptr{Void},), s.ios)
    if b == -1
        throw(EOFError())
    end
    uint8(b)
end

read(s::IOStream, ::Type{Char}) = ccall(:jl_getutf8, Char, (Ptr{Void},), s.ios)

function read{T<:Union(Int8,Uint8,Int16,Uint16,Int32,Uint32,Int64,Uint64,Float32,Float64,Complex64,Complex128)}(s::IOStream, a::Array{T})
    if isa(T,BitsKind)
        nb = numel(a)*sizeof(T)
        if ccall(:ios_readall, Uint,
                 (Ptr{Void}, Ptr{Void}, Uint), s.ios, a, nb) < nb
            throw(EOFError())
        end
        a
    else
        invoke(read, (Any, Array), s, a)
    end
end

function readuntil(s::IOStream, delim)
    # TODO: faster versions that avoid the encoding check
    ccall(:jl_readuntil, ByteString, (Ptr{Void}, Uint8), s.ios, delim)
end

function readall(s::IOStream)
    dest = memio()
    ccall(:ios_copyall, Uint, (Ptr{Void}, Ptr{Void}), dest.ios, s.ios)
    takebuf_string(dest)
end

readline(s::IOStream) = readuntil(s, '\n')

flush(s::IOStream) = ccall(:ios_flush, Void, (Ptr{Void},), s.ios)

truncate(s::IOStream, n::Integer) =
    ccall(:ios_trunc, Uint, (Ptr{Void}, Uint), s.ios, n)

seek(s::IOStream, n::Integer) =
    (ccall(:ios_seek, FileOffset, (Ptr{Void}, FileOffset), s.ios, n)==0 ||
     error("seek failed"))

skip(s::IOStream, delta::Integer) =
    (ccall(:ios_skip, FileOffset, (Ptr{Void}, FileOffset), s.ios, delta)==0 ||
     error("skip failed"))

position(s::IOStream) = ccall(:ios_pos, FileOffset, (Ptr{Void},), s.ios)

eof(s::IOStream) = bool(ccall(:jl_ios_eof, Int32, (Ptr{Void},), s.ios))

type IOTally <: IO
    nbytes::Int
    IOTally() = new(0)
end

write(s::IOTally, x::Uint8) = (s.nbytes += 1; nothing)
flush(s::IOTally) = ()

## select interface ##

type FDSet
    data::Array{Uint8,1}
    nfds::Int32

    function FDSet()
        ar = Array(Uint8, sizeof_fd_set)
        ccall(:jl_fd_zero, Void, (Ptr{Void},), ar)
        new(ar, 0)
    end
end

isempty(s::FDSet) = (s.nfds==0)

function add(s::FDSet, i::Integer)
    if !(0 <= i < sizeof_fd_set*8)
        error("invalid descriptor ", i)
    end
    ccall(:jl_fd_set, Void, (Ptr{Void}, Int32), s.data, i)
    if i >= s.nfds
        s.nfds = i+1
    end
    return s
end

function has(s::FDSet, i::Integer)
    if 0 <= i < sizeof_fd_set*8
        return ccall(:jl_fd_isset, Int32, (Ptr{Void}, Int32), s.data, i)!=0
    end
    return false
end

function del(s::FDSet, i::Integer)
    if 0 <= i < sizeof_fd_set*8
        ccall(:jl_fd_clr, Void, (Ptr{Void}, Int32), s.data, i)
        if i == s.nfds-1
            s.nfds -= 1
            while s.nfds>0 && !has(s, s.nfds-1)
                s.nfds -= 1
            end
        end
    end
    return s
end

function del_all(s::FDSet)
    ccall(:jl_fd_zero, Void, (Ptr{Void},), s.data)
    s.nfds = 0
    return s
end

begin
    local tv = Array(Uint8, int(ccall(:jl_sizeof_timeval, Int32, ())))
    global select_read
    function select_read(readfds::FDSet, timeout::Real)
        if timeout == Inf
            tout = C_NULL
        else
            ccall(:jl_set_timeval, Void, (Ptr{Void}, Float64), tv, timeout)
            tout = convert(Ptr{Void}, tv)
        end
        ccall(:select, Int32,
              (Int32, Ptr{Void}, Ptr{Void}, Ptr{Void}, Ptr{Void}),
              readfds.nfds, readfds.data, C_NULL, C_NULL, tout)
    end
end

## high-level iterator interfaces ##

type EachLine
    stream::IOStream
end
each_line(stream::IOStream) = EachLine(stream)

start(itr::EachLine) = readline(itr.stream)
function done(itr::EachLine, line)
    if !isempty(line)
        return false
    end
    close(itr.stream)
    true
end
next(itr::EachLine, this_line) = (this_line, readline(itr.stream))

function readlines(s, fx::Function...)
    a = {}
    for l = each_line(s)
        for f in fx
          l = f(l)
        end
        push(a, l)
    end
    return a
end
