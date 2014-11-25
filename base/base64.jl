module Base64
import Base: read, write, close, eof
export Base64Pipe, Base64EncodePipe, Base64DecodePipe, base64, base64encode, base64decode

# Base64EncodePipe is a pipe-like IO object, which converts into base64 data sent 
# to a stream. (You must close the pipe to complete the encode, separate from
# closing the target stream).  We also have a function base64encode(f,
# args...) which works like sprint except that it produces
# base64-encoded data, along with base64encode(args...)  which is equivalent
# to base64encode(write, args...), to return base64 strings.
# A Base64DecodePipe object can be used to decode base64-encoded data read from a stream
# , while function base64decode is useful for decoding strings
#############################################################################

type Base64EncodePipe <: IO
    io::IO
    # writing works in groups of 3, so we need to cache last two bytes written
    b0::UInt8
    b1::UInt8
    nb::UInt8 # number of bytes in cache: 0, 1, or 2

    function Base64EncodePipe(io::IO)
        b = new(io,0,0,0)
        finalizer(b, close)
        return b
    end
end

Base64Pipe = Base64EncodePipe

#############################################################################

# Based on code by Stefan Karpinski from https://github.com/hackerschool/WebSockets.jl (distributed under the same MIT license as Julia)

const b64chars = ['A':'Z','a':'z','0':'9','+','/']

const revb64chars = Dict('A'=> 0, 'B'=> 1, 'C'=> 2, 'D'=> 3, 'E'=> 4, 'F'=> 5, 'G'=> 6, 'H'=> 7, 'I'=> 8, 'J'=> 9, 
                         'K'=> 10, 'L'=> 11, 'M'=> 12, 'N'=> 13, 'O'=> 14, 'P'=> 15, 'Q'=> 16, 'R'=> 17, 'S'=> 18, 'T'=> 19, 
                         'U'=> 20, 'V'=> 21, 'W'=> 22, 'X'=> 23, 'Y'=> 24, 'Z'=> 25, 'a'=> 26, 'b'=> 27, 'c'=> 28, 'd'=> 29, 
                         'e'=> 30, 'f'=> 31, 'g'=> 32, 'h'=> 33, 'i'=> 34, 'j'=> 35, 'k'=> 36, 'l'=> 37, 'm'=> 38, 'n'=> 39, 
                         'o'=> 40, 'p'=> 41, 'q'=> 42, 'r'=> 43, 's'=> 44, 't'=> 45, 'u'=> 46, 'v'=> 47, 'w'=> 48, 'x'=> 49, 
                         'y'=> 50, 'z'=> 51, '0'=> 52, '1'=> 53, '2'=> 54, '3'=> 55, '4'=> 56, '5'=> 57, '6'=> 58, '7'=> 59,
                         '8'=> 60, '9'=> 61, '+'=> 62, '/'=> 63)
                         
function b64(x::UInt8, y::UInt8, z::UInt8)
    n = int(x)<<16 | int(y)<<8 | int(z)
    b64chars[(n >> 18) + 1],
    b64chars[(n >> 12) & 0b111111 + 1],
    b64chars[(n >> 6) & 0b111111 + 1],
    b64chars[(n ) & 0b111111 + 1]
end

function b64(x::UInt8, y::UInt8)
    a, b, c = b64(x, y, 0x0)
    a, b, c, '='
end

function b64(x::UInt8)
    a, b = b64(x, 0x0, 0x0)
    a, b, '=', '='
end

function b64decode(encvec::Vector{UInt8})
    if length(encvec) < 2 
        error("Incorrect base64 format")
    end
    n = revb64chars[encvec[1]] << 18 | revb64chars[encvec[2]] << 12
    if length(encvec) > 2
        n =  n | revb64chars[encvec[3]] << 6
    end
    if length(encvec) > 3
        n = n | revb64chars[encvec[4]]
    end
    decvec = [uint8(n >> 16)]
    if length(encvec) > 2
        push!(decvec, uint8(n >> 8 & 0b11111111))
    end
    if length(encvec) > 3
        push!(decvec, uint8(n & 0b11111111))
    end
    decvec
end

#############################################################################

function write(b::Base64EncodePipe, x::AbstractVector{UInt8})
    n = length(x)
    s = 1 # starting index
    # finish any cached data to write:
    if b.nb == 1
        if n >= 2
            write(b.io, b64(b.b0, x[1], x[2])...)
            s = 3
        elseif n == 1
            b.b1 = x[1]
            b.nb = 2
            return
        else
            return
        end
    elseif b.nb == 2
        if n >= 1
            write(b.io, b64(b.b0, b.b1, x[1])...)
            s = 2
        else
            return
        end
    end
    # write all groups of three bytes:
    while s + 2 <= n
        write(b.io, b64(x[s], x[s+1], x[s+2])...)
        s += 3
    end
    # cache any leftover bytes:
    if s + 1 == n
        b.b0 = x[s]
        b.b1 = x[s+1]
        b.nb = 2
    elseif s == n
        b.b0 = x[s]
        b.nb = 1
    else
        b.nb = 0
    end
end

function write(b::Base64EncodePipe, x::UInt8)
    if b.nb == 0
        b.b0 = x
        b.nb = 1
    elseif b.nb == 1
        b.b1 = x
        b.nb = 2
    else
        write(b.io, b64(b.b0,b.b1,x)...)
        b.nb = 0
    end
end

function close(b::Base64EncodePipe)
    if b.nb > 0
        # write leftover bytes + padding
        if b.nb == 1
            write(b.io, b64(b.b0)...)
        else # b.nb == 2
            write(b.io, b64(b.b0, b.b1)...)
        end
        b.nb = 0
    end
end

# like sprint, but returns base64 string
function base64encode(f::Function, args...)
    s = IOBuffer()
    b = Base64EncodePipe(s)
    f(b, args...)
    close(b)
    takebuf_string(s)
end
base64encode(x...) = base64encode(write, x...)

base64 = base64encode

#############################################################################

# read(b::Base64Pipe, ::Type{UInt8}) = # TODO: decode base64

#############################################################################

type Base64DecodePipe <: IO
    io::IO
    # reading works in blocks of 4 characters that are decoded into 3 bytes and 2 of them cached
    cache::Vector{UInt8}

    function Base64DecodePipe(io::IO)
        b = new(io,[])
        finalizer(b, close)
        return b
    end
end

function read(b::Base64DecodePipe, t::Type{UInt8})
    if length(b.cache) > 0
        val = shift!(b.cache)
    else
        encvec = Array(UInt8, 0)
        while !eof(b.io) && length(encvec) < 4
            c = read(b.io, t)
            if haskey(revb64chars, c)
                push!(encvec, c)
            end
        end
        decvec = b64decode(encvec)
        val = decvec[1]
        b.cache = decvec[2:end]
    end
    val
end

function eof(b::Base64DecodePipe)
    return length(b.cache) == 0 && eof(b.io)
end

function close(b::Base64DecodePipe)
end

# Decodes a base64-encoded string
function base64decode(s)
    b = IOBuffer(s)
    decoded = readall(Base64DecodePipe(b))
    close(b)
    decoded
end

end # module
