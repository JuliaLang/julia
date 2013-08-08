module Base64
import Base: read, write, close
export Base64Pipe, base64

# Base64Pipe is a pipe-like IO object, which converts writes (and
# someday reads?) into base64 encoded (decoded) data send to a stream.
# (You must close the pipe to complete the encode, separate from
# closing the target stream).  We also have a function base64(f,
# args...) which works like sprint except that it produces
# base64-encoded data, along with base64(args...)  which is equivalent
# to base64(write, args...), to return base64 strings.

#############################################################################

type Base64Pipe <: IO
    io::IO
    # writing works in groups of 3, so we need to cache last two bytes written
    b0::Uint8
    b1::Uint8
    nb::Uint8 # number of bytes in cache: 0, 1, or 2

    function Base64Pipe(io::IO)
        b = new(io,0,0,0)
        finalizer(b, close)
        return b
    end
end

#############################################################################

# Based on code by Stefan Karpinski from https://github.com/hackerschool/WebSockets.jl (distributed under the same MIT license as Julia)

const b64chars = ['A':'Z','a':'z','0':'9','+','/']

function b64(x::Uint8, y::Uint8, z::Uint8)
  n = int(x)<<16 | int(y)<<8 | int(z)
  b64chars[(n >> 18) + 1],
  b64chars[(n >> 12) & 0b111111 + 1],
  b64chars[(n >> 6) & 0b111111 + 1],
  b64chars[(n ) & 0b111111 + 1]
end

function b64(x::Uint8, y::Uint8)
  a, b, c = b64(x, y, 0x0)
  a, b, c, '='
end

function b64(x::Uint8)
  a, b = b64(x, 0x0, 0x0)
  a, b, '=', '='
end

#############################################################################

function write(b::Base64Pipe, x::AbstractVector{Uint8})
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

function write(b::Base64Pipe, x::Uint8)
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

function close(b::Base64Pipe)
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
function base64(f::Function, args...)
    s = IOBuffer()
    b = Base64Pipe(s)
    f(b, args...)
    close(b)
    takebuf_string(s)
end
base64(x...) = base64(write, x...)

#############################################################################

# read(b::Base64Pipe, ::Type{Uint8}) = # TODO: decode base64

#############################################################################

end # module
