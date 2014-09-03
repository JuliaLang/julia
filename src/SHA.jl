# This module is basically a direct port of NetBSD's sha2.c
module SHA

export sha224, sha256, sha384, sha512

include("constants.jl")
include("base_functions.jl")
include("types.jl")
include("sha2.jl")

_shasum(ctx::SHA_CTX, io::IO) = 
    (update!(ctx, readbytes(io)); bytes2hex(digest!(ctx)))

for (f, ctx) in [(:sha224, :SHA224_CTX),
                 (:sha256, :SHA256_CTX),
                 (:sha384, :SHA384_CTX),
                 (:sha512, :SHA512_CTX)]
    @eval begin
        $f(io::IO) = _shasum($ctx(), io)
        $f(str::ByteString) = $f(IOBuffer(str))
        $f(arr::Array{Uint8,1}) = $f(IOBuffer(arr))
    end
end

end
