isdefined(Base, :__precompile__) && __precompile__()

module SHA

using Compat

export sha1, sha224, sha256, sha384, sha512

include("constants.jl")
include("types.jl")
include("base_functions.jl")
include("sha1.jl")
include("sha2.jl")
include("common.jl")


# Create data types and convenience functions for each hash implemented
for (f, ctx) in [(:sha1, :SHA1_CTX),
                 (:sha224, :SHA224_CTX),
                 (:sha256, :SHA256_CTX),
                 (:sha384, :SHA384_CTX),
                 (:sha512, :SHA512_CTX)]
    @eval begin
        # Allows things like:
        # open("test.txt") do f
        #     sha256(f)
        # done
        function $f(io::IO)
            ctx = $ctx()
            update!(ctx, readbytes(io));
            return bytes2hex(digest!(ctx))
        end

        # Allows the same as above, but on ByteStrings and Arrays
        $f(str::ByteString) = $f(IOBuffer(str))
        $f(arr::Array{UInt8,1}) = $f(IOBuffer(arr))
    end
end

end #module SHA
