#isdefined(Base, :__precompile__) && __precompile__()

module SHA

using Compat

# Export convenience functions, context types, update!() and digest!() functions
export sha1, SHA1_CTX, update!, digest!
export sha224, sha256, sha384, sha512
export sha2_224, sha2_256, sha2_384, sha2_512
export sha3_224, sha3_256, sha3_384, sha3_512
export SHA224_CTX, SHA256_CTX, SHA384_CTX, SHA512_CTX
export SHA2_224_CTX, SHA2_256_CTX, SHA2_384_CTX, SHA2_512_CTX
export SHA3_224_CTX, SHA3_256_CTX, SHA3_384_CTX, SHA3_512_CTX


include("constants.jl")
include("types.jl")
include("base_functions.jl")
include("sha1.jl")
include("sha2.jl")
include("sha3.jl")
include("common.jl")

# Create data types and convenience functions for each hash implemented
for (f, ctx) in [(:sha1, :SHA1_CTX),
                 (:sha224, :SHA224_CTX),
                 (:sha256, :SHA256_CTX),
                 (:sha384, :SHA384_CTX),
                 (:sha512, :SHA512_CTX),
                 (:sha2_224, :SHA2_224_CTX),
                 (:sha2_256, :SHA2_256_CTX),
                 (:sha2_384, :SHA2_384_CTX),
                 (:sha2_512, :SHA2_512_CTX),
                 (:sha3_224, :SHA3_224_CTX),
                 (:sha3_256, :SHA3_256_CTX),
                 (:sha3_384, :SHA3_384_CTX),
                 (:sha3_512, :SHA3_512_CTX),]
    @eval begin
        # Our basic function is to process arrays of bytes
        function $f(data::Array{UInt8,1})
            ctx = $ctx()
            update!(ctx, data);
            return digest!(ctx)
        end

        # ByteStrings are a pretty handy thing to be able to crunch through
        $f(str::ByteString) = $f(str.data)

        # Convenience function for IO devices, allows for things like:
        # open("test.txt") do f
        #     sha256(f)
        # done
        $f(io::IO) = $f(readbytes(io))
    end
end

end #module SHA
