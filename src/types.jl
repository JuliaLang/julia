# Type hierarchy to aid in splitting up of SHA2 algorithms
# as SHA224/256 are similar, and SHA-384/512 are similar
abstract SHA_CTX
abstract SHA2_CTX_BIG <: SHA_CTX
abstract SHA2_CTX_SMALL <: SHA_CTX


# We derive SHA1_CTX straight from SHA_CTX since it doesn't need
# to follow the split between "big" and "small" algorithms like
# SHA2 needs to due to its two different Sigma_* functions
type SHA1_CTX <: SHA_CTX
    state::Array{UInt32,1}
    bytecount::UInt64
    buffer::Array{UInt8,1}
    W::Array{UInt32,1}
end

# SHA-224/256/384/512 Context Structures
type SHA224_CTX <: SHA2_CTX_SMALL
    state::Array{UInt32,1}
    bytecount::UInt64
    buffer::Array{UInt8,1}
end

type SHA256_CTX <: SHA2_CTX_SMALL
    state::Array{UInt32,1}
    bytecount::UInt64
    buffer::Array{UInt8,1}
end

type SHA384_CTX <: SHA2_CTX_BIG
    state::Array{UInt64,1}
    bytecount::UInt128
    buffer::Array{UInt8,1}
end

type SHA512_CTX <: SHA2_CTX_BIG
    state::Array{UInt64,1}
    bytecount::UInt128
    buffer::Array{UInt8,1}
end

# Define constants via functions so as not to bloat context objects.  Yay dispatch!
# The difference in block length here is part of what separates the "big" and "small" SHA2 algorithms
blocklen(::Type{SHA1_CTX}) = 64
blocklen(::Type{SHA224_CTX}) = 64
blocklen(::Type{SHA256_CTX}) = 64
blocklen(::Type{SHA384_CTX}) = 128
blocklen(::Type{SHA512_CTX}) = 128

digestlen(::Type{SHA1_CTX}) = 20
digestlen(::Type{SHA224_CTX}) = 28
digestlen(::Type{SHA256_CTX}) = 32
digestlen(::Type{SHA384_CTX}) = 48
digestlen(::Type{SHA512_CTX}) = 64

state_type(::Type{SHA1_CTX}) = UInt32
state_type(::Type{SHA224_CTX}) = UInt32
state_type(::Type{SHA256_CTX}) = UInt32
state_type(::Type{SHA384_CTX}) = UInt64
state_type(::Type{SHA512_CTX}) = UInt64

# short_blocklen is the size of a block minus the width of bytecount
short_blocklen{T<:SHA_CTX}(::Type{T}) = blocklen(T) - 2*sizeof(state_type(T))


# Once the "blocklength" methods are defined, we can define our outer constructors for SHA types:
for ALG in ["SHA224", "SHA256", "SHA384", "SHA512"]
    TYPE = symbol("$(ALG)_CTX")
    HASH_VAL = symbol("$(ALG)_initial_hash_value")
    @eval begin
        $TYPE() = $TYPE(copy($HASH_VAL), 0, zeros(UInt8, blocklen($TYPE)))
    end
end

# SHA1 is special; he needs extra workspace
SHA1_CTX() = SHA1_CTX(copy(SHA1_initial_hash_value), 0, zeros(UInt8, blocklen(SHA1_CTX)), Array(UInt32, 80))

# Make printing these types a little friendlier
import Base.show
show(io::IO, ::SHA1_CTX) = write(io, "SHA1 hash state")
show(io::IO, ::SHA224_CTX) = write(io, "SHA-224 hash state")
show(io::IO, ::SHA256_CTX) = write(io, "SHA-256 hash state")
show(io::IO, ::SHA384_CTX) = write(io, "SHA-384 hash state")
show(io::IO, ::SHA512_CTX) = write(io, "SHA-512 hash state")
