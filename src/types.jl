# SHA-224/256/384/512 Various Length Definitions
const SHA224_BLOCK_LENGTH   = 64
const SHA224_DIGEST_LENGTH  = 28
const SHA224_SHORT_BLOCK_LENGTH = (SHA224_BLOCK_LENGTH - 8)

const SHA256_BLOCK_LENGTH   = 64
const SHA256_DIGEST_LENGTH  = 32
const SHA256_SHORT_BLOCK_LENGTH = (SHA256_BLOCK_LENGTH - 8)

const SHA384_BLOCK_LENGTH   = 128
const SHA384_DIGEST_LENGTH  = 48
const SHA384_SHORT_BLOCK_LENGTH = (SHA384_BLOCK_LENGTH - 16)

const SHA512_BLOCK_LENGTH   = 128
const SHA512_DIGEST_LENGTH  = 64
const SHA512_SHORT_BLOCK_LENGTH = (SHA512_BLOCK_LENGTH - 16)

abstract SHA_CTX
abstract SHA_CTX_BIG <: SHA_CTX
abstract SHA_CTX_SMALL <: SHA_CTX

# SHA-256/384/512 Context Structures
type SHA224_CTX <: SHA_CTX_SMALL
    # state length = 8
    state::Array{Uint32,1}
    bitcount::Uint64
    # buffer length = SHA224_BLOCK_LENGTH
    buffer::Array{Uint32,1}
    blocklen::Int
    short_blocklen::Int
    digest_len::Int

    SHA224_CTX() = new( copy(sha224_initial_hash_value), 0,
                        zeros(Uint8, SHA224_BLOCK_LENGTH),
                        SHA224_BLOCK_LENGTH,
                        SHA224_SHORT_BLOCK_LENGTH,
                        SHA224_DIGEST_LENGTH)
end


type SHA256_CTX <: SHA_CTX_SMALL
    # state length = 8
    state::Array{Uint32,1}
    bitcount::Uint64
    # buffer length = SHA256_BLOCK_LENGTH
    buffer::Array{Uint32,1}
    blocklen::Int
    short_blocklen::Int
    digest_len::Int

    SHA256_CTX() = new( copy(sha256_initial_hash_value), 0,
                        zeros(Uint8, SHA256_BLOCK_LENGTH),
                        SHA256_BLOCK_LENGTH,
                        SHA256_SHORT_BLOCK_LENGTH,
                        SHA256_DIGEST_LENGTH)
end


type SHA384_CTX <: SHA_CTX_BIG
    # state length = 8
    state::Array{Uint64,1}
    bitcount::Uint128
    # buffer length = SHA384_BLOCK_LENGTH
    buffer::Array{Uint64,1}
    blocklen::Int
    short_blocklen::Int
    digest_len::Int

    SHA384_CTX() = new( copy(sha384_initial_hash_value), 0,
                        zeros(Uint8, SHA384_BLOCK_LENGTH),
                        SHA384_BLOCK_LENGTH,
                        SHA384_SHORT_BLOCK_LENGTH,
                        SHA384_DIGEST_LENGTH)
end


type SHA512_CTX <: SHA_CTX_BIG
    # state length = 8
    state::Array{Uint64,1}
    bitcount::Uint128
    # buffer length = SHA512_BLOCK_LENGTH
    buffer::Array{Uint64,1}
    blocklen::Int
    short_blocklen::Int
    digest_len::Int

    SHA512_CTX() = new( copy(sha512_initial_hash_value), 0,
                        zeros(Uint8, SHA512_BLOCK_LENGTH),
                        SHA512_BLOCK_LENGTH,
                        SHA512_SHORT_BLOCK_LENGTH,
                        SHA512_DIGEST_LENGTH)
end