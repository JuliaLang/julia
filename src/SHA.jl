# This module is basically a direct port of NetBSD's sha2.c
module SHA

export digest!, update!, SHA224_CTX, SHA256_CTX, SHA384_CTX, SHA512_CTX

include("constants.jl")
include("base_functions.jl")
include("types.jl")
include("sha2.jl")

end
