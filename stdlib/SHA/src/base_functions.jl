# This file is a part of Julia. License is MIT: https://julialang.org/license

# THE SIX LOGICAL FUNCTIONS
#
# Bit shifting and rotation (used by the six SHA-XYZ logical functions:
#
#   NOTE:  The naming of R and S appears backwards here (R is a SHIFT and
#   S is a ROTATION) because the SHA2-256/384/512 description document
#   (see http://csrc.nist.gov/cryptval/shs/sha256-384-512.pdf) uses this
#   same "backwards" definition.

# 32-bit Rotate-right (equivalent to S32 in SHA-256) and rotate-left
rrot(b,x,width) = ((x >> b) | (x << (width - b)))
lrot(b,x,width) = ((x << b) | (x >> (width - b)))

# Shift-right (used in SHA-256, SHA-384, and SHA-512):
R(b,x)   = (x >> b)
# 32-bit Rotate-right (used in SHA-256):
S32(b,x) = rrot(b,x,32)
# 64-bit Rotate-right (used in SHA-384 and SHA-512):
S64(b,x) = rrot(b,x,64)
# 64-bit Rotate-left (used in SHA3)
L64(b,x) = lrot(b,x,64)

# Two of six logical functions used in SHA-256, SHA-384, and SHA-512:
Ch(x,y,z)  = ((x & y) ⊻ (~x & z))
Maj(x,y,z) = ((x & y) ⊻ (x & z) ⊻ (y & z))

# Four of six logical functions used in SHA-256:
Sigma0_256(x) = (S32(2,  UInt32(x)) ⊻ S32(13, UInt32(x)) ⊻ S32(22, UInt32(x)))
Sigma1_256(x) = (S32(6,  UInt32(x)) ⊻ S32(11, UInt32(x)) ⊻ S32(25, UInt32(x)))
sigma0_256(x) = (S32(7,  UInt32(x)) ⊻ S32(18, UInt32(x)) ⊻ R(3 ,   UInt32(x)))
sigma1_256(x) = (S32(17, UInt32(x)) ⊻ S32(19, UInt32(x)) ⊻ R(10,   UInt32(x)))

# Four of six logical functions used in SHA-384 and SHA-512:
Sigma0_512(x) = (S64(28, UInt64(x)) ⊻ S64(34, UInt64(x)) ⊻ S64(39, UInt64(x)))
Sigma1_512(x) = (S64(14, UInt64(x)) ⊻ S64(18, UInt64(x)) ⊻ S64(41, UInt64(x)))
sigma0_512(x) = (S64( 1, UInt64(x)) ⊻ S64( 8, UInt64(x)) ⊻ R( 7,   UInt64(x)))
sigma1_512(x) = (S64(19, UInt64(x)) ⊻ S64(61, UInt64(x)) ⊻ R( 6,   UInt64(x)))

# Let's be able to bswap arrays of these types as well
bswap!(x::Vector{<:Integer}) = map!(bswap, x, x)
