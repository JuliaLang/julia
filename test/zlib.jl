require("extras/zlib")

import Zlib
using Zlib

########################
# Zlib tests
########################

# Initialize byte compression buffer
const BUFSIZE = 65536

# Ordered array
b = zeros(Uint8, BUFSIZE)
for i = 1:length(b)
    b[i] = (i-1)%256
end

# Random array
r = b[randi((1,256), BUFSIZE)]

########################
# type size tests
########################
# This test group is to make sure that our interpretation of zlib's
# type sizes is correct.  zlib gives this information via the
# zlibCompileFlags function.

# Get compile-time option flags
zlib_compile_flags = ccall(dlsym(Zlib._zlib, :zlibCompileFlags), Uint, ())

# Type sizes, two bits each, 00 = 16 bits, 01 = 32, 10 = 64, 11 = other:
#
#  1.0: size of uInt
#  3.2: size of uLong
#  5.4: size of voidpf (pointer)
#  7.6: size of z_off_t

z_uInt_sz    = 2 << ( zlib_compile_flags       & uint(3))
z_uLong_sz   = 2 << ((zlib_compile_flags >> 2) & uint(3))
z_voidpf_sz  = 2 << ((zlib_compile_flags >> 4) & uint(3))
z_off_t_sz   = 2 << ((zlib_compile_flags >> 6) & uint(3))

## The following assumptions should be true, and were used to make
## the zlib wrapper.  If they are not true, we should get an error here,
## so things can be fixed.

@test(z_uInt_sz == sizeof(Uint32))
@test(z_uLong_sz == sizeof(Uint))
@test(z_voidpf_sz == sizeof(Ptr))
@test(z_off_t_sz == sizeof(Zlib.ZFileOffset) || (dlsym(Zlib._zlib, :crc32_combine64) != C_NULL && sizeof(Zlib.ZFileOffset) == 8))

########################
# compress/uncompress tests
########################

# Simple string compression/decompression 
s = "This is a test string"
cs = compress(s)
us = bytestring(uncompress(cs))
@test us == s

# Test compression, uncompression of b
cb = compress(b)
ub = uncompress(cb)
@test ub == b

# Test uncompression of uncompressed data
@test_fails uncompress(b)


########################
# compress_to_buffer/uncompress tests
########################

# String compression to buffer
max_buf_s = compress_bound(length(s))
cs = zeros(Uint8, max_buf_s)
ncb = compress_to_buffer(s, cs)
us = bytestring(uncompress(cs))
@test ncb < max_buf_s
@test us == s

# Data compression to buffer
max_buf_b = compress_bound(length(b))
cb = zeros(Uint8, max_buf_b)
ncb = compress_to_buffer(b, cb)
ub = uncompress(cb)
@test ncb < max_buf_b
@test ub == b

# Random data compression to buffer
max_buf_r = compress_bound(length(r))
cr = zeros(Uint8, max_buf_r)
ncr = compress_to_buffer(r, cr)
ur = uncompress(cr)
@test ncr < max_buf_r
@test ur == r

########################
# uncompress to buffer tests
########################

# Test uncompression to tiny buffer (ZError)
ub = zeros(Uint8, 2)
@test_fails uncompress_to_buffer(cb, ub)

# Test uncompression to buffer which is slightly too small (ZError)
ub = zeros(Uint8, BUFSIZE-1)
@test_fails uncompress_to_buffer(cb, ub)

# Test uncompression of uncompressed data to buffer
ub = zeros(Uint8, BUFSIZE)
@test_fails uncompress_to_buffer(b, ub)

# Test uncompression to buffer
nb = uncompress_to_buffer(cb, ub)
@test nb == length(b)
@test ub == b

# Test uncompression to buffer which is larger than necessary
ub = zeros(Uint8, BUFSIZE + 100)
nb = uncompress_to_buffer(cb, ub)
@test nb == length(ub)-100
@test ub[1:nb] == b

# Test uncompress to small buffer, random data (ZError)
ur = zeros(Uint8, BUFSIZE-1)
@test_fails uncompress_to_buffer(cr, ur)

# Test uncompress to large buffer, random data
ur = zeros(Uint8, BUFSIZE+10)
nur = uncompress_to_buffer(cr, ur)
@test nur == BUFSIZE
@test ur[1:nur] == r

