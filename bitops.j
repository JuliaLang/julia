## boolean operations ##

!(x::Bool) = eq_int(unbox8(x),trunc8(unbox32(0)))
==(x::Bool, y::Bool) = eq_int(unbox8(x),unbox8(y))

(~)(x::Bool) = !x
(&)(x::Bool, y::Bool) = (x&&y)
(|)(x::Bool, y::Bool) = (x||y)
($)(x::Bool, y::Bool) = (x!=y)

## integer arithmetic ##

(-)(x::Int8 ) = boxsi8 (neg_int(unbox8 (x)))
(-)(x::Int16) = boxsi16(neg_int(unbox16(x)))
(-)(x::Int32) = boxsi32(neg_int(unbox32(x)))
(-)(x::Int64) = boxsi64(neg_int(unbox64(x)))

(+)(x::Int8 , y::Int8 ) = boxsi8 (add_int(unbox8 (x), unbox8 (y)))
(+)(x::Int16, y::Int16) = boxsi16(add_int(unbox16(x), unbox16(y)))
(+)(x::Int32, y::Int32) = boxsi32(add_int(unbox32(x), unbox32(y)))
(+)(x::Int64, y::Int64) = boxsi64(add_int(unbox64(x), unbox64(y)))

(-)(x::Int8 , y::Int8 ) = boxsi8 (sub_int(unbox8 (x), unbox8 (y)))
(-)(x::Int16, y::Int16) = boxsi16(sub_int(unbox16(x), unbox16(y)))
(-)(x::Int32, y::Int32) = boxsi32(sub_int(unbox32(x), unbox32(y)))
(-)(x::Int64, y::Int64) = boxsi64(sub_int(unbox64(x), unbox64(y)))

(*)(x::Int8 , y::Int8 ) = boxsi8 (mul_int(unbox8 (x), unbox8 (y)))
(*)(x::Int16, y::Int16) = boxsi16(mul_int(unbox16(x), unbox16(y)))
(*)(x::Int32, y::Int32) = boxsi32(mul_int(unbox32(x), unbox32(y)))
(*)(x::Int64, y::Int64) = boxsi64(mul_int(unbox64(x), unbox64(y)))

(/)(x::Int, y::Int) = float64(x)/float64(y)

div(x::Int8 , y::Int8 ) = boxsi8 (sdiv_int(unbox8 (x), unbox8 (y)))
div(x::Int16, y::Int16) = boxsi16(sdiv_int(unbox16(x), unbox16(y)))
div(x::Int32, y::Int32) = boxsi32(sdiv_int(unbox32(x), unbox32(y)))
div(x::Int64, y::Int64) = boxsi64(sdiv_int(unbox64(x), unbox64(y)))

(%)(x::Int8 , y::Int8 ) = boxsi8 (smod_int(unbox8 (x), unbox8 (y)))
(%)(x::Int16, y::Int16) = boxsi16(smod_int(unbox16(x), unbox16(y)))
(%)(x::Int32, y::Int32) = boxsi32(smod_int(unbox32(x), unbox32(y)))
(%)(x::Int64, y::Int64) = boxsi64(smod_int(unbox64(x), unbox64(y)))

## integer bitwise operations ##

(~)(x::Int8 ) = boxsi8 (not_int(unbox8 (x)))
(~)(x::Int16) = boxsi16(not_int(unbox16(x)))
(~)(x::Int32) = boxsi32(not_int(unbox32(x)))
(~)(x::Int64) = boxsi64(not_int(unbox64(x)))

(&)(x::Int8 , y::Int8 ) = boxsi8 (and_int(unbox8 (x), unbox8 (y)))
(&)(x::Int16, y::Int16) = boxsi16(and_int(unbox16(x), unbox16(y)))
(&)(x::Int32, y::Int32) = boxsi32(and_int(unbox32(x), unbox32(y)))
(&)(x::Int64, y::Int64) = boxsi64(and_int(unbox64(x), unbox64(y)))

(|)(x::Int8 , y::Int8 ) = boxsi8 (or_int(unbox8 (x), unbox8 (y)))
(|)(x::Int16, y::Int16) = boxsi16(or_int(unbox16(x), unbox16(y)))
(|)(x::Int32, y::Int32) = boxsi32(or_int(unbox32(x), unbox32(y)))
(|)(x::Int64, y::Int64) = boxsi64(or_int(unbox64(x), unbox64(y)))

($)(x::Int8 , y::Int8 ) = boxsi8 (xor_int(unbox8 (x), unbox8 (y)))
($)(x::Int16, y::Int16) = boxsi16(xor_int(unbox16(x), unbox16(y)))
($)(x::Int32, y::Int32) = boxsi32(xor_int(unbox32(x), unbox32(y)))
($)(x::Int64, y::Int64) = boxsi64(xor_int(unbox64(x), unbox64(y)))

(<<) (x::Int8 , y::Int32) = boxsi8 ( shl_int(unbox8 (x), unbox32(y)))
(<<) (x::Int16, y::Int32) = boxsi16( shl_int(unbox16(x), unbox32(y)))
(<<) (x::Int32, y::Int32) = boxsi32( shl_int(unbox32(x), unbox32(y)))
(<<) (x::Int64, y::Int32) = boxsi64( shl_int(unbox64(x), unbox32(y)))
(>>) (x::Int8 , y::Int32) = boxsi8 (ashr_int(unbox8 (x), unbox32(y)))
(>>) (x::Int16, y::Int32) = boxsi16(ashr_int(unbox16(x), unbox32(y)))
(>>) (x::Int32, y::Int32) = boxsi32(ashr_int(unbox32(x), unbox32(y)))
(>>) (x::Int64, y::Int32) = boxsi64(ashr_int(unbox64(x), unbox32(y)))
(>>>)(x::Int8 , y::Int32) = boxsi8 (lshr_int(unbox8 (x), unbox32(y)))
(>>>)(x::Int16, y::Int32) = boxsi16(lshr_int(unbox16(x), unbox32(y)))
(>>>)(x::Int32, y::Int32) = boxsi32(lshr_int(unbox32(x), unbox32(y)))
(>>>)(x::Int64, y::Int32) = boxsi64(lshr_int(unbox64(x), unbox32(y)))

bswap(x::Int16)  = boxsi16(bswap_int(unbox16(x)))
bswap(x::Uint16) = boxui16(bswap_int(unbox16(x)))
bswap(x::Int32)  = boxsi32(bswap_int(unbox32(x)))
bswap(x::Uint32) = boxui32(bswap_int(unbox32(x)))
bswap(x::Int64)  = boxsi64(bswap_int(unbox64(x)))
bswap(x::Uint64) = boxui64(bswap_int(unbox64(x)))

## integer comparisons ##

==(x::Int8 , y::Int8 ) = eq_int(unbox8 (x),unbox8 (y))
==(x::Int16, y::Int16) = eq_int(unbox16(x),unbox16(y))
==(x::Int32, y::Int32) = eq_int(unbox32(x),unbox32(y))
==(x::Int64, y::Int64) = eq_int(unbox64(x),unbox64(y))
==(x::Uint8 , y::Uint8 ) = eq_int(unbox8 (x),unbox8 (y))
==(x::Uint16, y::Uint16) = eq_int(unbox16(x),unbox16(y))
==(x::Uint32, y::Uint32) = eq_int(unbox32(x),unbox32(y))
==(x::Uint64, y::Uint64) = eq_int(unbox64(x),unbox64(y))

< (x::Int8 , y::Int8 ) = slt_int(unbox8 (x),unbox8 (y))
< (x::Int16, y::Int16) = slt_int(unbox16(x),unbox16(y))
< (x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y))
< (x::Int64, y::Int64) = slt_int(unbox64(x),unbox64(y))
# negating a comparison is ok for integers
<=(x::Int, y::Int) = !(x > y)
>=(x::Int, y::Int) = !(x < y)

# not sure if we can safely generalize this to floats:
cmp(x::Int, y::Int) = x < y ? -1 : x > y ? +1 : 0

## floating point arithmetic ##

(-)(x::Float32) = boxf32(neg_float(unbox32(x)))
(-)(x::Float64) = boxf64(neg_float(unbox64(x)))
(+)(x::Float32, y::Float32) = boxf32(add_float(unbox32(x), unbox32(y)))
(+)(x::Float64, y::Float64) = boxf64(add_float(unbox64(x), unbox64(y)))
(-)(x::Float32, y::Float32) = boxf32(sub_float(unbox32(x), unbox32(y)))
(-)(x::Float64, y::Float64) = boxf64(sub_float(unbox64(x), unbox64(y)))
(*)(x::Float32, y::Float32) = boxf32(mul_float(unbox32(x), unbox32(y)))
(*)(x::Float64, y::Float64) = boxf64(mul_float(unbox64(x), unbox64(y)))
(/)(x::Float32, y::Float32) = boxf32(div_float(unbox32(x), unbox32(y)))
(/)(x::Float64, y::Float64) = boxf64(div_float(unbox64(x), unbox64(y)))

## floating point comparisons ##

==(x::Float32, y::Float32) = eq_float(unbox32(x),unbox32(y))
==(x::Float64, y::Float64) = eq_float(unbox64(x),unbox64(y))
!=(x::Float32, y::Float32) = ne_float(unbox32(x),unbox32(y))
!=(x::Float64, y::Float64) = ne_float(unbox64(x),unbox64(y))
# negating a comparison is not ok for floats
!=(x::Float, y::Float) = (!=)(promote(x,y)...)
< (x::Float32, y::Float32) = lt_float(unbox32(x),unbox32(y))
< (x::Float64, y::Float64) = lt_float(unbox64(x),unbox64(y))
<=(x::Float32, y::Float32) = le_float(unbox32(x),unbox32(y))
<=(x::Float64, y::Float64) = le_float(unbox64(x),unbox64(y))
>=(x::Float32, y::Float32) = ge_float(unbox32(x),unbox32(y))
>=(x::Float64, y::Float64) = ge_float(unbox64(x),unbox64(y))
<=(x::Float, y::Float) = (<=)(promote(x,y)...)
>=(x::Float, y::Float) = (>=)(promote(x,y)...)

## character operations & comparisons ##

-(x::Char) = -int32(x)
~(x::Char) = ~int32(x)
(+)(x::Char, y::Char) = int32(x) + int32(y)
(-)(x::Char, y::Char) = int32(x) - int32(y)
(*)(x::Char, y::Char) = int32(x) * int32(y)
div(x::Char, y::Char) = div(int32(x), int32(y))
(%)(x::Char, y::Char) = int32(x) % int32(y)
(&)(x::Char, y::Char) = int32(x) & int32(y)
(|)(x::Char, y::Char) = int32(x) | int32(y)
($)(x::Char, y::Char) = int32(x) $ int32(y)
(<<)(x::Char, y::Int32) = int32(x) << y
(>>)(x::Char, y::Int32) = int32(x) >> y
(>>>)(x::Char, y::Int32) = int32(x) >>> y
==(x::Char, y::Char) = int32(x) == int32(y)
< (x::Char, y::Char) = int32(x) <  int32(y)

## pointer comparison ##

==(x::Ptr, y::Ptr) = eq_int(unbox(Ptr,x),unbox(Ptr,y))

## system word size ##

word_size() = ccall(dlsym(JuliaDLHandle,"jl_word_size"), Int32, ())
