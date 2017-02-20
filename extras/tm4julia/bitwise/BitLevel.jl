# source: BitLevel.jl
#
# author: Jeffrey A. Sanroff
# date  : 2012-Sep-19 in New York City



module BitLevel

export BYTE_SIZE, BYTE_SIZE_SHIFT,
       bitsizeof, bitspanof,
       allbits0, allbits1, lsbs0, lsbs1, msbs0, msbs1,
       shift0s_up, shift0s_dn, shift1s_up, shift1s_dn,
       signs_differ, signs_same


import Base.*


# convert( ::Type{FloatingPoint}, x::FloatingPoint ) = x
# convert( ::Type{Unsigned}     , x::Unsigned ) = x
# convert( ::Type{Signed}       , x::Signed   ) = x

# convert( ::Type{Unsigned}, x::Int128   ) = reinterpret( Uint128, x )
# convert( ::Type{Unsigned}, x::Int64    ) = reinterpret( Uint64 , x )
# convert( ::Type{Unsigned}, x::Int32    ) = reinterpret( Uint32 , x )
# convert( ::Type{Unsigned}, x::Int16    ) = reinterpret( Uint16 , x )
# convert( ::Type{Unsigned}, x::Int8     ) = reinterpret( Uint8  , x )

# convert( ::Type{Signed}  , x::Uint128  ) = reinterpret( Int128 , x )
# convert( ::Type{Signed}  , x::Uint64   ) = reinterpret( Int64  , x )
# convert( ::Type{Signed}  , x::Uint32   ) = reinterpret( Int32  , x )
# convert( ::Type{Signed}  , x::Uint16   ) = reinterpret( Int16  , x )
# convert( ::Type{Signed}  , x::Uint8    ) = reinterpret( Int8   , x )



const BYTE_SIZE =
    if     (sizeof(Int128) == 1) 128
    elseif (sizeof(Int64)  == 1)  64
    elseif (sizeof(Int32)  == 1)  32
    elseif (sizeof(Int16)  == 1)  16
    elseif (sizeof(Int8)   == 1)   8
    else   error("Unexpected value of BYTE_SIZE = $(BYTE_SIZE).")
    end


const BYTE_SIZE_SHIFT =
    if     (BYTE_SIZE ==   8)  3
    elseif (BYTE_SIZE ==  16)  4
    elseif (BYTE_SIZE ==  32)  5
    elseif (BYTE_SIZE ==  64)  6
    elseif (BYTE_SIZE == 128)  7
    else   error("Unexpected value of BYTE_SIZE = $(BYTE_SIZE).")
    end



bitsizeof{T}(::Type{T})  = sizeof(T) << BYTE_SIZE_SHIFT
bitsizeof{T}(x::T)       = sizeof(x) << BYTE_SIZE_SHIFT

#bitsizeof(x) = sizeof(x) << BYTE_SIZE_SHIFT

#bitsizeof(::Type{Uint8})    =   8 ;    bitsizeof(::Type{Int8})     =   8
#bitsizeof(::Type{Uint16})   =  16 ;    bitsizeof(::Type{Int16})    =  16
#bitsizeof(::Type{Uint32})   =  32 ;    bitsizeof(::Type{Int32})    =  32
#bitsizeof(::Type{Uint64})   =  64 ;    bitsizeof(::Type{Int64})    =  64
#bitsizeof(::Type{Uint128})  = 128 ;    bitsizeof(::Type{Int128})   = 128
#
#bitsizeof(::Type{Float32})  =  32 ;    bitsizeof(::Type{Float64})  =  64
#
#bitsizeof(x::Uint8)    =   8 ;    bitsizeof(x::Int8)     =   8
#bitsizeof(x::Uint16)   =  16 ;    bitsizeof(x::Int16)    =  16
#bitsizeof(x::Uint32)   =  32 ;    bitsizeof(x::Int32)    =  32
#bitsizeof(x::Uint64)   =  64 ;    bitsizeof(x::Int64)    =  64
#bitsizeof(x::Uint128)  = 128 ;    bitsizeof(x::Int128)   = 128
#
#bitsizeof(x::Float32)  =  32 ;    bitsizeof(x::Float64)  =  64

# bitwidth of value (x<0 adds 1 to bitwidth(abs(x)) for a sign-extension bit

bitspanof{T}(::Type{T})    = sizeof(T) << BYTE_SIZE_SHIFT

bitspanof(x::Unsigned)     = bitsizeof(x) - leading_zeros(x) + (x==0)
bitspanof{T<:Signed}(x::T) = bitspanof( convert(Unsigned,abs(x)) ) + (x < 0)


# allbits0{T<:Integer}( ::Type{T}  ) = convert(T, 0)
# allbits0{T<:Integer}( x::T       ) = convert(T, 0)
# allbits0() = allbits0(Int)

# allbits1{T<:Integer}( ::Type{T}  ) = ~convert(T, 0)
# allbits1{T<:Integer}( x::T       ) = ~convert(T, 0)
# allbits1() = allbits1(Int)

allbits0( ::Type{Uint8}   ) = zero(Uint8)
allbits0( ::Type{Uint16}  ) = zero(Uint16)
allbits0( ::Type{Uint32}  ) = zero(Uint32)
allbits0( ::Type{Uint64}  ) = zero(Uint64)
allbits0( ::Type{Uint128} ) = zero(Uint128)

allbits0( ::Type{Int8}    ) = zero(Int8)
allbits0( ::Type{Int16}   ) = zero(Int16)
allbits0( ::Type{Int32}   ) = zero(Int32)
allbits0( ::Type{Int64}   ) = zero(Int64)
allbits0( ::Type{Int128}  ) = zero(Int128)

allbits0() = allbits0(Int)
allbits0{T<:Unsigned}(x::T) = allbits0(T)
allbits0{T<:Signed  }(x::T) = allbits0(T)


allbits1( ::Type{Uint8}   ) = typemax(Uint8)
allbits1( ::Type{Uint16}  ) = typemax(Uint16)
allbits1( ::Type{Uint32}  ) = typemax(Uint32)
allbits1( ::Type{Uint64}  ) = typemax(Uint64)
allbits1( ::Type{Uint128} ) = typemax(Uint128)

allbits1( ::Type{Int8}    ) = reinterpret(Int8  , typemax(Uint8))
allbits1( ::Type{Int16}   ) = reinterpret(Int16 , typemax(Uint16))
allbits1( ::Type{Int32}   ) = reinterpret(Int32 , typemax(Uint32))
allbits1( ::Type{Int64}   ) = reinterpret(Int64 , typemax(Uint64))
allbits1( ::Type{Int128}  ) = reinterpret(Int128, typemax(Uint128))

allbits1() = allbits1(Int)
allbits1{T<:Unsigned}(x::T) = allbits1(T)
allbits1{T<:Signed  }(x::T) = allbits1(T)


# lsbs0(n)        == 1...10..0 (n 0b)
# msbs0(n)        == 0..01...1 (n 0b)
# bits0(n,lsbidx) == 1...10..01....1 (n 0b, ls0b at lsbidx)

lsbs0{T<:Unsigned}(n::T)              =  (allbits1(T) << n)
lsbs1{T<:Unsigned}(n::T)              = ~(allbits1(T) << n)
msbs0{T<:Unsigned}(n::T)              = lsbs1(bitsizeof(T)-n)
msbs1{T<:Unsigned}(n::T)              = ~(lsbs1(bitsizeof(T)-n))

lsbs0{T<:Unsigned}(::Type{T}, n::Int) = lsbs0(convert(T,n))
lsbs1{T<:Unsigned}(::Type{T}, n::Int) = lsbs1(convert(T,n))
msbs0{T<:Unsigned}(::Type{T}, n::Int) = msbs0(convert(T,n))
msbs1{T<:Unsigned}(::Type{T}, n::Int) = msbs1(convert(T,n))

lsbs0{T<:Signed}(n::T)                = convert(Signed,lsbs0(convert(Unsigned,n)))
lsbs1{T<:Signed}(n::T)                = convert(Signed,lsbs1(convert(Unsigned,n)))
msbs0{T<:Signed}(n::T)                = convert(Signed,msbs0(convert(Unsigned,n)))
msbs1{T<:Signed}(n::T)                = convert(Signed,msbs1(convert(Unsigned,n)))

lsbs0{T<:Signed}(::Type{T}, n::Int)   = lsbs0(convert(T,n))
lsbs1{T<:Signed}(::Type{T}, n::Int)   = lsbs1(convert(T,n))
msbs0{T<:Signed}(::Type{T}, n::Int)   = msbs0(convert(T,n))
msbs1{T<:Signed}(::Type{T}, n::Int)   = msbs1(convert(T,n))


# shift_up_0s: (n=2) 0b1110..01..011 ==> 0b..01..01100
# shift_dn_0s: (n=2) 0b1110..01..011 ==> 0b001110..01..0

shift0s_up(x::Integer, n::Integer) = (x <<  n)
shift0s_dn(x::Integer, n::Integer) = (x >>> n)

shift1s_up(x::Integer, n::Integer) = (x <<  n) | lsbs1(n)
shift1s_dn(x::Integer, n::Integer) = (x >>> n) | msbs1(n)

# signs_differ
# this algorithm is credited to Manfred Weis (2009) and given at
# http://graphics.stanford.edu/~seander/bithacks.html#DetectOppositeSigns
#

for T in (:Int8, :Int16, :Int32, :Int64, :Int128)
    @eval begin
        signs_differ(h::($T), k::($T)) = ((h $ k) <  zero(($T)))
        signs_same  (h::($T), k::($T)) = ((h $ k) >= zero(($T)))
end end

for T in (:Uint8, :Uint16, :Uint32, :Uint64, :Uint128)
    @eval begin
        signs_differ(h::($T), k::($T)) = false
        signs_same  (h::($T), k::($T)) = true
end end


signs_differ(h::Float32, k::Float32) =
    signs_differ(reinterpret(Int32,h),reinterpret(Int32,k))

signs_differ(h::Float64, k::Float64) =
    signs_differ(reinterpret(Int64,h),reinterpret(Int64,k))

signs_same(h::Float32, k::Float32) =
    signs_same(reinterpret(Int32,h),reinterpret(Int32,k))

signs_same(h::Float64, k::Float64) =
    signs_same(reinterpret(Int64,h),reinterpret(Int64,k))



end # module

import BitLevel
