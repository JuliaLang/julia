immutable Float
    s::UInt64
    e::Int32
    de::Int32
end

Float() = Float(0,0,0)
Float(x,y) = Float(x,y,int32(0))
Float(d::FloatingPoint) = Float(_significand(d), _exponent(d))

# Consts
const Float10MSBits = 0xFFC0000000000000 # used normalize(Float)
const FloatSignMask = 0x8000000000000000 # used in normalize(Float)
const FloatSignificandSize = int32(64)

function normalize(v::Float)
    f = v.s
    e = v.e
    while (f & Float10MSBits) == 0
        f <<= 10
        e -= 10
    end
    while (f & FloatSignMask) == 0
        f <<= 1
        e -= 1
    end
    return Float(f,e)
end
function normalize(v::Float64)
    s = _significand(v); e = _exponent(v)
    while (s & HiddenBit(Float64)) == 0
        s <<= uint64(1)
        e -= int32(1)
    end
    s <<= uint64(FloatSignificandSize - SignificandSize(Float64))
    e -=  int32( FloatSignificandSize - SignificandSize(Float64))
    return Float(s, e)
end

# Float128
#DenormalExponent(::Type{Float128}) = int32(-ExponentBias(Float128) + 1)
#ExponentMask(::Type{Float128}) = 0x7fff0000000000000000000000000000
#PhysicalSignificandSize(::Type{Float128}) = int32(112)
#SignificandSize(::Type{Float128}) = int32(113)
#ExponentBias(::Type{Float128}) = int32(0x00003fff + PhysicalSignificandSize(Float128))
#SignificandMask(::Type{Float128}) = 0x0000ffffffffffffffffffffffffffff
#HiddenBit(::Type{Float128}) = 0x00010000000000000000000000000000
#uint_t(d::Float128) = reinterpret(UInt128,d)
# Float64
DenormalExponent(::Type{Float64}) = int32(-ExponentBias(Float64) + 1)
ExponentMask(::Type{Float64}) = 0x7FF0000000000000
PhysicalSignificandSize(::Type{Float64}) = int32(52)
SignificandSize(::Type{Float64}) = int32(53)
ExponentBias(::Type{Float64}) = int32(0x3FF + PhysicalSignificandSize(Float64))
SignificandMask(::Type{Float64}) = 0x000FFFFFFFFFFFFF
HiddenBit(::Type{Float64}) = 0x0010000000000000
uint_t(d::Float64) = reinterpret(UInt64,d)
# Float32
DenormalExponent(::Type{Float32}) = int32(-ExponentBias(Float32) + 1)
ExponentMask(::Type{Float32}) = 0x7F800000
PhysicalSignificandSize(::Type{Float32}) = int32(23)
SignificandSize(::Type{Float32}) = int32(24)
ExponentBias(::Type{Float32}) = int32(0x7F + PhysicalSignificandSize(Float32))
SignificandMask(::Type{Float32}) = 0x007FFFFF
HiddenBit(::Type{Float32}) = 0x00800000
uint_t(d::Float32) = reinterpret(UInt32,d)
# Float16
DenormalExponent(::Type{Float16}) = int32(-ExponentBias(Float16) + 1)
ExponentMask(::Type{Float16}) = 0x7c00
PhysicalSignificandSize(::Type{Float16}) = int32(10)
SignificandSize(::Type{Float16}) = int32(11)
ExponentBias(::Type{Float16}) = int32(0x000f + PhysicalSignificandSize(Float16))
SignificandMask(::Type{Float16}) = 0x03ff
HiddenBit(::Type{Float16}) = 0x0400
uint_t(d::Float16) = reinterpret(UInt16,d)

function _exponent{T<:FloatingPoint}(d::T)
  isdenormal(d) && return DenormalExponent(T)
  biased_e::Int32 = int32((uint_t(d) & ExponentMask(T)) >> PhysicalSignificandSize(T))
  return int32(biased_e - ExponentBias(T))
end
function _significand{T<:FloatingPoint}(d::T)
  s = uint_t(d) & SignificandMask(T)
  return !isdenormal(d) ? s + HiddenBit(T) : s
end
isdenormal{T<:FloatingPoint}(d::T) = (uint_t(d) & ExponentMask(T)) == 0

function normalizedbound(f::FloatingPoint)
    v = Float(_significand(f),_exponent(f))
    m_plus = normalize(Float((v.s << 1) + 1, v.e - 1))
    if lowerboundaryiscloser(f)
        m_minus = Float((v.s << 2) - 1, v.e - 2)
    else
        m_minus = Float((v.s << 1) - 1, v.e - 1)
    end
    return Float(m_minus.s << (m_minus.e - m_plus.e), m_plus.e), m_plus
end
function lowerboundaryiscloser{T<:FloatingPoint}(f::T)
    physical_significand_is_zero = (uint_t(f) & SignificandMask(T)) == 0
    return physical_significand_is_zero && (_exponent(f) != DenormalExponent(T))
end

(-)(a::Float,b::Float) = Float(a.s - b.s,a.e,a.de)

const FloatM32 = 0xFFFFFFFF

function (*)(this::Float,other::Float)
    a::UInt64 = this.s >> 32
    b::UInt64 = this.s & FloatM32
    c::UInt64 = other.s >> 32
    d::UInt64 = other.s & FloatM32
    ac::UInt64 = a * c
    bc::UInt64 = b * c
    ad::UInt64 = a * d
    bd::UInt64 = b * d
    tmp::UInt64 = (bd >> 32) + (ad & FloatM32) + (bc & FloatM32)
    # By adding 1U << 31 to tmp we round the final result.
    # Halfway cases will be round up.
    tmp += uint64(1) << 31
    result_f::UInt64 = ac + (ad >> 32) + (bc >> 32) + (tmp >> 32)
    return Float(result_f,this.e + other.e + 64,this.de)
end

const CachedPowers = Float[
  Float(0xfa8fd5a0081c0288, -1220, -348),
  Float(0xbaaee17fa23ebf76, -1193, -340),
  Float(0x8b16fb203055ac76, -1166, -332),
  Float(0xcf42894a5dce35ea, -1140, -324),
  Float(0x9a6bb0aa55653b2d, -1113, -316),
  Float(0xe61acf033d1a45df, -1087, -308),
  Float(0xab70fe17c79ac6ca, -1060, -300),
  Float(0xff77b1fcbebcdc4f, -1034, -292),
  Float(0xbe5691ef416bd60c, -1007, -284),
  Float(0x8dd01fad907ffc3c, -980, -276),
  Float(0xd3515c2831559a83, -954, -268),
  Float(0x9d71ac8fada6c9b5, -927, -260),
  Float(0xea9c227723ee8bcb, -901, -252),
  Float(0xaecc49914078536d, -874, -244),
  Float(0x823c12795db6ce57, -847, -236),
  Float(0xc21094364dfb5637, -821, -228),
  Float(0x9096ea6f3848984f, -794, -220),
  Float(0xd77485cb25823ac7, -768, -212),
  Float(0xa086cfcd97bf97f4, -741, -204),
  Float(0xef340a98172aace5, -715, -196),
  Float(0xb23867fb2a35b28e, -688, -188),
  Float(0x84c8d4dfd2c63f3b, -661, -180),
  Float(0xc5dd44271ad3cdba, -635, -172),
  Float(0x936b9fcebb25c996, -608, -164),
  Float(0xdbac6c247d62a584, -582, -156),
  Float(0xa3ab66580d5fdaf6, -555, -148),
  Float(0xf3e2f893dec3f126, -529, -140),
  Float(0xb5b5ada8aaff80b8, -502, -132),
  Float(0x87625f056c7c4a8b, -475, -124),
  Float(0xc9bcff6034c13053, -449, -116),
  Float(0x964e858c91ba2655, -422, -108),
  Float(0xdff9772470297ebd, -396, -100),
  Float(0xa6dfbd9fb8e5b88f, -369, -92),
  Float(0xf8a95fcf88747d94, -343, -84),
  Float(0xb94470938fa89bcf, -316, -76),
  Float(0x8a08f0f8bf0f156b, -289, -68),
  Float(0xcdb02555653131b6, -263, -60),
  Float(0x993fe2c6d07b7fac, -236, -52),
  Float(0xe45c10c42a2b3b06, -210, -44),
  Float(0xaa242499697392d3, -183, -36),
  Float(0xfd87b5f28300ca0e, -157, -28),
  Float(0xbce5086492111aeb, -130, -20),
  Float(0x8cbccc096f5088cc, -103, -12),
  Float(0xd1b71758e219652c, -77, -4),
  Float(0x9c40000000000000, -50, 4),
  Float(0xe8d4a51000000000, -24, 12),
  Float(0xad78ebc5ac620000, 3, 20),
  Float(0x813f3978f8940984, 30, 28),
  Float(0xc097ce7bc90715b3, 56, 36),
  Float(0x8f7e32ce7bea5c70, 83, 44),
  Float(0xd5d238a4abe98068, 109, 52),
  Float(0x9f4f2726179a2245, 136, 60),
  Float(0xed63a231d4c4fb27, 162, 68),
  Float(0xb0de65388cc8ada8, 189, 76),
  Float(0x83c7088e1aab65db, 216, 84),
  Float(0xc45d1df942711d9a, 242, 92),
  Float(0x924d692ca61be758, 269, 100),
  Float(0xda01ee641a708dea, 295, 108),
  Float(0xa26da3999aef774a, 322, 116),
  Float(0xf209787bb47d6b85, 348, 124),
  Float(0xb454e4a179dd1877, 375, 132),
  Float(0x865b86925b9bc5c2, 402, 140),
  Float(0xc83553c5c8965d3d, 428, 148),
  Float(0x952ab45cfa97a0b3, 455, 156),
  Float(0xde469fbd99a05fe3, 481, 164),
  Float(0xa59bc234db398c25, 508, 172),
  Float(0xf6c69a72a3989f5c, 534, 180),
  Float(0xb7dcbf5354e9bece, 561, 188),
  Float(0x88fcf317f22241e2, 588, 196),
  Float(0xcc20ce9bd35c78a5, 614, 204),
  Float(0x98165af37b2153df, 641, 212),
  Float(0xe2a0b5dc971f303a, 667, 220),
  Float(0xa8d9d1535ce3b396, 694, 228),
  Float(0xfb9b7cd9a4a7443c, 720, 236),
  Float(0xbb764c4ca7a44410, 747, 244),
  Float(0x8bab8eefb6409c1a, 774, 252),
  Float(0xd01fef10a657842c, 800, 260),
  Float(0x9b10a4e5e9913129, 827, 268),
  Float(0xe7109bfba19c0c9d, 853, 276),
  Float(0xac2820d9623bf429, 880, 284),
  Float(0x80444b5e7aa7cf85, 907, 292),
  Float(0xbf21e44003acdd2d, 933, 300),
  Float(0x8e679c2f5e44ff8f, 960, 308),
  Float(0xd433179d9c8cb841, 986, 316),
  Float(0x9e19db92b4e31ba9, 1013, 324),
  Float(0xeb96bf6ebadf77d9, 1039, 332),
  Float(0xaf87023b9bf0ee6b, 1066, 340)]

const CachedPowersLength = length(CachedPowers)
const CachedPowersOffset = 348  # -1 * the first decimal_exponent.
const D_1_LOG2_10 = 0.30102999566398114  #  1 / lg(10)
# Difference between the decimal exponents in the table above.
const DecimalExponentDistance = 8
const MinDecimalExponent = -348
const MaxDecimalExponent = 340

function binexp_cache(min_exponent,max_exponent)
    k = ceil(Integer,(min_exponent+63)*D_1_LOG2_10)
    index = div(CachedPowersOffset+k-1,DecimalExponentDistance) + 1
    cp = CachedPowers[index+1]
    return cp
end
