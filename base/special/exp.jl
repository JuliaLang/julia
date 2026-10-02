# This file is a part of Julia. License is MIT: https://julialang.org/license

# magic rounding constant: 1.5*2^52 Adding, then subtracting it from a float rounds it to an Int.
# This works because eps(MAGIC_ROUND_CONST(T)) == one(T), so adding it to a smaller number aligns the lsb to the 1s place.
# Values for which this trick doesn't work are going to have outputs of 0 or Inf.
MAGIC_ROUND_CONST(::Type{Float64}) = 6.755399441055744e15
MAGIC_ROUND_CONST(::Type{Float32}) = 1.048576f7

# max, min, and subnormal arguments
# max_exp = T(exponent_bias(T)*log(base, big(2)) + log(base, 2 - big(2.0)^-significand_bits(T)))
MAX_EXP(n::Val{2}, ::Type{Float32}) = 128.0f0
MAX_EXP(n::Val{2}, ::Type{Float64}) = 1024.0
MAX_EXP(n::Val{:ℯ}, ::Type{Float32}) = 88.72284f0
MAX_EXP(n::Val{:ℯ}, ::Type{Float64}) = 709.7827128933841
MAX_EXP(n::Val{10}, ::Type{Float32}) = 38.53184f0
MAX_EXP(n::Val{10}, ::Type{Float64}) = 308.25471555991675

# min_exp = T(-(exponent_bias(T)+significand_bits(T)) * log(base, big(2)))
MIN_EXP(n::Val{2}, ::Type{Float32}) = -150.0f0
MIN_EXP(n::Val{2}, ::Type{Float64}) = -1075.0
MIN_EXP(n::Val{:ℯ}, ::Type{Float32}) = -103.97208f0
MIN_EXP(n::Val{:ℯ}, ::Type{Float64}) = -745.1332191019412
MIN_EXP(n::Val{10}, ::Type{Float32}) = -45.1545f0
MIN_EXP(n::Val{10}, ::Type{Float64}) = -323.60724533877976

# subnorm_exp = abs(log(base, floatmin(T))), rounded up for Float32
# these vals are positive since it's easier to take abs(x) than -abs(x)
SUBNORM_EXP(n::Val{2}, ::Type{Float32}) = 126.00001f0
SUBNORM_EXP(n::Val{2}, ::Type{Float64}) = 1022.0
SUBNORM_EXP(n::Val{:ℯ}, ::Type{Float32}) = 87.33655f0
SUBNORM_EXP(n::Val{:ℯ}, ::Type{Float64}) = 708.3964185322641
SUBNORM_EXP(n::Val{10}, ::Type{Float32}) = 37.929783f0 # rounded up so the strict checks are exact
SUBNORM_EXP(n::Val{10}, ::Type{Float64}) = 307.6526555685887

# 256/log(base, 2) (For Float64 reductions)
LogBo256INV(::Val{2}, ::Type{Float64}) = 256.0
LogBo256INV(::Val{:ℯ}, ::Type{Float64}) = 369.3299304675746
LogBo256INV(::Val{10}, ::Type{Float64}) = 850.4135922911647

# -log(base, 2)/256 in upper and lower bits
# Upper is truncated to only have 34 bits of significand since N has at most
# ceil(log2(-MIN_EXP(Val(2), Float64)*LogBo256INV(Val(2), Float64))) = 19 bits.
# This ensures no rounding when multiplying LogBo256U*N for FMAless hardware
LogBo256U(::Val{2}, ::Type{Float64}) = -0.00390625
LogBo256U(::Val{:ℯ}, ::Type{Float64}) = -0.002707606173999011
LogBo256U(::Val{10}, ::Type{Float64}) = -0.0011758984204561784
LogBo256L(::Val{2}, ::Type{Float64}) = 0.0
LogBo256L(::Val{:ℯ}, ::Type{Float64}) = -6.327543041662719e-14
LogBo256L(::Val{10}, ::Type{Float64}) = -1.0624811566412999e-13

# 64/log(base, 2) (For Float32 reductions)
LogBo64INV(::Val{2}, ::Type{Float32}) = 64.0f0
LogBo64INV(::Val{:ℯ}, ::Type{Float32}) = 92.33248f0
LogBo64INV(::Val{10}, ::Type{Float32}) = 212.6034f0
# -log(base, 2)/64 in upper and lower bits, split as for LogBo256U/L: the upper part has
# 10 bits of significand since N = round(x*LogBo64INV) has at most 14 bits for Float32
LogBo64U(::Val{2}, ::Type{Float32}) = -0.015625f0
LogBo64U(::Val{:ℯ}, ::Type{Float32}) = -0.01083374f0
LogBo64U(::Val{10}, ::Type{Float32}) = -0.0047073364f0
LogBo64L(::Val{2}, ::Type{Float32}) = 0.0f0
LogBo64L(::Val{:ℯ}, ::Type{Float32}) = 3.3155382f-6
LogBo64L(::Val{10}, ::Type{Float32}) = 3.7427435f-6

# Range reduced kernels
function expm1b_kernel(::Val{2}, x::Float64)
    return x * evalpoly(x, (0.6931471805599393, 0.24022650695910058,
                            0.05550411502333161, 0.009618129548366803))
end
function expm1b_kernel(::Val{:ℯ}, x::Float64)
    return x * evalpoly(x, (0.9999999999999912, 0.4999999999999997,
                            0.1666666857598779, 0.04166666857598777))
end
function expm1b_kernel(::Val{10}, x::Float64)
    return x * evalpoly(x, (2.3025850929940255, 2.6509490552391974,
                            2.034678825384765, 1.1712552025835192))
end

function expm1b_kernel(::Val{2}, x::Float32)
    return x * evalpoly(x, (0.6931472f0, 0.2402265f0, 0.05550411f0))
end
function expm1b_kernel(::Val{:ℯ}, x::Float32)
    return x * evalpoly(x, (1.0f0, 0.5f0, 0.16666667f0))
end
function expm1b_kernel(::Val{10}, x::Float32)
    return x * evalpoly(x, (2.3025851f0, 2.650949f0, 2.0346787f0))
end
# Kernels for Float16 (evaluated in Float32 on |r| <= log(base, 2)/2)
function expb_kernel(::Val{2}, x::Float32)
    return evalpoly(x, (1.0f0, 0.6931472f0, 0.2402265f0,
                        0.05550411f0, 0.009618025f0,
                        0.0013333423f0, 0.00015469732f0, 1.5316464f-5))
end
function expb_kernel(::Val{:ℯ}, x::Float32)
    return evalpoly(x, (1.0f0, 1.0f0, 0.5f0, 0.16666667f0,
                        0.041666217f0, 0.008333249f0,
                        0.001394858f0, 0.00019924171f0))
end
function expb_kernel(::Val{10}, x::Float32)
    return evalpoly(x, (1.0f0, 2.3025851f0, 2.650949f0,
                        2.0346787f0, 1.1712426f0, 0.53937745f0,
                        0.20788547f0, 0.06837386f0))
end

# Table stores data with 60 sig figs by using the fact that the first 12 bits of all the
# values would be the same if stored as regular Float64.
# This only gains 8 bits since the least significant 4 bits of the exponent
# of the small part are not the same for all table entries
const JU_MASK = typemax(UInt64)>>12
const JL_MASK = typemax(UInt64)>>8
const JU_CONST = 0x3FF0000000000000
const JL_CONST = 0x3C00000000000000


#function make_table(size)
#    t_array = zeros(UInt64, size);
#    for j in 1:size
#        val = 2.0^(BigFloat(j-1)/size)
#        valU = Float64(val, RoundDown)
#        valL = Float64(val-valU)
#        valU = reinterpret(UInt64, valU) & JU_MASK
#        valL = ((reinterpret(UInt64, valL) & JL_MASK)>>44)<<52
#        t_array[j] = valU | valL
#    end
#    return Tuple(t_array)
#end
#const J_TABLE = make_table(256);
const J_TABLE = (0x0000000000000000, 0xaac00b1afa5abcbe, 0x9b60163da9fb3335, 0xab502168143b0280, 0xadc02c9a3e778060,
                 0x656037d42e11bbcc, 0xa7a04315e86e7f84, 0x84c04e5f72f654b1, 0x8d7059b0d3158574, 0xa510650a0e3c1f88,
                 0xa8d0706b29ddf6dd, 0x83207bd42b72a836, 0x6180874518759bc8, 0xa4b092bdf66607df, 0x91409e3ecac6f383,
                 0x85d0a9c79b1f3919, 0x98a0b5586cf9890f, 0x94f0c0f145e46c85, 0x9010cc922b7247f7, 0xa210d83b23395deb,
                 0x4030e3ec32d3d1a2, 0xa5b0efa55fdfa9c4, 0xae40fb66affed31a, 0x8d41073028d7233e, 0xa4911301d0125b50,
                 0xa1a11edbab5e2ab5, 0xaf712abdc06c31cb, 0xae8136a814f204aa, 0xa661429aaea92ddf, 0xa9114e95934f312d,
                 0x82415a98c8a58e51, 0x58f166a45471c3c2, 0xab9172b83c7d517a, 0x70917ed48695bbc0, 0xa7718af9388c8de9,
                 0x94a1972658375d2f, 0x8e51a35beb6fcb75, 0x97b1af99f8138a1c, 0xa351bbe084045cd3, 0x9001c82f95281c6b,
                 0x9e01d4873168b9aa, 0xa481e0e75eb44026, 0xa711ed5022fcd91c, 0xa201f9c18438ce4c, 0x8dc2063b88628cd6,
                 0x935212be3578a819, 0x82a21f49917ddc96, 0x8d322bdda27912d1, 0x99b2387a6e756238, 0x8ac2451ffb82140a,
                 0x8ac251ce4fb2a63f, 0x93e25e85711ece75, 0x82b26b4565e27cdd, 0x9e02780e341ddf29, 0xa2d284dfe1f56380,
                 0xab4291ba7591bb6f, 0x86129e9df51fdee1, 0xa352ab8a66d10f12, 0xafb2b87fd0dad98f, 0xa572c57e39771b2e,
                 0x9002d285a6e4030b, 0x9d12df961f641589, 0x71c2ecafa93e2f56, 0xaea2f9d24abd886a, 0x86f306fe0a31b715,
                 0x89531432edeeb2fd, 0x8a932170fc4cd831, 0xa1d32eb83ba8ea31, 0x93233c08b26416ff, 0xab23496266e3fa2c,
                 0xa92356c55f929ff0, 0xa8f36431a2de883a, 0xa4e371a7373aa9ca, 0xa3037f26231e7549, 0xa0b38cae6d05d865,
                 0xa3239a401b7140ee, 0xad43a7db34e59ff6, 0x9543b57fbfec6cf4, 0xa083c32dc313a8e4, 0x7fe3d0e544ede173,
                 0x8ad3dea64c123422, 0xa943ec70df1c5174, 0xa413fa4504ac801b, 0x8bd40822c367a024, 0xaf04160a21f72e29,
                 0xa3d423fb27094689, 0xab8431f5d950a896, 0x88843ffa3f84b9d4, 0x48944e086061892d, 0xae745c2042a7d231,
                 0x9c946a41ed1d0057, 0xa1e4786d668b3236, 0x73c486a2b5c13cd0, 0xab1494e1e192aed1, 0x99c4a32af0d7d3de,
                 0xabb4b17dea6db7d6, 0x7d44bfdad5362a27, 0x9054ce41b817c114, 0x98e4dcb299fddd0d, 0xa564eb2d81d8abfe,
                 0xa5a4f9b2769d2ca6, 0x7a2508417f4531ee, 0xa82516daa2cf6641, 0xac65257de83f4eee, 0xabe5342b569d4f81,
                 0x879542e2f4f6ad27, 0xa8a551a4ca5d920e, 0xa7856070dde910d1, 0x99b56f4736b527da, 0xa7a57e27dbe2c4ce,
                 0x82958d12d497c7fd, 0xa4059c0827ff07cb, 0x9635ab07dd485429, 0xa245ba11fba87a02, 0x3c45c9268a5946b7,
                 0xa195d84590998b92, 0x9ba5e76f15ad2148, 0xa985f6a320dceb70, 0xa60605e1b976dc08, 0x9e46152ae6cdf6f4,
                 0xa636247eb03a5584, 0x984633dd1d1929fd, 0xa8e6434634ccc31f, 0xa28652b9febc8fb6, 0xa226623882552224,
                 0xa85671c1c70833f5, 0x60368155d44ca973, 0x880690f4b19e9538, 0xa216a09e667f3bcc, 0x7a36b052fa75173e,
                 0xada6c012750bdabe, 0x9c76cfdcddd47645, 0xae46dfb23c651a2e, 0xa7a6ef9298593ae4, 0xa9f6ff7df9519483,
                 0x59d70f7466f42e87, 0xaba71f75e8ec5f73, 0xa6f72f8286ead089, 0xa7a73f9a48a58173, 0x90474fbd35d7cbfd,
                 0xa7e75feb564267c8, 0x9b777024b1ab6e09, 0x986780694fde5d3f, 0x934790b938ac1cf6, 0xaaf7a11473eb0186,
                 0xa207b17b0976cfda, 0x9f17c1ed0130c132, 0x91b7d26a62ff86f0, 0x7057e2f336cf4e62, 0xabe7f3878491c490,
                 0xa6c80427543e1a11, 0x946814d2add106d9, 0xa1582589994cce12, 0x9998364c1eb941f7, 0xa9c8471a4623c7ac,
                 0xaf2857f4179f5b20, 0xa01868d99b4492ec, 0x85d879cad931a436, 0x99988ac7d98a6699, 0x9d589bd0a478580f,
                 0x96e8ace5422aa0db, 0x9ec8be05bad61778, 0xade8cf3216b5448b, 0xa478e06a5e0866d8, 0x85c8f1ae99157736,
                 0x959902fed0282c8a, 0xa119145b0b91ffc5, 0xab2925c353aa2fe1, 0xae893737b0cdc5e4, 0xa88948b82b5f98e4,
                 0xad395a44cbc8520e, 0xaf296bdd9a7670b2, 0xa1797d829fde4e4f, 0x7ca98f33e47a22a2, 0xa749a0f170ca07b9,
                 0xa119b2bb4d53fe0c, 0x7c79c49182a3f090, 0xa579d674194bb8d4, 0x7829e86319e32323, 0xaad9fa5e8d07f29d,
                 0xa65a0c667b5de564, 0x9c6a1e7aed8eb8bb, 0x963a309bec4a2d33, 0xa2aa42c980460ad7, 0xa16a5503b23e255c,
                 0x650a674a8af46052, 0x9bca799e1330b358, 0xa58a8bfe53c12e58, 0x90fa9e6b5579fdbf, 0x889ab0e521356eba,
                 0xa81ac36bbfd3f379, 0x97ead5ff3a3c2774, 0x97aae89f995ad3ad, 0xa5aafb4ce622f2fe, 0xa21b0e07298db665,
                 0x94db20ce6c9a8952, 0xaedb33a2b84f15fa, 0xac1b468415b749b0, 0xa1cb59728de55939, 0x92ab6c6e29f1c52a,
                 0xad5b7f76f2fb5e46, 0xa24b928cf22749e3, 0xa08ba5b030a10649, 0xafcbb8e0b79a6f1e, 0x823bcc1e904bc1d2,
                 0xafcbdf69c3f3a206, 0xa08bf2c25bd71e08, 0xa89c06286141b33c, 0x811c199bdd85529c, 0xa48c2d1cd9fa652b,
                 0x9b4c40ab5fffd07a, 0x912c544778fafb22, 0x928c67f12e57d14b, 0xa86c7ba88988c932, 0x71ac8f6d9406e7b5,
                 0xaa0ca3405751c4da, 0x750cb720dcef9069, 0xac5ccb0f2e6d1674, 0xa88cdf0b555dc3f9, 0xa2fcf3155b5bab73,
                 0xa1ad072d4a07897b, 0x955d1b532b08c968, 0xa15d2f87080d89f1, 0x93dd43c8eacaa1d6, 0x82ed5818dcfba487,
                 0x5fed6c76e862e6d3, 0xa77d80e316c98397, 0x9a0d955d71ff6075, 0x9c2da9e603db3285, 0xa24dbe7cd63a8314,
                 0x92ddd321f301b460, 0xa1ade7d5641c0657, 0xa72dfc97337b9b5e, 0xadae11676b197d16, 0xa42e264614f5a128,
                 0xa30e3b333b16ee11, 0x839e502ee78b3ff6, 0xaa7e653924676d75, 0x92de7a51fbc74c83, 0xa77e8f7977cdb73f,
                 0xa0bea4afa2a490d9, 0x948eb9f4867cca6e, 0xa1becf482d8e67f0, 0x91cee4aaa2188510, 0x9dcefa1bee615a27,
                 0xa66f0f9c1cb64129, 0x93af252b376bba97, 0xacdf3ac948dd7273, 0x99df50765b6e4540, 0x9faf6632798844f8,
                 0xa12f7bfdad9cbe13, 0xaeef91d802243c88, 0x874fa7c1819e90d8, 0xacdfbdba3692d513, 0x62efd3c22b8f71f1, 0x74afe9d96b2a23d9)

# :nothrow needed since the compiler can't prove `ind` is inbounds.
Base.@assume_effects :nothrow function table_unpack(ind::Int32)
    ind = ind & 255 + 1 # 255 == length(J_TABLE) - 1
    j = getfield(J_TABLE, ind) # use getfield so the compiler can prove consistent
    jU = reinterpret(Float64, JU_CONST | (j&JU_MASK))
    jL = reinterpret(Float64, JL_CONST | (j>>8))
    return jU, jL
end

# 2^(j/64) for j = 0:63, each stored as a Float32 hi part (upper 32 bits) and lo part (lower 32 bits)
const J_TABLE_F32 = (0x3f80000000000000, 0x3f8164d2b1c43fd0, 0x3f82cd87b34ea7a9, 0x3f843a29b2f14c87, 0x3f85aac3334f9891,
                     0x3f871f62b352c2e6, 0x3f88980fb37eda4b, 0x3f8a14d5336a92de, 0x3f8b95c2b260aba1, 0x3f8d1adf3336fcb7,
                     0x3f8ea43ab3697465, 0x3f9031dc330628cd, 0x3f91c3d333675624, 0x3f935a2b32bc4f9c, 0x3f94f4f0b32e0212,
                     0x3f96942d32dc8061, 0x3f9837f033231b71, 0x3f99e046b359be90, 0x3f9b8d3ab30c5563, 0x3f9d3edab331a601,
                     0x3f9ef53233412342, 0x3fa0b05131fb9715, 0x3fa2704330c3125a, 0x3fa43516b323ec33, 0x3fa5fed7b32c9d5e,
                     0x3fa7cd94b3162d36, 0x3fa9a15bb3162b08, 0x3fab7a3ab314ad82, 0x3fad583fb22deaf6, 0x3faf3b79b3252deb,
                     0x3fb123f6b37c5aa8, 0x3fb311c432154889, 0x3fb504f332cfe77a, 0x3fb6fd92b266b974, 0x3fb8fbaf330ec5f7,
                     0x3fbaff5bb31bd983, 0x3fbd08a4b3414fe8, 0x3fbf179ab3130b1a, 0x3fc12c4db2d6663e, 0x3fc346cdb2976da2,
                     0x3fc5672a320aa837, 0x3fc78d75b2dd5119, 0x3fc9b9beb37323a2, 0x3fcbec15b006c6c2, 0x3fce248c3228fc24,
                     0x3fd06334b2944353, 0x3fd2a81eb35c1daa, 0x3fd4f35bb3286024, 0x3fd744fdb2d4a58a, 0x3fd99d16b2f61d41,
                     0x3fdbfbb8b3504a1c, 0x3fde60f5b37b43e3, 0x3fe0ccdfb21eab59, 0x3fe33f8933657d15, 0x3fe5b907b2441be6,
                     0x3fe8396a33207898, 0x3feac0c7b24116de, 0x3fed4f303276cca1, 0x3fefe4bab348464a, 0x3ff2817732f167ff,
                     0x3ff5257d32292436, 0x3ff7d0df336615a2, 0x3ffa83b3b2923758, 0x3ffd3e0c31cf486c)

# :nothrow needed since the compiler can't prove `ind` is inbounds.
Base.@assume_effects :nothrow function table_unpack_f32(ind::Int32)
    j = getfield(J_TABLE_F32, ind & 63 + 1)
    return reinterpret(Float32, (j >> 32) % UInt32), reinterpret(Float32, j % UInt32)
end

# Method for Float64 and Float32, with a table of S = 256 (Float64) or S = 64 (Float32) entries
# 1. Argument reduction: Given x and base b, find r and integers k, j such that
#       x = (k + j/S)*log(b, 2) + r,  0 <= j < S, |r| <= log(b, 2)/(2S).
#
# 2. Approximate b^r - 1 by a polynomial p_b(r): a degree 4 minimax polynomial for Float64,
#    and the degree 3 Taylor polynomial for Float32. Since the bounds on r are very tight,
#    this is sufficient to be accurate to floating point epsilon.
#
# 3. Scale back: b^x = 2^k * 2^(j/S) * (1 + p_b(r))
#    2^(j/S) is stored for every j as a sum of two floats, so the result only rounds once to good accuracy.
#    Float32 does all of its arithmetic in Float32 so that the method vectorizes well.
# If b^x is subnormal, a little extra care needs to be taken: the scaling back is done in 2 steps,
# since just messing with the exponent wouldn't work.

@inline function exp_impl(x::Float64, base)
    T = Float64
    N_float = muladd(x, LogBo256INV(base, T), MAGIC_ROUND_CONST(T))
    N = reinterpret(UInt64, N_float) % Int32
    N_float -=  MAGIC_ROUND_CONST(T) #N_float now equals round(x*LogBo256INV(base, T))
    r = muladd(N_float, LogBo256U(base, T), x)
    r = muladd(N_float, LogBo256L(base, T), r)
    k = N >> 8
    jU, jL = table_unpack(N)
    small_part =  muladd(jU, expm1b_kernel(base, r), jL) + jU

    if !(abs(x) <= SUBNORM_EXP(base, T))
        isnan(x) && return x
        x >= MAX_EXP(base, T) && return Inf
        x <= MIN_EXP(base, T) && return 0.0
        if k <= -53
            # The UInt64 forces promotion. (Only matters for 32 bit systems.)
            twopk = ((k % UInt64) +% UInt64(53)) << 52
            return reinterpret(T, twopk +% reinterpret(UInt64, small_part))*0x1p-53
        end
        #k == 1024 && return (small_part * 2.0) * 0x1p1023
    end
    twopk = Int64(k) << 52
    return reinterpret(T, twopk + reinterpret(Int64, small_part))
end
# Computes base^(x+xlo). Used for pow.
@inline function exp_impl(x::Float64, xlo::Float64, base)
    T = Float64
    N_float = muladd(x, LogBo256INV(base, T), MAGIC_ROUND_CONST(T))
    N = reinterpret(UInt64, N_float) % Int32
    N_float -=  MAGIC_ROUND_CONST(T) #N_float now equals round(x*LogBo256INV(base, T))
    r = muladd(N_float, LogBo256U(base, T), x)
    r = muladd(N_float, LogBo256L(base, T), r)
    k = N >> 8
    jU, jL = table_unpack(N)
    kern = expm1b_kernel(base, r)
    very_small = muladd(kern, jU*xlo, jL)
    hi, lo = Base.canonicalize2(1.0, kern)
    small_part = fma(jU, hi, muladd(jU, (lo+xlo), very_small))
    if !(abs(x) <= SUBNORM_EXP(base, T))
        isnan(x) && return x
        x >= MAX_EXP(base, T) && return Inf
        x <= MIN_EXP(base, T) && return 0.0
        if k <= -53
            # The UInt64 forces promotion. (Only matters for 32 bit systems.)
            twopk = ((k % UInt64) +% UInt64(53)) << 52
            return reinterpret(T, twopk +% reinterpret(UInt64, small_part))*0x1p-53
        end
        k == 1024 && return (small_part * 2.0) * 0x1p1023
    end
    twopk = Int64(k) << 52
    return reinterpret(T, twopk + reinterpret(Int64, small_part))
end
@inline function exp_impl_fast(x::Float64, base)
    T = Float64
    x >= MAX_EXP(base, T) && return Inf
    x <= -SUBNORM_EXP(base, T) && return 0.0
    N_float = muladd(x, LogBo256INV(base, T), MAGIC_ROUND_CONST(T))
    N = reinterpret(UInt64, N_float) % Int32
    N_float -=  MAGIC_ROUND_CONST(T) #N_float now equals round(x*LogBo256INV(base, T))
    r = muladd(N_float, LogBo256U(base, T), x)
    r = muladd(N_float, LogBo256L(base, T), r)
    k = N >> 8
    jU, _ = table_unpack(N)
    small_part =  muladd(jU, expm1b_kernel(base, r), jU)
    twopk = Int64(k) << 52
    return reinterpret(T, twopk + reinterpret(Int64, small_part))
end

# Returns k and small_part such that base^x ≈ 2^k * small_part, with 2^(-1/128) <= small_part <= 2^(127/128)
@inline function exp_kernel_f32(x::Float32, base)
    T = Float32
    N_float = round(x*LogBo64INV(base, T))
    N = unsafe_trunc(Int32, N_float)
    r = muladd(N_float, LogBo64U(base, T), x)
    r = muladd(N_float, LogBo64L(base, T), r)
    jU, jL = table_unpack_f32(N)
    return N >> 6, muladd(jU, expm1b_kernel(base, r), jL) + jU
end

@inline function exp_impl(x::Float32, base)
    T = Float32
    k, small_part = exp_kernel_f32(x, base)
    if !(abs(x) < SUBNORM_EXP(base, T))
        isnan(x) && return x
        if x > 0
            x > MAX_EXP(base, T) && return Inf32
            # 2^k may not be representable, so scale in two steps
            return reinterpret(T, reinterpret(Int32, small_part) + ((k - Int32(1)) << Int32(23))) * 2.0f0
        else
            x < MIN_EXP(base, T) && return 0.0f0
            # the result is subnormal, so scale in two steps to round only once
            return reinterpret(T, reinterpret(Int32, small_part) + ((k + Int32(64)) << Int32(23))) * 5.421011f-20 # 2^-64
        end
    end
    return reinterpret(T, reinterpret(Int32, small_part) + (k << Int32(23)))
end

@inline function exp_impl_fast(x::Float32, base)
    T = Float32
    !(x < MAX_EXP(base, T)) && return x + Inf32 # overflow, or NaN
    x <= -SUBNORM_EXP(base, T) && return 0.0f0  # flush subnormal results to zero
    k, small_part = exp_kernel_f32(x, base)
    return reinterpret(T, reinterpret(Int32, small_part) + (k << Int32(23)))
end

@inline function exp_impl(a::Float16, base)
    T = Float32
    x = T(a)
    # Float16 needs only a single-word log(base, 2), so the Float32 reduction constants
    # are combined (the scalings by 64 are exact and fold to constants)
    N_float = round(x*(LogBo64INV(base, T)/64))
    N = unsafe_trunc(Int32, N_float)
    r = muladd(N_float, 64*(LogBo64U(base, T) + LogBo64L(base, T)), x)
    small_part = expb_kernel(base, r)
    if !(abs(x) <= 25)
        x > 16 && return Inf16
        x < 25 && return zero(Float16)
    end
    twopk = reinterpret(T, (N+Int32(127)) << Int32(23))
    return Float16(twopk*small_part)
end

for (func, fast_func, base) in ((:exp2,  :exp2_fast,  Val(2)),
                                (:exp,   :exp_fast,   Val(:ℯ)),
                                (:exp10, :exp10_fast, Val(10)))
    @eval begin
        @noinline $func(x::Union{Float16,Float32,Float64}) = exp_impl(x, $base)
        @noinline $fast_func(x::Union{Float32,Float64}) = exp_impl_fast(x, $base)
    end
end

@doc """
    exp(x)

Compute the natural base exponential of `x`, in other words ``ℯ^x``.

See also [`exp2`](@ref), [`exp10`](@ref), [`expm1`](@ref), [`cis`](@ref), [`log`](@ref).

# Examples
```jldoctest
julia> exp(1.0)
2.718281828459045

julia> exp(im * pi) ≈ cis(pi)
true
```
""" exp(x::Real)

"""
    exp2(x)

Compute the base 2 exponential of `x`, in other words ``2^x``.

See also [`exp`](@ref), [`log2`](@ref), [`ldexp`](@ref), [`<<`](@ref).

# Examples
```jldoctest
julia> exp2(5)
32.0

julia> 2^5
32

julia> exp2(63) > typemax(Int)
true
```
"""
exp2(x)

"""
    exp10(x)

Compute the base 10 exponential of `x`, in other words ``10^x``.

See also, [`exp`](@ref), [`log10`](@ref).

# Examples
```jldoctest
julia> exp10(2)
100.0

julia> 10^2
100
```
"""
exp10(x)

# functions with special cases for integer arguments
@inline function exp2(x::Base.BitInteger)
    if x > 1023
        Inf64
    elseif x < -1074
        0.0
    elseif x <= -1023
        # if -1073 < x <= -1023 then Result will be a subnormal number
        # Hex literal with padding must be used to work on 32bit machine
        reinterpret(Float64, 0x0000_0000_0000_0001 << ((x + 1074) % UInt))
    else
        # We will cast everything to Int64 to avoid errors in case of Int128
        # If x is an Int128, and is outside the range of Int64, then it is not -1023<x<=1023
        reinterpret(Float64, (exponent_bias(Float64) + (x % Int64)) << (significand_bits(Float64) % UInt))
    end
end

# min and max arguments for expm1 by type
MAX_EXP(::Type{Float64}) =  709.7827128933845   # log 2^1023*(2-2^-52)
MIN_EXP(::Type{Float64}) = -37.42994775023705   # log 2^-54
MAX_EXP(::Type{Float16}) =  Float16(11.09)      # log 2^15 *(2-2^-10)
MIN_EXP(::Type{Float16}) = -Float16(8.32)       # log 2^-12

# log(.75) <= x <= log(1.25)
@inline function expm1_small(x::Float64)
    p = evalpoly(x, (0.16666666666666632, 0.04166666666666556, 0.008333333333401227,
                     0.001388888889068783, 0.00019841269447671544, 2.480157691845342e-5,
                     2.7558212415361945e-6, 2.758218402815439e-7, 2.4360682937111612e-8))
    p2 = exthorner(x, (1.0, .5, p))
    return fma(x, p2[1], x*p2[2])
end
function expm1(x::Float64)
    T = Float64
    if -0.2876820724517809 <= x <= 0.22314355131420976
        return expm1_small(x)
    elseif !(abs(x)<=MIN_EXP(Float64))
        isnan(x) && return x
        x > MAX_EXP(Float64) && return Inf
        x < MIN_EXP(Float64) && return -1.0
    end

    N_float = muladd(x, LogBo256INV(Val(:ℯ), T), MAGIC_ROUND_CONST(T))
    N = reinterpret(UInt64, N_float) % Int32
    N_float -=  MAGIC_ROUND_CONST(T) #N_float now equals round(x*LogBo256INV(Val(:ℯ), T))
    r = muladd(N_float, LogBo256U(Val(:ℯ), T), x)
    r = muladd(N_float, LogBo256L(Val(:ℯ), T), r)
    k = Int64(N >> 8)
    jU, jL = table_unpack(N)
    p = expm1b_kernel(Val(:ℯ), r)
    twopk  = reinterpret(Float64, (1023+k) << 52)
    twopnk = reinterpret(Float64, (1023-k) << 52)
    k>=106 && return reinterpret(Float64, (1022+k) << 52)*(jU + muladd(jU, p, jL))*2
    k>=53 && return twopk*(jU + muladd(jU, p, (jL-twopnk)))
    k<=-2 && return twopk*(jU + muladd(jU, p, jL))-1
    return twopk*((jU-twopnk) + fma(jU, p, jL))
end

function expm1(x::Float32)
    # Computed in Float64 using the Float64 exp table: with x = (k + j/256 + r/256)*log(2),
    # expm1(x) = scale*(1 + p) - 1 where scale = 2^k*2^(j/256) and p = 2^(r/256) - 1.
    # scale - 1 is exact whenever it cancels, so the result rounds (almost) only once.
    if (reinterpret(UInt32, x) << 1) >= 0x8700_0000 # |x| >= 256, ±Inf, or NaN
        isnan(x) && return x
        return x > 0 ? Inf32 : -1.0f0
    end
    t = Float64(x) * LogBo256INV(Val(:ℯ), Float64)
    N_float = t + MAGIC_ROUND_CONST(Float64)
    N = reinterpret(UInt64, N_float) % Int32
    r = t - (N_float - MAGIC_ROUND_CONST(Float64)) # exact, |r| <= 1/2
    jU, _ = table_unpack(N)
    scale = reinterpret(Float64, reinterpret(Int64, jU) + (Int64(N >> 8) << 52))
    p = r * evalpoly(r, (0.0027076061740622863, 3.6655655969101062e-6, 3.3083026805413713e-9))
    return copysign(Float32(fma(scale, p, scale - 1.0)), x) # copysign keeps expm1(-0.0) == -0.0
end

function expm1(a::Float16)
    # Computed in Float32 with x = N*log(2) + r, |r| <= log(2)/2, as
    # expm1(x) = 2^N*(e^r - 1) + (2^N - 1), where 2^N - 1 is exact, so no separate
    # small-argument path is needed (N == 0 gives e^r - 1 directly).
    a > MAX_EXP(Float16) && return Inf16
    a < MIN_EXP(Float16) && return Float16(-1.0)
    iszero(a) && return a # keeps the sign of zero
    x = Float32(a)
    N = round(x*(LogBo64INV(Val(:ℯ), Float32)/64))
    r = muladd(N, 64*(LogBo64U(Val(:ℯ), Float32) + LogBo64L(Val(:ℯ), Float32)), x) # single-word -log(2), as in exp_impl(::Float16)
    r2 = r*r
    # e^r - 1 = r*(1 + r*q(r)) with q a degree 4 Taylor polynomial, evaluated with Estrin's scheme.
    # Computing p in this form (rather than as r + r^2*q(r)) matters: it rounds every Float16
    # input correctly, including ones whose correctly rounded Float32 result is a Float16 tie.
    q = muladd(r2, muladd(r2, 0.0013888889f0, muladd(r, 0.008333334f0, 0.041666668f0)), muladd(r, 0.16666667f0, 0.5f0))
    p = r * muladd(r, q, 1f0)
    twopk = reinterpret(Float32, (unsafe_trunc(Int32, N) + Int32(127)) << Int32(23))
    return Float16(fma(twopk, p, twopk - 1f0))
end

"""
    expm1(x)

Accurately compute ``e^x-1``. It avoids the loss of precision involved in the direct
evaluation of exp(x) - 1 for small values of x.

See also [`exp`](@ref), [`log1p`](@ref).

# Examples
```jldoctest
julia> expm1(1e-16)
1.0e-16

julia> exp(1e-16) - 1
0.0
```
"""
expm1(x)
