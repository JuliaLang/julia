# Implementation of
#  "Table-driven Implementation of the Logarithm Function in IEEE Floating-point Arithmetic"
#  Tang, Ping-Tak Peter
#  ACM Trans. Math. Softw. (1990), 16(4):378--400
#  http://dx.doi.org/10.1145/98267.98294

# Does not currently handle floating point flags (inexact, div-by-zero, etc).

import Base.unsafe_trunc

# Float64 lookup table.
# to generate values:
#   N=39 # (can be up to N=42).
#   sN = 2.0^N
#   isN = 1.0/sN
#   s7 = 2.0^7
#   is7 = 1.0/s7
#   for j=0:128
#     l_big = Base.log(big(1.0+j*is7))
#     l_hi = isN*Float64(round(sN*l_big))
#     l_lo = Float64(l_big-l_hi)
#     j % 2 == 0 && print("\n    ")
#     @printf "(%a,%a)," l_hi l_lo
#   end

const t_log_Float64 = [(0x0p+0,0x0p+0),(0x1.fe02a6b2p-8,-0x1.f30ee07912df9p-41),
    (0x1.fc0a8b1p-7,-0x1.fe0e183092c59p-42),(0x1.7b91b07d8p-6,-0x1.2772ab6c0559cp-41),
    (0x1.f829b0e78p-6,0x1.980267c7e09e4p-45),(0x1.39e87bap-5,-0x1.42a056fea4dfdp-41),
    (0x1.77458f634p-5,-0x1.2303b9cb0d5e1p-41),(0x1.b42dd7118p-5,0x1.71bec28d14c7ep-41),
    (0x1.f0a30c01p-5,0x1.62a6617cc9717p-41),(0x1.16536eea4p-4,-0x1.0a3e2f3b47d18p-41),
    (0x1.341d7961cp-4,-0x1.717b6b33e44f8p-43),(0x1.51b073f06p-4,0x1.83f69278e686ap-44),
    (0x1.6f0d28ae6p-4,-0x1.2968c836cc8c2p-41),(0x1.8c345d632p-4,-0x1.937c294d2f567p-42),
    (0x1.a926d3a4ap-4,0x1.aac6ca17a4554p-41),(0x1.c5e548f5cp-4,-0x1.c5e7514f4083fp-43),
    (0x1.e27076e2ap-4,0x1.e5cbd3d50fffcp-41),(0x1.fec9131dcp-4,-0x1.54555d1ae6607p-44),
    (0x1.0d77e7cd1p-3,-0x1.c69a65a23a17p-41),(0x1.1b72ad52fp-3,0x1.9e80a41811a39p-41),
    (0x1.29552f82p-3,-0x1.5b967f4471dfcp-44),(0x1.371fc201fp-3,-0x1.c22f10c9a4ea8p-41),
    (0x1.44d2b6ccbp-3,0x1.f4799f4f6543ep-41),(0x1.526e5e3a2p-3,-0x1.2f21746ff8a47p-41),
    (0x1.5ff3070a8p-3,-0x1.b0b0de3077d7ep-41),(0x1.6d60fe71ap-3,-0x1.6f1b955c4d1dap-42),
    (0x1.7ab890211p-3,-0x1.37b720e4a694bp-42),(0x1.87fa06521p-3,-0x1.b77b7effb7f41p-42),
    (0x1.9525a9cf4p-3,0x1.5ad1d904c1d4ep-41),(0x1.a23bc1fe3p-3,-0x1.2a739b23b93e1p-41),
    (0x1.af3c94e81p-3,-0x1.00349cc67f9b2p-41),(0x1.bc286742ep-3,-0x1.cca75818c5dbcp-41),
    (0x1.c8ff7c79bp-3,-0x1.97794f689f843p-41),(0x1.d5c216b5p-3,-0x1.11ba91bbca682p-41),
    (0x1.e27076e2bp-3,-0x1.a342c2af0003cp-44),(0x1.ef0adcbdcp-3,0x1.64d948637950ep-41),
    (0x1.fb9186d5ep-3,0x1.f1546aaa3361cp-42),(0x1.0402594b5p-2,-0x1.7df928ec217a5p-41),
    (0x1.0a324e2738p-2,0x1.0e35f73f7a018p-42),(0x1.1058bf9ae8p-2,-0x1.a9573b02faa5ap-41),
    (0x1.1675cabab8p-2,0x1.30701ce63eab9p-41),(0x1.1c898c1698p-2,0x1.9fafbc68e754p-42),
    (0x1.22941fbcf8p-2,-0x1.a6976f5eb0963p-44),(0x1.2895a13de8p-2,0x1.a8d7ad24c13fp-44),
    (0x1.2e8e2bae1p-2,0x1.d309c2cc91a85p-42),(0x1.347dd9a988p-2,-0x1.5594dd4c58092p-45),
    (0x1.3a64c55698p-2,-0x1.d0b1c68651946p-41),(0x1.4043086868p-2,0x1.3f1de86093efap-41),
    (0x1.4618bc21c8p-2,-0x1.09ec17a426426p-41),(0x1.4be5f95778p-2,-0x1.d7c92cd9ad824p-44),
    (0x1.51aad872ep-2,-0x1.f4bd8db0a7cc1p-44),(0x1.5767717458p-2,-0x1.2c9d5b2a49af9p-41),
    (0x1.5d1bdbf58p-2,0x1.394a11b1c1ee4p-43),(0x1.62c82f2bap-2,-0x1.c356848506eadp-41),
    (0x1.686c81e9bp-2,0x1.4aec442be1015p-42),(0x1.6e08eaa2b8p-2,0x1.0f1c609c98c6cp-41),
    (0x1.739d7f6bcp-2,-0x1.7fcb18ed9d603p-41),(0x1.792a55fdd8p-2,-0x1.c2ec1f512dc03p-41),
    (0x1.7eaf83b828p-2,0x1.7e1b259d2f3dap-41),(0x1.842d1da1e8p-2,0x1.62e927628cbc2p-43),
    (0x1.89a3386c18p-2,-0x1.ed2a52c73bf78p-41),(0x1.8f11e87368p-2,-0x1.d3881e8962a96p-42),
    (0x1.947941c21p-2,0x1.6faba4cdd147dp-42),(0x1.99d958118p-2,-0x1.f753456d113b8p-42),
    (0x1.9f323ecbf8p-2,0x1.84bf2b68d766fp-42),(0x1.a484090e58p-2,0x1.d8515fe535b87p-41),
    (0x1.a9cec9a9ap-2,0x1.0931a909fea5ep-43),(0x1.af12932478p-2,-0x1.e53bb31eed7a9p-44),
    (0x1.b44f77bcc8p-2,0x1.ec5197ddb55d3p-43),(0x1.b98589693p-2,0x1.0fb598fb14f89p-42),
    (0x1.beb4d9da7p-2,0x1.b7bf7861d37acp-42),(0x1.c3dd7a7cd8p-2,0x1.6a6b9d9e0a5bdp-41),
    (0x1.c8ff7c79a8p-2,0x1.a21ac25d81ef3p-42),(0x1.ce1af0b86p-2,-0x1.8290905a86aa6p-43),
    (0x1.d32fe7e01p-2,-0x1.42a9e21373414p-42),(0x1.d83e7258ap-2,0x1.79f2828add176p-41),
    (0x1.dd46a04c2p-2,-0x1.dafa08cecadb1p-41),(0x1.e24881a7c8p-2,-0x1.3d9e34270ba6bp-42),
    (0x1.e744261d68p-2,0x1.e1f8df68dbcf3p-44),(0x1.ec399d2468p-2,0x1.9802eb9dca7e7p-43),
    (0x1.f128f5fafp-2,0x1.bb2cd720ec44cp-44),(0x1.f6123fa7p-2,0x1.45630a2b61e5bp-41),
    (0x1.faf588f79p-2,-0x1.9c24ca098362bp-43),(0x1.ffd2e0858p-2,-0x1.6cf54d05f9367p-43),
    (0x1.02552a5a5cp-1,0x1.0fec69c695d7fp-41),(0x1.04bdf9da94p-1,-0x1.92d9a033eff75p-41),
    (0x1.0723e5c1ccp-1,0x1.f404e57963891p-41),(0x1.0986f4f574p-1,-0x1.5be8dc04ad601p-42),
    (0x1.0be72e4254p-1,-0x1.57d49676844ccp-41),(0x1.0e44985d1cp-1,0x1.917edd5cbbd2dp-42),
    (0x1.109f39e2d4p-1,0x1.92dfbc7d93617p-42),(0x1.12f719594p-1,-0x1.043acfedce638p-41),
    (0x1.154c3d2f4cp-1,0x1.5e9a98f33a396p-41),(0x1.179eabbd88p-1,0x1.9a0bfc60e6fap-41),
    (0x1.19ee6b467cp-1,0x1.2dd98b97baefp-42),(0x1.1c3b81f714p-1,-0x1.eda1b58389902p-44),
    (0x1.1e85f5e704p-1,0x1.a07bd8b34be7cp-46),(0x1.20cdcd192cp-1,-0x1.4926cafc2f08ap-41),
    (0x1.23130d7becp-1,-0x1.7afa4392f1ba7p-46),(0x1.2555bce99p-1,-0x1.06987f78a4a5ep-42),
    (0x1.2795e1289cp-1,-0x1.dca290f81848dp-42),(0x1.29d37fec2cp-1,-0x1.eea6f465268b4p-42),
    (0x1.2c0e9ed448p-1,0x1.d1772f5386374p-42),(0x1.2e47436e4p-1,0x1.34202a10c3491p-44),
    (0x1.307d7334fp-1,0x1.0be1fb590a1f5p-41),(0x1.32b133912p-1,0x1.d71320556b67bp-41),
    (0x1.34e289d9dp-1,-0x1.e2ce9146d277ap-41),(0x1.37117b5474p-1,0x1.ed71774092113p-43),
    (0x1.393e0d3564p-1,-0x1.5e6563bbd9fc9p-41),(0x1.3b6844ap-1,-0x1.eea838909f3d3p-44),
    (0x1.3d9026a714p-1,0x1.6faa404263d0bp-41),(0x1.3fb5b84d18p-1,-0x1.0bda4b162afa3p-41),
    (0x1.41d8fe8468p-1,-0x1.aa33736867a17p-42),(0x1.43f9fe2f9cp-1,0x1.ccef4e4f736c2p-42),
    (0x1.4618bc21c4p-1,0x1.ec27d0b7b37b3p-41),(0x1.48353d1ea8p-1,0x1.1bee7abd1766p-42),
    (0x1.4a4f85db04p-1,-0x1.44fdd840b8591p-45),(0x1.4c679afcdp-1,-0x1.1c64e971322cep-41),
    (0x1.4e7d811b74p-1,0x1.bb09cb0985646p-41),(0x1.50913cc018p-1,-0x1.794b434c5a4f5p-41),
    (0x1.52a2d265bcp-1,0x1.6abb9df22bc57p-43),(0x1.54b2467998p-1,0x1.497a915428b44p-41),
    (0x1.56bf9d5b4p-1,-0x1.8cd7dc73bd194p-42),(0x1.58cadb5cd8p-1,-0x1.9db3db43689b4p-43),
    (0x1.5ad404c358p-1,0x1.f2cfb29aaa5fp-41),(0x1.5cdb1dc6cp-1,0x1.7648cf6e3c5d7p-41),
    (0x1.5ee02a924p-1,0x1.67570d6095fd2p-41),(0x1.60e32f4478p-1,0x1.1b194f912b417p-42),
    (0x1.62e42fefa4p-1,-0x1.8432a1b0e2634p-43)]


# Float32 lookup table
# to generate values:
#   N=16
#   sN = 2f0^N
#   isN = 1f0/sN
#   s7 = 2.0^7
#   is7 = 1.0/s7
#   for j=0:128
#     j % 4 == 0 && print("\n    ")
#     @printf "%a," Float64(Base.log(big(1.0+j*is7)))
#   end

const t_log_Float32 = [0x0p+0,0x1.fe02a6b106789p-8,0x1.fc0a8b0fc03e4p-7,0x1.7b91b07d5b11bp-6,
    0x1.f829b0e7833p-6,0x1.39e87b9febd6p-5,0x1.77458f632dcfcp-5,0x1.b42dd711971bfp-5,
    0x1.f0a30c01162a6p-5,0x1.16536eea37ae1p-4,0x1.341d7961bd1d1p-4,0x1.51b073f06183fp-4,
    0x1.6f0d28ae56b4cp-4,0x1.8c345d6319b21p-4,0x1.a926d3a4ad563p-4,0x1.c5e548f5bc743p-4,
    0x1.e27076e2af2e6p-4,0x1.fec9131dbeabbp-4,0x1.0d77e7cd08e59p-3,0x1.1b72ad52f67ap-3,
    0x1.29552f81ff523p-3,0x1.371fc201e8f74p-3,0x1.44d2b6ccb7d1ep-3,0x1.526e5e3a1b438p-3,
    0x1.5ff3070a793d4p-3,0x1.6d60fe719d21dp-3,0x1.7ab890210d909p-3,0x1.87fa06520c911p-3,
    0x1.9525a9cf456b4p-3,0x1.a23bc1fe2b563p-3,0x1.af3c94e80bff3p-3,0x1.bc286742d8cd6p-3,
    0x1.c8ff7c79a9a22p-3,0x1.d5c216b4fbb91p-3,0x1.e27076e2af2e6p-3,0x1.ef0adcbdc5936p-3,
    0x1.fb9186d5e3e2bp-3,0x1.0402594b4d041p-2,0x1.0a324e27390e3p-2,0x1.1058bf9ae4ad5p-2,
    0x1.1675cababa60ep-2,0x1.1c898c16999fbp-2,0x1.22941fbcf7966p-2,0x1.2895a13de86a3p-2,
    0x1.2e8e2bae11d31p-2,0x1.347dd9a987d55p-2,0x1.3a64c556945eap-2,0x1.404308686a7e4p-2,
    0x1.4618bc21c5ec2p-2,0x1.4be5f957778a1p-2,0x1.51aad872df82dp-2,0x1.5767717455a6cp-2,
    0x1.5d1bdbf5809cap-2,0x1.62c82f2b9c795p-2,0x1.686c81e9b14afp-2,0x1.6e08eaa2ba1e4p-2,
    0x1.739d7f6bbd007p-2,0x1.792a55fdd47a2p-2,0x1.7eaf83b82afc3p-2,0x1.842d1da1e8b17p-2,
    0x1.89a3386c1425bp-2,0x1.8f11e873662c7p-2,0x1.947941c2116fbp-2,0x1.99d958117e08bp-2,
    0x1.9f323ecbf984cp-2,0x1.a484090e5bb0ap-2,0x1.a9cec9a9a084ap-2,0x1.af1293247786bp-2,
    0x1.b44f77bcc8f63p-2,0x1.b9858969310fbp-2,0x1.beb4d9da71b7cp-2,0x1.c3dd7a7cdad4dp-2,
    0x1.c8ff7c79a9a22p-2,0x1.ce1af0b85f3ebp-2,0x1.d32fe7e00ebd5p-2,0x1.d83e7258a2f3ep-2,
    0x1.dd46a04c1c4a1p-2,0x1.e24881a7c6c26p-2,0x1.e744261d68788p-2,0x1.ec399d2468ccp-2,
    0x1.f128f5faf06edp-2,0x1.f6123fa7028acp-2,0x1.faf588f78f31fp-2,0x1.ffd2e0857f498p-2,
    0x1.02552a5a5d0ffp-1,0x1.04bdf9da926d2p-1,0x1.0723e5c1cdf4p-1,0x1.0986f4f573521p-1,
    0x1.0be72e4252a83p-1,0x1.0e44985d1cc8cp-1,0x1.109f39e2d4c97p-1,0x1.12f719593efbcp-1,
    0x1.154c3d2f4d5eap-1,0x1.179eabbd899a1p-1,0x1.19ee6b467c96fp-1,0x1.1c3b81f713c25p-1,
    0x1.1e85f5e7040dp-1,0x1.20cdcd192ab6ep-1,0x1.23130d7bebf43p-1,0x1.2555bce98f7cbp-1,
    0x1.2795e1289b11bp-1,0x1.29d37fec2b08bp-1,0x1.2c0e9ed448e8cp-1,0x1.2e47436e40268p-1,
    0x1.307d7334f10bep-1,0x1.32b1339121d71p-1,0x1.34e289d9ce1d3p-1,0x1.37117b54747b6p-1,
    0x1.393e0d3562a1ap-1,0x1.3b68449fffc23p-1,0x1.3d9026a7156fbp-1,0x1.3fb5b84d16f42p-1,
    0x1.41d8fe84672aep-1,0x1.43f9fe2f9ce67p-1,0x1.4618bc21c5ec2p-1,0x1.48353d1ea88dfp-1,
    0x1.4a4f85db03ebbp-1,0x1.4c679afccee3ap-1,0x1.4e7d811b75bb1p-1,0x1.50913cc01686bp-1,
    0x1.52a2d265bc5abp-1,0x1.54b2467999498p-1,0x1.56bf9d5b3f399p-1,0x1.58cadb5cd7989p-1,
    0x1.5ad404c359f2dp-1,0x1.5cdb1dc6c1765p-1,0x1.5ee02a9241675p-1,0x1.60e32f44788d9p-1,
    0x1.62e42fefa39efp-1]

# determine if hardware FMA is available
# should probably check with LLVM, see #9855.
const FMA_NATIVE = muladd(nextfloat(1.0),nextfloat(1.0),-nextfloat(1.0,2)) == -4.930380657631324e-32

# truncate lower order bits (up to 26)
# ideally, this should be able to use ANDPD instructions, see #9868.
@inline function truncbits(x::Float64)
    reinterpret(Float64, reinterpret(UInt64,x) & 0xffff_ffff_f800_0000)
end


# Procedure 1
@inline function log_proc1(y::Float64,mf::Float64,F::Float64,f::Float64,jp::Int)
    ## Steps 1 and 2
    @inbounds hi,lo = t_log_Float64[jp]
    l_hi = mf* 0x1.62e42fefa4p-1 + hi
    l_lo = mf*-0x1.8432a1b0e2634p-43 + lo

    ## Step 3
    # @inbounds u = f*c_invF[jp]
    # u = f/F
    # q = u*u*@horner(u,
    #                 -0x1.0_0000_0000_0001p-1,
    #                 +0x1.5_5555_5550_9ba5p-2,
    #                 -0x1.f_ffff_ffeb_6526p-3,
    #                 +0x1.9_99b4_dfed_6fe4p-3,
    #                 -0x1.5_5576_6647_2e04p-3)

    ## Step 3' (alternative)
    u = (2.0f)/(y+F)
    v = u*u
    q = u*v*@horner(v,
                    0x1.5_5555_5555_0286p-4,
                    0x1.9_99a0_bc71_2416p-7)

    ## Step 4
    l_hi + (u + (q + l_lo))
end

# Procedure 2
@inline function log_proc2(f::Float64)
    ## Step 1
    g = 1.0/(2.0+f)
    u = 2.0*f*g
    v = u*u

    ## Step 2
    q = u*v*@horner(v,
                    0x1.5_5555_5555_54e6p-4,
                    0x1.9_9999_99ba_c6d4p-7,
                    0x1.2_4923_07f1_519fp-9,
                    0x1.c_8034_c85d_fff0p-12)

    ## Step 3
    # based on:
    #   2(f-u) = 2(f(2+f)-2f)/(2+f) = 2f^2/(2+f) = fu
    #   2(f-u1-u2) - f*(u1+u2) = 0
    #   2(f-u1) - f*u1 = (2+f)u2
    #   u2 = (2(f-u1) - f*u1)/(2+f)
    if FMA_NATIVE
        return u + fma(fma(-u,f,2(f-u)), g, q)
    else
        u1 = truncbits(u) # round to 24 bits
        f1 = truncbits(f)
        f2 = f-f1
        u2 = ((2.0*(f-u1)-u1*f1)-u1*f2)*g
        ## Step 4
        return u1 + (u2 + q)
    end
end


@inline function log_proc1(y::Float32,mf::Float32,F::Float32,f::Float32,jp::Int)
    ## Steps 1 and 2
    @inbounds hi = t_log_Float32[jp]
    l = mf*0.6931471805599453 + hi

    ## Step 3
    # @inbounds u = f*c_invF[jp]
    # q = u*u*@horner(u,
    #                 Float32(-0x1.00006p-1),
    #                 Float32(0x1.55546cp-2))

    ## Step 3' (alternative)
    u = (2f0f)/(y+F)
    v = u*u
    q = u*v*Float32(0x1.555584p-4)

    ## Step 4
    Float32(l + (u + q))
end

@inline function log_proc2(f::Float32)
    ## Step 1
    # compute in higher precision
    u64 = Float64(2f0*f)/(2.0+f)
    u = Float32(u64)
    v = u*u

    ## Step 2
    q = u*v*@horner(v,
                    Float32(0x1.555552p-4),
                    Float32(0x1.9a012ap-7))

    ## Step 3: not required

    ## Step 4
    Float32(u64 + q)
end



function log(x::Float64)
    if x > 0.0
        x == Inf && return x

        # Step 2
        if 0x1.e_0fab_fbc7_02a3p-1 < x < 0x1.1_082b_577d_34eep0
            f = x-1.0
            return log_proc2(f)
        end

        # Step 3
        xu = reinterpret(UInt64,x)
        m = Int(xu >> 52) & 0x07ff
        if m == 0 # x is subnormal
            x *= 0x1p54 # normalise significand
            xu = reinterpret(UInt64,x)
            m = Int(xu >> 52) & 0x07ff - 54
        end
        m -= 1023
        y = reinterpret(Float64,(xu & 0x000f_ffff_ffff_ffff) | 0x3ff0_0000_0000_0000)

        mf = Float64(m)
        F = (y + 0x1p45) - 0x1p45 # 0x1p-7*round(0x1p7*y)
        f = y-F
        jp = unsafe_trunc(Int,0x1p7*F)-127

        return log_proc1(y,mf,F,f,jp)
    elseif x == 0.0
        -Inf
    elseif isnan(x)
        NaN
    else
        throw(DomainError())
    end
end

function log(x::Float32)
    if x > 0f0
        x == Inf32 && return x

        # Step 2
        if Float32(0x1.e0fabep-1) < x < Float32(0x1.1082b6p+0)
            f = x-1f0
            return log_proc2(f)
        end

        # Step 3
        xu = reinterpret(UInt32,x)
        m = Int(xu >> 23) & 0x00ff
        if m == 0 # x is subnormal
            x *= Float32(0x1p25) # normalise significand
            xu = reinterpret(UInt32,x)
            m = Int(xu >> 23) & 0x00ff - 25
        end
        m -= 127
        y = reinterpret(Float32,(xu & 0x007f_ffff) | 0x3f80_0000)

        mf = Float32(m)
        F = (y + Float32(0x1p16)) - Float32(0x1p16) # 0x1p-7*round(0x1p7*y)
        f = y-F
        jp = unsafe_trunc(Int,Float32(0x1p7)*F)-127

        log_proc1(y,mf,F,f,jp)
    elseif x == 0f0
        -Inf32
    elseif isnan(x)
        NaN32
    else
        throw(DomainError())
    end
end


function log1p(x::Float64)
    if x > -1.0
        x == Inf && return x
        if -0x1p-53 < x < 0x1p-53
            return x

        # Step 2
        elseif -0x1.f_0540_438f_d5c4p-5 < x < 0x1.0_82b5_77d3_4ed8p-4
            return log_proc2(x)
        end

        # Step 3
        z = 1.0 + x
        zu = reinterpret(UInt64,z)
        s = reinterpret(Float64,0x7fe0_0000_0000_0000 - (zu & 0xfff0_0000_0000_0000)) # 2^-m
        m = Int(zu >> 52) & 0x07ff - 1023 # z cannot be subnormal
        c = m > 0 ? 1.0-(z-x) : x-(z-1.0) # 1+x = z+c exactly
        y = reinterpret(Float64,(zu & 0x000f_ffff_ffff_ffff) | 0x3ff0_0000_0000_0000)

        mf = Float64(m)
        F = (y + 0x1p45) - 0x1p45 # 0x1p-7*round(0x1p7*y)
        f = (y - F) + c*s #2^m(F+f) = 1+x = z+c
        jp = unsafe_trunc(Int,0x1p7*F)-127

        log_proc1(y,mf,F,f,jp)
    elseif x == -1.0
        -Inf
    elseif isnan(x)
        NaN
    else
        throw(DomainError())
    end
end

function log1p(x::Float32)
    if x > -1f0
        x == Inf32 && return x
        if -0x1p-24 < x < 0x1p-24
            return x # Inexact

        # Step 2
        elseif Float32(-0x1.f05406p-5) < x < Float32(0x1.082b58p-4)
            return log_proc2(x)
        end

        # Step 3
        z = 1f0 + x
        zu = reinterpret(UInt32,z)
        s = reinterpret(Float32,0x7f000000 - (zu & 0xff80_0000)) # 2^-m
        m = Int(zu >> 23) & 0x00ff - 127 # z cannot be subnormal
        c = m > 0 ? 1f0-(z-x) : x-(z-1f0) # 1+x = z+c
        y = reinterpret(Float32,(zu & 0x007f_ffff) | 0x3f80_0000)

        mf = Float32(m)
        F = (y + Float32(0x1p16)) - Float32(0x1p16) # 0x1p-7*round(0x1p7*y)
        f = (y - F) + s*c #2^m(F+f) = 1+x = z+c
        jp = unsafe_trunc(Int,Float32(0x1p7)*F)-127

        log_proc1(y,mf,F,f,jp)
    elseif x == -1f0
        -Inf32
    elseif isnan(x)
        NaN32
    else
        throw(DomainError())
    end
end

for f in (:log,:log1p)
    @eval begin
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg Number $f
    end
end
