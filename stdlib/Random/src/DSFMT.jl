# This file is a part of Julia. License is MIT: https://julialang.org/license

module DSFMT

import Base: copy, copy!, ==, hash
using Base.GMP.MPZ
Base.@include_stdlib_jll("dSFMT_jll")
using .dSFMT_jll


export DSFMT_state, dsfmt_get_min_array_size, dsfmt_get_idstring,
       dsfmt_init_gen_rand, dsfmt_init_by_array, dsfmt_gv_init_by_array,
       dsfmt_fill_array_close_open!, dsfmt_fill_array_close1_open2!,
       win32_SystemFunction036!

"Mersenne Exponent"
const MEXP = 19937

"DSFMT internal state array size of N 128-bit integers."
const N = floor(Int, ((MEXP - 128) / 104 + 1))

"""
Julia DSFMT state representation size counted in 32-bit integers.

Size: (DSFMT state array of Int128 + 1)*4 + Int32 index + Int32 padding
"""
const JN32 = (N+1)*4+1+1


struct DSFMT_state
    val::Vector{Int32}

    function DSFMT_state(val::Vector{Int32} = zeros(Int32, JN32))
        length(val) == JN32 ||
            throw(DomainError(length(val), "Expected length: $JN32."))
        new(val)
    end
end

copy!(dst::DSFMT_state, src::DSFMT_state) = (copyto!(dst.val, src.val); dst)
copy(src::DSFMT_state) = DSFMT_state(copy(src.val))

==(s1::DSFMT_state, s2::DSFMT_state) = s1.val == s2.val

hash(s::DSFMT_state, h::UInt) = hash(s.val, h)


## wrapper functions

function dsfmt_get_idstring()
    idstring = ccall((:dsfmt_get_idstring,libdSFMT),
                     Ptr{UInt8},
                     ())
    return unsafe_string(idstring)
end

function dsfmt_get_min_array_size()
    min_array_size = ccall((:dsfmt_get_min_array_size,libdSFMT),
                           Int32,
                           ())
end

function dsfmt_init_gen_rand(s::DSFMT_state, seed::UInt32)
    ccall((:dsfmt_init_gen_rand,libdSFMT),
          Cvoid,
          (Ptr{Cvoid}, UInt32,),
          s.val, seed)
end

function dsfmt_init_by_array(s::DSFMT_state, seed::Vector{UInt32})
    ccall((:dsfmt_init_by_array,libdSFMT),
          Cvoid,
          (Ptr{Cvoid}, Ptr{UInt32}, Int32),
          s.val, seed, length(seed))
end

function dsfmt_gv_init_by_array(seed::Vector{UInt32})
    ccall((:dsfmt_gv_init_by_array,libdSFMT),
          Cvoid,
          (Ptr{UInt32}, Int32),
          seed, length(seed))
end

function dsfmt_fill_array_close1_open2!(s::DSFMT_state, A::Ptr{Float64}, n::Int)
    @assert Csize_t(A) % 16 == 0 # the underlying C array must be 16-byte aligned
    @assert dsfmt_get_min_array_size() <= n && iseven(n)
    ccall((:dsfmt_fill_array_close1_open2,libdSFMT),
          Cvoid,
          (Ptr{Cvoid}, Ptr{Float64}, Int),
          s.val, A, n)
end

function dsfmt_fill_array_close_open!(s::DSFMT_state, A::Ptr{Float64}, n::Int)
    @assert Csize_t(A) % 16 == 0 # the underlying C array must be 16-byte aligned
    @assert dsfmt_get_min_array_size() <= n && iseven(n)
    ccall((:dsfmt_fill_array_close_open,libdSFMT),
          Cvoid,
          (Ptr{Cvoid}, Ptr{Float64}, Int),
          s.val, A, n)
end

# dSFMT jump poly calculation

"Represents a polynomial in GF(2)[X]"
struct GF2X
    z::BigInt
end

GF2X(s::AbstractString) = GF2X(parse(BigInt, reverse(s), base = 16))
Base.string(f::GF2X) = reverse(string(f.z, base = 16))
Base.:(==)(f::GF2X, g::GF2X) = f.z == g.z
Base.copy(f::GF2X) = GF2X(MPZ.set(f.z))

degree(f::GF2X) = Base.ndigits0z(f.z, 2) - 1
coeff(f::GF2X, i) = MPZ.tstbit(f.z, i)
setcoeff!(f::GF2X, i) = (MPZ.setbit!(f.z, i); f)

mulx!(f::GF2X, c::Int=1) = (MPZ.mul_2exp!(f.z, c), f)
xor!(f::GF2X, g::GF2X) = (MPZ.xor!(f.z, g.z); f)

# compute f*X mod m
function mulxmod!(f::GF2X, m::GF2X, deg=degree(m))::GF2X
    # precondition: degree(f) < degree(m)
    mulx!(f)
    degree(f) == deg && xor!(f, m)
    f
end

# cache for X^(2i) mod m
const _squares = Dict{GF2X, Vector{GF2X}}()

# compute f^2 mod m
function sqrmod!(f::GF2X, m::GF2X)::GF2X
    d = degree(m)-1
    0 <= degree(f) <= d || throw(DomainError("f must satisfy 0 <= degree(f) <= degree(m)-1"))
    sqrs = get!(_squares, m) do
        x2i = GF2X(1)
        GF2X[copy(mulxmod!(mulxmod!(x2i, m, d+1), m, d+1)) for i=1:d]
    end
    foldl(filter(i->coeff(f, i), 0:degree(f)); init=GF2X(0)) do g, i
        i <= d÷2 ? # optimization for "simple" squares
            setcoeff!(g, 2i) :
            xor!(g, sqrs[i])
    end
end

# compute X^e mod m
function powxmod(e::BigInt, m::GF2X)::GF2X
    e < 0 && throw(DomainError("e must be >= 0"))
    foldl(Base.ndigits0z(e, 2)-1:-1:0; init=GF2X(1)) do f, i
        MPZ.tstbit(e, i) ?
            mulxmod!(sqrmod!(f, m), m) :
            sqrmod!(f, m)
    end
end

"Cached jump polynomials for `MersenneTwister`."
const JumpPolys = Dict{BigInt,GF2X}()

const CharPoly_ref = Ref{GF2X}()
# Ref because it can not be initialized at load time
function CharPoly()
    if !isassigned(CharPoly_ref)
        CharPoly_ref[] = GF2X(Poly19937)
    end
    return CharPoly_ref[]
end

"""
    calc_jump(steps::Integer)

Compute the dSFMT jump polynomial for `steps` steps (by default for
the Mersenne-Twister with exponent 19937). Note that `steps` should be
less than the period (e.g. ``steps ≪ 2^19937-1``).
"""
function calc_jump(steps::Integer,
                   charpoly::GF2X=CharPoly())::GF2X
    steps < 0 && throw(DomainError("jump steps must be >= 0 (got $steps)"))
    if isempty(JumpPolys)
        JumpPolys[big(10)^20] = GF2X(JPOLY1e20)
    end
    get!(JumpPolys, steps) do
        powxmod(big(steps), charpoly)
    end
end


## dSFMT jump

function dsfmt_jump(s::DSFMT_state, jp::GF2X)
    degree(jp) < degree(CharPoly()) || throw(DomainError("degree of jump polynomial must be < $(degree(CharPoly())), got $(degree(jp))"))

    val = s.val
    nval = length(val)
    index = val[nval - 1]
    work = zeros(Int32, JN32)
    rwork = reinterpret(UInt64, work)
    dsfmt = Vector{UInt64}(undef, nval >> 1)
    ccall(:memcpy, Ptr{Cvoid}, (Ptr{UInt64}, Ptr{Int32}, Csize_t),
          dsfmt, val, (nval - 1) * sizeof(Int32))
    dsfmt[end] = UInt64(N*2)

    for i in 0:degree(jp)
        coeff(jp, i) && dsfmt_jump_add!(rwork, dsfmt)
        dsfmt_jump_next_state!(dsfmt)
    end

    rwork[end] = index
    return DSFMT_state(work)
end

function dsfmt_jump_add!(dest::AbstractVector{UInt64}, src::Vector{UInt64})
    dp = dest[end] >> 1
    sp = src[end] >> 1
    diff = ((sp - dp + N) % N)
    i = 1
    while i <= N-diff
        j = i*2-1
        p = j + diff*2
        dest[j]   ⊻= src[p]
        dest[j+1] ⊻= src[p+1]
        i += 1
    end
    while i <= N
        j = i*2-1
        p = j + (diff - N)*2
        dest[j]   ⊻= src[p]
        dest[j+1] ⊻= src[p+1]
        i += 1
    end
    dest[N*2+1] ⊻= src[N*2+1]
    dest[N*2+2] ⊻= src[N*2+2]
    return dest
end

function dsfmt_jump_next_state!(mts::Vector{UInt64})
    POS1 = 117
    SL1  = 19
    SR   = 12
    MSK1 = 0x000ffafffffffb3f
    MSK2 = 0x000ffdfffc90fffd

    idx = (mts[end] >> 1) % N

    a = idx*2+1
    b = ((idx + POS1) % N)*2+1
    u = N*2+1

    t0 = mts[a]
    t1 = mts[a+1]
    L0 = mts[u]
    L1 = mts[u+1]
    mts[u]   = xor(t0 << SL1, L1 >> 32, L1 << 32, mts[b])
    mts[u+1] = xor(t1 << SL1, L0 >> 32, L0 << 32, mts[b+1])
    mts[a]   = xor(mts[u]   >> SR, mts[u]   & MSK1, t0)
    mts[a+1] = xor(mts[u+1] >> SR, mts[u+1] & MSK2, t1)

    mts[end] = (mts[end] + 2) % (N*2)
    return mts
end

"Jump polynomial for 10^20 steps for dSFMT with exponent 19937"
const JPOLY1e20 = "e172e20c5d2de26b567c0cace9e7c6cc4407bd5ffcd22ca59d37b73d54fdbd937cd3abc6f502e8c186dbd4f1a06b9e2b894f31be77424f94dddfd5a45888a84ca66eeeb242eefe6764ed859dafccae7a6a635b3a63fe9dfbbd5f2d3f2610d39388f53060e84edae75be4f4f2272c0f1f26d1231836ad040ab091550f8a3a5423fb3ab83e068fe2684057f15691c4dc757a3aee4bca8595bf1ad03500d9620a5dbe3b2d64380694895d2f379ca928238293ea267ce14236d5be816a61f018fe4f6bc3c9865f5d4d4186e320ab653d1f3c035ae83e2ad725648a67d3480331e763a1dcdfb5711b56796170b124f5febd723a664a2deefbfa9999d922a108b0e683582ae8d3baacb5bb56683405ea9e6e0d71ddb24b2229c72bb9d07061f2d1fa097ade823b607a2029d6e121ae09d93de01a154199e8e6a6e77c970bda72ba8079b2b3a15dd494a3188b1d94a25ae108a8a5bd0b050e6ce64a365a21420e07fdeebecae02eb68a4304b59283055d22c27d680ea35952834d828c9b9b9dd1a886b4f7fe82fe8f2a962e1e5390e563dc281c799aee2a441b7a813facb6ff5e94c059710dcfe7e6b1635e21ae0dc878dd5f7cc0e1101a74452495a67d23a2672c939f32c81d4a2611073990e92a084cc3a62fd42ee566f29d963a9cc5100ccd0a200f49ce0a74fa891efa1b974d342b7fedf9269e40d9b34e3c59c3d37201aecd5a04f4ae3d0c9a68c7ab78c662390e4cf36cb63ea3539c442efd0bf4aace4b8c8bde93c3d84b4d6290adfae1c5e3fcd457b6f3159e501f17b72ff6bc13d6bf61fbdafabefd16ac1dae0bca667e4e16a2b800732f1d0a9274c8a4c6cccd2db62fc275dc308c31c11cd6fda78de2f81f0e542b76b42b2cc09ed8f965d94c714c9918064f53af5379cfbbc31edf9cbce694f63a75f122048de6e57b094908f749661456813a908027f5d8397ab7962bf75ac779a3e1b7ae3fbc93397a67b486bb849befff1de6162ef2819715a88f41881e366ace692a900796a2806393898dd1750ac2b4ca3d34ca48942322fb6375f0c9a00c9701048ee8d7d7a17e11739177a7ad5027556e85835daf8594d84a97fe6621c0fce1495ae6ab8676cdc992d247acf5a4e5ec8c4755fde28117228d2c3ecf89edb91e93d949e2174924572265e36d176d082ed1be884e51d885ba3cda175c51edcee5042eaf519d292aa05aa4185b03858d710a9d0880b3d4e5111f858a52fe352cbe0a24f06a3d977ae2eb85e2a03a68131d0ab91dac4941067cf90ecd0fce156bcd40b8968cd4aa11e0b4353b14508d79d13ac00af4a4d452496b7f2393699889aa1e508427dbf0be3db91d955feb51e559af57640c6b3f9d5f95609852c28f9462a9869dd93acbdb1aafb2381ebb886a0b3fcec278f8bb0f62c23e157e49b89245b0881268ce594acbddd3605b9eaa77c9ff513e0dbad514914136d96fe2843fe2b4e886a0b718a9b8d1132132110618d0d3595da284cd2a4c9d09386199e4f4d7723983d3a374b51cf20dac5cabb4ff7e7197c2ebd9318463409baa583d6a6115c1b768282ff37b0fe152c97671e400d5ccba7d6875df0bf95c5d91257fedb124de393f31908d0e36251326aa29dd5be86291c80b4bf78f419ec151eeaeff643a58b48ab35ad2cd2c0b77b1965966ef3db6b6373cb2c4b590cef2f16f4d6f62f13a6cbf1a481565b5935edd4e76f7b6a8fd0d74bc336b40a803aec38125c006c877dfdcdb9ba2b7aecab5cafe6076e024c73e3567adf97f607a71d180402c22a20a8388f517484cc4198f97c2fe4f3407e0dc577e61f0f71354aa601cf4e3e42e1edd8722d50f5af3441f68caa568cc1c3a19956c1233f265bb47236afab24ee42b27b0042b90693d77c1923147360ae6503f6ba6abbc9dd52a7b4c36a3b6b55f6a80cfa7f101dd9f1bfc7d7eaf09a5d636b510228f245bfb37b4625025d2c911435cdf6f878113753e0804ab8ecab870ad733b9728d7636b17578b41239393e7de47cbce871137d2b61729dda67b2b84cd3363aad64c5dd5bd172f1f091305b1ff78982abe7dab1588036d097cf497e300e6c78a926048febd1b9462c07f5868928357b74297c87f503056b89f786d22a538b6702e290bca04639a0f1d0939b67f409e5e58e472a6a07fa543e2531c2567ec73c41f6769b6ba94c5aa0a030d006f5b6b1c5fb218b86a8f63a48bc867466f20f699859e87956f48a182d26ed451861dd21201ecc7239037ada67319bdf0849c387c73a110af798b4c5f9018bc97993e060ea2a2937fa2eb095d65ec07009fc407a350f1d6fb3c98a0a5f204be985b0cb6962f0eb7844a179c4598a92ea32d2d706c800034d2e960ded5b476d77073316b933fb3e6ba2f4f24a3b73a1e4d8ed1491d757ecf56fd72465dac0000736744d28d29073091587c8bccad302f7054e8a32bb8724974d9f3e449fc70b2a41f0008b548f717ac0a2c3a6580bfb50774933a578ad6acdcb89940bb406ea540893f097d8a88d1609ed605f25499de939083a0c8a7c6db462df5dfa06c298dd233e249433a54267d5cdc22e5524705b7d6b16b96bb2cb83e00cef62e21d91528a74cf95bfd1d391590c93f4058e9bb02656fd087a5b63d738d1c3b5cf533fd59c81cf9136bfcd3e955c19daf9906ef175791fde6a1d98155d7881e241c3522551cf9fcae42e1e46929ea39fd00943446823f9755085ccc8456a3090b73a3031a201d9c704a4ad4868dd9b6d06205560013973f60d637de2f18354bf4523d9d81dc2a7e78cd42c586364bbe0ed86fde0f081f801c1a4abb830839b7796d9a01f141bec8bd93144104c6dc59170162c0a5a639eb63a0a164970de50eb2e04f027394b26ed48d341f7851994df79d7cd663672a556f25e5e16a3adbe1003d631de938fabfed234df12b5ff3027f4a2da823834cb098e0f977a4eb9614579d5c7a1d400a1a933a657aef8ea1a66743d73b0cf37a7d64e9a63e4c7b09945f0db750b311b39783fb5ea216616751967d480a630d3da7c89d1c7beae20369137e96734a4cfedca56a7887f076fe4fe97534ad3e4f74d1a81750581a5ea214b440c7f30331ab86c257534c71175d1e731303a48b01c589fda4fb0d4368b4dd63d91204cb6fc389b2202aa94391907bfb72902a4031f5589ed5f391c2ce92aa998c200ba3c77d8bd747b9d0a29fa85cda3949a6d2bd0c3402e68f98fd451aa27b6c2dfd170e004577cbdb25e3a1b9852e9f66a370789c47bfce722446dade1b32ceae71ee0e1d96edf7ed08a93e3690056f46c3d8e63f88e53673ee71d72cfedbeba493ee91333120e09e9ce9f9c9a7a400f814ea618b1de48f9805e092f4e20f301fbb65caa83735a2a5c89befe4bce4116dca3688e1e14c6f09a945671dedbb5c0ba526842b6cae31d8b5ff978bae928a17a75c134630dd9de988f6ad3d89a071b33775a9660a40b48ec61ad3f93ac81cb1c65d8b0bab5c214786abd13cc10a8ea2e2a370e86e2fa1a372d83c9697b5e37b281e51507685f714fdaebe49ffc93a5582e1936eaee8e4140a4b72"

"Characteristic polynomial used by dSFMT with exponent 19937"
const Poly19937 = "10455110544511444054554514541dbbb0a20820a288a00a80a82208280b73237d2ff2ab5e2fcac914510101df18802aaaa08a022820a02aaa202ae822c364b91fb08598fe4410154041f028a266586e59cf12ce316b3a56af45cb49d02c7059cb89d2d81b0d288badbb25fa4e53b280aa88a22aa82abe7fc0645b7d7a4650c1dec48f21224e3d0e6e04c062c2273ef0d8361415c724dbc8f79118d5fac429f35dc460f6007e54c3966c28808e4c9308cc46e2e0e153bd922223796d4101af867e16e6641e6831e06ebbd2beaf52b2b47076dbf3a3e36c6d14d19dbf5d4b2b44b4d3aa6e1ea102578d765f08e1cb0749220075b8aae81c6e99692a56b35ddd4cf91b1034f1398c98e2d4ac8dbed09bc73ede098514cf3ffdf45cbb59335e3ec42f5f6a5672acc4ca8daa02a2502350ac0485f8b36f27d7a1d4d4b22fc7601e22a4f7c6ba53782b335837a21a068e8fcf3fdebb28860157301519cdea328b1ef4b8d5bc512ce65ff33298c34cc98ea1558f7b6d4db594f4bcab58d9f7bcf5cc59e259216de13f77569bbcee1c8abd55de985b7129e611d156c08cafa022ad7a2206a34a61e5c4c91e75495112003ec03c182a0155d9221382d0b30f45a00d6c19740f9ecebb448de78dfc0342f14166f54afdb97d00ac1a390355ce2aa9de1b3c919d8dd188fc9451ce9c86fa560a2da9dcaa681efd21fe5b6055f8e35a833e5704936245d06e387bf9956251bf0a252628be7a3cb0edab4aaaf141e6d7a9929cc03afa76ca433016d880def9e4cf10f7274e58aa35c372b7b7fb210fe0dc1a6b8445e7774ad6001b9aa1f2a01a790ee849e7ac810e0a646e733a7121bd727a9b593e0c9f5306dff5105af5967e3cee81069100d7e91a9c266e64c9e073a6e527765308879b22ca062668f9a564da6314dcad51405f160e8259582b7c06c313c84b0acba44958c052e6be540a7688e240232bee40b990dc48ee07d560786ca34e7df1bbdd2bca38a30c548ec57e91906b8417ff0da68db9c154d8ad83b06a6fc2b2e14ca5fbe7bd50382949c9b1f6e8540d9d43b35fa76a6ded27c2f17095a4f330626c5e86e8ff88ae53e05a434356a04a1ddae43b6e2ab883719360fbece72b6090ab17414ca7006e49d183813422c808fde53a30f872254bb554653aef86855f95a9361782100de2402efd749cc8cf6a837066c1c40c0160e415d8119e53a09877f1160ef8b99b2839e9b8c09b1e461e906041344c8fc2ef0a8eda04e319da41e001e60bc64dcdfc0593dff0f4b390580e1cd5b3c16204e77df10217791f99de49fcdc9160b793fd980bff7ae0cbd570425eb439352e5032e03797461376b5fb92aa156ad64935cc201a162f10f04b6d2d20a0415ae16f299e98baa86466f6f517f05f430254884a4010ac196540b0644e3c274323d4f0206780d38175f1e41fcb65bd387be073abee61b99d43f6b9106953ae4f6906e6ac0741e26af05fa9150c4f380558668aa667e404e3784b839d631e35af015024dbfc3dce4685574cd1e58eb72c70011090a2a876b65e34cb6cf297d24cb61ccca5a9706f34ae1f66345998f850de4e48d77cdf6e00fb0d2210ec9fb53fed02a781f7dfcdb609b9f955504f450e4b7b623cb0f5ba1ea09d92cd8d14f7e827b4580855f3a7dc2e5a45817df9e26adb5934f6026f745cb0f65e71c590bd954d1abc3826379719b7c6f4a0cbe6eb22a903b98bef785bb96efe2fc96988722c91f3e59d28d8244c8bfb59ee26082d82cad937ee70f178b44082533308ca24f236d8b91ca7af5b3d865c90d61410e1ffe39be6433a12ef2932e101b4bb34befa76748e0364a96f06e7932f44297fd5f85765b662c3ade19d9a9d9479f6de20b6b753d3dcbadf63e344578b98af85b4c4c63be9d154864f5d341f210f3503a60efec38ee59069499d0049aedfaef9264a7ce9de460a01e5437254fc68dfaebaa5e0e791380806bc149aedbc1d771457770937a5d606fc5ae728993783e6c45e1f5e1b9492a32f9df46a01020792a3803af04837a3905e7cc6ae68c512cb58f4facb457476729bac1ac89a7a32dc6857edbd6624ebeffed9d4e84a2f4ded9759962635aac94ca72d039c14af6e932fc84c25de28688acab0f41ae931a0f35b9c4195483d902f95e0d3e745e08f334cf5062da9fbd18fbb9efeb275a50a8a479939aa3e376821a030f0d241a4c4f6e3298a43a7f2166db483946c5ca623087b6252e27b09805d92a834ad914b9555f9c36150bb17d7e85f6b1a10b41a5c36a1fd0fdc354ec91720b245b0abcde6b38fdd33f471aa6ba1dbb4f178c3c6d8140a1f0700e072b0f9aafe1208abc94eea750b84e74ba6781b48c398ddfd2842e5447a11767e4f467864e4447382379010669f07870339ca42ac290063b40eaba1daa63cf6cd70c5a600c018f6933dc8bec000a521344cad7320ba121dd21f4723d38a8d3ab8c1f0f6c5a6995b796af79407ae1a7ce6d78922677d9990dd2a588f8a3323d80e9a69417f5224565afa60b64604e7316229514dcb7282b4e638981a5751d114da1ac9bf31f0e2fff5213f2020f9f2f31a8fd0c614e80c1a056d4b1af3ded2f696578f56427962ca54f4a28a502a0ac59013416abd81515fb1956ccb494c05ef61cab48474b6b1609cc5a3871a5111f8bf0a76b378f0e646687829e30f5762156da66c1b1c7abc0eb84e6ff2b9f5b22d34540ab97d643e8dd2e35f6e9e4fc2c30d8af88b2caab7bd5d4a6cf967e8ef79967c1ae85bf7d410a79f4630f13fdc6507d339909b81a29d84741103371310e5b4e279758431df627553b6826fc4c98e5fb6551315b0bd811b7b0f357198210dd99ccd8fba2904114c3e0b344eba43832b3c507e8b6b586e4ab3dc7a2ec71e150c54a13eca2340328d0b3e419ab2ba862ee93fc239fb46d907628055e105318e7fa52f9a83deb0e3cb02c62b8817702ead02a315f76aa1d08860cc5214a867808e33f8e075241956f148f876f3bc66566773610c9c5935b559c0ac47d84b6bfb112f59842be58df51055cf9180264f53f7795d4c934718bc65f359e34a8d230408854685b59c3a9f4d73a229bb465d4da3165404c6786c767299f57dfa85a83492fb4f61386441c928224cd88a7f4b36f245b7aa2b5c97b545ac4db8afe9a1a87e27b57d94c2bbffdb6e88f812aa27e0908048812086c2a72289d7bf136b3a1042a44d1913d39caec24ffda22814706f080b6cbe00e9cd442ccdcb600a436c0daeacbc5482021ba8a06c1fedbb333793557d5175b9313799ff91dfa620380a9e2a10132f0818bba72072e359726e2bd1f2ec98e0face32e0f88ee2c6f7cef7c2fbceffe8d3ccdff97b7ff71d861ba8b98237ccfb00176ee02206ccc08026cee082a88a8a349a1c9016983ea10789272105032f89b3113fd9b75b35c884622ec884622ee8aee2aaa2aeae86868c868ea68ea6862e0624062ea22aa66ee66ee44ee64ce64ce64ce646464c4808081808080a0a0828282828282822222a8898888888888aaaa2aaaaaaaaaaaaa2000000000000000008"

end # module
