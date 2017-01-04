# This file is a part of Julia. License is MIT: http://julialang.org/license

module dSFMT

import Base: copy, copy!, ==

export DSFMT_state, dsfmt_get_min_array_size, dsfmt_get_idstring,
       dsfmt_init_gen_rand, dsfmt_init_by_array, dsfmt_gv_init_by_array,
       dsfmt_fill_array_close_open!, dsfmt_fill_array_close1_open2!,
       win32_SystemFunction036!

"Mersenne Exponent"
const MEXP = 19937
"DSFMT internal state array size of N 128-bit integers."
const N    = floor(Int, ((MEXP - 128) / 104 + 1))
"""Julia DSFMT state representation size counted in 32-bit integers.

Size: (DSFMT state array of Int128 + 1)*4 + Int32 index + Int32 padding
"""
const JN32 = (N+1)*4+1+1
"Jump polynomial for 10^20 steps for dSFMT with exponent 19937"
const JPOLY1e21  = "e172e20c5d2de26b567c0cace9e7c6cc4407bd5ffcd22ca59d37b73d54fdbd937cd3abc6f502e8c186dbd4f1a06b9e2b894f31be77424f94dddfd5a45888a84ca66eeeb242eefe6764ed859dafccae7a6a635b3a63fe9dfbbd5f2d3f2610d39388f53060e84edae75be4f4f2272c0f1f26d1231836ad040ab091550f8a3a5423fb3ab83e068fe2684057f15691c4dc757a3aee4bca8595bf1ad03500d9620a5dbe3b2d64380694895d2f379ca928238293ea267ce14236d5be816a61f018fe4f6bc3c9865f5d4d4186e320ab653d1f3c035ae83e2ad725648a67d3480331e763a1dcdfb5711b56796170b124f5febd723a664a2deefbfa9999d922a108b0e683582ae8d3baacb5bb56683405ea9e6e0d71ddb24b2229c72bb9d07061f2d1fa097ade823b607a2029d6e121ae09d93de01a154199e8e6a6e77c970bda72ba8079b2b3a15dd494a3188b1d94a25ae108a8a5bd0b050e6ce64a365a21420e07fdeebecae02eb68a4304b59283055d22c27d680ea35952834d828c9b9b9dd1a886b4f7fe82fe8f2a962e1e5390e563dc281c799aee2a441b7a813facb6ff5e94c059710dcfe7e6b1635e21ae0dc878dd5f7cc0e1101a74452495a67d23a2672c939f32c81d4a2611073990e92a084cc3a62fd42ee566f29d963a9cc5100ccd0a200f49ce0a74fa891efa1b974d342b7fedf9269e40d9b34e3c59c3d37201aecd5a04f4ae3d0c9a68c7ab78c662390e4cf36cb63ea3539c442efd0bf4aace4b8c8bde93c3d84b4d6290adfae1c5e3fcd457b6f3159e501f17b72ff6bc13d6bf61fbdafabefd16ac1dae0bca667e4e16a2b800732f1d0a9274c8a4c6cccd2db62fc275dc308c31c11cd6fda78de2f81f0e542b76b42b2cc09ed8f965d94c714c9918064f53af5379cfbbc31edf9cbce694f63a75f122048de6e57b094908f749661456813a908027f5d8397ab7962bf75ac779a3e1b7ae3fbc93397a67b486bb849befff1de6162ef2819715a88f41881e366ace692a900796a2806393898dd1750ac2b4ca3d34ca48942322fb6375f0c9a00c9701048ee8d7d7a17e11739177a7ad5027556e85835daf8594d84a97fe6621c0fce1495ae6ab8676cdc992d247acf5a4e5ec8c4755fde28117228d2c3ecf89edb91e93d949e2174924572265e36d176d082ed1be884e51d885ba3cda175c51edcee5042eaf519d292aa05aa4185b03858d710a9d0880b3d4e5111f858a52fe352cbe0a24f06a3d977ae2eb85e2a03a68131d0ab91dac4941067cf90ecd0fce156bcd40b8968cd4aa11e0b4353b14508d79d13ac00af4a4d452496b7f2393699889aa1e508427dbf0be3db91d955feb51e559af57640c6b3f9d5f95609852c28f9462a9869dd93acbdb1aafb2381ebb886a0b3fcec278f8bb0f62c23e157e49b89245b0881268ce594acbddd3605b9eaa77c9ff513e0dbad514914136d96fe2843fe2b4e886a0b718a9b8d1132132110618d0d3595da284cd2a4c9d09386199e4f4d7723983d3a374b51cf20dac5cabb4ff7e7197c2ebd9318463409baa583d6a6115c1b768282ff37b0fe152c97671e400d5ccba7d6875df0bf95c5d91257fedb124de393f31908d0e36251326aa29dd5be86291c80b4bf78f419ec151eeaeff643a58b48ab35ad2cd2c0b77b1965966ef3db6b6373cb2c4b590cef2f16f4d6f62f13a6cbf1a481565b5935edd4e76f7b6a8fd0d74bc336b40a803aec38125c006c877dfdcdb9ba2b7aecab5cafe6076e024c73e3567adf97f607a71d180402c22a20a8388f517484cc4198f97c2fe4f3407e0dc577e61f0f71354aa601cf4e3e42e1edd8722d50f5af3441f68caa568cc1c3a19956c1233f265bb47236afab24ee42b27b0042b90693d77c1923147360ae6503f6ba6abbc9dd52a7b4c36a3b6b55f6a80cfa7f101dd9f1bfc7d7eaf09a5d636b510228f245bfb37b4625025d2c911435cdf6f878113753e0804ab8ecab870ad733b9728d7636b17578b41239393e7de47cbce871137d2b61729dda67b2b84cd3363aad64c5dd5bd172f1f091305b1ff78982abe7dab1588036d097cf497e300e6c78a926048febd1b9462c07f5868928357b74297c87f503056b89f786d22a538b6702e290bca04639a0f1d0939b67f409e5e58e472a6a07fa543e2531c2567ec73c41f6769b6ba94c5aa0a030d006f5b6b1c5fb218b86a8f63a48bc867466f20f699859e87956f48a182d26ed451861dd21201ecc7239037ada67319bdf0849c387c73a110af798b4c5f9018bc97993e060ea2a2937fa2eb095d65ec07009fc407a350f1d6fb3c98a0a5f204be985b0cb6962f0eb7844a179c4598a92ea32d2d706c800034d2e960ded5b476d77073316b933fb3e6ba2f4f24a3b73a1e4d8ed1491d757ecf56fd72465dac0000736744d28d29073091587c8bccad302f7054e8a32bb8724974d9f3e449fc70b2a41f0008b548f717ac0a2c3a6580bfb50774933a578ad6acdcb89940bb406ea540893f097d8a88d1609ed605f25499de939083a0c8a7c6db462df5dfa06c298dd233e249433a54267d5cdc22e5524705b7d6b16b96bb2cb83e00cef62e21d91528a74cf95bfd1d391590c93f4058e9bb02656fd087a5b63d738d1c3b5cf533fd59c81cf9136bfcd3e955c19daf9906ef175791fde6a1d98155d7881e241c3522551cf9fcae42e1e46929ea39fd00943446823f9755085ccc8456a3090b73a3031a201d9c704a4ad4868dd9b6d06205560013973f60d637de2f18354bf4523d9d81dc2a7e78cd42c586364bbe0ed86fde0f081f801c1a4abb830839b7796d9a01f141bec8bd93144104c6dc59170162c0a5a639eb63a0a164970de50eb2e04f027394b26ed48d341f7851994df79d7cd663672a556f25e5e16a3adbe1003d631de938fabfed234df12b5ff3027f4a2da823834cb098e0f977a4eb9614579d5c7a1d400a1a933a657aef8ea1a66743d73b0cf37a7d64e9a63e4c7b09945f0db750b311b39783fb5ea216616751967d480a630d3da7c89d1c7beae20369137e96734a4cfedca56a7887f076fe4fe97534ad3e4f74d1a81750581a5ea214b440c7f30331ab86c257534c71175d1e731303a48b01c589fda4fb0d4368b4dd63d91204cb6fc389b2202aa94391907bfb72902a4031f5589ed5f391c2ce92aa998c200ba3c77d8bd747b9d0a29fa85cda3949a6d2bd0c3402e68f98fd451aa27b6c2dfd170e004577cbdb25e3a1b9852e9f66a370789c47bfce722446dade1b32ceae71ee0e1d96edf7ed08a93e3690056f46c3d8e63f88e53673ee71d72cfedbeba493ee91333120e09e9ce9f9c9a7a400f814ea618b1de48f9805e092f4e20f301fbb65caa83735a2a5c89befe4bce4116dca3688e1e14c6f09a945671dedbb5c0ba526842b6cae31d8b5ff978bae928a17a75c134630dd9de988f6ad3d89a071b33775a9660a40b48ec61ad3f93ac81cb1c65d8b0bab5c214786abd13cc10a8ea2e2a370e86e2fa1a372d83c9697b5e37b281e51507685f714fdaebe49ffc93a5582e1936eaee8e4140a4b72"

type DSFMT_state
    val::Vector{Int32}

    DSFMT_state(val::Vector{Int32} = zeros(Int32, JN32)) =
        new(length(val) == JN32 ? val : throw(DomainError()))
end

copy!(dst::DSFMT_state, src::DSFMT_state) = (copy!(dst.val, src.val); dst)
copy(src::DSFMT_state) = DSFMT_state(copy(src.val))

==(s1::DSFMT_state, s2::DSFMT_state) = s1.val == s2.val

function dsfmt_get_idstring()
    idstring = ccall((:dsfmt_get_idstring,:libdSFMT),
                     Ptr{UInt8},
                     ())
    return unsafe_string(idstring)
end

function dsfmt_get_min_array_size()
    min_array_size = ccall((:dsfmt_get_min_array_size,:libdSFMT),
                           Int32,
                           ())
end

const dsfmt_min_array_size = dsfmt_get_min_array_size()

function dsfmt_init_gen_rand(s::DSFMT_state, seed::UInt32)
    ccall((:dsfmt_init_gen_rand,:libdSFMT),
          Void,
          (Ptr{Void}, UInt32,),
          s.val, seed)
end

function dsfmt_init_by_array(s::DSFMT_state, seed::Vector{UInt32})
    ccall((:dsfmt_init_by_array,:libdSFMT),
          Void,
          (Ptr{Void}, Ptr{UInt32}, Int32),
          s.val, seed, length(seed))
end

function dsfmt_gv_init_by_array(seed::Vector{UInt32})
    ccall((:dsfmt_gv_init_by_array,:libdSFMT),
          Void,
          (Ptr{UInt32}, Int32),
          seed, length(seed))
end

function dsfmt_fill_array_close1_open2!(s::DSFMT_state, A::Ptr{Float64}, n::Int)
    @assert Csize_t(A) % 16 == 0 # the underlying C array must be 16-byte aligned
    @assert dsfmt_min_array_size <= n && iseven(n)
    ccall((:dsfmt_fill_array_close1_open2,:libdSFMT),
          Void,
          (Ptr{Void}, Ptr{Float64}, Int),
          s.val, A, n)
end

function dsfmt_fill_array_close_open!(s::DSFMT_state, A::Ptr{Float64}, n::Int)
    @assert Csize_t(A) % 16 == 0 # the underlying C array must be 16-byte aligned
    @assert dsfmt_min_array_size <= n && iseven(n)
    ccall((:dsfmt_fill_array_close_open,:libdSFMT),
          Void,
          (Ptr{Void}, Ptr{Float64}, Int),
          s.val, A, n)
end

# dSFMT jump
function dsfmt_jump(s::DSFMT_state, jp::AbstractString)
    index = s.val[end-1]
    work = zeros(UInt64, JN32>>1)
    dsfmt = reinterpret(UInt64, copy(s.val))
    dsfmt[end] = UInt64(N*2)

    for c in jp
        bits = parse(UInt8,c,16)
        for j in 1:4
            (bits & 0x01) != 0x00 && dsfmt_jump_add!(work, dsfmt)
            bits = bits >> 0x01
            dsfmt_jump_next_state!(dsfmt)
        end
    end

    work[end] = index
    return DSFMT_state(reinterpret(Int32, work))
end

function dsfmt_jump_add!(dest::Vector{UInt64}, src::Vector{UInt64})
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

## Windows entropy

if is_windows()
    function win32_SystemFunction036!{T}(a::Array{T})
        ccall((:SystemFunction036, :Advapi32), stdcall, UInt8, (Ptr{Void}, UInt32), a, sizeof(a))
    end
end

end # module
