using SHA
using Compat

# Define some data we will run our tests on
lorem = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
so_many_as = repmat([0x61], 1000000)
data = Any["", "test", lorem, so_many_as]

# Descriptions of the data, the SHA functions we'll run on the data, etc...
data_desc = ["the empty string", "the string \"test\"", "lorem ipsum", "one million a's"]
sha_types =Dict(sha1 => SHA.SHA1_CTX,
            sha2_224 => SHA.SHA2_224_CTX, sha2_256 => SHA.SHA2_256_CTX, sha2_384 => SHA.SHA2_384_CTX, sha2_512 => SHA.SHA2_512_CTX,
            sha3_224 => SHA.SHA3_224_CTX, sha3_256 => SHA.SHA3_256_CTX, sha3_384 => SHA.SHA3_384_CTX, sha3_512 => SHA.SHA3_512_CTX)
sha_funcs = [sha1,
            sha2_224, sha2_256, sha2_384, sha2_512,
            sha3_224, sha3_256, sha3_384, sha3_512]

answers = @compat Dict(
sha1 => [
"da39a3ee5e6b4b0d3255bfef95601890afd80709",
"a94a8fe5ccb19ba61c4c0873d391e987982fbbd3",
"19afa2a4a37462c7b940a6c4c61363d49c3a35f4",
"34aa973cd4c4daa4f61eeb2bdbad27316534016f",
],
sha2_224 => [
"d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f",
"90a3ed9e32b2aaf4c61c410eb925426119e1a9dc53d4286ade99a809",
"6a0644abcf1e2cecbec2814443dab5f24b7ad8ebb66c75667ab67959",
"20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67"
],
sha2_256 => [
"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
"9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08",
"2c7c3d5f244f1a40069a32224215e0cf9b42485c99d80f357d76f006359c7a18",
"cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
],
sha2_384 => [
"38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b",
"768412320f7b0aa5812fce428dc4706b3cae50e02a64caa16a782249bfe8efc4b7ef1ccb126255d196047dfedf17a0a9",
"63980fd0425cd2c3d8a400ee0f2671ef135db03b947ec1af21b6e28f19c16ca272036469541f4d8e336ac6d1da50580f",
"9d0e1809716474cb086e834e310a4a1ced149e9c00f248527972cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985"
],
sha2_512 => [
"cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e",
"ee26b0dd4af7e749aa1a8ee3c10ae9923f618980772e473f8819a5d4940e0db27ac185f8a0e1d5f84f88bc887fd67b143732c304cc5fa9ad8e6f57f50028a8ff",
"f41d92bc9fc1157a0d1387e67f3d0893b70f7039d3d46d8115b5079d45ad601159398c79c281681e2da09bf7d9f8c23b41d1a0a3c5b528a7f2735933a4353194",
"e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b"
],
sha3_224 => [
"6b4e03423667dbb73b6e15454f0eb1abd4597f9a1b078e3f5b5a6bc7",
"3797bf0afbbfca4a7bbba7602a2b552746876517a7f9b7ce2db0ae7b",
"ea5395370949ad8c7d2ca3e7c045ef3306fe3a3f4740de452ef87a28",
"d69335b93325192e516a912e6d19a15cb51c6ed5c15243e7a7fd653c"
],
sha3_256 => [
"a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a",
"36f028580bb02cc8272a9a020f4200e346e276ae664e45ee80745574e2f5ab80",
"8c8142d2ca964ab307ace567ddd5764f17ebb76eb8ff25543ab54c14fe2ab139",
"5c8875ae474a3634ba4fd55ec85bffd661f32aca75c6d699d0cdcb6c115891c1"
],
sha3_384 => [
"0c63a75b845e4f7d01107d852e4c2485c51a50aaaa94fc61995e71bbee983a2ac3713831264adb47fb6bd1e058d5f004",
"e516dabb23b6e30026863543282780a3ae0dccf05551cf0295178d7ff0f1b41eecb9db3ff219007c4e097260d58621bd",
"eb9fbba3eb916a4efe384b3125f5d03ceb9c5c1b94431ac30fa86c54408b92701ca5d2628cd7113aa5541177ec3ccd1d",
"eee9e24d78c1855337983451df97c8ad9eedf256c6334f8e948d252d5e0e76847aa0774ddb90a842190d2c558b4b8340"
],
sha3_512 => [
"a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26",
"9ece086e9bac491fac5c1d1046ca11d737b92a2b2ebd93f005d7b710110c0a678288166e7fbe796883a4f2e9b3ca9f484f521d0ce464345cc1aec96779149c14",
"3a4318353396a12dfd20442cfce1d8ad4d7e732e85cc56b01b4cf9057a41c8827c0a03c70812e76ace68d776759225c213b4f581aac0dba5dd43b785b1a33fe5",
"3c3a876da14034ab60627c077bb98f7e120a2a5370212dffb3385a18d4f38859ed311d0a9d5141ce9cc5c66ee689b266a8aa18ace8282a0e0db596c90b0a7b87"
]
)

function describe_hash{S<:SHA.SHA_CTX}(T::Type{S})
    if T <: SHA.SHA1_CTX
        return "SHA1"
    end

    if T <: SHA.SHA2_CTX
        return "SHA2-$(SHA.digestlen(T)*8)"
    end

    if T <: SHA.SHA3_CTX
        return "SHA3-$(SHA.digestlen(T)*8)"
    end
end

println("Loaded hash types: $(join(sort([describe_hash(t[2]) for t in sha_types]), ", ", " and "))")

# First, test processing the data in one go
nerrors = 0
for idx in 1:length(data)
    desc = data_desc[idx]
    print("Testing on $desc$(join(["." for z in 1:(34-length(desc))]))")
    nerrors_old = nerrors
    for sha_idx in 1:length(sha_funcs)
        sha_func = sha_funcs[sha_idx]

        hash = bytes2hex(sha_func(data[idx]))
        if hash != answers[sha_func][idx]
            print("\n")
            warn(
            """
            For $(describe_hash(sha_types[sha_func])) expected:
                $(answers[sha_func][idx])
            Calculated:
                $(hash)
            """)
            nerrors += 1
        else
            print(".")
        end
    end
    println("Done! [$(nerrors - nerrors_old) errors]")
end

# Do another test on the "so many a's" data where we chunk up the data into
# two chunks, (sized appropriately to AVOID overflow from one update to another)
# in order to test multiple update!() calls
print("Testing on one million a's (chunked properly)")
nerrors_old = nerrors
for sha_idx in 1:length(sha_funcs)
    ctx = sha_types[sha_funcs[sha_idx]]()
    SHA.update!(ctx, so_many_as[1:2*SHA.blocklen(typeof(ctx))])
    SHA.update!(ctx, so_many_as[2*SHA.blocklen(typeof(ctx))+1:end])
    hash = bytes2hex(SHA.digest!(ctx))
    if hash != answers[sha_funcs[sha_idx]][end]
        print("\n")
        warn(
        """
        For $(describe_hash(sha_types[sha_funcs[sha_idx]])) expected:
            $(answers[sha_funcs[sha_idx]][end-1])
        Calculated:
            $(hash)
        """)
        nerrors += 1
    else
        print(".")
    end
end
println("Done! [$(nerrors - nerrors_old) errors]")

# Do another test on the "so many a's" data where we chunk up the data into
# three chunks, (sized appropriately to CAUSE overflow from one update to another)
# in order to test multiple update!() calls as well as the overflow codepaths
print("Testing on one million a's (chunked clumsily)")
nerrors_old = nerrors
for sha_idx in 1:length(sha_funcs)
    ctx = sha_types[sha_funcs[sha_idx]]()

    # Get indices awkwardly placed for the blocklength of this hash type
    idx0 = round(Int, 0.3*SHA.blocklen(typeof(ctx)))
    idx1 = round(Int, 1.7*SHA.blocklen(typeof(ctx)))
    idx2 = round(Int, 2.6*SHA.blocklen(typeof(ctx)))

    # Feed data in according to our dastardly blocking scheme
    SHA.update!(ctx, so_many_as[0      + 1:1*idx0])
    SHA.update!(ctx, so_many_as[1*idx0 + 1:2*idx0])
    SHA.update!(ctx, so_many_as[2*idx0 + 1:3*idx0])
    SHA.update!(ctx, so_many_as[3*idx0 + 1:4*idx0])
    SHA.update!(ctx, so_many_as[4*idx0 + 1:idx1])
    SHA.update!(ctx, so_many_as[idx1 + 1:idx2])
    SHA.update!(ctx, so_many_as[idx2 + 1:end])

    # Ensure the hash is the appropriate one
    hash = bytes2hex(SHA.digest!(ctx))
    if hash != answers[sha_funcs[sha_idx]][end]
        print("\n")
        warn(
        """
        For $(describe_hash(sha_types[sha_funcs[sha_idx]])) expected:
            $(answers[sha_funcs[sha_idx]][end-1])
        Calculated:
            $(hash)
        """)
        nerrors += 1
    else
        print(".")
    end
end
println("Done! [$(nerrors - nerrors_old) errors]")

if nerrors == 0
    println("ALL OK")
else
    println("Failed with $nerrors failures")
end
exit(nerrors)
