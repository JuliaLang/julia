using SHA

lorem = "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
so_many_as = repmat([0x61], 1000000)
data = {"", "test", lorem, so_many_as}
data_desc = ["the empty string", "the string \"test\"", "lorem ipsum", "one million a's"]
#hash_types = [SHA224_CTX, SHA256_CTX, SHA384_CTX, SHA512_CTX]
sha_funcs =  [sha224, sha256, sha384, sha512]

answers = {
sha224 => [
"d14a028c2a3a2bc9476102bb288234c415a2b01f828ea62ac5b3e42f",
"90a3ed9e32b2aaf4c61c410eb925426119e1a9dc53d4286ade99a809",
"6a0644abcf1e2cecbec2814443dab5f24b7ad8ebb66c75667ab67959",
"20794655980c91d8bbb4c1ea97618a4bf03f42581948b2ee4ee7ad67"
],
sha256 => [
"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
"9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08",
"2c7c3d5f244f1a40069a32224215e0cf9b42485c99d80f357d76f006359c7a18",
"cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0"
],
sha384 => [ 
"38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b",
"768412320f7b0aa5812fce428dc4706b3cae50e02a64caa16a782249bfe8efc4b7ef1ccb126255d196047dfedf17a0a9",
"63980fd0425cd2c3d8a400ee0f2671ef135db03b947ec1af21b6e28f19c16ca272036469541f4d8e336ac6d1da50580f",
"9d0e1809716474cb086e834e310a4a1ced149e9c00f248527972cec5704c2a5b07b8b3dc38ecc4ebae97ddd87f3d8985"
],
sha512 => [ 
"cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e",
"ee26b0dd4af7e749aa1a8ee3c10ae9923f618980772e473f8819a5d4940e0db27ac185f8a0e1d5f84f88bc887fd67b143732c304cc5fa9ad8e6f57f50028a8ff",
"f41d92bc9fc1157a0d1387e67f3d0893b70f7039d3d46d8115b5079d45ad601159398c79c281681e2da09bf7d9f8c23b41d1a0a3c5b528a7f2735933a4353194",
"e718483d0ce769644e2e42c7bc15b4638e1f98b13b2044285632a803afa973ebde0ff244877ea60a4cb0432ce577c31beb009c5c2c49aa2e4eadb217ad8cc09b"
]
}

nerrors = 0
for idx in 1:length(data)
    println("Testing on $(data_desc[idx]):")
    for sha_func in sha_funcs
        print("  $("$(sha_func)"[1:6]): ")
        if sha_func(data[idx]) != answers[sha_func][idx]
            warn(
            """
            Expected:
                $(answers[sha_func][idx])
            Calculated:
                $hash
            """)
            nerrors += 1
        else
            println("OK")
        end
    end
end
exit(nerrors)
