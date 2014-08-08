using SHA

# test performance
@time begin 
    const fh = open(ARGS[1], "r")
    const bytes = readbytes(fh)
end

test_perf() = begin
    ctx = SHA512_CTX()
    update!(ctx, bytes)
    hash = bytes2hex(digest!(ctx))
end
test_perf()
@time test_perf()
