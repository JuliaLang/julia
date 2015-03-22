using SHA

if isempty(ARGS)
    error("need file to test sha perf")
elseif !isfile(ARGS[1])
    error("file $(ARGS[1]) does not exist")
end

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
