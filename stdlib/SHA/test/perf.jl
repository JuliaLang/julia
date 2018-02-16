using SHA

if isempty(ARGS)
    error("need file to test sha perf")
elseif !isfile(ARGS[1])
    error("file $(ARGS[1]) does not exist")
end


function do_tests(filepath)
    # test performance
    print("read:    ")
    @time begin
        fh = open(filepath, "r")
        bytes = read(fh)
    end
    GC.gc()

    print("SHA-1:   ")
    sha1(bytes)
    GC.gc()
    @time sha1(bytes)

    print("SHA2-256: ")
    sha256(bytes)
    GC.gc()
    @time sha256(bytes)

    print("SHA2-512: ")
    sha512(bytes)
    GC.gc()
    @time sha512(bytes)

    print("SHA3-256: ")
    sha3_256(bytes)
    GC.gc()
    @time sha3_256(bytes)

    print("SHA3-512: ")
    sha3_512(bytes)
    GC.gc()
    @time sha3_512(bytes)
end

do_tests(ARGS[1])
