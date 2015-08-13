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
        const fh = open(filepath, "r")
        const bytes = readbytes(fh)
    end
    gc()

    print("SHA-1:   ")
    sha1(bytes)
    gc()
    @time sha1(bytes)

    print("SHA-256: ")
    sha256(bytes)
    gc()
    @time sha256(bytes)

    print("SHA-512: ")
    sha512(bytes)
    gc()
    @time sha512(bytes)
end

do_tests(ARGS[1])