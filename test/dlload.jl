import Base.Libdl: dlopen, dlclose, dlext

macro good(arg)
    return quote
        hdl = $(arg)
        @test hdl != Ptr{Void}(0)
        dlclose(hdl)
    end
end

# test failures
#non-existent path
@test_throws ErrorException dlopen("/long/and/winding/road/to/nowhere.$dlext")
@test_throws ErrorException dlopen("./fakedl.$dlext")
@test_throws ErrorException dlopen(abspath("./fakedl.$dlext"))

# test search behavior
# case 1: libjulia handle
@good dlopen("")

# case 2: relative and absolute paths
@good dlopen("./libccalltest.$dlext")
@good dlopen("./libccalltest")
@good dlopen(abspath("./libccalltest.$dlext"))
@good dlopen(abspath("./libccalltest"))

# case 3: unqualified names present in DL_LOAD_PATH

push!(Libdl.DL_LOAD_PATH, "$(pwd())")
@good dlopen("libccalltest.$dlext")
@good dlopen("libccalltest")

# variant: /path/tmpdir/libccalltest (extension-free) findable in DL_LOAD_PATH
#          before the (usable) ./libccalltest
# set up fake paths
tmpdir = mktempdir()
write(open("$tmpdir/libccalltest", "w"), "foo")

# path with dlopen-able file first
push!(Libdl.DL_LOAD_PATH, "./", tmpdir)
@good dlopen("libccalltest")
empty!(Libdl.DL_LOAD_PATH)
# path with un-dlopen-able file first
# TODO: this order should also work
push!(Libdl.DL_LOAD_PATH, tmpdir, "./")
#@good dlopen("libccalltest")
empty!(Libdl.DL_LOAD_PATH)
