using Libdl
using Zstd_jll # Note this uses the vendored older non-LazyLibrary version of Zstd_jll

# JLL usage at build-time should function as expected
Zstd_jll.__init__()
const build_ver = unsafe_string(ccall((:ZSTD_versionString, libzstd), Cstring, ()))

function @main(args::Vector{String})::Cint
    # Test the basic "Hello, world!"
    println(Core.stdout, "Julia! Hello, world!")

    # JLL usage at run-time should function as expected
    ver = unsafe_string(ccall((:ZSTD_versionString, libzstd), Cstring, ()))
    println(Core.stdout, ver)
    @assert ver == build_ver

    # The 1-arg version of ccall should also work
    fptr = dlsym(Zstd_jll.libzstd_handle, :ZSTD_versionString)
    println(Core.stdout, unsafe_string(ccall(fptr, Cstring, ())))

    return 0
end
