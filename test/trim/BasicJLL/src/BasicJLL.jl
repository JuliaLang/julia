# Test that a JLL dependency works under trimming, at both build- and run-time
module BasicJLL

using Libdl
using Zstd_jll # the stdlib Zstd_jll, whose `libzstd` is a `Libdl.LazyLibrary`

# JLL usage at build-time should function as expected
const build_ver = unsafe_string(ccall((:ZSTD_versionString, libzstd), Cstring, ()))

function print_string(fptr::Ptr{Cvoid})
    println(Core.stdout, unsafe_string(ccall(fptr, Cstring, ())))
end

version_str::String = "1.2.3"

function @main(args::Vector{String})::Cint
    # Test the basic "Hello, world!"
    println(Core.stdout, "Julia! Hello, world!")

    # JLL usage at run-time should function as expected
    ver = unsafe_string(ccall((:ZSTD_versionString, libzstd), Cstring, ()))
    println(Core.stdout, ver)
    @assert ver == build_ver

    parsed_ver = VersionNumber(version_str)
    @assert parsed_ver == v"1.2.3"

    sleep(0.01)

    # Add an indirection via `@cfunction` / 1-arg ccall, with the function
    # pointer obtained through the `LazyLibrary` `dlsym` path
    cfunc = @cfunction(print_string, Cvoid, (Ptr{Cvoid},))
    fptr = dlsym(libzstd, :ZSTD_versionString)
    ccall(cfunc, Cvoid, (Ptr{Cvoid},), fptr)
    return 0
end

end
