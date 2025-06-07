using Libdl
using Zstd_jll # Note this uses the vendored older non-LazyLibrary version of Zstd_jll

function @main(args::Vector{String})::Cint
    println(Core.stdout, "Julia! Hello, world!")
    fptr = dlsym(Zstd_jll.libzstd_handle, :ZSTD_versionString)
    println(Core.stdout, unsafe_string(ccall(fptr, Cstring, ())))
    println(Core.stdout, unsafe_string(ccall((:ZSTD_versionString, libzstd), Cstring, ())))
    return 0
end
