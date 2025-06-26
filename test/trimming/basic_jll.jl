module MyApp

using Libdl
using Zstd_jll

function print_string(fptr::Ptr{Cvoid})
    println(Core.stdout, unsafe_string(ccall(fptr, Cstring, ())))
end

Base.@ccallable function main()::Cint
    # Test the basic "Hello, world!"
    println(Core.stdout, "Julia! Hello, world!")

    # Make sure that JLL's are working as expected
    println(Core.stdout, unsafe_string(ccall((:ZSTD_versionString, libzstd), Cstring, ())))

    # Add an indirection via `@cfunction`
    cfunc = @cfunction(print_string, Cvoid, (Ptr{Cvoid},))
    fptr = dlsym(Zstd_jll.libzstd_handle, :ZSTD_versionString)
    ccall(cfunc, Cvoid, (Ptr{Cvoid},), fptr)

    return 0
end

end
