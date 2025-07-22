using Libdl
using Zstd_jll # Note this uses the vendored older non-LazyLibrary version of Zstd_jll

# JLL usage at build-time should function as expected
Zstd_jll.__init__()
const build_ver = unsafe_string(ccall((:ZSTD_versionString, libzstd), Cstring, ()))

function print_string(fptr::Ptr{Cvoid})
    println(Core.stdout, unsafe_string(ccall(fptr, Cstring, ())))
end

function @main(args::Vector{String})::Cint
    # Test the basic "Hello, world!"
    println(Core.stdout, "Julia! Hello, world!")

    # JLL usage at run-time should function as expected
    ver = unsafe_string(ccall((:ZSTD_versionString, libzstd), Cstring, ()))
    println(Core.stdout, ver)
    @assert ver == build_ver

    sleep(0.01)

    # Add an indirection via `@cfunction` / 1-arg ccall
    cfunc = @cfunction(print_string, Cvoid, (Ptr{Cvoid},))
    fptr = dlsym(Zstd_jll.libzstd_handle, :ZSTD_versionString)
    ccall(cfunc, Cvoid, (Ptr{Cvoid},), fptr)

    # map/mapreduce should work but relies on inlining and other optimizations
    arr = rand(10)
    sorted_arr = sort(arr)
    tot = sum(sorted_arr)
    tot = prod(sorted_arr)
    a = any(x -> x > 0, sorted_arr)
    b = all(x -> x >= 0, sorted_arr)
    c = map(x -> x^2, sorted_arr)
    # d = mapreduce(x -> x^2, +, sorted_arr) this doesn't work because of mapreduce_empty_iter having F specialized
    # e = reduce(xor, rand(Int, 10))
    return 0
end
