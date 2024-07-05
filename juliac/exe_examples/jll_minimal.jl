#!/usr/bin/env -S julia --project=@scriptdir

module Main2

using Libdl
using Zstd_jll
Base.@ccallable function main()::Cint
    println(Core.stdout,"Julia! Hello, world!")
    fptr = dlsym(Zstd_jll.libzstd_handle, :ZSTD_versionString)
    println(Core.stdout, unsafe_string(ccall(fptr, Cstring, ())))
    println(Core.stdout, unsafe_string(ccall((:ZSTD_versionString, libzstd), Cstring, ())))
    return 0
end

end
