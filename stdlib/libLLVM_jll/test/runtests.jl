# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, libLLVM_jll

@testset "libLLVM_jll" begin
    # Try to find a symbol from the C API of libLLVM as a simple sanity check.
    @test dlsym(libLLVM_jll.libLLVM_handle, :LLVMContextCreate; throw_error=false) !== nothing
end
