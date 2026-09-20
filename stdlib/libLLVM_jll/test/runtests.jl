# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, libLLVM_jll

@testset "libLLVM_jll" begin
    # Try to find a symbol from the C API of libLLVM as a simple sanity check.
    @test dlsym(libLLVM_jll.libLLVM, :LLVMContextCreate; throw_error=false) !== nothing

    # Preserve the JLLWrappers path compatibility accessor used by packages.
    @test libLLVM_jll.get_libLLVM_path() == libLLVM_jll.libLLVM_path
end
