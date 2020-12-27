# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, libLLVM_jll

@testset "libLLVM_jll" begin
    @test dlsym(libLLVM_jll.libLLVM_handle, :LLVMInitializeTarget; throw_error=false) !== nothing
end
