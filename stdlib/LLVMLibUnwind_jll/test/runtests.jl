# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LLVMLibUnwind_jll
@testset "LLVMLibUnwind_jll" begin
    if Sys.isapple()
        @test dlsym(llvmlibunwind, :unw_getcontext; throw_error=false) !== nothing
        @test dlsym(llvmlibunwind, :unw_init_local; throw_error=false) !== nothing
        @test dlsym(llvmlibunwind, :unw_init_local_dwarf; throw_error=false) !== nothing
        @test dlsym(llvmlibunwind, :unw_step; throw_error=false) !== nothing
        @test dlsym(llvmlibunwind, :unw_get_reg; throw_error=false) !== nothing
        @test dlsym(llvmlibunwind, :unw_set_reg; throw_error=false) !== nothing
        @test dlsym(llvmlibunwind, :unw_resume; throw_error=false) !== nothing
    end
end
