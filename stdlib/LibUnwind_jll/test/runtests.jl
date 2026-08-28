# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LibUnwind_jll

@testset "LibUnwind_jll" begin
    if !Sys.isapple() && !Sys.iswindows()
        @test dlsym(LibUnwind_jll.libunwind, :unw_backtrace; throw_error=false) !== nothing
    end

    # Preserve the JLLWrappers path compatibility accessor used by packages.
    @test LibUnwind_jll.get_libunwind_path() == LibUnwind_jll.libunwind_path
end
