# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LibUnwind_jll

@testset "LibUnwind_jll" begin
    if !Sys.isapple() && !Sys.iswindows()
        @test dlsym(LibUnwind_jll.libunwind_handle, :unw_backtrace; throw_error=false) !== nothing
    end
end
