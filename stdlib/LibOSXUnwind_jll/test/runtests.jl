# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, LibOSXUnwind_jll

@testset "LibOSXUnwind_jll" begin
    @static if Sys.isapple()
        @test dlsym(LibOSXUnwind_jll.libosxunwind_handle, :_Unwind_Backtrace; throw_error=false) !== nothing
    end
end
