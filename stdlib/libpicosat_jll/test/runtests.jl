# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Libdl, libpicosat_jll

const PICOSAT_SATISFIABLE = 10
const PICOSAT_UNSATISFIABLE = 20

@testset "libpicosat_jll" begin
    @test unsafe_string(ccall((:picosat_version, libpicosat), Cstring, ())) == "965"

    # (x1 ∨ x2) ∧ ¬x1 is satisfiable only with x2
    ps = ccall((:picosat_init, libpicosat), Ptr{Cvoid}, ())
    for lit in (1, 2, 0, -1, 0)
        ccall((:picosat_add, libpicosat), Cint, (Ptr{Cvoid}, Cint), ps, lit)
    end
    @test ccall((:picosat_sat, libpicosat), Cint, (Ptr{Cvoid}, Cint), ps, -1) == PICOSAT_SATISFIABLE
    @test ccall((:picosat_deref, libpicosat), Cint, (Ptr{Cvoid}, Cint), ps, 1) == -1
    @test ccall((:picosat_deref, libpicosat), Cint, (Ptr{Cvoid}, Cint), ps, 2) == 1
    # adding ¬x2 makes it unsatisfiable
    for lit in (-2, 0)
        ccall((:picosat_add, libpicosat), Cint, (Ptr{Cvoid}, Cint), ps, lit)
    end
    @test ccall((:picosat_sat, libpicosat), Cint, (Ptr{Cvoid}, Cint), ps, -1) == PICOSAT_UNSATISFIABLE
    ccall((:picosat_reset, libpicosat), Cvoid, (Ptr{Cvoid},), ps)

    # Preserve the JLLWrappers path compatibility accessor used by packages.
    @test libpicosat_jll.get_libpicosat_path() == libpicosat_jll.libpicosat_path
end
