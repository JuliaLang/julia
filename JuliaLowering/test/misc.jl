@testset "Miscellanous" begin

test_mod = Module()

# Blocks
@test JuliaLowering.include_string(test_mod, """
begin
end
""") == nothing

# Placeholders
@test JuliaLowering.include_string(test_mod, """_ = 10""") == 10

# GC.@preserve
@test JuliaLowering.include_string(test_mod, """
let x = [1,2]
    GC.@preserve x begin
        x
    end
end
""") == [1,2]

@test JuliaLowering.include_string(test_mod, """
let x=11
    20x
end
""") == 220

# ccall
@test JuliaLowering.include_string(test_mod, """
ccall(:strlen, Csize_t, (Cstring,), "asdfg")
""") == 5

# cfunction
JuliaLowering.include_string(test_mod, """
function f_ccallable(x, y)
    x + y * 10
end
""")
cf_int = JuliaLowering.include_string(test_mod, """
@cfunction(f_ccallable, Int, (Int,Int))
""")
@test @ccall($cf_int(2::Int, 3::Int)::Int) == 32
cf_float = JuliaLowering.include_string(test_mod, """
@cfunction(f_ccallable, Float64, (Float64,Float64))
""")
@test @ccall($cf_float(2::Float64, 3::Float64)::Float64) == 32.0

# Test that hygiene works with @ccallable function names (this is broken in
# Base)
JuliaLowering.include_string(test_mod, raw"""
f_ccallable_hygiene() = 1

module Nested
    f_ccallable_hygiene() = 2
    macro cfunction_hygiene()
        :(@cfunction(f_ccallable_hygiene, Int, ()))
    end
end
""")
cf_hygiene = JuliaLowering.include_string(test_mod, """
Nested.@cfunction_hygiene
""")
@test @ccall($cf_hygiene()::Int) == 2

# Test that ccall can be passed static parameters in type signatures.
#
# Note that the cases where this works are extremely limited and tend to look
# like `Ptr{T}` or `Ref{T}` (`T` doesn't work!?) because of the compilation
# order in which the runtime inspects the arguments to ccall (`Ptr{T}` has a
# well defined C ABI even when `T` is not yet determined). See also
# https://github.com/JuliaLang/julia/issues/29400
# https://github.com/JuliaLang/julia/pull/40947
JuliaLowering.include_string(test_mod, raw"""
function sparam_ccallable(x::Ptr{T}) where {T}
    unsafe_store!(x, one(T))
    nothing
end

function ccall_with_sparams(::Type{T}) where {T}
    x = T[zero(T)]
    cf = @cfunction(sparam_ccallable, Cvoid, (Ptr{T},))
    @ccall $cf(x::Ptr{T})::Cvoid
    x[1]
end
""")
@test test_mod.ccall_with_sparams(Int) === 1
@test test_mod.ccall_with_sparams(Float64) === 1.0

# Test that ccall can be passed static parameters in the function name
JuliaLowering.include_string(test_mod, raw"""
# In principle, may add other strlen-like functions here for different string
# types
ccallable_sptest_name(::Type{String}) = :strlen

function ccall_with_sparams_in_name(s::T) where {T}
    ccall(ccallable_sptest_name(T), Csize_t, (Cstring,), s)
end
""")
@test test_mod.ccall_with_sparams_in_name("hii") == 3

@testset "CodeInfo: has_image_globalref" begin
    @test lower_str(test_mod, "x + y").args[1].has_image_globalref === false
    @test lower_str(Main, "x + y").args[1].has_image_globalref === true
end

end
