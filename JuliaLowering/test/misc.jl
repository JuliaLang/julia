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

@testset "docstrings: doc-only expressions" begin
    local jeval(mod, str) = JuliaLowering.include_string(mod, str; expr_compat_mode=true)
    jeval(test_mod, "function fun_exists(x); x; end")
    jeval(test_mod, "module M end; module M2 end")
    # TODO: return values are to be determined, currently Base.Docs.Binding for
    # both lowering implementations.  We can't return the value of the
    # expression in these special cases.
    jeval(test_mod, "\"docstr1\" sym_noexist")
    jeval(test_mod, "\"docstr2\" fun_noexist()")
    jeval(test_mod, "\"docstr3\" fun_exists(sym_noexist)")
    jeval(test_mod, "\"docstr4\" M.sym_noexist")
    jeval(test_mod, "\"docstr5\" M.fun_noexist()")
    jeval(test_mod, "\"docstr6\" M.fun_exists(sym_noexist)")
    @test jeval(test_mod, "@doc sym_noexist")               |> string === "docstr1\n"
    @test jeval(test_mod, "@doc fun_noexist()")             |> string === "docstr2\n"
    @test jeval(test_mod, "@doc fun_exists(sym_noexist)")   |> string === "docstr3\n"
    @test jeval(test_mod, "@doc M.sym_noexist")             |> string === "docstr4\n"
    @test jeval(test_mod, "@doc M.fun_noexist()")           |> string === "docstr5\n"
    @test jeval(test_mod, "@doc M.fun_exists(sym_noexist)") |> string === "docstr6\n"
    @test jeval(test_mod.M, "@doc M.sym_noexist")             |> string === "docstr4\n"
    @test jeval(test_mod.M, "@doc M.fun_noexist()")           |> string === "docstr5\n"
    @test jeval(test_mod.M, "@doc M.fun_exists(sym_noexist)") |> string === "docstr6\n"

    jeval(test_mod.M2, "\"docstr7\" M2.M2.sym_noexist")
    jeval(test_mod.M2, "\"docstr8\" M2.M2.fun_noexist()")
    jeval(test_mod.M2, "\"docstr9\" M2.M2.fun_exists(sym_noexist)")
    @test jeval(test_mod, "@doc M2.M2.sym_noexist")             |> string === "docstr7\n"
    @test jeval(test_mod, "@doc M2.M2.fun_noexist()")           |> string === "docstr8\n"
    @test jeval(test_mod, "@doc M2.M2.fun_exists(sym_noexist)") |> string === "docstr9\n"
    @test jeval(test_mod.M2, "@doc M2.M2.sym_noexist")             |> string === "docstr7\n"
    @test jeval(test_mod.M2, "@doc M2.M2.fun_noexist()")           |> string === "docstr8\n"
    @test jeval(test_mod.M2, "@doc M2.M2.fun_exists(sym_noexist)") |> string === "docstr9\n"

    # Try with signatures and type variables
    jeval(test_mod, "abstract type T_exists end")

    jeval(test_mod, "\"docstr10\" f10(x::Int, y, z::T_exists)")
    d = jeval(test_mod, "@doc f10")
    @test d |> string === "docstr10\n"
    # TODO: Is there a better way of accessing this? Feel free to change tests
    # if docsystem storage changes.
    @test d.meta[:results][1].data[:typesig] === Tuple{Int, Any, test_mod.T_exists}

    jeval(test_mod, "\"docstr11\" f11(x::T_exists, y::U, z::T) where {T, U<:Number}")
    d = jeval(test_mod, "@doc f11")
    @test d |> string === "docstr11\n"
    @test d.meta[:results][1].data[:typesig] === Tuple{test_mod.T_exists, U, T} where {T, U<:Number}

    jeval(test_mod, "\"docstr12\" f12(x::Int, y::U, z::T=1) where {T, U<:Number}")
    d = jeval(test_mod, "@doc f12")
    @test d |> string === "docstr12\n"
    @test d.meta[:results][1].data[:typesig] === Union{Tuple{Int64, U, T}, Tuple{Int64, U}} where {T, U<:Number}

end

end
