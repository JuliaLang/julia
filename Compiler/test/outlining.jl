# This file is a part of Julia. License is MIT: https://julialang.org/license

# Tests of the outlining of cold regions (`outline_cold_regions!`). Outlining is an
# optimization, so most of these check that behavior is unchanged where it applies.

using Test

include("setup_Compiler.jl")
include("irutils.jl")

is_outlined_invoke(@nospecialize x) = isexpr(x, :invoke) && x.args[1] isa Core.CodeInstance &&
    Base.StackTraces.is_outlined_code(x.args[1])
has_outlined(@nospecialize(f), @nospecialize(tt)) =
    any(is_outlined_invoke, first(only(code_typed(f, tt))).code)

function thrown(f, args...)
    try
        f(args...)
    catch err
        return err
    end
    return nothing
end

struct OutlineTestError <: Exception
    msg::String
end

# A small checked function whose error path builds a message.
@inline function outline_checked(x::Int, k::Int)
    x < 0 && throw(OutlineTestError("negative value $x for scale $k (expected x ≥ 0)"))
    return x * k
end
outline_checked_caller(v::Vector{Int}) = sum(x -> outline_checked(x, 3), v)

@testset "error paths of small functions" begin
    @test has_outlined(outline_checked, (Int, Int))
    @test outline_checked(2, 3) == 6
    @test outline_checked_caller([1, 2, 3]) == 18
    err = thrown(outline_checked_caller, [1, -2, 3])
    @test err isa OutlineTestError
    @test err.msg == "negative value -2 for scale 3 (expected x ≥ 0)"
end

# Two error paths with the same argument types: each still throws its own error.
@inline function outline_two_checks(x::Int, y::Int)
    x < 0 && throw(OutlineTestError("first argument is negative: $x (other: $y)"))
    y < 0 && throw(OutlineTestError("second argument is negative: $y (other: $x)"))
    return x + y
end
outline_two_checks_caller(x, y) = outline_two_checks(x, y) * 2

@testset "several error paths" begin
    @test outline_two_checks_caller(1, 2) == 6
    @test thrown(outline_two_checks_caller, -1, 2).msg == "first argument is negative: -1 (other: 2)"
    @test thrown(outline_two_checks_caller, 1, -2).msg == "second argument is negative: -2 (other: 1)"
end

# An error path that uses fields of a tuple from outside it is passed the fields, not the
# tuple, which callers would have to put in memory.
@inline function outline_fields(t::Tuple{Int,Int})
    t[1] < 0 && throw(OutlineTestError("negative first element: $(t[1]), second: $(t[2])"))
    return t[1] + t[2]
end
@inline function outline_varargs(x::Int, I::Int...)
    x < 0 && throw(OutlineTestError("negative input $x at $(I[1]), $(I[2])"))
    return x
end
outline_fields_caller(x, y) = outline_fields((x, y)) + outline_varargs(x, y, x)

@testset "fields of tuples from outside an error path" begin
    for (f, tt) in ((outline_fields, (Tuple{Int,Int},)), (outline_varargs, (Int, Int, Int)))
        ir = first(only(code_typed(f, tt)))
        i = findfirst(is_outlined_invoke, ir.code)
        @test i !== nothing
        sig = Base.get_ci_mi(ir.code[i].args[1]).specTypes
        @test !any(t -> t <: Tuple, sig.parameters)
    end
    @test outline_fields_caller(1, 2) == 4
    @test thrown(outline_fields_caller, -1, 2).msg == "negative first element: -1, second: 2"
    @test thrown(outline_varargs, -1, 5, 6).msg == "negative input -1 at 5, 6"
end

# An error path inside a `try` is still caught by the function's handler.
@inline function outline_in_try(x::Int)
    try
        x < 0 && throw(OutlineTestError("caught inside: $x, $(x - 1)"))
        return x
    catch err
        return err isa OutlineTestError ? -length(err.msg) : 0
    end
end
outline_in_try_caller(x) = outline_in_try(x) + 1

@testset "error paths in `try`" begin
    @test outline_in_try_caller(5) == 6
    @test outline_in_try_caller(-1) == 1 - length("caught inside: -1, -2")
end

# Error paths that use static parameters, both known and unknown to inference.
@inline function outline_sparam(x::T, n::Int) where {T}
    n < 0 && throw(OutlineTestError("bad count $n for a $T with value $x"))
    return ntuple(_ -> x, 2)
end
outline_sparam_caller(x, n) = outline_sparam(x, n)
outline_sparam_unknown(xs::Vector{Any}, n) = map(x -> outline_sparam(x, n), xs)

@testset "static parameters" begin
    @test outline_sparam_caller(1.5, 1) == (1.5, 1.5)
    @test thrown(outline_sparam_caller, 1.5, -1).msg == "bad count -1 for a Float64 with value 1.5"
    @test thrown(outline_sparam_caller, "s", -2).msg == "bad count -2 for a String with value s"
    @test thrown(outline_sparam_unknown, Any[1, 2.0], -3).msg == "bad count -3 for a $Int with value 1"
end

# Redefining a function that an error path calls takes effect.
outline_message(x) = "old message for $x"
@inline function outline_redefined(x::Int)
    x < 0 && throw(OutlineTestError(string(outline_message(x), " (", x, ")")))
    return x
end
outline_redefined_caller(x) = outline_redefined(x) + 1

@testset "redefinition of code an error path calls" begin
    @test outline_redefined_caller(1) == 2
    @test thrown(outline_redefined_caller, -1).msg == "old message for -1 (-1)"
    @eval outline_message(x) = "new message for $x"
    @test thrown(outline_redefined_caller, -1).msg == "new message for -1 (-1)"
end

# Code compiled without its callee calls it through the callee's method instance, which
# the runtime then compiles. That must work for outlined code too.
@inline function outline_via_mi(x::Int)
    x < 0 && throw(OutlineTestError("negative input to outline_via_mi: $x"))
    return x + 1
end

@testset "outlined code called through its method instance" begin
    ir = first(only(code_typed(outline_via_mi, (Int,))))
    i = findfirst(is_outlined_invoke, ir.code)
    @test i !== nothing
    stmt = ir.code[i]
    @test stmt.args[2] === outline_via_mi && stmt.args[3] == Core.Argument(2)
    mi = Base.get_ci_mi(stmt.args[1])
    args = Any[outline_via_mi, -5]
    err = try
        ccall(:jl_invoke, Any, (Any, Ptr{Any}, UInt32, Any),
              args[1], pointer(args, 2), length(args) - 1, mi)
    catch e
        e
    end
    @test err == OutlineTestError("negative input to outline_via_mi: -5")
end

# Outlined code can only be compiled, not interpreted, so nothing is outlined when code
# may not be compiled.
@testset "no outlining without compilation" begin
    script = """
        struct E <: Exception; msg::String; end
        @inline f(x::Int) = (x < 0 && throw(E("negative input: \$x, expected \$(typeof(x)) ≥ 0")); x + 1)
        outlined() = any(first(only(code_typed(f, (Int,)))).code) do x
            Meta.isexpr(x, :invoke) && x.args[1] isa Core.CodeInstance &&
                Base.StackTraces.is_outlined_code(x.args[1])
        end
        g(x) = f(x) * 2
        err = try g(-1) catch e e end
        print(outlined(), " ", g(1), " ", err.msg)
        """
    for compile in ("min", "no")
        cmd = `$(Base.julia_cmd()) --startup-file=no --compile=$compile -e $script`
        @test read(cmd, String) == "false 4 negative input: -1, expected $Int ≥ 0"
    end
end
