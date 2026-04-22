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

@test JuliaLowering.include_string(test_mod, raw"""
let
    x = 10
    @eval $x + 2
end
""") == 12

@test JuliaLowering.include_string(test_mod, raw"""
module EvalTest
    _some_var = 2
end
let
    x = 10
    @eval EvalTest $x + _some_var
end
""") == 12

@test JuliaLowering.include_string(test_mod, """
let x=11
    20x
end
""") == 220

@eval test_mod libccalltest_var = "libccalltest"

@testset "cglobal" begin
    cg = JuliaLowering.include_string(test_mod, """
        cglobal(:jl_, Any)
    """)
    @test cg isa Ptr{Any}
    @test cg !== C_NULL

    cg = JuliaLowering.include_string(test_mod, """
        cglobal((:global_var, libccalltest_var), Cint)
    """)
    @test cg isa Ptr{Cint}
    @test cg !== C_NULL
    @test unsafe_load(cg) == 1

    @eval test_mod global cglobal_tuple = (:global_var, libccalltest_var)
    cg = JuliaLowering.include_string(test_mod, """
        cglobal(cglobal_tuple, Cint)
    """)
    @test cg isa Ptr{Cint}
    @test cg !== C_NULL
    @test unsafe_load(cg) == 1
    cg = JuliaLowering.include_string(test_mod, """
        let local_tuple = (:global_var, libccalltest_var)
            cglobal(local_tuple, Cint)
        end
    """)
    @test cg isa Ptr{Cint}
    @test cg !== C_NULL
    @test unsafe_load(cg) == 1
end

# ccall
@test JuliaLowering.include_string(test_mod, """
ccall(:strlen, Csize_t, (Cstring,), "asdfg")
""") == 5
@test JuliaLowering.include_string(test_mod, """
function cvarargs_0()
    strp = Ref{Ptr{Cchar}}(0)
    fmt = "hi"
    len = ccall(:asprintf, Cint, (Ptr{Ptr{Cchar}}, Cstring, Cfloat...), strp, fmt)
    str = unsafe_string(strp[], len)
    Libc.free(strp[])
    return str
end
""") isa Function
@test test_mod.cvarargs_0() == "hi"
@test JuliaLowering.include_string(test_mod, """
function cvarargs_2(arg1::Float64, arg2::Float64)
    strp = Ref{Ptr{Cchar}}(0)
    fmt = "%3.1f %3.1f"
    len = ccall(:asprintf, Cint, (Ptr{Ptr{Cchar}}, Cstring, Cfloat...), strp, fmt, arg1, arg2)
    str = unsafe_string(strp[], len)
    Libc.free(strp[])
    return str
end
""") isa Function
@test test_mod.cvarargs_2(1.1, 2.2) == "1.1 2.2"
# (function, library) syntax
@test JuliaLowering.include_string(test_mod, """
    ccall((:ctest, :libccalltest), Complex{Int}, (Complex{Int},), 10 + 20im)
""") === 11 + 18im
# (function, library): library is a global
@test JuliaLowering.include_string(test_mod, """
    ccall((:ctest, libccalltest_var), Complex{Int}, (Complex{Int},), 10 + 20im)
""") === 11 + 18im

@testset "(robot-generated) ccall (sym, lib) tuple: globals and hygiene" begin
    # library is a module-qualified global
    JuliaLowering.include_string(test_mod, """
    module CCallLibMod
        const the_lib = "libccalltest"
    end
    """)
    @test JuliaLowering.include_string(test_mod, """
        ccall((:ctest, CCallLibMod.the_lib), Complex{Int}, (Complex{Int},), 10 + 20im)
    """) === 11 + 18im

    # macro in a nested module produces ccall with lib from that module (hygiene)
    JuliaLowering.include_string(test_mod, raw"""
    module CCallHygieneMod
        const mylib = "libccalltest"
        macro do_ccall()
            :(ccall((:ctest, mylib), Complex{Int}, (Complex{Int},), 10 + 20im))
        end
    end
    """)
    @test JuliaLowering.include_string(test_mod, """
        CCallHygieneMod.@do_ccall()
    """) === 11 + 18im

    # hygiene: `mylib` in the macro body should resolve in CCallHygieneMod, not
    # the caller, even when the caller defines a different `mylib`
    @test JuliaLowering.include_string(test_mod, """
        mylib = "this_lib_does_not_exist"
        CCallHygieneMod.@do_ccall()
    """) === 11 + 18im

    # macro that interpolates the lib value at expansion time
    JuliaLowering.include_string(test_mod, raw"""
    module CCallHygieneMod2
        const mylib2 = "libccalltest"
        macro do_ccall_interp()
            lib = mylib2
            :(ccall((:ctest, $lib), Complex{Int}, (Complex{Int},), 10 + 20im))
        end
    end
    """)
    @test JuliaLowering.include_string(test_mod, """
        CCallHygieneMod2.@do_ccall_interp()
    """) === 11 + 18im

    # ccall with plain symbol name still works inside a function
    @test JuliaLowering.include_string(test_mod, """
        function ccall_plain_sym()
            ccall(:strlen, Csize_t, (Cstring,), "abc")
        end
    """) isa Function
    @test test_mod.ccall_plain_sym() == 3

    # ccall with (sym, lib) tuple where lib is a global, inside a function
    @test JuliaLowering.include_string(test_mod, """
        function ccall_global_lib()
            ccall((:ctest, libccalltest_var), Complex{Int}, (Complex{Int},), 10 + 20im)
        end
    """) isa Function
    @test test_mod.ccall_global_lib() === 11 + 18im

    # ccall with module-qualified lib inside a function
    @test JuliaLowering.include_string(test_mod, """
        function ccall_qualified_lib()
            ccall((:ctest, CCallLibMod.the_lib), Complex{Int}, (Complex{Int},), 10 + 20im)
        end
    """) isa Function
    @test test_mod.ccall_qualified_lib() === 11 + 18im
end

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

# Test that hygiene works with @ccallable function names
JuliaLowering.include_string(test_mod, raw"""
f_ccallable_hygiene() = 1

module Nested
    f_ccallable_hygiene() = 2
    macro cfunction_hygiene()
        :(@cfunction($f_ccallable_hygiene, Int, ()))
    end
end
""")
cf_hygiene = JuliaLowering.include_string(test_mod, """
Nested.@cfunction_hygiene
""")
@test @ccall($cf_hygiene()::Int) == 2
# Same as above, but non-interpolated symbol.  Arguably this could return 20,
# but if it should, this is a bug in the macro implementation, not lowering.
# Match Base for now.
JuliaLowering.include_string(test_mod, raw"""
f_ccallable_hygiene() = 10

module Nested
    f_ccallable_hygiene() = 20
    macro cfunction_hygiene()
        :(@cfunction(f_ccallable_hygiene, Int, ()))
    end
end
""")
cf_hygiene = JuliaLowering.include_string(test_mod, """
Nested.@cfunction_hygiene
""")
@test @ccall($cf_hygiene()::Int) == 10

# quoted function in cfunction
quoted_cfn_anon = JuliaLowering.include_string(test_mod, raw"""
    @cfunction((function(x); x; end), Int, (Int,))
""")
@test ccall(quoted_cfn_anon, Int, (Int,), 1) == 1

quoted_cfn_named = JuliaLowering.include_string(test_mod, raw"""
    @cfunction((function fname_unused(x); x; end), Int, (Int,))
""")
@test ccall(quoted_cfn_named, Int, (Int,), 1) == 1

# flisp-expanded ccall, cfunction should be lowerable by us
let fl_ex = macroexpand(
    test_mod,
    :(cfun_flisp_thunk() = @cfunction(+, Cint, (Cint, Cint))))

    fl_st = JuliaLowering.expr_to_est(fl_ex)
    fl_fn = JuliaLowering.eval(test_mod, fl_st)
    fl_cfn = @invokelatest fl_fn()
    @test fl_fn isa Function
    @test fl_cfn isa Ptr
    @test ccall(fl_cfn, Int, (Int,Int), 1, 2) == 3
end
let fl_ex = macroexpand(
    test_mod,
    :(cfun_flisp_thunk() = @cfunction(function some_cfunc_add(x,y); x+y; end,
                                      Cint, (Cint, Cint))))

    fl_st = JuliaLowering.expr_to_est(fl_ex)
    fl_fn = JuliaLowering.eval(test_mod, fl_st)
    fl_cfn = @invokelatest fl_fn()
    @test fl_fn isa Function
    @test fl_cfn isa Ptr
    @test ccall(fl_cfn, Int, (Int,Int), 1, 2) == 3
end
let fl_ex = macroexpand(
    test_mod,
    :(@ccall(libccalltest_var.ctest((10+20im)::Complex{Int})::Complex{Int})))
    fl_st = JuliaLowering.expr_to_est(fl_ex)
    @test JuliaLowering.eval(test_mod, fl_st) == 11 + 18im
end

# raw :foreigncall should also be lowerable
let raw_foreigncall_ex = Expr(
    :foreigncall, Expr(
        :tuple, QuoteNode(:ctest), "libccalltest"),
    :(Complex{Int}),
    :(Core.svec(Complex{Int})),
    0,
    QuoteNode((:ccall, 0x0000, false)),
    10+20im,
    Complex{Int})

    # test flisp does this: it's unclear how much desugaring the user is
    # responsible for here
    @test fl_eval(test_mod, raw_foreigncall_ex) == 11 + 18im

    @test JuliaLowering.eval(test_mod, JuliaLowering.expr_to_est(raw_foreigncall_ex)) == 11 + 18im
end

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
# Note that this only works with `@generated` functions from 1.13 onwards,
# where the function name can be evaluated at code generation time.
JuliaLowering.include_string(test_mod, raw"""
# In principle, may add other strlen-like functions here for different string
# types
ccallable_sptest_name(::Type{String}) = :strlen

@generated function ccall_with_sparams_in_name(s::T) where {T}
    name = QuoteNode(ccallable_sptest_name(T))
    :(ccall($name, Csize_t, (Cstring,), s))
end
""")
@test test_mod.ccall_with_sparams_in_name("hii") == 3

@testset "CodeInfo: has_image_globalref" begin
    @test lower_str(test_mod, "x + y").args[1].has_image_globalref === false
    @test lower_str(Main, "x + y").args[1].has_image_globalref === true
end

baremodule baremod
macro int128_str(x);  error("baremod macro; expected call to Core macro"); end
macro uint128_str(x); error("baremod macro; expected call to Core macro"); end
macro big_str(x);     error("baremod macro; expected call to Core macro"); end
macro cmd(x);         error("baremod macro; expected call to Core macro"); end
macro doc(x, y);      error("baremod macro; expected call to Core macro"); end
global nothing = "baremod.nothing; expected core nothing"
end
@testset "globalrefs inserted by parsing" begin
    local jl_s_eval = x->JuliaLowering.include_string(baremod, x; expr_compat_mode=true)
    local fl_s_eval = x->fl_eval(baremod, JuliaSyntax.parsestmt(Expr, x; filename="file"))

    let s = "100000000000000000000000000000"
        @test jl_s_eval(s) == fl_s_eval(s)
    end
    let s = "0x100000000000000000000000000000"
        @test jl_s_eval(s) == fl_s_eval(s)
    end
    let s = "10000000000000000000000000000000000000000000000000000000000000000"
        @test jl_s_eval(s) == fl_s_eval(s)
    end
    let s = "`ls`"
        @test jl_s_eval(s) == fl_s_eval(s)
    end

    let s = """
            "foo" function fl_documented_function(); fl_documented_function; end
            """
        @test fl_s_eval(s) isa Function
    end
    @test baremod.fl_documented_function() == baremod.fl_documented_function
    let s = """
            "foo" function jl_documented_function(); jl_documented_function; end
            """
        @test jl_s_eval(s) isa Function
    end
    @test baremod.jl_documented_function() == baremod.jl_documented_function

    let s = """
            function fl_ret_nothing(); return; end
            """
        @test fl_s_eval(s) isa Function
    end
    @test baremod.fl_ret_nothing() == Core.nothing
    let s = """
            function jl_ret_nothing(); return; end
            """
        @test fl_s_eval(s) isa Function
    end
    @test baremod.jl_ret_nothing() == Core.nothing
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

    jeval(test_mod, "\"docstr11\" f11(x::T_exists, y::U, z::T) where {T, U<:Number}")
    d = jeval(test_mod, "@doc f11")
    @test d |> string === "docstr11\n"

    jeval(test_mod, "\"docstr12\" f12(x::Int, y::U, z::T=1) where {T, U<:Number}")
    d = jeval(test_mod, "@doc f12")
    @test d |> string === "docstr12\n"

    # doc-strings on macrocalls (punned on quoted macrocall)
    # TODO: implement and test `doc!` support for this
    @test_broken jeval(test_mod, """
        "doc string"
        :@test
    """) isa Expr
end

# SyntaxTree @eval should pass along expr_compat_mode
@test JuliaLowering.include_string(test_mod, "@eval quote x end";
                                   expr_compat_mode=false) isa SyntaxTree
@test JuliaLowering.include_string(test_mod, "@eval quote x end";
                                   expr_compat_mode=true) isa Expr
@test JuliaLowering.include_string(test_mod, raw"""
    let T = :foo
        @eval @doc $"This is a $T" $T = 1
    end
"""; expr_compat_mode=true) === 1

@testset "tryfinally with scopedvalues" begin
    @eval test_mod scopedval = Base.ScopedValues.ScopedValue(1)
    @eval test_mod val_history = []
    ex = Expr(:tryfinally,
              :(push!(val_history, scopedval[])),
              :(push!(val_history, scopedval[])),
              :(Base.ScopedValues.Scope(Core.current_scope(),
                                        $test_mod.scopedval => 2)))
    JuliaLowering.eval(test_mod, JuliaLowering.expr_to_est(ex); expr_compat_mode=true)
    # try block uses "inner" dynamic scope, finally does not
    @test test_mod.val_history == [2, 1]
    JuliaLowering.eval(test_mod, JuliaLowering.expr_to_est(ex))
    @test test_mod.val_history == [2, 1, 2, 1]
end

# JuliaLowering/issues/144
emptyblock_result = JuliaLowering.eval(test_mod, Expr(:(=), :emptyblock_144, Expr(:block)))
@test emptyblock_result == nothing

@testset "string forms" begin
    @test JuliaLowering.include_string(test_mod, raw"""
        "str"
    """) == "str"
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = 1
        "str$x"
    end
    """) == "str1"
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = 1
        "str$(x)"
    end
    """) == "str1"
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = [1,2,3]
        "str$(x...)"
    end
    """) == "str123"
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = [1,2,3]
        "str$(x)"
    end
    """) == "str[1, 2, 3]"
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = [1,2,3]
        "str$("innerstr$(x...)")"
    end
    """) == "strinnerstr123"
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = [1,2,3]
        "str$(["innerstr$(x...)"]...)"
    end
    """) == "strinnerstr123"

    # cmds
    @test JuliaLowering.include_string(test_mod, raw"""
        `cmdstr`
    """) == `cmdstr`
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = 1
        `cmdstr$x`
    end
    """) == `cmdstr1`
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = 1
        `cmdstr$(x)`
    end
    """) == `cmdstr1`
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = [1,2,3]
        `cmdstr$(x...)`
    end
    """) == `cmdstr123`
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = [1,2,3]
        `cmdstr$(x)`
    end
    """) == `cmdstr1 cmdstr2 cmdstr3`
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = [1,2,3]
        `cmdstr$("innerstr$(x...)")`
    end
    """) == `cmdstrinnerstr123`
    @test JuliaLowering.include_string(test_mod, raw"""
    let x = [1,2,3]
        `cmdstr$(["innerstr$(x...)"]...)`
    end
    """) == `cmdstrinnerstr123`
end

let op_mod = Module(:opmod, false)
    @testset "operators" for run in [
            s->fl_eval(op_mod, JuliaSyntax.parseall(Expr, s)),
            s->JuliaLowering.include_string(op_mod, s; expr_compat_mode=true),
            s->JuliaLowering.include_string(op_mod, s; expr_compat_mode=false)]

        @testset "unary prefix (no parens needed)" for op in String["⋆", "±", "∓", "~", "!", "¬", "√", "∛", "∜"]
            @test run("$(op)x = (x,)") isa Function
            Core.@latestworld
            @test run(op) isa Function
            @test run("$(op)1") == (1,)
            @test run("""
                let $(op)x = (x,x)
                    $(op)1
                end """) == (1,1)
        end
        @testset "prefix" for op in String["..", "+", "-", "⋆", "±", "∓", "~", "!", "¬", "√", "∛", "∜"]
            @test run("$op(a,b,c) = 3") isa Function
            Core.@latestworld
            @test run(op) isa Function
            @test run("$op(1,2,3)") == 3
            @test run("""
                let $op(a,b,c) = (c,b,a)
                    $op(1,2,3)
                end """) == (3,2,1)
        end
        @testset "infix" for op in String["&", "|", "+", "-", ":", ".."]
            @test run("a$(op)b = (a,b)") isa Function
            Core.@latestworld
            @test run(op) isa Function
            @test run("var\"$op\"(1,2)") == (1,2)
            @test run("1$(op)2") == (1,2)
            @test run("""
                let a$(op)b = (b,a)
                    1$(op)2
                end """) == (2,1)
        end
    end
end

@testset "jl_assert" begin
    st = @ast_ [K"function" "foo"::K"Identifier"]
    if JL.DEBUG
        err = try
            JuliaLowering.@jl_assert(1 == 2, (st, "error message 1"), (st, "error message 2"))
            nothing
        catch err
            err
        end
        @test err isa LoweringError
        @test err.internal === true
        @test length(err.sts) == 2
        @test length(err.msgs) == 2
        shown = sprint(show, err)
        @test contains(shown, "error message 1")
        @test contains(shown, "error message 2")
    else
        @test nothing !== try
            JuliaLowering.@jl_assert false st
        catch err
            err
        else
            nothing
        end
    end
end

@testset "(static_parameter n) form as lowering input" begin
    # function (::Type{T},) where T; return (sp 1); end
    ex = Expr(:function,
         Expr(:where,
              Expr(:tuple, Expr(:(::), Expr(:curly, :Type, :T))),
              :T),
              Expr(:block, Expr(:return, Expr(:static_parameter, 1))))
    local f
    @test (f = fl_eval(test_mod, ex)) isa Function
    @test f(String) == String
    @test (f = jl_eval(test_mod, ex; expr_compat_mode=true)) isa Function
    @test f(String) == String
    @test (f = jl_eval(test_mod, ex; expr_compat_mode=false)) isa Function
    @test f(String) == String

    # function (x::T, y::U) where {T, U}; (x, y, (sp 1), (sp 2)); end
    ex = Expr(:function,
         Expr(:where,
              Expr(:tuple, Expr(:(::), :x, :T), Expr(:(::), :y, :U)),
              :T, :U),
              Expr(:block,
                   Expr(:return,
                        Expr(:tuple, :x, :y,
                             Expr(:static_parameter, 1),
                             Expr(:static_parameter, 2)))))
    @test (f = fl_eval(test_mod, ex)) isa Function
    @test f(1, 'a') == (1, 'a', Int, Char)
    @test (f = jl_eval(test_mod, ex; expr_compat_mode=true)) isa Function
    @test f(1, 'a') == (1, 'a', Int, Char)
    @test (f = jl_eval(test_mod, ex; expr_compat_mode=false)) isa Function
    @test f(1, 'a') == (1, 'a', Int, Char)
end
