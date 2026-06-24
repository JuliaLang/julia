test_mod = Module()

# Test attributes are correctly set for export/public
JuliaLowering.include_string(test_mod, """
x = 1
y = 2
export x
public y
""")
@test Base.isexported(test_mod, :x)
@test Base.ispublic(test_mod, :x)
@test Base.ispublic(test_mod, :y)
@test !Base.isexported(test_mod, :y)

# Test various forms of `using`
C = JuliaLowering.include_string(test_mod, """
module C
    module D
        export x
        public y, f
        x = [101]
        y = [202]

        function f()
            "hi"
        end
    end
    module E
        using ..D: f
        using ..D
        using .D: y as D_y
        using .D: x as D_x_2, y as D_y_2
        import .D.y as D_y_3
    end
end
""")
@test C.D.f === C.E.f
@test C.D.x === C.E.x
@test C.D.y === C.E.D_y
@test C.D.x === C.E.D_x_2
@test C.D.y === C.E.D_y_2
@test C.D.y === C.E.D_y_3

# Test that using F brings in the exported symbol G immediately and that it can
# be used next in the import list.
F = JuliaLowering.include_string(test_mod, """
module F
    export G
    module G
        export G_global
        G_global = "exported from G"
    end
end
""")
JuliaLowering.include_string(test_mod, """
using .F, .G
""")
@test test_mod.F === F
@test test_mod.G === F.G
@test test_mod.G_global === "exported from G"

# Similarly, that import makes symbols available immediately
H = JuliaLowering.include_string(test_mod, """
module H
    module I
        module J
        end
    end
end
""")
JuliaLowering.include_string(test_mod, """
import .H.I, .I.J
""")
@test test_mod.I === H.I
@test test_mod.J === H.I.J
@test test_mod.G_global === "exported from G"

@testset "(AI) from macro expansion" for expr_compat_mode in (true, false)
    macrocall_mod = Module()
    @eval macrocall_mod import JuliaLowering, JuliaLowering.@legacy_quote_to_syntax
    JuliaLowering.include_string(macrocall_mod, raw"""
    module Exporter
        export val
        val = [123]
        other = [456]
    end
    macro imp_names()
        @legacy_quote_to_syntax :(import .Exporter: val, other as o)
    end
    macro use_target()
        @legacy_quote_to_syntax :(using .Exporter)
    end
    """; expr_compat_mode)
    Core.@latestworld
    JuliaLowering.include_string(macrocall_mod, "@imp_names"; expr_compat_mode)
    JuliaLowering.include_string(macrocall_mod, "@use_target"; expr_compat_mode)
    Core.@latestworld
    @test macrocall_mod.val === macrocall_mod.Exporter.val
    @test macrocall_mod.o === macrocall_mod.Exporter.other
    @test !isdefined(macrocall_mod, :other)  # imported only under the name `o`
end

@testset "Imported macrocalls" for expr_compat_mode in (true, false)
    # Test importing macros by their @-name
    macname_mod = Module()
    JuliaLowering.include_string(macname_mod, raw"""
    module Macros
        macro mac1(); "mac1"; end
        macro mac2(); "mac2"; end
        module Inner
            macro mac3(); "mac3"; end
            macro mac4(); "mac4"; end
        end
    end
    """; expr_compat_mode)
    JuliaLowering.include_string(macname_mod, raw"""
    module UseMacros
        import ..Macros:
            @mac1,
            @mac2 as @mac2_renamed,
            Inner.@mac3,
            Inner.@mac4 as @mac4_renamed
    end
    """; expr_compat_mode)
    Core.@latestworld
    @test JuliaLowering.include_string(macname_mod.UseMacros,
                                       "@mac1()"; expr_compat_mode) == "mac1"
    @test JuliaLowering.include_string(macname_mod.UseMacros,
                                       "@mac2_renamed()"; expr_compat_mode) == "mac2"
    @test JuliaLowering.include_string(macname_mod.UseMacros,
                                       "@mac3()"; expr_compat_mode) == "mac3"
    @test JuliaLowering.include_string(macname_mod.UseMacros,
                                       "@mac4_renamed()"; expr_compat_mode) == "mac4"
end

fl_eval(test_mod, :(
    module mod_p_e_n
    public_var = 1
    public p
    exported_var = 2
    export e
    neither_var = 3
    end))
@testset "colon followed by only from-path" begin
    jl_eval(test_mod, Expr(:import, Expr(:(:), Expr(:., :., :mod_p_e_n))))
    @test !isdefined(test_mod, :public_var)
    @test !isdefined(test_mod, :exported_var)
    @test !isdefined(test_mod, :neither_var)
    @test test_mod.mod_p_e_n isa Module
    jl_eval(test_mod, Expr(:using, Expr(:(:), Expr(:., :., :mod_p_e_n))))
    @test !isdefined(test_mod, :public_var)
    @test !isdefined(test_mod, :exported_var)
    @test !isdefined(test_mod, :neither_var)
    @test test_mod.mod_p_e_n isa Module
end

@testset "(AI) public/export module resolution from macros" for (is_new, run) in [
    (false, (mod, x)->fl_eval(mod,JuliaSyntax.parsestmt(Expr, x))),
    (true, (mod, x)->JuliaLowering.include_string(mod, x; expr_compat_mode=true)),
    (true, (mod, x)->JuliaLowering.include_string(mod, x; expr_compat_mode=false))
    ]

    defs_mod = Module(:Defs)
    call_mod = Module(:CallSite)
    Core.eval(call_mod, :(const Defs = $defs_mod))
    Core.eval(defs_mod, :(import JuliaLowering, JuliaLowering.@legacy_quote_to_syntax))
    Core.eval(defs_mod, :(const var"@ast" = $(JuliaLowering.var"@ast")))
    Core.eval(defs_mod, :(const var"@K_str" = $(JuliaSyntax.var"@K_str")))

    # old-style macros: hygienic plain name vs escaped argument
    fl_eval(defs_mod, :(macro old_pub_plain(); Expr(:public, :op_hyg); end))
    fl_eval(defs_mod, :(macro old_exp_plain(); Expr(:export, :oe_hyg); end))
    fl_eval(defs_mod, :(macro old_pub_arg(name); Expr(:public, esc(name)); end))
    fl_eval(defs_mod, :(macro old_exp_arg(name); Expr(:export, esc(name)); end))

    # new-style macros: hygienic name (from syntaxquote) vs argument
    JuliaLowering.include_string(defs_mod, raw"""
        macro new_exp_plain(); @legacy_quote_to_syntax quote export ne_hyg end; end
        macro new_pub_arg(name); @ast __context__ __context__.macrocall [K"public" name]; end
        macro new_exp_arg(name); @ast __context__ __context__.macrocall [K"export" name]; end
    """)
    Core.@latestworld

    # Define the names being marked so `isexported`/`ispublic` are meaningful.
    # Hygienic names live in defs_mod; argument/escaped names live in call_mod.
    Core.eval(defs_mod, :(global op_hyg=1; global oe_hyg=1; global ne_hyg=1))
    Core.eval(call_mod, :(global cp=1; global ce=1; global np=1; global ne=1))

    # old macros, hygienic plain name -> macro-definition module (defs_mod)
    # flisp error: globalref means malformed public
    # @test run(call_mod, "Defs.@old_pub_plain()")
    # @test Base.ispublic(defs_mod, :op_hyg)
    # @test !Base.ispublic(call_mod, :op_hyg)
    run(call_mod, "Defs.@old_exp_plain()"); Core.@latestworld
    @test !Base.isexported(defs_mod, :oe_hyg)
    @test Base.isexported(call_mod, :oe_hyg)

    # old macros, escaped argument -> call-site module (call_mod)
    run(call_mod, "Defs.@old_pub_arg(cp)"); Core.@latestworld
    @test Base.ispublic(call_mod, :cp)
    @test !Base.ispublic(defs_mod, :cp)
    run(call_mod, "Defs.@old_exp_arg(ce)"); Core.@latestworld
    @test Base.isexported(call_mod, :ce)
    @test !Base.isexported(defs_mod, :ce)

    if is_new
        # new macros, hygienic name -> macro-definition module (defs_mod)
        run(call_mod, "Defs.@new_exp_plain()"); Core.@latestworld
        @test Base.isexported(defs_mod, :ne_hyg)
        @test !Base.isexported(call_mod, :ne_hyg)

        # new macros, argument -> call-site module (call_mod)
        run(call_mod, "Defs.@new_pub_arg(np)"); Core.@latestworld
        @test Base.ispublic(call_mod, :np)
        @test !Base.ispublic(defs_mod, :np)
        run(call_mod, "Defs.@new_exp_arg(ne)"); Core.@latestworld
        @test Base.isexported(call_mod, :ne)
        @test !Base.isexported(defs_mod, :ne)
    end
end
