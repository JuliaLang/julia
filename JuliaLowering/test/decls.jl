test_mod = Module()

@test JuliaLowering.include_string(test_mod, """
begin
    local x::Int = 1.0
    x
end
""") === 1

# In value position, yield the right hand side, not `x`
@test JuliaLowering.include_string(test_mod, """
begin
    local x::Int = 1.0
end
""") === 1.0

# Global decl in value position without assignment returns nothing
@test JuliaLowering.include_string(test_mod, "global x_no_assign") === nothing

# Unadorned declarations
@test JuliaLowering.include_string(test_mod, """
let
    a = 0.0
    x::Int = a
    x
end
""") === 0

@test JuliaLowering.include_string(test_mod, """
let
    local x::Int = 1
    x1 = x
    x = 20.0
    x2 = x
    (x1,x2)
end
""") === (1, 20)

# Global const mixes
@test JuliaLowering.include_string(test_mod, "global x_g = 1") === 1
@test Base.isdefinedglobal(test_mod, :x_g)
@test !Base.isconst(test_mod, :x_g)
@test test_mod.x_g === 1

@test JuliaLowering.include_string(test_mod, "const x_c = 1") === 1
@test Base.isdefinedglobal(test_mod, :x_c)
@test Base.isconst(test_mod, :x_c)
@test test_mod.x_c === 1

@test JuliaLowering.include_string(test_mod, "global const x_gc = 1") === 1
@test Base.isdefinedglobal(test_mod, :x_gc)
@test Base.isconst(test_mod, :x_gc)
@test test_mod.x_gc === 1

@test JuliaLowering.include_string(test_mod, "const global x_cg = 1") === 1
@test Base.isdefinedglobal(test_mod, :x_cg)
@test Base.isconst(test_mod, :x_cg)
@test test_mod.x_cg === 1

# lowering is strict about the nesting order where parsing is not
@test_throws LoweringError jl_eval(test_mod, Expr(:global, Expr(:const, Expr(:(=), :a, 1))))
@test_throws ErrorException fl_eval(test_mod, Expr(:global, Expr(:const, Expr(:(=), :a, 1))))

# Possibly worth testing excessive global/const keywords or invalid combinations
# (local + global/const) once we decide whether that's a parse error or a
# lowering error

# Global decls with types
@test JuliaLowering.include_string(test_mod, """
global a_typed_global::Int = 10.0
""") === 10.0
@test Core.get_binding_type(test_mod, :a_typed_global) === Int
@test test_mod.a_typed_global === 10
@test JuliaLowering.include_string(test_mod, """
global a_curly_typed_global::Union{Int, Float64} = 10.0
""") === 10.0
@test Core.get_binding_type(test_mod, :a_curly_typed_global) === Union{Int, Float64}
@test test_mod.a_curly_typed_global === 10.0
@test JuliaLowering.include_string(test_mod, """
begin
    global opassign_global = 1
    global opassign_global += 1
end
""") === 2
@test test_mod.opassign_global === 2
@test JuliaLowering.include_string(test_mod, """
begin
    global dotopassign_global = [1,2,3]
    global dotopassign_global .+= 1
end
""") == [2,3,4]
@test test_mod.dotopassign_global == [2,3,4]

# Also allowed in nontrivial scopes in a top level thunk
@test JuliaLowering.include_string(test_mod, """
let
    global a_typed_global_2::Int = 10.0
end
""") === 10.0
@test Core.get_binding_type(test_mod, :a_typed_global_2) === Int
@test test_mod.a_typed_global_2 === 10
@test JuliaLowering.include_string(test_mod, """
let
    global a_curly_typed_global_2::Union{Int, Float64} = 10.0
end
""") === 10.0
@test Core.get_binding_type(test_mod, :a_curly_typed_global_2) === Union{Int, Float64}
@test test_mod.a_curly_typed_global_2 === 10.0
@test JuliaLowering.include_string(test_mod, """
begin
    global opassign_global_t::Int = 1
    global opassign_global_t::Int += 1.0
end
""") === 2.0
@test Core.get_binding_type(test_mod, :opassign_global_t) === Int
@test test_mod.opassign_global_t === 2
@test JuliaLowering.include_string(test_mod, """
begin
    global dotopassign_global_t::Vector{Int} = [1,2,3]
    global dotopassign_global_t::Vector{Int} .+= [1.0,2.0,3.0]
end
""") == [2.0,4.0,6.0]
@test Core.get_binding_type(test_mod, :dotopassign_global_t) === Vector{Int}
@test test_mod.dotopassign_global_t == [2,4,6]

@test JuliaLowering.include_string(test_mod, "const x_c_T::Int = 9") === 9
@test Base.isdefinedglobal(test_mod, :x_c_T)
@test Base.isconst(test_mod, :x_c_T)

@testset "typed const redeclaration" begin
    # redeclaration of the same value used to be allowed
    @test_throws ErrorException JuliaLowering.include_string(test_mod, "x_c_T = 9")
    @test_throws ErrorException JuliaLowering.include_string(test_mod, "x_c_T = 10")
    # redeclaration with const should be OK
    @test JuliaLowering.include_string(test_mod, "const x_c_T::Int = 0") === 0
end

# Tuple/destructuring assignments
@test JuliaLowering.include_string(test_mod, "(a0, a1, a2) = [1,2,3]") == [1,2,3]
@test JuliaLowering.include_string(test_mod, "const a,b,c = 1,2,3") === (1, 2, 3)

@testset "Placeholder decls" begin
    @test JuliaLowering.include_string(test_mod, "global _ = 1") === 1
    @test JuliaLowering.include_string(test_mod, "global _::Int = 1") === 1
    @test JuliaLowering.include_string(test_mod, "let; local _; _ = 1; end") === 1
    @test JuliaLowering.include_string(test_mod, "let; local _::Int = 1; end") === 1
    @test JuliaLowering.include_string(test_mod, "let; local (a0, _, a2) = [1,2,3]; end") == [1,2,3]
    @test JuliaLowering.include_string(test_mod, "let; local (a0, _::Int, a2) = [1,2,3]; end") == [1,2,3]
end

test_mod_2 = Module()
@testset "toplevel-preserving syntax" begin
    JuliaLowering.include_string(test_mod_2, "if true; global v1::Bool; else const v1 = 1; end")
    @test !isdefined(test_mod_2, :v1)
    @test Base.binding_kind(test_mod_2, :v1) == Base.PARTITION_KIND_GLOBAL
    @test Core.get_binding_type(test_mod_2, :v1) == Bool

    JuliaLowering.include_string(test_mod_2, "if false; global v2::Bool; else const v2 = 2; end")
    @test test_mod_2.v2 === 2
    @test Base.binding_kind(test_mod_2, :v2) == Base.PARTITION_KIND_CONST

    JuliaLowering.include_string(test_mod_2, "v3 = if true; global v4::Bool; 4 else const v4 = 5; 6; end")
    @test test_mod_2.v3 == 4
    @test !isdefined(test_mod_2, :v4)
    @test Base.binding_kind(test_mod_2, :v4) == Base.PARTITION_KIND_GLOBAL
    @test Core.get_binding_type(test_mod_2, :v4) == Bool

    JuliaLowering.include_string(test_mod_2, "v5 = if false; global v6::Bool; 4 else const v6 = 5; 6; end")
    @test test_mod_2.v5 === 6
    @test test_mod_2.v6 === 5
    @test Base.binding_kind(test_mod_2, :v6) == Base.PARTITION_KIND_CONST
end

@testset "decls on functions" begin
    # local
    @gensym func func2
    @testset let ex = Expr(:let, Expr(:block),
                           Expr(:block,
                                Expr(:local,
                                     Expr(:(=), Expr(:call, func, :x), :x),
                                     Expr(:(=), Expr(:call, func2, :y), :(y+1))),
                                Expr(:tuple,
                                     Expr(:call, func, 1),
                                     Expr(:call, func2, 1))))
        @test jl_eval(test_mod, ex) == (1, 2)
        @test !isdefined(test_mod, func)
        @test !isdefined(test_mod, func2)
    end

    # const
    @gensym func func2
    @testset let ex = Expr(:const, Expr(:(=), Expr(:call, func, :x), :x))
        @test_broken jl_eval(test_mod, ex) isa Function
        @test_broken getproperty(test_mod, func)(1) == 1
    end

    # global
    @gensym func func2
    @testset let ex = Expr(:global,
                           Expr(:(=), Expr(:call, func, :x), :x),
                           Expr(:(=), Expr(:call, func2, :y), :(y+1)))
        @test jl_eval(test_mod, ex) isa Function
        Core.@latestworld
        @test getproperty(test_mod, func)(1) == 1
        @test getproperty(test_mod, func2)(1) == 2
    end

    # const global
    @gensym func func2
    @testset let ex = Expr(:const,
                           Expr(:global,
                                Expr(:(=), Expr(:call, func, :x), :x),
                                Expr(:(=), Expr(:call, func2, :y), :(y+1))))
        @test_broken jl_eval(test_mod, ex) isa Function
        Core.@latestworld
        @test_broken getproperty(test_mod, func)(1) == 1
        # also broken in flisp (func2 doesn't get defined)
        @test_broken getproperty(test_mod, func2)(1) == 2
    end

    # global in local scope
    @gensym func func2
    @testset let ex = Expr(:let, Expr(:block),
                           Expr(:global,
                                Expr(:(=), Expr(:call, func, :x), :x),
                                Expr(:(=), Expr(:call, func2, :y), :(y+1))))
        @test jl_eval(test_mod, ex) isa Function
        Core.@latestworld
        @test getproperty(test_mod, func)(1) == 1
        @test getproperty(test_mod, func2)(1) == 2
    end

    # const global in local scope
    @gensym func func2
    @testset let ex = Expr(:let, Expr(:block),
                           Expr(:const,
                                Expr(:global,
                                     Expr(:(=), Expr(:call, func, :x), :x),
                                     Expr(:(=), Expr(:call, func2, :y), :(y+1)))))
        @test_broken jl_eval(test_mod, ex) isa Function
        Core.@latestworld
        @test_broken getproperty(test_mod, func)(1) == 1
        # also broken in flisp (func2 doesn't get defined)
        @test_broken getproperty(test_mod, func2)(1) == 2
    end
end

@testset "all non-call assignment forms within global, local" for declkind in (:local, :global)
    # basic form
    @gensym sym
    @testset let ex =
        Expr(:let, Expr(:block),
             Expr(:block,
                  Expr(declkind, Expr(:(=), sym, 1)),
                  Expr(:tuple, sym)))
        @test jl_eval(test_mod, ex) == (1,)
        Core.@latestworld
        if declkind === :global
            @test getproperty(test_mod, sym) == 1
        else
            @test !isdefined(test_mod, sym)
        end
    end

    # setproperty form: decl is ignored (this is misleading, syntax TODO)
    @gensym sym
    @testset let ex =
        Expr(:let, Expr(:block),
             Expr(:block,
                  Expr(declkind, Expr(:(=), sym, (;a=1))),
                  Expr(declkind, Expr(:(=), Expr(:., sym, :a), 2)),
                  Expr(:tuple, sym)))
        @test_broken jl_eval(test_mod, ex) == ((;a=2),)
        Core.@latestworld
        @test !isdefined(test_mod, sym)
    end

    # ref form: decl is ignored (syntax TODO)
    @gensym sym
    @testset let ex =
        Expr(:let, Expr(:block),
             Expr(:block,
                  Expr(declkind, Expr(:(=), sym, [1,2,3])),
                  Expr(declkind, Expr(:(=), Expr(:ref, sym, 2), 0)),
                  Expr(:tuple, sym)))
        @test_broken jl_eval(test_mod, ex) == ([1,0,3],)
        Core.@latestworld
        @test !isdefined(test_mod, sym)
    end

    # chained, decl on first
    @gensym sym1 sym2 sym3
    @testset let ex =
        Expr(:let, Expr(:block),
             Expr(:block,
                  Expr(declkind,
                       Expr(:(=), sym1,
                            Expr(:(=), sym2,
                                 Expr(:(=), sym3, :(gensym()))))),
                  Expr(:tuple, sym1, sym2, sym3)))
        res = jl_eval(test_mod, ex)
        Core.@latestworld
        @test res isa Tuple
        @test res[1] == res[2] == res[3]
        if declkind === :global
            @test isdefined(test_mod, sym1)
        else
            @test !isdefined(test_mod, sym1)
        end
        @test !isdefined(test_mod, sym2)
        @test !isdefined(test_mod, sym3)
    end

    # decl sym += val
    @gensym sym
    @testset let ex =
        Expr(:let, Expr(:block),
             Expr(:block,
                  Expr(declkind, Expr(:(=), sym, 1)),
                  Expr(declkind, Expr(:(+=), sym, 2)),
                  Expr(:tuple, sym)))
        @test jl_eval(test_mod, ex) == (3,)
        Core.@latestworld
        if declkind === :global
            @test getproperty(test_mod, sym) == 3
        else
            @test !isdefined(test_mod, sym)
        end
    end

    # decl sym .= val
    @gensym sym
    @testset let ex =
        Expr(:let, Expr(:block),
             Expr(:block,
                  Expr(declkind, Expr(:(=), sym, [1,2,3])),
                  Expr(declkind, Expr(:(.=), sym, 0)),
                  Expr(:tuple, sym)))
        @test jl_eval(test_mod, ex) == ([0,0,0],)
        Core.@latestworld
        if declkind === :global
            @test getproperty(test_mod, sym) == [0,0,0]
        else
            @test !isdefined(test_mod, sym)
        end
    end

    # decl sym .+= val
    @gensym sym
    @testset let ex =
        Expr(:let, Expr(:block),
             Expr(:block,
                  Expr(declkind, Expr(:(=), sym, [1,2,3])),
                  Expr(declkind, Expr(:(.+=), sym, [4,5,6])),
                  Expr(:tuple, sym)))
        @test jl_eval(test_mod, ex) == ([5,7,9],)
        Core.@latestworld
        if declkind === :global
            @test getproperty(test_mod, sym) == [5,7,9]
        else
            @test !isdefined(test_mod, sym)
        end
    end
end

@testset "all non-call assignment forms within `const`" begin
    # prohibited by parsing as of writing this, so hard to make into an IR test
    ex = Expr(:const, Expr(:(.=), :x, 1))
    @test_throws LoweringError jl_lower(test_mod, ex)
    ex = Expr(:const, Expr(:(+=), :x, 1))
    @test_throws LoweringError jl_lower(test_mod, ex)
    ex = Expr(:const, Expr(:(.+=), :x, 1))
    @test_throws LoweringError jl_lower(test_mod, ex)

    # pre-desugared const
    @gensym sym
    ex = Expr(:const, sym, 1)
    @test jl_eval(test_mod, ex) == 1
    @test Base.binding_kind(test_mod, sym) == Base.PARTITION_KIND_CONST

    # chained, const first
    @gensym sym1 sym2 sym3
    @testset let ex = Expr(:const,
                           Expr(:(=), sym1,
                                Expr(:(=), sym2,
                                     Expr(:(=), sym3, :(gensym())))))
        @test jl_eval(test_mod, ex) isa Symbol
        Core.@latestworld

        @test Base.binding_kind(test_mod, sym1) == Base.PARTITION_KIND_CONST
        @test Base.binding_kind(test_mod, sym2) == Base.PARTITION_KIND_GLOBAL
        @test Base.binding_kind(test_mod, sym3) == Base.PARTITION_KIND_GLOBAL
        @test getproperty(test_mod, sym1) ==
            getproperty(test_mod, sym2) ==
            getproperty(test_mod, sym3)
    end

    # chained, const first, with types
    @gensym sym1 sym2 sym3
    @testset let ex = Expr(:const,
                           Expr(:(=), Expr(:(::), sym1, :Symbol),
                                Expr(:(=), Expr(:(::), sym2, :Symbol),
                                     Expr(:(=), Expr(:(::), sym3, :Symbol), :(gensym())))))
        @test_broken jl_eval(test_mod, ex) isa Symbol
        Core.@latestworld

        @test_broken Base.binding_kind(test_mod, sym1) == Base.PARTITION_KIND_CONST
        @test_broken Base.binding_kind(test_mod, sym2) == Base.PARTITION_KIND_GLOBAL
        @test_broken Base.binding_kind(test_mod, sym3) == Base.PARTITION_KIND_GLOBAL
        # also broken in flisp (sym1 has type Any, others are Symbol)
        @test_broken Core.get_binding_type(test_mod, sym1) == Symbol
        @test_broken Core.get_binding_type(test_mod, sym2) == Symbol
        @test_broken Core.get_binding_type(test_mod, sym3) == Symbol
        @test_broken getproperty(test_mod, sym1) ==
            getproperty(test_mod, sym2) ==
            getproperty(test_mod, sym3)
    end

    # chained, const all
    @gensym sym1 sym2 sym3
    @testset let ex = Expr(:const,
                           Expr(:(=), sym1,
                                Expr(:const,
                                     Expr(:(=), sym2,
                                          Expr(:const,
                                               Expr(:(=), sym3, :(gensym())))))))
        @test jl_eval(test_mod, ex) isa Symbol
        Core.@latestworld

        @test Base.binding_kind(test_mod, sym1) == Base.PARTITION_KIND_CONST
        @test Base.binding_kind(test_mod, sym2) == Base.PARTITION_KIND_CONST
        @test Base.binding_kind(test_mod, sym3) == Base.PARTITION_KIND_CONST
        @test getproperty(test_mod, sym1) ==
            getproperty(test_mod, sym2) ==
            getproperty(test_mod, sym3)
    end

    # destructured
    @gensym sym1 sym2 sym3
    @testset let ex = :(const ($sym1, ($sym2, $sym3)) = (1, (2, 3)))
        @test jl_eval(test_mod, ex) == (1, (2, 3))
        Core.@latestworld

        @test Base.binding_kind(test_mod, sym1) == Base.PARTITION_KIND_CONST
        @test Base.binding_kind(test_mod, sym2) == Base.PARTITION_KIND_CONST
        @test Base.binding_kind(test_mod, sym3) == Base.PARTITION_KIND_CONST
        @test getproperty(test_mod, sym1) == 1
        @test getproperty(test_mod, sym2) == 2
        @test getproperty(test_mod, sym3) == 3
    end

    # destructured, with types
    @gensym sym1 sym2 sym3
    @testset let ex = :(const ($sym1::Int, ($sym2::Int, $sym3::Int)) = (1, (2, 3)))
        @test jl_eval(test_mod, ex) == (1, (2, 3))
        Core.@latestworld

        @test Base.binding_kind(test_mod, sym1) == Base.PARTITION_KIND_CONST
        @test Base.binding_kind(test_mod, sym2) == Base.PARTITION_KIND_CONST
        @test Base.binding_kind(test_mod, sym3) == Base.PARTITION_KIND_CONST
        @test getproperty(test_mod, sym1) == 1
        @test getproperty(test_mod, sym2) == 2
        @test getproperty(test_mod, sym3) == 3

        # note flisp also doesn't set binding types, though it does for globals
        @test Core.get_binding_type(test_mod, sym1) == Any
        @test Core.get_binding_type(test_mod, sym2) == Any
        @test Core.get_binding_type(test_mod, sym3) == Any
    end

    # destructured, nested NamedTuple
    @gensym sym1 sym2 sym3
    @testset let ex = :(const ($sym1, (;$sym2, $sym3)) = (1, (;$sym2=2, $sym3=3)))
        @test jl_eval(test_mod, ex) == (1, (;sym2=>2, sym3=>3))
        Core.@latestworld

        @test Base.binding_kind(test_mod, sym1) == Base.PARTITION_KIND_CONST
        @test_broken Base.binding_kind(test_mod, sym2) == Base.PARTITION_KIND_CONST
        @test_broken Base.binding_kind(test_mod, sym3) == Base.PARTITION_KIND_CONST
        @test getproperty(test_mod, sym1) == 1
        @test getproperty(test_mod, sym2) == 2
        @test getproperty(test_mod, sym3) == 3
    end

    # destructured, slurp
    @gensym sym1 sym2 sym3
    @testset let ex = :(const ($sym1, $sym2..., $sym3) = (1, 2, 22, 222, 3))
        @test jl_eval(test_mod, ex) == (1, 2, 22, 222, 3)
        Core.@latestworld

        @test Base.binding_kind(test_mod, sym1) == Base.PARTITION_KIND_CONST
        @test Base.binding_kind(test_mod, sym2) == Base.PARTITION_KIND_CONST
        @test Base.binding_kind(test_mod, sym3) == Base.PARTITION_KIND_CONST
        @test getproperty(test_mod, sym1) == 1
        @test getproperty(test_mod, sym2) == (2, 22, 222)
        @test getproperty(test_mod, sym3) == 3
    end
end
