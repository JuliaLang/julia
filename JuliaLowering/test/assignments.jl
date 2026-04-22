test_mod = Module()

Base.include_string(test_mod,
"""
mutable struct X
    a
    b
end
""")

# TODO: Desugaring of assignment done, but needs `where` lowering
JuliaLowering.include_string(test_mod, """
MyVector{T} = Array{1,T}
""")
@test test_mod.MyVector{Int} == Array{1,Int}

# Chained assignment
@test JuliaLowering.include_string(test_mod, """
let
    a = b = 42
end
""") == 42

# Assignment in value but not tail position
@test JuliaLowering.include_string(test_mod, """
let
    x = begin
        y = 42
    end
    x
end
""") == 42

@test JuliaLowering.include_string(test_mod, """
let
    x = []
    a = b = (push!(x, 1); 42)
    (a,b,x)
end
""") == (42,42,[1])

# setproperty!
@test JuliaLowering.include_string(test_mod, """
let
    x = X(1,2)
    x.a = 10
    (x.a, x.b)
end
""") == (10,2)

# Declarations
@test JuliaLowering.include_string(test_mod, """
let
    x::Int = 1
    x = 10.0
    x
end
""") === 10

# Updating assignments
@test JuliaLowering.include_string(test_mod, """
let x = "hi"
    x *= " ho"
    x
end
""") == "hi ho"

@test JuliaLowering.include_string(test_mod, """
let x = [1,3]
    x .-= [0,1]
    x
end
""") == [1,2]

@test JuliaLowering.include_string(test_mod, """
let x = [1 2; 3 4]
    x[begin, 1:end] .-= 1
    x
end
""") == [0 1 ; 3 4]

# Test that side effects of computing indices in left hand side only occur
# once.
@test JuliaLowering.include_string(test_mod, """
let
    x = [1, 2]
    n_calls = 0
    the_index() = (n_calls = n_calls + 1; 1)
    x[the_index()] += 1
    x[the_index()]::Int += 1
    x[the_index():end] .+= 1
    n_calls
end
""") == 3

# removing argument side effect in kwcall lhs
@eval test_mod f60152(v, pa; kw) = copy(v)
@test JuliaLowering.include_string(test_mod, """
    f60152([1, 2, 3], 0; kw=0) .*= 2
""") == [2,4,6]
@test JuliaLowering.include_string(test_mod, """
let
    pa_execs = 0
    kw_execs = 0
    out = f60152([1, 2, 3], (pa_execs+=1); kw=(kw_execs+=1)) .*= 2
    (out, pa_execs, kw_execs)
end
""") == ([2,4,6], 1, 1)

@testset "distinction between `=`` and `kw`" begin
    eq = Expr(:(=), :a, 1)
    peq = Expr(:parameters, eq)
    kw = Expr(:kw, :b, 2)
    pkw = Expr(:parameters, kw)

    function outer_ab(ex::Expr)
        Expr(:let, Expr(:block, :(a = 0), :(b = 0)),
             Expr(:block, ex,
                  Expr(:tuple, :a, :b)))
    end

    @eval test_mod function collect_args(args...; kws...)
        (args..., :semicolon, kws...)
    end

    @testset "in :call" begin
        # call
        @testset let ex = Expr(:call, :collect_args, eq)
            @test fl_eval(test_mod, ex) == (1, :semicolon)
            @test jl_eval(test_mod, ex) == (1, :semicolon)
            # `=` in a call assigns the value
            @test fl_eval(test_mod, outer_ab(ex)) == (1, 0)
            @test jl_eval(test_mod, outer_ab(ex)) == (1, 0)
        end
        @testset let ex = Expr(:call, :collect_args, peq)
            @test fl_eval(test_mod, ex) == (:semicolon, :a=>1)
            @test jl_eval(test_mod, ex) == (:semicolon, :a=>1)
            @test fl_eval(test_mod, outer_ab(ex)) == (0, 0)
            @test jl_eval(test_mod, outer_ab(ex)) == (0, 0)
        end
        # `kw` always passes a kwarg and does not assign a value
        @testset let ex = Expr(:call, :collect_args, kw)
            @test fl_eval(test_mod, ex) == (:semicolon, :b=>2)
            @test jl_eval(test_mod, ex) == (:semicolon, :b=>2)
            @test fl_eval(test_mod, outer_ab(ex)) == (0, 0)
            @test jl_eval(test_mod, outer_ab(ex)) == (0, 0)
        end
        @testset let ex = Expr(:call, :collect_args, pkw)
            @test fl_eval(test_mod, ex) == (:semicolon, :b=>2)
            @test jl_eval(test_mod, ex) == (:semicolon, :b=>2)
            @test fl_eval(test_mod, outer_ab(ex)) == (0, 0)
            @test jl_eval(test_mod, outer_ab(ex)) == (0, 0)
        end
    end

    @testset "in dotcall" begin
        let eq = Expr(:(=), :a, [1]),
            peq = Expr(:parameters, eq),
            kw = Expr(:kw, :b, 2),
            pkw = Expr(:parameters, kw)

            @testset let ex = Expr(:(.), :collect_args, Expr(:tuple, eq))
                @test fl_eval(test_mod, ex) == [(1, :semicolon)]
                @test jl_eval(test_mod, ex) == [(1, :semicolon)]
                @test fl_eval(test_mod, outer_ab(ex)) == ([1], 0)
                @test jl_eval(test_mod, outer_ab(ex)) == ([1], 0)
            end
            @testset let ex = Expr(:(.), :collect_args, Expr(:tuple, peq))
                @test fl_eval(test_mod, ex) == (:semicolon, :a=>[1])
                @test jl_eval(test_mod, ex) == (:semicolon, :a=>[1])
                @test fl_eval(test_mod, outer_ab(ex)) == (0, 0)
                @test jl_eval(test_mod, outer_ab(ex)) == (0, 0)
            end
            @testset let ex = Expr(:(.), :collect_args, Expr(:tuple, kw))
                @test fl_eval(test_mod, ex) == (:semicolon, :b=>2)
                @test jl_eval(test_mod, ex) == (:semicolon, :b=>2)
                @test fl_eval(test_mod, outer_ab(ex)) == (0, 0)
                @test jl_eval(test_mod, outer_ab(ex)) == (0, 0)
            end
            @testset let ex = Expr(:(.), :collect_args, Expr(:tuple, pkw))
                @test fl_eval(test_mod, ex) == (:semicolon, :b=>2)
                @test jl_eval(test_mod, ex) == (:semicolon, :b=>2)
                @test fl_eval(test_mod, outer_ab(ex)) == (0, 0)
                @test jl_eval(test_mod, outer_ab(ex)) == (0, 0)
            end
        end
    end

    @testset "in :ref" begin
        @eval test_mod struct DummyGetIndex; field; end
        @eval test_mod function Base.getindex(s::DummyGetIndex, args...; kws...)
            (args..., :semicolon, kws...)
        end
        @testset let ex = Expr(:ref, test_mod.DummyGetIndex(1), eq)
            @test fl_eval(test_mod, ex) == (1, :semicolon)
            @test_broken jl_eval(test_mod, ex) == (1, :semicolon)
            @test fl_eval(test_mod, outer_ab(ex)) == (1, 0)
            @test_broken jl_eval(test_mod, outer_ab(ex)) == (1, 0)
        end
        @testset let ex = Expr(:ref, test_mod.DummyGetIndex(1), peq)
            @test_throws "unexpected semicolon" fl_eval(test_mod, ex)
            @test_throws "unexpected semicolon" jl_eval(test_mod, ex)
        end
        @testset let ex = Expr(:ref, test_mod.DummyGetIndex(1), kw)
            @test fl_eval(test_mod, ex) == (:semicolon, :b=>2)
            @test_broken jl_eval(test_mod, ex) == (:semicolon, :b=>2)
            @test fl_eval(test_mod, outer_ab(ex)) == (0, 0)
            @test_broken jl_eval(test_mod, outer_ab(ex)) == (0, 0)
        end
        @testset let ex = Expr(:ref, test_mod.DummyGetIndex(1), pkw)
            @test_throws "unexpected semicolon" fl_eval(test_mod, ex)
            @test_throws "unexpected semicolon" jl_eval(test_mod, ex)
        end
    end

    @testset "in :tuple" begin
        @testset let ex = Expr(:tuple, eq)
            @test fl_eval(test_mod, ex) == (a=1,)
            @test jl_eval(test_mod, ex) == (a=1,)
        end
        @testset let ex = Expr(:tuple, peq)
            @test fl_eval(test_mod, ex) == (a=1,)
            @test jl_eval(test_mod, ex) == (a=1,)
        end
        @testset let ex = Expr(:tuple, kw) # calls tuple constructor with kw
            @test_throws MethodError fl_eval(test_mod, ex)
            @test_throws MethodError jl_eval(test_mod, ex) broken=true
        end
        @testset let ex = Expr(:tuple, pkw)
            @test fl_eval(test_mod, ex) == (b=2,)
            @test jl_eval(test_mod, ex) == (b=2,)
        end
    end

    @testset "in :curly" begin
        @testset let ex = Expr(:curly, Array, Int, eq)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
        @testset let ex = Expr(:curly, Array, Int, peq)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
        @testset let ex = Expr(:curly, Array, Int, kw) # calls constructor with kw
            @test_throws MethodError fl_eval(test_mod, ex)
            @test_throws MethodError jl_eval(test_mod, ex) broken=true
        end
        @testset let ex = Expr(:curly, Array, Int, pkw)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
    end
    @testset "in :vect" begin
        @testset let ex = Expr(:vect, eq)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
        @testset let ex = Expr(:vect, peq)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
        @testset let ex = Expr(:vect, kw) # calls vect constructor with kw
            @test_throws MethodError fl_eval(test_mod, ex)
            @test_throws MethodError jl_eval(test_mod, ex) broken=true
        end
        @testset let ex = Expr(:vect, pkw)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
    end
    @testset "in :braces" begin
        @testset let ex = Expr(:braces, eq)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
        @testset let ex = Expr(:braces, peq)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
        @testset let ex = Expr(:braces, kw)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
        @testset let ex = Expr(:braces, pkw)
            @test_throws ErrorException fl_eval(test_mod, ex)
            @test_throws LoweringError jl_eval(test_mod, ex)
        end
    end
end

@testset "macros can have lhs-reserved or underscore names" begin
    local m = Module()

    @test JuliaLowering.include_string(m, """
    module ShortForm
        macro ccall end
        macro cglobal end
        macro _ end
    end
    """) isa Module
    @test Base.isdefinedglobal(m.ShortForm, Symbol("@ccall"))
    @test Base.isdefinedglobal(m.ShortForm, Symbol("@cglobal"))
    @test Base.isdefinedglobal(m.ShortForm, Symbol("@_"))

    @test JuliaLowering.include_string(m, """ macro ccall(x); x; end """) isa Function
    @test JuliaLowering.include_string(m, "@ccall(1)") == 1

    @test JuliaLowering.include_string(m, """ macro cglobal(x); x; end """) isa Function
    @test JuliaLowering.include_string(m, "@cglobal(1)") == 1

    @test JuliaLowering.include_string(m, """ macro _(x); x; end """) isa Function
    @test JuliaLowering.include_string(m, "@_(3)") == 3
end
