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

@testset "setproperty" begin
    @test JuliaLowering.include_string(test_mod, """
    let
        x = X(1,2)
        x.a = 10
        (x.a, x.b)
    end
    """) == (10,2)

    # RHS of the dot is not restricted like getproperty, and can be anything but
    # a syntactic tuple (tested as "no assignment to dotcall").
    JuliaLowering.include_string(test_mod, """
    mutable struct AnyDotSetProperty; x; end
    global anydotsetproperty = AnyDotSetProperty(1)
    function Base.setproperty!(asp::AnyDotSetProperty, y, z)
        setfield!(asp, :x, (y, z))
    end
    """)
    @test jl_eval(test_mod, Expr(:(=), Expr(:., :anydotsetproperty, 1), 2)) == 2
    @test test_mod.anydotsetproperty.x == (1,2)
    @test jl_eval(test_mod,
                  Expr(:(=), Expr(:., :anydotsetproperty,
                                  Expr(:call, :identity, 1)), 2)) == 2
    @test test_mod.anydotsetproperty.x == (1,2)
    @test jl_eval(test_mod,
                  Expr(:(=), Expr(:., :anydotsetproperty,
                                  QuoteNode(Expr(:call, :identity, 1))), 2)) == 2
    @test test_mod.anydotsetproperty.x == (Expr(:call, :identity, 1),2)
end

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

@testset "lhs forms" begin
    @test JuliaLowering.include_string(test_mod, """
    mutable struct with_mutable_x; x; end
    let x1 = 1, x2 = [2], x3 = Ref(3), x4 = with_mutable_x(4)
        val = (x1, x2[1], x3[], x4.x) = 10,20,30,40
        (val, x1, x2[1], x3[], x4.x)
    end
    """) == ((10,20,30,40), 10,20,30,40)

    # TODO: both flisp and JL drop the final conversion
    @test JuliaLowering.include_string(test_mod, """
    let x1 = 1, x2 = [2], x3 = Ref(3), x4 = with_mutable_x(4)
        val = (x1::Int, x2[1]::Int, x3[]::Int, x4.x::Int) = 10.0,20.0,30.0,40.0
        (val, x1, x2[1], x3[], x4.x)
    end
    """) == ((10.0,20.0,30.0,40.0), 10,20,30,40.0)
end

@testset "chaining assignments (robot-generated)" begin
    @test_throws MethodError JuliaLowering.include_string(test_mod, """
    let vec = [1,2,3]
        vec[1] = vec = 0
    end
    """)

    # same, but with an identifier rhs (assigned directly, no temporary)
    @test JuliaLowering.include_string(test_mod, """
    let x = Any[1,2,3], w = Any[0,0]
        x[1] = x = w
        (x === w, w[1] === w)
    end
    """) == (true, true)

    # setproperty! also sees the newly assigned value
    @test_throws FieldError JuliaLowering.include_string(test_mod, """
    let x = X(1,2)
        x.a = x = 0
    end
    """)

    # nesting the other way around assigns the element first
    @test JuliaLowering.include_string(test_mod, """
    let x = [1,2,3]
        x = x[1] = 2
        x
    end
    """) === 2

    # side effects run right to left: innermost rhs, then each lhs in turn
    @test JuliaLowering.include_string(test_mod, """
    let order = Symbol[]
        x = [0, 0]
        obj = X(nothing, nothing)
        getarr() = (push!(order, :arr); x)
        getidx() = (push!(order, :idx); 2)
        getobj() = (push!(order, :obj); obj)
        getarr()[getidx()] = getobj().a = v = (push!(order, :rhs); 42)
        (order, x[2], obj.a, v)
    end
    """) == ([:rhs, :obj, :arr, :idx], 42, 42, 42)

    # destructuring of an outer lhs happens after the inner assignment
    @test JuliaLowering.include_string(test_mod, """
    let a = 0, b = 0
        (a, b) = a = (1, 2)
        (a, b)
    end
    """) == (1, 2)

    # the value of the chain is the rhs itself; conversions performed by
    # setindex!, setproperty! or decls in the chain don't leak into other lhss
    @test JuliaLowering.include_string(test_mod, """
    let x = [1.0, 2.0]
        a = x[2] = 3
        (a === 3, x[2] === 3.0)
    end
    """) == (true, true)
    @test JuliaLowering.include_string(test_mod, """
    let r = Ref{Float64}(0.0)
        a = r.x = 3
        (a === 3, r.x === 3.0)
    end
    """) == (true, true)
    @test JuliaLowering.include_string(test_mod, """
    let
        a = b::Int = 1.0
        (a, b)
    end
    """) === (1.0, 1)
    @test JuliaLowering.include_string(test_mod, """
    let
        a::Int = b = 1.0
        (a, b)
    end
    """) === (1, 1.0)

    # destructuring middles yield the unmodified rhs, not a new container
    @test JuliaLowering.include_string(test_mod, """
    let t = (1, 2)
        a = (b, c) = t
        (a === t, b, c)
    end
    """) == (true, 1, 2)
    @test JuliaLowering.include_string(test_mod, """
    let nt = (p = 1, q = 2)
        a = (; p) = nt
        (a === nt, p)
    end
    """) == (true, 1)
    @test JuliaLowering.include_string(test_mod, """
    let
        a = (b, c...) = (1, 2, 3)
        (a, b, c)
    end
    """) == ((1, 2, 3), 1, (2, 3))
    @test JuliaLowering.include_string(test_mod, """
    let a = 0, b = 0, c = 0
        r = (a, b) = (c, d) = (1, 2)
        (r, a, b, c, d)
    end
    """) == ((1, 2), 1, 2, 1, 2)

    # underscores may be assigned anywhere in a chain, but read never
    @test JuliaLowering.include_string(test_mod, "let; a = _ = 3; a; end") === 3
    @test JuliaLowering.include_string(test_mod, "let; _ = a = 3; a; end") === 3
    @test JuliaLowering.include_string(test_mod, "let; _ = _ = 5; end") === 5
    @test_throws LoweringError JuliaLowering.include_string(test_mod, "let; a = b = _; end")

    # repeated variable
    @test JuliaLowering.include_string(test_mod, "let x = 1; x = x = 2; x; end") === 2

    # an identifier rhs is read, so must be defined
    @test_throws UndefVarError JuliaLowering.include_string(test_mod, """
    let
        local p, q, r
        p = q = r
    end
    """)

    # globals and locals can be mixed in one chain, including via a
    # value-position `global` declaration
    @test JuliaLowering.include_string(test_mod, """
    let
        global chain_gmid
        lmix = chain_gmid = 8
        (lmix, chain_gmid)
    end
    """) == (8, 8)
    @test JuliaLowering.include_string(test_mod, """
    let
        a = global chain_gval = 2
        (a, chain_gval)
    end
    """) == (2, 2)

    # decl forms wrapping a chain declare (and convert for) only the first lhs
    @test JuliaLowering.include_string(test_mod, """
    let
        local lt::Int = ltb = 1.0
        (lt, ltb)
    end
    """) === (1, 1.0)
    @test JuliaLowering.include_string(test_mod, """
    global chain_tg::Int = chain_tgb = 2.0
    (chain_tg, chain_tgb)
    """) === (2, 2.0)

    # chains stop at `+=` and `.=`
    @test JuliaLowering.include_string(test_mod, """
    let c = 1
        a = b = c += 1
        (a, b, c)
    end
    """) == (2, 2, 2)
    @test JuliaLowering.include_string(test_mod, """
    let y = [1, 2]
        a = b = y .= 0
        (a === y, b === y)
    end
    """) == (true, true)

    # ... and at short form function definitions, also when written as `=`
    # in pre-parsed ASTs (see "short form function def" in assignments_ir.jl)
    @testset let ex = Expr(:block,
                           Expr(:local, :a),
                           Expr(:(=), :a,
                                Expr(:(=), Expr(:call, :chain_f), Expr(:(=), :c, 1))),
                           Expr(:call, :(===), :a, :chain_f))
        @test fl_eval(test_mod, ex) == true
        @test jl_eval(test_mod, ex) == true
    end

    # curly (type alias definition) in the middle of a chain
    @test JuliaLowering.include_string(test_mod, """
    chain_ga = ChainAlias{T} = Vector
    (chain_ga, ChainAlias)
    """) == (Vector, Vector)
    # but type parameters aren't in scope in the hoisted rhs (same in flisp)
    @test_throws UndefVarError JuliaLowering.include_string(test_mod, """
    chain_gb = ChainAlias2{T} = AbstractVector{T}
    """)

    # invalid lhss are rejected anywhere in a chain
    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    let b = 0, c = 0
        a = [b, c] = (1, 2)
    end
    """)

    # const applies to the first lhs only
    @test JuliaLowering.include_string(test_mod, """
    global chain_arr = [0, 0]
    const chain_c1 = chain_arr[1] = 99
    (chain_c1, chain_arr[1])
    """) == (99, 99)
    @test JuliaLowering.include_string(test_mod, """
    const chain_ca2 = (chain_cb2, chain_cc2) = (3, 4)
    (chain_ca2, chain_cb2, chain_cc2)
    """) == ((3, 4), 3, 4)
    Core.@latestworld
    @test Base.isconst(test_mod, :chain_c1)
    @test !Base.isconst(test_mod, :chain_arr)
    @test Base.isconst(test_mod, :chain_ca2)
    @test !Base.isconst(test_mod, :chain_cb2)

    @test_throws LoweringError JuliaLowering.include_string(test_mod, """
    const chain_arr[1] = b = 1
    """)

    # FIXME: flisp accepts non-identifier first lhss in const chains; these
    # currently throw an internal lowering error
    @test_broken JuliaLowering.include_string(test_mod, """
    const (chain_ca, chain_cb) = chain_cc = (1, 2)
    """) == (1, 2)
    @test_broken JuliaLowering.include_string(test_mod, """
    const chain_ct::Int = chain_ctb = 1.0
    """) == 1.0
end # assignment chaining

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

    # empty name is usable (though won't parse)
    @test jl_eval(m, Expr(:macro, Expr(:call, Symbol(""), :x), :x)) isa Function
    @test jl_eval(m, Expr(:macrocall, Symbol("@"), LineNumberNode(1), 123)) == 123
end
